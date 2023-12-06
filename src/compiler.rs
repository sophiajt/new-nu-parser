use crate::errors::Severity;
use crate::{
    errors::SourceError,
    parser::{AstNode, Block, NodeId},
};
use std::collections::HashMap;
use std::env::var;
use std::thread::scope;

pub struct RollbackPoint {
    idx_span_start: usize,
    idx_nodes: usize,
    idx_errors: usize,
    idx_blocks: usize,
    span_offset: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeId(pub usize);

#[derive(Debug, PartialEq)]
pub enum ScopeType {
    /// Default scope marking the scope of a block/closure
    Scope,
    /// Immutable scope brought in by an overlay
    Overlay,
    /// Mutable scope inserted after activating an overlay to prevent mutating the overlay frame
    Light,
}

#[derive(Debug)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub variables: HashMap<Vec<u8>, NodeId>,
    /// Node that defined the scope (e.g., a block or overlay)
    pub node_id: NodeId,
}

impl Scope {
    pub fn new(scope_type: ScopeType, node_id: NodeId) -> Self {
        Scope {
            scope_type,
            variables: HashMap::new(),
            node_id,
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub is_mutable: bool,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct VarId(pub usize);

#[derive(Debug)]
pub struct Compiler {
    // Core information, indexed by NodeId
    pub spans: Vec<Span>,
    ast_nodes: Vec<AstNode>,
    // node_types: Vec<TypeId>,
    // node_lifetimes: Vec<AllocationLifetime>,

    // Blocks, indexed by BlockId
    pub blocks: Vec<Block>,

    pub source: Vec<u8>,

    pub file_offsets: Vec<(String, usize, usize)>, // fname, start, end

    // Definitions:
    // indexed by FunId
    // pub functions: Vec<Function>,
    // indexed by TypeId
    // types: Vec<Type>,

    // Use/def
    // pub call_resolution: HashMap<NodeId, CallTarget>,
    // pub type_resolution: HashMap<NodeId, TypeId>,
    pub errors: Vec<SourceError>,

    // === The following are used for the resolution pass ===
    /// All scopes ever entered, indexed by ScopeId
    pub scopes: Vec<Scope>,
    /// Stack of currently entered scopes
    pub scope_stack: Vec<ScopeId>,

    /// Variables, indexed by VarId
    pub variables: Vec<Variable>,
    /// Mapping of variable's name node -> Variable
    pub var_resolution: HashMap<NodeId, VarId>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            spans: vec![],
            ast_nodes: vec![],
            // node_types: vec![],
            blocks: vec![],

            source: vec![],

            file_offsets: vec![],

            // variables: vec![],
            // functions: vec![],
            // types: vec![],

            // call_resolution: HashMap::new(),
            // var_resolution: HashMap::new(),
            // type_resolution: HashMap::new(),
            errors: vec![],

            scopes: vec![],
            scope_stack: vec![],

            variables: vec![],
            var_resolution: HashMap::new(),
        }
    }

    pub fn print(&self) {
        for (idx, ast_node) in self.ast_nodes.iter().enumerate() {
            println!(
                "{}: {:?} ({} to {})",
                idx, ast_node, self.spans[idx].start, self.spans[idx].end
            );
        }
        if !self.errors.is_empty() {
            println!("==== ERRORS ====");
            for error in &self.errors {
                println!(
                    "{:?} (NodeId {}): {}",
                    error.severity, error.node_id.0, error.message
                );
            }
        }
    }

    #[allow(clippy::format_collect)]
    pub fn display_state(&self) -> String {
        self.ast_nodes
            .iter()
            .enumerate()
            .map(|(idx, ast_node)| {
                format!(
                    "{}: {:?} ({} to {})\n",
                    idx, ast_node, self.spans[idx].start, self.spans[idx].end
                )
            })
            .collect()
    }

    pub fn add_file(&mut self, fname: &str, contents: &[u8]) {
        let span_offset = self.source.len();

        self.file_offsets
            .push((fname.to_string(), span_offset, span_offset + contents.len()));

        self.source.extend_from_slice(contents);
    }

    pub fn span_offset(&self) -> usize {
        self.source.len()
    }

    pub fn get_node(&self, node_id: NodeId) -> &AstNode {
        &self.ast_nodes[node_id.0]
    }

    pub fn get_node_mut(&mut self, node_id: NodeId) -> &mut AstNode {
        &mut self.ast_nodes[node_id.0]
    }

    pub fn push_node(&mut self, ast_node: AstNode) -> NodeId {
        self.ast_nodes.push(ast_node);

        NodeId(self.ast_nodes.len() - 1)
    }

    pub fn get_rollback_point(&self, span_offset: usize) -> RollbackPoint {
        RollbackPoint {
            idx_span_start: self.spans.len(),
            idx_nodes: self.ast_nodes.len(),
            idx_errors: self.errors.len(),
            idx_blocks: self.blocks.len(),
            span_offset,
        }
    }

    pub fn apply_compiler_rollback(&mut self, rbp: RollbackPoint) -> usize {
        self.blocks.truncate(rbp.idx_blocks);
        self.ast_nodes.truncate(rbp.idx_nodes);
        self.errors.truncate(rbp.idx_errors);
        self.spans.truncate(rbp.idx_span_start);

        rbp.span_offset
    }

    /// Get span of node
    pub fn get_span(&self, node_id: NodeId) -> Span {
        *self
            .spans
            .get(node_id.0)
            .expect("internal error: missing span of node")
    }

    /// Get the source contents of a span of a node
    pub fn get_span_contents(&self, node_id: NodeId) -> &[u8] {
        let span = self.get_span(node_id);
        self.source
            .get(span.start..span.end)
            .expect("internal error: missing source of span")
    }

    // === The following methods are used for the resolution pass ===

    pub fn resolve(&mut self) {
        if !self.ast_nodes.is_empty() {
            let last = self.ast_nodes.len() - 1;
            let last_node_id = NodeId(last);

            self.scopes.push(Scope::new(ScopeType::Scope, last_node_id));
            self.scope_stack.push(ScopeId(0));

            self.resolve_node(last_node_id)
        }
    }

    pub fn resolve_node(&mut self, node_id: NodeId) {
        match self.ast_nodes[node_id.0] {
            AstNode::Block(block_id) => {
                // TODO: Remove clone
                let block = self
                    .blocks
                    .get(block_id.0)
                    .expect("internal error: missing block")
                    .clone();

                self.enter_scope(node_id);
                for inner_node_id in block.nodes {
                    self.resolve_node(inner_node_id);
                }
                self.exit_scope();
            }
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            } => {
                self.resolve_node(initializer);
                self.define_variable(variable_name, is_mutable)
            }
            AstNode::Variable => self.resolve_variable(node_id),
            _ => (),
        }
    }

    /// Enter a scope, e.g., a block or a closure
    pub fn enter_scope(&mut self, node_id: NodeId) {
        self.scopes.push(Scope::new(ScopeType::Scope, node_id));
        self.scope_stack.push(ScopeId(self.scopes.len() - 1));
    }

    /// Exit a scope, e.g., when reaching the end of a block or a closure
    ///
    /// When exiting a scope, all overlays (and corresponding light scopes) are removed along
    /// with the removed scope.
    pub fn exit_scope(&mut self) {
        match self
            .scope_stack
            .iter()
            .rposition(|scope_id| self.scopes[scope_id.0].scope_type == ScopeType::Scope)
        {
            None => panic!("internal error: no scope frame to exit"),
            Some(0) => panic!("internal error: can't exit the top-most scope frame"),
            Some(pos) => self.scope_stack.truncate(pos),
        }
    }

    pub fn define_variable(&mut self, var_name_id: NodeId, is_mutable: bool) {
        let var_name = self.get_span_contents(var_name_id).to_vec();

        let current_scope_id = self
            .scope_stack
            .last()
            .expect("internal error: missing scope frame id");

        self.scopes[current_scope_id.0]
            .variables
            .insert(var_name, var_name_id);

        let var = Variable { is_mutable };
        self.variables.push(var);
        let var_id = VarId(self.variables.len() - 1);

        // let the definition of a variable also count as its use
        self.var_resolution.insert(var_name_id, var_id);
    }

    pub fn find_variable(&self, var_name: &[u8]) -> Option<NodeId> {
        for scope_id in self.scope_stack.iter().rev() {
            if let Some(id) = self.scopes[scope_id.0].variables.get(var_name) {
                return Some(*id);
            }
        }

        None
    }

    pub fn resolve_variable(&mut self, unbound_node_id: NodeId) {
        let var_name = self.get_span_contents(unbound_node_id);

        if let Some(node_id) = self.find_variable(var_name) {
            let var_id = self
                .var_resolution
                .get(&node_id)
                .expect("internal error: missing resolved variable");

            self.var_resolution.insert(unbound_node_id, *var_id);
        } else {
            self.errors.push(SourceError {
                message: "variable not found".to_string(),
                node_id: unbound_node_id,
                severity: Severity::Error,
            })
        }
    }
}
