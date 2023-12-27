use crate::errors::Severity;
use crate::{
    errors::SourceError,
    parser::{AstNode, Block, BlockId, NodeId},
};
use std::collections::HashMap;

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
pub enum FrameType {
    /// Default scope frame marking the scope of a block/closure
    Scope,
    /// Immutable frame brought in by an overlay
    Overlay,
    /// Mutable frame inserted after activating an overlay to prevent mutating the overlay frame
    Light,
}

#[derive(Debug)]
pub struct Frame {
    pub frame_type: FrameType,
    pub variables: HashMap<Vec<u8>, NodeId>,
    /// Node that defined the scope frame (e.g., a block or overlay)
    pub node_id: NodeId,
}

impl Frame {
    pub fn new(scope_type: FrameType, node_id: NodeId) -> Self {
        Frame {
            frame_type: scope_type,
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
    /// All scope frames ever entered, indexed by ScopeId
    pub scope: Vec<Frame>,
    /// Stack of currently entered scope frames
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

            scope: vec![],
            scope_stack: vec![],

            variables: vec![],
            var_resolution: HashMap::new(),
        }
    }

    pub fn print(&self) {
        let output = self.display_state();
        print!("{output}");
    }

    #[allow(clippy::format_collect)]
    pub fn display_state(&self) -> String {
        let mut result: String = self
            .ast_nodes
            .iter()
            .enumerate()
            .map(|(idx, ast_node)| {
                format!(
                    "{}: {:?} ({} to {})\n",
                    idx, ast_node, self.spans[idx].start, self.spans[idx].end
                )
            })
            .collect();

        if !self.errors.is_empty() {
            result.push_str("==== ERRORS ====\n");
            for error in &self.errors {
                result.push_str(&format!(
                    "{:?} (NodeId {}): {}\n",
                    error.severity, error.node_id.0, error.message
                ));
            }
        }

        result.push_str("==== SCOPE ====\n");
        for (i, scope) in self.scope.iter().enumerate() {
            let mut vars: Vec<String> = scope
                .variables
                .iter()
                .map(|(name, id)| format!("{0}: {id:?}", String::from_utf8_lossy(name)))
                .collect();

            vars.sort();

            let line = format!(
                "{i}: Frame {0:?}, variables: [ {1} ], node_id: {2:?}\n",
                scope.frame_type,
                vars.join(", "),
                scope.node_id
            );
            result.push_str(&line);
        }

        result
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

    // === The following methods are used for the name binding pass ===

    pub fn resolve(&mut self) {
        if !self.ast_nodes.is_empty() {
            let last = self.ast_nodes.len() - 1;
            let last_node_id = NodeId(last);

            self.scope.push(Frame::new(FrameType::Scope, last_node_id));
            self.scope_stack.push(ScopeId(0));

            self.resolve_node(last_node_id)
        }
    }

    pub fn resolve_node(&mut self, node_id: NodeId) {
        match self.ast_nodes[node_id.0] {
            AstNode::Variable => self.resolve_variable(node_id),
            AstNode::Block(block_id) => self.resolve_block(node_id, block_id, None),
            AstNode::Closure { params, block } => {
                // making sure the closure parameters and body end up in the same scope frame
                let closure_scope = if let Some(params) = params {
                    self.enter_scope(block);
                    self.resolve_node(params);
                    Some(self.exit_scope())
                } else {
                    None
                };

                let AstNode::Block(block_id) = self.ast_nodes[block.0] else {
                    panic!("internal error: closure's body is not a block");
                };

                self.resolve_block(block, block_id, closure_scope);
            }
            AstNode::Def {
                name: _,
                params,
                return_ty: _,
                block,
            } => {
                // making sure the def parameters and body end up in the same scope frame
                self.enter_scope(block);
                self.resolve_node(params);
                let def_scope = self.exit_scope();

                let AstNode::Block(block_id) = self.ast_nodes[block.0] else {
                    panic!("internal error: command definition's body is not a block");
                };

                self.resolve_block(block, block_id, Some(def_scope));
            }
            AstNode::Params(ref params) => {
                // TODO: Remove clone
                let params = params.clone();
                for param in params {
                    self.define_variable(param, false);
                }
            }
            AstNode::Let {
                variable_name,
                ty: _,
                initializer,
                is_mutable,
            } => {
                self.resolve_node(initializer);
                self.define_variable(variable_name, is_mutable)
            }
            AstNode::While { condition, block } => {
                self.resolve_node(condition);
                self.resolve_node(block);
            }
            AstNode::For {
                variable,
                range,
                block,
            } => {
                // making sure the for loop variable and body end up in the same scope frame
                self.enter_scope(block);
                self.define_variable(variable, false);
                let for_body_scope = self.exit_scope();

                self.resolve_node(range);

                let AstNode::Block(block_id) = self.ast_nodes[block.0] else {
                    panic!("internal error: for's body is not a block");
                };

                self.resolve_block(block, block_id, Some(for_body_scope));
            }
            AstNode::Loop { block } => {
                self.resolve_node(block);
            }
            AstNode::Call { head: _, ref args } => {
                // TODO: Remove clone
                let args = args.clone();
                for arg in args {
                    self.resolve_node(arg);
                }
            }
            AstNode::BinaryOp { lhs, op: _, rhs } => {
                self.resolve_node(lhs);
                self.resolve_node(rhs);
            }
            AstNode::Range { lhs, rhs } => {
                self.resolve_node(lhs);
                self.resolve_node(rhs);
            }
            AstNode::List(ref nodes) => {
                // TODO: Remove clone
                let nodes = nodes.clone();
                for node in nodes {
                    self.resolve_node(node);
                }
            }
            AstNode::Table { header, ref rows } => {
                // TODO: Remove clone
                let rows = rows.clone();
                self.resolve_node(header);
                for row in rows {
                    self.resolve_node(row);
                }
            }
            AstNode::Record { ref pairs } => {
                // TODO: Remove clone
                let pairs = pairs.clone();
                for (key, val) in pairs {
                    self.resolve_node(key);
                    self.resolve_node(val);
                }
            }
            AstNode::MemberAccess { target, field } => {
                self.resolve_node(target);
                self.resolve_node(field);
            }
            AstNode::MethodCall { target: _, call } => {
                self.resolve_node(call);
            }
            AstNode::If {
                condition,
                then_block,
                else_block,
            } => {
                self.resolve_node(condition);
                self.resolve_node(then_block);
                if let Some(block) = else_block {
                    self.resolve_node(block);
                }
            }
            AstNode::Match {
                target,
                ref match_arms,
            } => {
                // TODO: Remove clone
                let match_arms = match_arms.clone();
                self.resolve_node(target);
                for (arm_lhs, arm_rhs) in match_arms {
                    self.resolve_node(arm_lhs);
                    self.resolve_node(arm_rhs);
                }
            }
            AstNode::Statement(node) => self.resolve_node(node),
            AstNode::Param { .. } => (/* seems unused for now */),
            AstNode::Type { .. } => ( /* probably doesn't make sense to resolve? */ ),
            AstNode::NamedValue { .. } => (/* seems unused for now */),
            // All remaining matches do not contain NodeId => there is nothing to resolve
            _ => (),
        }
    }

    pub fn resolve_block(
        &mut self,
        node_id: NodeId,
        block_id: BlockId,
        reused_scope: Option<ScopeId>,
    ) {
        // TODO: Remove clone
        let block = self
            .blocks
            .get(block_id.0)
            .expect("internal error: missing block")
            .clone();

        if let Some(scope_id) = reused_scope {
            self.enter_existing_scope(scope_id);
        } else {
            self.enter_scope(node_id);
        }

        for inner_node_id in block.nodes {
            self.resolve_node(inner_node_id);
        }
        self.exit_scope();
    }

    /// Enter an existing scope frame, e.g., a block or a closure
    pub fn enter_existing_scope(&mut self, scope_id: ScopeId) {
        self.scope_stack.push(scope_id);
    }

    /// Enter a new scope frame, e.g., a block or a closure
    pub fn enter_scope(&mut self, node_id: NodeId) {
        self.scope.push(Frame::new(FrameType::Scope, node_id));
        self.scope_stack.push(ScopeId(self.scope.len() - 1));
    }

    /// Exit a scope frame, e.g., when reaching the end of a block or a closure
    ///
    /// When exiting a scope frame, all overlays (and corresponding light frames) are removed along
    /// with the removed frame.
    pub fn exit_scope(&mut self) -> ScopeId {
        match self
            .scope_stack
            .iter()
            .rposition(|scope_id| self.scope[scope_id.0].frame_type == FrameType::Scope)
        {
            None => panic!("internal error: no scope frame to exit"),
            Some(0) => panic!("internal error: can't exit the top-most scope frame"),
            Some(pos) => {
                let scope_id = self.scope_stack[pos];
                self.scope_stack.truncate(pos);
                scope_id
            }
        }
    }

    pub fn define_variable(&mut self, var_name_id: NodeId, is_mutable: bool) {
        let var_name = self.get_span_contents(var_name_id);
        let var_name = trim_var_name(var_name).to_vec();

        let current_scope_id = self
            .scope_stack
            .last()
            .expect("internal error: missing scope frame id");

        self.scope[current_scope_id.0]
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
            if let Some(id) = self.scope[scope_id.0]
                .variables
                .get(trim_var_name(var_name))
            {
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

fn trim_var_name(name: &[u8]) -> &[u8] {
    if name.starts_with(b"$") && name.len() > 1 {
        &name[1..]
    } else {
        name
    }
}
