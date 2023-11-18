use crate::{
    errors::SourceError,
    parser::{AstNode, Block, NodeId},
};
use std::collections::HashMap;

pub struct RollbackPoint {
    idx_span_start: usize,
    idx_nodes: usize,
    idx_errors: usize,
    idx_blocks: usize,
    span_offset: usize,
}

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
    frame_type: FrameType,
    pub vars: HashMap<Vec<u8>, NodeId>, // name -> var
}

impl Frame {
    pub fn new(frame_type: FrameType) -> Self {
        Frame {
            frame_type,
            vars: HashMap::new(),
        }
    }

    pub fn find_variable(&self, name: &[u8]) -> Option<&NodeId> {
        self.vars.get(name)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

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
    // indexed by VarId
    // pub variables: Vec<Variable>,
    // indexed by FunId
    // pub functions: Vec<Function>,
    // indexed by TypeId
    // types: Vec<Type>,

    // Use/def
    // pub call_resolution: HashMap<NodeId, CallTarget>,
    // pub var_resolution: HashMap<NodeId, VarId>,
    // pub type_resolution: HashMap<NodeId, TypeId>,
    pub errors: Vec<SourceError>,

    scope: Vec<Frame>,
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
            scope: vec![Frame::new(FrameType::Scope)],
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

    /// Enter a scope, e.g., a block or a closure
    pub fn enter_scope(&mut self) {
        self.scope.push(Frame {
            frame_type: FrameType::Scope,
            vars: HashMap::new(),
        });
    }

    /// Exit a scope, e.g., when reaching the end of a block or a closure
    ///
    /// When exiting a scope, all overlays (and corresponding light scope frames) are removed along
    /// with the removed scope.
    pub fn exit_scope(&mut self) {
        match self
            .scope
            .iter()
            .rposition(|frame| frame.frame_type == FrameType::Scope)
        {
            None => panic!("no scope frame to exit"),
            Some(0) => panic!("can't exit the top-most scope frame"),
            Some(pos) => self.scope.truncate(pos),
        }
    }

    /// Get a span of a node
    pub fn get_span(&self, node_id: NodeId) -> Span {
        *self.spans.get(node_id.0).expect("node doesn't have span")
    }

    /// Get the source contents under the span
    pub fn get_span_contents(&self, span: Span) -> &[u8] {
        self.source
            .get(span.start..span.end)
            .expect("tried getting invalid span contents")
    }

    /// Fetch last scope frame that can be written to
    pub fn last_frame_mut(&mut self) -> &mut Frame {
        for frame in self.scope.iter_mut().rev() {
            if frame.frame_type == FrameType::Scope || frame.frame_type == FrameType::Light {
                return frame;
            }
        }

        panic!("No valid scope frame");
    }

    pub fn add_variable(&mut self, name: Vec<u8>, var_id: NodeId) {
        self.last_frame_mut().vars.insert(name, var_id);
    }

    pub fn find_variable(&self, name: &[u8]) -> Option<NodeId> {
        for frame in self.scope.iter().rev() {
            if let Some(var_id) = frame.vars.get(name) {
                return Some(*var_id);
            }
        }

        None
    }

    pub fn get_variable(&self, var_id: NodeId) -> &AstNode {
        self.ast_nodes
            .get(var_id.0)
            .expect("tried getting invalid variable AST node")
    }

    /// Flatten the scope frames into only one frame
    /// (experiment)
    pub fn flatten_scope(&self) -> Frame {
        let mut vars = HashMap::new();

        for frame in &self.scope {
            vars.extend(frame.vars.clone());
        }

        Frame {
            frame_type: FrameType::Scope,
            vars,
        }
    }
}
