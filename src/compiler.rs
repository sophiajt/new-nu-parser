use crate::{
    errors::SourceError,
    parser::{AstNode, Block, NodeId},
};

pub struct RollbackPoint {
    idx_span_start: usize,
    idx_nodes: usize,
    idx_errors: usize,
    idx_blocks: usize,
    span_offset: usize,
}

#[derive(Debug)]
pub struct Compiler {
    // Core information, indexed by NodeId
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
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
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            span_start: vec![],
            span_end: vec![],
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
        }
    }

    pub fn print(&self) {
        for (idx, ast_node) in self.ast_nodes.iter().enumerate() {
            println!(
                "{}: {:?} ({} to {})",
                idx, ast_node, self.span_start[idx], self.span_end[idx]
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
                    idx, ast_node, self.span_start[idx], self.span_end[idx]
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
        assert_eq!(self.span_start.len(), self.span_end.len());
        RollbackPoint {
            idx_span_start: self.span_start.len(),
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
        self.span_start.truncate(rbp.idx_span_start);
        self.span_end.truncate(rbp.idx_span_start);

        rbp.span_offset
    }
}
