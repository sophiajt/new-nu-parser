use crate::compiler::Compiler;
use crate::parser::NodeId;

#[derive(Debug, Clone, Copy)]
pub struct TypeId(pub usize);

pub const UNKNOWN_TYPE: TypeId = TypeId(usize::MAX);

pub struct Types {
    pub node_types: Vec<TypeId>,
}

pub struct Typechecker<'a> {
    // Immutable reference to a compiler after the name binding pass
    compiler: &'a Compiler,

    node_types: Vec<TypeId>,
}

impl<'a> Typechecker<'a> {
    pub fn new(compiler: &'a Compiler) -> Self {
        Self {
            compiler,
            node_types: vec![UNKNOWN_TYPE; compiler.ast_nodes.len()],
        }
    }

    pub fn to_types(self) -> Types {
        Types {
            node_types: self.node_types,
        }
    }

    pub fn print(&self) {
        let output = self.display_state();
        print!("{output}");
    }

    pub fn display_state(&self) -> String {
        let mut result = String::new();

        result.push_str("==== TYPES ====\n");

        result
    }

    pub fn typecheck(&mut self) {
        if !self.compiler.ast_nodes.is_empty() {
            let last = self.compiler.ast_nodes.len() - 1;
            let last_node_id = NodeId(last);
            self.typecheck_node(last_node_id)
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) {
        match self.compiler.ast_nodes[node_id.0] {
            _ => (),
        }
    }
}
