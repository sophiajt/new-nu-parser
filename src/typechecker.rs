use crate::compiler::Compiler;
use crate::errors::{Severity, SourceError};
use crate::parser::{AstNode, NodeId};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub usize);

/// Unit type signifies "no type". For example, statemenets like let x = ... do not have any type.
pub const UNIT_TYPE: TypeId = TypeId(0);
pub const ANY_TYPE: TypeId = TypeId(1);
pub const NOTHING_TYPE: TypeId = TypeId(2);
pub const INT_TYPE: TypeId = TypeId(3);
pub const FLOAT_TYPE: TypeId = TypeId(4);
pub const BOOL_TYPE: TypeId = TypeId(5);
pub const STRING_TYPE: TypeId = TypeId(6);
pub const CLOSURE_TYPE: TypeId = TypeId(7);

pub const UNKNOWN_TYPE: TypeId = TypeId(usize::MAX);

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let msg = match *self {
            UNIT_TYPE => "()",
            ANY_TYPE => "any",
            NOTHING_TYPE => "nothing",
            INT_TYPE => "int",
            FLOAT_TYPE => "float",
            BOOL_TYPE => "bool",
            STRING_TYPE => "string",
            CLOSURE_TYPE => "closure",
            UNKNOWN_TYPE => "unknown",
            _ => "invalid",
        };

        write!(f, "{}", msg)
    }
}

pub struct Types {
    pub node_types: Vec<TypeId>,
    pub errors: Vec<SourceError>,
}

pub struct Typechecker<'a> {
    // Immutable reference to a compiler after the name binding pass
    compiler: &'a Compiler,

    /// Types of nodes. Each type in this vector matches a node in compiler.ast_nodes at the same position.
    pub node_types: Vec<TypeId>,
    /// Type of each Variable in compiler.variables, indexed by VarId
    pub variable_types: Vec<TypeId>,
    /// Errors encountered during type checking
    pub errors: Vec<SourceError>,
}

impl<'a> Typechecker<'a> {
    pub fn new(compiler: &'a Compiler) -> Self {
        Self {
            compiler,
            node_types: vec![UNKNOWN_TYPE; compiler.ast_nodes.len()],
            variable_types: vec![UNKNOWN_TYPE; compiler.variables.len()],
            errors: vec![],
        }
    }

    pub fn to_types(self) -> Types {
        Types {
            node_types: self.node_types,
            errors: self.errors,
        }
    }

    pub fn print(&self) {
        let output = self.display_state();
        print!("{output}");
    }

    pub fn display_state(&self) -> String {
        let mut result = String::new();

        result.push_str("==== TYPES ====\n");

        for (idx, node_type) in self.node_types.iter().enumerate() {
            result.push_str(&format!("{}: {}\n", idx, node_type,));
        }

        if !self.errors.is_empty() {
            result.push_str("==== TYPE ERRORS ====\n");
            for error in &self.errors {
                result.push_str(&format!(
                    "{:?} (NodeId {}): {}\n",
                    error.severity, error.node_id.0, error.message
                ));
            }
        }

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
            AstNode::Null => {
                self.node_types[node_id.0] = NOTHING_TYPE;
            }
            AstNode::Int => {
                self.node_types[node_id.0] = INT_TYPE;
            }
            AstNode::Float => {
                self.node_types[node_id.0] = FLOAT_TYPE;
            }
            AstNode::True | AstNode::False => {
                self.node_types[node_id.0] = BOOL_TYPE;
            }
            AstNode::String => {
                self.node_types[node_id.0] = STRING_TYPE;
            }
            AstNode::Type {
                name,
                params: _params,
                optional: _optional,
            } => {
                // TODO: Add support for compound and optional types
                self.node_types[node_id.0] = self.name_to_type(name);
            }
            AstNode::Block(block_id) => {
                let block = &self.compiler.blocks[block_id.0];

                for inner_node_id in &block.nodes {
                    self.typecheck_node(*inner_node_id);
                }

                self.node_types[node_id.0] = UNIT_TYPE;
            }
            AstNode::Closure { params, block } => {
                if let Some(params_node_id) = params {
                    self.typecheck_node(params_node_id);
                }

                self.typecheck_node(block);

                self.node_types[node_id.0] = CLOSURE_TYPE;
            }
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable: _,
            } => self.typecheck_let(variable_name, ty, initializer, node_id),
            AstNode::Variable => {
                let var_id = self
                    .compiler
                    .var_resolution
                    .get(&node_id)
                    .expect("missing resolved variable");

                self.node_types[node_id.0] = self.variable_types[var_id.0];
            }
            _ => self.error(
                format!(
                    "unsupported ast node '{:?}' in typechecker",
                    self.compiler.ast_nodes[node_id.0]
                ),
                node_id,
            ),
        }
    }

    pub fn typecheck_let(
        &mut self,
        variable_name: NodeId,
        ty: Option<NodeId>,
        initializer: NodeId,
        node_id: NodeId,
    ) {
        self.typecheck_node(initializer);

        if let Some(ty) = ty {
            self.typecheck_node(ty);

            // TODO make this a compatibility check rather than equality check
            if self.node_types[ty.0] != self.node_types[initializer.0] {
                self.error("initializer does not match declared type", initializer)
            }
        }

        let var_id = self
            .compiler
            .var_resolution
            .get(&variable_name)
            .expect("missing declared variable");

        let ty = if let Some(ty) = ty {
            self.node_types[ty.0]
        } else {
            self.node_types[initializer.0]
        };

        self.variable_types[var_id.0] = ty;
        self.node_types[variable_name.0] = ty;

        self.node_types[node_id.0] = UNIT_TYPE;
    }

    pub fn error(&mut self, msg: impl Into<String>, node_id: NodeId) {
        self.errors.push(SourceError {
            message: msg.into(),
            node_id,
            severity: Severity::Error,
        })
    }

    pub fn name_to_type(&mut self, name_node_id: NodeId) -> TypeId {
        let name = self.compiler.get_span_contents(name_node_id);

        // taken from parse_shape_name() in Nushell:
        match name {
            b"any" => ANY_TYPE,
            // b"binary" => SyntaxShape::Binary,
            // b"block" => {
            //     working_set.error(ParseError::LabeledErrorWithHelp {
            //         error: "Blocks are not support as first-class values".into(),
            //         label: "blocks are not supported as values".into(),
            //         help: "Use 'closure' instead of 'block'".into(),
            //         span,
            //     });
            //     SyntaxShape::Any
            // }
            b"bool" => BOOL_TYPE,
            // b"cell-path" => SyntaxShape::CellPath,
            b"closure" => CLOSURE_TYPE, //FIXME: Closures should have known output types
            // b"datetime" => SyntaxShape::DateTime,
            // b"directory" => SyntaxShape::Directory,
            // b"duration" => SyntaxShape::Duration,
            // b"error" => SyntaxShape::Error,
            b"float" => FLOAT_TYPE,
            // b"filesize" => SyntaxShape::Filesize,
            // b"glob" => SyntaxShape::GlobPattern,
            b"int" => INT_TYPE,
            // _ if bytes.starts_with(b"list") => parse_list_shape(working_set, bytes, span, use_loc),
            b"nothing" => NOTHING_TYPE,
            // b"number" => SyntaxShape::Number,
            // b"path" => SyntaxShape::Filepath,
            // b"range" => SyntaxShape::Range,
            // _ if bytes.starts_with(b"record") => {
            //     parse_collection_shape(working_set, bytes, span, use_loc)
            // }
            b"string" => STRING_TYPE,
            // _ if bytes.starts_with(b"table") => {
            //     parse_collection_shape(working_set, bytes, span, use_loc)
            // }
            _ => {
                // if bytes.contains(&b'@') {
                //     // type with completion
                // } else {
                self.error("unknown type", name_node_id);
                UNKNOWN_TYPE
                // }
            }
        }
    }
}
