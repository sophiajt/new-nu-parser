use crate::{compiler::Compiler, parser::AstNode, parser::Parser};
use std::path::Path;

fn evaluate_example(fname: &Path) -> String {
    let mut compiler = Compiler::new();
    let contents = std::fs::read(fname).expect("We only run tests found by glob");

    let span_offset = compiler.span_offset();
    compiler.add_file(&fname.to_string_lossy(), &contents);

    let parser = Parser::new(compiler, span_offset);

    compiler = parser.parse();

    compiler.display_state()
}

#[test]
fn test_node_output() {
    insta::glob!("../tests", "*.nu", |path| {
        insta::assert_snapshot!(evaluate_example(path));
    });
}

#[test]
fn nested_if() {
    let src = b"let x = 10; if true { let x = 20 }";

    let mut compiler = Compiler::new();
    let span_offset = compiler.span_offset();

    compiler.add_file("test", src);
    let parser = Parser::new(compiler, span_offset);
    compiler = parser.parse();

    let var_node_id = compiler
        .find_variable(b"x")
        .expect("did not find added var node id");

    let var_node = compiler.get_variable(var_node_id);
    assert!(matches!(var_node, AstNode::Variable));

    let var_span = compiler.get_span(var_node_id);
    assert_eq!(var_span.start, 4);
    assert_eq!(var_span.end, 5);

    let var_name = compiler.get_span_contents(var_span);
    assert_eq!(var_name, b"x");
}
