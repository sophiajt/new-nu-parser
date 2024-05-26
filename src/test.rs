use crate::resolver::Resolver;
use crate::{compiler::Compiler, parser::Parser};
use std::path::Path;

fn evaluate_example(fname: &Path) -> String {
    let mut compiler = Compiler::new();
    let contents = std::fs::read(fname).expect("We only run tests found by glob");

    // Homogenize line endings from CRLF to LF to make sure tests work both on Windows and Linux/macOS
    let contents = String::from_utf8(contents)
        .unwrap()
        .replace("\r\n", "\n")
        .as_bytes()
        .to_owned();

    let span_offset = compiler.span_offset();
    compiler.add_file(&fname.to_string_lossy(), &contents);

    let parser = Parser::new(compiler, span_offset);
    compiler = parser.parse();

    let mut result = compiler.display_state();

    if !compiler.errors.is_empty() {
        return result;
    }

    let mut resolver = Resolver::new(&compiler);
    resolver.resolve();
    result.push_str(&resolver.display_state());

    compiler.merge_name_bindings(resolver.to_name_bindings());

    result
}

#[test]
fn test_node_output() {
    insta::glob!("../tests", "*.nu", |path| {
        insta::assert_snapshot!(evaluate_example(path));
    });
}
