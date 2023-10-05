use std::path::Path;
use crate::{compiler::Compiler, parser::Parser};

fn evaluate_example(fname: &Path) -> String {
    let mut compiler = Compiler::new();
    let contents = std::fs::read(&fname).expect("We only run tests found by glob");

    let span_offset = compiler.span_offset();
    compiler.add_file(&fname.to_string_lossy(), &contents);

    let parser = Parser::new(compiler, span_offset);

    compiler = parser.parse();

    compiler.display_state()
}
