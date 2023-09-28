use std::process::exit;

use compiler::Compiler;
use parser::Parser;

mod compiler;
mod errors;
mod parser;

fn main() {
    let mut compiler = Compiler::new();

    for fname in std::env::args().skip(1) {
        let contents = std::fs::read(&fname);

        let Ok(contents) = contents else {
            eprintln!("can't find {}", fname);
            exit(1);
        };

        let span_offset = compiler.span_offset();
        compiler.add_file(&fname, &contents);

        let parser = Parser::new(compiler, span_offset);

        compiler = parser.parse();
    }

    compiler.print();
}
