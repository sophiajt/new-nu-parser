use new_nu_parser::compiler::Compiler;
use new_nu_parser::parser::Parser;

// Allows profiling of allocations and deallocations.
#[global_allocator]
static ALLOC: divan::AllocProfiler = divan::AllocProfiler::system();

fn main() {
    divan::main();
}

/// Parses a largly nested record.
#[divan::bench]
fn parse_record() {
    let mut compiler = Compiler::new();

    let content = include_bytes!("record.nu");
    compiler.add_file(&"record.nu", content);

    let parser = Parser::new(compiler, 0);

    parser.parse();
}

/// Parses a small table with various types.
#[divan::bench]
fn parse_table() {
    let mut compiler = Compiler::new();

    let content = include_bytes!("table.nu");
    compiler.add_file(&"table.nu", content);

    let parser = Parser::new(compiler, 0);

    parser.parse();
}
