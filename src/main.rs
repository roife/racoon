use std::fs::{self, File};
use std::io::Write;
use clap::Parser;

use racoon::compiler::{
    ir_builder::*,
    syntax::{*, visitor::AstVisitorMut},
};

mod options;

fn main() {
    let options = options::Options::parse();

    let input_file = options.input_file;
    let input = fs::read_to_string(input_file)
        .expect("Failed to read from input file");

    let lexer = lexer::Lexer::new(input.chars());
    let mut parser = parser::Parser::new(lexer);

    let mut ast = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    let mut ty_checker = type_checker::TypeChecker::new();
    if let Err(e) = ty_checker.visit_program(&mut ast) {
        println!("{:?}", e);
        return;
    };

    let mut ir_builder = ir_builder::IrBuilder::new();
    let ir = match ir_builder.visit(&ast) {
        Ok(_) => ir_builder.ctx.cur_module,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    let output_file = options.output_file;
    let mut output = File::create(output_file)
        .expect("Failed to open or create output file");
    writeln!(output, "{}", ir).expect("Failed to write output file");
}
