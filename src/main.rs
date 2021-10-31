use std::fs;
use std::io::Write;
use itertools::Itertools;

use structopt::StructOpt;

use racoon::compiler::{
    irbuilder::*,
    syntax::*,
};
use racoon::compiler::ir::value::constant::Constant;
use racoon::compiler::ir::value::ty::IrTy;
use racoon::compiler::ir::value::value::Operand::Global;
use racoon::compiler::irbuilder::typeck::TypeChecker;
use racoon::compiler::syntax::visitor::AstVisitorMut;

mod options;

fn main() {
    let options = options::Options::from_args();

    let input_file = options.input_file;
    let input = fs::read_to_string(input_file)
        .expect("Failed to read from input file");

    let lexer = lexer::Lexer::new(input.chars());
    // println!("{:?}", lexer::Lexer::new(input.chars()).into_iter().collect_vec());

    let mut parser = parser::Parser::new(lexer);
    // println!("{:?}", parser::Parser::new(lexer::Lexer::new(input.chars())).parse());
    let mut ast = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    let mut ty_checker = typeck::TypeChecker::new();
    if let Err(e) = ty_checker.visit_program(&mut ast) {
        println!("{:?}", e);
        return;
    };

    let mut ir_builder = irbuilder::IrBuilder::new();
    let ir = match ir_builder.visit(&ast) {
        Ok(_) => ir_builder.ctx.cur_module,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    let output_file = options.output_file;
    let mut output = Box::new(
        fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(output_file)
            .expect("Failed to open or create output file")
    );
    writeln!(output, "{}", ir).expect("Failed to write output file");
}
