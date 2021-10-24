use structopt;
use structopt::StructOpt;

use racoon::compiler::{
    irbuilder::*,
    syntax::*,
};

mod options;

fn main() {
    let options = options::Options::from_args();

    let input_file = options.input_file;
    let input = std::fs::read_to_string(input_file)
        .expect("Unable to read from input file");

    let lexer = lexer::Lexer::new(input.chars());
    // println!("{:?}", lexer::Lexer::new(input.chars()).into_iter().collect::<Vec<_>>());

    let mut parser = parser::Parser::new(lexer);
    let ast = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    // println!("{:?}", parser::Parser::new(lexer::Lexer::new(input.chars())).parse());

    let mut ir_builder = irbuilder::IrBuilder::new();
    let ir = match ir_builder.visit(&ast) {
        Ok(_) => ir_builder.ctx.cur_module,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };
}
