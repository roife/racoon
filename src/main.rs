use structopt;
use structopt::StructOpt;
use rust_mini_sysy::compiler::syntax::{lexer, token};

mod options;

fn main() {
    let options = options::Options::from_args();

    let input_file = options.input_file;
    let input = std::fs::read_to_string(input_file)
        .expect("Unable to read from input file");

    let token_iter = lexer::Lexer::new("int".chars()).into_iter();
    println!("{:?}", token_iter.collect::<Vec<token::Token>>());
}
