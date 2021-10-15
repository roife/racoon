use structopt;
use structopt::StructOpt;
use racoon::compiler::syntax::{
    lexer,
    parser,
};

mod options;

fn main() {
    let options = options::Options::from_args();

    let input_file = options.input_file;
    let input = std::fs::read_to_string(input_file)
        .expect("Unable to read from input file");

    // let lexer = lexer::Lexer::new(input.chars());

    println!("{:?}", lexer::Lexer::new(input.chars()).into_iter().collect::<Vec<_>>());
    // let ast = parser::Parser::new(lexer).parse();
    println!("{:?}", parser::Parser::new(lexer::Lexer::new(input.chars())).parse());
}
