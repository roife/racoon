use std::path::PathBuf;
use std::str::FromStr;
use clap::Parser;

#[derive(Parser, Debug)]
#[structopt(name = "racoon",
            about = "An implementation for mini-SysY compiler in Rust",
            author = "roife <roifewu@gmail.com>")]
pub struct Options {
    pub input_file: PathBuf,

    #[arg(short, long, default_value = "a.out")]
    pub output_file: PathBuf,

    #[arg(value_enum, long="emit", default_value = "ir")]
    pub emit_option: EmitOption,

    #[arg(short, long)]
    pub passes: Option<Vec<String>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum EmitOption {
    Ir,
}

impl FromStr for EmitOption {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ir" => Ok(EmitOption::Ir),
            _ => Err("Allowed emit options: ir"),
        }
    }
}
