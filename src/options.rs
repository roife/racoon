use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "racoon",
            about = "An implementation for mini-SysY compiler in Rust",
            author = "roife <roifewu@gmail.com>")]
pub struct Options {
    #[structopt(parse(from_os_str))]
    pub input_file: PathBuf,

    #[structopt(short, long, default_value = "a.out", parse(from_os_str))]
    pub output_file: PathBuf,

    #[structopt(long, default_value = "ir")]
    pub emit: EmitOption,

    #[structopt(short, long)]
    pub passes: Option<Vec<String>>,

    #[structopt(short, long, default_value = "warn")]
    pub log_level: log::LevelFilter,
}

#[derive(Debug, Eq, PartialEq)]
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