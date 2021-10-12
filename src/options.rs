use std::path::PathBuf;
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

    #[structopt(short, long)]
    pub passes: Option<Vec<String>>,

    #[structopt(short, long, default_value = "warn")]
    pub log_level: log::LevelFilter,
}
