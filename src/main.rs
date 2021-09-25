#![feature(stdio_locked, new_uninit)]

use structopt::StructOpt;

use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::fs::File;
use std::path::PathBuf;

use parse::ParseStream;
use interpret::Interpreter;
use logic::Function;

pub mod parse;
pub mod interpret;
pub mod logic;

mod tests;

#[derive(Debug, StructOpt)]
#[structopt(name = "macron-rs", about = "A non-sandboxing Macron interpreter written in Rust.")]
pub struct Options {
	/// Only interpret intrinsics, `__expander_end` is not treated as special and will error.
	#[structopt(short)]
	pub intrinsics: bool,
	/// Input file, stdin if not present.
	#[structopt(parse(from_os_str))]
	pub input: Option<PathBuf>
}

fn main() {
	let args = Options::from_args();

	match &args.input {
		Some(x) => {
			run(BufReader::new(File::open(x).unwrap()), args);
		},
		None => {
			run(io::stdin_locked(), args);
		}
	};
}

fn run<R: BufRead>(input: R, args: Options) {
	let parsed = ParseStream::new(input, args.intrinsics);
	let mut interpreter = Interpreter::new();

	let mut func = Function::new();

	for inst in parsed {
		let inst = inst.expect("syntax error");
		if !interpreter.add_to_func(inst, &mut func) { break; }
	}
	unsafe {
		interpreter.interpret(&func);
	}
}