use std::convert::{Infallible, TryFrom, TryInto};
use std::io;
use std::io::BufRead;
use std::fmt;
use std::fmt::{Display, LowerHex, Formatter};

#[derive(Debug, Clone, Copy)]
pub enum Register { Sp, Bp }

#[derive(Debug, Clone)]
pub enum Lvalue {
	Mem(Box<Rvalue>, u8),
	Var(u64),
	Reg(Register)
}

impl Lvalue {
	pub fn inst_display(&self) -> InstDisplayer<&Self> { InstDisplayer(self) }
}

#[derive(Debug, Clone)]
pub enum Rvalue {
	Imm(u64),
	L(Lvalue)
}

impl Rvalue {
	pub fn inst_display(&self) -> InstDisplayer<&Self> { InstDisplayer(self) }
}

impl TryFrom<Rvalue> for Lvalue {
	type Error = Error;
	fn try_from(x: Rvalue) -> Result<Self> {
		if let Rvalue::L(l) = x { Ok(l) } else { Err(Error::WrongArg(x)) }
	}
}

impl TryFrom<Rvalue> for u64 {
	type Error = Error;
	fn try_from(x: Rvalue) -> Result<Self> {
		if let Rvalue::Imm(l) = x { Ok(l) } else { Err(Error::WrongArg(x)) }
	}
}

pub fn parse_arg(arg: &str) -> Result<Rvalue> {
	parse_arg_inner(arg, false)
}

fn parse_arg_inner(arg: &str, mem: bool) -> Result<Rvalue> {
	let mut chrs = arg.chars();
	let n = if let Some(x) = chrs.next() { x } else { return Err(Error::InvalidArg(String::new())); };
	Ok(if n == 'v' {
		let n = &arg[1..];
		if let Ok(x) = u64::from_str_radix(n, 16) {
			Rvalue::L(Lvalue::Var(x))
		} else {
			return Err(Error::InvalidVar(n.into()));
		}
	} else if n == '*' {
		if mem {
			return Err(Error::NestedMem(arg.into()));
		}
		let size = if let Some(x) = chrs.next() { match x {
			'1' => 1,
			'2' => 2,
			'4' => 4,
			'8' => 8,
			_ => return Err(Error::InvalidMem(arg.into()))
		} } else { return Err(Error::InvalidMem(arg.into()))};
		if chrs.next() != Some('|') { return Err(Error::InvalidMem(arg.into())); }
		let d = parse_arg_inner(chrs.as_str(), true)?;
		Rvalue::L(Lvalue::Mem(
			Box::new(d),
			size
		))
	} else if arg == "s" {
		Rvalue::L(Lvalue::Reg(Register::Sp))
	} else if arg == "b" {
		Rvalue::L(Lvalue::Reg(Register::Bp))
	} else {
		if let Ok(x) = u64::from_str_radix(arg, 16) {
			Rvalue::Imm(x)
		} else {
			return Err(Error::InvalidArg(arg.into()));
		}
	})
}

/// Displays an instruction or instruction element as it would be displayed in source code.
pub trait InstDisplay {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result;
}

impl<T: InstDisplay> InstDisplay for &T {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		InstDisplay::fmt(*self, f)
	}
}

macro_rules! impl_int_instdisplay {
	($($t:ty)*) => {
		$(
			impl InstDisplay for $t {
				fn fmt(&self, f: &mut Formatter) -> fmt::Result {
					LowerHex::fmt(self, f)
				}
			}
			impl InstDisplay for [$t] {
				fn fmt(&self, f: &mut Formatter) -> fmt::Result {
					if let Some(x) = self.get(0) {
						InstDisplay::fmt(x, f)?;
						for r in &self[1..] {
							write!(f, " ")?;
							InstDisplay::fmt(r, f)?;
						}
					}
					Ok(())
				}
			}
			impl InstDisplay for Vec<$t> {
				fn fmt(&self, f: &mut Formatter) -> fmt::Result {
					InstDisplay::fmt(&**self, f)
				}
			}
		)*
	};
}

impl_int_instdisplay! {
	i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 isize usize
}

impl InstDisplay for Lvalue {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Mem(x, s) => {
				write!(f, "*")?;
				InstDisplay::fmt(s, f)?;
				write!(f, "|")?;
				InstDisplay::fmt(&**x, f)
			},
			Self::Var(x) => {
				write!(f, "v")?;
				InstDisplay::fmt(x, f)
			},
			Self::Reg(Register::Sp) => write!(f, "s"),
			Self::Reg(Register::Bp) => write!(f, "b")
		}
	}
}

impl InstDisplay for Rvalue {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Imm(x) => InstDisplay::fmt(x, f),
			Self::L(x) => InstDisplay::fmt(x, f)
		}
	}
}

/// Displays a value with its `InstDisplay` impl.
#[repr(transparent)]
pub struct InstDisplayer<T: InstDisplay>(pub T);

impl<T: InstDisplay> Display for InstDisplayer<T> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		InstDisplay::fmt(&self.0, f)
	}
}

trait ParseFromArgs: Sized {
	fn parse<I: Iterator<Item = Result<Rvalue>>>(args: &mut I) -> Option<Result<Self>>;
}

trait IntoError {
	fn into_error(self) -> Error;
}

impl IntoError for Error {
	fn into_error(self) -> Error { self }
}

impl IntoError for Infallible {
	fn into_error(self) -> Error { match self {} }
}

impl<T: TryFrom<Rvalue>> ParseFromArgs for T where T::Error: IntoError {
	fn parse<I: Iterator<Item = Result<Rvalue>>>(args: &mut I) -> Option<Result<Self>> {
		match args.next()? {
			Ok(x) => Some(x.try_into().map_err(|x: T::Error| x.into_error())),
			Err(e) => Some(Err(e))
		}
	}
}

impl<T: ParseFromArgs> ParseFromArgs for Vec<T> {
	fn parse<I: Iterator<Item = Result<Rvalue>>>(args: &mut I) -> Option<Result<Self>> {
		let mut v = Vec::new();
		while let Some(x) = T::parse(args) {
			match x {
				Ok(r) => v.push(r),
				Err(e) => return Some(Err(e))
			}
		}
		Some(Ok(v))
	}
}

macro_rules! def_insts {
	($(
		$inst:literal $(($parse:expr))? => $name:ident $(($($tn:ident: $t:ty),* $(,)?))?
	),* $(,)?) => {
		#[derive(Debug, Clone)]
		pub enum Instruction {
			$(
				#[doc = concat!("The `", $inst $($(, " ", stringify!($tn), ":", stringify!($t))*)?, "` instruction")]
				$name $(($($t),*))?
			),*
		}

		impl InstDisplay for Instruction {
			fn fmt(&self, f: &mut Formatter) -> fmt::Result {
				match self {
					$(Self::$name$(($($tn),*))? => {
						write!(f, $inst)?;
						$($(
							write!(f, " ")?;
							InstDisplay::fmt($tn, f)?;
						)*)?
						Ok(())
					})*
				}
			}
		}

		impl Instruction {
			#[allow(unused_variables)]
			pub fn name(&self) -> &'static str {
				match self {
					$(Self::$name$(($($tn),*))? => $inst),*
				}
			}

			pub fn parse(inst: &str, mut args: impl Iterator<Item = Result<Rvalue>>) -> Result<Self> {
				Ok(match inst {
					$($inst $(if $parse)? => {
						$($(
							let $tn = match <$t as ParseFromArgs>::parse(&mut args) {
								Some(x) => x?,
								None => return Err(Error::NotEnoughArgs)
							};
						)*)?
						if args.next().is_some() { return Err(Error::TooManyArgs); }
						Self::$name$(($($tn),*))?
					})*
					x => return Err(Error::InvalidInstruction(x.into()))
				})
			}
		}
	};
}

def_insts! {
	"allocvars" => Allocvars(prev_alloc: u64, var_sizes: Vec<u64>),

	"add" => Add(result: Lvalue, x: Lvalue, y: Rvalue),
	"nand" => Nand(result: Lvalue, x: Lvalue, y: Rvalue),
	"shiftl" => Shiftl(result: Lvalue, x: Rvalue, amt: Rvalue),
	"shiftrz" => Shiftrz(result: Lvalue, x: Rvalue, amt: Rvalue),
	"shiftrs" => Shiftrs(result: Lvalue, x: Rvalue, amt: Rvalue),

	"blockaddr" => BlockAddr(result: Lvalue, block_id: u64),

	// Block Branches

	"b" => Branch(to: u64),
	"baddr" => BranchAddr(addr: Rvalue),
	"beq" => BranchEq(t: u64, f: u64, x: Lvalue, y: Rvalue),
	"bgt" => BranchGt(t: u64, f: u64, x: Rvalue, y: Rvalue),
	"bab" => BranchAb(t: u64, f: u64, x: Rvalue, y: Rvalue),

	"__test_eq" => TestEq(x: Rvalue, y: Rvalue),
	"__tmp_printstr" => PrintStr(ptr: Rvalue, len: Rvalue),
	"__tmp_printnum" => PrintNum(v: Rvalue),
	"__tmp_printbytes" => PrintBytes(ptr: Rvalue, len: Rvalue),

	"__expander_end" (false) => ExpanderEnd
}

impl Instruction {
	pub fn inst_display(&self) -> InstDisplayer<&Self> { InstDisplayer(self) }
}

#[derive(Debug)]
pub enum Error {
	/// Invalid instruction.
	InvalidInstruction(String),
	/// For a variable `vX`, `X` must be a u64. The argument is `X`.
	InvalidVar(String),
	/// Invalid immediate value.
	InvalidImm(String),
	/// Invalid memory argument. Memory arguments are formed as follows: `*X|Y` where `X` is the size: 1, 2, 4, or 8, and `Y` is any other operand.
	InvalidMem(String),
	/// Invalid argument, should be a variable, dereference, register, or immediate.
	InvalidArg(String),
	/// Not enough arguments for instruction.
	NotEnoughArgs,
	/// Too many arguments for instruction.
	TooManyArgs,
	/// Wrong type of argument.
	WrongArg(Rvalue),
	/// Nested `Mem` type argument. (i.e. double dereference).
	NestedMem(String),
	/// An IO error occurred.
	Io(io::Error)
}

impl From<io::Error> for Error {
	fn from(x: io::Error) -> Self {
		Self::Io(x)
	}
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct ParseStream<T: BufRead> {
	/// The input stream.
	pub input: T,
	/// Only parse intrinsics?
	pub intrinsics: bool
}

impl<T: BufRead> Iterator for ParseStream<T> {
	type Item = Result<Instruction>;
	
	fn next(&mut self) -> Option<Result<Instruction>> {
		let mut line = String::new();
		match self.input.read_line(&mut line) {
			Ok(0) => { return None; },
			Ok(_) => {},
			Err(x) => { return Some(Err(x.into())); },
		};
		let line = line.trim();

		if line.starts_with('#') { return self.next(); }

		if !self.intrinsics && line == "__expander_end" {
			return Some(Ok(Instruction::ExpanderEnd));
		}

		let mut args = line.split_whitespace();
		let inst = if let Some(x) = args.next() { x } else { return self.next(); };
		let args = args.map(parse_arg);

		Some(Instruction::parse(inst, args))
	}
}

impl<T: BufRead> ParseStream<T> {
	pub fn new(input: T, intrinsics: bool) -> Self {
		Self { input, intrinsics }
	}

	pub fn into_inner(self) -> T { self.input }
}