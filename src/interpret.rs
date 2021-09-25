use std::alloc;
use std::alloc::Layout;

use crate::logic::*;
use crate::parse::{Lvalue, Rvalue, Register, Instruction};

pub const STACK_SIZE: usize = 0x10000;

#[derive(Debug, Clone, Copy)]
pub struct Allocvars {
	pub size: usize
}

#[derive(Debug, Clone, Copy)]
pub struct Var {
	pub bp_offset: usize,
	pub size: u8
}

#[derive(Debug)]
pub struct Interpreter {
	stack: *mut u64,
	pub sp: u64,
	pub bp: u64,
	pub allocvars: Vec<Allocvars>,
	pub vars: Vec<Var>,
	pub blocks: Vec<Block>,
	pub expander_end: bool
}

impl Drop for Interpreter {
	fn drop(&mut self) {
		unsafe {
			alloc::dealloc(self.stack as _, Layout::array::<u64>(STACK_SIZE).unwrap());
		}
	}
}

pub fn above(x: u64, y: u64, size: u8) -> bool {
	match size {
		1 => x as i8 > y as i8,
		2 => x as i16 > y as i16,
		4 => x as i32 > y as i32,
		8 => x as i64 > y as i64,
		_ => panic!("invalid size given: {}", size)
	}
}

pub fn shiftrs(x: u64, y: u64, size: u8) -> u64 {
	match size {
		1 => (x as i8 >> y) as u64,
		2 => (x as i16 >> y) as u64,
		4 => (x as i32 >> y) as u64,
		8 => (x as i64 >> y) as u64,
		_ => panic!("invalid size given: {}", size)
	}
}

impl Interpreter {
	pub fn new() -> Self {
		let stack = unsafe { alloc::alloc_zeroed(Layout::array::<u64>(STACK_SIZE).unwrap()) as *mut u64 };
		let sp = unsafe { stack.cast::<u8>().offset(STACK_SIZE as _) as u64 };
		Self {
			stack,
			sp,
			bp: sp,
			allocvars: Vec::new(),
			vars: Vec::new(),
			blocks: Vec::new(),
			expander_end: false
		}
	}

	pub fn r_size(&self, x: &Rvalue, y: &Rvalue) -> u8 {
		if let Rvalue::L(l) = x { return self.l_size(l); }
		if let Rvalue::L(l) = y { return self.l_size(l); }
		8
	}

	pub fn l_size(&self, x: &Lvalue) -> u8 {
		match x {
			Lvalue::Mem(_, s) => *s,
			Lvalue::Var(v) => self.vars[*v as usize].size,
			Lvalue::Reg(_) => 8
		}
	}

	pub unsafe fn get_r(&self, x: &Rvalue) -> u64 {
		match x {
			Rvalue::Imm(v) => *v,
			Rvalue::L(l) => self.get_l(l)
		}
	}

	pub unsafe fn get_l(&self, x: &Lvalue) -> u64 {
		match x {
			Lvalue::Mem(r, size) => {
				let size = *size;
				let ptr = self.get_r(&**r) as *const u8;
				let mut b = [0; 8];
				assert!(size <= 8);
				std::ptr::copy_nonoverlapping(ptr, b.as_mut_ptr(), size as _);
				u64::from_le_bytes(b)
			},
			Lvalue::Var(v) => self.get_var(self.vars[*v as usize]),
			Lvalue::Reg(Register::Sp) => self.sp,
			Lvalue::Reg(Register::Bp) => self.bp
		}
	}

	pub unsafe fn set(&mut self, l: &Lvalue, val: u64) {
		match l {
			Lvalue::Mem(r, size) => {
				let size = *size;
				let ptr = self.get_r(&**r) as *mut u8;
				let b = val.to_le_bytes();
				assert!(size <= 8);
				std::ptr::copy_nonoverlapping(b.as_ptr(), ptr, size as _);
			},
			Lvalue::Var(v) => self.set_var(self.vars[*v as usize], val),
			Lvalue::Reg(Register::Sp) => self.sp = val,
			Lvalue::Reg(Register::Bp) => self.bp = val
		}
	}

	pub unsafe fn get_var(&self, v: Var) -> u64 {
		let ptr = (self.bp - v.bp_offset as u64) as *const u8;
		let mut b = [0; 8];
		assert!(v.size <= 8);
		std::ptr::copy_nonoverlapping(ptr, b.as_mut_ptr(), v.size as usize);
		u64::from_le_bytes(b)
	}

	pub unsafe fn set_var(&mut self, v: Var, x: u64) {
		let ptr = (self.bp - v.bp_offset as u64) as *mut u8;
		let b = x.to_le_bytes();
		assert!(v.size <= 8);
		std::ptr::copy_nonoverlapping(b.as_ptr(), ptr, v.size as usize);
	}

	/// # Safety
	/// The provided instruction can cause all sorts of UB, this does not sandbox.
	pub unsafe fn interpret_inst(&mut self, inst: &Instruction) {
		match inst {
			Instruction::Allocvars(_p, av) => {
				self.sp -= av.iter().sum::<u64>();
			},
			Instruction::Add(r, x, y) => {
				self.set(r, self.get_l(x).wrapping_add(self.get_r(y)));
			},
			Instruction::Nand(r, x, y) => {
				self.set(r, !(self.get_l(x) & self.get_r(y)));
			},
			Instruction::Shiftl(r, x, amt) => {
				self.set(r, self.get_r(x) << self.get_r(amt));
			},
			Instruction::Shiftrs(r, x, amt) => {
				let size = self.r_size(x, amt);
				self.set(r, shiftrs(self.get_r(x), self.get_r(amt), size));
			},
			Instruction::Shiftrz(r, x, amt) => {
				self.set(r, self.get_r(x) >> self.get_r(amt));
			},
			Instruction::BlockAddr(r, l) => {
				self.set(r, *l);
			},
			Instruction::TestEq(x, y) => {
				let xv = self.get_r(x);
				let yv = self.get_r(y);
				if xv != yv {
					panic!("equality test failed: `{}` = {}, `{}` = {}", x.inst_display(), xv, y.inst_display(), yv);
				}
			}
			Instruction::PrintStr(ptr, len) => {
				println!("DEBUG OUTPUT (str):");
				let p = self.get_r(ptr) as *const u8;
				let l = self.get_r(len) as usize;
				println!("{}", std::str::from_utf8(std::slice::from_raw_parts(p, l)).unwrap());
				println!("-----------");
			},
			Instruction::PrintNum(n) => {
				println!("DEBUG OUTPUT (num):");
				let v = self.get_r(n);
				println!("Decimal: {} | Hex: {:#x}", v, v);
			}
			Instruction::PrintBytes(ptr, len) => {
				println!("DEBUG OUTPUT (bytes):");
				let p = self.get_r(ptr) as *const u8;
				let l = self.get_r(len) as usize;
				println!("{:?}", std::slice::from_raw_parts(p, l));
				println!("-----------");
			},
			_ => panic!("encountered `{}` instruction while interpreting.", inst.name())
		}
	}

	/// # Safety
	/// The branch can include arbitrary memory reads and other UB-inducing behavior.
	pub unsafe fn resolve_branch(&self, branch: &Branch) -> u64 {
		match branch {
			Branch::B(to) => *to,
			Branch::Baddr(to) => self.get_r(to),
			Branch::Beq(t, f, x, y) => if self.get_l(x) == self.get_r(y) { *t } else { *f },
			Branch::Bgt(t, f, x, y) => if self.get_r(x) > self.get_r(y) { *t } else { *f },
			Branch::Bab(t, f, x, y) => if above(self.get_r(x), self.get_r(y), self.r_size(x, y)) { *t } else { *f }
		}
	}

	/// Add some code into the interpreter. A return value of `false` means to
	/// stop calling this method as the end of the expander has been reached.
	/// Instead, you should call `interpret_with_src`. Calling this method
	/// after a return value of `false` will keep returning `false` and do nothing.
	///
	/// In addition to calling this function, you should probably add the instruction
	/// to a `Block` or `Function` so it can be interpreted.
	pub fn add(&mut self, inst: &Instruction) -> bool {
		if self.expander_end { return false; }
	
		match &inst {
			Instruction::Allocvars(_prev, sizes) => {
				let mut size = 0;
				self.vars.reserve(sizes.len());
				for s in sizes {
					// We add to `size` first so that the bp offset doesn't include
					// bp itself.
					size += *s as usize;
					self.vars.push(Var {
						size: *s as _,
						bp_offset: size
					});
				}
				self.allocvars.push(Allocvars { size });
			},
			Instruction::ExpanderEnd => {
				self.expander_end = true;
				return false;
			},
			_ => {}
		}
		true
	}

	pub fn add_to_func(&mut self, inst: Instruction, func: &mut Function) -> bool {
		if !self.add(&inst) { return false; }

		let block = match func.blocks.last().copied() {
			Some(b) if self.blocks[b as usize].branch.is_none() => &mut self.blocks[b as usize],
			_ => {
				let id = self.blocks.len();
				self.blocks.push(Block::new());
				func.blocks.push(id);
				self.blocks.last_mut().unwrap()
			}
		};

		if let Some(b) = Branch::from_inst(&inst) {
			block.branch = Some(b);
		} else {
			block.insts.push(inst);
		}

		true
	}

	/// # Safety
	/// The provided function can cause all sorts of UB, this doesn't sandbox.
	pub unsafe fn interpret(&mut self, f: &Function) {
		let mut block = if let Some(x) = f.blocks.first() {
			self.blocks[*x].clone()
		} else { return; };

		loop {
			for inst in &block.insts {
				self.interpret_inst(inst);
			}
			if let Some(b) = &block.branch {
				let id = self.resolve_branch(b);
				if id == u64::MAX { break; }
				block = self.blocks[id as usize].clone();
			} else {
				break;
			}
		}
	}
}