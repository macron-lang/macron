use crate::parse::{Instruction, Lvalue, Rvalue};

#[derive(Debug, Clone)]
pub enum Branch {
	B(u64),
	Baddr(Rvalue),
	Beq(u64, u64, Lvalue, Rvalue),
	Bgt(u64, u64, Rvalue, Rvalue),
	Bab(u64, u64, Rvalue, Rvalue),
}

impl Branch {
	pub fn from_inst(inst: &Instruction) -> Option<Self> {
		Some(match inst {
			Instruction::Branch(to) => Self::B(*to),
			Instruction::BranchAddr(addr) => Self::Baddr(addr.clone()),
			Instruction::BranchEq(t, f, x, y) => Self::Beq(*t, *f, x.clone(), y.clone()),
			Instruction::BranchGt(t, f, x, y) => Self::Bgt(*t, *f, x.clone(), y.clone()),
			Instruction::BranchAb(t, f, x, y) => Self::Bab(*t, *f, x.clone(), y.clone()),
			_ => return None
		})
	}
}

#[derive(Debug, Clone, Default)]
pub struct Block {
 	pub insts: Vec<Instruction>,
	pub branch: Option<Branch>
}

impl Block {
	pub fn new() -> Self { Self::default() }
}

#[derive(Debug, Clone, Default)]
pub struct Function {
	pub blocks: Vec<usize>
}

impl Function {
	pub fn new() -> Self { Self::default() }
}