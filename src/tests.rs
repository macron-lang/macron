#![cfg(test)]

macro_rules! t {
	($(#[$($meta:meta)*])* $n:ident) => {
		#[test]
		$(#[$($meta)*])*
		fn $n() {
			let src = include_bytes!(concat!("tests/", stringify!($n), ".mnl"));
			let cursor = ::std::io::Cursor::new(src);
			let parse = $crate::parse::ParseStream::new(cursor, false);
			let mut interpreter = $crate::interpret::Interpreter::new();

			let mut func = $crate::logic::Function::new();

			for inst in parse {
				let inst = inst.unwrap();
				if !interpreter.add_to_func(inst, &mut func) { panic!("`add_to_func` returned false in test."); }
			}
			unsafe {
				interpreter.interpret(&func);
			}
		}
	};
}

t!(sanity);
t!(#[should_panic] insanity);

t!(nand);
t!(stack);
t!(allocvars);
t!(shift);