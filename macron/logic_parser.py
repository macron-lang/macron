import sys

import logic


class ParsingError(Exception):
    pass

def parse_arg(n):
    try:
        n, size = n.split("_", 1)
        size = int(size)
    except ValueError:
        raise ParsingError("invalid size")
    try:
        return logic.Immediate(size, int(n))
    except ValueError:
        pass
    if n.startswith("r"):
        return logic.Register(size, int(n[1:]))
    elif n.startswith("[") and n.endswith("]"):
        offset = logic.Immediate(64, 0)
        reg = None
        reg2 = None
        for operand in n.removeprefix("[").removesuffix("]").split("+"):
            if "*" in operand:
                if reg2:
                    raise ParsingError("multiple registers multiplied by cofficients in dereference")
                x, y = operand.split("*")
                val = None
                coeff = None
                for v in (x, y):
                    v = parse_arg(v.strip())
                    if isinstance(v, logic.Immediate):
                        if coeff:
                            raise ParsingError("???")
                        coeff = v.val
                    elif isinstance(v, logic.Register):
                        if val:
                            raise ParsingError("???")
                        val = v
                    else:
                        raise ParsingError("what *are* you doing")
                # if we're here, these are definitely not None, but the typechecker is too stupid to know that
                assert val is not None and coeff is not None
                reg2 = (val, coeff)
                continue
            v = parse_arg(operand.strip())
            if isinstance(v, logic.Immediate):
                offset.val += v.val
            elif isinstance(v, logic.Register):
                if reg:
                    if reg2:
                        raise ParsingError("too many registers in dereference")
                    reg2 = (v, 1)
                reg = v
            else:
                raise ParsingError("you can't put a dereference inside a dereference bro")
        return logic.Address(size, offset, reg, reg2)


def parse_logic(lines):
    insts = []
    for line in lines:
        try:
            inst_name, args_string = line.strip().split(" ", 1)
        except ValueError:
            inst_name = line.strip()
            args_string = ""
        arg_strings = [x.strip() for x in args_string.split(",") if x]

        try:
            inst = logic.ops[inst_name]
        except KeyError:
            raise ParsingError(f"unknown opcode '{inst_name}'")

        args = [parse_arg(x) for x in arg_strings]
        insts.append(logic.Inst(inst, *args))
    return insts


if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        print(parse_logic(f))
