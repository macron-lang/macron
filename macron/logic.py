import itertools

from dataclasses import dataclass
from typing import Optional, Tuple, Type


class InvalidLogicError(Exception):
    pass

class Argument:
    pass

@dataclass
class Immediate(Argument):
    size: int
    val: int

@dataclass
class Register(Argument):
    size: int
    idx: int

@dataclass
class Address(Argument):
    size: int
    offset: Immediate
    reg: Optional[Register]
    reg2: Optional[Tuple[Register, int]]

lvalues = ["reg", "mem"]
rvalues = ["imm"] + lvalues
sizes = ["8", "16", "32", "64", "128"]
m = {
    "T": rvalues,
    "U": rvalues,
    "V": rvalues,
    "B": sizes,
    "C": sizes,
    "L": lvalues,
    "M": lvalues,
    "F": ["32", "64"],
}

class Op:
    args: dict[str, str]
    def __repr__(self):
        return f"<{type(self).__name__} {', '.join(f'{name}: {type}' for name, type in self.args.items())}>"

ops = {}
def make_op(class_name: str, s: str) -> Type[Op]:
    name, _, arg_string = s.partition(" ")

    vs = []
    for char in name:
        if char in m:
            vs.append([(char, x) for x in m[char]])

    class OpGen(Op):
        def __init__(self, **args: str):
            self.args = args
    OpGen.__name__ = class_name

    for o in itertools.product(*vs):
        name_temp = name
        arg_string_temp = arg_string
        for char, p in o:
            name_temp = name_temp.replace(char, p)
            arg_string_temp = arg_string_temp.replace(char, p)
        args = {(sp := x.split(": "))[0]: sp[1] for x in arg_string_temp.split(", ")} if arg_string_temp else {}
        ops[name_temp] = OpGen(**args)

    return OpGen


def verify_type(t, v):
    if t.endswith("..."):
        return [verify_type(t.removesuffix("..."), x) for x in v]

    assert isinstance(v, {
        "reg": Register,
        "mem": Address,
        "imm": Immediate,
        "***": Argument,
    }[t[:3]]) and (t[3] == "*" or v.size == int(t[3:])), "invalid operand"

    # assert invariants
    if t.startswith("imm"):
        assert abs(v.val) < 2**(v.size-1), "immediate is too large"
    elif t.startswith("reg"):
        assert 1 <= v.idx <= 8, "register index out of range"
    elif t.startswith("mem"):
        assert 0 <= v.offset.val < 2**64, "memory offset out of range"
        assert not v.reg or 1 <= v.reg <= 8, "register index out of range"
        assert not v.reg2 or 1 <= v.reg2[0] <= 8, "register index out of range"
        assert not v.reg2 or v.reg2[1] in (1, 2, 4, 8), "invalid coefficient"

class Inst:
    def __init__(self, op, *args):
        self.op = op
        assert len(args) == len(op.args), "wrong number of arguments"
        for arg, (name, t) in zip(args, op.args.items()):
            verify_type(t, arg)
            setattr(self, name, arg)


Add = make_op("Add", "addB_L_U x: LB, y: UB")
Sub = make_op("Sub", "subB_L_U x: LB, y: UB")
Umul = make_op("Umul", "umulB_L_U x: LB, y: UB")
Imul = make_op("Imul", "imulB_L_U x: LB, y: UB")
Udiv = make_op("Udiv", "udivB_L_U x: LB, y: UB")
Idiv = make_op("Idiv", "idivB_L_U x: LB, y: UB")

BitAnd = make_op("BitAnd", "bitandB_L_U x: LB, y: UB")
BitOr = make_op("BitOr", "bitorB_L_U x: LB, y: UB")
BitNot = make_op("BitNot", "bitnotB_L x: LB")

Mov = make_op("Mov", "movB_L_U dst: LB, src: UB")
MovZero = make_op("MovZero", "movzx_LB_UC x: LB, y: UC")
MovSign = make_op("MovSign", "movsx_LB_UC x: LB, y: UC")
Push = make_op("Push", "pushB_T x: TB")
Pop = make_op("Pop", "popB_L x: LB")
Lea = make_op("Lea", "leaB_L x: LB, y: mem*")
MemCopy = make_op("MemCopy", "memcpy_T_U_V dst: T64, src: U64, n: V64")
MemCopyNonOverlapping = make_op("MemCopyNonOverlapping", "memcpynv_T_U_V dst: T64, src: U64, n: V64")

AddF = make_op("AddF", "addfF_L_U x: LF, y: UF")
SubF = make_op("SubF", "subfF_L_U x: LF, y: UF")
MulF = make_op("MulF", "mulfF_L_U x: LF, y: UF")
DivF = make_op("DivF", "divfF_L_U x: LF, y: UF")
SqrtF = make_op("SqrtF", "sqrtfF_L x: LF")

Label = make_op("Label", "label")
Jump = make_op("Jump", "jumpB_T_U x: imm32, a: TB, b: UB")
Switch = make_op("Switch", "switchB_L_T_U x: L32, a: TB, b: UB, cases: imm32...")
Call = make_op("Call", "callB x: imm32")
Return = make_op("Return", "return")

CallExt = make_op("CallExt", "cext ext: imm32, inst_num: imm32, args: ****...")
