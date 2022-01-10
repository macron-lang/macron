from __future__ import annotations

from dataclasses import dataclass


Variable = int
ScopeRef = int

class Type:
    pass

@dataclass
class Scope:
    variables: list[Type]

class Inst:
    pass

@dataclass
class Immediate(Inst):
    value: int  

@dataclass
class Var(Inst):
    var: Variable

@dataclass
class Add(Inst):
    augend: Variable
    addend: Variable

@dataclass
class Sub(Inst):
    minuend: Variable
    subtrahend: Variable

@dataclass
class Mul(Inst):
    multiplicand: Variable
    multiplier: Variable
    signed: bool

@dataclass
class Div(Inst):
    dividend: Variable
    divisor: Variable
    signed: bool

@dataclass
class Lt(Inst):
    x: Variable
    y: Variable
    signed: bool

@dataclass
class Le(Inst):
    x: Variable
    y: Variable
    signed: bool

@dataclass
class Eq(Inst):
    x: Variable
    y: Variable

@dataclass
class Ne(Inst):
    x: Variable
    y: Variable

@dataclass
class Ge(Inst):
    x: Variable
    y: Variable
    signed: bool

@dataclass
class Gt(Inst):
    x: Variable
    y: Variable
    signed: bool

@dataclass
class AllocScope(Inst):
    scope: Scope

@dataclass
class DeallocScope(Inst):
    scope: Scope

@dataclass
class Print(Inst):
    val: Variable


class Terminator:
    pass

@dataclass
class Switch(Terminator):
    value: Variable
    cases: list[tuple[int, Block]]

@dataclass
class Br(Terminator):
    target: Block

@dataclass
class Block:
    name: str
    scope: Scope
    body: list[tuple[Variable | None, Inst, ScopeRef]]
    terminator: Terminator

@dataclass
class Program:
    scopes: list[Scope]
    blocks: list[Block]
