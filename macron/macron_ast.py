from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from typing import Union, Optional


@dataclass
class ByteLit:
    val: bytes

@dataclass
class BytesLit:
    val: bytes

@dataclass
class CharLit:
    val: str

@dataclass
class StringLit:
    val: str

@dataclass
class IntLit:
    val: int

@dataclass
class ListLit:
    val: list[Expr]

@dataclass
class BlockLit:
    val: list[Expr]

@dataclass
class Name:
    name: str

@dataclass
class Call:
    func: Expr
    args: list[Expr]
    is_tight: bool

@dataclass
class AltCall:
    func: Expr
    args: list[Expr]

class Operator(Enum):
    DOT = 0
    SUBSCRIPT = 1
    MAYBE = 2
    COALESCE = 5
    EXPONENT = 6
    NEGATE = 7
    PLUS = 8
    MULTIPLY = 9
    MATRIX_MULTIPLY = 10
    DIVIDE = 11
    FLOOR_DIVIDE = 12
    MODULUS = 13
    ADD = 14
    SUBTRACT = 15
    RIGHT_SHIFT = 16
    LEFT_SHIFT = 17
    BITWISE_AND = 18
    BITWISE_XOR = 19
    BITWISE_OR = 20
    LESS_THAN = 21
    LESS_THAN_EQUAL = 22
    EQUAL = 23
    NOT_EQUAL = 24
    GREATER_THAN = 25
    GREATER_THAN_EQUAL = 26
    NOT = 27
    AND = 28
    OR = 29
    ASSIGN = 30
    ASSIGN_ADD = 31
    ASSIGN_SUBTRACT = 32
    ASSIGN_MULTIPLY = 33
    ASSIGN_MATRIX_MULTIPLY = 34
    ASSIGN_EXPONENT = 35
    ASSIGN_DIVIDE = 36
    ASSIGN_FLOOR_DIVIDE = 37
    ASSIGN_MODULUS = 38
    ASSIGN_COALESCE = 39
    ASSIGN_RIGHT_SHIFT = 40
    ASSIGN_LEFT_SHIFT = 41
    ASSIGN_BITWISE_AND = 42
    ASSIGN_BITWISE_XOR = 43
    ASSIGN_BITWISE_OR = 44
    COLON = 45
    ACCESS = 46
    BITWISE_NOT = 47
    ARROW = 48

@dataclass
class OpCall:
    op: Operator
    args: Expr | tuple[Expr, Expr]


Expr = Union[StringLit, IntLit, ListLit, BlockLit, Name, Call, OpCall]
