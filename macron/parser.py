import sys
from enum import Enum
from contextlib import contextmanager

import macron_ast as ast


class ParseResult:
    def __init__(self, result, success, expected=None):
        self.result = result
        self.success = success

    def __bool__(self):
        return self.success

    def __repr__(self):
        if self.success:
            return f"<successful ParseResult {self.result}>"
        else:
            return "<failed ParseResult>"

class ParseFailed(BaseException):
    pass

class StringView:
    def __init__(self, string):
        self.string = string
        self.idx = 0
        self.line = 0
        self.column = 0
        self.newline_state = []

    @contextmanager
    def newlines(self, allow_newlines):
        self.newline_state.append(allow_newlines)
        yield
        self.newline_state.pop()

    @property
    def next_char(self):
        return self.string[self.idx] if not self.eof else None

    @property
    def eof(self):
        return self.idx >= len(self.string)

    def consume(self, c):
        t = self.string[self.idx:self.idx+c]
        self.idx += c
        for c in t:
            self.column += 1
            if c == "\n":
                self.column = 0
                self.line += 1
        return t

    def fail(self, message, *, say_unexpected=True):
        o = []
        line = self.string[self.idx-self.column:].split("\n", 1)[0]
    
        o.append(f"{self.line+1}:{self.column+1}:")
        t = " "*len(str(self.line+1)) + " | "
        o.append(t)
        o.append(f"{self.line+1} | {line}")
        o.append(t + " "*self.column + "^")
        if message:
            if say_unexpected:
                unexpected = self.next_char
                o.append(f"unexpected {unexpected!r}" if unexpected else "unexpected EOF")
            o.append(f"{message}")
        else:
            o.append("Parsing failed (no information)")
        print("\n".join(o), file=sys.stderr)
        raise ParseFailed

    def literal(self, expected):
        if self.string[self.idx:self.idx+len(expected)] != expected:
            return ParseResult(None, False)
        self.consume(len(expected))
        return ParseResult(None, True)

    def expect(self, result, string):
        if not result:
            self.fail(f"expected {string}")
        return result

    def expect_lit(self, expected, message=None):
        return self.expect(self.literal(expected), message or repr(expected))

    def skip_ws(self):
        s = {" ", "\t", "\r"}
        if self.newline_state[-1]:
            s.add("\n")
        while True:
            if self.next_char in s:
                self.consume(1)
            elif self.next_char == "#":
                while not self.eof and self.next_char != "\n":
                    self.consume(1)
            else:
                return

    def symbol(self, expected):
        r = self.literal(expected)
        if not r:
            return r
        self.skip_ws()
        return r

    def parse_int(self):
        base = 10
        BASES = {
            "b": 2,
            "o": 8,
            "x": 16,
        }
        if self.next_char == "0":
            self.consume(1)
            if self.next_char in BASES:
                base = BASES[self.consume(1)]
            else:
                return ParseResult(0, True)
        n = 0
        while not self.eof:
            if self.next_char == "_":
                self.consume(1)
                continue
            try:
                n = n * base + int(self.next_char, base)
            except ValueError:
                break
            else:
                self.consume(1)
        if n == 0:
            return ParseResult(None, False)
        return ParseResult(n, True)

    def _parse_byte(self, quote):
        if not self.next_char.isascii():
            self.fail("expected ASCII character")
        if self.next_char == "\\":
            self.consume(1)
            if self.next_char == "n":
                self.consume(1)
                return b"\n"
            elif self.next_char == "r":
                self.consume(1)
                return b"\r"
            elif self.next_char == "t":
                self.consume(1)
                return b"\t"
            elif self.next_char == "\\":
                self.consume(1)
                return b"\\"
            elif self.next_char == "0":
                self.consume(1)
                return b"\0"
            elif self.next_char == "x":
                self.consume(1)
                try:
                    r = int(self.string[self.idx:self.idx+2], 16)
                except ValueError:
                    self.fail("invalid byte escape", say_unexpected=False)
                else:
                    self.consume(2)
                    return bytes([r])
            elif self.next_char == '"':
                self.consume(1)
                return b'"'
            elif self.next_char == "'":
                self.consume(1)
                return b"'"
            else:
                self.fail("invalid escape", say_unexpected=False)
        elif self.next_char == quote:
            self.consume(1)
            return None
        elif self.next_char:
            return self.consume(1).encode()
        else:
            return -1

    def _parse_unicode(self, quote):
        if self.next_char == "\\":
            self.consume(1)
            if self.next_char == "n":
                self.consume(1)
                return "\n"
            elif self.next_char == "r":
                self.consume(1)
                return "\r"
            elif self.next_char == "t":
                self.consume(1)
                return "\t"
            elif self.next_char == "\\":
                self.consume(1)
                return "\\"
            elif self.next_char == "0":
                self.consume(1)
                return "\0"
            elif self.next_char == "x":
                self.consume(1)
                try:
                    r = int(self.string[self.idx:self.idx+2], 16)
                except ValueError:
                    self.fail("invalid ASCII escape", say_unexpected=False)
                else:
                    if r > 0x7F:
                        example = "b'f'" if quote == "'" else 'b"foo"'
                        self.fail(f"""\\xXX escapes in normal strings are ASCII escapes and must be lower than 0x7F, the highest ASCII codepoint
help: to make a byte string, prepend `b` to the string (e.g. {example})""", say_unexpected=False)
                    self.consume(2)
                    return chr(r)
            elif self.next_char == '"':
                return self.consume(1)
            elif self.next_char == "'":
                return self.consume(1)
            elif self.next_char in "uU":
                size = 4 if self.next_char == "u" else 8
                self.consume(1)
                if self.next_char == "{":
                    self.consume(1)
                    r = 0
                    while self.next_char != "}":
                        try:
                            r = r * 16 + int(self.next_char, 16)
                        except ValueError:
                            self.fail("expected hex digit")
                        else:
                            self.consume(1)
                    self.consume(1)
                else:
                    try:
                        r = int(self.string[self.idx:self.idx+size], 16)
                    except ValueError:
                        if size == 8:
                            self.fail("invalid Unicode character escape\nhelp: \\UXXXXXXXX escapes are exactly 8 digits long", say_unexpected=False)
                        else:
                            self.fail("invalid Unicode character escape\nhelp: \\uXXXX escapes are exactly 4 digits long", say_unexpected=False)
                    else:
                        self.consume(size)
                if r > 0x10FFFF:
                    self.fail("Unicode character escapes must be lower than 0x10FFFF, the highest possible Unicode codepoint", say_unexpected=False)
                return chr(r)
            else:
                self.fail("invalid escape", say_unexpected=False)
        elif self.next_char == quote:
            self.consume(1)
            return None
        else:
            return self.consume(1) or -1

    def _next_by_i(self, i):
        return None if self.eof else self.string[self.idx+i]

    def parse_string(self):
        i = 0
        raw = False
        hash_depth = 0
        byte = False
        if self._next_by_i(i) == "b":
            i += 1
            byte = True
        if self._next_by_i(i) == "r":
            i += 1
            raw = True
            while self._next_by_i(i) == "#":
                i += 1
                hash_depth += 1

        if self._next_by_i(i) == '"':
            char = False
            i += 1
        elif self._next_by_i(i) == "'":
            char = True
            i += 1
        else:
            return ParseResult(None, False)

        if char and raw:
            self.fail("character literals cannot be raw", say_unexpected=False)

        self.consume(i)

        quote = "'" if char else '"'
        meth = self._parse_byte if byte else self._parse_unicode

        output = b"" if byte else ""
        while True:
            r = meth(quote)
            if r == -1:
                self.fail(f"expected {quote}")
            if not r and all(c == "#" for c in self.string[self.idx:self.idx+hash_depth]):
                self.consume(hash_depth)
                break
            else:
                output += r or quote
        if char:
            if len(output) == 0:
                self.fail("character literals cannot be empty", say_unexpected=False)
            elif len(output) > 1:
                self.fail("character literals cannot be longer than 1 character", say_unexpected=False)

        return ParseResult((output, char, byte), True)

    def parse_name(self):
        if not (not self.eof and self.next_char.isascii() and (self.next_char.isalpha() or self.next_char == "_")):
            return ParseResult(None, False)
        n = self.consume(1)
        while not self.eof and self.next_char.isascii() and (self.next_char.isalnum() or self.next_char == "_"):
            n += self.consume(1)
        return ParseResult(n, True)
        

def qual_name(view):
    name = view.parse_name()
    if not name:
        return name
    v = ast.Name(name.result)
    while view.literal("::").success:
        name = view.parse_name()
        view.expect(name, "name")
        v = ast.OpCall(ast.Operator.ACCESS, (v, name.result))
    return ParseResult(v, True)

def comma_sep_expr(view, *, restrict=False):
    if restrict:
        x = core_expr(view, BINARY_OPS["|"][1])
        view.expect(x, "expression")
    else:
        x = expr(view)
    if not x:
        return []
    l = [x.result]
    while view.symbol(",").success:
        if restrict:
            x = core_expr(view, BINARY_OPS["|"][1])
            view.expect(x, "expression")
        else:
            x = expr(view)
        if not x:
            return l
        l.append(x.result)
    return l

def int_lit(view):
    r = view.parse_int()
    if not r:
        return r
    return ParseResult(ast.IntLit(r.result), True)

def string_lit(view):
    r = view.parse_string()
    if not r:
        return r
    v, char, byte = r.result
    if char and byte:
        t = ast.ByteLit
    elif char:
        t = ast.CharLit
    elif byte:
        t = ast.BytesLit
    else:
        t = ast.StringLit
    return ParseResult(t(v), True)

def list_lit(view):
    if not view.literal("["):
        return ParseResult(None, False)
    with view.newlines(True):
        view.skip_ws()
        r = comma_sep_expr(view)
    view.expect_lit("]")
    return ParseResult(ast.ListLit(r), True)

def block(view):
    if not view.literal("{"):
        return ParseResult(None, False)
    with view.newlines(True):
        view.skip_ws()
        r = code(view)
    view.expect_lit("}")
    return ParseResult(ast.BlockLit(r.result), True)

def atom(view):
    if view.literal("("):
        with view.newlines(True):
            view.skip_ws()
            r = expr(view)
        view.expect_lit(")")
        return r
    return block(view) or list_lit(view) or int_lit(view) or string_lit(view) or qual_name(view)

def base_expr(view):
    a = atom(view)
    if not a:
        return a
    x = a.result
    while True:
        if view.literal("."):
            d = atom(view)
            view.expect(d, "literal or name")
            x = ast.OpCall(ast.Operator.DOT, (x, d))
        elif view.next_char == "?" and view.string[view.idx+1] != "?":
            view.consume(1)
            x = ast.OpCall(ast.Operator.MAYBE, x)
        elif view.literal("["):
            with view.newlines(True):
                view.skip_ws()
                e = expr(view)
            view.expect_lit("]")
            x = ast.OpCall(ast.Operator.SUBSCRIPT, (x, e))
        elif view.literal("("):
            with view.newlines(True):
                view.skip_ws()
                es = comma_sep_expr(view)
            view.expect_lit(")")
            x = ast.Call(x, es, True)
        elif view.literal("<"):
            with view.newlines(True):
                view.skip_ws()
                es = comma_sep_expr(view, restrict=True)
            view.expect_lit(">", "> to match opening <\nhelp: a space is required before the comparison operator <")
            x = ast.AltCall(x, es)
        else:
            break
    view.skip_ws()
    return x

class Assoc(Enum):
    LEFT = 0
    RIGHT = 1
    PREFIX = 2

OPS = [
    ([("??", ast.Operator.COALESCE)], Assoc.RIGHT),
    ([("**", ast.Operator.EXPONENT)], Assoc.RIGHT),
    ([("-", ast.Operator.NEGATE), ("+", ast.Operator.PLUS), ("~", ast.Operator.BITWISE_NOT)], Assoc.PREFIX),
    ([("*", ast.Operator.MULTIPLY), ("@", ast.Operator.MATRIX_MULTIPLY), ("/", ast.Operator.DIVIDE), ("//", ast.Operator.FLOOR_DIVIDE), ("%", ast.Operator.MODULUS)], Assoc.LEFT),
    ([("+", ast.Operator.ADD), ("-", ast.Operator.SUBTRACT)], Assoc.LEFT),
    ([(">>", ast.Operator.RIGHT_SHIFT), ("<<", ast.Operator.LEFT_SHIFT)], Assoc.LEFT),
    ([("&", ast.Operator.BITWISE_AND)], Assoc.LEFT),
    ([("^", ast.Operator.BITWISE_XOR)], Assoc.LEFT),
    ([("|", ast.Operator.BITWISE_OR)], Assoc.LEFT),
    ([
        ("<", ast.Operator.LESS_THAN),
        ("<=", ast.Operator.LESS_THAN_EQUAL),
        ("==", ast.Operator.EQUAL),
        ("!=", ast.Operator.NOT_EQUAL),
        (">=", ast.Operator.GREATER_THAN),
        (">", ast.Operator.GREATER_THAN_EQUAL)
    ], Assoc.LEFT),
    ([("not", ast.Operator.NOT)], Assoc.PREFIX),
    ([("and", ast.Operator.AND)], Assoc.LEFT),
    ([("or", ast.Operator.OR)], Assoc.LEFT),
    ([("=>", ast.Operator.ARROW)], Assoc.RIGHT),
    ([(":", ast.Operator.COLON)], Assoc.RIGHT),
    ([("=", ast.Operator.ASSIGN)], Assoc.RIGHT),
]

ASSIGNMENT_OPS = {ast.Operator.ASSIGN}
# augmented ops
for group, binding in OPS:
    for name, variant in group:
        n = "ASSIGN_" + variant.name
        if n in ast.Operator.__members__:
            ASSIGNMENT_OPS.add(ast.Operator[n])
            OPS.append(([(name + "=", ast.Operator[n])], Assoc.RIGHT))

PREFIX_OPS = {}
SHORTCUT_PREFIX = {}
BINARY_OPS = {}
SHORTCUT_BINARY = {}

PREFIX_OP_CHARS = set()
BINARY_OP_CHARS = set()

t = 1
for group, binding in reversed(OPS):
    for name, variant in group:
        if binding == Assoc.PREFIX:
            PREFIX_OPS[name] = variant, t
            if len(name) > 1:
                SHORTCUT_PREFIX[name[0]] = False
            elif name not in SHORTCUT_PREFIX:
                SHORTCUT_PREFIX[name] = True
            PREFIX_OP_CHARS.add(name[0])
        else:
            BINARY_OP_CHARS.add(name[0])
            if len(name) > 1:
                SHORTCUT_BINARY[name[0]] = False
            elif name not in SHORTCUT_BINARY:
                SHORTCUT_BINARY[name] = True
            if binding == Assoc.LEFT:
                BINARY_OPS[name] = variant, t, t+1
            elif binding == Assoc.RIGHT:
                BINARY_OPS[name] = variant, t+1, t
    t += 1 if binding == Assoc.PREFIX else 2

PREFIX_OPS = dict(sorted(PREFIX_OPS.items(), key=lambda d: len(d[0]), reverse=True))
BINARY_OPS = dict(sorted(BINARY_OPS.items(), key=lambda d: len(d[0]), reverse=True))

def parse_prefix_op(view):
    if view.next_char not in PREFIX_OP_CHARS:
        return ParseResult(None, False)
    if view.next_char in PREFIX_OPS and SHORTCUT_PREFIX[view.next_char]:
        r = ParseResult(PREFIX_OPS[view.consume(1)], True)
        view.skip_ws()
        return r
    for v, p in PREFIX_OPS.items():
        if len(v) < 2:
            break
        # stupid hack to prevent `not` from causing problems with names like `notify`
        if v.isidentifier() and view.string[view.idx+len(v)].isidentifier():
            continue
        if view.symbol(v):
            return ParseResult(p, True)
    return ParseResult(None, False)

def parse_binary_op(view, min_power):
    if view.next_char not in BINARY_OP_CHARS:
        return ParseResult(None, False)
    if view.next_char in BINARY_OPS and SHORTCUT_BINARY[view.next_char]:
        p = BINARY_OPS[view.next_char]
        if p[1] < min_power:
            return ParseResult(None, False)
        view.consume(1)
        r = ParseResult(p, True)
        view.skip_ws()
        return r
    for v, p in BINARY_OPS.items():
        if p[1] < min_power:
            continue
        if view.symbol(v):
            return ParseResult(p, True)
    return ParseResult(None, False)

def core_expr(view, min_power=0):
    FAIL_GANG_UNARY = {
        ast.Operator.NEGATE: "unary -",
        ast.Operator.PLUS: "unary +",
        ast.Operator.BITWISE_NOT: "unary ~",
    }
    FAIL_GANG_BINARY = {
        ast.Operator.EXPONENT: "exponentiation operator **",
        ast.Operator.COALESCE: "coalescence operator ??",
    }
    prefix_op = parse_prefix_op(view)
    if prefix_op:
        op, power = prefix_op.result
        r = core_expr(view, power).result
        if op in FAIL_GANG_UNARY and isinstance(r, ast.OpCall) and r.op in FAIL_GANG_BINARY:
            view.fail(f"{FAIL_GANG_UNARY[op]} cannot be used before the {FAIL_GANG_BINARY[r.op]} without parentheses", say_unexpected=False)
        x = ast.OpCall(op, r)
    else:
        x = base_expr(view)
        if not x:
            return ParseResult(None, False)

    while True:
        r = parse_binary_op(view, min_power)
        if not r:
            break
        op, _, right_power = r.result
        if op in ASSIGNMENT_OPS:
            x = ast.OpCall(op, (x, commaless_call(view).result))
            break  # nothing more to parse
        else:
            x = ast.OpCall(op, (x, core_expr(view, right_power).result))
    return ParseResult(x, True)

def commaless_call(view, *, expect=True):
    head = core_expr(view)
    if expect:
        view.expect(head, "expression")
    elif not head:
        return ParseResult(None, False)
    head = head.result
    args = []
    while True:
        arg = core_expr(view)
        if not arg:
            break
        if not args and isinstance(head, ast.OpCall) and head.op not in (ast.Operator.MAYBE, ast.Operator.SUBSCRIPT):
            view.fail("head of call must be a literal, name, or parenthesized expression with optional use of the ? and subscript operators")
        args.append(arg.result)
    if not args:
        return ParseResult(head, True)
    else:
        return ParseResult(ast.Call(head, args, False), True)

expr = commaless_call

def code(view):
    lines = []
    while True:
        with view.newlines(False):
            r = expr(view, expect=False)
        if not r:
            break
        lines.append(r.result)
        if not (view.symbol(";") or view.symbol("\n")):
            break
    return ParseResult(lines, True)

def macron(view):
    with view.newlines(True):
        view.skip_ws()
        r = code(view).result
    if not view.eof:
        view.fail("expected EOF")
    return ParseResult(r, True)


import sys
with open(sys.argv[1], "r") as f:
    program = f.read()
try:
    v = StringView(program)
    with v.newlines(True):
        print(macron(v).result)
except ParseFailed:
    pass
