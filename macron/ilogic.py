from logic import *

class Interpreter:
    def __init__(self):
        self.scopes = []

    def alloc_scope(self, s: Scope):
        self.scopes.append((s, [0]*len(s.variables)))

    def dealloc_scope(self, s: Scope | None):
        v = self.scopes.pop()[0]
        if s is not None:
            assert v == s

    def call_block(self, b: Block):
        self.alloc_scope(b.scope)
        self.interpret_from_block(b)
        self.dealloc_scope(b.scope)

    def evaluate(self, e: Inst, scope_ref: ScopeRef):
        _, top = self.scopes[-(scope_ref+1)]
        match e:
            case Immediate(v):
                return v
            case Var(v):
                return top[v]
            case Add(x, y):
                return top[x]+top[y]
            case Sub(x, y):
                return top[x]-top[y]
            case Mul(x, y):
                return top[x]*top[y]
            case Div(x, y):
                return top[x]//top[y]
            case Lt(x, y):
                return top[x] < top[y]
            case Le(x, y):
                return top[x] <= top[y]
            case Eq(x, y):
                return top[x] == top[y]
            case Ne(x, y):
                return top[x] != top[y]
            case Ge(x, y):
                return top[x] >= top[y]
            case Gt(x, y):
                return top[x] > top[y]
            case AllocScope(s):
                self.alloc_scope(s)
            case DeallocScope(s):
                self.dealloc_scope(s)
            case Print(p):
                print(top[p])

    def interpret_from_block(self, b: Block):
        for target, expr, scope_ref in b.body:
            r = self.evaluate(expr, scope_ref)
            if target is not None:
                self.scopes[-1][1][target] = r
