#!/usr/bin/env python3
import ast
import operator
import sys
import inspect

"""A reference big-int"""


class Builtin:
    """functions built-in to the expression language"""

    zero = 0
    one = 1
    minus_one = -1
    add = operator.add
    sub = operator.sub
    mul = operator.mul
    logand = operator.and_
    logor = operator.or_
    logxor = operator.xor
    lognot = operator.inv
    shift_left = operator.lshift
    shift_right = operator.rshift
    neg = operator.neg
    abs = abs

    @classmethod
    def _unsupported_operand_types(cls, *args, func_name):
        return TypeError(
            f"unsupported operand type(s) for {func_name}: "
            + " and ".join(type(arg).__name__ for arg in args)
        )

    @classmethod
    def _validate_operand_types(cls, *args, func_name):
        if all(isinstance(arg, types) for arg, types in args):
            return
        else:
            args = [arg for arg, _ in args]
            raise cls._unsupported_operand_types(*args, func_name=func_name)

    @classmethod
    def div(cls, x, y):
        "division rounding toward zero"
        if isinstance(x, float) and isinstance(y, float):
            return x / y
        elif isinstance(x, int) and isinstance(y, int):
            if cls.sign(x) * cls.sign(y) < 0:
                return x // y
            else:
                return -(abs(x) // abs(y))
        else:
            raise cls._unsupported_operand_types(x, y, func_name="div")

    @classmethod
    def rem(cls, x, y):
        "remainder for `div`"
        if isinstance(x, float) and isinstance(y, float):
            return x % y
        elif isinstance(x, int) and isinstance(y, int):
            return x - (y * cls.div(x, y))
        else:
            raise cls._unsupported_operand_types(x, y, func_name="rem")

    @classmethod
    def bit(cls, i, bit):
        cls._validate_operand_types((i, int), (bit, int), func_name="bit")
        if bit < 0:
            raise ValueError("bit argument must be non-negative")
        return bool(i & (1 << bit))

    @classmethod
    def zero_extend(cls, i, bits):
        cls._validate_operand_types((i, int), (bits, int), func_name="zero_extend")
        if bits < 0:
            raise ValueError("bits argument must be non-negative")
        return i & ((1 << bits) - 1)

    @classmethod
    def sign_extend(cls, i, bits):
        cls._validate_operand_types((i, int), (bits, int), func_name="sign_extend")
        if bits < 0:
            raise ValueError("bits argument must be non-negative")
        if bits == 0:
            return 0
        if cls.bit(i, bits - 1):
            return i | (-1 << (bits - 1))
        else:
            return cls.zero_extend(i, bits)

    @classmethod
    def byteswap(cls, i, bytes_):
        cls._validate_operand_types((i, int), (bytes_, int), func_name="byteswap")
        if bytes_ < 0:
            raise ValueError("bytes_ argument must be non-negative")
        i = cls.zero_extend(i, bits=bytes_ * 8)
        return int.from_bytes(
            int.to_bytes(i, bytes_, "little", signed=False), "big", signed=False
        )

    @staticmethod
    def compare(a, b):
        if a < b:
            return -1
        if a == b:
            return 0
        if a > b:
            return 1
        else:
            assert False

    @classmethod
    def sign(cls, i):
        cls._validate_operand_types((i, int), func_name="sign")
        return cls.compare(i, 0)

    @classmethod
    def of_float(cls, f):
        cls._validate_operand_types((f, float), func_name="of_float")
        return int(f)

    @classmethod
    def to_float(cls, i):
        cls._validate_operand_types((i, int), func_name="to_float")
        return float(i)

    @classmethod
    def of_string(cls, s):
        cls._validate_operand_types((s, bytes), func_name="of_string")
        return int(s)

    @classmethod
    def to_string(cls, i):
        cls._validate_operand_types((i, int), func_name="to_string")
        return str(i).encode()

    @classmethod
    def to_bytes_le(cls, i, length, *, signed):
        cls._validate_operand_types(
            (i, int), (length, int), (signed, bool), func_name="to_bytes_le"
        )
        return int.to_bytes(i, length, byteorder="little", signed=signed)

    @classmethod
    def to_bytes_be(cls, i, length, *, signed):
        cls._validate_operand_types(
            (i, int), (length, int), (signed, bool), func_name="to_bytes_be"
        )
        return int.to_bytes(i, length, byteorder="big", signed=signed)

    @classmethod
    def of_bytes_be(cls, b, *, signed):
        cls._validate_operand_types((b, bytes), (signed, bool), func_name="of_bytes_be")
        return int.from_bytes(b, byteorder="big", signed=signed)

    @classmethod
    def of_bytes_le(cls, b, *, signed):
        cls._validate_operand_types((b, bytes), (signed, bool), func_name="of_bytes_le")
        return int.from_bytes(b, byteorder="little", signed=signed)


class UpgradeConstants(ast.NodeTransformer):
    """upgrade old ast nodes from pre-3.8 to ast.Constant"""

    def visit_Num(self, node):
        return ast.copy_location(ast.Constant(node.n), node)

    def visit_Str(self, node):
        return ast.copy_location(ast.Constant(node.s), node)

    def visit_Bytes(self, node):
        return ast.copy_location(ast.Constant(node.s), node)

    def visit_NameConstant(self, node):
        return ast.copy_location(ast.Constant(node.value), node)


class Compile:
    def __init__(self, source, filename="<stdin>"):
        self.filename = filename
        self.source = source
        expr = ast.parse(source, filename=filename, mode="eval")
        self.body = UpgradeConstants().visit(expr.body)

    def location(self, node):
        try:
            line = self.source.split("\n")[node.lineno]
        except ValueError:
            line = ""
        return (self.filename, node.lineno, node.col_offset, line)

    OPERATORS = {
        ast.Add: Builtin.add,
        ast.Sub: Builtin.sub,
        ast.Mult: Builtin.mul,
        ast.Div: Builtin.div,
        ast.Mod: Builtin.rem,
        ast.BitXor: Builtin.logxor,
        ast.BitAnd: Builtin.logand,
        ast.BitOr: Builtin.logor,
        ast.LShift: Builtin.shift_left,
        ast.RShift: Builtin.shift_right,
        ast.Not: operator.not_,
        ast.Invert: operator.inv,
        ast.USub: operator.neg,
        ast.UAdd: operator.pos,
        ast.Eq: operator.eq,
        ast.NotEq: operator.ne,
        ast.Lt: operator.lt,
        ast.LtE: operator.le,
        ast.Gt: operator.gt,
        ast.GtE: operator.ge,
    }

    def eval_(self, node):
        if type(node) in self.OPERATORS:
            return self.OPERATORS[type(node)]
        elif isinstance(node, ast.Name):
            if node.id.startswith("_") or not hasattr(Builtin, node.id):
                raise NameError(f"name {repr(node.id)} is not defined")
            else:
                return getattr(Builtin, node.id)
        elif isinstance(node, ast.Constant) and isinstance(
            node.value, (int, bytes, float, bool, str)
        ):
            if isinstance(node.value, str):
                return node.value.encode()
            return node.value
        elif isinstance(node, ast.UnaryOp):
            return self.eval_(node.op)(self.eval_(node.operand))
        elif isinstance(node, ast.BinOp):
            return self.eval_(node.op)(self.eval_(node.left), self.eval_(node.right))
        elif isinstance(node, ast.IfExp):
            return (
                self.eval_(node.body)
                if self.eval_(node.test)
                else self.eval_(node.orelse)
            )
        elif isinstance(node, ast.Compare) and len(node.comparators) == 1:
            left = self.eval_(node.left)
            for op, right in zip(node.ops, node.comparators):
                if not self.eval_(op)(left, self.eval_(right)):
                    return False
                left = right
            return True
        elif isinstance(node, ast.Call):
            func = self.eval_(node.func)
            args = [self.eval_(arg) for arg in node.args]
            kwargs = {k.arg: self.eval_(k.value) for k in node.keywords}
            return func(*args, **kwargs)
        else:
            raise SyntaxError("unsupported operation", self.location(node))


try:
    for line in sys.stdin:
        try:
            expr = Compile(line)
            result = expr.eval_(expr.body)
        except Exception as e:
            short = type(e).__name__
            long = str(e)
            if "\n" in long:
                print(short, flush=True)
            else:
                print(f"{short}: {long}", flush=True)

            if not isinstance(e, ZeroDivisionError):
                print(e, file=sys.stderr, flush=True)
        else:
            print(result, flush=True)
except KeyboardInterrupt:
    exit(0)
