## Evaluate Mathematical Expression

# A 2nd kyu problem posed by codewars.com:
# https://www.codewars.com/kata/52a78825cdfc2cfc87000005

# Given a mathematical expression as a string you must return the result as a number.

# Numbers may be either a whole numbers and/or decimal numbers.

# You need to support the following mathematical operators:
#   Multiplication *
#   Division / (as floating point division)
#   Addition +
#   Subtraction -

# Operators are always evaluated from left-to-right, and * and / must be evaluated before + and -.

# You need to support multiple levels of nested parentheses, ex. (2 / (2 + 3.33) * 4) - -6

def test_calc():
  tests = [
    ("1+1", 2),
    ("1 - 1", 0),
    ("1* 1", 1),
    ("1 /1", 1),
    ("-123", -123),
    ("123", 123),
    ("2 /2+3 * 4.75- -6", 21.25),
    ("12* 123", 1476),
    ("2 / (2 + 3) * 4.33 - -6", 7.732)
  ]
  for expr, expected in tests:
    assert calc(expr) == expected, f"Test failed for: {expr}"

def calc(expr):
  return eval_expression(tokenise(expr))

# -------------------------------------
# Tokenisation

Token = str | float


def is_numeric_char(c):
  return c.isdigit() or c == '.'

def tokenise(input: str) -> list[Token]: 
  tokens = []
  i = 0
  while i < len(input):
    c = input[i]
    if c.isspace():
        i += 1
    elif is_numeric_char(c):
        token, j = tokenise_numeric(input, i)
        tokens.append(token)
        i = j
    else:
        # Operator or parentheses
        tokens.append(c)
        i += 1

  return tokens

def tokenise_numeric(input: str, start: int) -> tuple[float, int]:
  i = start
  while i < len(input) and is_numeric_char(input[i]):
    i += 1
  num = float(input[start:i])
  return num, i

# -------------------------------------
# Evaluation

def eval_expression(tokens: list[Token]) -> float:
  result, remainder = eval_next_expression(tokens)
  if remainder:
    raise ValueError(f"Trailing tokens: {remainder}")
  return result

# Expressions are composed of terms that are added or subtracted
def eval_next_expression(tokens) -> tuple[float, list[Token]]:
  accumulator, remainder = eval_next_term(tokens)
  
  def next_is_add_or_sub():
    return remainder and (remainder[0] == '+' or remainder[0] == '-')
  
  while next_is_add_or_sub():
    op = remainder.pop(0)
    term, remainder = eval_next_term(remainder)
    accumulator = combine_terms(accumulator, op, term)
  
  return accumulator, remainder

def combine_terms(x: float, op: str, y: float) -> float:
  if op == '+':
    return x + y
  elif op == '-':
    return x - y
  else:
    raise ValueError(f"Invalid operator: {op}")

# Terms are things that are added or subtracted,
# they are composed of factors that are multiplied or divided
def eval_next_term(tokens: list[Token]) -> tuple[float, list[Token]]:
  accumulator, remainder = eval_next_factor(tokens)
  
  def next_is_mul_or_div():
    return remainder and (remainder[0] == '*' or remainder[0] == '/')
  
  while next_is_mul_or_div():
    op = remainder.pop(0)
    factor, remainder = eval_next_factor(remainder)
    accumulator = combine_factors(accumulator, op, factor)
  
  return accumulator, remainder

def combine_factors(x: float, op: str, y: float) -> float:
  if op == '*':
    return x * y
  elif op == '/':
    return x / y
  else:
    raise ValueError(f"Invalid operator: {op}")

# Factors are things that are multiplied or divided,
# they can numbers, negative numbers, or sub-expressions in parentheses
def eval_next_factor(tokens: list[Token]) -> tuple[float, list[Token]]:
  token = tokens.pop(0)
  
  if isinstance(token, float):
    return token, tokens
  
  if token == '-':
    n, remainder = eval_next_factor(tokens)
    return -n, remainder
  
  if token == '(':
    n, remainder = eval_next_expression(tokens)
    if remainder.pop(0) != ')':
        raise ValueError("Unmatched parentheses")
    return n, remainder
  
  raise ValueError(f"Invalid next factor: {tokens}")

