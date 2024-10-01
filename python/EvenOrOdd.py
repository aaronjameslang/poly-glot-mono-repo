## Even Or Odd

# An 8th kyu problem posed by codewars.com:
# https://www.codewars.com/kata/53da3dbb4a5168369a0000fe

# Create a function that takes an integer as an argument and returns "Even" for
# even numbers or "Odd" for odd numbers.

def test():
  assert evenOrOdd(1) == "Odd"
  assert evenOrOdd(2) == "Even"
  assert evenOrOdd(-1) == "Odd"
  assert evenOrOdd(-2) == "Even"
  assert evenOrOdd(0) == "Even"

def evenOrOdd(n: int) -> str:
  return "Odd" if n % 2 else "Even"
