"""
I have deliberately not made it easy to understand
how or why this works, to encourage fellow adventurers
to find their own solutions.
"""

import math
from functools import lru_cache

LOG3 = math.log(3)


@lru_cache(maxsize=1024)
def solve(row: str) -> str:
    if len(row) == 1:
        return row

    a = math.log(len(row) - 1)
    b = a / LOG3
    c = math.floor(b)
    d = 3**c + 1
    e = len(row) - d + 1
    f = row[:(e)]
    g = row[-(e):]
    h = solve(f)
    i = solve(g)
    return solve_pair(h, i)


def solve_pair(a: str, b: str) -> str:
    if a == b:
        return a

    if a == "B" and b == "G":
        return "R"
    if a == "B" and b == "R":
        return "G"
    if a == "G" and b == "B":
        return "R"
    if a == "G" and b == "R":
        return "B"
    if a == "R" and b == "B":
        return "G"
    if a == "R" and b == "G":
        return "B"

    raise RuntimeError("unreachable")
