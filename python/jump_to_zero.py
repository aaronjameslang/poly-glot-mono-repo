## Jump to Zero

# Where a jump from n is defined as n minus the sum of its digits,
# count the number of jumps to get to zero.

import random

def jump(origin):
    destination = origin
    digits = origin
    while digits > 0:
        digit = digits % 10
        destination -= digit
        digits = (digits - digit) // 10
    return destination

table = [0]

def count_jumps(n):
    global table
    for i in range(len(table), n + 1):
        dst = jump(i)
        table.append(table[dst] + 1)
    return table[n]


def count_jumpss(ns):
    return list(map(count_jumps, ns))


def test_count_jumps():
    _test_count(count_jumps, 0, 0)
    _test_count(count_jumps, 1, 1)
    _test_count(count_jumps, 18, 2)
    _test_count(count_jumps, 19, 2)
    _test_count(count_jumps, 20, 3)
    # _test_count(count_jumps, 1e8) # This is now very slow


def test_count_jumpss():
    ns = [random.randint(0, int(1e5)) for _ in range(1000)]
    _test_counts(count_jumpss, [18, 19, 20], [2, 2, 3])
    _test_counts(count_jumpss, ns)


def _test_count(func, n, expected=None):
    result = func(int(n))
    if expected:
        assert result == expected, f"Expected {expected}, but got {result}"
    else:
        assert type(result) == int, f"Expected an integer, but got {result}"


def _test_counts(func, ns, expected=None):
    results = func(ns)
    if expected:
        assert results == expected, f"Expected {expected}, but got {results}"
    else:
        for result in results:
            assert type(result) == int, f"Expected an integer, but got {result}"
