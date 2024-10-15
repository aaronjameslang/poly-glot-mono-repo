## Jump to Zero

# Where a jump from n is defined as n minus the sum of its digits,
# count the number of jumps to get to zero.


def jump(origin):
    destination = origin
    digits = origin
    while digits > 0:
        digit = digits % 10
        destination -= digit
        digits = (digits - digit) // 10
    return destination


def count_jumps(n):
    if n == 0:
        return 0
    destination = jump(n)
    return count_jumps(destination) + 1


def test_count_jumps():
    _test_count(count_jumps, 18, 2)
    _test_count(count_jumps, 19, 2)
    _test_count(count_jumps, 20, 3)
    # _test_count(count_jumps_recursion, 1e8) // stack overflow


def _test_count(func, n, expected=None):
    result = func(n)
    if expected:
        assert result == expected, f"Expected {expected}, but got {result}"
    else:
        assert type(result) == int, f"Expected an integer, but got {result}"
