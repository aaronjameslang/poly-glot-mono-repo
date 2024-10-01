## Middle Permutation

# An 4th kyu problem posed by codewars.com:
# https://www.codewars.com/kata/58ad317d1541651a740000c5

# You are given a string s. Every letter in s appears once.
# Consider all strings formed by rearranging the letters in s. After ordering
# these strings in dictionary order, return the middle term. (If the sequence
# has a even length n, define its middle term to be the (n/2)th term.)

def test():
  assert middle_permutation('abc') == 'bac'
  assert middle_permutation('abcd') == 'bdca'
  assert middle_permutation('abcdx') == 'cbxda'
  assert middle_permutation('abcdxg') == 'cxgdba'
  assert middle_permutation('abcdxgz') == 'dczxgba'

def middle_permutation(input: str):
  s = sorted(input, reverse=True)
  l = len(s)
  m = s.pop(l//2)
  if l % 2:
    return m + middle_permutation(s)
  else:
    return m + ''.join(s)
