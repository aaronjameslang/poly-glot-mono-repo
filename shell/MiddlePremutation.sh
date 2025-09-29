#! /bin/sh

## Middle Permutation

# An 4th kyu problem posed by codewars.com:
# https://www.codewars.com/kata/58ad317d1541651a740000c5

# You are given a string s. Every letter in s appears once.
# Consider all strings formed by rearranging the letters in s. After ordering
# these strings in dictionary order, return the middle term. (If the sequence
# has a even length n, define its middle term to be the (n/2)th term.)

# Compatibility:
#    ash ?
#   bash √
#    csh x, not posix
#   dash √
#   fish x, not posix
#    ksh √
#     sh √
#   tcsh x, not posix
#    zsh √

set -eu

middle() {
  input="$1"
  sorted=$(echo "$input" | grep -o . | sort -r | tr -d '\n')
  len=${#sorted}
  mid=$((len / 2))
  middle_char=$(echo "$sorted" | cut -c$((mid + 1)))
  remaining=$(echo "$sorted" | sed "s/$middle_char//")

  if test $((len % 2)) -eq 1
  then
    echo "$middle_char$(middle "$remaining")"
  else
    echo "$middle_char$remaining"
  fi
}

run_test() {
  input="$1"
  expected="$2"
  actual=$(middle "$input")
  if test "$actual" = "$expected"
  then
    echo "Test passed: $input -> $actual"
  else
    echo "Test failed: $input -> $actual (expected $expected)"
  fi
}

run_test 'ab' 'ab'
run_test 'abc' 'bac'
run_test 'abcd' 'bdca'
run_test 'abcdx' 'cbxda'
run_test 'abcdxg' 'cxgdba'
run_test 'abcdxgz' 'dczxgba'    

echo "Done"