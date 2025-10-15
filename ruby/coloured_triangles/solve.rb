#!/usr/bin/env ruby

# I have deliberately not made it easy to understand
# how or why this works, to encourage fellow adventurers
# to find their own solutions.

def solve(input)
  return input if input.length == 1
  a = Math.log(input.length - 1)
  b = a / Math.log(3)
  c = b.floor
  d = (3**c) + 1
  e = input.length - d
  f = input[0..(e)]
  g = input[-(e+1)..-1]
  h = solve(f)
  i = solve(g)
  solve_pair(h, i)
end

def solve_pair(a, b)
  return a if a == b

  if a == "B" && b == "G"
    return "R"
  elsif a == "B" && b == "R"
    return "G"
  elsif a == "G" && b == "B"
    return "R"
  elsif a == "G" && b == "R"
    return "B"
  elsif a == "R" && b == "B"
    return "G"
  elsif a == "R" && b == "G"
    return "B"
  end
end
