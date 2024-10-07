## Middle Permutation

# An 4th kyu problem posed by codewars.com:
# https://www.codewars.com/kata/58ad317d1541651a740000c5

# You are given a string s. Every letter in s appears once.
# Consider all strings formed by rearranging the letters in s. After ordering
# these strings in dictionary order, return the middle term. (If the sequence
# has an even length n, define its middle term to be the (n/2)th term.)

# Usage: `rspec MiddlePermutation.rb`

describe "Basic tests" do
  def self.testt(input, expected)
    it "returns the correct middle permutation for '#{input}'" do
      expect(middle_permutation(input)).to eq(expected)
    end
  end

  testt "abc", "bac"
  testt "abcd", "bdca"
  testt "abcdx", "cbxda"
  testt "abcdxg", "cxgdba"
  testt "abcdxgz", "dczxgba"
end

def middle_permutation(input)
  s = input.chars.sort.reverse
  l = s.length
  m = s.delete_at(l / 2)
  if l.odd?
    m + middle_permutation(s.join)
  else
    m + s.join
  end
end
