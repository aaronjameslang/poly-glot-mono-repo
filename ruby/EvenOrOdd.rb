## Even Or Odd

# An 8th kyu problem posed by codewars.com:
# https://www.codewars.com/kata/53da3dbb4a5168369a0000fe

# Create a function that takes an integer as an argument and returns "Even" for
# even numbers or "Odd" for odd numbers.

# Usage: `rspec EvenOrOdd.rb`

describe "Example tests" do
  it 'even_or_odd(1) should return "Odd"' do
    expect(even_or_odd(1)).to eq("Odd")
  end
  it 'even_or_odd(2) should return "Even"' do
    expect(even_or_odd(2)).to eq("Even")
  end
  it 'even_or_odd(-1) should return "Odd"' do
    expect(even_or_odd(-1)).to eq("Odd")
  end
  it 'even_or_odd(-2) should return "Even"' do
    expect(even_or_odd(-2)).to eq("Even")
  end
  it 'even_or_odd(0) should return "Even"' do
    expect(even_or_odd(0)).to eq("Even")
  end
end

def even_or_odd(n)
  n.even? ? "Even" : "Odd"
end