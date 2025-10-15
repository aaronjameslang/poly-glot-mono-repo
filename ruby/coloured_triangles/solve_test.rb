#!/usr/bin/env ruby
require 'rspec'
require_relative 'solve'

RSpec.describe "Coloured Triangles" do
  describe "correctness" do
    test_cases = [
      ["B", "B"],
      ["GB", "R"],
      ["RRR", "R"],
      ["RGBG", "B"],
      ["RBRGBRB", "G"],
      ["RBRGBRBGGRRRBGBBBGG", "G"]
    ]

    test_cases.each do |input, expected|
      it "solve(#{input.inspect}) == #{expected.inspect}" do
        expect( solve input ).to eq expected
      end
    end
  end
end
