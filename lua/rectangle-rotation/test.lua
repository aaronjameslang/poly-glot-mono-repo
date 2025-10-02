local rectangleRotation = require 'solution'
describe("Rectangle Rotation", function()
  it("Basic tests", function()
    assert.are.equal(23, rectangleRotation(6, 4))
    assert.are.equal(65, rectangleRotation(30, 2))
    assert.are.equal(49, rectangleRotation(8, 6))
    assert.are.equal(333, rectangleRotation(16, 20))
  end)
end)
