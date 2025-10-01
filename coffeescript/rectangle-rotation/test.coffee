{ rectangleRotation } = require('./solution')
expect = require('chai').expect

describe 'Basic Tests', ->
  it 'It should work for basic tests.', ->
    expect(rectangleRotation(6, 4)).to.equal(23)
    expect(rectangleRotation(30, 2)).to.equal(65)
    expect(rectangleRotation(8, 6)).to.equal(49)
    expect(rectangleRotation(16, 20)).to.equal(333)
    return
  return
