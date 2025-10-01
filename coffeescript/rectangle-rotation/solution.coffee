scaleRed = (x) ->
  x = x / Math.sqrt(2)
  x = x / 2
  x = Math.floor(x)
  x = x * 2 + 1
  return x


scaleBlue = (x) ->
  x = x / Math.sqrt(2)
  x = x + 1
  x = x / 2
  x = Math.floor(x)
  x = x * 2
  return x


rectangleRotation = (a, b) ->
  areaRed = scaleRed(a) * scaleRed(b)
  areaBlue = scaleBlue(a) * scaleBlue(b)
  return areaRed + areaBlue

module.exports = { rectangleRotation }
