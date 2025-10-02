local function scaleRed(x)
  x = x / math.sqrt(2)
  x = x / 2
  x = math.floor(x)
  x = x * 2
  x = x + 1
  return x
end

local function scaleBlue(x)
  x = x / math.sqrt(2)
  x = x + 1
  x = x / 2
  x = math.floor(x)
  x = x * 2
  return x
end

local function rectangleRotation(a, b)
  local areaRed = scaleRed(a) * scaleRed(b)
  local areaBlue = scaleBlue(a) * scaleBlue(b)
  return areaRed + areaBlue
end

return rectangleRotation
