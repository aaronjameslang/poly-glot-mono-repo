#include <cmath>

int scaleRed(int x) {
  x = x / std::sqrt(2);
  x = x / 2;
  // floor
  x = x * 2;
  x = x + 1;
  return x;
}

int scaleBlue(int x) {
  x = x / std::sqrt(2);
  x = x + 1;
  x = x / 2;
  // floor
  x = x * 2;
  return x;
}

long long rectangle_rotation(int a, int b) {
  int areaRed = scaleRed(a) * scaleRed(b);
  int areaBlue = scaleBlue(a) * scaleBlue(b);
  return areaRed + areaBlue;
}
