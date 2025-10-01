#import "Solution.h"
#import <Foundation/Foundation.h>
#import <math.h>

int scale_red(int x) {
  x = x / sqrt(2);
  x = x / 2;
  // floor
  x = x * 2;
  x = x + 1;
  return x;
}

int scale_blue(int x) {
  x = x / sqrt(2);
  x = x + 1;
  x = x / 2;
  // floor
  x = x * 2;
  return x;
}

long long rectangle_rotation(int a, int b) {
  int areaRed = scale_red(a) * scale_red(b);
  int areaBlue = scale_blue(a) * scale_blue(b);
  return areaRed + areaBlue;
}
