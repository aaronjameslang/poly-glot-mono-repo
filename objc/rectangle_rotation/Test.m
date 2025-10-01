#import "Solution.h"
#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
  NSArray *testCases = @[
    @[ @6, @4, @23 ],
    @[ @30, @2, @65 ],
    @[ @8, @6, @49 ],
    @[ @16, @20, @333 ],
  ];

  for (NSArray *testCase in testCases) {
    int a = [testCase[0] intValue];
    int b = [testCase[1] intValue];
    int expected = [testCase[2] intValue];
    int actual = rectangle_rotation(a, b);

    BOOL testPassed = actual == expected;
    if (testPassed) {
      NSLog(@"Test PASSED: convert(%d) = %d ✓", a + b, expected);
    } else {
      NSLog(@"Test FAILED: convert(%d) = %d (expected %d) ✗", a + b, actual,
            expected);
    }
  }
}
