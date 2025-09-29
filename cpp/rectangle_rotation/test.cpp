// AI wrote this file, it works for now TODO review in detail

#include <iostream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

// Forward declaration of the function to test
long long rectangle_rotation(int a, int b);

// Test case structure: (a, b, expected result)
using TestCase = std::tuple<int, int, long long>;

// Simple test helper function
bool runTest(int a, int b, long long expected, std::string& error_message) {
  long long result = rectangle_rotation(a, b);
  if (result != expected) {
    error_message = "rectangle_rotation(" + std::to_string(a) + ", " +
                    std::to_string(b) + ") returned " + std::to_string(result) +
                    ", expected " + std::to_string(expected);
    return false;
  }
  return true;
}

// Run all tests
bool runAllTests() {
  // Define all test cases in one place
  std::vector<TestCase> testCases = {
      {6, 4, 23}, {30, 2, 65}, {8, 6, 49}, {16, 20, 333}};

  // Run all test cases
  std::string error_message;
  for (size_t i = 0; i < testCases.size(); ++i) {
    const auto& testCase = testCases[i];
    int a = std::get<0>(testCase);
    int b = std::get<1>(testCase);
    long long expected = std::get<2>(testCase);

    if (!runTest(a, b, expected, error_message)) {
      std::cerr << "Test " << (i + 1) << " failed: " << error_message
                << std::endl;
      return false;
    }
    std::cout << "Test " << (i + 1) << " passed: rectangle_rotation(" << a
              << ", " << b << ") == " << expected << std::endl;
  }

  std::cout << "All tests passed!" << std::endl;
  return true;
}

int main() {
  std::cout << "Running rectangle_rotation tests..." << std::endl;

  if (runAllTests()) {
    return 0;  // Success
  } else {
    return 1;  // Failure
  }
}
