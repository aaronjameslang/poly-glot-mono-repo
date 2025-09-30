using NUnit.Framework;

[TestFixture]
public class Test
{
  private static int Fib(int n) => Lib.Fibonacci(n);

  [TestCase(0, 0)]
  [TestCase(1, 1)]
  [TestCase(2, 1)]
  [TestCase(3, 2)]
  [TestCase(4, 3)]
  [TestCase(5, 5)]
  [TestCase(6, 8)]
  [TestCase(7, 13)]
  [TestCase(8, 21)]
  [TestCase(9, 34)]
  [TestCase(10, 55)]
  public void Fibonacci_VariousInputs_ReturnsExpectedResults(int input, int expected)
  {
    int actual = Fib(input);
    Assert.That(actual, Is.EqualTo(expected));
  }
}
