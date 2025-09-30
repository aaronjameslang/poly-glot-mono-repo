public class Cli
{
  public static void Main(string[] args)
  {
    if (args.Length == 0)
    {
      Console.WriteLine("Please provide a number as a command line argument");
      return;
    }

    if (int.TryParse(args[0], out int n))
    {
      int result = Lib.Fibonacci(n);
      Console.WriteLine($"Fibonacci({n}) = {result}");
    }
    else
    {
      Console.WriteLine("Invalid input. Please provide a valid integer.");
    }
  }
}
