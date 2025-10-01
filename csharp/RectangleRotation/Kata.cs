public class Kata
{
  public int RectangleRotation(int a, int b)
  {
    int areaRed = scaleRed(a) * scaleRed(b);
    int areaBlue = scaleBlue(a) * scaleBlue(b);
    return areaRed + areaBlue;
  }

  private int scaleRed(int x)
  {
    x = (int)(x / Math.Sqrt(2));
    x /= 2;
    // floor
    x *= 2;
    x += 1;
    return x;
  }

  private int scaleBlue(int x)
  {
    x = (int)(x / Math.Sqrt(2));
    x += 1;
    x /= 2;
    // floor
    x *= 2;
    return x;
  }
}
