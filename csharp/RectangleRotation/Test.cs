using NUnit.Framework;
using System;

[TestFixture]
public class Test
{
  [Test]
  public void BasicTests()
  {
    var kata = new Kata();

    Assert.That(kata.RectangleRotation(6, 4), Is.EqualTo(23));
    Assert.That(kata.RectangleRotation(30, 2), Is.EqualTo(65));
    Assert.That(kata.RectangleRotation(8, 6), Is.EqualTo(49));
    Assert.That(kata.RectangleRotation(16, 20), Is.EqualTo(333));
  }
}
