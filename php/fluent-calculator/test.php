<?php

include 'kata.php';

use PHPUnit\Framework\TestCase;

class MyTest extends TestCase
{
  public function testBasicValueTests()
  {
    $this->assertNotNull(FluentCalculator::init());
    $this->assertSame(FluentCalculator::init()->zero(), 0);
    $this->assertSame(FluentCalculator::init()->one(), 1);
    $this->assertSame(FluentCalculator::init()->two(), 2);
    $this->assertSame(FluentCalculator::init()->three(), 3);
    $this->assertSame(FluentCalculator::init()->four(), 4);
    $this->assertSame(FluentCalculator::init()->five(), 5);
    $this->assertSame(FluentCalculator::init()->six(), 6);
    $this->assertSame(FluentCalculator::init()->seven(), 7);
    $this->assertSame(FluentCalculator::init()->eight(), 8);
    $this->assertSame(FluentCalculator::init()->nine(), 9);
    $this->assertSame(FluentCalculator::init()->one->zero(), 10);
    $this->assertSame(FluentCalculator::init()->minus->three->zero(), -30);
    $this->assertSame(FluentCalculator::init()->nine->nine->nine->nine->nine->nine->nine->nine->nine(), 999999999);
  }
  public function testBasicOperationTests()
  {
    $this->assertSame(FluentCalculator::init()->two->one->plus(), 21);
    $this->assertSame(FluentCalculator::init()->two->one->plus->three(), 24);
    $this->assertSame(FluentCalculator::init()->one->minus->three(), -2);
    $this->assertSame(FluentCalculator::init()->two->times->four->five(), 90);
    $this->assertSame(FluentCalculator::init()->three->three->dividedBy->six(), 5);
    $this->assertSame(FluentCalculator::init()->two->one->plus->three->times(), 24);
    $this->assertSame(FluentCalculator::init()->one->minus->three->times(), -2);
    $this->assertSame(FluentCalculator::init()->two->times->four->five->minus(), 90);
    $this->assertSame(FluentCalculator::init()->three->three->dividedBy->six->dividedBy(), 5);
    $this->assertSame(FluentCalculator::init()->two->one->plus->dividedBy->three(), 7);
    $this->assertSame(FluentCalculator::init()->one->zero->times->minus->three->three(), -23);
    $this->assertSame(FluentCalculator::init()->two->times->minus->four->five->seven(), -455);
  }
  public function testMoreThanOneOperations()
  {
    $this->assertSame(55, FluentCalculator::init()->one->zero->plus->seven->plus->four->plus->one->plus->zero->plus->two->plus->nine->plus->five->plus->eight->plus->six->plus->three());
    $this->assertSame(-35, FluentCalculator::init()->five->minus->one->minus->nine->minus->two->minus->eight->minus->seven->minus->three->minus->one->zero->minus->zero());
    $this->assertSame(362880, FluentCalculator::init()->seven->times->three->times->two->times->nine->times->one->times->eight->times->four->times->six->times->five());
    $this->assertSame(0, FluentCalculator::init()->one->zero->dividedBy->one->dividedBy->two->dividedBy->five->dividedBy->six->dividedBy->seven->dividedBy->four());
    $this->assertSame(4, FluentCalculator::init()->zero->plus->three->plus->one());
    $this->assertSame(14, FluentCalculator::init()->one->minus->one->zero->dividedBy->nine->plus->three->times->seven());
    $this->assertSame(15, FluentCalculator::init()->one->plus->two->dividedBy->three->times->one->zero->minus->three->plus->eight());
    $this->assertSame(-4, FluentCalculator::init()->three->dividedBy->six->times->one->zero->plus->three->minus->seven());
  }
  public function testShouldThrowInvalidInputException1()
  {
    $this->expectException(InvalidInputException::class);
    FluentCalculator::init()->limit();
  }
  public function testShouldThrowInvalidInputException2()
  {
    $this->expectException(InvalidInputException::class);
    FluentCalculator::init()->power();
  }
  public function testShouldThrowInvalidInputException3()
  {
    $this->expectException(InvalidInputException::class);
    FluentCalculator::init()->sin();
  }
  public function testShouldThrowInvalidInputException4()
  {
    $this->expectException(InvalidInputException::class);
    FluentCalculator::init()->cos();
  }
  public function testShouldThrowDigitCountOverflowException1()
  {
    $this->expectException(DigitCountOverflowException::class);
    FluentCalculator::init()->one->two->three->four->five->six->seven->eight->nine->zero();
  }
  public function testShouldThrowDigitCountOverflowException2()
  {
    $this->expectException(DigitCountOverflowException::class);
    FluentCalculator::init()->one->two->three->four->five->six->seven->eight->nine->times->one->two();
  }
  public function testShouldThrowDigitCountOverflowException3()
  {
    $this->expectException(DigitCountOverflowException::class);
    FluentCalculator::init()->nine->nine->nine->nine->nine->nine->nine->nine->nine->plus->one();
  }
  public function testShouldThrowDigitCountOverflowException4()
  {
    $this->expectException(DigitCountOverflowException::class);
    FluentCalculator::init()->one->zero->zero->zero->zero->zero->times->one->zero->zero->zero->zero->zero();
  }
  public function testShouldThrowDivisionByZeroException()
  {
    $this->expectException(DivisionByZeroException::class);
    FluentCalculator::init()->one->zero->zero->zero->zero->zero->dividedBy->zero();
  }
  public function testX()
  {
    $expected = -450113334;
    $actual = FluentCalculator::init()->dividedBy->plus->minus->one->nine->seven->six->plus->seven->times->eight->dividedBy->times->five->plus->dividedBy->nine->five->seven->minus->three->six->four->one->four->times->minus->three->zero->two->times->zero->five->five->six->minus->nine->times->two->two->dividedBy->plus();
    $this->assertSame($expected, $actual);
  }
  public function testX2()
  {
    // Leading zer test, octal
    $expected = 18616200;
    $actual = FluentCalculator::init()->zero->six->six->five->dividedBy->three->minus->eight->times->four->seven->five->times->one->eight->four->minus->dividedBy();
    $this->assertSame($expected, $actual);
  }
  public function testX3()
  {
    // Leading zero test, long
    $expected = 216080653;
    $actual = FluentCalculator::init()->zero->two->one->six->zero->eight->zero->six->five->eight->plus->one->minus->six->plus();
    $this->assertSame($expected, $actual);
  }
}
