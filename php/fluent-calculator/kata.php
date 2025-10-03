<?php

class DigitCountOverflowException  extends Exception {}
class DivisionByZeroException extends Exception {}
class InvalidInputException extends Exception {}

/**
 * This would be cleaner without all the string manipulations,
 *   but it works for now
 */
class FluentCalculator
{

  public static function init()
  {
    return new FluentCalculator();
  }

  private $code = '';

  public function __get($property)
  {

    $operators = ['plus' => '+', 'minus' => '-', 'times' => '*', 'dividedBy' => '/'];
    if (array_key_exists($property, $operators)) {
      $operator = $operators[$property];
      $n = $this->value();
      $this->code = $n . $operator;
      return $this;
    }

    $digits = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];
    if (in_array($property, $digits)) {
      $digit = array_search($property, $digits);
      if (substr($this->code, -1) === '0' && in_array(substr($this->code, -2, 1), ['+', '-', '*', '/'])) {
        $this->code = substr($this->code, 0, -1);
      }
      if ($this->code === '0') {
        $this->code = '';
      }
      $this->code .= (string)$digit;
      $this->check();
      return $this;
    }
  }

  public function __call($name, $arguments)
  {
    if ($name ===  'value') {
      if (in_array(substr($this->code, -1), ['+', '-', '*', '/'])) {
        $this->code = substr($this->code, 0, -1);
      }
      if (preg_match('/^0\d/', $this->code)) {
        $this->code = substr($this->code, 1);
      }
      if (substr($this->code, -2) === '/0') {
        throw new DivisionByZeroException("Division by zero");
      }
      $n = $this->code == '' ? 0 : (int)eval('return ' . $this->code . ';');

      if (preg_match('/\d{10}$/', $n)) {
        $msg = "Digit count overflow, n: $n";
        throw new DigitCountOverflowException($msg);
      }
      return (int)($n);
    }

    if ($name ===  'check') {
      if (preg_match('/\d{10}$/', $this->code)) {
        $msg = "Digit count overflow, code: $this->code";
        throw new DigitCountOverflowException($msg);
      }
      return;
    }

    $_ = ($this->$name);

    if ($_ == null) {
      throw new InvalidInputException("Invalid method: $name");
    }

    $n = $this->value();
    return $n;
  }
}
