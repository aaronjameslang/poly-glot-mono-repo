program DoubleTests;

uses
  Double,
  SysUtils;

procedure AssertEquals(Expected, Actual: Integer);
begin
  if Expected = Actual then
  begin
    WriteLn('PASS');
  end
  else
  begin
    WriteLn('FAIL');
    WriteLn('  Expected: ', Expected);
    WriteLn('  Actual: ', Actual);
  end;
end;

begin
  AssertEquals(0, Double.Double(0));
  AssertEquals(2, Double.Double(1));
  AssertEquals(4, Double.Double(2));
  AssertEquals(100, Double.Double(50));
end.
