,>,<
[ Loop while not 0
  > Goto 1
  Move 1 to 2 and 3; so |a|0|b|b|; effecitvely adding b to @2 each loop
  [>+>+<<-]
  >> Goto 3
  [-<<+>>] Move 3 to 1; so |a|b|b|
  <<< Goto 0
  - drop @0
]
>> Goto 2
.
