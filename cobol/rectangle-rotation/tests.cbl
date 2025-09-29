       identification division.
       program-id. tests.

       data division.
       working-storage section.
       01 a           pic 9(8).
       01 b           pic 9(8).
       01 result      pic 9(10).
       01 expected    pic 9(10).
       01 a-disp      pic z(7)9.
       01 b-disp      pic z(7)9.
       procedure division.
           move 1 to a
           move 1 to b
           move 1 to expected
           perform dotest
           move 2 to a
           move 2 to b
           move 5 to expected
           perform dotest
           move 6 to a
           move 4 to b
           move 23 to expected
           perform dotest
           move 30 to a
           move 2 to b
           move 65 to expected
           perform dotest
           move 8 to a
           move 6 to b
           move 49 to expected
           perform dotest
           move 16 to a
           move 20 to b
           move 333 to expected
           perform dotest
           goback.

       dotest.
           move a to a-disp
           move b to b-disp
           initialize result
           call 'solution'
               using by content a
               by content b
               by reference result
           display "Test: a = " a-disp
           display "b = " b-disp
           display "result = " result ", expected = " expected
           if result = expected
               display "  Test passed"
           else
               display "X Test FAILED"
           end-if.

       end program tests.
