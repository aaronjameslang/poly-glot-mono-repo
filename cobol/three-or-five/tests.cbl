       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N          PIC S9(8).
       01 RESULT     PIC 9(8).
       01 EXPECTED   PIC 9(8).
       01 N-DISP     PIC -(7)9.
       PROCEDURE DIVISION.
       TESTSUITE.
           Move 10 to n,
           Move 23 to expected,
           Perform DOTEST
           Move 20 to n,
           Move 78 to expected,
           Perform DOTEST
           Move 200 to n,
           Move 9168 to expected,
           Perform DOTEST
           Move -1 to n,
           Move 0 to expected,
           Perform DOTEST
           Move 0 to n,
           Move 0 to expected,
           Perform DOTEST
           Move 1 to n,
           Move 0 to expected,
           Perform DOTEST
           Move 2 to n,
           Move 0 to expected,
           Perform DOTEST
           Move 3 to n,
           Move 0 to expected,
           Perform DOTEST
           Move 4 to n,
           Move 3 to expected,
           Perform DOTEST
           Move 5 to n,
           Move 3 to expected,
           Perform DOTEST
           Move 6 to n,
           Move 8 to expected,
           Perform DOTEST
           GOBACK.

       DOTEST.
           Move n to n-disp
           Initialize result
           Call 'SOLUTION'
               using by content n
               by reference result
           DISPLAY "Test: n = " n-disp
           DISPLAY "result = " result ", expected = " expected
           IF result = expected
               DISPLAY "  Test passed"
           ELSE
               DISPLAY "X Test FAILED"
           END-IF.

       END PROGRAM TESTS.
