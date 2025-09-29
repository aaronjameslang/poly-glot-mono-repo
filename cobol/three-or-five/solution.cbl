      * Count the sum of all numbers from 1 to N that are multiples of 3 or 5
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLUTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 R3    PIC 9(1) VALUE 0.
       01 R5    PIC 9(1) VALUE 0.
       01 I        PIC 9(3) VALUE 0.

       LINKAGE SECTION.
       01 N           PIC S9(8).
       01 RESULT      PIC 9(8).

       PROCEDURE DIVISION USING N RESULT.
       MAIN-PROCEDURE.
           MOVE 0 TO RESULT
           MOVE 0 TO I
           PERFORM LOOP UNTIL I >= N
           GOBACK.

       LOOP.
           COMPUTE R3 = FUNCTION MOD(I, 3)
           COMPUTE R5 = FUNCTION MOD(I, 5)

           IF R3 = 0 OR R5 = 0
               ADD I TO RESULT
           END-IF

           ADD 1 TO I.
