       identification division.
       program-id. solution.

       data division.
       working-storage section.
       01 area-red     pic 9(8).
       01 area-blue    pic 9(8).
       01 x            pic 9(8).
       01 y            pic 9(8).

       linkage section.
       01 a           pic 9(8).
       01 b           pic 9(8).
       01 result      pic 9(10).

       procedure division using a b result.
           move 0 to result

           move a to x
           move b to y
           call 'scale-red' using x
           call 'scale-red' using y
           compute area-red = x * y

           move a to x
           move b to y
           call 'scale-blue' using x
           call 'scale-blue' using y
           compute area-blue = x * y

           compute result = area-red + area-blue

           goback.

       end program solution.


       identification division.
       program-id. scale-red.

       data division.

       linkage section.
       01 x           pic 9(8).

       procedure division using x.
           compute x = x / 1.41421356
           compute x = x / 2
      *    floor x
           compute x = x * 2
           compute x = x + 1
           goback.

       end program scale-red.


       identification division.
       program-id. scale-blue.

       data division.

       linkage section.
       01 x           pic 9(8).

       procedure division using x.
           compute x = x / 1.41421356
           compute x = x + 1
           compute x = x / 2
      *    floor x
           compute x = x * 2
           goback.

       end program scale-blue.
