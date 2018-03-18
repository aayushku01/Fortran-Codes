       logical function signcheck(x,y)

       implicit none

       real :: x,y

       signcheck = .false.

       if ((x*y)<0) then
          signcheck = .true.
       end if

      end function
