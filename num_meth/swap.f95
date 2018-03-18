      subroutine swap(x,y)
       implicit none

       real,intent(out) :: x,y
       real :: swapper

       swapper = y
       y = x
       x = swapper


      end subroutine
