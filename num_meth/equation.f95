      real function equation(x)
       implicit none

       real,intent(in) :: x
       !real,intent(out) :: equation
       
       equation = 3*x+sin(x)-exp(x)

      end function
