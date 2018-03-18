      real function NewtonRaphson(xg,tolerance)
        implicit none
        real,intent(in) :: xg, tolerance

        real::x,xPrevious
        real::error
        real,external::getFunction,getDerivative

        integer::iter=0

        x=xg
        xPrevious=x

        do while(error>tolerance .or. iter<=2)
          iter=iter+1
          if(getDerivative(x) ==0) then
            stop "Error : Value of derivative "
          end if
          
          x = x-( getFunction(x) ) / (getDerivative(x) )
          
          error=x-xprevious
          xprevious=x
        end do

        NewtonRaphson=x

      end function NewtonRaphson
