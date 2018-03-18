      real function NewtonRaphson(x1g,x2g,tolerance)
        implicit none
        real,intent(in) :: x1g,x2g,tolerance
        real,external :: getFunction
        
        real::xOne,xtwo,xThree,xThreePrev
        real::fOne,fTwo,FThree
        real::error
        real::slope
        integer::iter
        xOne=x1g
        xTwo=x2g

        fOne=getFunction(xOne)
        fTwo=getFunction(xTwo)

        do while(error>tolerance .or. iter<=2)
          iter=iter+1
          slope =(fOne-fTwo)/(xOne-xTwo)
          xThree = xOne- (fOne/slope)
          xOne=xTwo
          fOne=fTwo
          xTwo=xThree
          fThree=getFunction(xThree)
          fTwo=fThree
          !xThree = xTwo- (fTwo/slope)
          error=abs(xThree-xThreePrev)
          write(*,*)xThree
          xThreePrev=xThree
        end do

        NewtonRaphson=xThree
      end function NewtonRaphson
