      real function abc(tolerance)
       implicit none

       real :: x,y,z,xf,yf,zf
       real :: root
       integer :: i,j
       real :: error
       real,intent(in) :: tolerance
       real,external :: equation
       logical,external :: signcheck

       write(*,*) "Enter Number 1"
       read(*,*) x
       write(*,*) "Enter Number 2"
       read(*,*) y
       !write(*,*) "Enter Number of iterations"
       !read(*,*) j

       if (x>y) then
        call swap(x,y)
       end if

       xf = equation(x)
       yf = equation(y)
 
       if (xf == 0 ) then
          root = x
          return
       end if

       if (yf == 0 ) then
          root = y
          return
       end if

       if (signcheck(xf,yf) .eqv. .false.) then
           stop
       end if

       z = (x+y)/2
       zf = equation(z)
      
       error = (y-x)/2

       write (*,*) "|","LOW","|","HIGH","|","MID","|","FLOW","|","FHIGH","|","FMID","|"
       ! WRITE A FORMAT STATEMENT
       !1 format(a1,a7,a1,a8,a1,a7,a1,a10,a1,a10,a1,a10,a1)

       j = 0
       do while (error > tolerance)
         j = j+1

         write (*,*) "|",x,"|",y,"|",z,"|",xf,"|",yf,"|",zf,".",j
         ! WRITE A FORMAT STATEMENT

         z = (x+y)/2
         zf = equation(z)
         if (signcheck(zf,xf))then
            y = z
            yf = zf
         else if(signcheck(zf,yf)) then
            x = z
            xf = zf
         else
            exit
         end if
         error = (y - x)/2

       end do
       !2 format(a1,f2.4,a1,f2.5,a1,f2.4,a1,f2.7,a1,f2.7,a1,f2.7,a1)

       write(*,*) error
       root = z
      
      end function
