      real function a(number,n)
       implicit none

       integer :: b,c,n
       real,dimension(n),intent(in) :: number
       !a = number
       b = 0
       do c = 1,n,1
         b = b + number(c) 
       end do
       a = b
      end function
