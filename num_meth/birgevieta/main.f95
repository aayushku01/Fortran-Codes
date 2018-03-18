      program main
       implicit none
       integer :: i
       integer :: degree
       real :: xguess
       real,dimension(:),allocatable :: a
       real,dimension(:),allocatable :: root
       real :: tolerance = 1.0e-7
    
       degree = 5

       allocate(a(degree+1))
       allocate(root(degree))

       a(1) = 1
       a(2) = -7
       a(3) = -3
       a(4) = 79
       a(5) = -46
       a(6) = -120
 
       xguess = 1000.0

       call method(a,root,xguess,tolerance,degree)
   
       do i=1,degree
          write(*,*) "Root ", root(i)
       end do

      end program main