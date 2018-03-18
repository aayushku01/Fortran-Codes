subroutine method(aIn,root,xguess,tolerance,degree)
    implicit none
    real,intent(in) :: xguess,tolerance
    integer,intent(in) :: degree
    real,intent(in),dimension(degree+1) :: aIn
    real,intent(out),dimension(degree) :: root

    real,dimension(:),allocatable :: a,b,c
    real :: x,xprev,error
    integer :: i,rootleft,iter=0

    allocate(a(degree+1))
    allocate(b(degree+1))
    allocate(c(degree))

    a = aIn
    b=0
    c=0

    rootleft = degree
    xprev = xguess;

    do while(rootleft>0)
       iter=0
       x = xguess

       do while((error>tolerance) .or. (iter<=2))
          iter = iter+1
          b(1) = a(1)
          do i = 2,size(b)
            b(i) = a(i) + (x*b(i-1))
          end do
          c(1) = b(1)
          do i=1,size(c)
            c(i) = b(i) + (x*c(i-1))
          end do
          if (size(c)==0) then
            stop "Size C = 0"
          end if

          x = x - b(size(b))/c(size(c))

          error = abs(x - xprev )
          xprev = x
       end do

       rootleft = rootleft - 1
       root(degree - rootleft) = x

       if (rootleft==1) then
         root(degree) = -(b(2)/b(1))
         return
       endif

       deallocate(a)
       allocate(a(rootleft+1))

       do i=1,rootleft+1
         a(i) = b(i)
       end do

       deallocate(b)
       deallocate(c)
       allocate(b(rootleft+1))
       allocate(c(rootleft))

       b=0
       c=0

    end do


end subroutine method