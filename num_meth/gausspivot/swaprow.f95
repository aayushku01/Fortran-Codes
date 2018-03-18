subroutine swap(a,n,row1,row2)
    implicit none
    integer :: row1,row2,n,i
    real ,dimension(n,n+1) :: a
    real :: swapper
    
    do i=row1,n+1
       swapper = a(row1,i)
       a(row1,i) = a(row2,i)
       a(row2,i) = swapper
    end do

end subroutine