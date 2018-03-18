subroutine print(a,n,m)
    implicit none
    real,dimension(n,m) :: a
    integer :: i,j,n,m

    do i=1,n
      do j=1,m
        write(*,10,advance = 'no') a(i,j)
      end do
      write(*,*) ""
    end do
    write(*,*) ""

    10 format(f7.3)

end subroutine print