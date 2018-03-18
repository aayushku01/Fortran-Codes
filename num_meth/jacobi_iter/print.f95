subroutine print(a,n,m)
    implicit none
    real,dimension(n,m) :: a
    integer :: i,j,n,m

    do i=1,n
      do j=1,m
        write(*,10,advance = 'no') a(i,j)
      enddo
      write(*,*) ""
    enddo
    write(*,*) ""

    10 format(f20.10)

end subroutine print