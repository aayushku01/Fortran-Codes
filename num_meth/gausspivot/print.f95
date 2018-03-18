subroutine print(a,n)
    implicit none
    real,dimension(n,n+1) :: a
    integer :: i,j,n

    do i=1,n
      do j=1,n+1
        write(*,10,advance = 'no') a(i,j)
      enddo
      write(*,*) ""
    enddo
    write(*,*) ""

    10 format(f20.10)

end subroutine print