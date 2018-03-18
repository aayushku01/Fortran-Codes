integer function getmax(a,n,column,rowstart)
    implicit none
    real,dimension(n,n+1) :: a
    integer :: i,n,column,rowstart,maxv

    getmax = rowstart
    maxv = abs(a(rowstart,column))
    
    do i=rowstart,n
      if (abs(a(i,column))>maxv) then
        getmax = i
        maxv = abs(a(i,column))
      end if
    end do

    10 format(f20.10)
    
end function