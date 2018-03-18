subroutine swap(xlow,xhigh)
    implicit none

    real,intent(out) :: xlow,xhigh
    real :: swapper

    swapper = xhigh
    xhigh = xlow
    xlow = swapper

end subroutine