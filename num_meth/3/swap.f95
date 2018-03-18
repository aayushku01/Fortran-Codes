subroutine swap(xlow,xhigh)
    implicit none

    real,intent(inout) :: xlow,xhigh
    real :: swapper

    swapper = xhigh
    xhigh = xlow
    xlow = swapper

end subroutine
