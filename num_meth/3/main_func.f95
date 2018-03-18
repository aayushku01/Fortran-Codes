program main
    implicit none
        
    real :: xlow,xhigh
    real , parameter :: tolerance = 1.0e-6
    real :: root
    real ,external :: getrootposition

    xlow = 0
    xhigh = 1

    root = getrootposition(xlow,xhigh,tolerance)

    write(*,*) "Root = ",root
 
end program
