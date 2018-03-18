      program main
        implicit none
        
        real :: x,y
        real , parameter :: tolerance = 1.0e-6
        real :: root

        real ,external :: abc

        root = abc(tolerance)

        write(*,*) "Root = ",root
 
      end program 
