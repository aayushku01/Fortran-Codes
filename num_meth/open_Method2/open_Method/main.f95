      program main
        implicit none
        
        real,external :: NewtonRaphson
        real::x1g,x2g
        real,parameter :: tolerance =1.0e-5
        real :: rootNR
        
        x1g= 0.0
        x2g= 1.0
        rootNR=NewtonRaphson(x1g,x2g,tolerance)

        write(*,*) rootNR
      end program main       
