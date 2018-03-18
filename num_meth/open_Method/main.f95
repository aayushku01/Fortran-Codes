      program main
        implicit none
        
        real,external :: NewtonRaphson
        real::xg
        real,parameter :: tolerance =1.0e-5
        real :: rootNR
        
        xg= 0.0
        rootNR=NewtonRaphson(xg,tolerance)

        write(*,*) rootNR
      end program main       
