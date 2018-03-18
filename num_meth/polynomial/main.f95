      function main
        implicit none
        integer :: n,counter = 1
        real :: tolerance = 1e-4
        real, allocatable ,dimension(:) :: poly,root

        write(*,*) "Enter n"
        read(*,*) n

        allocate(poly(n+1))
        allocate(root(n))

        do while(counter<n+1)
          read(*,*) poly(counter)
          counter = counter + 1
        end do

        call rootcal(n,poly,root,tolerance)
        counter = 1
         
        do while(counter<n+1)
          write(*,*) root(counter)
          counter = counter + 1
        end do
  
 
      end main
