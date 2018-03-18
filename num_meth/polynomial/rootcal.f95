      subroutine rootcal(n,a,root)
        implicit none

        integer :: n
        real :: tolerance
        real ,dimension(n+1) :: a
        real ,dimension(n) :: root
        integer :: counter = 1,counter2 = 1,iter=0
        real :: xGuess = 1, xprev
        
        real,dimension(n+1) :: b,c

        do while(counter<n+1)
          b(counter2) = a(counter2)
          c(counter2) = b(counter2)
          counter2 = counter2 + 1

          do while(counter2<n+1)
            b(counter2) = a(counter2) + x*b(counter2-1)
            c(counter2) = b(counter2) + x*c(counter2-1)
            counter2 = counter2 + 1
          end do

          do while(error>tolerance .or. iter<2)
            iter = iter+1
            x = x - b(n+1)/c(n)








      end subroutine
