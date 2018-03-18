      program student
        implicit none
        
        integer :: n,i
        character(len = 10),dimension(:),allocatable :: names
        real,allocatable,dimension(:) :: points

        write (*,*) "Enter No Of students"
        read (*,*) n

        allocate(names(n))
        allocate(points(n))

        do i = 1,n
          write (*,*) "Enter Name Of ",i,"Student"
          read (*,*) names(i)
          write (*,*) "Enter points Of ",i,"Student"
          read (*,*) points(i)
        end do  


        call printd(n,names,points)

        write (*,*) "Sorted"
 
        call sortedp(n,names,points)

      end program student


      subroutine printd(n,a,b)
         
        implicit none
        character(len = 10),dimension(n) :: a
        real,dimension(n) :: b
        integer :: n,i
        
        
        do i = 1,n
           write (*,1) a(i),' | ',b(i)
        end do
        1 format(a10,a3,f5.2)
      end subroutine

      subroutine sortedp(n,a,b)
        implicit none
        character(len = 10),dimension(n) :: a
        real,dimension(n) :: b
        integer :: n,i,c,count,j
        character :: temp
        logical sorted

        do count = 1,n-1
           sorted = .true.
             do i = 1,n-1,1
                 do j = i,n,1
                     if (b(i) > b(j)) then
                          !integer :: c
                          c = b(j-1)
                          b(j-1) = b(j)
                          b(j) = c
                          sorted = .false.

                          temp = a(j-1)
                          a(j-1) = a(j)
                          a(j) = temp

                     end if
                 end do
             end do
             if (sorted .eqv. .true. ) then
                 exit
             end if
         end do

         do i = 1,n
            write (*,1) a(i),' | ',b(i)
         end do
         1 format(a10,a3,f5.2)

      end subroutine
