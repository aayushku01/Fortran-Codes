      program bubble_sort
         integer :: n,count,c,a
         real,dimension(:),allocatable :: list
         logical sorted
         a = 0

         write (*,*) "Enter list No Of Elements"
         read (*,*) n
         allocate(list(n))
         
         do count = 1,n,1
            write(*,*),'Enter the' ,count ,' element'
            read (*,*) list(count)
         end do



         do count = 1,n-1
             sorted = .true.
             do i = 1,n-1,1
                 do j = i,n,1
                     if (list(i) > list (j)) then
                          !integer :: c
                          c = list(j-1)
                          list(j-1) = list(j) 
                          list(j) = c
                          sorted = .false.
                                           
                     end if
                 end do
             end do
             if (sorted .eqv. .true. ) then
                 exit
             end if
         end do



       write(*,*) list      

       end program

