        program budget
         implicit none
         real,dimension(:) , allocatable :: e
         character(len = 10) , dimension(:) , allocatable :: cat
         integer :: catcount,n
         real , external :: a
         real :: sumof
         do
            write (*,*) 'Enter The Number of Categories'
            read (*,*) n
            if (n <0) then 
               write (*,*) '----ERROR---'
            else
               exit
            end if
         end do

         allocate(cat(n))
         allocate(e(n))

         do catcount = 1,n,1
            write(*,3),'Enter the' ,catcount ,' category and expense'
            read (*,*) cat(catcount) , e(catcount)
         end do

         write (*,*) 'Budget 2k17'
         write (*,2) 'Category  ','|','Expense'

         do catcount = 1,n,1 
            write (*,1) cat(catcount) ,'|', e(catcount)
         end do
         sumof = a(e,n)
         write(*,*) "Sum Of Expense = ",sumof
         1 format (a10,a1,f10.2)
         2 format (a10,a1,a10)
         3 format (a10,i2,a21)
        end program
