program matmul
   implicit none
   
   real,dimension(:,:),allocatable :: a,b,c
   integer :: n,m,i,j,k

   write(*,*) "Enter No Of Rows and Columns"
   read(*,*) m
   read(*,*) n

   allocate(a(m,n))
   allocate(b(n,m))
   allocate(c(m,m))

   c = 0

   do i=1,m
     do j=1,n
       write(*,10) "Enter a(",i,j,")"
       read(*,*) a(i,j)
     end do
   end do
   do i=1,n
     do j=1,m
       write(*,10) "Enter b(",i,j,")"
       read(*,*) b(i,j)
     end do
   end do

   do i=1,m
     do j=1,m
       write(*,20) "Evaluating c(",i,j,")" 
       c(i,j) = 0
       do k=1,n
         c(i,j) = c(i,j) + a(i,k)*b(k,j)
       end do
     end do
   end do

   write(*,*) ""
   call print(c,m,m)

   10 format(a8,i2,i2,a2)
   20 format(a13,i2,i2,a2)

end program