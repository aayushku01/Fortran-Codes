      program a
       implicit none
       
       integer :: int1,int2,sum1,prod,expo1
       real :: divi
       real :: int3,int4
       integer , external :: sumOf,pro,expo
       real , external :: div

       write(*,*) 'Enter Numbers For Sum and Product :- '
       read(*,*) int1, int2


       int3 = int1
       int4 = int2

       sum1 = sumOf(int1,int2)
       prod = pro(int1,int2)
       divi = div(int3,int4)
       expo1 = expo(int1,int2)

       write(*,10) 'Sum Of',int1,' and ',int2,' is ',sum1,'.'
       write(*,10) 'Product Of',int1,' and ',int2,' is ',prod,'.'
       write(*,20) 'Division Of',int1,' and ',int2,' is ',divi,'.'
       write(*,10) 'Exponent Of ',int1,' and ',int2,' is ',expo1,"."
       10 format(a11,i2,a5,i2,a4,i7,a2)
       20 format(a11,i2,a5,i2,a4,f7.2,a2)
      end program a
