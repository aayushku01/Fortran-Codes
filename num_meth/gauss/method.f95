subroutine method(a,root,n)
  implicit none
  integer,intent(in) :: n
  real,dimension(n,n+1) :: a
  real,intent(out),dimension(n) :: root
  integer :: i,rowc,colmc,pivotr,pivotc,iter=0
  real :: factor,pivot

  do i = 1,n-1
    pivotr = i
    pivotc = i
    pivot = a(pivotr,pivotc)
    write(*,*) "Elimination"

    !write(*,*) pivotr,pivotc,pivot

    do rowc=pivotr+1,n
      factor = a(rowc,pivotc)/pivot
      !write(*,*) "factor = ",factor
      do colmc =pivotc,n+1
        !write(*,*) "Initail a",a(rowc,colmc)
        a(rowc,colmc) = a(rowc,colmc) - factor*a(pivotr,colmc)
        !write(*,*) a(rowc,colmc)
      end do  
    end do
    call print(a,n)
    factor = 0
  end do

  write(*,*) "Part 2 Back Sub"

  root(n) = a(n,n+1)/a(n,n)
  !write(*,*) "x(" , n , ") = " , root(n)


  do rowc=n-1,1,-1
    !write(*,*) "Evaluating x",rowc
    factor = 0

    do i=rowc+1,n
      factor = factor + a(rowc,i)*root(i)
    end do

    root(rowc) = (1/a(rowc,rowc))*(a(rowc,n+1) - factor ) 
    write(*,*) ""
  end do


end subroutine method