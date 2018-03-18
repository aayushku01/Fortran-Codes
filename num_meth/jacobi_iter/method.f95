subroutine method(a,x,n,xguess,tolerance)
    implicit none
    integer :: n
    real :: tolerance
    real,dimension(n,n+1) :: a
    real,dimension(n,n) :: ac    
    real,dimension(n) :: x , xguess,xprev,error
    logical :: solution = .false.
    
    logical,external :: dd

    integer :: counter1,counter2,iter = 0
    real :: factor

    do counter1=1,n
      do counter2=1,n
        ac(counter1,counter2) = a(counter1,counter2)
      end do
    end do

    if (dd(ac,n,n) .eqv. .false.) then
      stop "Matrix not Diagnally Dominant"
    endif

    x = xguess
    xprev = x

    do while(solution .eqv. .false.)
      iter = iter + 1

      write(*,*) "Iter ", iter
      do counter1=1,n
        factor = 0
        do counter2=1,n
          if (counter1 /= counter2) then
          factor = factor + (ac(counter1,counter2)*x(counter2))
          endif
        enddo
          !write(*,*) "Factor ",factor
          x(counter1) = (1/ac(counter1,counter1))*( a(counter1,(n+1)) - factor)
          !write(*,*) "Root ", x(counter1)
      enddo

      do counter1=1,n
        error(counter1) = abs(x(counter1) - xprev(counter1))
      enddo

      write(*,*) "Est.Soln"
      call print(x,n,1)

      write(*,*) "Error"
      call print(error,n,1)

      if (maxval(error) < tolerance) then
        solution  = .true.
      endif

      xprev = x
    end do


    
end subroutine method