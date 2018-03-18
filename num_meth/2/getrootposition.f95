real function getrootposition(xlow,xhigh,tolerance)
  implicit none

  real,intent(inout) :: xlow,xhigh
  real,intent(in) :: tolerance
       
  real :: flow,fhigh
  real :: xint,fint,xintprev
  real,external :: equation
  logical,external :: signcheck 

  real :: error
       
  integer :: iteration=0

  if (xlow>xhigh) then
    call swap(xlow,xhigh)
  end if
  
  flow = equation(xlow)
  fhigh = equation(xhigh)
 
  if (flow==0) then
    getrootposition = xlow
    return
  end if

  if (fhigh==0) then
    getrootposition = xhigh
    return
  end if

  if (signcheck(flow,fhigh) .eqv. .false.) then
    stop "ERROR -Both have same sign-"
  end if

  do while (error > tolerance .or. iteration<2)
    iteration = iteration + 1
    xint = xhigh-((fhigh*(xhigh-xlow))/(fhigh-flow))
    fint = equation(xint)

    if (signcheck(fint,flow))then
      xhigh = xint
      fhigh = fint
    else if(signcheck(fint,fhigh)) then
      xlow = xint
      flow = fint
    else
      exit
    end if
    error = abs(xint-xintprev)
    xintprev = xint
  end do

  getrootposition = xint
  write(*,*) error
       
end function