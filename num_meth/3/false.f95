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

