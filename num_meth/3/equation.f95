real function equation(xlow)
    implicit none

    real,intent(in) :: xlow
    !real,intent(out) :: equation
       
    equation = 3*xlow+sin(xlow)-exp(xlow)
end function