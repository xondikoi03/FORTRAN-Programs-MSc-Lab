!!-----------------------------------------------------
!! This program performs Euler's method for numerical solution of 1st order differential equation (Both simple & modified Euler)
!! Original Author: Raj Handique
!! Date Created: 13 November 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program DIFFERENTIAL_EQUATION_FIRST_ORDER

  !!---------------------------------
  !! This program will require the user to declare all the variables and typecast explicitly !!
  !!---------------------------------

  implicit none

  !!---------------------------------
  !! Declare the variables
  !!---------------------------------

  real::df, t, y, h, f 

  !!---------------------------------
  !! Analysis Block
  !!---------------------------------

  WRITE(*,*)"The given program finds the solution for the differential equation involving current through the inductor."
  
  WRITE(*,*)"Enter the initial value of time"
  READ(*,*) t
  WRITE(*,*)"------------------------------"
  WRITE(*,*)"The initial value of time: ", t
  WRITE(*,*)"------------------------------"

  WRITE(*,*)"Enter the inital value of current:"
  READ(*,*) y
  WRITE(*,*)"------------------------------"
  WRITE(*,*)"The initial value of current:", y
  WRITE(*,*)"------------------------------"

  WRITE(*,*)"Enter the value of increment"
  READ(*,*) h
  WRITE(*,*)"------------------------------"
  WRITE(*,*)"The number of increment: ", h
  WRITE(*,*)"------------------------------"


  OPEN(UNIT=1,file='eulerdata.dat')
  
  call euler(t, y, h)

end program DIFFERENTIAL_EQUATION_FIRST_ORDER

!!---------------------------------
!! function sub-program
!!---------------------------------

real function f(t)
  implicit none
  !! t - time, I - Current, E - EMF, R - Resistance, L - Inductance
  real::t, c, R, E, L
  R = 10.0
  E = 20.0
  L = 5.0

  c = (E/R) * (1 - exp((-R * 0.5)/ L))
  c = c / exp((-R * 0.5)/ L)

  if(t < 0.5) then
     f = (E/R) * (1 - exp((-R * t)/L))
  else
     f = c * exp((-R * t)/L)
  endif
  return 
end function f

!! Differential Equation

real function df(t, I)
  real:: t, I, R, L, E 

  R = 10.0
  E = 20.0
  L = 5.0
  
  if(t < 0.5) then
     df = (E/L) - ((R * I)/ L)
  else
     df = -((R * I)/ L)
  endif
end function df

!!---------------------------------
!! sub-routine
!!---------------------------------

subroutine euler(t, y, h)

  implicit none
  real::t, y, y_mod
  real::v, h, df, f, val, mod_val
  integer::n, i, j

  !! Storing Initial Values !!
  n = 500
  DO i=1, n
     val = df(t,y) + df(t+h,y)
     mod_val = mod_val/2.0

     y_mod = y + (h*mod_val)
     y = y + (h*df(t, y))

     t = t + h
     WRITE(1,*) t, y, f(t), y_mod 
  END DO
end subroutine euler
