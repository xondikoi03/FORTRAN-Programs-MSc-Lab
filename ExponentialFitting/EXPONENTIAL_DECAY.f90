!!-----------------------------------------------------
!! This program is written to plot the exponential decay of a given sample
!! Original Author: Raj Handique
!! Date Created: 16 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program EXPONENTIAL_DECAY
  
  !!---------------------------------
  !! This program will require the user to declare all the variables and typecast explicitly !!
  !!---------------------------------
  
  implicit none
  
  !!---------------------------------
  !! Declare the variables
  !!---------------------------------
  
  real::T(40), N(40), Y_FIT(40)
  real::time, lambda, intercept, half_life, N0
  integer::i, j, num, time_counter
  
  !!---------------------------------
  !! Analysis Block
  !!---------------------------------

  i=1
  num=0
  time_counter=0
  OPEN(UNIT=2, FILE="exp_data.dat")

  !! READING A PREDEFINED DATAFILE !!
  DO
     IF(time_counter>150) EXIT
     READ(2,*) T(i), N(i)
     i=i+1
     time_counter=time_counter+10
     num=num+1
  ENDDO
  
  !! READ & WRITE OUT THE DATA FILES YOURSELF !!
  !!DO
  !!   IF(time_counter>150) EXIT
  !!   WRITE(*,*)"--------------------------------------------------------------------------"
  !!   WRITE(*,10)"Enter the values for number of nuclei in time(in minutes): ", time_counter
  !!   WRITE(*,*)"--------------------------------------------------------------------------"
  !!   READ(*,*) N(i)
  !!   WRITE(*,*)"--------------------------------------------------------------------------"
  !!   WRITE(*,20)"The entered value is: ", N(i)
  !!   T(i) = time_counter
  !!   WRITE(2,*) T(i), N(i)
     
  !!   i=i+1
  !!   time_counter=time_counter+10
  !!   num=num+1
  !!END DO

  WRITE(*,*)"------------------------------------------------"
  WRITE(*,10)"The number of times the decay happened: ", num

  call exp_fit(num, T, N, intercept, lambda)

  N0 = exp(intercept)
  lambda = (-1.0000)*lambda
  half_life = (0.693/lambda)
  
  WRITE(*,20)"The value of decay constant: ", lambda
  WRITE(*,20)"The value of initial amount of given sample: ", N0
  WRITE(*,20)"The half-life of the given sample(in minutes) :", half_life
  WRITE(*,*)"------------------------------------------------"

  !! FITTING & PLOTTING THE CURVE !!

  OPEN(UNIT=1,FILE='exp_fit.dat')
  
  DO j=1,num
     Y_FIT(j) = N0*exp(-(lambda)*T(j))
     WRITE(1,*) T(j), Y_FIT(j)
  END DO
  
  !! FORMATTING STATEMENTS !!
  10 FORMAT(A,I4)
  20 FORMAT(A, F8.4)
end program EXPONENTIAL_DECAY

!!---------------------------------
!! function sub-program
!!---------------------------------


subroutine exp_fit(num, time, N_dec, icpt, slp)

  implicit none
  real::N_dec(40), time(40)
  real::s1, s2, s3, s4, y_lin(40)
  integer::num, i

  real, INTENT(OUT)::icpt, slp

  s1=0
  s2=0
  s3=0
  s4=0

  DO i=1, num
     y_lin(i)= log(N_dec(i))
     s1=s1+(time(i)*y_lin(i))
     s2=s2+time(i)
     s3=s3+y_lin(i)
     s4=s4+(time(i)**2)
  END DO

  icpt = ((s4*s3) - (s2*s1))/((num*s4) - (s2**2))
  slp = ((num*s1) - (s2*s3))/((num*s4) - (s2**2))

end subroutine exp_fit

