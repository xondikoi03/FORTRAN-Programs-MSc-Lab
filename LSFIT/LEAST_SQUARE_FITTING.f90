!!-----------------------------------------------------
!! This program is written to perform the Least Sqaure Fitting of data points
!! Original Author: Raj Handique
!! Date Created: 16 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program LEAST_SQUARE_FITTING

  !!---------------------------------
  !! This program will require the user to declare all the variables and typecast explicitly !!
  !!---------------------------------

  implicit none

  !!---------------------------------
  !! Declare the variables
  !!---------------------------------

  real::intercept, slope
  real::X(40), Y(40), x_fit(40), y_fit(40)
  integer:: i, j, n_counter
  !!---------------------------------
  !! Analysis Block
  !!---------------------------------
  
  OPEN(UNIT=1, FILE="lsf_data.dat")
  i=1
  j=4
  n_counter=0

  !! READING A PREDEFINED DATAFILE !!

  DO
     IF(j>19) EXIT
     READ(1,*) X(i), Y(i)
     i=i+1
     j=j+1
     n_counter=n_counter+1
  ENDDO
  
  !! READ THE DATA FILES YOURSELF !!
  
  !!DO
    !! IF(j>19) EXIT
    !! WRITE(*,*)"-------------------------------------"
    !! WRITE(*,10)"Enter the charge on the drop: ", j
    !! WRITE(*,*)"-------------------------------------"
    !! READ(*,*) Y(i)
    !! WRITE(*,*)"-------------------------------------"
    !! WRITE(*,20)"The entered value is: ", Y(i)
    !! X(i) = j
    !! WRITE(1,*) X(i), Y(i)
     
    !! i=i+1
    !! j=j+1
    !! n_counter=n_counter+1
  !!ENDDO

  call lsfit(X, Y, n_counter, intercept, slope)
  
  !! FITTING THE LINE !!
  OPEN(UNIT=2, FILE='lsf_fit.dat')
  DO i=1, n_counter
     x_fit(i) = slope*X(i)
     y_fit(i) = x_fit(i) + intercept
     WRITE(2,*) X(i), y_fit(i)
  ENDDO

  !! FORMATTING STATEMENTS !!
  10 FORMAT(A,I4)
  20 FORMAT(A, F8.4)

  
end program LEAST_SQUARE_FITTING

!!---------------------------------
!! function sub-program
!!---------------------------------


subroutine lsfit(X, Y, num, icpt, slp)

  implicit none
  real::X(40), Y(40)
  real::s1, s2, s3, s4
  integer::num, i

  real, INTENT(OUT)::icpt, slp

  s1=0
  s2=0
  s3=0
  s4=0

  DO i=1, num
     s1=s1+(X(i)*Y(i))
     s2=s2+X(i)
     s3=s3+Y(i)
     s4=s4+(X(i)**2)
  END DO

  icpt = ((s4*s3) - (s2*s1))/((num*s4) - (s2**2))
  slp = ((num*s1) - (s2*s3))/((num*s4) - (s2**2))

end subroutine lsfit

