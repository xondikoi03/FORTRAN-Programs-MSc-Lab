!!-----------------------------------------------------
!! This program performs interpolation of data points
!! Original Author: Raj Handique
!! Date Created: 16 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program INTERPOLATION_VELOCITY

  !!---------------------------------
  !! This program will require the user to declare all the variables and typecast explicitly !!
  !!---------------------------------

  implicit none

  !!---------------------------------
  !! Declare the variables
  !!---------------------------------
  real::X(10), Y(10), x_inplt, Y_pred
  integer::i,N,deg

  !!---------------------------------
  !! Analysis Block
  !!---------------------------------
  WRITE(*,*)"-----------------------------------"
  WRITE(*,*)"Enter the degree of polynomial: "
  READ(*,*) deg
  WRITE(*,20)"The degree of polynomial is: ", deg
  WRITE(*,*)"-----------------------------------"

  N = (deg+1)
  OPEN(UNIT=1, FILE="velocity_data.dat")

  DO i=1, 5
     READ(1,*) X(i), Y(i)
  ENDDO

  WRITE(*,*)"Enter the value of X to interpolate: "
  READ(*,*)x_inplt
  WRITE(*,*)"The entered value is: ", x_inplt

  call interpolate(X, Y, x_inplt, deg, Y_pred)

  WRITE(*,*)"----------------------------------------------------"
  WRITE(*,10)"The value of interpolated value of Y is: ", Y_pred
  WRITE(*,*)"----------------------------------------------------"

  !! FORMATING STATEMENTS

10 FORMAT(A, F15.4)
20 FORMAT(A, I2)

end program INTERPOLATION_VELOCITY

!!---------------------------------
!!  subroutines
!!---------------------------------


subroutine interpolate(X, Y, x_inplt, deg, Y_pred)

  implicit none
  real::X(10), Y(10), A(10), x_inplt  
  integer::i,j,k, deg, n
  real:: P, S

  real, INTENT(OUT)::Y_pred


  !! Interpolation Method !!

  n=(deg+1) !! Degree of Polynomial + 1 

  S=0

  !! Finding the points

  DO i=1, 5	
     IF(x_inplt<X(i)) THEN
        WRITE(*,*)"1 LOOP"
        A(i)=X(i-1)
        A(i+1)=X(i)
        A(i+2)=X(i+1)
        A(i+3)=X(i+2)
        A(i+4)=X(i+3)
        !!EXIT
     ELSE IF (x_inplt==5) THEN
        WRITE(*,*)"2 LOOP"
        A(i)=X(i-3)
        A(i+1)=X(i-2)
        A(i+2)=X(i-1)
        A(i+3)=X(i)
        A(i+4)=X(i+1)
        !!EXIT
     ENDIF
  ENDDO


  !! Evaluating the product
  DO i=1, n
     P=1
     DO j=1,n 
        IF(i .NE. j) THEN
           P = P * ((x_inplt - A(j))/(A(i) - A(j)))		
        ENDIF
     ENDDO
     S = S + (Y(i)*P) 
  ENDDO

  Y_pred = 0
  Y_pred = Y_pred+S

end subroutine interpolate

