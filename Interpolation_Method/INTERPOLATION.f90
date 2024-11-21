!!-----------------------------------------------------
!! This program performs interpolation of data points
!! Original Author: Raj Handique
!! Date Created: 16 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program INTERPOLATION

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

DO i=1, N
	WRITE(*,*)"---------------------------------"
	WRITE(*,20)"Enter the value for Y for X-",i
	READ(*,*) Y(i)
	WRITE(*,10)"The entered value is: ", Y(i)
	WRITE(*,*)"---------------------------------"
	X(i) = i
ENDDO

WRITE(*,*)"Enter the value of X to interpolate: "
READ(*,*)x_inplt
WRITE(*,*)"The entered value is: ", x_inplt

call interpolate(X, Y, x_inplt, deg, Y_pred)

WRITE(*,*)"----------------------------------------------------"
WRITE(*,10)"The value of interpolated value of Y is: ", Y_pred
WRITE(*,*)"----------------------------------------------------"

!! FORMATING STATEMENTS

10 FORMAT(A, F8.4)
20 FORMAT(A, I2)

end program INTERPOLATION

!!---------------------------------
!!  subroutines
!!---------------------------------


subroutine interpolate(X, Y, x_inplt, deg, Y_pred)

implicit none
real::X(10), Y(10), x_inplt  
integer::i,j, deg, n
real:: P, S

real, INTENT(OUT)::Y_pred

!! Interpolation Method !!

n=(deg+1) !! Degree of Polynomial + 1 

S=0

!! Evaluating the product
DO i=1, n
	P=1
	DO j=1, n
		IF(i .NE. j) THEN
			P = P * ((x_inplt - X(j))/(X(i) - X(j)))		
		ENDIF
	ENDDO
	S = S + (Y(i)*P) 
ENDDO

Y_pred = 0
Y_pred = Y_pred+S

end subroutine

