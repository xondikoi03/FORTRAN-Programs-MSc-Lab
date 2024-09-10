!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 21 August, 2024     !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

program MATRIXMULT

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!!----------------------------------
!! Declaring the variables
!!----------------------------------

real :: A(10,10), B(10,10), MTX(10,10)
integer :: r1, r2, c1, c2, i, j, k, g


!! INTERFACE MESSAGE !!

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"This is program written to perform multiplication of two matrices"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 21 August, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"---------------------------------------------------------------------------"

g = 1
GOTO (100), g


!! MATRIX A

100 CONTINUE 

WRITE(*,*)"Enter the number of rows for Matrix A!"
READ(*,*) r1
WRITE(*,*)"------------------------------------------"
WRITE(*,20)"The number of rows for Matrix A:", r1
WRITE(*,*)"------------------------------------------"

WRITE(*,*)"Enter the number of columns for Matrix A!"
READ(*,*) c1

WRITE(*,*)"------------------------------------------"
WRITE(*,20)"The number of columns for Matrix A :", c1
WRITE(*,*)"------------------------------------------"

!! MATRIX B

WRITE(*,*)"Enter the number of rows for Matrix B!"
READ(*,*) r2
WRITE(*,*)"------------------------------------------"
WRITE(*,20)"The number of rows for Matrix A:", r2
WRITE(*,*)"------------------------------------------"

WRITE(*,*)"Enter the number of columns for Matrix B!"
READ(*,*) c2

WRITE(*,*)"------------------------------------------"
WRITE(*,20)"The number of columns for Matrix B :", c2
WRITE(*,*)"------------------------------------------"

!! Check whether r2 and c1 are equal (condition for matrix multiplication)

IF (c1==r2) THEN

	!! Reading the values of Matrix A
	
	WRITE(*,*)"Enter the values of Matrix A:"
	DO i=1,r1
		DO j=1, c1
			READ(*,*)A(i,j)
		ENDDO
	ENDDO
	WRITE(*,*)"------------------------------------------"
	DO i=1,r1
		WRITE(*,10)(A(i,j), j=1,c1)
	ENDDO
	WRITE(*,*)"------------------------------------------"
	
	!! Reading the values of Matrix B
	
	WRITE(*,*)"Enter the values of Matrix B:"
	DO i=1,r2
		DO j=1, c2
			READ(*,*)B(i,j)
		ENDDO
	ENDDO
	WRITE(*,*)"------------------------------------------"
	DO i=1,r2
		WRITE(*,10)(B(i,j), j=1,c2)
	ENDDO
	WRITE(*,*)"------------------------------------------"


	!! Matrix Multiplication - Process
	
	DO i=1,r1
		DO j=1, c2
			MTX(i,j) = 0 !! Initialising the matrix to have a value of zero (Summing purposes)
			DO k=1, c1
				MTX(i,j) = MTX(i,j) + (A(i,k)*B(k,j))
			ENDDO
		ENDDO
	ENDDO
	
	!! Show the output Matrix

	WRITE(*,*)"The Multiplication of the two matrices is shown below:"

	WRITE(*,*)"------------------------------------------"
	DO i=1,r1
		WRITE(*,10)(MTX(i,j), j=1,c2)
	ENDDO
	WRITE(*,*)"------------------------------------------"
ELSE
	WRITE(*,*)"--------------------------------------------------------------"
	WRITE(*,*)"!! ERROR MESSAGE !!"
	WRITE(*,*)"Row elements of MTX A do not match column elements of MTX B!!"
	WRITE(*,*)"!! PLEASE RE-ENTER THE VALUES !!"	
	WRITE(*,*)"--------------------------------------------------------------"
	GOTO 100
ENDIF


!! FORMATING

10 FORMAT (10F7.2)
20 FORMAT (A, I2)

end program MATRIXMULT
