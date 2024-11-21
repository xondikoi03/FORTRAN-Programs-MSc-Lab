!!-----------------------------------------------------
!! PRN: 24021073
!! Original Author: Raj Handique
!! Date Created: 17 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program GAUSS_ELIMINATION_METHOD

!!---------------------------------
!! This program will require the user to declare all the variables and typecast explicitly !!
!!---------------------------------

implicit none

!!---------------------------------
!! Declare the variables
!!---------------------------------
real::A(10,10), B(10,10),C(10,10)
integer::i, j, k, n_Eq
integer::r1,r2,d1,d2

!!---------------------------------
!! Analysis Block
!!---------------------------------

call mtx_tngle(A, B, C)

end program GAUSS_ELIMINATION_METHOD

!!---------------------------------
!! sub-routine
!!---------------------------------


subroutine mtx_tngle(A, B, C)

implicit none
real::A(10,10), B(10,10),TEMP(10,10), C(10,10)
integer::FLAG
integer::i, j, k, n_Eq, ind
integer::r1,r2,d1,d2

WRITE(*,*)"-------------------------------------"
WRITE(*,*)"Enter the total number of equations: "
READ(*,*) n_Eq
WRITE(*,20)"The Number of equations: ", n_Eq
WRITE(*,*)"-------------------------------------"

r1=n_Eq !!row of matrix A
d1=n_Eq !!column of matirx A

A=0
TEMP=0

DO i=1, r1

	WRITE(*,*)"----------------------------"
	WRITE(*,20)" For Equation -",i
	WRITE(*,*)"----------------------------"
	
	DO j=1, d1
		
		WRITE(*,20)"Enter the value of x-",j
		READ(*,*)A(i,j)
	ENDDO
	
	DO k=1, d1
		IF(k>=2) EXIT
		WRITE(*,*)"Enter the value to be equated"
		READ(*,*)B(i,j)
	ENDDO
ENDDO

WRITE(*,*)"----------------------------"
WRITE(*,*)"The equation in mtx form"
DO i=1, r1
	WRITE(*,10)(A(i,j), j=1,d1) 
ENDDO
WRITE(*,*)"----------------------------"

WRITE(*,*)"----------------------------"
WRITE(*,*)"Equated values in mtx form"
DO i=1, r1
	WRITE(*,10)(B(i,j)) 
ENDDO
WRITE(*,*)"----------------------------"


!! Row-Echilon Formation- Row Transformations

 
DO i=1, r1
	DO j=1, d1
		IF( A(i,j)<(A((i+1),j)) ) THEN
			TEMP(i,j) = A(i,j)
			A(i,j) = A((i+1),j)
			A((i+1),j) = TEMP(i,j)	
		ENDIF
		
		IF(i==d1) THEN
			IF (A((i-(d1-1)),j) < A((i-(d1-2)),j) ) THEN
				TEMP(i,j) = A((i-(d1-1)),j)
				A((i-(d1-1)),j) = A((i-(d1-2)),j)
				A((i-(d1-2)),j) = TEMP(i,j)
			ENDIF	
		ENDIF  
	ENDDO
ENDDO

WRITE(*,*)"----------------------------"
WRITE(*,*)"New equation in mtx form"
DO i=1, r1
	WRITE(*,10)(A(i,j), j=1,d1) 
ENDDO
WRITE(*,*)"----------------------------"


!!---------------------------------
!! FORMATING STATEMENTS
!!---------------------------------

10 FORMAT(5F8.3)
20 FORMAT(A, I3)

end subroutine mtx_tngle

