!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 21 August, 2024     !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

program MATRIXADDITION

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!!----------------------------------
!! Declaring the variables
!!----------------------------------

real :: A(10,10), B(10,10), ADD(10,10)
integer :: n, m, l, i, j, k, FLAG


!! INTERFACE MESSAGE !!

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"This is program written to perform addition of two matrices"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 21 August, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"---------------------------------------------------------------------------"

WRITE(*,*)"Enter the number of rows!"
READ(*,*) n
WRITE(*,*)"------------------------------------------"
WRITE(*,20)"The number of rows :", n
WRITE(*,*)"------------------------------------------"

WRITE(*,*)"Enter the number of columns!"
READ(*,*) m

WRITE(*,*)"------------------------------------------"
WRITE(*,20)"The number of columns :", m
WRITE(*,*)"------------------------------------------"

!!----------------------------------
!! Matrix Addition
!!----------------------------------

!!Reading First Matrix!!

WRITE(*,*)"Enter the elements of the first matrix:"

DO i=1,n
	!!FLAG = 0
	DO j=1,m
		READ(*,*)A(i,j)
		!!FLAG = 1
	ENDDO
ENDDO

WRITE(*,*)"The First Matrix is :"

WRITE(*,*)"------------------------------------------"
DO i=1,n
	WRITE(*,10)(A(i,j), j=1,m)
ENDDO
WRITE(*,*)"------------------------------------------"

!!Reading the second matrix!!


WRITE(*,*)"Enter the elements of the second matrix:"

DO i=1,n
	!!FLAG = 0
	DO j=1,m
		READ(*,*)B(i,j)
		!!FLAG = 1
	ENDDO
ENDDO

WRITE(*,*)"The Second Matrix is :"

WRITE(*,*)"------------------------------------------"
DO i=1,n
	WRITE(*,10)(B(i,j), j=1,m)
ENDDO
WRITE(*,*)"------------------------------------------"

!! Matrix Addition - Process

DO i=1,n
	DO j=1,m
		ADD(i,j) = A(i,j) + B(i,j)
	ENDDO
ENDDO

!! Show the output Matrix

WRITE(*,*)"The Addition of the two matrices is shown below:"

WRITE(*,*)"------------------------------------------"
DO i=1,n
	WRITE(*,10)(ADD(i,j), j=1,m)
ENDDO
WRITE(*,*)"------------------------------------------"

!! Let us try a completely implicit do loop !!

WRITE(*,*)"The Addition of the two matrices(implied do loop):"

WRITE(*,*)"------------------------------------------"
WRITE(*,10)((ADD(i,j),j=1,m),i=1,n)
WRITE(*,*)"------------------------------------------"

!! FORMATING

10 FORMAT (10F7.2)
20 FORMAT (A,I2)

end program MATRIXADDITION
