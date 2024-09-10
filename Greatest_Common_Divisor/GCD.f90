!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 8 August, 2024      !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

!---------------------------------------!
!  This program finds the gratest       ! 
!  common divisor of two given numbers  ! 
!---------------------------------------! 

program GreatestCommonDivider

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ----------------------

integer NUM, DEN, REM, A, B, GCD, COUNTER

!! INTERFACE MESSAGE !!

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"This is program written to find the Greatest Common Divisor of two numbers "
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 14 August, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"---------------------------------------------------------------------------"

!! Take the input variables

WRITE(*,*)"Enter the first number: "
READ(*,*) A
WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,10)"The first number is: ", A
WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"Enter the second number: "
READ(*,*) B
WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,10)"The second number is: ", B
WRITE(*,*)"---------------------------------------------------------------------------"

!! The algorithm to find the GCD is to be written below !! 

IF(A > B) THEN
	NUM = A
	DEN = B
	
ELSE IF (A < B) THEN
	NUM = B
	DEN = A 	
ENDIF

COUNTER = 1

DO
	COUNTER = COUNTER + 1 
	REM = NUM-(NUM/DEN)*DEN
	IF(REM == 0) EXIT
	NUM = DEN
	DEN = REM	
ENDDO


GCD = DEN

!! FORMATTING STATEMENTS !!

10 FORMAT(A, I3)

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,10)"The Greatest Common Divisor is :", GCD
WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,10)"The No.of steps required to get to GCD: ", COUNTER
WRITE(*,*)"---------------------------------------------------------------------------"


end program GreatestCommonDivider
