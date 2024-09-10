!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 8 August, 2024      !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

!---------------------------------------!
!  This program finds the roots of a    ! 
!  quadratic equation (only if the      ! 
!  roots are real)                      !
!---------------------------------------! 

program QUADRATIC

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ----------------------

real D1, D2, N1, A, B, C, DISC 

!! Take the input variables

WRITE(*,*)"------------------------------------------------------------------"
WRITE(*,*)"This is program written to find the roots of a quadratic equation "
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 8 August, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"------------------------------------------------------------------"

WRITE(*,*)"------------------------------------------------------------------"
WRITE(*,*)"The Equation is of the form"
WRITE(*,*)"A*(x)**2 + B*x + C = 0"
WRITE(*,*)"------------------------------------------------------------------"

WRITE(*,*)"Enter the Coefficient A: "
READ(*,*) A
WRITE(*,*)"---------------------------------------"
WRITE(*,*)"The Coefficient of A is: ", A
WRITE(*,*)"---------------------------------------"

WRITE(*,*)"Enter the Coefficient B: "
READ(*,*) B
WRITE(*,*)"---------------------------------------"
WRITE(*,*)"The Coefficient of B is: ", B
WRITE(*,*)"---------------------------------------"

WRITE(*,*)"Enter the Coefficient C: "
READ(*,*) C
WRITE(*,*)"---------------------------------------"
WRITE(*,*)"The Coefficient of B is: ", C
WRITE(*,*)"---------------------------------------"

!! Let us write the discriminant of the quadratic equation !!

N1 = (B**2 - (4*A*C))
DISC = sqrt(N1)
D1 = ((-b) + DISC)/(2*A)
D2 = ((-b) - DISC)/(2*A)

!! Let us check if the roots are real or not

IF(N1 < 0) THEN
	WRITE(*,*)"-------------------------------------------------------------------------------------"
	WRITE(*,*)"THE ROOTS OF THE EQUATION ARE COMPLEX AND THIS PROGRAM IS NOT DESIGNED TO DO THAT !!"
	WRITE(*,*)"-------------------------------------------------------------------------------------"
ELSE IF(DISC==0) THEN
	WRITE(*,*)"-------------------------------------------------------------------"
	WRITE(*,*)"THE ROOTS OF THE EQUATION ARE REAL AND EQUAL: ", D1, D2	
	WRITE(*,*)"-------------------------------------------------------------------"
ELSE
	WRITE(*,*)"-------------------------------------------------------------------"
	WRITE(*,*)"THE ROOTS OF THE EQUATION ARE REAL !!"
	WRITE(*,*)"-------------------------------------------------------------------"
	WRITE(*,*)"THE FIRST ROOT IS:", D1
	WRITE(*,*)"-------------------------------------------------------------------"
	WRITE(*,*)"THE SECOND ROOT IS", D2
	WRITE(*,*)"-------------------------------------------------------------------"
ENDIF

end program QUADRATIC
