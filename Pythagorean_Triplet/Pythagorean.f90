!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 7 August, 2024      !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

!---------------------------------------!
!  Program find Pythagorean Triplet     !
!---------------------------------------!

program PYTHA

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ---------------------- 
integer x1, x2, x3
integer SUM1, SUM2, SUM3 


!!-----------------------
!! The program will try to compare all the three numbers that are fed into the program if they form a pythagorean triplet
!!-----------------------

WRITE(*,*)"--------------------------------------------------------"
WRITE(*,*)"This is Program Written to Find the Pythagorean Triplets"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"--------------------------------------------------------"

write(*,*) "Enter the value of the first number: " 
read(*,*) x1 
write(*,*) "The entered number is: ", x1

write(*,*) "Enter the value of the second number: "
read(*,*) x2
write(*,*) "The entered number is: ", x2

write(*,*) "Enter the value of the third number: "
read(*,*) x3
write(*,*) "The entered number is: ", x3

!!------------------------
!! Analysis Block
!!------------------------


IF((x1 == x2) .AND. (x2 == x3)) THEN
	WRITE(*,*)"---------------------------------------------------------------"
	WRITE(*,*)"All numbers in a triplet cannot be equal !!"
	WRITE(*,*)"---------------------------------------------------------------"	
ELSE IF(x1 > x2) THEN
	IF(x1 > x3) THEN
		SUM1 = x3**2 + x2**2
		IF(SUM1 == x1**2) THEN
			WRITE(*,*)"---------------------------------------------------------------"
			WRITE(*,*)"All three numbers", x1, x2, x3, "form a pythogorean triplet !!"
			WRITE(*,*)"---------------------------------------------------------------"
		ENDIF
	ELSE IF(x1 == x3) THEN
		WRITE(*,*)"---------------------------------------------------------------"
		WRITE(*,*)"Two of the sides cannot be equal !!"
		WRITE(*,*)"---------------------------------------------------------------"
	ELSE
		WRITE(*,*)"---------------------------------------------------------------"
		WRITE(*,*)"These numbers donot form a pythagorean triplet !!"
		WRITE(*,*)"---------------------------------------------------------------"
	ENDIF
	
ELSE IF(x1 == x2) THEN
	WRITE(*,*)"---------------------------------------------------------------"
        WRITE(*,*)"Two of the sides cannot be equal !!"
	WRITE(*,*)"---------------------------------------------------------------"        
ELSE IF(x2 == x3) THEN
	WRITE(*,*)"---------------------------------------------------------------"
	WRITE(*,*)"Two of the sides cannot be equal !!"
	WRITE(*,*)"---------------------------------------------------------------"	
ELSE IF(x3 > x2) THEN
	SUM2 = x1**2 + x2**2
	IF(SUM2 == x3**2) THEN
		WRITE(*,*)"---------------------------------------------------------------"
		WRITE(*,*)"All three numbers", x1, x2, x3, "form a pythogorean triplet !!"
		WRITE(*,*)"---------------------------------------------------------------"
	ELSE
		WRITE(*,*)"---------------------------------------------------------------"
		WRITE(*,*)"These numbers donot form a pythagorean triplet !!"
		WRITE(*,*)"---------------------------------------------------------------"
	ENDIF
ELSE
	SUM3 = x1**2 + x3**2
	IF(SUM3 == x2**2) THEN
		WRITE(*,*)"---------------------------------------------------------------"
		WRITE(*,*)"All three numbers", x1, x2, x3, "form a pythogorean triplet !!"
		WRITE(*,*)"---------------------------------------------------------------"
	ELSE
		WRITE(*,*)"---------------------------------------------------------------"
		WRITE(*,*)"These numbers donot form a pythagorean triplet !!"
		WRITE(*,*)"---------------------------------------------------------------"
	ENDIF
ENDIF

!!--- TRIPLET CALC -----




!!----------------------
!! END OF ANALYSIS BLOCK
!!----------------------

end program PYTHA
