!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 7 August, 2024      !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

!---------------------------------------!
!  Program to find the smallest number  !
!---------------------------------------!

program SMALLEST

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ---------------------- 
integer x1, x2, x3

!!-----------------------
!! The program will try to compare all the three numbers that are fed into the program
!!-----------------------

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
	WRITE(*,*)"All Numbers are equal !!"

ELSE IF(x1 < x2) THEN
	IF(x1 < x3) THEN
		WRITE(*,*)"The smallest number is: ", x1
	ELSE IF(x1 == x3) THEN
		WRITE(*,*)"The first and third number are equal and smallest !!"
	ELSE
		WRITE(*,*)"The smallest number is: ", x3	
	ENDIF
ELSE IF(x1 == x2) THEN
	IF(x2 < x3) THEN
                WRITE(*,*)"The first and second number are equal and smallest !!"
	ELSE
		WRITE(*,*)"The smallest number is: ", x3
	ENDIF
ELSE IF(x2 == x3) THEN
	WRITE(*,*)"The second and third number are equal and smallest !!"
ELSE IF(x3 < x2) THEN
	WRITE(*,*)"The smallest number is: ", x3	
ELSE
	WRITE(*,*)"The smallest number is: ", x2
ENDIF

!!----------------------
!! END OF ANALYSIS BLOCK
!!----------------------

end program SMALLEST
