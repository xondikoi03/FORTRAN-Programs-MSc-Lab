!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 14 August, 2024     !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!

!---------------------------------------!
! This program performs the bubble sort !
! algorithm to sort numbers in          ! 
! ascending manner                      !
!---------------------------------------!

program BUBBLESORT

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ----------------------

real :: Arr(10)
integer :: i, n, FLAG, TEMP, SWAP

!! Interface Message !!

WRITE(*,*)"------------------------------------------------------------"
WRITE(*,*)"This is program written to perform the Bubble Sort Algortihm"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 14 August, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"------------------------------------------------------------"

!! Take the input variables

WRITE(*,*)"ENTER THE TOTAL NUMBERS TO UNDERGO BUBBLE SORTING ALGORITHM:"
READ(*,*) n
WRITE(*,*)"------------------------------------------------------------"
WRITE(*,100)"THE TOTAL NUMBERS TO UNDERGO BUBBLE SORTING ALGORITHM:", n
WRITE(*,*)"------------------------------------------------------------"

!! FORMATTING STATEMENTS !!

20 FORMAT(A, 10F5.1)
10 FORMAT(A,1F5.1)
100 FORMAT(A, I3)

!! Take in the elements of the Array

DO i=1,n
	WRITE(*,*)"ENTER THE VALUE FOR THE ARRAY ELEMENT:"
	READ(*,*) Arr(i)
	WRITE(*,*)"--------------------------------------"
	WRITE(*,10)"THE ENTERED VALUE IS:", Arr(i)
	WRITE(*,*)"--------------------------------------"
ENDDO



!! BUBBLE SORTING ALGORITHM STARTS HERE !!

DO 
FLAG = 0
	DO i=1,(n-1)
		IF(Arr(i) > Arr(i+1)) THEN
			TEMP = Arr(i)
			Arr(i) = Arr(i+1)
			Arr(i+1) = TEMP
			FLAG = 1 
		ENDIF
	ENDDO
	IF (FLAG==0) EXIT !! THE PROGRAM EXITS IF THERE IS NO MORE SORTING TO BE DONE
ENDDO

WRITE(*,*)"-------------------------------------------------------------------------"
WRITE(*,20)"THE SORTED ARRAY IS AS FOLLOWS:", (Arr(i),i=1,n)
WRITE(*,*)"-------------------------------------------------------------------------"
end program BUBBLESORT
