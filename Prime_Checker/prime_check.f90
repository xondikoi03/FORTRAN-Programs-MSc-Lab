!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 4 September, 2024   !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!



program PRIME_CHECKER

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ---------------------- 

integer::i, cand, sqroot  
logical::isPrime

!! INTERFACE MESSAGE !!

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"This is program written to check prime numbers"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 4 September, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"---------------------------------------------------------------------------"

!! ----------------------------
!! TAKING INPUT FROM USER
!! ----------------------------


WRITE(*,*)"--------------------------------------------------"
WRITE(*,*)"Enter the number to check if prime: "
READ(*,*) cand
WRITE(*,10)"The entered number is: ", cand
WRITE(*,*)"--------------------------------------------------"

!! Floor division of the number - returns only the value of type of variable in integer type !!

sqroot = floor(sqrt(real(cand)))

DO i=2, sqroot
	isPrime = .TRUE.
	IF(MOD(cand,i)==0) THEN 
		isPrime = .FALSE. 
		EXIT
	ENDIF
ENDDO

IF(isPrime.EQV. .TRUE.) THEN
	WRITE(*,*)"--------------------------------------------------"
	WRITE(*,10)"The given number is a prime number: ", cand
	WRITE(*,*)"--------------------------------------------------"
ELSE
	WRITE(*,*)"--------------------------------------------------"
	WRITE(*,10)"The given number is not a prime number: ", cand
	WRITE(*,*)"--------------------------------------------------"
ENDIF

!! FORMATTING STATEMENTS
10 FORMAT (A,I3)

end program PRIME_CHECKER
