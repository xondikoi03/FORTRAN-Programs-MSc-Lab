!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 2 September, 2024   !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!



program NUM_INACCURACY

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ---------------------- 

integer::i, j, itr
real::Arr_1(1000), Arr_2(1000), func1, func2, x, eps  

!! INTERFACE MESSAGE !!

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"This is program written to exhibit numerator inaccuracy"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 2 September, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"---------------------------------------------------------------------------"

!! ---------------------------------------------
!! The Program exhibits numerator inaccuracy !!
!! ---------------------------------------------
!! Given 
!! func1 = {1 - cos(x)}/(x)**2 
!! func2 = {sin(x)**2}/(1 + cos(x))*(x)**2
!! --------------------------------------------

WRITE(*,*)"Enter the error margin: "
READ(*,*)eps
WRITE(*,*)"The error margin is: ", eps

!! --------------------------------------------

itr = 0
x=1
i=0
j=0

DO
	i = i+1
	j = j+1
	
	IF(x<eps) EXIT
	
	func1 = (1 - cos(x))/(x**2)
	func2 = (sin(x)**2)/((1+cos(x))*((x)**2))
	
	Arr_1(i)=func1
	Arr_2(j)=func2
	
	x = x/10	
	itr = itr+1
ENDDO

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"The results for expression in func1:{(1-cos(x))/((x)**2)}"
WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,10)(Arr_1(i), i=1, itr)
WRITE(*,*)"---------------------------------------------------------------------------"

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"The results for expression in func2:{(sin(x)**2)/((x)**2*(1+cos(x)))}"
WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,10)(Arr_2(j), j=1, itr)
WRITE(*,*)"---------------------------------------------------------------------------"

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"The number of iterations:", itr
WRITE(*,*)"---------------------------------------------------------------------------"

!! FORMATTING STATEMENTS

10 FORMAT (15F10.6)

end program NUM_INACCURACY
