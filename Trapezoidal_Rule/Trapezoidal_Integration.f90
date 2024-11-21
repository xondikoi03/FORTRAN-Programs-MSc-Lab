!!-----------------------------------------------------
!! This program performs integration of functions using trapezoidal rule
!! Original Author: Raj Handique
!! Date Created: 06 November 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program Trapezoidal_Integration

!!---------------------------------
!! This program will require the user to declare all the variables and typecast explicitly !!
!!---------------------------------

implicit none
!!---------------------------------
!! Declare the variables
!!---------------------------------
real::a, b, func, Integration
integer::n
!!---------------------------------
!! Analysis Block
!!---------------------------------

WRITE(*,*)"-------------------------------"
WRITE(*,*)"Enter the initial no. of steps:"
READ(*,*) n
WRITE(*,*)"-------------------------------"
WRITE(*,*)"Initial number of steps: ", n
WRITE(*,*)"-------------------------------"

WRITE(*,*)"-------------------------------"
WRITE(*,*)"Enter the lower limit: "
READ(*,*) a
WRITE(*,*)"The lower limit is: ",a
WRITE(*,*)"-------------------------------"
WRITE(*,*)"Enter the upper limit: "
READ(*,*) b
WRITE(*,*)"The upper limit is: ",b
WRITE(*,*)"-------------------------------"

call integration_trap(a, b, n, Integration)

end program Trapezoidal_Integration

!!---------------------------------
!! function sub-programs
!!---------------------------------

real function func(x)
	implicit none
	real::x
	func = x**3
	return
end function func

!!----------------------------------
!! subroutines
!!----------------------------------
subroutine integration_trap(a, b, n, Integration)

	implicit none
	integer::n, i, counter
	real::func, diff
	real::a, b, h, inc, Intgn_init
	real,intent(out)::Integration
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	WRITE(*,*)"|       n     |        h         |       h_sq       |        I         |        I_n       |       I-I_n      |"
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	counter=1
	intgn_init = 0.25
	DO 
		IF(counter>10) EXIT
		
		h = (b-a)/n	
		Integration = func(a) + func(b)
		DO i=1, (n-1)
			inc = a + (i*h)
			Integration = Integration + (2.0000*func(inc))	
		ENDDO	
		
		Integration = (h/2.0000)*Integration
		diff = Integration - Intgn_init
		
		WRITE(*,*)"|",n,"|",h,"|",h**2,"|",Intgn_init,"|",Integration,"|",diff,"|"
		
		n = n+10
		counter=counter+1
	ENDDO
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	WRITE(*,10)"The value of integration of given function is:", Integration
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	
	10 FORMAT(1A, F15.8)
end subroutine

