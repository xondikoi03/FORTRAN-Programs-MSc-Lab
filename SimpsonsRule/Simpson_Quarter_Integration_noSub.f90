!!-----------------------------------------------------
!! This program performs integration of functions using Simpson's 1/3rd Rule
!! Original Author: Raj Handique
!! Date Created: 06 November 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program Simpson_Quarter_Integration

!!---------------------------------
!! This program will require the user to declare all the variables and typecast explicitly !!
!!---------------------------------
!! Declare the variables
!!---------------------------------
implicit none
integer::n, i, counter
real::func, diff
real::a, b, h, inc, true_integration_val
real::Integration

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
	
	!! Simpsons's Rule
	
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	WRITE(*,*)"|       n     |        h         |        h^4       |        I         |        I_n       |       Error      |"
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	counter=1
	true_integration_val = 0.25
	DO 
		IF(counter>15) EXIT
		
		h = (b-a)/n	
		Integration = func(a) + func(b)
		
		DO i=1, (n-1)
			inc = a + (i*h)	
			!! For even terms
			IF(MOD(i,2)==0) THEN
				Integration = Integration + (2.0*func(inc))	
			!! For odd terms
			ELSE
				Integration = Integration + (4.0*func(inc))
			ENDIF
		ENDDO	
		Integration = (h/3.0)*Integration
		diff = ABS(true_integration_val - Integration)
		
		WRITE(*,*)"|",n,"|",h,"|",h**4,"|",true_integration_val,"|",Integration,"|",diff,"|"
		
		n = n+10
		counter=counter+1
	ENDDO
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	WRITE(*,10)"The value of integration of given function is:", Integration
	WRITE(*,*)"--------------------------------------------------------------------------------------------------------------"
	
	10 FORMAT(1A, F15.8)

end program Simpson_Quarter_Integration

!!---------------------------------
!! function sub-programs
!!---------------------------------

real function func(x)
	implicit none
	real::x
	func = x**3
	return
end function func

