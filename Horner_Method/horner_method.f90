  !!------------------------------------!!
  !!  Original Author: Raj Handique     !!
  !!  PRN: 240210723                    !!
  !!  Date Created: 5 September, 2024   !!
  !!  FORTRAN 95 - Source Code          !!
  !!------------------------------------!!


program HORNER_METHOD

    !!---------------------------------------------------------
    !! This program will require the user to declare all the variables and typecast explicitly !!
    !!---------------------------------------------------------

	implicit none

    !!--------------------------------------------------------
    !! Declare the variables
    !!--------------------------------------------------------
	
	real::P, Arr(100), x
	integer::n_coeff, n_deg, i
	
    !! INTERFACE MESSAGE !!

	WRITE(*,*)"------------------------------------------------------------------------"
	WRITE(*,*)"This program performs the Horner's Method for polynomial evaluation"
	WRITE(*,*)"Original Author: Raj Handique"
	WRITE(*,*)"Created on: 5 September, 2024"
	WRITE(*,*)"PRN: 24021073"
	WRITE(*,*)"------------------------------------------------------------------------"

    !! Taking input from the user !!
    	
    	!! Degree of polynomial
    	
   	WRITE(*,*)"Enter the degree of polynomial:"
   	READ(*,*)n_deg
   	WRITE(*,*)"----------------------------------------------"
   	WRITE(*,10)"The degree of the polynomial will be:", n_deg
   	WRITE(*,*)"----------------------------------------------"
   	
   	WRITE(*,*)"Enter the value of x: "
   	READ(*,*)x
   	WRITE(*,*)"----------------------------------------------"
   	WRITE(*,20)"The value of x in the polynomial: ", x
   	WRITE(*,*)"----------------------------------------------"
   	
   	n_coeff = n_deg + 1 !!(number of coefficients = degree + 1)
   	
   	WRITE(*,*)"Enter the coefficient of the polynomial: "
   	DO i=1, n_coeff
   		WRITE(*,30)"Enter the coefficient a-",i
   		READ(*,*)Arr(i)
   		WRITE(*,*)"----------------------------------------------"
   		WRITE(*,20)"The entered coefficient is: ", Arr(i)
   		WRITE(*,*)"----------------------------------------------"
   	ENDDO
   	
   	P = Arr(n_coeff) !! storing the array in the variable P
   	
   	DO i=n_coeff, 2, -1
   		P = (P*x) + Arr(i-1) 
   	ENDDO
  
  	WRITE(*,*)"----------------------------------------------"
   	WRITE(*,20)"The value of polynomial for given x: ", P
   	WRITE(*,*)"----------------------------------------------"
  	
  	!! FORMATING STATEMENTS
  	
  	10 FORMAT(A, I4)
  	20 FORMAT(A, F6.2)
  	30 FORMAT(A, I2)
  	
end program HORNER_METHOD
