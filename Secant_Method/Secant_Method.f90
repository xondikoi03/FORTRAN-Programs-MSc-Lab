  !!------------------------------------!!
  !!  Original Author: Raj Handique     !!
  !!  PRN: 240210723                    !!
  !!  Date Created: 12 September 2024   !!
  !!  FORTRAN 95 - Source Code          !!
  !!------------------------------------!!


  program SECANT_METHOD

    !!---------------------------------------------------------
    !! This program will require the user to declare all the variables and typecast explicitly !!
    !!---------------------------------------------------------

    implicit none

    !!--------------------------------------------------------
    !! Declare the variables
    !!--------------------------------------------------------
    
    real::x1, x2, x3, f1, f2, f3, f, check_flag, eps, del, rel
    integer::counter
    
    !!--------------------------------------------------------
    !! INTERFACE MESSAGE
    !!--------------------------------------------------------
    
    WRITE(*,*)'------------------------------------------------------------------'
    WRITE(*,*)'This is program written to find root using Secant Method'
    WRITE(*,*)'Original Author: Raj Handique'
    WRITE(*,*)'Date Created: 12 September 2024'
    WRITE(*,*)'PRN: 24021073'
    WRITE(*,*)'------------------------------------------------------------------'

    !!--------------------------------------------------------
    !! USER INPUT
    !!--------------------------------------------------------
    
    WRITE(*,*)"Enter the value of x1: "
    READ(*,*) x1
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The value of x1 is: ", x1
    WRITE(*,*)"----------------------------------------"
    
    WRITE(*,*)"Enter the value of x2 "
    READ(*,*) x2
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The value of x2 is: ", x2
    WRITE(*,*)"----------------------------------------"

    	  
    
    !!--------------------------------------------------------
    !! Analysis Block
    !!--------------------------------------------------------

    counter=0
    eps=10e-7
    del=10e-5
    
    DO
    	f1=f(x1)
    	f2=f(x2)
    	
    	!! Finding x3 !!
    	x3 = ((x1*f2) - (x2*f1))/(f2 - f1)
    	f3 = f(x3)
    	
    	rel = ABS(x3-x2)/ABS(x3)
    	
    	counter=counter+1
    	IF(ABS(f3)<eps.OR.rel<del) EXIT
    	
    	!! Replacing the values of x1 and x2
    	x1=x2 
    	x2=x3
    
    ENDDO
    
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The root of equation is: ", x3
    WRITE(*,*)"----------------------------------------"
    WRITE(*,10)"No. of iterations: ", counter
    WRITE(*,*)"----------------------------------------"

    !! FORMATTING STATEMENTS !!

    10 FORMAT (A, I3)
    20 FORMAT (A, F6.2)

  end program SECANT_METHOD
  
  !!----------------------------------------!!
  !!         Function Sub-Programs          !!
  !!----------------------------------------!!
  
  !! Function !!
  
  real function f(x)
  	implicit none
  	real::a1, a2, a3, x, func
  	
  	a1=1
  	a2=-6
  	a3=2
  	
  	func = a1*(x**2) + a2*x + a3
  	f = func
  	return
  end function f
  
