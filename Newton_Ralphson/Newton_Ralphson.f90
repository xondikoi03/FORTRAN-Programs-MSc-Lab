  !!------------------------------------!!
  !!  Original Author: Raj Handique     !!
  !!  PRN: 240210723                    !!
  !!  Date Created: 12 September 2024   !!
  !!  FORTRAN 95 - Source Code          !!
  !!------------------------------------!!

  program Newton_Ralphson
    !!---------------------------------------------------------
    !! This program will require the user to declare all the variables and typecast explicitly !!
    !!---------------------------------------------------------

    implicit none

    !!--------------------------------------------------------
    !! Declare the variables
    !!--------------------------------------------------------
    real::f, g, x1, x2, x3, f1, f2, f3, d1, check_flag, eps, del, rel
    integer::counter
        
    !!--------------------------------------------------------
    !! INTERFACE MESSAGE
    !!--------------------------------------------------------
    
    WRITE(*,*)'------------------------------------------------------------------'
    WRITE(*,*)'This is program written to find roots using Newton-Ralphson Method'
    WRITE(*,*)'Original Author: Raj Handique'
    WRITE(*,*)'Date Created: 12 September 2024'
    WRITE(*,*)'PRN: 24021073'
    WRITE(*,*)'------------------------------------------------------------------'

    
    !!--------------------------------------------------------
    !! Analysis Block
    !!--------------------------------------------------------

    WRITE(*,*)"Enter the value of x1:"
    READ(*,*) x1
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The value of x1:", x1
    WRITE(*,*)"----------------------------------------"
    
    !! checking if the derivative is zero !!
    
    check_flag = g(x1)
    IF(check_flag==0) THEN
    	WRITE(*,*)"The value of x1 makes the derivative zero - doesn't satisfy NR Method!"
    ENDIF
    
    !! Initialisng Values for counter and errors
    eps=10e-7
    del=10e-5
    counter=0
    
    !! Newton-Ralphson Method !!
    
    DO
    	
    	f1 = f(x1) !! Function !!
    	d1 = g(x1) !! Derivative !!
    
    	x2 = x1 - (f1/d1)
     	
     	!!Relative error between x1 and x2 
     	rel = ABS(x1-x2)/ABS(x1)
     	
     	counter=counter+1
 	IF(ABS(f1)<eps.OR.rel<del) EXIT
 	
     	!! Replacing the value of x1 with x2 (Reassigning)
    	x1 = x2
    ENDDO
    
    !! Output And Results !!
    
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The root of equation is: ", x2
    WRITE(*,*)"----------------------------------------"
    WRITE(*,10)"No. of iterations: ", counter
    WRITE(*,*)"----------------------------------------"

    !! FORMATTING STATEMENTS !!

    10 FORMAT (A, I3)
    20 FORMAT (A, F6.2)
  
  end program Newton_Ralphson
  
  !!---------------------------------------------------!!
  !! Sub-programs for the function and it's derivative !!
  !!---------------------------------------------------!!
  
  !! Function !!
  
  real function f(x)
  	implicit none
  	real::a1, a2, a3, a4, a5, x, func
  	
  	a1=1
  	a2=-8
  	a3=-39
  	a4=-62
  	a5=50
  	
  	func = a1*(x**4) + a2*(x**3) + a3*(x**2) + a4*(x) + a5
  	f = func
  	return
  end function f
  
  !! Derivative
  
  real function g(x)
  	implicit none
  	real::b1, b2, b3, b4, x, func
  	
  	b1=4
  	b2=-24
  	b3=-78
  	b4=-62
  	
  	func= b1*(x**3) + b2*(x**2) + b3*(x) + b4
  	g = func
  	return
  end function g
  
