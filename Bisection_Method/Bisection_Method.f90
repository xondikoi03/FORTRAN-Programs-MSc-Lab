  !!------------------------------------!!
  !!  Original Author: Raj Handique     !!
  !!  PRN: 240210723                    !!
  !!  Date Created: 5 September, 2024   !!
  !!  FORTRAN 95 - Source Code          !!
  !!------------------------------------!!


  program BISECTION_METHOD

    !!---------------------------------------------------------
    !! This program will require the user to declare all the variables and typecast explicitly !!
    !!---------------------------------------------------------

    implicit none

    !!--------------------------------------------------------
    !! Declare the variables
    !!--------------------------------------------------------
    integer::g, counter
    real:: eps, del, init1, init2, x1, x2, x3, root, f1, f2, f3, f, check_flag, sign_check
    !! INTERFACE MESSAGE !!

    WRITE(*,*)"------------------------------------------------------------------------"
    WRITE(*,*)"This program performs the Bisection method to find roots of a function"
    WRITE(*,*)"Original Author: Raj Handique"
    WRITE(*,*)"Created on: 5 September, 2024"
    WRITE(*,*)"PRN: 24021073"
    WRITE(*,*)"------------------------------------------------------------------------"

    !! Taking input from the user !!
    
    !! GOTO STATEMENTS !!
    g=1
    GOTO (100), g

    100 CONTINUE
    
    WRITE(*,*)"Enter the initial guess, init1: "
    READ(*,*)init1
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The initial guess init1 is: ",init1
    WRITE(*,*)"----------------------------------------"
    
    WRITE(*,*)"Enter the initial guess, init2: "
    READ(*,*)init2
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The initial guess init2 is: ",init2
    WRITE(*,*)"----------------------------------------"

    !! Compare the initial values !!

    !! Puts the smaller number in x1 and larger number in x2 !!

    IF(init1>init2) THEN
       x2 = init1
       x1 = init2
    ELSE
       x2 = init2
       x1 = init1
    ENDIF
    
    !! initialising the error values !!

    eps = 0.0000001
    !!----------------------------------
    !!  initial values of functions
    !!----------------------------------
    
    f1 = f(x1)
    f2 = f(x2)

    check_flag = f1*f2
    IF(check_flag>0) THEN
       WRITE(*,*)"Initial Guesses aare wrong!! Please re-enter the guesses again!!"
       GOTO 100
    ENDIF

    counter = 0 !! initiaising the counter !!

    
    DO
       x3 = (x2+x1)/2
       f3 = f(x3)
       sign_check = f1*f3
       
       IF(sign_check>0) THEN
          x1 = x3
       ELSE
          x2 = x3
       ENDIF
       
       counter=counter+1
       IF(ABS(f3)<eps) EXIT

    ENDDO
    
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The root of equation is: ", x3
    WRITE(*,*)"----------------------------------------"
    WRITE(*,10)"No. of iterations: ", counter
    WRITE(*,*)"----------------------------------------"

    !! FORMATTING STATEMENTS !!

    10 FORMAT (A, I3)
    20 FORMAT (A, F6.2)
  end program BISECTION_METHOD


  !!------------------------------------!!
  !!        Function Sub-Program        !! 
  !!------------------------------------!!

  real function f(x)
    implicit none
    real::a1, a2, a3, x, func
    
    !!-------------------------
    !!  Values of coefficients
    !!-------------------------

    a1= 1 
    a2= 2
    a3= -15
    
    func = ((a1*(x**2))+(a2*(x))+a3)
    f = func
    return
  end function f
