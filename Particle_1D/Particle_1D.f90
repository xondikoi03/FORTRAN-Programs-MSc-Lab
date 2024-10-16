!!-----------------------------------------------------
!! This program is written to find the solution of 1D Particle in a box
!! Original Author: Raj Handique
!! Date Created: 03 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program Particle_1D

    !!---------------------------------------------------------
    !! This program will require the user to declare all the variables and typecast explicitly !!
    !!---------------------------------------------------------

    implicit none

    !!--------------------------------------------------------
    !! Declare the variables
    !!--------------------------------------------------------
    integer:: g, counter, ind
    real:: eps, rel, del, init1, init2, x1, x2, x3, root, f1, f2, f3, f, check_flag, sign_check
    real:: y, x, psi_1, psi_2, psi_3, alpha, beta, eta, i, psi
    real:: arr_x(1000), arr_psi(1000), iter, a, V, a0, V0, mlt

    !! PART-I : Finding the roots of the function using Regula-Falsi Method 
	
    !! Taking input from the user !!
    
    !! GOTO STATEMENTS !!
    g=1
    GOTO (100), g

    100 CONTINUE
    
    WRITE(*,*)"----------------------------------------"    
    WRITE(*,*)"Enter the initial guess, init1: "
    WRITE(*,*)"----------------------------------------"
    READ(*,*)init1
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The initial guess init1 is: ",init1
    WRITE(*,*)"----------------------------------------"
    
    WRITE(*,*)"----------------------------------------"
    WRITE(*,*)"Enter the initial guess, init2: "
    WRITE(*,*)"----------------------------------------"   
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
    
    a0=5
    V0=10
    a=a0/(0.529)
    V=V0/(27.21)
    mlt=2*V*(a**2)
    
    WRITE(*,20)"the value of V: ", V
    WRITE(*,20)"the value of mlt: ", mlt
    WRITE(*,20)"the value of a: ", a
    
    eps = 0.00001
    del = 0.0001
    !!----------------------------------
    !!  initial values of functions
    !!----------------------------------
    
    f1 = f(x1,mlt)
    f2 = f(x2,mlt)
    WRITE(*,20)"the value of x1: ", x1
    WRITE(*,20)"the value of x2: ", x2
    WRITE(*,20)"the value of f1: ", f1
    WRITE(*,20)"the value of f2: ", f2
    
    check_flag = f1*f2
    IF(check_flag>0) THEN
       WRITE(*,*)"Initial Guesses aare wrong!! Please re-enter the guesses again!!"
       GOTO 100
    ENDIF

    counter = 0 !! initiaising the counter 
    f1=f(x1, mlt)
    f2=f(x2, mlt) 
    x3 = ((x1*f2) - (x2*f1))/(f2 - f1)
    f3 = f(x3, mlt)
    
    WRITE(*,20)"the value of x3: ", x3
    WRITE(*,20)"the value of f3: ", f3	
    
    DO
       f1=f(x1, mlt)
       f2=f(x2, mlt) 
       x3 = ((x1*f2) - (x2*f1))/(f2 - f1)
       f3 = f(x3, mlt)
       sign_check = f1*f3
          
       IF(sign_check>0) THEN
          x2 = x3
       ELSE
          x1 = x3
       ENDIF
       
       counter=counter+1
       
       rel = ABS(x2-x1)/ABS(x2)
       
       IF(ABS(f3)<eps.OR.rel<del) EXIT	

    ENDDO
    
    !! PART-II: Finding the wavefunction
    
    OPEN(UNIT=1,FILE="Wave_func2.dat")
    
    y = x3
        
    eta = y*tan(y)
    beta = eta/a
    alpha = y/a
    
    WRITE(*,*)"---------------------"
    WRITE(*,*)"Alpha: ", alpha
    WRITE(*,*)"Beta: ", beta
    WRITE(*,*)"Eta: ", eta
    WRITE(*,*)"Root: ", y
    WRITE(*,*)"---------------------"
    
    iter = -2*a
    ind = 1
    DO     
    	x = iter
    	IF(iter > 2*a) EXIT
    	
    	IF(x<(-a)) THEN
    		psi = psi_1(alpha, beta, x, a)
    	ELSE IF(x>(a)) THEN
    		psi = psi_3(alpha, beta, x, a)
    	ELSE IF((x<a).AND.(x>(-a))) THEN
    		psi = psi_2(alpha, x)
    	ENDIF
    	
    	arr_x(ind) = x !! Converting to Armstrong
    	arr_psi(ind) = psi !! Converting to eV
    	
    	WRITE(1,*) arr_x(ind), arr_psi(ind)
    	
    	iter = iter + 0.05
    	ind = ind + 1
    	
    ENDDO
    
    
    !! WRITE STATEMENTS
    
    WRITE(*,*)"----------------------------------------"
    WRITE(*,*)"The value of function at root: ", f3
    WRITE(*,*)"----------------------------------------"
    WRITE(*,*)"----------------------------------------"
    WRITE(*,20)"The root of equation is: ", x3
    WRITE(*,*)"----------------------------------------"
    WRITE(*,*)"No. of iterations: ", counter
    WRITE(*,*)"----------------------------------------"

    !! FORMATTING STATEMENTS !!

    10 FORMAT (A, I3)
    20 FORMAT (A, F6.2)

end program Particle_1D

!!---------------------------------
!! function sub-program
!!---------------------------------


real function f(x, mlt)
implicit none
	
	real:: x, func1, mlt
	
	func1 = ((x/cos(x))**2) - mlt
	f = func1
	return 
end function f

!!--------------------------------------

real function psi_1(alpha, beta, y, a)
implicit none
	real:: func2, y, alpha, beta, D, a1, a2, a3, a
	D = 1
	a1 = cos(alpha*a)
	a2 = EXP(beta*a)
	a3 = EXP(beta*y)
	
	func2 = D*a1*a2*a3 
	psi_1 = func2
	return
end function psi_1

!!---------------------------------------

real function psi_2(alpha, y)
implicit none
	real:: func3, y, alpha, D, a1
	D = 1
	a1 = cos(alpha*y)
	
	func3 = D*a1
	psi_2 = func3
	return
end function psi_2

!!-----------------------------------------

real function psi_3(alpha, beta, y, a)
implicit none
	real:: func4, y, alpha, beta, D, a1, a2, a3, a

	D = 1
	a1 = cos(alpha*a)
	a2 = EXP(beta*a)
	a3 = EXP(-(beta*y))
	
	func4 = D*a1*a2*a3 
	psi_3 = func4
	return
end function psi_3 



