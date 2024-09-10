!---------------------------------------!
!     Original Author: Raj Handique     !
!     PRN: 24021073                     !
!     Date Created: 4 September, 2024   !
!     FORTRAN 95 - Source Code          !
!---------------------------------------!



program MAGIC_SQUARE

!!----------------------------------
!! The program will require one to declare all varibles and typecast explicitly !!
!!----------------------------------

implicit none

!! ----------------------
!! Declare the varibales
!! ---------------------- 
  
integer::i, j, g, S, init_val, counter, n, nsqr, odd_checker, row_old, col_old, row_init, col_init, new_val, mgk(100,100)  

!! INTERFACE MESSAGE !!

WRITE(*,*)"---------------------------------------------------------------------------"
WRITE(*,*)"This is program written to print a magic square"
WRITE(*,*)"Original Author: Raj Handique"
WRITE(*,*)"Created on: 4 September, 2024"
WRITE(*,*)"PRN: 24021073"
WRITE(*,*)"---------------------------------------------------------------------------"

!! -------------------------------
!! Taking the values from the user
!! -------------------------------

!! GOTO STATEMENTS (Always set as true!!)
g=1
GOTO (100), g

100 CONTINUE

WRITE(*,*)"Enter the number of rows & columns for the Magic Sqaure (odd numbers only!!):"
READ(*,*) n

!! Check if the number entered is odd

odd_checker = MOD(n,2)

IF(odd_checker==0) THEN
	WRITE(*,*)"The entered value is not an odd number!!"
	GOTO 100
ELSE
	nsqr = n**2
	WRITE(*,*)"--------------------------------------------------------"
	WRITE(*,30)"The no. of elements magic square will have !!", nsqr 
	WRITE(*,*)"--------------------------------------------------------"
	
ENDIF

WRITE(*,*)"Enter the initial value to be written in the magic square"
READ(*,*) init_val
WRITE(*,*)"--------------------------------------------------------"
WRITE(*,30)"Initial Value is to be written is: ", init_val
WRITE(*,*)"--------------------------------------------------------"

!! ---------------
!! ANALYSIS BLOCK
!! ---------------

S = init_val - 1 

!! Filling up the magic squares !!

row_init = 1
col_init = (n+1)/2

mgk = S !! filling the magic sqaure with the predefined value

mgk(row_init,col_init) = init_val !! initialise the first user given value in the magic sqaure !!

new_val = init_val !! initialise the new value with the initial value

counter = 0

DO
	new_val = new_val + 1
	col_old = col_init 
	row_old = row_init 
	col_init = col_init + 1
	
	IF(col_init > n)col_init = 1
	
	row_init = row_init - 1	
	
	IF(row_init < 1)row_init = n
		
	IF(mgk(row_init,col_init)==S) THEN
	
		mgk(row_init, col_init) = new_val
	ELSE 
		row_init = row_old + 1
		col_init = col_old 
		mgk(row_init, col_init) = new_val  
	
	ENDIF	
	counter = counter + 1
	IF(counter==(nsqr)) EXIT
ENDDO

WRITE(*,*)"--------------------------------------------------------"
WRITE(*,*)"The Magic Square is: "
DO i=1, n
	WRITE(*,10)(mgk(i,j),j=1,n)
ENDDO
WRITE(*,*)"--------------------------------------------------------"

!! FORMATTING STATEMEMTS
10 FORMAT (10I5)
30 FORMAT (A, I3)


end program MAGIC_SQUARE
