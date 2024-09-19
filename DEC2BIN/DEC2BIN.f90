!!------------------------------------!!
!!  Original Author: Raj Handique     !!
!!  PRN: 240210723                    !!
!!  Date Created: 18 September 2024   !!
!!  FORTRAN 95 - Source Code          !!
!!------------------------------------!!

program DEC2BIN
	
!!---------------------------------------------------------
!! This program will require the user to declare all the variables and typecast explicitly !!
!!---------------------------------------------------------

implicit none

!!--------------------------------------------------------
!! Declare the variables
!!--------------------------------------------------------

real::dec, num, intg, rem, fracNum
integer::bin, iter1, qt, iter2, fracIntg, precision_iter, k, l, intArr(1000), decArr(1000)
		    
!!--------------------------------------------------------
!! INTERFACE MESSAGE
!!--------------------------------------------------------
    
WRITE(*,*)'------------------------------------------------------------------'
WRITE(*,*)'This is program written to convert decimal to binary'
WRITE(*,*)'Original Author: Raj Handique'
WRITE(*,*)'Date Created: 18 September 2024'
WRITE(*,*)'PRN: 24021073'
WRITE(*,*)'------------------------------------------------------------------'
    
!!--------------------------------------------------------
!! Analysis Block
!!--------------------------------------------------------

!! Take the user input for the number to be taken

WRITE(*,*)"--------------------------------------------------------"
WRITE(*,*)"Enter the integer part of the number to be converted: "
READ(*,*) intg
WRITE(*,*)"--------------------------------------------------------"
WRITE(*,30)"The number is: ", intg
WRITE(*,*)"--------------------------------------------------------"

WRITE(*,*)"--------------------------------------------------------"
WRITE(*,*)"Enter the fractional part of the number to be converted:"
READ(*,*) dec
WRITE(*,*)"--------------------------------------------------------"
WRITE(*,30)"The number is: ", dec
WRITE(*,*)"--------------------------------------------------------"

WRITE(*,*)"--------------------------------------------------------"
WRITE(*,*)"Enter the precision of fractional part: "
READ(*,*) precision_iter
WRITE(*,*)"--------------------------------------------------------"
WRITE(*,20)"The number is: ", precision_iter
WRITE(*,*)"--------------------------------------------------------"

num = (intg+dec) 

WRITE(*,*)"--------------------------------------------------------"
WRITE(*,30)"The number to be converted is: ", num
WRITE(*,*)"--------------------------------------------------------"


!! For integer part of the number

iter1=1

DO 
	rem = MOD(intg, 2.0)
	qt = (intg/2)
	
	intArr(iter1) = rem !! Filling the array with the binary numbers
	intg = qt !! Replacing the value of quotient with the integer for the next iteration
	
	IF(qt==0) EXIT
	iter1=iter1+1
ENDDO

!! For the fractional part of the number

iter2=1

DO
	fracNum = dec*2
	!WRITE(*,*)"Test fracNum: ",fracNum
	
	fracIntg = fracNum
	!WRITE(*,*)"Test fracIntg: ", fracIntg
	decArr(iter2) = fracIntg
	
	IF(fracIntg == 0) THEN
		dec = fracNum
		!WRITE(*,*)"Test dec(-lt 0): ", dec
	ELSE IF(fracIntg == 1) THEN
		dec = (fracNum - 1.0)
		!WRITE(*,*)"Test dec(-gt 0):", dec
	ENDIF
	
	IF(iter2==precision_iter) EXIT
	iter2=iter2+1
ENDDO

!! Let us Print out the binary number !!

WRITE(*,*)"------------------------------------------------------------------------------------------------"
WRITE(*,10)"The integral part of the number in binary: ", (intArr(k), k=(iter1), 1, -1)
WRITE(*,*)"------------------------------------------------------------------------------------------------"
WRITE(*,10)"The fractional part of the number in binary: ", (decArr(l), l=1, iter2, 1)
WRITE(*,*)"------------------------------------------------------------------------------------------------"

WRITE(*,*)"The Binary Number is: "
WRITE(*,50) (intArr(k), k=(iter1), 1, -1)
WRITE(*,*)"."
WRITE(*,50) (decArr(l), l=1, iter2, 1)

!WRITE(*,40)"The string is", iter1,"string",iter2

!! FORMATING STATEMENTS !!

10 FORMAT (A, 15I3)
20 FORMAT (A, I2)
30 FORMAT (A, F12.8)
50 FORMAT (15I2)

end program DEC2BIN
