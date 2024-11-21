!!-----------------------------------------------------
!! PRN: 24021073
!! Original Author: Raj Handique
!! Date Created: 17 October 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------


program gauss_elimination
real, dimension(:,:), allocatable :: A
real, dimension(:), allocatable::X
real, dimension(:), allocatable::temp 
real, dimension(:,:), allocatable::sort
real, dimension(:,:), allocatable::tri
real::temp_1, temp_2
integer::n, i,j,k, var1, var2
character(len = 1)::string
character(len = 46)::string1

write(*,*)'-----------------------------------------------------'
write(*,*)'Enter the number of unknowns'
read(*,*)n
write(*,20)'The value entered is:', n
write(*,*)'-----------------------------------------------------'
allocate(A(n,n+1), X(n), temp(n+1), sort(n,n+1), tri(n,n+1))
string ='x'
string1 = 'The dimensions for your agumented matrix are:'
var1 = n
var2 = n+1

write(*,30)string1,var1,string,var2
write(*,*)'-----------------------------------------------------'
write(*,*)'Enter your equations in agumented matrix form'
write(*,*)'-----------------------------------------------------'
open(unit =1, file = 'gauss_data.dat')
do i = 1,n
	read(*,*)(A(i,j), j = 1,n+1)
	write(1,*)(A(i,j), j = 1,n+1)
enddo
write(*,*)'-----------------------------------------------------'
write(*,*)'The agumented matrix is'
do i = 1,n
	write(*,10)(A(i,j), j = 1,n+1)
enddo
write(*,*)'-----------------------------------------------------'
!!-------------Calling SUBROUTINE----------------------------!!
call gaussf90(A,X,temp,n, sort, tri)
!!------------------------------------------------------------!!

!!------------Displaying sorted matrix------------------------!!
write(*,*)'The sorted matrix is'
do i = 1,n
	write(*,10)(sort(i,j), j = 1, n+1)
enddo
write(*,*)'-----------------------------------------------------'

!!----------Displaying trangularized matrix--------------------!!
write(*,*)'The triangularized matrix is'
do i = 1,n
	write(*,10)(tri(i,j), j = 1, n+1)
enddo
write(*,*)'-----------------------------------------------------'

!!---------Displaying Solutions--------------------------------!!
write(*,*)'The solutions of the equations are X(1,2,3....)Ampere'
write(*,10)(X(i), i = 1,n)
write(*,*)'-----------------------------------------------------'
!!--------------------------------------------------------------!!

!!--------------FORMAT STATEMENTS--------------------------------!!
10 format(7F8.2)
20 format(A,I2)
30 format(A, I2, A, I2)

end program gauss_elimination


!!----------------------SUBROUTINE----------------------------!!
subroutine gaussf90(A,X,temp,n, sort, tri)
integer, intent(in)::n
real, intent(inout)::A(n,n+1)
real, intent(out)::sort(n,n+1), tri(n,n+1), X(n), temp(n+1)
!!---------Pivoting and row swapping------------------!!
do i = 1,n
	flag = 0
	do k = 1,n-1
		if(abs(A(k,i))<A(k+1,i))then
			temp = A(k,:)
			A(k,:) = A(k+1,:)
			A(k+1,:) = temp
			flag = 1
		endif
	enddo
	if(flag==0)exit
enddo
do i = 1,n
	do j = 1, n+1
		sort(i,j) = A(i,j) 	
	enddo
enddo


!!-----------------Triangularization------------------------!!
do k = 1,n-1
	do i = k+1, n
	temp_1 = A(i,k)/A(k,k)
		do j = 1, n+1
			A(i,j) = A(i,j) - temp_1*(A(k,j))
		enddo
	enddo
enddo
do i = 1,n
	do j = 1,n+1
		tri(i,j) = A(i,j)
	enddo
enddo

!!----------Back substitution------------------------------!!
X(n) = A(n,n+1)/A(n,n)

do i = n-1, 1, -1
	temp_2 = 0
	do j = i+1, n
	temp_2 = temp_2 + (A(i,j)*X(j))
	enddo
	X(i) = (A(i,n+1) - temp_2)/(A(i,i))
enddo
return
end subroutine
			
	
