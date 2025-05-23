!!-----------------------------------------------------
!! This program performs LU Decomposition method
!! Original Author: Raj Handique
!! Date Created: 16 November 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program LU_DECOMPOSITION

  !!---------------------------------
  !! This program will require the user to declare all the variables and typecast explicitly !!
  !!---------------------------------

  implicit none

  !!---------------------------------
  !! Declare the variables
  !!---------------------------------

  integer:: n !! for n x n matrix
  real::A(10,10)
  integer::i, j

  !!---------------------------------
  !! Analysis Block
  !!---------------------------------

  WRITE(*,*)"Enter the dimension of matrix:"
  READ(*,*) n
  WRITE(*,*)"The entered dimension is:", n

  WRITE(*,*)"Enter the values for Matrix A:"
  DO i=1, n
        READ(*,*)(A(i, j), j=1, n)
  ENDDO

  WRITE(*,*)"The Matrix A:"
  WRITE(*,*)"-----------------------------"
  DO i=1, n
     WRITE(*,*)(A(i,j), j=1, n)
  ENDDO
  WRITE(*,*)"-----------------------------"

  call ludcp(n, A)
  
end program LU_DECOMPOSITION

!!---------------------------------
!! sub-routine
!!---------------------------------


subroutine ludcp(n, A)
  implicit none
  integer:: n, i, j, k
  real:: temp, temp2, sum
  real, intent(IN)::A(10,10)
  real::L(10,10), U(10,10), Y(10), M(10,10), X(10)

  !! Intialise the Matrix 
  L = 0.0
  U = 0.0
  
  DO i=1, n
     DO j=1, n
        U(i,j) = A(i,j)
        DO k=1, i-1
           U(i,j) = U(i,j) - L(i,k) * U(k,j)
        ENDDO
     ENDDO
     
     !! Making L an identity Matrix
     L(i,i)=1.0
     
     DO j=1+1,n
        L(j,i) = A(j,i)
        DO k=1, i-1
           L(j,i) = L(j,i) - L(j,k) * U(k,i) 
        ENDDO
        L(j,i) = L(j,i) / U(i,i)
     ENDDO
  ENDDO

  WRITE(*,*)"The Lower Triangularised Matrix(L):"
  WRITE(*,*)"-----------------------------------"
  DO i=1, n
     WRITE(*,*)(L(i,j),j=1, n)
  ENDDO
  WRITE(*,*)"-----------------------------------"

  WRITE(*,*)"The Upper Triangularised Matrix(U):"
  WRITE(*,*)"-----------------------------------"
  DO i=1, n
     WRITE(*,*)(U(i,j),j=1, n)
  ENDDO
  WRITE(*,*)"-----------------------------------"

  !! Verify if LU = A
  DO i=1, n
     DO j=1, n
        sum = 0
        DO k=1, n
           sum = sum + L(i,k)*U(k,j)
        ENDDO
        M(i,j) = sum
     ENDDO
  ENDDO
  
  WRITE(*,*)"The Verification (LU = A):"
  WRITE(*,*)"-----------------------------------"
  DO i=1, n
     WRITE(*,*)(M(i,j),j=1, n)
  ENDDO
  WRITE(*,*)"-----------------------------------"
  
  !! Forward Substitution

  Y(1) = A(1,n)
  DO i=2,n
     temp =0
     DO j = 1, n-1
        temp = temp + L(i,j)*Y(j)
     ENDDO
     Y(i) = A(i,n) - temp
  ENDDO

  WRITE(*,*)"-----------------------------------"
  WRITE(*,*)"The Matrix Y is:"
  WRITE(*,*)(Y(i), i=1,n)
  WRITE(*,*)"-----------------------------------"

  !! Backward Substitution to find the solution
  X(n) = Y(n)/U(n,n)

  DO i=n-1, 1, -1
     temp2 = 0
     DO j = i+1, n
        temp2 = temp2 + (U(i,j)*X(j))
     ENDDO
     X(i) = (Y(i) - temp2)/(U(i,i))
  ENDDO
  
  WRITE(*,*)"-----------------------------------"
  WRITE(*,*)"The Matrix X (Solution) is:"
  WRITE(*,*)(X(i), i=1,n)
  WRITE(*,*)"-----------------------------------"
  
end subroutine ludcp

