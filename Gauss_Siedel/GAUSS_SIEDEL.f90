!!-----------------------------------------------------
!! This program performs Gauss Siedel Method
!! Original Author: Raj Handique
!! Date Created: 16 November 2024
!! FORTRAN 95 Source Code
!!-----------------------------------------------------
program GAUSS_SIEDEL

  !!---------------------------------
  !! This program will require the user to declare all the variables and typecast explicitly !!
  !!---------------------------------

  implicit none
  
  !!---------------------------------
  !! Declare the variables
  !!---------------------------------
  real::A(10,10), X(10), B(10), tolerance, X_init(10), sum
  integer::n, i, j, k, n_iter
  
  !!---------------------------------
  !! Analysis Block
  !!---------------------------------
  tolerance = 0.001
  WRITE(*,*)"---------------------------------"
  WRITE(*,*)"Enter the dimensions of matrix A:"
  READ(*,*) n
  WRITE(*,*)"The dimension is:", n
  WRITE(*,*)"---------------------------------"

  OPEN(UNIT=1, FILE='gSiedel.dat')

  WRITE(*,*)"The Augmented Matrix is:"
  WRITE(*,*)"---------------------------------"
  DO i=1, n
     READ(1,*) (A(i,j),j=1, n+1)
     WRITE(*,*) (A(i,j),j=1, n+1)
  ENDDO
  WRITE(*,*)"---------------------------------"

  WRITE(*,*)"The Matrix B: [AX = B]"
  WRITE(*,*)"---------------------------------"
  DO i=1, n
     B(i) = A(i, n+1)
     WRITE(*,*) B(i)
  ENDDO
  WRITE(*,*)"---------------------------------"

  WRITE(*,*)"Enter the number of iterations:"
  READ(*,*) n_iter
  WRITE(*,*)"The number of iterations:", n_iter

  DO k=1, n_iter
     X_init = X
     DO i=1, n
        sum = 0.0
        DO j=1, n
           IF(j.NE.i) THEN
              sum = sum + (A(i,j) * X(j))
           ENDIF
        ENDDO
        X(i) = (B(i) - sum)/A(i,i)
     ENDDO
     IF(MAXVAL(ABS(X-X_init)) < tolerance) THEN
        WRITE(*,*)"The number of iterations used in convergence:(k)", k
        EXIT
     ENDIF
  ENDDO

  WRITE(*,*)"---------------------------------"
  WRITE(*,*)"The Solution Matrix is:"
  DO i=1, n
     WRITE(*,*) X(i)
  ENDDO
  WRITE(*,*)"---------------------------------"
  
end program GAUSS_SIEDEL
