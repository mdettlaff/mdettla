module my_matrix

contains
	subroutine write_matrix(A)
		real, dimension(:,:) :: A
		print*
		do i = lbound(A,1), ubound(A,1)
				print*, (A(i,j), j = lbound(A,2), ubound(A,2))
		end do
		print*
	end subroutine write_matrix

	function eye(n)
		integer, intent(in) :: n
		real,allocatable,dimension(:,:) :: eye
		real::temp(n*n)
		
		temp = 0; temp(1:n*n:n+1) = 1
		allocate(eye(n,n))
		eye = reshape(temp,(/n,n/))
	end function eye

end module my_matrix

program aj
	use my_matrix
	
	integer,parameter::n = 3
	integer::matrix_sizes(2) = (/n,n/)
	real::A(n,n) = reshape( (/ 1,2,3,1,2,2,1,3,1 /), (/n,n/) ) ! 1,2,3,1,... - elementy czytane kolumnami
        real::copy_of_A(n,n)
	real::b(n) = (/3,7,6/) 
	real::Ainv(n,n)
        integer :: IPIV(n), INFO
	
        print*, "Macierz   A:"
	call write_matrix(A)
        copy_of_A = A
	
	call sgesv(n, 1, copy_of_A, n, IPIV, b, n, INFO) ! rozwiązuje układ równań
	print*, "Rozwiazanie ukladu rownan Ax=b,", " czyli x =", b
	
	Ainv = eye(n)
        copy_of_A = A
	call sgesv(n, n, copy_of_A, n, IPIV, Ainv, n, INFO) ! odwraca macierz 
	print*, "Macierz odwrotna do macierzy A : "
	call write_matrix(Ainv)
	
end program aj


! http://www.physics.orst.edu/~rubin/nacphy/lapack/routines/sgesv.html :

! SGESV(l)	     LAPACK driver routine (version 1.1)	     SGESV(l)
! 
! NAME
!   SGESV	- compute the solution to a real system	of linear equations  A * X =
!   B,
! 
! SYNOPSIS
! 
!   SUBROUTINE SGESV( N, NRHS, A,	LDA, IPIV, B, LDB, INFO	)
! 
!       INTEGER	    INFO, LDA, LDB, N, NRHS
! 
!       INTEGER	    IPIV( * )
! 
!       REAL	    A( LDA, * ), B( LDB, * )
! 
! PURPOSE
!   SGESV	computes the solution to a real	system of linear equations
!      A * X = B,	where A	is an N-by-N matrix and	X and B	are N-by-NRHS
!   matrices.
! 
!   The LU decomposition with partial pivoting and row interchanges is used to
!   factor A as
!      A = P * L * U,
!   where	P is a permutation matrix, L is	unit lower triangular, and U is	upper
!   triangular.  The factored form of A is then used to solve the	system of
!   equations A *	X = B.
! 
! ARGUMENTS
! 
!   N	  (input) INTEGER
! 	  The number of	linear equations, i.e.,	the order of the matrix	A.  N
! 	  >= 0.
! 
!   NRHS	  (input) INTEGER
! 	  The number of	right hand sides, i.e.,	the number of columns of the
! 	  matrix B.  NRHS >= 0.
! 
!   A	  (input/output) REAL array, dimension (LDA,N)
! 	  On entry, the	N-by-N coefficient matrix A.  On exit, the factors L
! 	  and U	from the factorization A = P*L*U; the unit diagonal elements
! 	  of L are not stored.
! 
!   LDA	  (input) INTEGER
! 	  The leading dimension	of the array A.	 LDA >=	max(1,N).
! 
!   IPIV	  (output) INTEGER array, dimension (N)
! 	  The pivot indices that define	the permutation	matrix P; row i	of
! 	  the matrix was interchanged with row IPIV(i).
! 
!   B	  (input/output) REAL array, dimension (LDB,NRHS)
! 	  On entry, the	N-by-NRHS matrix of right hand side matrix B.  On
! 	  exit,	if INFO	= 0, the N-by-NRHS solution matrix X.
! 
!   LDB	  (input) INTEGER
! 	  The leading dimension	of the array B.	 LDB >=	max(1,N).
! 
!   INFO	  (output) INTEGER
! 	  = 0:	successful exit
! 	  < 0:	if INFO	= -i, the i-th argument	had an illegal value
! 	  > 0:	if INFO	= i, U(i,i) is exactly zero.  The factorization	has
! 	  been completed, but the factor U is exactly singular,	so the solu-
! 	  tion could not be computed.
