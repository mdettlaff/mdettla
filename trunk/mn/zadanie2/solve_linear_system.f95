module my_matrix

contains
  subroutine write_matrix(A)
    real, dimension(:,:) :: A
    do i = lbound(A,1), ubound(A,1)
        print*, (A(i,j), j = lbound(A,2), ubound(A,2))
    end do
    print*
  end subroutine write_matrix
end module my_matrix

program ZAD2
  use my_matrix
  
  integer,parameter::n = 3
  integer::matrix_sizes(2) = (/n,n/)
  ! 1,2,3,1,... - elementy czytane kolumnami
  real::A(n,n) = reshape( (/ 1,2,3,1,2,2,1,3,1 /), (/n,n/) )
  real::copy_of_A(n,n)
  real::b(n) = (/3,7,6/) 
  integer :: IPIV(n), INFO
  
  print*, "Macierz A:"
  call write_matrix(A)
  copy_of_A = A
  print*, "b:"
  print*, b
  
  call sgesv(n, 1, copy_of_A, n, IPIV, b, n, INFO) ! rozwiązuje układ równań
  print*
  print*, "Rozwiazanie ukladu rownan Ax=b:"
  print*, b
  
end program ZAD2
