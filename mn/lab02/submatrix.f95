interface
        subroutine write_matrix(A)
		real, dimension(:,:) :: A
end subroutine  write_matrix
end interface

        integer, parameter :: n = 5, j=3,k=3
	real, dimension(n,n)    :: A = reshape( (/ (i,i=1,n*n) /), (/n,n/) )
        integer :: ind(2) 

        A = transpose (A)

        call write_matrix( A((/ (i,i=1,j-1),(i,i=j+1,n) /),(/ (i,i=1,k-1),(i,i=k+1,n) /)))

end

subroutine write_matrix(A)
	real, dimension(:,:) :: A
        print*
        do i = lbound(A,1), ubound(A,1)
                        print*, (A(i,j), j = lbound(A,2), ubound(A,2))
        end do
end subroutine write_matrix
