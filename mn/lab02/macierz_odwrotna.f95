program macierz_odwrotna
implicit none
integer :: k, i, j, z
integer, dimension(1) :: max_j
integer, parameter :: n=2
integer, parameter :: DP=selected_real_kind(14)
real(kind=DP), dimension(n) :: jedna_kolumna
real(kind=DP), dimension(n,n) :: a
real(kind=DP) :: det, pi8=3.14159265359_DP/8.0_DP
!do i=1, n; do j=1, n; a(i,j) = sin(pi8/i + pi8/j)
!do i=1, n; do j=1, n; z=z+1; a(j,i) = z
a(1,1)=1; a(1,2)=2
a(2,1)=3; a(2,2)=4
!end do; end do;

print*, A

det=1.0_DP

po_wierszach: do k=1, n-1
max_j = maxloc(abs(a(k, :)))
j=max_j(1)

if (j > k) then
  jedna_kolumna = a(:, j)
  a(:, j) = a(:, k)
  a(:, k) = jedna_kolumna
  det = -det
end if

if ( a(k,k) .eq. 0.0_DP) then
  det=0.0_DP
  EXIT po_wierszach
end if

a(k,k:) = a(k, k:)/a(k,k)
det = det*a(k,k)

po_i: do i=k+1,n
a(i,k:) = a(i,k:) - a(k,k:)*a(i,k)
end do po_i;
end do po_wierszach

det=det*a(n,n)

write(*,4444) det
4444 format(" wyznacznik = ", E25.12)
stop
end program macierz_odwrotna
