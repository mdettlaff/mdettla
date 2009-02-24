program ZAD2
  integer,parameter::n = 3 ! wymiar macierzy i wektorów
  integer::matrix_sizes(2) = (/n,n/)
  real::A(n,n) ! macierz pochodnych cz±stkowych F'(x_k)
  real::copy_of_A(n,n)
  real::x(n) = (/0,0,0/) ! przybli¿enie startowe dla x, y, z
  real::d(n) ! wektor d
  real::F_x_k(n) ! wektor F(x_k) obliczony dla uk³adu funkcji F(x*)
  real::minusF_x_k(n) ! -F(x_k)
  integer :: IPIV(n), INFO
  integer :: k, n_iter ! liczba iteracji pêtli licz±cej rozwi±zanie
  
  print*, "Rozwi±zywanie uk³adu równañ nieliniowych metod± Newtona."

  do n_iter = 0, 4, 1 ! pêtla licz±ca rozwi±zanie dla ró¿nych n
    do k = 0, n_iter-1, 1 ! wlasciwa petla liczaca rozwiazanie
      ! rozwiazujemy uk³ad liniowy, aby uzyskaæ d
      A(1,1) = F1_dx(x(1), x(2), x(3))
      A(1,2) = F1_dy(x(1), x(2), x(3))
      A(1,3) = F1_dz(x(1), x(2), x(3))
      A(2,1) = F2_dx(x(1), x(2), x(3))
      A(2,2) = F2_dy(x(1), x(2), x(3))
      A(2,3) = F2_dz(x(1), x(2), x(3))
      A(3,1) = F3_dx(x(1), x(2), x(3))
      A(3,2) = F3_dy(x(1), x(2), x(3))
      A(3,3) = F3_dz(x(1), x(2), x(3))
      F_x_k = (/F1(x(1),x(2),x(3)), F2(x(1),x(2),x(3)), F3(x(1),x(2),x(3))/)
      minusF_x_k = - F_x_k
      copy_of_A = A
      ! rozwi±zuje uk³ad równañ
      call sgesv(n, 1, copy_of_A, n, IPIV, minusF_x_k, n, INFO)
      d = minusF_x_k ! rozwi±zanie uk³adu równañ

      x = x + d ! tutaj nastêpuje krok kolejnego przybli¿enia rozwi±zania
    end do

    print*, "\tdla n=", n_iter, ":\t", "x=",x(1), "  y=",x(2), "  z=",x(3)
  end do

  !print*
  !print*, "Sprawdzenie:"
  !print*, x(1) + x(1)**2 - 2*x(2)*x(3), " = ", 0.1
  !print*, x(2) - x(2)**2 + 3*x(1)*x(3), " = ", -0.2
  !print*, x(3) + x(3)**2 + 2*x(1), " = ", 0.3
  
end program ZAD2


real function F1(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F1 = x + x**2 -2*y*z - 0.1
end function F1

real function F2(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F2 = y - y**2 + 3*x*z + 0.2
end function F2

real function F3(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F3 = z + z**2 + 2*x - 0.3
end function F3


real function F1_dx(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F1_dx = 2*x + 1
end function F1_dx

real function F1_dy(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F1_dy = -2*z
end function F1_dy

real function F1_dz(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F1_dz = -2*y
end function F1_dz

real function F2_dx(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F2_dx = 3*z
end function F2_dx

real function F2_dy(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F2_dy = -2*y + 1
end function F2_dy

real function F2_dz(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F2_dz = 3*x
end function F2_dz

real function F3_dx(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F3_dx = 2
end function F3_dx

real function F3_dy(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F3_dy = 0
end function F3_dy

real function F3_dz(x, y, z)
  implicit none
  real, intent(in) :: x
  real, intent(in) :: y
  real, intent(in) :: z

  F3_dz = 2*z + 1
end function F3_dz
