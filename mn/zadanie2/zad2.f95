program ZAD2
  implicit none

  interface
    function F(x)
      implicit none
      real, dimension(3), intent(in) :: x
      real, dimension(3) :: F
    end function F

    function F_d(x)
      implicit none
      real, dimension(3), intent(in) :: x
      real, dimension(3,3) :: F_d
    end function F_d
  end interface

  integer, parameter :: n = 3 ! wymiar macierzy i wektorów
  real, dimension(n) :: x(n) = (/0,0,0/) ! przybli¿enie startowe dla x, y, z
  real :: d(n) ! wektor d
  real :: minusF_x_k(n) ! -F(x_k)
  integer :: IPIV(n), INFO
  integer :: k, n_iter ! liczba iteracji pêtli licz±cej rozwi±zanie
  
  print*, "Rozwi±zywanie uk³adu równañ nieliniowych metod± Newtona."

  do n_iter = 0, 4, 1 ! pêtla licz±ca rozwi±zanie dla ró¿nych n
    do k = 0, n_iter-1, 1 ! w³a¶ciwa pêtla licz±ca rozwi±zanie
      ! rozwi±zujemy uk³ad liniowy F'(x)d=-F(x), aby uzyskaæ d
      minusF_x_k = -F(x)
      ! rozwi±zuje uk³ad równañ (na kopii A, ¿eby nie zmodyfikowaæ A)
      call sgesv(n, 1, F_d(x), n, IPIV, minusF_x_k, n, INFO)
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


! zwraca uk³ad funkcji F(x) obliczony dla danego wektora x
function F(x)
  implicit none
  real, dimension(3) :: F(3)
  real, intent(in) :: x(3)

  F(1)= x(1) + x(1)**2 -2*x(2)*x(3) - 0.1
  F(2) = x(2) - x(2)**2 + 3*x(1)*x(3) + 0.2
  F(3) = x(3) + x(3)**2 + 2*x(1) - 0.3
end function F

! zwraca macierz pochodnych uk³adu funkcji F(x) dla danego wektora x
function F_d(x)
  implicit none
  real, dimension(3,3) :: F_d(3,3)
  real, intent(in) :: x(3)

  F_d(1,:) = (/ 2*x(1)+1, -2*x(3), -2*x(2) /)
  F_d(2,:) = (/ 3*x(3), -2*x(2)+1, 3*x(1) /)
  F_d(3,:) = (/ 2.0, 0.0, 2*x(3)+1 /)
end function F_d
