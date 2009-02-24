PROGRAM METODA_TRAPEZOW
  IMPLICIT NONE

  INTERFACE
    FUNCTION INTEGRAL(h, y_n, a, b)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: h
      REAL(8), DIMENSION(9999), INTENT(IN) :: y_n
      REAL(8), INTENT(IN) :: a
      REAL(8), INTENT(IN) :: b
      REAL(8) :: INTEGRAL
    END FUNCTION INTEGRAL
    FUNCTION F(x)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: x
      REAL(8) :: F
    END FUNCTION F
  END INTERFACE

  REAL(8) :: a ! pocz±tek obszaru ca³kowania
  REAL(8) :: b ! koniec obszaru ca³kowania
  REAL(8) :: x, h
  REAL(8) :: y_n(9999) ! ci±g {y_n}
  INTEGER :: i

  a = 0.0
  b = 0.99
  h = 0.1

  WRITE(*,"('Ca³kowanie na przedziale [',f4.2,', ',f4.2,']')") a, b
  x = a
  DO i = 1, 9999, 1 ! definiujemy funkcjê jako zbiór punktów
    y_n(i) = F(x)
    x = x + h
  END DO
  WRITE(*,"('\tdla h =',f4.2,'\tcalka = ',f8.6)") h, INTEGRAL(h, y_n, a, b)

END PROGRAM METODA_TRAPEZOW

! liczy ca³kê metod± trapezów
REAL FUNCTION INTEGRAL(h, y_n, a, b)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: h
  REAL(8), DIMENSION(9999), INTENT(IN) :: y_n
  REAL(8), INTENT(IN) :: a
  REAL(8), INTENT(IN) :: b

  REAL(8) :: x
  INTEGER :: j

  INTEGRAL = 0
  x = a
  j = 1
  DO ! pêtla licz±ca ca³kê
    IF (x .GT. b) EXIT
    INTEGRAL = INTEGRAL + h*y_n(j)
    j = j + 1
    x = x + h
  END DO
  ! bierzemy pod uwagê po³ówki na krañcach
  INTEGRAL = INTEGRAL - 0.5*h*y_n(1)
  INTEGRAL = INTEGRAL - 0.5*h*y_n(j)

  ! bierzemy poprawkê je¶li prawa granica ca³kowania nie wypad³a w wê¼le
  x = x-h
  IF (x .LT. b) THEN
    INTEGRAL = INTEGRAL + y_n(j)*(b-x) + (y_n(j+1)-y_n(j))/(4*h)*(b-x)**2
  END IF
END FUNCTION INTEGRAL

! funkcja, któr± bêdziemy ca³kowaæ
REAL FUNCTION F(x)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x

  F = SIN(x)
END FUNCTION F
