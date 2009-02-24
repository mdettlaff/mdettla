PROGRAM ZAD4
  IMPLICIT NONE

  INTERFACE
    FUNCTION F(x, h, y_n, i)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: x
      REAL(8), INTENT(IN) :: h
      REAL(8), DIMENSION(9999), INTENT(IN) :: y_n
      INTEGER, INTENT(IN) :: i ! ilo¶æ wêz³ów
      REAL(8) :: F
    END FUNCTION F
  END INTERFACE

  REAL(8) :: x, h
  REAL(8) :: y_n(9999) ! ci±g {y_n}
  REAL(8) :: ERROR, EPS
  INTEGER :: i, j, n

  PRINT *,'Metoda Eulera. Przybli¿one rozwi±zanie zagadnienia.'
  PRINT *,'Wynik teoretyczny dla przedzia³u [0,1]: 0.367879441'
  n = 5
  DO j = 0, 3, 1 ! petla liczaca rozwiazanie dla roznych n
    x = 0.0
    h = 1.0/n
    y_n(1) = 1.0
    DO i = 1, n, 1 ! wlasciwa petla liczaca rozwiazanie
      y_n(i+1) = y_n(i) + h * F(x, h, y_n, i)
      x = x + h
    END DO
    WRITE(*,"('\tdla n =',i7,'\ty_n = ',f8.6)") n, y_n(i)

    ! liczymy b³±d metody
    EPS = 0.0 ! EPS to najwiêkszy znaleziony b³±d (ten którego szukamy)
    x = 0.0
    DO i = 1, n, 1
      ERROR = ABS(EXP(-x) - y_n(i))
      IF (ERROR .GT. EPS) EPS = ERROR
      x = x + h
    END DO
    WRITE(*,"('\t  b³±d metody: \tEps_n = ',f8.6)") EPS

    n = n*2
  END DO
END PROGRAM ZAD4


REAL FUNCTION F(x, h, y_n, i)
  IMPLICIT NONE
  INTERFACE
    FUNCTION INTEGRAL(h, y_n, i)
      IMPLICIT NONE
      REAL(8), INTENT(IN) :: h
      REAL(8), DIMENSION(9999), INTENT(IN) :: y_n
      INTEGER, INTENT(IN) :: i ! ilo¶æ wêz³ów
      real(8) :: INTEGRAL
    END FUNCTION INTEGRAL
  END INTERFACE
  REAL(8), INTENT(IN) :: x
  REAL(8), INTENT(IN) :: h
  REAL(8), DIMENSION(9999), INTENT(IN) :: y_n
  INTEGER, INTENT(IN) :: i ! ilo¶æ wêz³ów

  F = -y_n(i) - x + INTEGRAL(h, y_n, i)
END FUNCTION F

! liczy ca³kê od 0 do t, z e^s * y(s)
REAL FUNCTION INTEGRAL(h, y_n, i)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: h
  REAL(8), DIMENSION(9999), INTENT(IN) :: y_n
  INTEGER, INTENT(IN) :: i ! ilo¶æ wêz³ów

  REAL(8) :: s
  INTEGER :: j

  s = 0.0
  INTEGRAL = 0
  DO j = 1, i, 1 ! pêtla licz±ca ca³kê
    INTEGRAL = INTEGRAL + h*y_n(j)*EXP(s)
    s = s + h
  END DO
  ! bierzemy pod uwagê po³ówki na krañcach
  INTEGRAL = INTEGRAL - 0.5*h*y_n(1)*EXP(0.0)
  INTEGRAL = INTEGRAL - 0.5*h*y_n(i)*EXP(s-h)
END FUNCTION INTEGRAL
