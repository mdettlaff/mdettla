PROGRAM ZAD3
  REAL(8) :: x, y, h
  INTEGER :: i, n

  PRINT *,'Metoda Eulera. Przybli¿one rozwi±zanie równania ró¿niczkowego.'
  PRINT *,"y' = -y^2 - 1/(4x^2 + 1) na przedziale [0,2]; y(0)=1"
  n = 10
  DO j = 0, 4, 1 ! petla liczaca rozwiazanie dla roznych n
    x = 0.0
    y = 2.0
    h = y/n
    DO i = 0, n-1, 1 ! wlasciwa petla liczaca rozwiazanie
      y = y + h * F(x, y)
      x = x + h
    END DO
    WRITE(*,"('\tdla n =',i7,'\ty_n = ',f8.6)") n, y
    n = n*10
  END DO
END PROGRAM ZAD3


REAL FUNCTION F(x, y)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x
  REAL(8), INTENT(IN) :: y

  F = -y**2 - 1/(4*x**2 + 1)
END FUNCTION F
