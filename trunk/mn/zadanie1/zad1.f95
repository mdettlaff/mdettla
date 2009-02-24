PROGRAM ZAD1
  REAL(8) :: W_i, x, f_small, suma = 0
  INTEGER :: i, j, n, n_start = 5
  REAL(8) :: PI
  PI = 2*ACOS(0.0)

  PRINT *,'Ca³ka oznaczona z F(x)=1/(x^2+1) od 0 do niesk. = PI/2 ~= 1.5707963'
  n = n_start
  DO j = 0, 3, 1 ! petla liczaca sume przyblizajaca dana calke dla roznych n
    suma = 0
    W_i = PI/n
    DO i = 0, n-1, 1 ! wlasciwa petla liczaca sume
      x = x_i(i, n)
      f_small = 2 * F3((1+x)/(1-x)) * ((1-x)**(-2)) * SQRT(1-x**2)
      suma = suma + W_i * f_small
    END DO
    WRITE(*,"('\tdla n =',i5,'\t',f8.6)") n, suma
    n = n*10
  END DO

  PRINT *
  PRINT *,'Ca³ka oznaczona z F(x)=exp(-x) od 0 do niesk. = 1'
  n = n_start
  DO j = 0, 3, 1 ! petla liczaca sume przyblizajaca dana calke dla roznych n
    suma = 0
    W_i = PI/n
    DO i = 0, n-1, 1 ! wlasciwa petla liczaca sume
      x = x_i(i, n)
      f_small = 2 * F1((1+x)/(1-x)) * ((1-x)**(-2)) * SQRT(1-x**2)
      suma = suma + W_i * f_small
    END DO
    WRITE(*,"('\tdla n =',i5,'\t',f8.6)") n, suma
    n = n*10
  END DO

  PRINT *
  PRINT *,'Ca³ka oznaczona z F(x)=sin(x)*exp(-x) od 0 do niesk. = 1/2'
  n = n_start
  DO j = 0, 3, 1 ! petla liczaca sume przyblizajaca dana calke dla roznych n
    suma = 0
    W_i = PI/n
    DO i = 0, n-1, 1 ! wlasciwa petla liczaca sume
      x = x_i(i, n)
      f_small = 2 * F2((1+x)/(1-x)) * ((1-x)**(-2)) * SQRT(1-x**2)
      suma = suma + W_i * f_small
    END DO
    WRITE(*,"('\tdla n =',i5,'\t',f8.6)") n, suma
    n = n*4
  END DO

  PRINT *
  PRINT *,'Ca³ka oznaczona z F(x)=x^2/2^x od 0 do niesk. ~= 6.0055614'
  n = n_start
  DO j = 0, 3, 1 ! petla liczaca sume przyblizajaca dana calke dla roznych n
    suma = 0
    W_i = PI/n
    DO i = 0, n-1, 1 ! wlasciwa petla liczaca sume
      x = x_i(i, n)
      f_small = 2 * F4((1+x)/(1-x)) * ((1-x)**(-2)) * SQRT(1-x**2)
      suma = suma + W_i * f_small
    END DO
    WRITE(*,"('\tdla n =',i5,'\t',f8.6)") n, suma
    n = n*4
  END DO
END PROGRAM ZAD1


REAL FUNCTION F1(x)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x

  F1 = EXP(-x)
END FUNCTION F1

REAL FUNCTION F2(x)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x

  F2 = SIN(x) * EXP(-x)
END FUNCTION F2

REAL FUNCTION F3(x)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x

  F3 = 1/(x**2+1)
END FUNCTION F3

REAL FUNCTION F4(x)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x

  F4 = x**2 / 2**x
END FUNCTION F4

REAL FUNCTION x_i(i, n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: i
  INTEGER, INTENT(IN) :: n
  REAL(8) :: PI
  PI = 2*ACOS(0.0)

  x_i = COS((PI*(2*i-1))/(2*n))
END FUNCTION x_i
