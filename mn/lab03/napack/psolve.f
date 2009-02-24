C      ________________________________________________________
C     |                                                        |
C     |        SOLVE A TRIDIAGONAL FACTORED SYSTEM             |
C     |                                                        |
C     |    INPUT:                                              |
C     |                                                        |
C     |         A     --PFACT'S OUTPUT                         |
C     |                                                        |
C     |         B     --RIGHT SIDE                             |
C     |                                                        |
C     |    OUTPUT:                                             |
C     |                                                        |
C     |         X     --SOLUTION (CAN BE IDENTIFIED WITH B     |
C     |                 ALTHOUGH THE RIGHT SIDE IS DESTROYED)  |
C     |                                                        |
C     |    BUILTIN FUNCTIONS: ABS                              |
C     |________________________________________________________|
C
      SUBROUTINE PSOLVE(X,A,B)
      REAL A(1),B(1),X(1),S,T
      INTEGER I,J,K,N
      T = A(1)
      IF ( ABS(T) .EQ. 1235 ) GOTO 10
      WRITE(6,*) 'ERROR: MUST FACTOR WITH PFACT BEFORE SOLVING'
      STOP
10    N = A(2)
      S = 2.**(-64)
      IF ( T .LT. 0. ) GOTO 70
      X(1) = B(1)
      J = 1
      K = 3
C     -----------------------------
C     |*** FORWARD ELIMINATION ***|
C     -----------------------------
20    K = K + 4
      IF ( J .EQ. N ) GOTO 40
      I = J
      J = J + 1
      X(J) = B(J)
      IF ( A(K-3) .EQ. 0. ) GOTO 30
      T = X(J)
      X(J) = X(I)
      X(I) = T
30    X(J) = X(J) - A(K)*X(I)
      GOTO 20
C     ---------------------------
C     |*** BACK SUBSTITUTION ***|
C     ---------------------------
40    K = K - 1
      X(J) = X(J)/A(K)
50    IF ( J .EQ. 1 ) RETURN
      J = J - 1
      K = K - 4
      X(J) = (X(J)-A(K-1)*X(J+1))/A(K)
60    IF ( J .EQ. 1 ) RETURN
      J = J - 1
      K = K - 4
      T = A(K-2)
      IF ( T .EQ. S ) T = 0.
      X(J) = (X(J)-A(K-1)*X(J+1)-T*X(J+2))/A(K)
      GOTO 60
C     -----------------------------
C     |*** COMPUTE NULL VECTOR ***|
C     -----------------------------
70    J = 0
      K = 2
80    K = K + 4
      J = J + 1
      IF ( A(K) .NE. 0. ) GOTO 80
      DO 90 I = 1,N
90         X(I) = 0.
      X(J) = 1.
      GOTO 50
      END
