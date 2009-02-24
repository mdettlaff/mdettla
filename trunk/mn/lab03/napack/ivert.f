C
C      ________________________________________________________
C     |                                                        |
C     |     INVERT A SYMMETRIC MATRIX WITH PARTIAL PIVOTING    |
C     |                                                        |
C     |    INPUT:                                              |
C     |                                                        |
C     |         V     --IFACT'S OUTPUT                         |
C     |                 (LENGTH AT LEAST 3+(N+9)N/2)           |
C     |                                                        |
C     |         W     --WORK ARRAY WITH AT LEAST 2N-2 ELEMENTS |
C     |                                                        |
C     |    OUTPUT:                                             |
C     |                                                        |
C     |         V     --SYMMETRIC INVERSE MATRIX STORED IN COM-|
C     |                 PRESSED MODE                           |
C     |                                                        |
C     |    BUILTIN FUNCTIONS: ABS                              |
C     |    PACKAGE SUBROUTINES: PSOLVE                         |
C     |________________________________________________________|
C
      SUBROUTINE IVERT(V,W)
      REAL V(1),W(1),T
      INTEGER H,I,J,K,L,M,N,O,P
      T = V(1)
      IF ( ABS(T) .EQ. 1237 ) GOTO 10
      WRITE(6,*) 'ERROR: MUST FACTOR WITH IFACT BEFORE INVERTING'
      STOP
10    IF ( T .GT. 0. ) GOTO 20
      WRITE(6,*) 'ERROR: MATRIX HAS NO INVERSE'
      STOP
20    N = V(2)
      IF ( N .GT. 1 ) GOTO 30
      V(1) = 1./V(9)
      RETURN
30    IF ( N .GT. 2 ) GOTO 40
      W(1) = 1.
      W(2) = 0.
      CALL PSOLVE(W,V(4),W)
      V(1) = W(1)
      V(2) = W(2)
      W(1) = 0.
      W(2) = 1.
      CALL PSOLVE(W,V(4),W)
      V(3) = W(2)
      RETURN
C     ------------------------
C     |*** EXTRACT PIVOTS ***|
C     ------------------------
40    O = 1 + (N*(N+1))/2
      L = N + N - 2
      K = 9 + O + L
      M = N + 1
      J = K - M
      DO 50 I = M,L
50         W(I) = V(I+J)
C     ---------------------------
C     |*** REARRANGE STORAGE ***|
C     ---------------------------
      J = N + N - 4
60    K = K - 1
      V(K+J) = V(K)
      IF ( K .GT. 1 ) GOTO 60
      M = N - 1
      DO 70 I = 1,N
70         W(I) = 0.
      W(1) = 1.
      CALL PSOLVE(W,V(O),W)
      L = N
      J = M
      P = O - 1 - N
80    T = W(J)
      DO 90 I = L,N
90         T = T - W(I)*V(I+P)
      L = J
      W(L) = T
      J = J - 1
      P = P - N + L
      IF ( L .GT. 2 ) GOTO 80
      DO 100 I = 1,N
100        V(I) = W(I)
      DO 180 K = 2,N
           DO 110 I = 1,N
110             W(I) = 0.
           W(K) = 1.
           IF ( K .EQ. N ) GOTO 140
C     -----------------------------
C     |*** FORWARD ELIMINATION ***|
C     -----------------------------
           I = (K*(N+N-K-1))/2
           L = K
120        T = W(L)
           L = L + 1
           DO 130 J = L,N
130             W(J) = W(J) - T*V(I+J)
           I = I + N - L
           IF ( L .LT. N ) GOTO 120
140        CALL PSOLVE(W,V(O),W)
C     ---------------------------
C     |*** BACK SUBSTITUTION ***|
C     ---------------------------
           L = N
           J = M
           P = O - 1 - N
150        T = W(J)
           DO 160 I = L,N
160             T = T - W(I)*V(I+P)
           L = J
           W(L) = T
           J = J - 1
           P = P - N + L
           IF ( L .GT. K ) GOTO 150
           J = ((K-1)*(N+N-K))/2
           DO 170 I = K,N
170             V(I+J) = W(I)
180   CONTINUE
C     ------------------------
C     |*** PERFORM PIVOTS ***|
C     ------------------------
      K = N
190   K = K - 1
      IF ( K .LE. 1 ) RETURN
      L = W(M+K)
      IF ( K .EQ. L ) GOTO 190
      H = L
      O = N
      I = K
      J = 1 + ((K-1)*(N+N-K+2))/2
200   T = V(I)
      V(I) = V(L)
      V(L) = T
      O = O - 1
      I = I + O
      L = L + O
      IF ( I .LT. J ) GOTO 200
      P = J
      O = K - J - N - 1
      I = L
210   J = J + 1
      I = I - J - O
      IF ( J .EQ. L ) GOTO 220
      T = V(J)
      V(J) = V(I)
      V(I) = T
      GOTO 210
220   T = V(P)
      V(P) = V(I)
      V(I) = T
      IF ( H .EQ. N ) GOTO 190
      J = I + N - H
230   I = I + 1
      L = L + 1
      T = V(I)
      V(I) = V(L)
      V(L) = T
      IF ( I .LT. J ) GOTO 230
      GOTO 190
      END
