REAL A(10,10), B(10,10), C(10,10)
INTEGER Aw,Ak,Bw,Bk,Cw,Ck,i,j,k
PRINT *,'Wprowadz wymiary macierzy A (w,k)'
READ *,Aw,Ak
PRINT *,'Wprowadz wymiary macierzy B (w,k)'
READ *,Bw,Bk
IF (Ak.NE.bw) THEN
PRINT *,'Nie zgadzaja sie wymiary macierzy'
STOP
ENDIF

Cw=Aw
Ck=Bk

PRINT *,'Wprowadz wiersze macierzy A'
DO i=1,Aw
READ *,(A(i,j),j=1,Ak)
ENDDO

PRINT *,'Wprowadz wiersze macierzy B'
DO i=1,Bw
READ *,(B(i,j),j=1,Bk)
ENDDO

DO i=1,Cw
 DO j=1,Ck
   C(i,j)=0
   DO k=1,Ak
    C(i,j)=C(i,j)+A(i,k)*B(k,j)
   ENDDO
 ENDDO
ENDDO


PRINT *,'Macierz wynikowa  ='
DO i=1,Cw
PRINT *,(C(i,j),j=1,Ck)
ENDDO

END
