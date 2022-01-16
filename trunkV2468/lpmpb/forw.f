      DOUBLE PRECISION FUNCTION FORW  (X,P,Q,X1,X2,N)
      IMPLICIT NONE
C----------
C  **FORW          DATE OF LAST REVISION:  07/02/10
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
      INTEGER I, N, N1
      DOUBLE PRECISION P,Q,X,T1,XI,X1,X2,FOR,PQ1,XA1,TEMP
      IF(N-1)1,2,3
    1 FORW=X1
      RETURN
    2 FORW = X2
      RETURN
    3 N1= N-1
      T1 = X1
      PQ1 = P+Q-1.
      XA1 = 1.-X
      FOR = X2
      DO 4 I = 1,N1
      XI = I
      TEMP = FOR
      FOR = FOR+(FOR-T1)*(PQ1+XI)*XA1/(XI+Q)
    4 T1 = TEMP
      FORW = FOR
      RETURN
      END
