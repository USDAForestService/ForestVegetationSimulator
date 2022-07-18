      SUBROUTINE SVCROL (X1,Y1,R1,X2,Y2,R2,KODE)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C
C     RETURN KODE = 0 IF TWO CIRCLES WITH CENTER X,Y AND RADUS
C                   R, DO NOT OVERLAP, 1 IF THEY DO.
C
      REAL X1,Y1,R1,X2,Y2,R2,D2
      INTEGER KODE
      D2 = ((X2-X1)**2)+((Y2-Y1)**2)
      R2 = R1+R2
      R2 = R2*R2
      IF (R2 .GT. D2) THEN
         KODE = 1
      ELSE
         KODE = 0
      ENDIF
      RETURN
      END
