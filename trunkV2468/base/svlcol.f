      SUBROUTINE SVLCOL (X,Y,R,X1,Y1,X2,Y2,KODE)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C
C     RETURN KODE = 0 IF THE CIRCLE AND LINE DO NOT OVERLAP
C     RETURN KODE = 1 IF THEY DO.
C
      INTEGER KODE
      REAL Y1,Y2,X1,X2,R,X,Y,B1,A1,B,A,XS,YS
C
C     COMPUTE THE FORMULA FOR THE LINE.
C
      CALL SVDFLN (X1,Y1,X2,Y2,A1,B1,KODE)
C
C     CASE 1: KODE = 1 IF THE SLOPE IS INFINITE.
C
      IF (KODE.EQ.1) THEN
C
C        THE CIRCLE AND LINE INTERSECT IF X1 IS LESS THAN R FROM
C        THE LINE.
C
         IF (ABS(X1-X) .LT. R) THEN
            KODE = 1
         ELSE
            KODE = 0
         ENDIF
C
C     CASE 2: IF THE SLOPE IS ZERO.
C
      ELSEIF (ABS(B1) .LE. .000001) THEN

         IF (ABS(Y1-Y) .LT. R) THEN
            KODE = 1
         ELSE
            KODE = 0
         ENDIF
C
      ELSE

C     CASE 3: SLOPE IS NON ZERO AND NOT INFINITE.
C
C        DEFINE A PERPENDICULAR LINE THAT IS THROUGH THE CENTER OF THE
C        CIRCLE.

         B = -1.0/B1
         A = Y-(B*X)

C        FIND THE POINT WHERE THE TWO LINES CROSS.

         XS = (A-A1)/(B1-B)
         YS = A+(B*XS)

C        IF XS,YS IS CLOSER THAN R AWAY FROM X,Y THEN THE LINE BISECTS
C        THE CIRCLE.

         IF ( ((X-XS)**2)+((Y-YS)**2) .LT. R*R) THEN
            KODE = 1
         ELSE
            KODE = 0
         ENDIF
      ENDIF
      RETURN     
      END
