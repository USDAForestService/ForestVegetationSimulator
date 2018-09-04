      SUBROUTINE SVGTPT (IPGLEM,X1R1,X2R2,Y1A1,Y2A2,X,Y,IMETRIC)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
C     D. ROBINSON   -- ESSA        -- MAY 2005
C
C     GET A RANDOM POINT IN THE REGION DEFINED BY X1R1,X2R2,Y1A1,Y2A2
C
      INTEGER IPGLEM,IMETRIC
      REAL X1R1,X2R2,Y1A1,Y2A2,X,Y,A,R

      IF (IPGLEM.LT.2) THEN
        CALL SVRANN (X)
        X = X1R1 + ((X2R2-X1R1)*X)
        CALL SVRANN (Y)
        Y = Y1A1 + ((Y2A2-Y1A1)*Y)
      ELSE
        CALL SVRANN (A)
        A = Y1A1 + ((Y2A2-Y1A1)*A)
        CALL SVRANN (R)
        R = SQRT(X1R1*X1R1 + R*(X2R2*X2R2-X1R1*X1R1))

        IF (IMETRIC.EQ.0) THEN
          X = COS(A)*R + 117.7522
          Y = SIN(A)*R + 117.7522
        ELSE
          X = COS(A)*R + 56.42
          Y = SIN(A)*R + 56.42
        ENDIF

      ENDIF
      RETURN
      END
