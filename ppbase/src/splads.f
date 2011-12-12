      SUBROUTINE SPLADS (I,J,DIST,IRC)
      IMPLICIT NONE
C----------
C  **SPLADS  DATE OF LAST REVISION:  07/31/08
C----------
C     RETURN THE DISTANCE BETWEEN STAND I AND J.
C
C     I   = STAND SUBSCRIPT I
C     J   = STAND SUBSCRIPT J
C     DIST= DISTANCE BETWEEN I AND J.
C     IRC = RETURN CODE, 0=OK, 1=NO DATA FOR ONE OR MORE OF THESE
C           STANDS.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PPEPRM.F77'
C
C
      INCLUDE 'PPSPLA.F77'
C
C
COMMONS
C
      INTEGER IRC,J,I
      REAL DIST,X,Y
C
      IF (I.LE.NLAS) THEN
         IF (LAORD(I).NE.0) THEN
            IF (XLOC(LAORD(I)).NE.-99999.0) THEN
               X=XLOC(LAORD(I))
               Y=YLOC(LAORD(I))
               IF (LAORD(J).NE.0) THEN
                  IF (XLOC(LAORD(J)).NE.-99999.0) THEN
                     X=X-XLOC(LAORD(J))
                     Y=Y-YLOC(LAORD(J))
                     DIST=SQRT((X*X)+(Y*Y))
                     IRC=0
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      DIST=0.0
      IRC=1
      RETURN
      END
