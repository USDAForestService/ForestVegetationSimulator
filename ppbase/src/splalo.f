      SUBROUTINE SPLALO (I,X,Y,IRC)
      IMPLICIT NONE
C----------
C  **SPNBED  DATE OF LAST REVISION:  07/31/08
C----------
C     RETURN THE LOCATION OF THE I TH STAND.
C
C     I   = STAND SUBSCRIPT
C     X   = X LOCATION
C     Y   = Y LOCATION
C     IRC = RETURN CODE, 0=OK, 1=NO DATA FOR THIS STAND.
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
      INTEGER IRC,I
      REAL X,Y
C
      IF (I.LE.NLAS) THEN
         IF (LAORD(I).NE.0) THEN
            IF (XLOC(LAORD(I)).NE.-99999.0) THEN
               X=XLOC(LAORD(I))
               Y=YLOC(LAORD(I))
               IRC=0
               RETURN
            ENDIF
         ENDIF
      ENDIF
      X=0
      Y=0
      IRC=1
      RETURN
      END
