      SUBROUTINE SPLAAR (I,A,IRC)
      IMPLICIT NONE
C----------
C  **SPLAAR  DATE OF LAST REVISION:  07/31/08
C----------
C     RETURN THE AREA OF THE I TH STAND.
C
C     I   = STAND SUBSCRIPT
C     A   = THE STAND AREA
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
      REAL A
C
      IF (I.LE.MXSTND) THEN
         IF (LAORD(I).NE.0) THEN
            IF (AREA(LAORD(I)).NE.-99999.0) THEN
               A=AREA(LAORD(I))
               IRC=0
               RETURN
            ENDIF
         ENDIF
      ENDIF
      A=0.0
      IRC=1
      RETURN
      END
