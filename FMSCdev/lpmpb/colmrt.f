      SUBROUTINE COLMRT
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     MORTALITY ROUTINE FOR COLES MPB MODEL.
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS.
C     FORESTRY SCIENCES LAB -- MOSCOW, IDAHO
C
C Revision History
C   05/31/00 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
      INCLUDE 'COLCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
      INTEGER I, I1, I2, J, INDEX, NUMYRS
      REAL    PRKILL(10), CFTVOL(10), SUMDED, XT

      SUMDED = 0.0
      NUMYRS = MPMXYR
      IF (NUMYRS .GT. IFINT) NUMYRS = IFINT

      DO 100 I=1,10
         IF (START(I) .LE. 0.0) THEN
            PRKILL(I) = 0.0
         ELSE
            PRKILL(I) = (START(I) - GREEN(NUMYRS,I)) / START(I)
         ENDIF

         SUMDED = SUMDED + PRKILL(I)
         CFTVOL(I) = 0.0
  100 CONTINUE

C
C     APPLY MOUNTAIN PINE BEETLE MORTALITY.
C
C
C     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
C     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
C
      I1 = ISCT(IDXLP,1)
C      I1 = ISCT(7,1)
      IF (I1 .EQ. 0) GOTO 300
C
C     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
C     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
C
      I2 = ISCT(IDXLP,2)
C      I2 = ISCT(7,2)

      DO 200 I=I1,I2
         J = IND1(I)
C
C        LOAD THE WK2 ARRAY WITH THE NUMBER OF TREES PER ACRE
C        KILLED BY MOUNTAIN PINE BEETLE FOR EACH TREE RECORD.
C
         CALL COLIND (DBH(J),INDEX)
         XT = PRKILL(INDEX) * PROB(J)
         CFTVOL(INDEX) = CFTVOL(INDEX) + XT * CFV(J)
         IF (XT .GT. WK2(J)) WK2(J) = XT
         IF (PROB(J) - WK2(J) .LT. 1E-6) WK2(J) = PROB(J) - 1E-6
  200 CONTINUE

      CALL MPBTAB(CFTVOL)

  300 CONTINUE
      RETURN
      END
