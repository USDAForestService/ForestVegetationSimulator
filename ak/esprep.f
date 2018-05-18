      SUBROUTINE ESPREP (PNONE,PMECH,PBURN)
      IMPLICIT NONE
C----------
C  **ESPREP--AK     DATE OF LAST REVISION:   02/14/08
C----------
C     PREDICT DEFAULT SITE PREP PROBABILITIES.
C
C     PROB(NO SITE PREP)
C----------
      REAL PBURN,PMECH,PNONE
      PNONE = 1.0
C----------
C     PROB(MECH SITE PREP)
C----------
      PMECH = 0.0
C----------
C     PROB(BURN SITE PREP)
C----------
      PBURN = 0.0
C
      RETURN
      END
