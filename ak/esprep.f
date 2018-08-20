      SUBROUTINE ESPREP (PNONE,PMECH,PBURN)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICT DEFAULT SITE PREP PROBABILITIES.
C
C     PROB(NO SITE PREP)
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      REAL PBURN,PMECH,PNONE
C
C----------
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
