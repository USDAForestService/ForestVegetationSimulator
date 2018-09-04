      SUBROUTINE RDTRP (LTRP)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  SUBROUTINE WHICH RETURNS LTRP TO INDICATE WHETHER TRIPLING WILL
C  OCCUR IN THE CONTEXT OF THE ROOT DISEASE MODEL.  IF TRIPLING WILL
C  RESULT IN MORE THAN IRRTRE TREE RECORDS BEING CREATED THEN
C  TRIPLING IS SUPPRESSED.
C
C  CALLED BY :
C     GRINCR  [PROGNOSIS]
C
C  CALLS     :
C     RDATV   (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     LTRP   -
C
C  Revision History:
C   11/06/89 - Last revision date.
C   09/04/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'CONTRL.F77'
C
COMMONS
C

      LOGICAL LTRP, LGO, LTEE
      INTEGER ILIM

      CALL RDATV (LGO,LTEE)
      IF (LGO) THEN
         ILIM = IRRTRE / 3
         LTRP = (ICYC .LE. ICL4 .AND. ITRN .LE. ILIM .AND. .NOT. NOTRIP)
      ENDIF

      RETURN
      END
