      SUBROUTINE FMTDEL (IVAC,IREC)
      IMPLICIT NONE
C----------
C  **FMTDEL  FIRE--DATE OF LAST REVISION:  06/21/00
C----------
C
C     THIS SUBROUTINE IS USED TO DELETE TREE RECORDS BY MOVING
C     RECORD IREC TO POSITION IVAC.
C
C     THE FIRE/SNAG MODEL.
C
C  CALLED BY :
C     TREDEL
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
COMMONS
C
      INTEGER IREC,IVAC,JJ

      IF (.NOT. LFMON) RETURN

C     fmprob and fmicr MAY not need to be compressed, tre-del'ed, etc???

      FMPROB(IVAC) = FMPROB(IREC)       
      FMICR(IVAC)  = FMICR(IREC)

      OLDHT(IVAC)  = OLDHT(IREC)
      OLDCRL(IVAC) = OLDCRL(IREC)
      GROW(IVAC)   = GROW(IREC)

      DO JJ = 0, 5
         OLDCRW(IVAC, JJ) = OLDCRW(IREC, JJ)
         CROWNW(IVAC, JJ) = CROWNW(IREC, JJ)
      ENDDO

      RETURN
      END



