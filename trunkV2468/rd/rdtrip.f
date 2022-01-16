      SUBROUTINE RDTRIP (ITFN,I,WEIGHT)
      IMPLICIT NONE
C----------
C  **RDTRIP      LAST REVISION:  03/01/16
C----------
C
C  THIS ROOT DISEASE MODEL SUBROUTINE IS USED
C  TO TRIPLE THE ROOT DISEASE TREE LISTS.
C
C  CALLED BY :
C     TRIPLE  [PROGNOSIS]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     ITFN   -
C     I      -
C     WEIGHT -
C
C  Revision History:
C    22-JUL-02 Lance R. David (FHTET)
C      Previous revision date noted was 11/06/89.
C      Removed unused array PROBO. It was also unused in the old
C      annosus model.
C   09/04/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/2016 Lance R. David (FMSC)
C     Moved one condition to exit to top.
C
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'              
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
C
COMMONS
C
      INTEGER I, IDI, ITFN, ITYP, J, JINF
      REAL    TPAREA, WEIGHT

      IF (IROOT .EQ. 0) RETURN

      TPAREA = 0.0
      DO 59 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
   59 CONTINUE     
      IF (TPAREA .EQ. 0) RETURN

      IF (WEIGHT .EQ. 0.6) ITFN = I

      FPROB(ITFN) = FPROB(I) * WEIGHT
      FFPROB(ITFN,1) = FFPROB(I,1) * WEIGHT
      FFPROB(ITFN,2) = FFPROB(I,2) * WEIGHT
      RROOTT(ITFN) = RROOTT(I)
      WK22(ITFN) = WK22(I) * WEIGHT

      DO 100 J = 1,4
         ROOTH(J,ITFN) = ROOTH(J,I)
         XMTH(J,ITFN) = XMTH(J,I) * WEIGHT
  100 CONTINUE

      ROOTL(ITFN)  = ROOTL(I)
      RRKILL(ITFN) = RRKILL(I) * WEIGHT
      RDKILL(ITFN) = RDKILL(I) * WEIGHT
      PROBIT(ITFN) = PROBIT(I) * WEIGHT
      PROBIU(ITFN) = PROBIU(I) * WEIGHT
      PROBL(ITFN)  = PROBL(I) * WEIGHT

C     WRITE (JOSTND,200) FPROB(ITFN),FPROB(I),PROBIU(ITFN),PROBIU(I)
C 200 FORMAT (' IN RDTRIP :  FPROB(ITFN),FPROB(I),PROBIU',4F8.2)

C     OAKL ALSO NEEDS TO BE TRIPLED SINCE IT IS USED (AS PROAKL) NEXT TIME

      DO 250 ITYP = 1,3
         OAKL(ITYP,ITFN) = OAKL(ITYP,I) * WEIGHT
  250 CONTINUE

      DO 300 JINF = 1,ISTEP
         PROBI(ITFN,JINF,1) = PROBI(I,JINF,1) * WEIGHT
         PROPI(ITFN,JINF,1) = PROPI(I,JINF,1)
         PROBI(ITFN,JINF,2) = PROBI(I,JINF,2) * WEIGHT
         PROPI(ITFN,JINF,2) = PROPI(I,JINF,2)
  300 CONTINUE

      RETURN
      END
