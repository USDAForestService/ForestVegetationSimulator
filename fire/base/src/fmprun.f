      SUBROUTINE FMPRUN(CTCRWN)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     PART OF THE FIRE MODEL EXTENSION.
C	    THIS ROUTINE ADDS THE MATERIAL THAT WAS PRUNED IN CUTS.
C     CALLED FROM -- CUTS
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
C
C
COMMONS
C
      DIMENSION CTCRWN(MAXTRE)
      LOGICAL   DEBUG
      INTEGER   I,IDC,ISZ
      REAL      CTCRWN,X

      CALL DBCHK (DEBUG,'FMPRUN',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
  7   FORMAT(' ENTERING FMPRUN CYCLE = ',I2,' LFMON=',L2)

      IF (.NOT.LFMON) RETURN

      DO I=1,ITRN

         IDC = DKRCLS(ISP(I))
         X = CTCRWN(I)*P2T
         CWD(1,10,2,IDC) = CWD(1,10,2,IDC) + (CROWNW(I,0) * X)
         DO ISZ = 1,5
            CWD(1,ISZ,2,IDC) = CWD(1,ISZ,2,IDC) + (CROWNW(I,ISZ) * X)
         ENDDO

         IF (DEBUG) WRITE (JOSTND,10) I,CTCRWN(I),DBH(I),ISP(I),IDC,X,
     >                                (CROWNW(I,ISZ),ISZ=0,5)
 10      FORMAT (' I=',I4,' CTCRWN=',F5.3,' DBH=',F7.2,' ISP=',I3,
     >           ' IDC=',I2,' X=',E10.4,' CROWNW=',6F9.2)

      ENDDO

C     Update the crown weight material arrays for post-prune values.

      CALL FMCROW

      RETURN
      END
