      SUBROUTINE RDSTR (IT,PREM,PREPRB)
      IMPLICIT NONE
C----------
C RD $Id: rdstr.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  SUBROUTINE FOR DECREASING INFESTED TREES WHEN CUTTING OR
C  THINNING
C
C  CALLED BY :
C     CUTS    [PROGNOSIS]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDSADD  (SUBROUTINE)   [ROOT DISEASE]
C     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C     RDSPUP  (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     IT     -
C     PREM   -
C     PREPRB -
C
C  REVISION HISTORY:
C    01-JUL-2002 Lance R. David (FHTET)
C      Previous date might have been June 1998.
C      Root radius for live trees was only being set when spore infection
C      was active. This value is needed elsewhere regardless of spore
C      infection processes.
C    16-JUL-2002 Lance R. David (FHTET)
C      Modified comment and debug statements.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/2016 Lance R. David (FMSC)
C     Moved check to exit if not active, no trees or no disease area
C     to top.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDCOM.F77'

      INCLUDE 'RDARRY.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'RDADD.F77'
C
COMMONS
C

      LOGICAL DEBUG
      INTEGER I, IDI, IRG, ISL, IT, JJ
      REAL    ANS, PREM, PREPRB, TESTAG, TP, TPAREA, XXX

C
C     SEE IF WE NEED TO DO SOME DEBUG.
C
      CALL DBCHK (DEBUG,'RDSTR',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,100) ICYC
  100 FORMAT (' ENTER RDSTR: CYCLE=',I5)
  
      IF (IROOT .EQ. 0 .OR. PREPRB .EQ. 0.0) GOTO 1100

      TPAREA = 0.0
      DO 101 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
  101 CONTINUE       

      TP = (1.0 - (PREM / PREPRB))
C
C     IF THE ROOT ARRAYS HAVE NOT YET BEEN CREATED (BECAUSE THERE IS 
C     NO INFECTION IN THE STAND OR BECAUSE A CUT OCCURS IN THE FIRST 
C     TIMESTEP) THEN DO SO. 
C
      JJ = ISP(IT) 
      IDI = MAXRR
      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(JJ))

C--------------
C     This block of code is replaced below. Why array that is suppose
C     to hold root radius for all each tree records is only calculated
C     when spore infection is active does not make sense.
C     lrd 01-jul-02
C      IF (ROOTL(IT) .GT. 0.0 .OR. SPINF(IDI) .LE. 0.0) GOTO 300
C
C          CALL RDROOT(JJ,DBH(IT),ANS,PROOT(IRTSPC(JJ)),
C     &                RSLOP(IRTSPC(JJ)),HT(IT))                           
C          ROOTL(IT) = ANS
C
C  300 CONTINUE
C---------------

      IF (ROOTL(IT) .EQ. 0.0) THEN
          CALL RDROOT(JJ,DBH(IT),ANS,PROOT(IRTSPC(JJ)),
     &                RSLOP(IRTSPC(JJ)),HT(IT))                           
          ROOTL(IT) = ANS
      ENDIF
C
C     WHY EQUAL WEIGHTS FOR OUTSIDE AND INSIDE ON A PER ACRE BASIS ?
C
      XXX = TP * PREPRB
      RROOTT(IT) = (RROOTT(IT) * WK22(IT) +
     &             ROOTL(IT) * XXX) / (XXX + WK22(IT) + 0.0001)

C
C     CALL THE SPORE MODEL.
C
C      IF (SPINF(IDI) .GT. 0.0) THEN
C
C     Only call spore model if both IDI > 0, and SPINF(IDI) > 0
C     (RNH June98)
C
      IF ((IDI .GT. 0) .AND. (SPINF(IDI) .GT. 0.0)) THEN
C
          CALL RDSSIZ(JJ,DBH(IT),STCUT,ISL,ISPS,IRTSPC)
          CALL RDSPUP(IT,ISL,JJ,TP,DBH(IT),ROOTL(IT))
      ENDIF
C
C     REDUCE TREE DENSITY VARIABLES
C
      PROBIU(IT) = PROBIU(IT) * TP
      FPROB(IT) = FPROB(IT) * TP
      WK22(IT) = WK22(IT) + XXX

      DO 500 I = 1,ISTEP
         PROBI(IT,I,1) = PROBI(IT,I,1) * TP
         PROBI(IT,I,2) = PROBI(IT,I,2) * TP
  500 CONTINUE

C     ADD THE ALREADY INFECTED STUMPS TO THE STUMP LIST
      CALL RDSADD(IT,TP)

      CALL RDSUM (IT,PROBIT,PROBI,ISTEP)
      TESTAG = FLOAT (IY(ICYC))

      IF (DEBUG) WRITE (JOSTND,1010) TESTAG, AGECUR, INFLAG
 1010 FORMAT(' IN RDSTR: TESTAG AGECUR INFLAG=',2F6.0,I5)

      IF ((TESTAG .EQ. AGECUR) .AND. (INFLAG .NE. 0)) INFLAG = 3
      IF (INFLAG .NE. 0) GOTO 1100

C
C     SET THE CUTTING FLAG ON FOR ROOT DISEASE MODEL
C
      INFLAG = 1
      IF (DEBUG) WRITE (JOSTND,1020)
 1020 FORMAT (' IN RDSTR: SET INFLAG=1')

C
C     SET UP AGE FOR THE CARRYOVER MODEL CALL TIMING
C
      IRG = ICYC + IRGEN(1)
      IF (IRG .LE. NCYC) AGECUR = IY(IRG)
      IF (DEBUG) WRITE (JOSTND,1030) AGECUR
 1030 FORMAT(' IN RDSTR: AGECUR=',F6.0)

 1100 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDSTR'
      RETURN
      END
