      SUBROUTINE FMTRET (IYR)
      IMPLICIT NONE
C----------
C  **FMTRET FIRE--DATE OF LAST REVISION:  10/13/09
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN
C     CALLS:   FMCONS
C              FMMOIS
C              FMSSEE
C              FMSADD
*----------------------------------------------------------------------
*  Purpose:
*     This routine activates the fuel treatment options (jackpot burns
*     and pile burns).  CWD is moved into the piled category, and then
*     FMCONS is called to calculate the fuel consumption. Any remain
*     fuel is considered unpiled, and is moved back to the unpiled category.
*     Some tree mortality may occur, if the user requests it.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     AFFECT:  % area in the stand that is affected by the treatment.
*     ATREAT:  % of affected area in which the fuel is piled and burned.
*     FULCON:  % of fuel from the affected area that is in the burned area
*     TRMORT:  % trees (in stand) which are killed by the treatment.
*
*  Common block variables and parameters:
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'


C.... Variable declarations.
      INTEGER   FMOIS
      REAL      AFFECT, ATREAT, FULCON, TRMORT, PILE
      REAL      PRMS(5)
      INTEGER   MYACTS(1)
      LOGICAL   DEBUG
      DATA      MYACTS /2523/
      INTEGER   IYR,JTODO,JDO,NPRM,IACTK,JYR,K,ISPD,ISZ,I
      REAL      PSMOKE,TRKIL
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMTRET',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMTRET CYCLE=',I3)
C
C.... Begin routine.
C
C     First, check to see if fuel treatment (FUELBURN KEYWORD) is to
C     be done this year.
C
      LFLBRN = .FALSE.
      CALL OPFIND(1,MYACTS,JTODO)
      IF (JTODO .LE. 0) RETURN
      DO JDO = 1,JTODO
        CALL OPGET(JDO,5,JYR,IACTK,NPRM,PRMS)
CSB        IF (JYR .EQ. IYR) THEN
          AFFECT = PRMS(2) / 100.0
          ATREAT = PRMS(3) / 100.0
          FULCON = PRMS(4) / 100.0
          TRMORT = MIN(MAX(0.,PRMS(5) / 100.0),1.)
          LFLBRN = .TRUE.
          CALL OPDONE(JDO,IYR)
CSB        ENDIF
      ENDDO

      IF (.NOT. LFLBRN) RETURN

      IF (DEBUG) WRITE(JOSTND,8) AFFECT,ATREAT,FULCON,TRMORT,LFLBRN
    8 FORMAT(' IN FMTRET AFFECT=',F10.3,' ATREAT=',F10.3,
     >       ' FULCON=',F10.3,' TRMORT=',F10.3,' LFLBRN=',L2)

*     CWD:     Array for Coarse Woody Debris. Indexed 4 ways:
*              (1:3,  -,  -,  -) 1 = Unpiled, 2 = Piled, 3 = summation
*              (  -,1:11,  -,  -) 1 = <.25 in,...,duff (standard breakpoints)
*              (  -,  -,1:2,  -) 1 = SOFT, 2 = HARD
*              (  -,  -,  -,1:5) 1 = V.Slow,..., 4 = Fast decay rate

C     The amount of fuel burned is the amount that is in the affected area
C     (CWD*AFFECT) times the amount from the affected area that is in the
C     treated area (*FULCON).  Transfer this amount from the unpiled to the
C     piled

      DO K = 1,2
        DO ISPD = 1,4
          DO ISZ = 1,9
            PILE = CWD(1,ISZ,K,ISPD) * AFFECT * FULCON
            CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) - PILE
            CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) + PILE
          ENDDO
          DO ISZ = 10,11
            PILE = CWD(1,ISZ,K,ISPD) * AFFECT
            CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) - PILE
            CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) + PILE
          ENDDO
        ENDDO
      ENDDO

C     Call the fuel consumption routine (FMCONS) to burn the piled stuff.
c        **(WE STILL NEED TO ASSIGN SOME MOISTURE LEVELS SO CAN CALCULATE
c           SMOKE PRODUCTION PROPERLY)**
C           (for now, assume that it is a medium moisture level)

      FMOIS = 3
      CALL FMMOIS(FMOIS, MOIS)
      CALL FMCONS(FMOIS, 1, AFFECT*ATREAT, IYR, 0, PSMOKE,100.)

C     Now, transfer any remaining piled stuff back into the unpiled pool.
      DO K = 1,2
        DO ISPD = 1,4
          DO ISZ = 1,MXFLCL
            PILE = CWD(2,ISZ,K,ISPD)
            CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) - PILE
            CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) + PILE
          ENDDO
        ENDDO
      ENDDO

C     Now kill some trees, if requested.
C     Also set PBurnYR, since this will determine sprout ages in fmkill

      PBURNYR = IYR
      IF (TRMORT .GT. 0.0) THEN
        DO I = 1,ITRN
          TRKIL = FMPROB(I) * TRMORT
          IF (TRKIL.GE.FMPROB(I)) THEN     ! watch out for rounding errors
            TRKIL = FMPROB(I)
            FMPROB(I) = 0.
          ELSE
            FMPROB(I) = MAX(0.,FMPROB(I) - TRKIL)
          ENDIF
          CURKIL(I) = CURKIL(I) + TRKIL
          FIRKIL(I) = FIRKIL(I) + TRKIL

C         Store the fire-caused mortality in the snag mgmt routines for
C         addition to the snag pools.  R&C 07/11/96

          CALL FMSSEE (I,ISP(I),DBH(I),HT(I),CURKIL(I),
     >                 1,DEBUG,JOSTND)

        ENDDO

C        Store the fire-caused mortality in the snag pools. R&C 07/11/96

        CALL FMSADD (IYR,1)

      ENDIF
C
      RETURN
      END
