      SUBROUTINE FMTRET (IYR)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id: fmtret.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
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
C        IF (JYR .EQ. IYR) THEN
          AFFECT = PRMS(2) / 100.0
          ATREAT = PRMS(3) / 100.0
          FULCON = PRMS(4) / 100.0
          TRMORT = MIN(MAX(0.,PRMS(5) / 100.0),1.)
          LFLBRN = .TRUE.
          CALL OPDONE(JDO,IYR)
C        ENDIF
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
C     piled.  For litter and duff, only the material under the piles gets burned,
C     so we multiply by ATREAT instead.

      DO K = 1,2
        DO ISPD = 1,4
          DO ISZ = 1,9
            PILE = CWD(1,ISZ,K,ISPD) * AFFECT * FULCON
            CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) - PILE
            CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) + PILE
          ENDDO
          DO ISZ = 10,11
            PILE = CWD(1,ISZ,K,ISPD) * AFFECT * ATREAT
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

C         Include call to fmscro to make sure crown of trees killed by pileburns
C         aren't getting missed.  SAR May 2014

          CALL FMSCRO(I,ISP(I),IYR,TRKIL,1)

        ENDDO

C        Store the fire-caused mortality in the snag pools. R&C 07/11/96

        CALL FMSADD (IYR,1)

      ENDIF
C
      RETURN
      END
      
***********************************************************************
***********************************************************************
      SUBROUTINE FMFMOV(IYR)

C     CALLED FROM: FMMAIN
C     This routine has been extracted from FMCWD.
C     If desired, it could be made into a seperate file/routine.
C     Comments below will help in that process.

*  Purpose:
*     This subroutine implements the FUELMOVE keyword
***********************************************************************
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'

C.... Variable declarations.
C         These are declared above
      LOGICAL DEBUG, LALTER
      INTEGER I, K, L
      INTEGER IYR
      INTEGER J1, J2
      INTEGER MYACT(1), NTODO, JYR, IACTK, NPRM, IFROM, ITO
      REAL    XGET
      REAL    FTRG(0:MXFLCL), FSRC(0:MXFLCL), FORG(0:MXFLCL), PRMS(6)
      REAL    X, Y, Z, Q

C     OPTION PROCESSOR CODES FOR
C     FUELMOVE (2530) - TRANSFER FUEL AMONG CATEGORIES

      DATA     MYACT/2530/

C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMFMOV',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,107) 'FMFMOV',ICYC
  107 FORMAT(' ENTERING ',A,' CYCLE = ',I2)
      
C     SEE IF FUELMOVE KEYWORD IS SCHEDULED; TRANSFER AMONG
C     FUEL POOLS AS REQUIRED

      TONRMC = 0.0
      CALL OPFIND(1,MYACT,NTODO)
      IF (NTODO.EQ.0) GOTO 506

C     FORG - ORIGINAL VALUE OF SOURCE FUEL POOL - UNALTERED
C     FTRG - FUEL TO BE PUT IN TARGET POOL - VARIABLE
C     FSRC - FUEL TO BE TAKEN FROM SOURCE POOL - VARIABLE

      DO I = 0,MXFLCL
        FORG(I) = 0.0
        FTRG(I) = 0.0
      ENDDO

      DO I = 1,2
        DO K = 1,2
          DO L = 1,4
            DO J1 = 1,MXFLCL
              FORG(J1) = FORG(J1) + CWD(I,J1,K,L)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO I = 0,MXFLCL
        FSRC(I) = FORG(I)
      ENDDO

      LALTER = .FALSE.
      DO I = 1,NTODO
        CALL OPGET(I,6,JYR,IACTK,NPRM,PRMS)
CSB        IF ((IACTK .GE. 0) .AND. (JYR .EQ. IYR)) THEN
        IF (IACTK .GE. 0) THEN

C         IFROM - SOURCE CATEGORY (0,1-11)
C         ITO   - TARGET CATEGORY (0,1-11)
C         X     - AMOUNT TO TAKE FROM SOURCE (T/AC)
C         Y     - PROPORTION TO TAKE FROM SOURCE (0-1 PROPORTION)
C         Z     - AMOUNT TO LEAVE IN SOURCE (T/AC)
C         Q     - AMOUNT TO END WITH IN TARGET (T/AC)

          IFROM = INT(PRMS(1))
          ITO = INT(PRMS(2))
          X = PRMS(3)
          Y = PRMS(4)
          Z = PRMS(5)
          Q = PRMS(6)

C         MIMIC TESTS OF FUELMOVE KEYWORD IN **FMIN**

          IF ((IFROM .GE. 0) .AND. (IFROM .LE. MXFLCL) .AND.
     &        (ITO   .GE. 0) .AND. (ITO   .LE. MXFLCL) .AND.
     &        (IFROM .NE. ITO) .AND.
     &        (X .GE. 0.0) .AND.
     &        (Y .GE. 0.0) .AND. (Y .LE. 1.0) .AND.
     &        (Z .GE. 0.0)) THEN

C           CHOOSE THE GREATER OF THE 3 REMOVALS (IGNORE IF IMPORTING: IFROM=0)
C           CONSTRAIN XGET TO TAKE ONLY FUEL THAT IS PRESENT.

            XGET = 0.0
            IF (IFROM .GT. 0) THEN

              IF ((FSRC(IFROM) .LE. 0.0)) THEN
                CALL OPDEL1(I)
                GOTO 550
              ENDIF

C             CHOOSE MAXIMUM OF THESE 3, CONVERTING TO T/A:

C               X             - AMOUNT TO TAKE FROM SOURCE (T/AC)
C               Y*FSCR(IFROM) - PROPORTION TO TAKE FROM SOURCE (0-1 PROPORTION)
C               FSCR(IFROM)-Z - RESIDUAL AMOUNT TO LEAVE IN SOURCE (T/AC)
C               Q-FSRC(ITO)   - FINAL AMOUNT TO HAVE IN TARGET (T/AC)

C             NOTE THAT THE AMOUNT IS BASED ON THE CURRENT AMOUNT (DECLINING
C             BALANCE) AND NOT ON THE AMOUNT PRESENT AT THE BEGINNING OF THE
C             KEYWORD PROCESSING FOR THIS YEAR. 'Q' IS NOT IN USE IF IT IS LESS
C             THAN ZERO.

              IF (Q .GE. 0.0) THEN
                XGET = MAX(X, Y*FSRC(IFROM), FSRC(IFROM)-Z, Q-FSRC(ITO))
              ELSE
                XGET = MAX(X, Y*FSRC(IFROM), FSRC(IFROM)-Z)
              ENDIF

              IF (XGET .GT. FSRC(IFROM)) XGET = FSRC(IFROM)
              PRMS(3) = XGET
              PRMS(4) = XGET/(FSRC(IFROM))
              PRMS(5) = FSRC(IFROM) - XGET
              PRMS(6) = FSRC(ITO) + XGET
              FSRC(IFROM) = FSRC(IFROM) - XGET
              IF (ITO .EQ. 0) TONRMC  =  TONRMC + XGET
            ELSE
              IF (Q .GE. 0.0) THEN
                XGET = MAX(X,Q-FSRC(ITO))
              ELSE
                XGET = X
              ENDIF

              PRMS(3) =  XGET
              PRMS(4) =  0.0
              PRMS(5) =  0.0
              PRMS(6) =  FTRG(ITO) + XGET
              TONRMC  = TONRMC - XGET
            ENDIF
            FTRG(ITO) = FTRG(ITO) + XGET

C           RECORD ACTIVITIES OR CANCEL THOSE THAT MOVE NOTHING

            IF (XGET .GT. 0.0) THEN
              CALL OPCHPR(I,6,PRMS)
              CALL OPDONE(I,IYR)
              LALTER = .TRUE.
            ELSE
              CALL OPDEL1(I)
            ENDIF
          ELSE
            CALL OPDEL1(I)
          ENDIF
  550     CONTINUE
        ENDIF
      ENDDO

C     ALTER ORIGINAL FUELS IF ANY OPTIONS WERE PROCESSED
C     STORE NEW POOL VALUES IN IN FTRG.
C     - SKIP ASSIGNMENT TO SUBPOOLS IF THERE HAS BEEN NO CHANGE
C     - IN THE CASE WHERE FUEL IS ADDED TO A PREVIOUSLY EMPTY CATEGORY
C       ADD ALL THE FUEL TO UNPILED(I=1), HARD(K=2), FAST(L=3);
C      -OTHERWISE ADD IN PROPORTION TO THE EXISTING FUEL, BY CREATING
C       A SCALAR 'X' TO MODIFY THE EXISTING FUEL.

      IF (LALTER) THEN
        DO I = 0,MXFLCL
          FTRG(I) = FSRC(I) + FTRG(I)
        ENDDO
        DO J1 = 1,MXFLCL
          IF (ABS(FORG(J1) - FTRG(J1)) .GE. 1.0E-6) THEN
            IF (FORG(J1) .LE. 1.0E-6) THEN
              CWD(1,J1,2,3) = FTRG(J1)
            ELSE
              X = FTRG(J1)/FORG(J1)
              DO K = 1,2
                DO L = 1,4
                  DO I = 1,2
                    CWD(I,J1,K,L) = CWD(I,J1,K,L) * X
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF

C     CALCULATE LARGE AND SMALL FUEL; PILED AND UNPILED.

  506 OLARGE = LARGE
      OSMALL = SMALL

      LARGE  = 0.0
      SMALL  = 0.0

      DO I = 1,2
        DO K = 1,2
          DO L = 1,4
            DO J1 = 1,3
              SMALL = SMALL + CWD(I,J1,K,L)
            ENDDO
            SMALL = SMALL   + CWD(I, 10,K,L) ! LITTER IS SMALL
            DO J2 = 4,9
              LARGE = LARGE + CWD(I,J2,K,L)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

C     COMPUTE PERCENT CHANGE IN FUELS; TRIGGERS
C     ACTIVITY FUELS LOGIC IN **FMCFMD**

      X = OLARGE + OSMALL
      IF (X .GT. 1.0E-6) THEN
        SLCHNG = 100.0 * (LARGE + SMALL - X) / X
      ELSE
        SLCHNG = 0.0
      ENDIF

      RETURN      
      END
