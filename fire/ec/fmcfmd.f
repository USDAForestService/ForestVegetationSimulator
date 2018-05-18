      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C   **FMCFMD FIRE-EC-DATE OF LAST REVISION: 03/12/12
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
*     FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE BY THE DYNAMIC
*     FUEL MODEL
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
*     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
*
***********************************************************************
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'SSTGMC.F77'
C
COMMONS
C----------
C  LOCAL VARIABLE DECLARATIONS
C
C  NUMBER OF HABITAT TYPES IN **HABTYP*
C----------
      INTEGER    NPA
      PARAMETER (NPA=155)
C----------
C  NUMBER OF POTENTIAL FIRE MODELS
C----------
      INTEGER   ICLSS
      PARAMETER(ICLSS = 13)
C----------
C  COVER METAGROUP ENUMS
C----------
      INTEGER*2 DFCT, LPCT, SAFCT, MHCT, WPCT, ESCT, PSFCT, WLCT, PPCT,
     &          DFGFCT, PPDFCT, LPWLCT, SAFMCT, MMIXCT, DMIXCT, UBCT

      PARAMETER(
C----------
C  PURE (1 SPECIES > 50% BA)
C----------
     &  DFCT   =  1, ! Douglas-fir
     &  LPCT   =  2, ! lodgepole pine and whitebark pine
     &  SAFCT  =  3, ! subalpine fir, noble fir, and white fir
     &  MHCT   =  4, ! mountain hemlock and western hemlock
     &  WPCT   =  5, ! white pine
     &  ESCT   =  6, ! Engelmann spruce
     &  PSFCT  =  7, ! pacific silver fir
     &  WLCT   =  8, ! western larch and subalpine larch
     &  PPCT   =  9, ! ponderosa pine
C----------
C  MIXTURES (2 SPECIES > 50% BA)
C----------
     &  DFGFCT = 10, ! Douglas-fir/grand fir
     &  PPDFCT = 11, ! ponderosa pine/Douglas-fir
     &  LPWLCT = 12, ! lodgepole pine/whitebark pine/
C                      western larch/subalpine larch
C----------
C  DOG'S BREAKFAST
C----------
     &  SAFMCT = 13, ! SAF leading but not > 50%
     &  MMIXCT = 14, ! moist habitat mixed conifer
     &  DMIXCT = 15, ! dry habitat mixed conifer
     &  UBCT   = 15) ! UPPER BOUND
C
      INTEGER   FMD
      INTEGER   IYR,I,J,ICT, IFMST
      INTEGER   IPTR(ICLSS), ITYP(ICLSS)
      REAL      XPTS(ICLSS,2), EQWT(ICLSS), WT1(4), WT2(4)
      REAL      CTBA(UBCT), STNDBA, X(2), Y(2), X1, Y1, QMD, AFWT
      REAL      ALGSLP, X3
      LOGICAL   DEBUG,  LSAFD, LSNGL
C----------
C  FIXED VALUES FOR INTERPOLATION FUNCTION
C----------
      DATA     Y / 0.0, 1.0 /
C----------
C  These are the integer tags associated with each fire model
C  class. They are returned with the weight
C----------
      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13 /
C----------
C  These are 0 for regular lines, -1 for horizontal and 1 for
C  vertical lines. If any of the lines defined by xpts() are of
C  an unusual variety, this must be entered here so that
C  special logic can be invoked.  In this case, all the line
C  segments have a |slope| that is > 0 and less than inif.
C----------
      DATA ITYP / ICLSS * 0 /
C----------
C  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C    WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C    COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C    SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).
C----------
      DATA ((XPTS(I,J), J=1,2), I=1,ICLSS) /
     >   5., 15.,   ! FMD   1
     >   5., 15.,   ! FMD   2
     >   5., 15.,   ! FMD   3
     >   5., 15.,   ! FMD   4
     >   5., 15.,   ! FMD   5
     >   5., 15.,   ! FMD   6
     >   5., 15.,   ! FMD   7
     >   5., 15.,   ! FMD   8
     >   5., 15.,   ! FMD   9
     >  15., 30.,   ! FMD  10 ! shares with 11
     >  15., 30.,   ! FMD  11
     >  30., 60.,   ! FMD  12
     >  45.,100./   ! FMD  13
C----------
C  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
C  WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C  MUST SUM TO 1, WRT EACH OTHER
C----------
      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO
C----------
C  BEGIN ROUTINE
C----------
      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)
C
      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)
C----------
C  IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.
C----------
      IF (LUSRFM) RETURN
C
      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C----------
C  COMPUTE QMD
C----------
      X1 = 0.0
      Y1 = 0.0
      DO J = 1,ITRN
        X1 = X1 + FMPROB(I) * DBH(I) * DBH(I)
        Y1 = Y1 + FMPROB(I)
      ENDDO
      QMD = 0.0
      IF (Y1 .GT. 0.0) QMD = SQRT(X1/Y1)
C----------
C  COMPUTE NUMBER OF LAYERS, USING STRUCTURE CLASS
C
C    0 = BG = BARE GROUND
C    1 = SI = STAND INITIATION
C    2 = SE = STEM EXCLUSION
C    3 = UR = UNDERSTORY REINITIATION
C    4 = YM = YOUNG FOREST MULTISTRATA
C    5 = OS = OLD FOREST SINGLE STRATUM
C    6 = OM = OLD FOREST MULTISTRATA
C----------
      CALL FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,IFMST,X3,
     &     FMPROB,FMICR)
C 
      SELECT CASE (IFMST)
        CASE (0,1,2,5)
          LSNGL = .TRUE.
        CASE (3,4,6)
          LSNGL = .FALSE.
        CASE DEFAULT
          LSNGL = .FALSE.
      END SELECT
C----------
C  ZERO OUT THE COVER GROUPS AND COMPUTE TOTAL STAND BASAL AREA
C----------
      DO I = 1,UBCT
        CTBA(I) = 0.
      ENDDO
      STNDBA = 0.
      DO I = 1,MAXSP
        STNDBA = STNDBA + FMTBA(I)
      ENDDO
C
      CTBA(DFCT)      = FMTBA(3)
      CTBA(LPCT)      = FMTBA(7) + FMTBA(14)
      CTBA(SAFCT)     = FMTBA(9) + FMTBA(15) + FMTBA(16)
      CTBA(MHCT)      = FMTBA(12) + FMTBA(11)
      CTBA(WPCT)      = FMTBA(1)
      CTBA(ESCT)      = FMTBA(8)
      CTBA(PSFCT)     = FMTBA(4)
      CTBA(WLCT)      = FMTBA(2) + FMTBA(17)
      CTBA(PPCT)      = FMTBA(10)
      CTBA(DFGFCT)    = FMTBA(3)  + FMTBA(6)
      CTBA(PPDFCT)    = FMTBA(10) + FMTBA(3)
      CTBA(LPWLCT)    = FMTBA(7)  + FMTBA(14) + FMTBA(2) + FMTBA(17)
C----------
C  FIND IF MOISTURE REGIME IS MOIST OR DRY **FMCMBA**
C----------
      CALL ECMOIST(I)
      IF (I .EQ. 1) THEN
        CTBA(MMIXCT) = STNDBA
      ELSE
        CTBA(DMIXCT) = STNDBA
      ENDIF
C----------
C  2 - FIND COVER TYPE METAGROUP *ICT* - (1) LOOK FOR >50% OF
C      BASAL AREA IN A SINGLE-SPECIES GROUP; (2) LOOK FOR
C      TWO-SPECIES GROUPS; (3) LOOK FOR SAF AS THE LARGEST SINGLE
C      COMPONENT; (4) USE MOIST/DRY ASSIGNMENT. IF
C      THERE ARE NO TREES, USE THE METAGROUP OF THE PREVIOUS CYCLE,
C      INITIALIZED TO *LPCT* IN **FMVINIT**
C----------
      ICT = 0
      IF ((ITRN .GT. 0) .AND. (STNDBA.GT.0.001)) THEN

        DO I = DFCT,PPCT      ! (1) - ONE SPP DOMINANT
          IF ((CTBA(I)/STNDBA) .GT. 0.50) THEN
            ICT = I
            GOTO 10
          ENDIF
        ENDDO
C
        DO I = DFGFCT,LPWLCT  ! (2) - TWO SPP DOMINANT
          IF ((CTBA(I)/STNDBA) .GT. 0.50) THEN
            ICT = I
            GOTO 10
          ENDIF
        ENDDO
C
        LSAFD = .TRUE.        ! (3) - SAF LEADING
        DO I = 1, MAXSP
          IF (I .NE. 9 .AND. FMTBA(I) .GT. CTBA(SAFCT)) THEN
            LSAFD = .FALSE.
            GOTO 11
          ENDIF
        ENDDO
   11   IF (LSAFD) THEN
          ICT = SAFMCT
          GOTO 10
        ENDIF
C
        DO I = MMIXCT,DMIXCT  ! (4) - MOIST/DRY
          IF ((CTBA(I)/STNDBA) .GT. 0.50) THEN
            ICT = I
            GOTO 10
          ENDIF
        ENDDO
        GOTO 10
      ELSE
        ICT = OLDICT
      ENDIF
   10 OLDICT = ICT
C----------
C  3 - RESOLVE DYNAMIC FMD
C  RULES ARE BASED ON A TABLE GIVEN BY TOM L. SARAH BEUKEMA AT
C  ESSA DERIVED THIS LOGIC BASED ON THOSE VALUES. TRANSITION CUTPOINTS
C  WERE MODIFIED AS FOLLOWS: %CC [ PERCOV ] CUTPOINTS WERE ADJUSTED SO
C  THAT THEY BEGIN TO CHANGE -10% FROM THE CUTPOINT AND FINISH CHANGING
C  +10% ABOVE THE CUTPOINT. SIZE [ QMD ] CUTPOINTS ARE ADJUSTED USING
C  -1" AND +1" ABOUT THE CUTPOINT. THESE TRANSITIONS INSURE THAT THE
C  MODEL BEHAVIOUR DOES NOT (USUALLY) EXHIBIT EXTREME JUMPS DURING STAND
C  DEVELOPMENT
C----------
      DO I = 1,4
        WT1(I) = 0.0
      ENDDO
C
      SELECT CASE (ICT)
C----------
C  FIR, LODGEPOLE PINE, AND MIXED
C----------
        CASE (DFCT,DFGFCT,LPCT,LPWLCT,MHCT,PSFCT,
     &        SAFCT,WPCT,SAFMCT,MMIXCT)
C
          IF (PERCOV .LT. 30.0) THEN
            X(1) =  10.0
            X(2) =  30.0
            J = 2
          ELSEIF (PERCOV .GE. 30.0 .AND. PERCOV .LT. 60.0) THEN
            X(1) = 40.0
            X(2) = 60.0
            J = 3
          ELSEIF (PERCOV .GE. 60.0) THEN
            X(1) = 70.0
            X(2) = 90.0
            J = 4
          ENDIF
          WT1(J)   = ALGSLP(PERCOV,X,Y,2)
          WT1(J-1) = 1.0 - WT1(J)
C
          IF (WT1(1) .GT. 0.0) THEN
C
            IF (LSNGL) THEN
              IF (ICT .EQ. MMIXCT) THEN
C
                DO I = 1,2
                  WT2(I) = 0.0
                ENDDO
C
                IF (QMD .LE. 9.0) THEN
                  X(1) = 7.0
                  X(2) = 9.0
                  J = 2
                ELSE
                  X(1) = 19.0
                  X(2) = 21.0
                  J = 3
                ENDIF
                WT2(J)   = ALGSLP(QMD,X,Y,2)
                WT2(J-1) = 1.0 - WT2(J)
C
                EQWT(1) = EQWT(1) + WT1(1) * (WT2(1) + WT2(3))
                EQWT(5) = EQWT(5) + WT1(1) * WT2(2)
C
              ELSE
                EQWT(1) = EQWT(1) + WT1(1)
              ENDIF
            ELSE
              EQWT(5)   = EQWT(5) + WT1(1)
            ENDIF
          ENDIF
C
          IF (WT1(2) .GT. 0.0) THEN
C
            IF (ICT .EQ. LPCT   .OR.
     >          ICT .EQ. LPWLCT .OR.
     >          ICT .EQ. WPCT   .OR.
     >          ICT .EQ. DFCT) THEN
C
              X(1) =  2.0
              X(2) =  4.0
              J = 2
              WT2(J)   = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)
C
              IF (ICT .EQ. DFCT) THEN
                IF (LSNGL) THEN
                  EQWT(5) = EQWT(5) + WT1(2)
                ELSE
                  EQWT(8) = EQWT(8) + WT1(2)
                ENDIF
              ELSE
                EQWT(1)   = EQWT(1) + WT1(2) * WT2(1)
                IF (LSNGL) THEN
                  EQWT(5) = EQWT(5) + WT1(2) * WT2(2)
                ELSE
                  EQWT(8) = EQWT(8) + WT1(2) * WT2(2)
                ENDIF
              ENDIF
C
            ELSEIF (ICT .EQ. DFGFCT .OR.
     >              ICT .EQ. MHCT   .OR.
     >              ICT .EQ. PSFCT  .OR.
     >              ICT .EQ. MMIXCT) THEN
C
              IF (LSNGL) THEN
C
                DO I = 1,3
                  WT2(I) = 0.0
                ENDDO
C
                IF (QMD .LE. 4.0) THEN
                  X(1) =  2.0
                  X(2) =  4.0
                  J = 2
                ELSE
                  X(1) = 19.0
                  X(2) = 21.0
                  J = 3
                ENDIF
                WT2(J)   = ALGSLP(QMD,X,Y,2)
                WT2(J-1) = 1.0 - WT2(J)
C
                EQWT(5) = EQWT(5) + WT1(2) * (WT2(1)+WT2(3))
                EQWT(8) = EQWT(8) + WT1(2) * WT2(2)
C
              ELSE
                EQWT(8) = EQWT(8) + WT1(2)
              ENDIF
C
            ELSEIF (ICT .EQ. SAFCT .OR.
     >              ICT .EQ. SAFMCT) THEN
C
              IF (LSNGL) THEN
                X(1) =  2.0
                X(2) =  4.0
                J = 2
                WT2(J)   = ALGSLP(QMD,X,Y,2)
                WT2(J-1) = 1.0 - WT2(J)
C
                EQWT(5) = EQWT(5) + WT1(2) * WT2(1)
                EQWT(8) = EQWT(8) + WT1(2) * WT2(2)
              ELSE
                EQWT(8) = EQWT(8) + WT1(2)
              ENDIF
C
            ENDIF
          ENDIF
C
          IF (WT1(3) .GT. 0.0) THEN
C
            X(1) =  2.0
            X(2) =  4.0
            J = 2
            WT2(J)   = ALGSLP(QMD,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            EQWT(5) = EQWT(5) + WT1(3) * WT2(1)
            EQWT(8) = EQWT(8) + WT1(3) * WT2(2)
C
          ENDIF
C
          IF (WT1(4) .GT. 0.0) THEN
C
            IF (LSNGL .AND.
     >          (ICT .EQ. LPCT .OR.
     >           ICT .EQ. WPCT .OR.
     >           ICT .EQ. LPWLCT)) THEN
C
              X(1) =  2.0
              X(2) =  4.0
              J = 2
              WT2(J)   = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)
C
              EQWT(5) = EQWT(5) + WT1(4) * WT2(1)
              EQWT(8) = EQWT(8) + WT1(4) * WT2(2)
            ELSE
              EQWT(8)   = EQWT(8) + WT1(4)
            ENDIF
C
          ENDIF
C----------
C  ENGELMANN SPRUCE, WESTERN LARCH
C----------
        CASE (ESCT,WLCT)
C
          IF (QMD .LE. 4.0) THEN
            X(1) =  2.0
            X(2) =  4.0
            J = 2
          ELSEIF (QMD .GT.  4.0 .AND. QMD .LT.  9.0) THEN
            X(1) =  7.0
            X(2) =  9.0
            J = 3
          ELSEIF (QMD .GE.  9.0) THEN
            X(1) = 19.0
            X(2) = 21.0
            J = 4
          ENDIF
          WT1(J)   = ALGSLP(QMD,X,Y,2)
          WT1(J-1) = 1.0 - WT1(J)
C
          IF (WT1(1) .GT. 0.0) THEN
C
            DO I = 1,3
              WT2(I) = 0.0
            ENDDO
C
            IF (PERCOV .LE. 30.0) THEN
              X(1) = 10.0
              X(2) = 30.0
              J = 2
            ELSE
              X(1) = 70.0
              X(2) = 90.0
              J = 3
            ENDIF
            WT2(J)   = ALGSLP(PERCOV,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            EQWT(1) = EQWT(1) + WT1(1) * WT2(1)
            EQWT(5) = EQWT(5) + WT1(1) * WT2(2)
            EQWT(8) = EQWT(8) + WT1(1) * WT2(3)
C
          ENDIF
C
          IF (WT1(2) .GT. 0.0) THEN
C
            DO I = 1,2
              WT2(I) = 0.0
            ENDDO
C
            X(1) = 10.0
            X(2) = 30.0
            J = 2
C
            WT2(J)   = ALGSLP(PERCOV,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            IF (LSNGL) THEN
              EQWT(1) = EQWT(1) + WT1(2) * WT2(1)
            ELSE
              EQWT(5) = EQWT(5) + WT1(2) * WT2(1)
            ENDIF
            EQWT(8)   = EQWT(8) + WT1(2) * WT2(2)
C
          ENDIF
C
          IF (WT1(3) .GT. 0.0) THEN
C
            DO I = 1,2
              WT2(I) = 0.0
            ENDDO
C
            X(1) = 10.0
            X(2) = 30.0
            J = 2
C
            WT2(J)   = ALGSLP(PERCOV,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            IF ((ICT .EQ. ESCT) .OR.
     &          ((ICT .EQ. WLCT) .AND. LSNGL)) THEN
              EQWT(5) = EQWT(5) + WT1(3) * WT2(1)
              EQWT(8) = EQWT(8) + WT1(3) * WT2(2)
            ELSE
              EQWT(8) = EQWT(8) + WT1(3)
            ENDIF
C
          ENDIF
C
          IF (WT1(4) .GT. 0.0) THEN
C
            DO I = 1,2
              WT2(I) = 0.0
            ENDDO
C
            X(1) = 10.0
            X(2) = 30.0
            J = 2
C
            WT2(J)   = ALGSLP(PERCOV,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            IF (((ICT .EQ. ESCT) .AND. LSNGL) .OR.
     &          ((ICT .EQ. WLCT) .AND. .NOT.LSNGL)) THEN
              EQWT(5) = EQWT(5) + WT1(4) * WT2(1)
            ELSE
              EQWT(1) = EQWT(1) + WT1(4) * WT2(1)
            ENDIF
            EQWT(8)   = EQWT(8) + WT1(4) * WT2(2)
C
          ENDIF
C----------
C  PONDEROSA PINE
C----------
        CASE (PPCT)
C
          IF (PERCOV .LT. 30.0) THEN
            X(1) =  10.0
            X(2) =  30.0
            J = 2
          ELSEIF (PERCOV .GE. 30.0 .AND. PERCOV .LT. 60.0) THEN
            X(1) = 40.0
            X(2) = 60.0
            J = 3
          ELSEIF (PERCOV .GE. 60.0) THEN
            X(1) = 70.0
            X(2) = 90.0
            J = 4
          ENDIF
          WT1(J)   = ALGSLP(PERCOV,X,Y,2)
          WT1(J-1) = 1.0 - WT1(J)
C
          IF (WT1(1) .GT. 0.0) THEN
            IF (LSNGL) THEN
              EQWT(2) = EQWT(2) + WT1(1)
            ELSE
              EQWT(6) = EQWT(6) + WT1(1)
            ENDIF
          ENDIF
C
          IF (WT1(2) .GT. 0.0) THEN
            IF (LSNGL) THEN
              EQWT(6) = EQWT(6) + WT1(2)
            ELSE
              EQWT(9) = EQWT(9) + WT1(2)
            ENDIF
          ENDIF
C
          IF (WT1(3) .GT. 0.0) THEN
C
            DO I = 1,2
              WT2(I) = 0.0
            ENDDO
C
            X(1) = 2.0
            X(2) = 4.0
            J = 2
C
            WT2(J)   = ALGSLP(QMD,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            EQWT(6) = EQWT(6) + WT1(3) * WT2(1)
            EQWT(9) = EQWT(9) + WT1(3) * WT2(2)
C
          ENDIF
C
          IF (WT1(4) .GT. 0.0) THEN
            EQWT(9) = EQWT(9) + WT1(4)
          ENDIF
C----------
C  DRY MIXED
C----------
        CASE (DMIXCT)
C
          IF (PERCOV .LT. 30.0) THEN
            X(1) =  10.0
            X(2) =  30.0
            J = 2
          ELSEIF (PERCOV .GE. 30.0 .AND. PERCOV .LT. 60.0) THEN
            X(1) = 40.0
            X(2) = 60.0
            J = 3
          ELSEIF (PERCOV .GE. 60.0) THEN
            X(1) = 70.0
            X(2) = 90.0
            J = 4
          ENDIF
          WT1(J)   = ALGSLP(PERCOV,X,Y,2)
          WT1(J-1) = 1.0 - WT1(J)
C
          IF (WT1(1) .GT. 0.0) THEN
            IF (LSNGL) THEN
              EQWT(2) = EQWT(2) + WT1(1)
            ELSE
              EQWT(6) = EQWT(6) + WT1(1)
            ENDIF
          ENDIF
C
          IF (WT1(2) .GT. 0.0) THEN
            IF (LSNGL) THEN
              EQWT(6) = EQWT(6) + WT1(2)
            ELSE
              EQWT(8) = EQWT(8) + WT1(2)
            ENDIF
          ENDIF
C
          IF (WT1(3) .GT. 0.0) THEN
C
            DO I = 1,3
              WT2(I) = 0.0
            ENDDO
C
            IF (QMD .LE. 4.0) THEN
              X(1) = 2.0
              X(2) = 4.0
              J = 2
            ELSE
              X(1) = 19.0
              X(2) = 21.0
              J = 3
            ENDIF
            WT2(J)   = ALGSLP(QMD,X,Y,2)
            WT2(J-1) = 1.0 - WT2(J)
C
            EQWT(6)   = EQWT(6) + WT1(3) * WT2(1)
            IF (LSNGL) THEN
              EQWT(9) = EQWT(9) + WT1(3) * WT2(2)
            ELSE
              EQWT(8) = EQWT(8) + WT1(3) * WT2(2)
            ENDIF
            EQWT(8)   = EQWT(8) + WT1(3) * WT2(3)
C
          ENDIF
C
          IF (WT1(4) .GT. 0.0) THEN
            EQWT(8) = EQWT(8) + WT1(4)
          ENDIF
C----------
C  PONDEROSA PINE - DOUGLAS-FIR
C----------
        CASE (PPDFCT)
C
          IF (PERCOV .LT. 30.0) THEN
            X(1) =  10.0
            X(2) =  30.0
            J = 2
          ELSEIF (PERCOV .GE. 30.0 .AND. PERCOV .LT. 60.0) THEN
            X(1) = 40.0
            X(2) = 60.0
            J = 3
          ELSEIF (PERCOV .GE. 60.0) THEN
            X(1) = 70.0
            X(2) = 90.0
            J = 4
          ENDIF
          WT1(J)   = ALGSLP(PERCOV,X,Y,2)
          WT1(J-1) = 1.0 - WT1(J)
C
          IF (WT1(1) .GT. 0.0) THEN
            IF (LSNGL) THEN
              EQWT(2) = EQWT(2) + WT1(1)
            ELSE
              EQWT(6) = EQWT(6) + WT1(1)
            ENDIF
          ENDIF
C
          IF (WT1(2) .GT. 0.0) THEN
C
            IF (LSNGL) THEN
C
              DO I = 1,3
                WT2(I) = 0.0
              ENDDO
C
              IF (QMD .LE. 4.0) THEN
                X(1) = 2.0
                X(2) = 4.0
                J = 2
              ELSE
                X(1) = 19.0
                X(2) = 21.0
                J = 3
              ENDIF
              WT2(J)   = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)
C
              EQWT(6)   = EQWT(6) + WT1(2) * (WT2(1) + WT2(3))
              EQWT(8)   = EQWT(8) + WT1(2) *  WT2(2)
            ELSE
              EQWT(8)   = EQWT(8) + WT1(2)
            ENDIF
C
          ENDIF
C
          IF (WT1(3) .GT. 0.0) THEN
C
            IF (LSNGL) THEN
              DO I = 1,2
                WT2(I) = 0.0
              ENDDO
C
              X(1) = 2.0
              X(2) = 4.0
              J = 2
              WT2(J)   = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)
C
              EQWT(6)  = EQWT(6) + WT1(3) * WT2(1)
              EQWT(9)  = EQWT(9) + WT1(3) * WT2(2)
C
            ELSE
              EQWT(8)  = EQWT(8) + WT1(3)
            ENDIF
C
          ENDIF
C
          IF (WT1(4) .GT. 0.0) THEN
C
            IF (LSNGL) THEN
              DO I = 1,3
                WT2(I) = 0.0
              ENDDO
C
              X(1) =  2.0
              X(2) =  4.0
              J = 2
              WT2(J)   = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)
C
              EQWT(8)  = EQWT(8) + WT1(3) * WT2(1)
              EQWT(9)  = EQWT(9) + WT1(3) * WT2(2)
C
            ELSE
              EQWT(8)   = EQWT(8) + WT1(4)
            ENDIF
          ENDIF
C
      END SELECT
C----------
C  END OF DETAILED LOW FUEL MODEL SELECTION
C
C  DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C  ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
C  CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
C  RELATIONSHIP DECLINES FROM PURE 11 INITIALLY TO PURE 10 AFTER
C  THE PERIOD EXPIRES.
C----------
      AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
      IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
        LATFUEL  = .TRUE.
        EQWT(11) = AFWT
        IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
      ENDIF
      IF (.NOT. LATFUEL) AFWT = 0.0
C----------
C  MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
C  OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
C----------
      EQWT(10) = 1.0 - AFWT
      EQWT(12) = 1.0
      EQWT(13) = 1.0
C----------
C  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)
C----------
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)
C
      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)
C
      RETURN
      END
