      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C   **FMCFMD FIRE-BM-DATE OF LAST REVISION: 08/20/04
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGHT OF FWT(1)=1.0 AND THE CLOSEST
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

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'SSTGMC.F77'

C     LOCAL VARIABLE DECLARATIONS
C
C     NUMBER OF POTENTIAL FIRE MODELS
C
      INTEGER   ICLSS
      PARAMETER(ICLSS = 14)
C
      INTEGER   IYR, FMD

      INTEGER   I,J,K
      INTEGER   IPTR(ICLSS),ITYP(ICLSS)
      REAL      XPTS(ICLSS,2),EQWT(ICLSS),WT1(2),WT2(4),WT3(2),WT4(2)
      REAL      STNDBA,X(2),Y(2),COVA,COVB,AFWT
      REAL      ALGSLP,SZCLS,WD(13),SCLAB(13)

      REAL      PRDF,PRPP,SWT
      LOGICAL   DEBUG,VL1(MAXTRE)
C
C     FIXED VALUES FOR INTERPOLATION FUNCTION
C
      DATA     Y / 0.0, 1.0 /
C
C     SIZE CLASS LABELS (AS REAL) FOR THE BLUE MOUNTAIN
C
      DATA SCLAB /1.0, 2.0, 3.0, 4.0, 5.0,
     >            6.0, 6.5, 7.0, 7.5, 8.0,
     >            9.0,10.0,11.0/
C
C     These are the integer tags associated with each fire model
C     class. They are returned with the weight
C
      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /
C
c     These are 0 for regular lines, -1 for horizontal and 1 for
c     vertical lines. If any of the lines defined by xpts() are of
c     an unusual variety, this must be entered here so that
c     special logic can be invoked.  In this case, all the line
c     segments have a |slope| that is > 0 and less than inif.
C
      DATA ITYP / ICLSS * 0 /
C
C     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C       WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C       COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C       SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).
C
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
     >  30., 60.,   ! FMD  12 ! shares with 14
     >  45.,100.,   ! FMD  13
     >  30., 60./   ! FMD  14
C
C     INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
C     WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C     MUST SUM TO 1, WRT EACH OTHER. FMDYN WILL RESCALE COLINEAR WEIGHTS SO THAT THEY SUM
C     TO ONE, IF THEY DON'T ALREADY!
C
      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO
C
C     BEGIN ROUTINE
C
      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)

      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)
C
C     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.
C
      IF (LUSRFM) RETURN

      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C
C     COMPUTE TOTAL STAND BASAL AREA AND PROPORTION IN PONDEROSA
C     PLUS DOUGLAS-FIR
C
      STNDBA = 0.0
      DO I = 1,MAXSP
        STNDBA = STNDBA + FMTBA(I)
      ENDDO

      PRDF = 0.0
      PRPP = 0.0
      IF (STNDBA .GT. 0.01) THEN
        PRDF = FMTBA(3) /STNDBA
        PRPP = FMTBA(10)/STNDBA
      ENDIF
C
C     3 - RESOLVE DYNAMIC FMD
C     RULES ARE BASED ON A TABLE GIVEN BY LES HOLSAPPLE. DON ROBINSON AT
C     ESSA DERIVED THIS LOGIC BASED ON THOSE VALUES. TRANSITION CUTPOINTS
C     WERE MODIFIED AS FOLLOWS: %CC [ PERCOV ] CUTPOINTS WERE ADJUSTED SO
C     THAT THEY BEGIN TO CHANGE -5% FROM THE CUTPOINT AND FINISH CHANGING
C     +5% ABOVE THE CUTPOINT.
C
      X(1) =  0.40
      X(2) =  0.60
      J = 2

      WT1(J)   = ALGSLP(MAX(PRDF,PRPP),X,Y,2)
      WT1(J-1) = 1.0 - WT1(J)
C
C     NEITHER PONDEROSA NOR DOUGLAS-FIR DOMINATES
C
      IF (WT1(1) .GT. 0.0) THEN
C
C     CALL A SIMPLIFIED VERSION OF /BASE/SRC/SSTAGE (01/16/02 VERSION)
C     TO COMPUTE COVER IN THE UPPER (COVA) AND LOWER (COVB) CANOPY (ALL
C     IN UPPER IF <2 CANOPY LAYERS IDENTIFIED. LOGICAL VECTOR VL1()
C     IDENTIFIES MEMBERS OF THE UPPER CANOPY, TO BE USED IN FINDINGING
C     THE STAND SIZE CLASS.
C
        CALL BMSTAGE(COVA,COVB,VL1)
        CALL BMSZCLS(DBH,FMPROB,VL1,ITRN,WD)
C
C       SZCLS - SIZE CLASS SYSTEM USED IN BM (13 GROUPS IN ALL)
C       COVA  - % CANOPY COVER IN UPPER STRATUM
C       COVB  - % CANOPY COVER IN LOWER STRATUM
C
        DO K = 1,13
          IF (WD(K) .GT. 0.0) THEN
            SZCLS = SCLAB(K)

            IF (SZCLS .LE. 3.0) THEN

              EQWT(5) = EQWT(5) + WT1(1) * WD(K)

            ELSEIF (SZCLS .GT. 3.0 .AND. SZCLS .LT. 7.0) THEN

              X(1) = 25.0
              X(2) = 35.0
              J = 2

              WT2(J)   = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              IF (WT2(1) .GT. 0.0) THEN
                EQWT(5)  = EQWT(5) + WT1(1) * WT2(1) * WD(K)
              ENDIF

              IF (WT2(2) .GT. 0.0) THEN
                EQWT(8)  = EQWT(8) + WT1(1) * WT2(2) * WD(K)
              ENDIF

            ELSE  !SZCLS >7

              X(1) = 25.0
              X(2) = 35.0
              J = 2
              WT2(J)   = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              IF (WT2(1) .GT. 0.0) THEN
                EQWT(5) = EQWT(5) + WT1(1) * WT2(1) * WD(K)
              ENDIF

              IF (WT2(2) .GT. 0.0) THEN

                X(1) = 25.0
                X(2) = 35.0
                J = 2
                WT3(J)   = ALGSLP(COVA,X,Y,2)
                WT3(J-1) = 1.0 - WT3(J)

                X(1) = 10.0
                X(2) = 20.0
                J = 2
                WT4(J)   = ALGSLP(COVB,X,Y,2)
                WT4(J-1) = 1.0 - WT4(J)

                EQWT(8)  = EQWT(8 ) + WT1(1) * WT2(2) * WT3(1) * WT4(1)
     >                                * WD(K)
                EQWT(8)  = EQWT(8 ) + WT1(1) * WT2(2) * WT3(1) * WT4(2)
     >                                * WD(K)
                EQWT(8)  = EQWT(8 ) + WT1(1) * WT2(2) * WT3(2) * WT4(1)
     >                                * WD(K)

                EQWT(10) = EQWT(10) + WT1(1) * WT2(2) * WT3(2) * WT4(2)
     >                                * WD(K)

              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C     CASE 2: DOUGLAS-FIR OR PONDEROSA DOMINATES BASAL AREA
C
      IF (WT1(2) .GT. 0.0) THEN

        DO I = 1,4
          WT2(I) = 0.0
        ENDDO

        IF (PERCOV .LT. 15.0) THEN
          X(1) =   5.0
          X(2) =  15.0
          J = 2
        ELSEIF (PERCOV .GE. 15.0 .AND. PERCOV .LT. 35.0) THEN
          X(1) = 25.0
          X(2) = 35.0
          J = 3
        ELSEIF (PERCOV .GE. 35.0) THEN
          X(1) = 45.0
          X(2) = 55.0
          J = 4
        ENDIF
        WT2(J)   = ALGSLP(PERCOV,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        IF (WT2(1) .GT. 0.0) THEN
          EQWT(1) = EQWT(1) + WT1(2) * WT2(1)
        ENDIF

        IF (WT2(2) .GT. 0.0) THEN
          EQWT(2) = EQWT(2) + WT1(2) * WT2(2)
        ENDIF

        IF (WT2(3) .GT. 0.0) THEN

          X(1) = 0.40
          X(2) = 0.60
          J = 2
          WT3(J)   = ALGSLP((PRPP/(PRPP+PRDF)),X,Y,2)
          WT3(J-1) = 1.0 - WT3(J)

          IF (WT3(1) .GT. 0.0) THEN
            EQWT(10) = EQWT(10) + WT1(2) * WT2(3) * WT3(1)
          ENDIF

          IF (WT3(2) .GT. 0.0) THEN
            EQWT(2)  = EQWT(2)  + WT1(2) * WT2(3) * WT3(2)
          ENDIF

        ENDIF

        IF (WT2(4) .GT. 0.0) THEN
          EQWT(9) = EQWT(9) + WT1(2) * WT2(4)
        ENDIF

      ENDIF
C
C     END OF DETAILED LOW FUEL MODEL SELECTION
C
C     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 AND 14 ARE
C     CANDIDATE MODELS, SHARING WITH 10 AND 12 RESPECTIVELY. THE
C     WEIGHT OF THE SHARED RELATIONSHIP DECLINES FROM PURE 11 INITIALLY,
C     TO PURE 10 AFTER THE PERIOD EXPIRES. SIMILARLY, COMPUTE WEIGHT FOR
C     MODEL 14 ACTIVITY FUELS, SHARED WITH CURRENT MODEL 12. THE
C     RELATIONSHIP CHANGES IN THE SAME WAS AS THE 10/11 FUELS.
C
      AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
      IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
        LATFUEL = .TRUE.
        EQWT(11)  = AFWT
        EQWT(14)  = AFWT
        IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
      ENDIF
      IF (.NOT. LATFUEL) AFWT = 0.0
C
C     MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
C     OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
C
      EQWT(10) = 1.0 - AFWT
      EQWT(12) = 1.0 - AFWT
      EQWT(13) = 1.0
C
C     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)
C
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)

      RETURN
      END
C--------------------------------------------------------------------
C
C     THIS IS CRIBBED FROM SSTAGE, AND STRIPPED SO THAT IT YIELDS ONLY
C     THE FIRST CANOPY BREAKPOINT, IF ANY.
C
      SUBROUTINE BMSTAGE(CCA,CCB,LA)
      IMPLICIT NONE
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
C
COMMONS
C
C     INDEX: HT-ORDERED LIST OF TREE INDEX
C     LA:    INDEX OF MEMBERS IN LAYER "A"
C
      INTEGER INDEX(MAXTRE)
      LOGICAL LA(MAXTRE)

      EQUIVALENCE (WK5,INDEX)

      REAL    CCA,CCB

      REAL    X,DIFF,DIFF1,SUMPRB,XHT,SPROB
      REAL    CRS1,CRS2
      INTEGER II,NSTR
      INTEGER ID1I1,ID1I2,ID2I1,ID2I2
      INTEGER IS1I1,IS1I2,IS2I1,IS2I2
      INTEGER IILG,ILARGE,ISMALL
      INTEGER IS1OK,IS2OK

      INTEGER I,NTREES
C
C     INITIALIZE
C
      XHT    = 0.0
      CRS1   = 0.0
      CRS2   = 0.0
      DO I = 1,MAXTRE
        LA(I) = .FALSE.
      ENDDO

C
C     LOAD THE INDEX WITH TREES THAT REPRESENT OVER A LIMITED
C     TREES/ACRE
C
      NTREES = 0
      SPROB  = 0.0
      DO I = 1,ITRN
        SPROB = SPROB + PROB(I)
        IF (PROB(I) .GT. 0.00001) THEN
          NTREES = NTREES + 1
          INDEX(NTREES) = I
        ENDIF
      ENDDO
      IF (NTREES .EQ. 0) GOTO 100
C
C     1-RECORD SPECIAL CASE
C
      IF (NTREES .LE. 1) THEN
        I=INDEX(1)
        WK6(I)=CRWDTH(I)
        CRS1 = WK6(I) * WK6(I) * PROB(I) * 0.785398 / 43560.0
        IS1I1 = 1
        IS1I2 = 1
        GOTO 80
      ENDIF

      CALL RDPSRT (NTREES,HT,INDEX,.FALSE.)

C     CALCULATE THE CCF-BASED "CROWN WIDTH" FOR EACH TREE...AND
C     CONVERT THIS TO CROWN COVER.

      DO II = 1,NTREES
        I= INDEX(II)
        WK6(I)=CRWDTH(I)
        WK6(I) = WK6(I) * WK6(I) * PROB(I) * 0.785398
      ENDDO
C
C     FIND THE LARGEST DIFFERENCE: DIFF1
C
C     THE FOLLOWING POINTERS POINT TO THE FIRST AND THE LAST TREE
C     RECORD IN A GAP.  THAT IS, A GAP MAY SPAN MORE THAN ONE TREE
C     RECORD (IN THE CASE THAT WE FIND INSIGNIFICANT "LADDER TREES").
C
      DIFF1  = -1E20
      ID1I1  = 0
      ID1I2  = 0
      ID2I1  = 0
      ID2I2  = 0

      IILG   = 1
      ILARGE = INDEX(IILG)
      SUMPRB = 0.0

      DO II = 2,NTREES
        ISMALL=INDEX(II)
C
C       IF THE SMALLER TREE CREATES A GAP...
C       (A GAP MUST BE AT LEAST 10 FEET or 30% OF THE HEIGHT)...
C       THE CONTRIBUTION OF THE TREE MUST BE AT LEAST 2% BEFORE
C       THE GAP IS COUNTED
C
        X = MAX(10.0, HT(ILARGE) * 30.0 * 0.01)
        IF (HT(ISMALL) .LT. HT(ILARGE)-X) THEN
          IF (PROB(ISMALL) + SUMPRB .LT. 2.0) THEN
            SUMPRB = SUMPRB + PROB(ISMALL)
          ELSE
            DIFF = HT(ILARGE) - HT(ISMALL)
            IF (DIFF .GT. DIFF1) THEN
              DIFF1  = DIFF
              ID1I1  = IILG
              ID1I2  = II
            ENDIF
            ILARGE = ISMALL
            IILG   = II
            SUMPRB = 0.0
          ENDIF
        ELSE
C
C       IF THE SMALLER TREE DOES NOT CREATE A GAP...THEN SEE IF
C       IF IT IS AN INSIGNIFICANT "LADDER" TREE. IF IT IS AN
C       INSIGNIFICANT LADDER...AND NO GAP WAS FOUND. RESET THE
C       SUMMATION AND MOVE THE "TOP" TREE DOWN.
C
          IF (PROB(ISMALL) + SUMPRB .LT. 2.0) THEN
            SUMPRB = SUMPRB + PROB(ISMALL)
          ELSE
            ILARGE = ISMALL
            IILG   = II
            SUMPRB = 0.0
          ENDIF
        ENDIF
      ENDDO
C
C     THESE POINTERS POINT TO THE TOP AND BOTTOM OF EACH STRATUM,
C     IF ID1I1 HAS BEEN SET, THEN THERE ARE 2 POTENTIAL STRATA (NSTR)
C
      NSTR  = 1
      IS1I1 = 1
      IS1I2 = NTREES
      IS2I1 = 0
      IS2I2 = 0
      IF (ID1I1.GT.0) THEN
        NSTR  = 2
        IS1I2 = ID1I1
        IS2I1 = ID1I2
        IS2I2 = NTREES
      ENDIF
C
C     THE FIRST POTENTIAL STRATUM CONTAINS TREES FROM 1 TO ID1I1
C     THE SECOND CONTAINS TREES FROM ID1I2 TO ID2I1
C     COMPUTE AND SAVE THE COVER IN EACH POTENTIAL STRATUM AND
C     CHECK FOR MIN COVER.
C
C     CHECK THE FIRST (UPPER) LAYER
C     FOR THE PURPOSES OF COMPUTING THE COVER, INCLUDE THE TREES THAT
C     ARE WITHIN THE GAP IN THE UPPER STRATUM.
C
      IS1OK = 0
      CRS1  = 0.0
      IS1I2 = MAX(IS1I2,IS2I1-1)
      CALL COVOLP (.FALSE.,JOSTND,IS1I2-IS1I1+1,INDEX(IS1I1),WK6,CRS1,
     &  CCCOEF)
      IF (CRS1 .GT. 5.0) IS1OK = 1
C
C     CHECK THE SECOND (LOWER) LAYER
C
      IS2OK = 0
      CRS2  = 0.0
      IF (NSTR .GE. 2) THEN
        CALL COVOLP (.FALSE.,JOSTND,IS2I2-IS2I1+1,INDEX(IS2I1),WK6,CRS2,
     &   CCCOEF)
        IF (CRS2 .GT. 5.0) IS2OK = 1
      ENDIF
C
C     COUNT THE VALID (>5%) STRATA
C     IF THERE ARE NOT VALID STRATA AND IF THE NUMBER OF TREES ARE
C     UNDER 200, THEN WE ARE DONE.  OTHERWISE, FORM ONE STRATUM OF ALL
C     THE TREES AND RECOMPUTE COVER.
C
      NSTR = IS1OK + IS2OK
      IF (NSTR.EQ.0) THEN
        IF (SPROB .LT. 200.0) GOTO 80
        CRS2 = 0.0
        IS1I1 = 1
        IS1I2 = NTREES
        CALL COVOLP (.FALSE.,JOSTND,IS1I2,INDEX(IS1I1),WK6,CRS1,
     &   CCCOEF)
      ENDIF

   80 CCA = CRS1
      CCB = CRS2

      DO I = IS1I1,IS1I2
        LA(INDEX(I)) = .TRUE.
      ENDDO

  100 CONTINUE

      RETURN
      END
C--------------------------------------------------------------------
C
C     COMPUTE THE SIZE CLASS OF TREES IN THE UPPER CANOPY
C
C     D  = REAL VECTOR OF DBH
C     P  = REAL VECTOR OF STEMS/ACRE
C     VL = LOGICAL VECTOR OF MEMEBERSHIP IN TOP LAYER
C     I  = INTEGER LENGTH OF D() AND VL() VECTORS
C     S  = WT (BA-BASE) OF EACH SIZE CLASS
C     C  = COMPLEXITY OF SIZE CLASS (1=SIMPLE, 2=COMPLEX)
C     WDOM = WEIGHTED DOMINANT CATEGORY [ RETURNED ]
C
      SUBROUTINE BMSZCLS(D,P,VL,N,WDOM)
      IMPLICIT NONE

      REAL    D,P
      LOGICAL VL
      INTEGER N
      REAL    WDOM(13)
      DIMENSION D(N),P(N),VL(N)

      INTEGER I,J,ICLS,CCLS,C(13),PASS
      REAL    BA,DT,S(13),SWT,XRAN,JITTER
      DOUBLE PRECISION SAVESO

      DATA PASS /50/, JITTER /0.2/
C
C     ZERO BINS TO WEIGHTED DOMINANT CATEGORY AND COMPLEXITY INDEX
C
      DO I = 1,13
        WDOM(I) = 0.0
        C(I)    = 1
      ENDDO
C
C     POPULATE BASIC SIZE CATEGORIES USING UNIFORM RANDOM JITTER.
C     THE IDEA HERE IS TO SAMPLE DIAMETERS JITTERED
C     BY 20%, THEN RECOMPUTE A WEIGHTED DOMINANT CATEGORY.
C
      CALL RANNGET(SAVESO)
      
      DO J = 1,PASS
C
C     ZERO BINS TO HOLD BA-BASED WEIGHTS FOR THE BLUE MOUNTAINS
C     SIZE CATEGORIES
C
        DO I = 1,13
          S(I) = 0.0
        ENDDO

        DO I = 1,N
          IF (VL(I)) THEN
            CALL RANN(XRAN)
            DT = D(I) * (1.0 + JITTER * ((XRAN * 2.0) - 1.0))
            BA = DT * DT * P(I)
            IF     (DT .GE.  0.0 .AND. DT .LT.  1.0) THEN ! SEEDLING    [1]
              S(1) = S(1) + BA
              C(1) = 1
            ELSEIF (DT .GE.  1.0 .AND. DT .LT.  5.0) THEN ! SAPLING     [2]
              S(2) = S(2) + BA
              C(2) = 1
            ELSEIF (DT .GE.  5.0 .AND. DT .LT.  9.0) THEN ! POLE        [3]
              S(3) = S(3) + BA
              C(3) = 1
            ELSEIF (DT .GE.  9.0 .AND. DT .LT. 15.0) THEN ! SMALLTREE:9-15  [4]
              S(4) = S(4) + BA
              C(4) = 1
            ELSEIF (DT .GE. 15.0 .AND. DT .LT. 21.0) THEN ! SMALLTREE:15-21 [5]
              S(5) = S(5) + BA
              C(5) = 1
            ELSEIF (DT .GE. 21.0 .AND. DT .LT. 32.0) THEN ! MEDIUMTREE  [6]
              S(6) = S(6) + BA
              C(6) = 1
            ELSE                                          ! LARGETREE   [7]
              S(7) = S(7) + BA
              C(7) = 1
            ENDIF
          ENDIF
        ENDDO
        
C
C     CREATE COMPOSITE SIZE CATEGORIES
C
        S(8)  = S(1)  + S(2)  ! SEEDLING+SAPLING                        [8]
        C(8)  = 2

        S(9)  = S(2)  + S(3)  ! SAPLING+POLE                            [9]
        C(9)  = 2

        S(10) = S(4)  + S(5)  ! SMALLTREE(9-21)                        [10]
        C(10)  = 2

        S(11) = S(3)  + S(10) ! POLE+SMALLTREE(9-21)                   [11]
        C(11)  = 3

        S(12) = S(10) + S(6)  ! SMALLTREE(9-21)+MEDIUM                 [12]
        C(12)  = 3

        S(13) = S(6)  + S(7)  ! MEDIUM+LARGE                           [13]
        C(13)  = 2

        ICLS =  0
        CCLS =  4
        SWT  = -1.0
        DO I = 1,13
          IF (S(I) .EQ. SWT) THEN
            IF (C(I) .LT. CCLS) THEN
              CCLS = C(I)
              ICLS = I
            ENDIF
          ELSE
            IF (S(I) .GT. SWT) THEN
              SWT = S(I)
              CCLS = C(I)
              ICLS = I
            ENDIF
          ENDIF
        ENDDO

        WDOM(ICLS) = WDOM(ICLS) + 1.0/FLOAT(PASS)

      ENDDO
      
      CALL RANNPUT(SAVESO)

      RETURN
      END

