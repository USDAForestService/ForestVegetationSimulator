      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C FIRE-NC $Id$
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGHT OF FWT(1)=1.0 AND THE CLOSEST
*     FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE BY THE DYNAMIC
*     FUEL MODEL
*
*
*  CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
*     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
*
***********************************************************************

C     PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'

C     LOCAL VARIABLE DECLARATIONS

C     NUMBER OF POTENTIAL FIRE MODELS

      INTEGER ICLSS,CWHRR,CWHRC
      PARAMETER(ICLSS = 13)
      PARAMETER(CWHRR=9)  ! ROWS IN CWHR MATRIX
      PARAMETER(CWHRC=18)  ! COLS IN CWHR MATRIXC
      CHARACTER(LEN=3) :: SZDN, SDTMP
      CHARACTER(LEN=2), DIMENSION(0:CWHRR) :: FTLABL
      CHARACTER(LEN=2), DIMENSION(0:CWHRC) :: SSLABL
      INTEGER   FMD, IFT, JSS
      INTEGER   IYR,I,J
      INTEGER   IPTR(ICLSS), ITYP(ICLSS)
      INTEGER   CWHR_MOD(4), IMD(2)
      INTEGER   FMD_R5(CWHRR,CWHRC),FMD_R6(CWHRR,CWHRC)
!      INTEGER, DIMENSION (:,:), POINTER :: CWHRFMD
      INTEGER   CWHRFMD(CWHRR,CWHRC)
      REAL      XPTS(ICLSS,2), EQWT(ICLSS)
      REAL      BAPCT(MAXSP), STNDBA, AFWT
      REAL      CWHR_WT(4), CWXPTS(4,2), CCBP(3), DBHBP(4)
      LOGICAL   DEBUG, LDRY

      INTEGER   FINDJSS, FINDMOD

C     LABELS FOR DEBUG OUTPUT: FOREST TYPE CODES AND CWHR
C     STRUCTURAL STAGES

      DATA     FTLABL /
     >          "? ","P ","R ","WE","WW","D ","H ","MP","MC","OS" /

      DATA     SSLABL /
     >          "? ",
     >          "1 ",
     >          "2S", "2P", "2M", "2D",
     >          "3S", "3P", "3M", "3D",
     >          "4S", "4P", "4M", "4D",
     >          "5S", "5P", "5M", "5D",
     >          "6 " /

C     CWHR FUEL MODEL CLASSIFICATION: ROWS ARE FOREST TYPE; COLUMNS ARE STRUCTURAL
C     STAGE DENSITY GROUPS:

C     Row labels: "P","R","WE","WW","D","J","H","X","MP","MC","OS"


C     OREGON (R5) CWHR FUEL MODEL CLASSIFICATION: ROWS ARE FOREST TYPE; COLUMNS ARE
C     STRUCTURAL STAGE DENSITY GROUPS:

C     Row labels: "P","R","W","WE","D","J","H","X","MP","MC","OS"

      DATA ((FMD_R5(I,J), J=1,CWHRC), I=1,CWHRR) /
     >  5, 6, 6, 6, 6, 2, 2, 9, 9, 2, 2, 2, 9, 2, 2, 9, 9,10, ! 1 pine
     >  5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 2 red fir
     >  5, 5, 5, 8, 8,11,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 3 wh fir E
     >  5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 4 wh fir W
     >  5, 5, 5, 6, 6, 6, 6, 8, 8,11,11, 9, 8,11,11, 9, 8,10, ! 5 Douglas-fir
     >  5, 5, 5, 6, 6,11,11,11, 9, 9, 9, 9, 9, 9, 9, 9, 9,10, ! 6 hardwoods
     >  5, 5, 5, 6, 6, 6, 6, 6, 9, 9, 9, 8, 8, 8, 8, 8, 8,10, ! 7 mixed pine
     >  5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10, ! 8 mixed conifer
     >  5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10/ ! 9 other soft

C     CALIFORNIA (R6) CWHR FUEL MODEL CLASSIFICATION: ROWS ARE FOREST TYPE; COLUMNS ARE
C     STRUCTURAL STAGE DENSITY GROUPS:

C     Row labels: "P","R","WW","WE","D","J","H","X","MP","MC","OS"

      DATA ((FMD_R6(I,J), J=1,CWHRC), I=1,CWHRR) /
     >  5, 6, 6, 6, 6, 2, 2, 9, 9, 2, 2, 2, 9, 2, 2, 9, 9,10, ! 1 pine
     >  5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 2 red fir
     >  5, 5, 5, 8, 8,11,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 3 wh fir E
     >  5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 4 wh fir W
     >  5, 5, 5, 6, 6, 6, 6, 8, 8,11,11, 9, 8,11,11, 9, 8,10, ! 5 Douglas-fir
     >  5, 5, 5, 6, 6,11,11,11, 9, 9, 9, 9, 9, 9, 9, 9, 9,10, ! 6 hardwoods
     >  5, 5, 5, 6, 6, 6, 6, 6, 9, 9, 9, 8, 8, 8, 8, 8, 8,10, ! 7 mixed pine
     >  5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10, ! 8 mixed conifer
     >  5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10/ ! 9 other soft

C     CWXPTS, CCBP & DBHBP ARE ALL USED BY **CWHR** AND ARE NOT UNDER
C     USER CONTROL

C     VALUES OF X-INTERCEPT AND Y-INTERCEPT FOR THE 4 CWHR MODELS
C     X-AXIS IS % CANOPY COVER; Y-AXIS IS % CORRECTED CANOPY COVER
C     THESE CORRESPOND TO SETTING THE HOCKEY-STICK FUNCTION
C     BREAKPOINTS TO 10, 40 & 70%

      DATA (CWXPTS(J,1),J=1,4) /  10.3,  45.0,  88.3, 185.0/
      DATA (CWXPTS(J,2),J=1,4) / 195.0, 225.0, 265.0, 185.0/
      DATA CCBP  / 10.0, 40.0, 70.0 /
      DATA DBHBP / 1.0, 5.0, 11.0, 21.0 /

C     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
C     CLASS. THEY ARE RETURNED WITH THE WEIGHT

      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13/

C     THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
C     VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
C     AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
C     SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
C     SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.

      DATA ITYP / ICLSS * 0 /

C     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C     WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C     COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C     SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

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

C     INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
C     WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C     MUST SUM TO 1, WRT EACH OTHER

      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO

      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)

      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

C     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

      IF (LUSRFM) RETURN

      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)

C     ** BEGIN ROUTINE **

      LDRY = .FALSE.
      IF (IYR .GE. IDRYB .AND. IYR .LE. IDRYE) LDRY = .TRUE.

C     COPY OREGON- (R5) OR CAL-SPECIFIC (R6) FUEL MODEL MATRIX TO MATRIX
C     THAT WILL ACTUALLY BE USED

      IF ((KODFOR .GE. 500 .AND. KODFOR .LT. 600)
     >    .OR. KODFOR .GE. 705) THEN
        DO I = 1,CWHRR
          DO J = 1,CWHRC
            CWHRFMD(I,J) = FMD_R5(I,J)
          ENDDO
        ENDDO
      ELSE
       DO I = 1,CWHRR
          DO J = 1,CWHRC
            CWHRFMD(I,J) = FMD_R6(I,J)
          ENDDO
        ENDDO
      ENDIF

!      IF (IFOR .GE. 6) THEN
!        CWHRFMD => LOC(FMD_R6)
!      ELSE
!        CWHRFMD => LOC(FMD_R5)
!      ENDIF


C     COMPUTE TOTAL STAND BASAL AREA AND PERCENTAGE IN EACH SPECIES

      STNDBA = 0.0
      DO I = 1, MAXSP
        BAPCT(I) = 0.0
        STNDBA = STNDBA + FMTBA(I)
      ENDDO
      IF (STNDBA .GT. 0.001) THEN
        DO I = 1, MAXSP
          BAPCT(I) = 100.0 * (FMTBA(I)/STNDBA)
        ENDDO
      ENDIF

C     CALL CALIFORNIA WILDLIFE HABITAT RATING LOGIC TO CALCULATE
C     STRUCTURAL STAGE/DENSITY CLASSIFICATION. THE RETURNED STRING
C     CODES ARE CONVERTED TO GIVE THE COLUMN (JSS) FOR THE MAPPING
C     TABLE

      CALL CWHR(ISP,DBH,HT,FMICR,FMPROB,CRWDTH,ITRN,
     &    CWXPTS,CCBP,DBHBP,
     &    SZDN,CWHR_MOD,CWHR_WT)

      JSS = FINDJSS(SZDN)

C     FIND THE FOREST TYPE ROWS (IFT) FOR THE FIRE BEHAVIOUR MODEL TABLE

      IFT = 0
      IF (BAPCT(10) .GE. 80.0) THEN
        IFT = 1 ! pine
      ELSE
        IF ((BAPCT(9)+BAPCT(4)) .GE. 80.0) THEN
          IF (BAPCT(9) .GE. BAPCT(4)) THEN
             IFT = 2 ! red fir
          ELSE
            IF (SITEAR(ISISP) .LT. 55.0) THEN
              IFT = 3 ! wh fir E
            ELSE
              IFT = 4 ! wh fir W
            ENDIF
          ENDIF
        ELSE
          IF ((BAPCT(1)+BAPCT(3)) .GE. 80.0) THEN
            IFT = 5 ! Douglas-fir
          ELSEIF ((BAPCT(5)+BAPCT(7)+BAPCT(8)+BAPCT(11)) .GE. 80.0) THEN
            IFT = 6 ! hardwoods
          ELSEIF ((BAPCT(2)+BAPCT(6)) .GE. 80.0) THEN
            IFT = 8 ! mixed conifer
          ELSE
            IF (BAPCT(10) .GT. BAPCT(4)) THEN
              IFT = 7 ! mixed pine
            ELSE
              IFT = 9 ! other softwood
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF (DEBUG) WRITE(JOSTND,2) ICYC,IYR,
     >  FTLABL(IFT),IFT,SSLABL(JSS),JSS
    2 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I4, ' IFT= ',A3,'(',I2,')',
     >  ' JSS= ',A3,'(',I2,')')

C     TAKE FUEL MODEL FROM ROW (IFT) AND COLUMN (JSS) INDEX
C     FAILURE TO FIND A VALID ENTRY: SET FMD=8 AND EXIT CODE

      IF (IFT .GT. 0 .AND. IFT .LE. CWHRR .AND.
     >    JSS .GT. 0 .AND. JSS .LE. CWHRC) THEN
        EQWT(FINDMOD(CWHRFMD(IFT, JSS),IPTR,ICLSS)) = 1.0
      ELSE
        CALL RCDSET (2,.TRUE.)
      ENDIF

C     RESOLVE DYNAMIC FMD ASSIGNMENTS FROM MODIFIED CWHR MODEL
C     CWHR_WT:  WEIGHT OF MODEL TYPE (AT MOST 2 WILL BE RETURNED)
C     CWHR_MOD: DYNAMIC ASSIGNMENT OF CWHR MODEL TYPE
C               10=S-TYPE; 20=P-TYPE; 30=M-TYPE; 40=D-TYPE

      IF (LDYNFM) THEN
        SELECT CASE(SZDN(1:1))
          CASE ("2","3","4","5") ! BLENDING ONLY OCCURS WITH SS 2-5

C           FIND CWHR MODEL COLUMNS FOR THE BLENDED MODEL GROUPS

            DO I=1,2
              IMD(I)  = 0
              IF (CWHR_WT(I) .GT. 0.0) THEN
                SDTMP(1:2) = SZDN(1:2)
                SELECT CASE(CWHR_MOD(I))
                  CASE(10)      ! S MODEL TYPES
                    SDTMP(3:3) = "S"
                  CASE(20)      ! P MODEL TYPES
                    SDTMP(3:3) = "P"
                  CASE(30)      ! M MODEL TYPE
                    SDTMP(3:3) = "M"
                  CASE(40)      ! D MODEL TYPE
                    SDTMP(3:3) = "D"
                END SELECT
                IMD(I) = FINDMOD(CWHRFMD(IFT,FINDJSS(SDTMP)),IPTR,ICLSS)
              ENDIF
            ENDDO

C           SOME EXTRA CARE IS REQUIRED WHEN FMD>13; THESE
C           DON'T HAVE A DIRECT MAPPING INTO THE ARRAY;
C           RATHER: 111 -> 14; 25 -> 15; 26 -> 16

            IF (IMD(1) .EQ. 0 .AND. IMD(2) .EQ. 0) THEN
              EQWT(8) = 1.0
              CALL RCDSET (2,.TRUE.)
            ELSEIF (IMD(1) .NE. 0 .AND. IMD(2) .NE. 0 .AND.
     >             (IMD(1) .NE. IMD(2)) ) THEN
              IF (IMD(1) .NE. IMD(2)) THEN
                DO I=1,2
                  EQWT(IMD(I))= CWHR_WT(I)
                ENDDO
              ENDIF
            ENDIF
        END SELECT
      ENDIF

C     END OF DETAILED LOW FUEL MODEL SELECTION

C     IN CA-VARIANTS (WS,NC,IC) FM11 CAN BE PRESENT WITHOUT ACTIVITIES,
C     VIA THE CWHR TABLE. WHEN THIS OCCURS, FM10/FM11 SHARING IS BYPASSED.

      IF (EQWT(11) .EQ. 0.0) THEN

C     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
C     CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
C     RELATIONSHIP DECLINES FROM PURE 11 INITIALLY, TO PURE 10 AFTER
C     THE PERIOD EXPIRES.

        AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
        IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
          LATFUEL  = .TRUE.
          EQWT(11) = AFWT
          IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
        ENDIF
        IF (.NOT. LATFUEL) AFWT = 0.0
        EQWT(10) = 1.0 - AFWT
      ELSE
        EQWT(10) = 0.0
      ENDIF

      EQWT(12) = 1.0
      EQWT(13) = 1.0

C     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

C     DELAY SHRUB DELAY IN CALIFORNIA VARIANTS

      CALL FMSHRUB (IYR,FMD)

      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)

      RETURN
      END

C     "LOCAL SUBROUTINE"
C     FIND FUEL MODEL NUMBER (FOR LABELLING PURPOSES USUALLY,
C     GIVEN THE INDEX POSITION IN THE IPTR VECTOR. THIS NEED
C     ARISES WHEN FUEL MODEL LABELS DO NOT MATCH THEIR INDEX
C     E.G. MODELS 25 & 26, WHICH ARE IN POSITIONS 14 & 15 IN
C     THIS VARIANT. 8 IS RETURNED IF THE MODEL LABEL IS NOT FOUND,

      INTEGER FUNCTION FINDMOD(JMOD,IPTR,ICLSS)

      INTEGER JMOD, K, IPTR
      DIMENSION IPTR(ICLSS)

      DO K=1,ICLSS
        IF (JMOD .EQ. IPTR(K)) THEN
          GOTO 11
        ENDIF
      ENDDO
      K = 8
   11 CONTINUE
      FINDMOD = K

      RETURN
      END

C     "LOCAL SUBROUTINE"
C     FIND COLUMN IN CWHR FUEL MODEL TABLE, GIVEN STRUCTURAL
C     STAGE/DENSITY STRING

      INTEGER FUNCTION FINDJSS(STR)

      CHARACTER STR*3
      INTEGER   II

      II = 0
      SELECT CASE (STR(1:1))
        CASE("X")
          II = 1 ! Initiating stand: < 150 tpa & <10% CC
        CASE("1")
          II = 1
        CASE("2")
          SELECT CASE (STR(3:3))
            CASE("S")
              II = 2
            CASE("P")
              II = 3
            CASE("M")
              II = 4
            CASE("D")
              II = 5
          END SELECT
        CASE("3")
          SELECT CASE (STR(3:3))
            CASE("S")
              II = 6
            CASE("P")
              II = 7
            CASE("M")
              II = 8
            CASE("D")
              II = 9
          END SELECT
        CASE("4")
          SELECT CASE (STR(3:3))
            CASE("S")
              II = 10
            CASE("P")
              II = 11
            CASE("M")
              II = 12
            CASE("D")
              II = 13
          END SELECT
        CASE("5")
          SELECT CASE (STR(3:3))
            CASE("S")
              II = 14
            CASE("P")
              II = 15
            CASE("M")
              II = 16
            CASE("D")
              II = 17
          END SELECT
        CASE("6")
          II = 18
      END SELECT

      FINDJSS = II

      RETURN
      END
