      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C   **FMCFMD FIRE-WS-DATE OF LAST REVISION:  05/10/12
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
C----------
COMMONS
C
C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
C
COMMONS
C----------

C     LOCAL VARIABLE DECLARATIONS

C     NUMBER OF POTENTIAL FIRE MODELS

      INTEGER ICLSS,CWHRR,CWHRC
      PARAMETER(ICLSS = 15)
      PARAMETER(CWHRR=12)  ! ROWS IN CWHR MATRIX
      PARAMETER(CWHRC=18)  ! COLS IN CWHR MATRIX
      CHARACTER(LEN=3) :: SZDN, SDTMP
      CHARACTER(LEN=2), DIMENSION(0:CWHRR) :: FTLABL
      CHARACTER(LEN=2), DIMENSION(0:CWHRC) :: SSLABL
      LOGICAL   DEBUG, LDRY
      INTEGER   FMD, IFT, JSS
      INTEGER   IYR,I,J
      INTEGER   IPTR(ICLSS), ITYP(ICLSS)
      INTEGER   CWHR_MOD(4), IMD(2)
      INTEGER   CWHRFMD(CWHRR,CWHRC)
      INTEGER   FINDJSS, FINDMOD
      REAL      XPTS(ICLSS,2), EQWT(ICLSS)
      REAL      BAPCT(MAXSP), STNDBA, AFWT, XMCON, XHARD
      REAL      CWHR_WT(4), CWXPTS(4,2), CCBP(3), DBHBP(4)
C----------
C     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
C
C     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
C     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
C     3 = WHITE FIR (WF)                    ABIES CONCOLOR
C     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
C     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
C     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
C     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
C     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
C     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
C    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
C    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
C    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
C    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
C    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
C    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
C    16 = COULTER PINE (CP)                 PINUS COULTERI
C    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
C    18 = MONTEREY PINE (MP)                PINUS RADIATA
C    19 = GRAY PINE (GP)                    PINUS SABINIANA
C         (OR CALIFORNIA FOOTHILL PINE)
C    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
C    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
C    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
C    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
C    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
C    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
C    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
C    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
C    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
C    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
C    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
C    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
C    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
C         (OR CALIFORNIA WHITE OAK)
C    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
C    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
C    35 = GIANT CHINKAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA
C    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
C    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
C    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
C    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
C    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
C    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
C    42 = OTHER SOFTWOODS (OS)
C    43 = OTHER HARDWOODS (OH)
C
C----------
C  DATA STATEMENTS
C----------
C     LABELS FOR DEBUG OUTPUT: FOREST TYPE CODES AND CWHR
C     STRUCTURAL STAGES

      DATA     FTLABL /
     >          "? ", "E ","P ","R ","W ","F ","D ","G ","J ",
     >          "H ","X ","MP","MC" /
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

C     Row labels: "E","P","R","W","F","D","G","J","H","X", "MP", "MC"
C     Col labels: modified from original WS-FFE workshop. Now expanded
C                 so that there is some duplication

      DATA ((CWHRFMD(I,J), J=1,CWHRC), I=1,CWHRR) /
     >  9, 2, 2, 9, 9, 2, 2, 2, 9, 2, 2, 8, 8, 2, 2, 8, 8,10, ! 1 pine E
     >  9, 5, 5, 9, 9,26,26,25, 9,26,26, 8, 8,26,26, 8, 8,10, ! 2 pine W
     >  8, 8, 8, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 3 red fir
     >  8, 8, 8, 8, 8,11,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 4 wh fir E
     >  8, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, ! 5 wh fir W
     >  8, 5, 5, 8, 8, 5, 5, 8, 8,11,11, 9, 8,11,11, 9, 8,10, ! 6 Douglas-fir
     >  8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,10, ! 7 giant sequoia
     >  9, 9, 9, 9, 9, 2, 2, 2, 9, 2, 2, 2, 9, 2, 2, 2, 9,10, ! 8 Jeffrey pine
     >  8, 5, 5, 9, 9,11,11,11, 9, 9, 9, 9, 9, 9, 9, 9, 9,10, ! 9 hardwoods
     >  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10, !10 lodgepole pine
     >  9, 5, 5, 9, 9,26,26,25, 9,26,26, 8, 8,26,26, 8, 8,10, !11 mixed pine
     >  8, 9, 9, 8, 8,26,26,11, 8, 5, 5, 8, 8, 5, 5, 8, 8,10/ !12 mixed conifer

C     CWXPTS, CCBP & DBHBP ARE ALL USED BY **CWHR** AND ARE NOT UNDER
C     USER CONTROL

C     VALUES OF X-INTERCEPT AND Y-INTERCEPT FOR THE 4 CWHR MODELS
C     X-AXIS IS % CANOPY COVER; Y-AXIS IS % CORRECTED CANOPY COVER
C     THESE CORRESPOND TO SETTING THE HOCKEY-STICK FUNCTION
C     BREAKPOINTS TO 25, 40 & 60%

C     VALUES OF X-INTERCEPT AND Y-INTERCEPT FOR THE 4 MODELS

      DATA (CWXPTS(J,1),J=1,4) /  26.8,  42.2,  70.0, 180.0/
      DATA (CWXPTS(J,2),J=1,4) / 187.5, 435.8, 350.0, 180.0/
      DATA CCBP  / 25.0, 40.0, 60.0 /
      DATA DBHBP / 1.0, 6.0, 11.0, 24.0 /

C     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
C     CLASS. THEY ARE RETURNED WITH THE WEIGHT

      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,25,26 /

c     These are 0 for regular lines, -1 for horizontal and 1 for
c     vertical lines. If any of the lines defined by xpts() are of
c     an unusual variety, this must be entered here so that
c     special logic can be invoked.  In this case, all the line
c     segments have a |slope| that is > 0 and less than inif.

      DATA ITYP / ICLSS * 0 /

C     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C       WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C       COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C       SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

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
     >  45.,100.,   ! FMD  13
     >   5., 15.,   ! FMD  25
     >   5., 15. /  ! FMD  26
C----------
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
C----------
C  TALLY UP THE BASAL AREA IN MIXED CONIFERS AND HARDWOODS
C  SOFTWOODS CONSIDERED OUTSIDE MIXED CONIFERS ARE: 
C    2=DF, 3=WF, 4=GS, 6=JP, 7=RF, 8=PP, 9=LP
C    (NOT SURE IF 22=BD SHOULD BE IN WITH DF, OR 23=RW SHOULD BE IN WITH GS
C     THESE ARE BOTH GOING TO MIXED CONIFERS RIGHT NOW)
C  OLD BLACK OAK AND TANOAK/CHINKAPIN GROUPS ARE NOW IN HARDWOODS
C----------
      XMCON  = 0.0 ! mixed conifers
      XHARD  = 0.0 ! total hardwoods
      DO I = 1,MAXSP
        SELECT CASE (I)
        CASE(1,5,10:27,42)
          XMCON = XMCON + BAPCT(I)
        CASE(28:41,43)
          XHARD = XHARD + BAPCT(I)
        END SELECT
      ENDDO

C     FIND THE FOREST TYPE ROWS (IFT) FOR THE FIRE BEHAVIOUR MODEL TABLE

      IFT = 0
      IF (BAPCT(8) .GE. 80.0) THEN
        IF (SITEAR(ISISP) .LT. 55.0) THEN
          IFT = 1 ! E
        ELSE
          IFT = 2 ! P
        ENDIF
      ELSE
        IF ((BAPCT(7)+BAPCT(3)) .GE. 80.0) THEN
          IF (BAPCT(7) .GE. BAPCT(3)) THEN
             IFT = 3 ! R
          ELSE
            IF (SITEAR(ISISP) .LT. 55.0) THEN
              IFT = 4 ! W
            ELSE
              IFT = 5 ! F
            ENDIF
          ENDIF
        ELSE
          IF (BAPCT(2) .GE. 80.0) THEN
            IFT = 6 ! D
          ELSEIF (BAPCT(4) .GE. 80.0) THEN
            IFT = 7 ! G
          ELSEIF (BAPCT(6) .GE. 80.0) THEN
            IFT = 8 ! J
          ELSEIF (XHARD .GE. 80.0) THEN
            IFT = 9 ! H
          ELSEIF (BAPCT(9) .GE. 80.0) THEN
            IFT = 10 ! X
          ELSEIF (XMCON .GE. 80.0) THEN
            IFT = 12 ! MC
          ELSE
            IF (BAPCT(8) .GE. BAPCT(3)) THEN
              IFT = 11 ! MP
            ELSE
              IFT = 12 ! MC
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF (DEBUG) WRITE(JOSTND,2) ICYC,IYR,
     >  FTLABL(IFT),IFT,SSLABL(JSS),JSS
    2 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I4, ' IFT= ',A3,'(',I2,')'
     >  ' JSS= ',A3,'(',I2,')')

C     TAKE FUEL MODEL FROM ROW (IFT) AND COLUMN (JSS) INDEX
C     FAILURE TO FIND A VALID ENTRY: SET FMD=8 AND EXIT CODE

      IF (IFT .GT. 0 .AND. IFT .LE. CWHRR.AND.
     >    JSS .GT. 0 .AND. JSS .LE. CWHRC) THEN
        EQWT(FINDMOD(CWHRFMD(IFT, JSS),IPTR,ICLSS)) = 1.0
      ELSE
        EQWT(8) = 1.0
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
C     STAGE/DENSITY STRING OF THE FORM "2,P"

      INTEGER FUNCTION FINDJSS(STR)

      CHARACTER STR*3
      INTEGER   II

      II = 0
      SELECT CASE (STR(1:1))
        CASE("X","1")  ! Initiating stand: < 150 tpa & <10% CC
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
