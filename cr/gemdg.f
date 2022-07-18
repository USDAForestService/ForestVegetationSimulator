      SUBROUTINE GEMDG(DDS,IMODTY,IS,BAUTBA,SPBA,SI,DP,BAT,BARK,
     &CR,SLOPE,ASPECT,ELEV,PBAL,PCCFI,RELDEN,BAL)
      IMPLICIT NONE
C----------
C CR $Id$
C----------
C   THIS SUBROUTINE CALCULATES DDS. IT IS CALLED FROM DGF.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C----------
      REAL BAL,RELDEN,PCCFI,PBAL,ELEV,SLOPE,CR,BARK,BAT,DP
      REAL SI,SPBA,BAUTBA,DDS,DPP,BATEM,BGTTBA,BAL100
      INTEGER IS,IMODTY,IDDS
      REAL ASPECT,DF,CON1,CON2,DFMCPP,DFSWPP,ADJB,BAPP,BARAT,ADJR
      REAL ADJ,SIMINR,XSLOPE,DIAGR      
      REAL RDANUW
C----------
C  SPECIES ORDER:
C   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
C  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
C  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
C  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
C
C  SPECIES EXPANSION:
C  UJ,AJ,RM,OJ,ER USE CR JU                              
C  NC,PW USE CR CO
C  GO,AW,EM,BK,SO USE CR OA                             
C  PB USES CR AS                              
C  PM,PD,AZ USE CR PI
C  CI USES CR PP                              
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = ELEV
C----------
C  SET APPROPRIATE BOUNDS BY MODEL TYPE.
C----------
        DPP = DP
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BAT
        BGTTBA = BAUTBA
        IDDS=0
        BAL100=BAL/100.
C----------
C SOUTHWEST MIXED CONIFERS TYPE
C----------
      IF(IMODTY .EQ. 1) THEN
        IF(BATEM .LT. 65.0) BGTTBA = 0.0
        IF(BATEM .LT.  1.0) BATEM = 1.0
C----------
C SOUTHWEST PONDEROSA PINE TYPE
C----------
      ELSEIF(IMODTY .EQ. 2) THEN
        IF(BATEM .LT. 21.0) BATEM = 21.0
C----------
C BLACK HILLS PONDEROSA PINE TYPE
C----------
      ELSEIF(IMODTY .EQ. 3) THEN
        IF(BATEM .LT. 5.0) BATEM=5.0
C----------
C SPRUCE-FIR TYPE
C----------
      ELSEIF(IMODTY .EQ. 4) THEN
        IF(BATEM .LT. 5.0) BATEM = 5.0
C----------
C  LODGEPOLE PINE TYPE
C----------
      ELSEIF(IMODTY .EQ. 5) THEN
        IF(BATEM .LT. 14.0) BATEM = 14.0
      ENDIF
C----------
C  BRANCH TO EQUATIONS FOR APPROPRIATE SPECIES.
C----------
      SELECT CASE (IS)
C----------
C  SUB-ALPINE FIR
C  CORKBARK FIR
C
C  SWMC,SWPP USE CORKBARK EQN FOR GENGYM SWMC
C  OTHER MODEL TYPES USE AF/CB EQN FROM GENGYM S-F & LP
C----------
      CASE(1:2)
        IF(IMODTY .LE. 2) THEN
          DF = 0.50283 + 0.93999 * DPP - 0.0016822 * BAT + 0.00791
     &         * SI + 0.16235 * (ALOG(DPP))**2.0
        ELSE
          DF = 2.19459 + 0.94659 * DPP - 0.20410 * BGTTBA
     1         - 0.35970 * ALOG(BATEM) + 0.11326 * ((ALOG(DPP))**2.0)
     2         + 0.00615 * SI
        ENDIF
C----------
C  DOUGLAS-FIR
C----------
      CASE(3)
        DF = 0.58234 + 0.94542 * DPP - 0.0025092 * BAT
     &       + 0.0070474 * SI - 0.14044 * BGTTBA + 0.15738 *
     &       (ALOG(DPP))**2.0
C----------
C  GRAND FIR
C  EQN FROM CENTRAL IDAHO VARIANT, HABITAT TYPE 520 (ABGR/CLUN)
C  SAWTOOTH NATIONAL FOREST, ELEV=6000 FT
C----------
      CASE(4)
        DDS = -0.057982 + 0.977362 + 0.005045*60.
     &        - 0.000093*60. *60.  + 0.009335*SIN(ASPECT)*SLOPE
     &        - 0.004469*COS(ASPECT)*SLOPE - 0.033374*SLOPE
     &        - 0.418343*SLOPE*SLOPE
     &        + 1.286963*ALOG(DPP) - 0.217923*ALOG(BAT)
     &        + 1.175105*CR + 0.219013*CR*CR - 0.0004408*DPP*DPP
     &        - 0.000578*PBAL/ALOG(DPP+1) - 0.000512*PCCFI
        IDDS = 1
C----------
C  WHITE FIR
C----------
      CASE(5)
        DF = 0.46172 + 1.04089 * DPP - 0.0019984 * BAT + 0.0065947
     &       * SI - 0.09197 * BGTTBA - 0.0011355 * DPP * DPP
C----------
C  MOUNTAIN HEMLOCK
C  MH EQUATION FROM NORTH IDAHO VARIANT, HABITAT TYPE 710 (TSME/XETE),
C  BITTERROOT NATIONAL FOREST, ELEV=6000 FT
C----------
      CASE(6)
        DDS = -1.52111 + 0.08518*60.
     &        - 0.000943*60. *60.  + 0.13363*SIN(ASPECT)*SLOPE
     &        + 0.17935*COS(ASPECT)*SLOPE + 0.07628*SLOPE
     &        + 0.89778*ALOG(DPP) - 0.10744*0.01*RELDEN
     &        + 1.28403*CR - 0.000484*DPP*DPP
     &        - 0.66110*BAL100/ALOG(DPP+1)
        IDDS = 1
C----------
C  WESTERN RED CEDAR
C  EQN FROM NI VARIANT, HABITAT TYPE 550 (THPL/OPHO)
C  ELEVATION = 6000 FT
C----------
      CASE(7)
        DDS =  1.61452 - 0.00175*60.
     &        - 0.000067*60. *60.  + 0.05534*SIN(ASPECT)*SLOPE
     &        - 0.06625*COS(ASPECT)*SLOPE + 0.11931*SLOPE
     &        + 0.58705*ALOG(DPP) - 0.15356*0.01*RELDEN
     &        + 0.74596 * BAL100
     &        + 1.29360*CR
     &        - 2.28375*BAL100/ALOG(DPP+1)
        IDDS = 1
C----------
C  WESTERN LARCH
C  EQN FROM NI VARIANT, 260 HABITAT TYPE, BITTERROOT NF
C  ELEV = 6000 FT
C----------
      CASE(8)
        DDS =  0.51291 + 0.20004 + 0.03730*60.
     &        - 0.000433*60. *60.  + 0.03430*SIN(ASPECT)*SLOPE
     &        - 0.21337*COS(ASPECT)*SLOPE + 0.33523*SLOPE
     &        - 0.70216*SLOPE*SLOPE - 0.05438*0.01*RELDEN
     &        + 0.54140*ALOG(DPP) + 0.43637 * BAL100
     &        + 1.03478*CR + 0.07509*CR*CR - 0.000310*DPP*DPP
     &        - 2.03256*BAL100/ALOG(DPP+1)
        IDDS = 1
C----------
C  LIMBER PINE
C  EQN FROM UT VARIANT
C  ORIGINAL EQN HAD + D-SQUARED TERM, CHANGED TO - TO AVOID
C  RUNAWAY DIAMETER GROWTH  GD 01/11/00
C----------
      CASE(10)
        CON1=SIN(ASPECT-0.7854)*SLOPE*(-0.01752)
        CON2=COS(ASPECT-0.7854)*SLOPE*(-0.609774)
        DDS =  1.911884 + 0.001766*SI
     &        + CON1
     &        + CON2 - 2.05706*SLOPE
     &        + 2.113263*SLOPE*SLOPE - 0.199592*0.01*RELDEN
     &        + 0.213947*ALOG(DPP) - 0.358634 * BAL100
     &        + 1.523464*CR - 0.0006538*DPP*DPP
        IDDS = 1
C----------
C  LODGEPOLE PINE
C----------
      CASE(11)
        DF = 1.32652 + 0.96279 * DPP - 0.17138 * BGTTBA
     &       - 0.21827 * ALOG(BATEM) + 0.06298 * ((ALOG(DPP))**2.0)
     &       + 0.00590 * SI
C----------
C  BRISTLECONE PINE
C  LIMBER PINE
C  PINYON PINE, SINGLELEAF PINYON, BORDER PINYON, ARIZONA PINYON PINE
C  UTAH JUNIPER, ALLIGATOR JUNIPER, ROCKY MTN JUNIPER, ONESEED JUNIPER
C  EASTERN REDCEDAR
C  GAMBEL OAK, ARIZONA WHITE OAK, EMORY OAK, BUR OAK, SILVERLEAF OAK
C  OTHER SOFTWOODS
C----------
      CASE(9,12,16,23:27,29:35,37)
        DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM
     &       + 0.00177 * SI
        IF((DF-DPP) .GT. 1.0) DF = DPP + 1.0
C----------
C  PONDEROSA PINE, CHIHUAHUA PINE
C----------
      CASE(13,36)
C
C PONDEROSA PINE   --- SWMC TYPE
C
        IF(IMODTY .EQ. 1)THEN
          DFMCPP = 0.26265 + 0.94001 * DPP - 0.0016405 * BAT
     &             + 0.0060074 * SI + 0.16328 * (ALOG(DPP))**2.0
          BGTTBA = BAUTBA
          BATEM = BAT
          IF(BATEM .LT. 21.0) BATEM = 21.0
          DFSWPP = 4.10552 + 0.88872 * DPP
     &    -0.83531 * ALOG(BATEM) + 0.013781 * SI - 0.52784 * BGTTBA
     &    + 0.23834 * (ALOG(DP))**2.0
          ADJB = 0.0
          IF(BAT .GT. 60.0 .AND. BAT .LT. 100.0) THEN
            ADJB = -0.025 * BAT + 2.5
          ELSEIF(BAT .LT. 60.1) THEN
            ADJB = 1.0
          ENDIF
          BAPP = SPBA
          IF(BAPP .LT. 1.0) BAPP = 1.0
          BARAT = BAPP / BAT
          ADJR = 0.0
          IF(BARAT .GT. 0.3 .AND. BARAT .LT. 0.5) THEN
            ADJR = 5.0 * BARAT - 1.5
          ELSEIF(BARAT .GT. 0.499) THEN
            ADJR = 1.0
          ENDIF
          ADJ = AMAX1(ADJB,ADJR)
          DF = (1.0 - ADJ) * DFMCPP + ADJ * DFSWPP
C
C PONDEROSA PINE  --- SWPP TYPE
C
        ELSEIF (IMODTY .EQ. 2) THEN
          DF = 4.10552 + 0.88872 * DPP
     1     - 0.83531 * ALOG(BATEM) + 0.013781 * SI
     2     - 0.52784 * BGTTBA + 0.23834 * (ALOG(DPP))**2.0
C
C PONDEROSA PINE -- BLACK HILLS PONDEROSA PINE TYPE
C
        ELSEIF (IMODTY .EQ. 3) THEN
          DF = 2.50428 + 0.94573 * DPP - 0.45765 * ALOG(BATEM)
     &    + 0.01354*SI - 0.46350*BGTTBA + 0.05940*(ALOG(DPP))**2.0
          IF(AGERNG.GT.40. .AND. BGTTBA.GT.0.3) THEN
            SIMINR = 0.57593 + 0.92561*SI
            IF(BATEM .LT. 21.) BATEM = 21.
            DFSWPP = 4.10552 + 0.88872 * DPP
     &       - 0.83531 * ALOG(BATEM) + 0.013781 * SIMINR
     &       - 0.52784 * BGTTBA + 0.23834 * (ALOG(DPP))**2.0
            IF(DF .GT. DFSWPP) THEN
              IF(BGTTBA .GE. 0.5) THEN
                DF = DFSWPP
                IF(DF.LT.(DPP+.1))DF=DPP+.1      
              ELSE
                DF = (2.5 - 5.0 * BGTTBA) * DF
     &                + (-1.5 + 5.0 * BGTTBA) * DFSWPP
              ENDIF
            ENDIF
          ENDIF
C
C  PONDEROSA PINE  --- LODGEPOLE PINE TYPE
C  ALSO USED FOR SPRUCE-FIR MODEL TYPE
C
        ELSE
          DF = 2.50428 + 0.94573 * DPP - 0.45765 * ALOG(BATEM)
     1     + 0.01354*SI - 0.46350*BGTTBA + 0.05940*(ALOG(DPP))**2.0
        ENDIF
C----------
C  WHITEBARK PINE
C  EQN FROM EM VARIANT, HABITAT TYPE 850
C  ELEV = 8000 FEET
C----------
      CASE(14)
        XSLOPE = SLOPE/10.
        DDS =  0.01545 + 1.5675 - 0.00565*80.
     &        - 0.01606*SIN(ASPECT)*XSLOPE
     &        + 0.00270*COS(ASPECT)*XSLOPE - 0.20011*XSLOPE
     &        + 0.80110*ALOG(DPP) + 0.00064 * BAL
     &        + 1.02878*CR - 0.45448*CR*CR
     &        - 0.00328*BAL/ALOG(DPP+1) - 0.25717*ALOG(RELDEN)
        IDDS = 1
C----------
C  WESTERN WHITE PINE
C----------
      CASE(15)
        DF = 0.89451 + 0.95435 * DPP - 0.0012183 * BAT + 0.00159
     &       * SI - 0.20815 * BGTTBA + 0.09987 * (ALOG(DPP))**2.0
C----------
C  BLUE SPRUCE
C----------
      CASE(17)
        DF = 0.50941 + 1.02697 * DPP - 0.0035783 * BAT
     &       + 0.01022 * SI - 0.00098825 * DPP * DPP
C----------
C  ENGELMANN SPRUCE
C  SWMC & SWPP USE SWMC EQN
C  ALL OTHER MODEL TYPES USE S-F EQN
C----------
      CASE(18)
        IF(IMODTY .LE. 2) THEN
          DF = 0.67074 + 1.03068 * DPP - 0.0020055 * BAT
     &         + 0.00633 * SI - 0.0010875 * DPP * DPP
        ELSE
          DF = 2.28652 + 0.94475 * DPP - 0.48056 * BGTTBA
     1         - 0.41632 * ALOG(BATEM) + 0.10778 * ((ALOG(DPP))**2.0)
     2         + 0.01295 * SI
        ENDIF
C----------
C  WHITE SPRUCE
C----------
      CASE(19)
        DF = 2.94734 + 0.88423 * DPP - 0.64449 * ALOG(BATEM)
     &       + 0.01207 * SI + 0.25363 * (ALOG(DPP))**2.0
C----------
C  ASPEN, PAPER BIRCH
C  NARROWLEAF COTTONWOOD, PLAINS COTTONWOOD
C  OTHER HARDWOODS
C
C  SWMC AND SWPP USE SWMC EQN
C  ALL OTHER MODEL TYPES USE ASPEN MODEL TYPE EQN
C----------
      CASE(20,21:22,28,38)
        IF(IMODTY .LE. 2) THEN
          DF = 0.24506 + 1.01291 * DPP - 0.00084659 * BAT
     &         + 0.00631*SI
        ELSE
          DF = 1.55986 + 1.01825 * DPP - 0.29342 * ALOG(BATEM)
     &         + 0.00672*SI - 0.00073*BGTTBA
        ENDIF
C
C DIAMETER GROWTH BEING UNDERESTIMATED, INCREASE ACCORDINGLY.
C
        DF = DF * 1.05
C----------
C  PLACE FOR OTHER SPECIES
C----------
      CASE DEFAULT
        DDS=-9.21
        IDDS=1
C
      END SELECT
C----------
C  IF DIRECT LN(DDS) ESTIMATE, FIRST CONVERT TO DG ESTIMATE,
C  THEN APPLY DSTAG REDUCTION IF CALLED FOR.  IF DIRECT DG
C  ESTIMATE, APPLY DSTAG REDUCTION IF CALLED FOR. IN BOTH CASES,
C  CONVERT TO LN(DDS) SCALE BEFORE EXITING.
C----------
      IF(IDDS .GT. 0) THEN
        DIAGR = SQRT(EXP(DDS)+((DPP*BARK)**2.0))-DPP*BARK
      ELSE
        IF(DF .GT. DBHMAX(IS)) DF = DBHMAX(IS)
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
      ENDIF
      IF(ISTAGF(IS).NE.0) DIAGR=DIAGR*DSTAG
C----------
C  TEST RUNS SHOW BH PP GROWTH TO HIGH, REDUCE.  DIXON 3-10-93.
C----------
      IF(IMODTY.EQ.3 .AND. (IS.EQ.13 .OR. IS.EQ.36)) DIAGR=DIAGR*0.80
C
      IF(DIAGR .LE. 0.) THEN
        DDS=-9.21
      ELSE
        DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
        IF(DDS .LT. -9.21) DDS=-9.21
      ENDIF
      RETURN
      END
