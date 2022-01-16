      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C UT $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUEMENT DIAM.  THIS GIVES THE PROGRAM THE FLEXIBILITY TO
C  COMPUTE THE GROWTHS NEEDED TO PROCESS THE DIFFERENT
C  CALIBRATION OPTIONS.
C   THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
COMMONS
C----------
C  DIMENSIONS FOR INTERNAL VARIABLES.
C
C     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
C             ARGUEMENT).
C     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
C             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
C             SPECIES).
C    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
C             LARGER TREES TERM IN THE DDS MODEL
C             (ONE COEFFICIENT FOR EACH SPECIES).
C    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
C             COEFFICIENT FOR EACH SPECIES).
C  DGCCFM -- ARRAY CONTAINING THE MAPFOR CCF SITE SPECIES COEFFICIENTS
C            (ONE FOR EACH SPECIES)
C     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
C             EACH SPECIES).
C   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO SQUARED TERM IN THE DDS MODEL (ONE
C             COEFFICIENT FOR EACH SPECIES).
C----------
      LOGICAL DEBUG
      INTEGER IBSERV(5,MAXSP)
      REAL DGFOR(5,MAXSP),DGDS(4,MAXSP),TEMEL,YOUNG,OLD,SUMD2,SUMTRE
      REAL DGSIC(5,MAXSP),DGQMD,STDSDI,RELSDI,ALD,DGLCCF,DGDBLS
      INTEGER IDGSIM(7,MAXSP),ICLS,II
      REAL DGSASP(MAXSP),DGCASP(MAXSP),DGSLOP(MAXSP),DGEL2(MAXSP)
      REAL DGSLSQ(MAXSP),DGCCFA(MAXSP),OBSERV(MAXSP),DGEL(MAXSP)
      INTEGER MAPLOC(6,MAXSP),MAPDSQ(6,MAXSP),ISMAP(MAXSP)
      REAL DIAM(MAXTRE),DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP)
      REAL DGCRSQ(MAXSP),DGPCCF(MAXSP),DGBA(MAXSP),DGDBAL(MAXSP)
      REAL BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,DGPTCC
      REAL SI,D,BARK,BRATIO,DPP,BATEM,DF,DIAGR,DDS,CR,ASPDG,BAL
      REAL XSITE,BAUTBA,ASPTEM,DPROB
      INTEGER ISI,ISC,I1,I2,ISPC,I,LSI,ISIC,I3
C----------
C SPECIES ORDER FOR UTAH VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
C 21=BI, 22=BE, 23=OS, 24=OH
C
C VARIANT EXPANSION:
C GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
C NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM UT
C
C----------
      DATA DGLD/
     &  0.213947,  0.213947,  0.479631,  0.827167, 0.587579,
     &       0.0,  0.587503,  0.587579,  0.833096, 0.733305,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0, 0.889596,
     &  1.024186,       0.0,  0.213947,       0.0/
C
      DATA DGCR/
     &  1.523464,  1.523464,  3.182592, -0.207507, 0.331129,
     &       0.0,  2.148640,  0.331129,  1.422919, 1.315804,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0, 1.732535,
     &  0.459387,       0.0,  1.523464,       0.0/
C
      DATA DGCRSQ/
     &       0.0,       0.0, -1.310144,  1.578941, 0.816301,
     &       0.0, -0.598897,  0.816301,  0.225676, 0.238917,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0/
C
      DATA DGPCCF/
     &       0.0,       0.0, -0.001613, -0.000428,      0.0,
     &       0.0,  -.000467,       0.0,  -.000200, -.002576,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     & -0.000757,       0.0,       0.0,       0.0/
C
      DATA DGBA/
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,-0.000981,
     &       0.0,       0.0,       0.0,       0.0/
C
      DATA DGBAL/
     & -0.358634, -0.358634, -0.707380, -0.010478, -0.399357,
     &       0.0, -0.192073, -0.399357, -0.182808, -0.320124,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.358634,       0.0/
C
      DATA DGDBAL/
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,      0.0,
     &       0.0,       0.0,       0.0,       0.0,-0.001265,
     & -0.010222,       0.0,       0.0,       0.0/
C
      DATA ISMAP/ 1, 1, 2, 6, 4, 5, 1, 4, 3, 7,
     &            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     &            1, 1, 7, 1/
C----------
C  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
C  MODEL BY SPECIES AND SITE CLASS.
C----------
      DATA  IBSERV/
     & 27,70,123,101,33,
     & 27,70,123,101,33,
     & 202,541,323,103,42,
     & 283,45,49,24,12,
     & 586,544,272,95,32,
     & 184,429,356,162,74,
     & 865,1023,372,10,1,
     & 586,544,272,95,32,
     & 203,432,397,126,84,
     & 289,241,41,24,5,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 5*1000,
     & 27,70,123,101,33,
     & 5*1000/
C
      DATA OBSERV/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,     220.0,
     &      78.0,       0.0,       0.0,       0.0/
C----------
C  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
C  HABITAT CLASS.
C----------
      DATA DGCCFA/
     & -0.199592, -0.199592,       0.0, -0.098821, -0.043414,
     &       0.0, -0.407523, -0.043414,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.199592,       0.0/
C----------
C  MAPLOC IS A VECTOR WHICH MAPS FOREST ONTO A LOCATION CLASS.
C----------
      DATA MAPLOC/
     &6*1,
     &6*1,
     &1,2,2,2,3,4,
     &1,2,3,3,4,4,
     &1,1,1,2,3,4,
     &1,1,1,1,1,1,
     &1,1,1,1,2,3,
     &1,1,1,2,3,4,
     &1,2,1,1,1,3,
     &1,2,2,3,3,3,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1,
     &6*1/
C-----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C-----------
      DATA DGFOR/
     &  1.911884,       0.0,       0.0,      0.0,      0.0,
     &  1.911884,       0.0,       0.0,      0.0,      0.0,
     &  0.192136, -0.064516,  0.477698, 0.589169,      0.0,
     & -0.061856, -0.130667, -0.314746, 0.421806,      0.0,
     &  0.011943,  0.265071, -0.094861, 0.796948,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     & -0.256987, -0.425846,  0.530457,      0.0,      0.0,
     &  0.011943,  0.265071, -0.094861, 0.796948,      0.0,
     & -0.467188, -0.638653,  0.116430,      0.0,      0.0,
     &  -0.13235, -0.460129, -0.302309,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     & -0.107648,       0.0,       0.0,      0.0,      0.0,
     & -7.753469,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0,
     &  1.911884,       0.0,       0.0,      0.0,      0.0,
     &       0.0,       0.0,       0.0,      0.0,      0.0/
C----------
C  MAPDSQ IS A VECTOR WHICH MAPS FOREST ONTO A DBH**2
C  COEFFICIENT.
C----------
      DATA MAPDSQ/
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,2,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1,
     &1,1,1,1,1,1/
C-------------
C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
C  SPECIES.
C  ORIGINAL EQN FOR SPECIES 1,2,14 HAD + TERM. CHANGED TO - TO
C  AVOID RUNAWAY DIAMETER GROWTH.  GD. 1/11/00
C-------------
      DATA DGDS/
     & -0.0006538,        0.0,        0.0,        0.0,
     & -0.0006538,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &  -0.000018,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     & -0.0001672,        0.0,        0.0,        0.0,
     & -0.0005345, -0.0006363,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     & -0.0001737,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0,
     & -0.0006538,        0.0,        0.0,        0.0,
     &        0.0,        0.0,        0.0,        0.0/
C----------
C  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
C  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
C  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
C  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
C  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
C  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
C  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.
C----------
      DATA DGCASP /
     & -0.609774, -0.609774,  0.015235,  0.012704, -0.198194,
     &       0.0, -0.168522, -0.198194, -0.232267, -0.411292,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.085958,
     &       0.0,       0.0, -0.609774,       0.0/
      DATA DGSASP/
     & -0.017520, -0.017520,  0.022753, -0.082731, -0.122483,
     &       0.0,  0.128610, -0.122483, -0.192975, -0.287654,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  -0.86398,
     &       0.0,       0.0, -0.017520,       0.0/
       DATA DGSLOP/
     & -2.057060, -2.057060, -0.532905, -1.133123,  0.240433,
     &       0.0,  0.120589,  0.240433,  0.383578,  0.016965,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -2.057060,       0.0/
       DATA DGSLSQ/
     &  2.113263,  2.113263, -0.086518,  1.931351,       0.0,
     &       0.0,  -.266226,       0.0, -0.333955,  2.282665,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  2.113263,       0.0/
      DATA DGEL/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0, -0.075986,
     & -0.012111,       0.0,       0.0,       0.0/
      DATA DGEL2/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.001193,
     &       0.0,       0.0,       0.0,       0.0/
C
C SITE BASE SPECIES REFERENCE
C 1-LP, 2-DF, 3-SAF, 4-ES, 5-ASP, 6-WF, 7-OTH,PP
C
      DATA IDGSIM/
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,2,1,2,1,1,1,
     &1,2,1,1,1,1,2,
     &1,2,2,2,2,2,2,
     &1,1,1,1,1,1,1,
     &1,1,1,2,2,2,2,
     &1,2,2,2,2,2,2,
     &1,1,2,2,3,2,2,
     &1,1,1,1,1,2,3,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1,
     &1,1,1,1,1,1,1/
C
      DATA DGSIC /
     & 0.001766,      0.0,       0.0,      0.0,      0.0,
     & 0.001766,      0.0,       0.0,      0.0,      0.0,
     & 0.010968, 0.006827,  0.199457,      0.0,      0.0,
     & 0.017663, 0.005327,       0.0,      0.0,      0.0,
     & 0.015133, 0.021085,       0.0,      0.0,      0.0,
     & 0.006460,      0.0,       0.0,      0.0,      0.0,
     & 0.021764, 0.027956,       0.0,      0.0,      0.0,
     & 0.015133, 0.021085,       0.0,      0.0,      0.0,
     & 0.004468, 0.008147, -0.015283,      0.0,      0.0,
     & 0.019282, 0.049804,   0.02943,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     & 0.227307,      0.0,       0.0,      0.0,      0.0,
     & 1.965888,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0,
     & 0.001766,      0.0,       0.0,      0.0,      0.0,
     &      0.0,      0.0,       0.0,      0.0,      0.0/
C
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
      IF(DEBUG)WRITE(JOSTND,*) ' DGCON= ', DGCON
C----------
C  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
C  NEEDS; COMPUTE THOSE HERE.
C----------
      AGERNG= 1000.
      YOUNG= 1000.
      OLD= 0.0
C----------
C CALL ARANGE AND BADIST
C----------
      CALL BADIST(DEBUG)
C----------
C FIND AGE DISTRIBUTION
C----------
      DO I= 1,ITRN
        IF(ABIRTH(I).LE.1.)GO TO 5
        IF(HT(I) .LE. 4.5) GO TO 5
        IF(ABIRTH(I) .LT. YOUNG) YOUNG = ABIRTH(I)
        IF(ABIRTH(I) .GT. OLD) OLD = ABIRTH(I)
    5   CONTINUE
C----------
C COMPUTE AGE RANGE.
C----------
        AGERNG = OLD - YOUNG
        AGERNG = ABS(AGERNG)
      ENDDO
C----------
C COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.
C IF THIS PASS THROUGH **DGF** IS DURING CALIBRATION (LSTART = .TRUE.)
C INCLUDE RECENT MORTALITY TREES (IMC(I)=7) IN THE CALCULATION.
C----------
      DSTAG = 1.0
      SUMD2=0.
      SUMTRE=0.
      DGQMD=0.
      IF(ITRN .EQ. 0) GO TO 1001
      DO 1000 II=1,ITRN
        I=IND1(II)
        IF(I.GE.IREC2) GO TO 1000 
        SUMD2=SUMD2+DBH(I)*DBH(I)*PROB(I)
        SUMTRE=SUMTRE+PROB(I)
        IF(DEBUG)WRITE(JOSTND,*)' DGF II,I,DBH,PROB,SUMD2,',
     &  'SUMTRE= ',II,I,DBH(I),PROB(I),SUMD2,SUMTRE
 1000 CONTINUE
C----------
C ADD IN RECENT MORTALITY TREES IF CALIBRATING
C----------
      IF(LSTART) THEN
        DO II = IREC2,MAXTRE
        ISPC = ISP(II)
        DPROB = PROB(II)/(FINT/FINTM)
        IF(DEBUG)WRITE(JOSTND,*)' II,ISPC,IMC,DBH,DPROB= ',
     &  II,ISPC,IMC(II),DBH(II),DPROB
        IF(IMC(II) .EQ. 7) THEN
          SUMD2=SUMD2+DBH(II)*DBH(II)*DPROB
          SUMTRE=SUMTRE+DPROB
          IF(DEBUG)WRITE(JOSTND,*)' DGF II,ISPC,DBH,DPROB,SUMD2,',
     &    'SUMTRE= ',II,ISPC,DBH(II),DPROB,SUMD2,SUMTRE
          ENDIF
        ENDDO
      ENDIF
C
      IF(DEBUG)WRITE(JOSTND,*)' SUMD2,SUMTRE= ',SUMD2,SUMTRE
      DGQMD = SQRT(SUMD2/SUMTRE)
 1001 CONTINUE
      IF(ICYC.GT.0 .AND. BA.GT.0.0) THEN
        IF(DGQMD.GT.0.)THEN
          STDSDI=SUMTRE*(DGQMD/10.0)**1.605
        ELSE
          STDSDI=0.
        ENDIF
        RELSDI=0.
        CALL SDICAL(1,SDIMAX)
        IF(SDIMAX.GT.0.)RELSDI=STDSDI/SDIMAX
        IF(DEBUG)WRITE(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',
     &  STDSDI,SDIMAX,RELSDI
        IF(RELSDI .GT. 0.85) RELSDI = 0.85
        IF(RELSDI .GT. 0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
        IF(DEBUG)WRITE(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
        IF(DEBUG)WRITE(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
      ENDIF
C----------
C  END OF SPECIAL NEEDS SECTION
C----------
C  SCALE BASAL AREA.
C----------
      BA100 = BA/100.
      ISI = INT(SITEAR(ISISP))
      IF(ISI.GT.59)ISI=60
      IF(ISI.LT.20)ISI=20
      ISC=INT(REAL(ISI)/10.)
C----------
C PUT THE SITE IN 10'S INTO A CLASS 1-5
C----------
      ISC=ISC-1
C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      IF(ISC.GE.4 .AND. ISPC.EQ.8) DGCCF(8) = -0.154870
      CONSPP= DGCON(ISPC) + COR(ISPC) +0.01*DGCCF(ISPC)*RELDEN
C
      IF(DEBUG)WRITE(JOSTND,9001)ISPC,DGCON(ISPC),COR(ISPC),
     &DGCCF(ISPC),RELDEN,CONSPP
 9001 FORMAT(' IN DGF 9001 ISPC,DGCON,COR,DGCCF,RELDEN,CONSPP=',
     &I5,5F10.5)
      DGLDS= DGLD(ISPC)
      DGBALS = DGBAL(ISPC)
      DGCRS= DGCR(ISPC)
      DGCRS2=DGCRSQ(ISPC)
      DGDSQS=DGDSQ(ISPC)
      DGPTCC=DGPCCF(ISPC)
      DGDBLS=DGDBAL(ISPC)
      SI=SITEAR(ISPC)
C
      IF(DEBUG)WRITE(JOSTND,9002)ISPC,CONSPP,DGLDS,DGBALS,
     & DGCRS,DGCRS2,DGDSQS,ISC,DGPTCC,DGCCF(ISPC),DGPTCC
 9002 FORMAT(' DGF ISPC',I2,1X,6(E12.4,1X),I5,3E10.4)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 10 I3=I1,I2
      BAL = 0.0
      CR  = 0.0
      I=IND1(I3)
      D=DIAM(I)
      BARK=BRATIO(ISPC,D,HT(I))
      IF (D .LE. 0.0) GO TO 10

      SELECT CASE (ISPC)
C
      CASE(11:16,24)
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BA
        IF(BATEM .LT.  1.0) BATEM = 1.0
        DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM
     &         + 0.00177 * SI
        IF((DF-DPP) .GT. 1.0) DF = DPP + 1.0
CCCC        IF(DF .GT. DBHMAX(IS)) DF = DBHMAX(IS)
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
C
      CASE(6)
        CR = ICR(I)
        CALL DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
        DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
C
      CASE(1:5,7:10,23)
        CR = ICR(I) * 0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA100
        DDS=CONSPP + DGLDS*ALOG(D) + DGBALS*BAL + CR*DGCRS+CR*CR*DGCRS2
     &      +DGDSQS*D*D + DGPTCC*PCCF(ITRE(I))
        IF(ISPC.EQ.5)DDS=DDS*0.95
C
      CASE(20:21)
        ALD=ALOG(D)
        CR=ICR(I)*0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA
        IF(DEBUG)WRITE(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)
        DGLCCF=0.0
        DDS=CONSPP +DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)
     &             +DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))
     &             +DGPTCC*PCCF(ITRE(I))
     &             +DGBA(ISPC)*BA
C
C  USE SPRUCE-FIR MODEL TYPE LOWER BA LIMIT FOR SPECIES USING SURROGATE
C  EQUATIONS FROM THE CR VARIANT
C
      CASE(17:19,22)
        ICLS = IFIX(D + 1.0)
        IF(ICLS .GT. 41) ICLS = 41
        BAUTBA = BAU(ICLS) / BA
        SI = SITEAR(ISPC)
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BA
        IF(BATEM .LT. 5.0) BATEM = 5.0
        IF(ISPC .EQ. 17)THEN
          DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM
     &         + 0.00177 * SI
          IF((DF-DPP) .GT. 1.0) DF = DPP + 1.0
        ELSE
          DF = (1.55986 + 1.01825 * DPP - 0.29342 * ALOG(BATEM)
     &         + 0.00672*SI - 0.00073*BAUTBA)*1.05
        ENDIF
CCCC        IF(DF .GT. DBHMAX(ISPC)) DF = DBHMAX(ISPC)
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(ISTAGF(ISPC).NE.0) DIAGR=DIAGR*DSTAG
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
     &          + COR(ISPC) + DGCON(ISPC)     
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
C
      END SELECT
C
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(.NOT.DEBUG) GO TO 10
      WRITE(JOSTND,9000) I,ISPC,D,BAL,CR,BA,DDS
 9000 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &      ',  BAL=',F7.2,',  CR=',F7.4,
     &      ',  BA=',F9.3,',   LN(DDS),=',F7.4)
   10 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
      RETURN
C
C
      ENTRY DGCONS
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
C------------
C  PUT SITE INTO ONE OF 5 CLASSES
C----------
C**   ISI=SITEAR(ISISP)/100.0
C**   XSITE=SITEAR(ISISP)-ISI*100
      XSITE = SITEAR(ISISP)
      LSI=INT(XSITE/10.)
      IF(LSI.LT.2)ISIC=1
      IF(LSI.GE.5)ISIC=5
      IF(LSI.GE.3 .AND. LSI.LT.4)ISIC=3
      IF(LSI.GE.4 .AND. LSI.LT.5)ISIC=4
      IF(LSI.GE.2 .AND. LSI.LT.3)ISIC=2
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO 30 ISPC=1,MAXSP
      DGCCF(ISPC) = DGCCFA(ISPC)
      ISPFOR=MAPLOC(IFOR,ISPC)
      ISPDSQ=MAPDSQ(IFOR,ISPC)
C----------
C  LOAD DGCON, DGDSQ,  AND ATTEN.   SHIFT ASPECT 45 DEGREES
C  FOR THE DIAMETER INCREMENT MODEL INTERCEPT CALCULATIONS.
C
C  NOTE: 0.7854 IS 45 DEGREES IN RADIANS. THIS CORRECTION IS
C        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
C        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.
C----------
      ISI=ISMAP(ISISP)
      TEMEL=ELEV
      XSITE = SITEAR(ISISP)
      SELECT CASE(ISPC)
      CASE(20)
        IF (TEMEL.GT.30.)TEMEL=30.
        XSITE=ALOG(XSITE)
        ASPTEM=ASPECT
      CASE(21)
        XSITE=ALOG(XSITE)
        ASPTEM=ASPECT
      CASE DEFAULT
        ASPTEM=ASPECT-0.7854
      END SELECT
      DGCON(ISPC)= DGSIC(IDGSIM(ISI,ISPC),ISPC) *XSITE
     &                 + DGFOR(ISPFOR,ISPC)
     &                 +(DGSASP(ISPC) * SIN(ASPTEM)
     &                 + DGCASP(ISPC) * COS(ASPTEM)
     &                 + DGSLOP(ISPC)) * SLOPE
     &                 + DGSLSQ(ISPC) * SLOPE * SLOPE
     &                 + DGEL(ISPC) * TEMEL
     &                 + DGEL2(ISPC) * TEMEL * TEMEL
      DGDSQ(ISPC)=DGDS(ISPDSQ,ISPC)
      SMCON(ISPC)=0.
      SELECT CASE (ISPC)
      CASE(20:21)
        ATTEN(ISPC)=OBSERV(ISPC)
      CASE DEFAULT
        ATTEN(ISPC)=IBSERV(ISIC,ISPC)
      END SELECT
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
      IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC)
     &  + ALOG(COR2(ISPC))
   30 CONTINUE
C
      RETURN
      END
