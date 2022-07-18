      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C CI $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
C  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
C  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
C  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
C
C
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
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'CICOM.F77'
C
C
COMMONS
C----------
C  DIMENSIONS FOR INTERNAL VARIABLES.
C
C     DIAM -- ARRAY OF TREE DIAMETERS (PASSED AS AN ARGUMENT).
C    DGLBA -- COEFFICIENTS FOR LOG(BASAL AREA)
C     DGBA -- COEFFICIENTS FOR BASAL AREA
C    DGBAL -- COEFFICIENTS FOR BASAL AREA IN LARGER TREES
C     DGLD -- COEFFICIENTS FOR LOG(DIAMETER)
C     DGCR -- COEFFICIENTS FOR CROWN RATIO
C   DGCRSQ -- COEFFICIENTS FOR CROWN RATIO SQUARED
C     DGEL -- COEFFICIENTS FOR ELEVATION
C    DGEL2 -- COEFFICIENTS FOR ELEVATION SQUARED
C   DGCASP -- COEFFICIENTS FOR COSINE(ASPECT)*SLOPE
C   DGSASP -- COEFFICIENTS FOR SINE(ASPECT)*SLOPE
C   DGSLOP -- COEFFICIENTS FOR SLOPE
C   DGSLSQ -- COEFFICIENTS FOR SLOPE SQUARED
C   DGDBAL -- COEFFICIENTS FOR INTERACTION
C             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH+1)
C   DGPCCF -- COEFFICIENTS FOR POINT CROWN COMPETETION FACTOR
C   DGCCFA -- COEFFICIENTS FOR STAND CROWN COMPETETION FACTOR
C   DGHAB  -- CONSTANTS FOR HABITAT TYPE
C   DGFOR  -- CONSTANTS FOR FOREST LOCATION
C    DGDS  -- COEFFICIENTS FOR DIAMETER SQUARED
C  MAPLOC  -- MAPPING FUNCTION FOREST --> FOREST LOCATION CONSTANT
C  OBSERV  -- NUMBER OF OBSERVATIONS USED TO DEVELOP THE SPECIES
C             SPECIFIC EQUATION.
C  ICHBCL  -- MAPPING FUNCTION HABITAT TYPE --> HAB TYPE CONSTANT
C----------
      LOGICAL DEBUG
      INTEGER MAPLOC(6,MAXSP),OBSERV(MAXSP),ICHBCL(130,MAXSP)
      INTEGER I,I1,I2,I3,IPCCF,MAPHAB,ISPC
      INTEGER IBSERV(6,3),LSI,ISIC,ICLS
      REAL DIAM(MAXTRE),DGLD(MAXSP),DGCR(MAXSP),DGCRSQ(MAXSP),
     &   DGDBAL(MAXSP),DGHAB(12,MAXSP),DGFOR(3,MAXSP),DGDS(MAXSP),
     &   DGEL2(MAXSP),DGSASP(MAXSP),DGCASP(MAXSP),DGSLOP(MAXSP),
     &   DGLBA(MAXSP),DGBA(MAXSP),DGPCCF(MAXSP),DGEL(MAXSP),
     &   DGSLSQ(MAXSP),DGBAL(MAXSP),DGCCFA(MAXSP)
      REAL CONSPP,DGLDS,DGCRS,DGDSQS,DGDBLS,DGLBAS,DGPCFS,DGBAS
      REAL D,CR,BAL,PBAL,DDS,DHAB,DGCRS2,BRATIO
      REAL TMPASP,XSITE,TEMEL,DGBALS,BARK,SI,ASPDG,DPP,DF,DIAGR,BATEM
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
C----------
      DATA (ICHBCL(I,1),I=1,130)/
     & 12*1,2*2,5*3,5*4,2*3,4,10*0,5*4,18*0,5*5,6*6,4*4,55*0/
      DATA (ICHBCL(I,2),I=1,130)/
     & 69*1,61*0/
      DATA (ICHBCL(I,3),I=1,130)/
     & 37*1,9*2,14*3,2*2,2*1,3,1,5,2*3,2*6,4*8,27*7,28*0/
      DATA (ICHBCL(I,4),I=1,130)/
     & 62*0,2*1,0,3,4,1,3,2*5,2*3,2*6,55*0/
      DATA (ICHBCL(I,5),I=1,130)/
     & 62*0,2*1,0,3,4,1,3,2*5,2*3,2*6,55*0/
      DATA (ICHBCL(I,6),I=1,130)/
     & 69*1,61*0/
      DATA (ICHBCL(I,7),I=1,130)/
     & 60*1,4*10,2,5,3,6*5,11*6,14*7,2*8,9,8,5*11,3*9,20*0/
      DATA (ICHBCL(I,8),I=1,130)/
     & 61*0,4*1,2,3,2*4,6*5,5*3,4*5,11*3,35*0/
      DATA (ICHBCL(I,9),I=1,130)/
     & 65*1,4*2,6*1,5*0,6*1,9*0,2*1,0,1,0,4,1,9*4,2*1,2*0,3*1,12*0/
      DATA (ICHBCL(I,10),I=1,130)/
     & 12*1,2*2,5*3,5*4,2*3,4,10*0,5*4,18*0,5*5,6*6,4*4,55*0/
      DATA (ICHBCL(I,12),I=1,130)/
     & 130*0/
      DATA (ICHBCL(I,13),I=1,130)/
     & 130*0/
      DATA (ICHBCL(I,14),I=1,130)/
     & 130*0/
      DATA (ICHBCL(I,15),I=1,130)/
     & 130*0/
      DATA (ICHBCL(I,16),I=1,130)/
     & 130*0/
      DATA (ICHBCL(I,17),I=1,130)/
     & 130*0/
      DATA (ICHBCL(I,18),I=1,130)/
     & 60*1,4*10,2,5,3,6*5,11*6,14*7,2*8,9,8,5*11,3*9,20*0/
      DATA (ICHBCL(I,19),I=1,130)/
     & 130*0/
C----------
      DATA DGLBA/
     &  -.257322,  -.121785,  -.196025,  -.217923,  -.217923,
     &  -.121785,  -.178038,  -.204602,  -.113511,  -.257322,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  -.178038,        0./
C----------
      DATA DGBA/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,  -.000926,       0.0,
     &       0.0,       0.0,       0.0,       0.0, -0.001265,
     &       0.0,       0.0,       0.0,       0.0/
C----------
      DATA DGLD/
     &   .822203,   .725693,  1.045093,  1.286963,  1.286963,
     &   .725693,   .900528,  1.241561,   .968212,   .822203,
     &  0.213947,  0.213947,       0.0,       0.0,  0.889596,
     &  0.213947,       0.0,   .900528,       0.0/
C----------
      DATA DGCR/
     &  1.768935,  2.362259,  1.658968,  1.175105,  1.175105,
     &  2.362259,  1.913105,   .506551,   .626180,  1.768935,
     &  1.523464,  1.523464,       0.0,       0.0,  1.732535,
     &  1.523464,       0.0,  1.913105,       0.0/
C----------
      DATA DGCRSQ/
     &  -.176164, -1.719086,  -.361716,   .219013,   .219013,
     & -1.719086,  -.258206,   .762026,   .688924,  -.176164,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  -.258206,       0.0/
C----------
      DATA DGEL/
     &       0.0,  -.000064,   .003527,   .005045,   .005045,
     &  -.000064,   .006343,       0.0,   .010618,       0.0,
     &       0.0,       0.0,       0.0,       0.0, -0.075986,
     &       0.0,       0.0,   .006343,       0.0/
C----------
      DATA DGEL2/
     &       0.0,  -.000012,  -.000095,  -.000093,  -.000093,
     & -0.000012,  -.000096,       0.0,  -.000194,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.001193,
     &       0.0,       0.0,  -.000096,       0.0/
C----------
      DATA DGCASP/
     &   .127311,  -.120816,   .059683,  -.004469,  -.004469,
     &  -.120816,   .081123,   .013843,  -.137792,   .127311,
     & -0.609774, -0.609774,       0.0,       0.0,  0.085958,
     & -0.609774,       0.0,   .081123,       0.0/
C----------
      DATA DGSASP/
     &   .076531,   .008765,   .023297,   .009335,   .009335,
     &   .008765,   .066831,  -.157633,  -.073830,   .076531,
     &  -0.01752,  -0.01752,       0.0,       0.0,  -0.86398,
     &  -0.01752,       0.0,   .066831,       0.0/
C----------
      DATA DGSLOP/
     &   .024336,  -.293028,  -.133723,  -.033374,  -.033374,
     &  -.293028,   .000452,  1.271265,   .161578,   .024336,
     &  -2.05706,  -2.05706,       0.0,       0.0,       0.0,
     &  -2.05706,       0.0,   .000452,       0.0/
C----------
      DATA DGSLSQ/
     &  -.781480,       0.0,  -.484626,  -.418343,  -.418343,
     &       0.0,  -.207088, -1.842644,  -.050890,  -.781480,
     &  2.113263,  2.113263,       0.0,       0.0,       0.0,
     &  2.113263,       0.0,  -.207088,       0.0/
C----------
      DATA DGDBAL/
     &  -.006065,  -.003403,  -.002579,  -.000578,  -.000578,
     &  -.003403,  -.001641,  -.000638,       0.0,  -.006065,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  -.001641,       0.0/
C----------
      DATA DGPCCF/
     &       0.0,  -.000521,  -.000422,  -.000512,  -.000512,
     &  -.000521,       0.0,  -.000210,  -.000601,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0/
C----------
      DATA DGBAL/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     & -0.358634, -0.358634,       0.0,       0.0,       0.0,
     & -0.358634,       0.0,       0.0,       0.0/
C----------
      DATA DGCCFA/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     & -0.199592, -0.199592,       0.0,       0.0,       0.0,
     & -0.199592,       0.0,       0.0,       0.0/
C----------
      DATA  OBSERV/
     &     8420,     3091,    16232,    21277,    21277,
     &     3091,    20986,     5517,     6533,     8420,
     &        0,        0,        0,        0,      220,
     &        0,        0,    20986,        0/
C----------
      DATA IBSERV/
C ROW 1 USED FOR: 11=WB, 12=PY, 16=LM
     &     27,    70,   123,   101,    33,     0,
C ROW 2 USED FOR: 13=AS
     &    184,   429,   356,   162,    74,     0,
C ROW 3 USED FOR: 14=WJ, 17=CW AND 19=OH
     &   1000,  1000,  1000,  1000,  1000,  1000/
C----------
C  SPECIES 1
      DATA (DGHAB(I,1),I=1,12)/
     &       0.0,  0.006074,  0.181590, -0.196098, -0.055780, 
     &  0.133907,  0.045857,     5*0.0/
C  SPECIES 2
      DATA (DGHAB(I,2),I=1,12)/
     &       0.0,  0.098727, -0.066114,  0.174716,     8*0.0/
C  SPECIES 3
      DATA (DGHAB(I,3),I=1,12)/
     &       0.0,  0.143953,  0.054519,  0.216254, -0.160600,
     &  0.085439,  0.337503, -0.041782,  0.016502,     3*0.0/
C  SPECIES 4
      DATA (DGHAB(I,4),I=1,12)/
     &       0.0, -0.091074,  0.051189, -0.057982, -0.157537,
     &  0.094401, -0.198203,     5*0.0/
C  SPECIES 5
      DATA (DGHAB(I,5),I=1,12)/
     &       0.0, -0.091074,  0.051189, -0.057982, -0.157537,
     &  0.094401, -0.198203,     5*0.0/
C  SPECIES 6
      DATA (DGHAB(I,6),I=1,12)/
     &       0.0,  0.098727, -0.066114,  0.174716,     8*0.0/
C  SPECIES 7
      DATA (DGHAB(I,7),I=1,12)/
     &       0.0,  0.155479,  0.255922,  0.543851,  0.468293,
     &  0.412304,  0.367085,  0.171232,  0.262057,  0.048266,
     &  0.301184,  0.542920/
C  SPECIES 8
      DATA (DGHAB(I,8),I=1,12)/
     &       0.0,  0.108200,  0.332189,  0.161025,  0.376800,
     &  0.229605,     6*0.0/
C  SPECIES 9
      DATA (DGHAB(I,9),I=1,12)/
     &       0.0,  0.151514,  0.092675,  0.318444, -0.038935,
     &     7*0.0/
C  SPECIES 10
      DATA (DGHAB(I,10),I=1,12)/
     &       0.0,  0.006074,  0.181590, -0.196098, -0.055780,
     &  0.133907,  0.045857,     5*0.0/
C  SPECIES 11
      DATA (DGHAB(I,11),I=1,12)/
     &     12*0.0/
C  SPECIES 12
      DATA (DGHAB(I,12),I=1,12)/
     &     12*0.0/
C  SPECIES 13
      DATA (DGHAB(I,13),I=1,12)/
     &     12*0.0/
C  SPECIES 14
      DATA (DGHAB(I,14),I=1,12)/
     &     12*0.0/
C  SPECIES 15
      DATA (DGHAB(I,15),I=1,12)/
     &     12*0.0/
C  SPECIES 16
      DATA (DGHAB(I,16),I=1,12)/
     &     12*0.0/
C  SPECIES 17
      DATA (DGHAB(I,17),I=1,12)/
     &     12*0.0/
C  SPECIES 18
      DATA (DGHAB(I,18),I=1,12)/
     &       0.0,  0.155479,  0.255922,  0.543851,  0.468293,
     &  0.412304,  0.367085,  0.171232,  0.262057,  0.048266,
     &  0.301184,  0.542920/
C  SPECIES 19
      DATA (DGHAB(I,19),I=1,12)/
     &     12*0.0/
C----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
C----------
      DATA MAPLOC/
     & 1, 2, 2, 3, 2, 2,
     & 1, 2, 2, 2, 2, 2,
     & 1, 2, 2, 3, 2, 2,
     & 1, 2, 2, 1, 2, 2,
     & 1, 2, 2, 1, 2, 2,
     & 1, 2, 2, 2, 2, 2,
     & 1, 2, 2, 3, 2, 2,
     & 1, 1, 1, 1, 1, 1,
     & 1, 2, 2, 3, 2, 2,
     & 1, 2, 2, 3, 2, 2,
     & 1, 1, 1, 1, 1, 1,
     & 1, 1, 1, 1, 1, 1,
     & 1, 1, 1, 1, 1, 1,
     & 1, 1, 1, 1, 1, 1,
     & 1, 1, 1, 1, 1, 1,
     & 1, 1, 1, 1, 1, 1,
     & 1, 1, 1, 1, 1, 1,
     & 1, 2, 2, 3, 2, 2,
     & 1, 1, 1, 1, 1, 1/
      DATA DGFOR/
     & 2.017817, 1.851178, 1.879603,
     & 1.445729, 1.550427, 0.0,
     & 1.116401, 1.004272, 1.035798,
     & 0.691340, 0.977362, 0.0,
     & 0.691340, 0.977362, 0.0,
     & 1.445729, 1.550427, 0.0,
     & 0.473751, 0.425722, 0.523818,
     & 0.255416, 0.0,      0.0,
     & 0.442641, 0.475575, 0.593639,
     & 2.017817, 1.851178, 1.879603,
     & 1.568742,      0.0,      0.0,
     & 1.568742,      0.0,      0.0,
     &      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,
     &-0.107648,      0.0,      0.0,
     & 1.568742,      0.0,      0.0,
     &      0.0,      0.0,      0.0,
     & 0.473751, 0.425722, 0.523818,
     &      0.0,      0.0,      0.0/
C----------
C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY SPECIES.
C----------
      DATA DGDS/
     & -0.0004163, -0.0003576, -0.0004562, -0.0004408, -0.0004408,
     & -0.0003576, -0.0009134, -0.0003958, -0.0002569, -0.0004163,
     & -0.0006538, -0.0006538,        0.0,        0.0,        0.0,
     & -0.0006538,        0.0, -0.0009134,        0.0/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      CONSPP= DGCON(ISPC) + COR(ISPC) + 0.01*DGCCFA(ISPC)*RELDEN
      DGLDS= DGLD(ISPC)
      DGBALS = DGBAL(ISPC)
      DGCRS= DGCR(ISPC)
      DGCRS2=DGCRSQ(ISPC)
      DGDSQS=DGDSQ(ISPC)
      DGDBLS=DGDBAL(ISPC)
      DGLBAS=DGLBA(ISPC)
      DGPCFS=DGPCCF(ISPC)
      DGBAS=DGBA(ISPC)
      SI = SITEAR(ISPC)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 10 I3=I1,I2
      I=IND1(I3)
      IPCCF=ITRE(I)
      D=DIAM(I)
      IF (D.LE.0.0) GOTO 10
      CR=ICR(I)*0.01
      SELECT CASE (ISPC)
      CASE(11,12,16)
        BAL = (1.0 - (PCT(I)/100.)) * BA/100.
      CASE DEFAULT
        BAL = (1.0 - (PCT(I)/100.)) * BA
      END SELECT
      PBAL= (1.0 - (PCT(I)/100.)) * PTBAA(ITRE(I))
      BARK=BRATIO(ISPC,D,HT(I))
C
      SELECT CASE (ISPC)
C
C  13=ASPEN; FROM IE(18)=UT(6)
C
      CASE (13)
        CR = ICR(I)
        CALL DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
        DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
C
C  14=WJ; FROM UT(12)
C
      CASE (14)
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BA
        IF(BATEM .LT.  1.0) BATEM = 1.0
        DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM
     &         + 0.00177 * SI
        IF((DF-DPP) .GT. 1.0) DF = DPP + 1.0
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
C
C  15=MC; FROM UT(20)=SO(30)=WC(39)
C
      CASE (15)
        IF(DEBUG)WRITE(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)
        DDS=CONSPP +DGLDS*ALOG(D) + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)
     &             +DGDSQS*D*D  + DGBAS*BAL/(ALOG(D+1.0))
     &             -0.000981*BA
C
C  17=CW AND 19=OH; FROM IE(19)=CR(38)
C
      CASE(17,19)
        ICLS = IFIX(D + 1.0)
        IF(ICLS .GT. 41) ICLS = 41
        CR=ICR(I)*0.01
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        DF = 0.24506 + 1.01291 * DPP - 0.00084659 * BA
     &         + 0.00631*SI
        IF(DF .GT. 36.) DF = 36.
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
        DDS = DDS+COR(ISPC)+DGCON(ISPC)
C
C  SPECIES USING ORIGINAL CI EQNS, PLUS THOSE FROM IE THAT HAVE THE
C  SAME FUNCTIONAL FORM: 11=WB, 12=PY, AND 16=LM, 
C
      CASE DEFAULT
        DDS=CONSPP +DGLDS*ALOG(D) + DGLBAS*ALOG(BA) 
     &      + DGDSQS*D*D + DGDBLS*PBAL/(ALOG(D+1.0))
     &      + DGBAS*BAL/(ALOG(D+1.0)) + DGPCFS*PCCF(IPCCF)
     &      + CR*(DGCRS+CR*DGCRS2)+ DGBALS*BAL
      END SELECT
C---------
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(.NOT.DEBUG) GO TO 10
      WRITE(JOSTND,9001) I,ISPC,D,BAL,CR,PCCF(IPCCF),BA,
     &                   PBAL,DDS
 9001 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &      ',  BAL=',F7.2,',  CR=',F7.4/
     &      '     PCCF=',F9.3,',  BA=',F9.3,',  PBAL=',F9.3,
     &      ',   LN(DDS)=',F7.4)
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
C  ICINDX IS A CI HABITAT TYPE INDEX COMPUTED IN **HABTYP**.
C  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
C  OBSERVATIONS BY SPECIES FOR THE UNDERLYING
C  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).
C----------
C CHECK FOR DEBUG.
C----------
      CALL DBCHK(DEBUG,'DGF',3,ICYC)
      IF(DEBUG)
     &WRITE(JOSTND,*)' ICINDX',ICINDX,'SLOPE',SLOPE,'ELEV',ELEV,'IFOR',
     &    IFOR,'ASP',ASPECT
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO 30 ISPC=1,MAXSP
C------------
C  PUT SITE INTO ONE OF 5 CLASSES
C----------
      XSITE = SITEAR(ISPC)
      LSI=INT(XSITE/10.)
      IF(LSI.LT.2)ISIC=1
      IF(LSI.GE.5)ISIC=5
      IF(LSI.GE.3 .AND. LSI.LT.4)ISIC=3
      IF(LSI.GE.4 .AND. LSI.LT.5)ISIC=4
      IF(LSI.GE.2 .AND. LSI.LT.3)ISIC=2
C
      MAPHAB = ICHBCL(ICINDX,ISPC) + 1
      DHAB=DGHAB(MAPHAB,ISPC)
      ISPFOR=MAPLOC(IFOR,ISPC)
C
      SELECT CASE (ISPC)
      CASE(11,12,13,16)
        TMPASP = ASPECT - 0.7854
      CASE DEFAULT
        TMPASP = ASPECT
      END SELECT
C
      SELECT CASE (ISPC)
      CASE(15)
        TEMEL = ELEV
        IF (TEMEL.GT.30.)TEMEL=30.
      CASE DEFAULT
        TEMEL = ELEV
      END SELECT
C
      DGCON(ISPC)=   DHAB
     &             + DGFOR(ISPFOR,ISPC)
     &             + DGEL(ISPC) * TEMEL
     &             + DGEL2(ISPC) * TEMEL * TEMEL
     &             +(DGSASP(ISPC) * SIN(TMPASP)
     &             + DGCASP(ISPC) * COS(TMPASP)
     &             + DGSLOP(ISPC)) * SLOPE
     &             + DGSLSQ(ISPC) * SLOPE * SLOPE
C
      SELECT CASE (ISPC)
      CASE(11,12,16)
           DGCON(ISPC)=DGCON(ISPC)+.001766*XSITE
      CASE(13)
           DGCON(ISPC)=DGCON(ISPC)+.006460*XSITE
      CASE(15)
           DGCON(ISPC)=DGCON(ISPC)+0.227307*ALOG(XSITE)
      END SELECT
C
      IF(DEBUG)
     &WRITE(JOSTND,*)' CONSTANT COMPONENTS ISPC = ',ISPC,
     &'DGCON',DGCON(ISPC),'DHAB',DHAB,'DHAB',DHAB,'DGFOR',
     &DGFOR(ISPFOR,ISPC),'DGEL',DGEL(ISPC),'DGEL2',DGEL2(ISPC),
     & ' SINASP',DGSASP(ISPC),'COSASP',DGCASP(ISPC),
     &'DGSLOP',DGSLOP(ISPC),'DGSLOSQ',DGSLSQ(ISPC),
     &'XSITE',XSITE
C
      SELECT CASE (ISPC)
      CASE(11,12,16)
        ATTEN(ISPC)=IBSERV(ISIC,1)
      CASE(13)
        ATTEN(ISPC)=IBSERV(ISIC,2)
      CASE(14,17,19)
        ATTEN(ISPC)=IBSERV(ISIC,3)
      CASE DEFAULT
        ATTEN(ISPC)=OBSERV(ISPC)
      END SELECT
C
      DGDSQ(ISPC)=DGDS(ISPC)
      SMCON(ISPC)=0.
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
      IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC)
     &  + ALOG(COR2(ISPC))
   30 CONTINUE
      RETURN
      END
