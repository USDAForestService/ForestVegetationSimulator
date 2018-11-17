      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C WS $Id$
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
C     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO TERM IN THE DDS MODEL
C   DGSITE -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SITE
C             TERM IN THE DDS MODEL
C    DGBA  -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA
C             TERM IN THE DDS MODEL
C   DGDBA  -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
C             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH)
C    DGDS  -- ARRAY CONTAINING THE COEFFICIENTS FOR THE DIAMETER
C             SQUARED TERM IN THE DDS MODEL
C     DGEL -- ARRAY CONTAINING THE COEFFICIENTS FOR THE ELEVATION TERM.
C    DGSLP -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SLOPE TERM.
C    DGSLQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SLOPE SQUARED TE
C    DGLAT -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LATITUDE TERM.
C
C
C IF SPECIES IS 5 OR 9, USE LEROY'S ORIGINAL WS EQUATIONS WITH SITE.
C IF SPECIES IS 7 OR 11, USE NC EQNS FOR BO & TO.
C FOR ALL OTHER SPECIES USE EQUATIONS REFIT BY JOHNSON 1992.
C
C IF SPECIES IS 3, 4, 8, OR 9, USE SEPERATE EQUATIONS FOR TREES
C SMALLER THAN 10" DBH.
C----------
      EXTERNAL RANN
      LOGICAL DEBUG
      INTEGER OBSERV(MAXSP),MAPLOC(7,MAXSP)
      INTEGER ILAT,I,IPCCF,I1,I2,I3,ISPC,J,ITLAT
      REAL DIAM(MAXTRE),DGLD(MAXSP),DGCR(MAXSP),DGCRSQ(MAXSP),
     &   DGDBAL(MAXSP),DGBA(MAXSP),DGHAH(MAXSP),DGPCCF(MAXSP),
     &   DGFOR(3,MAXSP),DGSITE(MAXSP),DGELSQ(MAXSP),DGEL(MAXSP),
     &   DGCASP(MAXSP),DGSASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP),
     &   DGLAT5(5),DGLAT9(5),DGDS(MAXSP)
      REAL FORCON,CONSJP,CONSPP,DGLDS,DGCRS,DGCRS2,SI
      REAL DGDBLS,DGPCFS,DGHAHS,DGBAS,D,DPP,HOAVH,CR,ALD,CRID,BAL
      REAL PBA,PBAL,ALBA,DDS,TDDS,DDSMAX,TSITE,DGDSQS,DGBAL
      REAL BATEM,DF,DIAGR,BARK,SUMD2,SUMTRE,DGQMD,STDSDI,RELSDI
      REAL DDSMX1(MAXSP),DDSMX2(MAXSP),BRATIO,TEMEL
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
C    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
C    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
C    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
C    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
C    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
C    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
C    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
C    42 = OTHER SOFTWOODS (OS)
C    43 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C    FROM EXISTING WS EQUATIONS --
C      USE 1(SP) FOR 11(WP) AND 24(MH) 
C      USE 2(DF) FOR 22(BD)
C      USE 3(WF) FOR 13(SF)
C      USE 4(GS) FOR 23(RW)
C      USE 8(PP) FOR 18(MP)
C      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
C      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
C                     43(OH)
C
C    FROM CA VARIANT --
C      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE), 
C                       25(WJ), 26(WJ), AND 27(CJ)
C      USE CA12(LP) FOR 9(LP) AND 10(WB)
C
C    FROM SO VARIANT --
C      USE SO30(MC) FOR 41(MC)
C
C    FROM UT VARIANT --
C      USE UT17(GB) FOR 21(GB)
C----------
C  DATA STATEMENTS
C----------
      DATA DDSMX1/
     & -3.91, -3.06, -2.81, -3.91, -2.20,
     & -3.71, -2.81, -2.07,   0.0,   0.0,
     & -3.91,   0.0, -2.81,   0.0,   0.0,
     &   0.0,   0.0, -2.07,   0.0,   0.0,
     &   0.0, -3.06, -3.91, -3.91,   0.0,
     &   0.0,   0.0, -2.20, -2.20, -2.20,
     & -2.20, -2.20, -2.20, -2.20, -2.20,
     & -2.20, -2.20, -2.20, -2.20, -2.20,
     &   0.0, -3.71, -2.20/
C
      DATA DDSMX2/
     &  1.42,  1.27,  1.24,  1.42,  1.08,
     &  1.25,  1.24,  1.09,   0.0,   0.0,
     &  1.42,   0.0,  1.24,   0.0,   0.0,
     &   0.0,   0.0,  1.09,   0.0,   0.0,
     &   0.0,  1.27,  1.42,  1.42,   0.0,
     &   0.0,   0.0,  1.08,  1.08,  1.08,
     &  1.08,  1.08,  1.08,  1.08,  1.08,
     &  1.08,  1.08,  1.08,  1.08,  1.08,
     &   0.0,  1.25,  1.08/
C
      DATA DGLD/
     &    1.0857,    0.8641,    1.2854,       0.0,   1.29079,
     &   0.51047,       0.0,   0.96103,  1.218279,  1.218279,
     &    1.0857,  1.077154,    1.2854,  1.077154,  1.077154,
     &  1.077154,  1.077154,   0.96103,  1.077154,  1.077154,
     &       0.0,    0.8641,       0.0,    1.0857,  1.077154,
     &  1.077154,  1.077154,   1.23911,   1.23911,   1.23911,
     &   1.23911,   1.23911,   1.23911,   0.99531,   0.99531,
     &   0.99531,   0.99531,   0.99531,   0.99531,   1.23911,
     &  0.889596,    1.1783,   1.23911/
C
      DATA DGCR/
     &    0.3910,    0.4246,   -1.0191,       0.0,   -0.0906,
     &   0.91422,       0.0,    0.4126,  3.167164,  3.167164,
     &    0.3910, -0.276387,   -1.0191, -0.276387, -0.276387,
     & -0.276387, -0.276387,    0.4126, -0.276387, -0.276387,
     &       0.0,    0.4246,       0.0,    0.3910, -0.276387,
     & -0.276387, -0.276387,  -1.20841,  -1.20841,  -1.20841,
     &  -1.20841,  -1.20841,  -1.20841,   2.08524,   2.08524,
     &   2.08524,   2.08524,   2.08524,   2.08524,  -1.20841,
     &  1.732535,    0.9492,  -1.20841/
C
      DATA DGCRSQ/
     &       0.0,       0.0,    0.9104,       0.0,       0.0,
     &   0.27758,       0.0,       0.0, -1.568333, -1.568333,
     &       0.0,  1.063732,    0.9104,  1.063732,  1.063732,
     &  1.063732,  1.063732,       0.0,  1.063732,  1.063732,
     &       0.0,       0.0,       0.0,       0.0,  1.063732,
     &  1.063732,  1.063732,   2.31782,   2.31782,   2.31782,
     &   2.31782,   2.31782,   2.31782,  -0.98396,  -0.98396,
     &  -0.98396,  -0.98396,  -0.98396,  -0.98396,   2.31782,
     &       0.0,       0.0,   2.31782/
C
      DATA DGDBAL/
     &  -0.00579,  -0.01127,  -0.00628,       0.0,  -0.00544,
     &  -0.01282,       0.0,  -0.01265,       0.0,       0.0,
     &  -0.00579,       0.0,  -0.00628,       0.0,       0.0,
     &       0.0,       0.0,  -0.01265,       0.0,       0.0,
     &       0.0,  -0.01127,       0.0,  -0.00579,       0.0,
     &       0.0,       0.0,  -0.00199,  -0.00199,  -0.00199,
     &  -0.00199,  -0.00199,  -0.00199,  -0.00147,  -0.00147,
     &  -0.00147,  -0.00147,  -0.00147,  -0.00147,  -0.00199,
     & -0.001265,  -0.00016,  -0.00199/
C
      DATA DGBA /
     &   -0.1313,       0.0,  -0.21056,       0.0,  -0.23182,
     &  -0.01880,       0.0,   -0.1431, -0.267873, -0.267873,
     &   -0.1313,       0.0,  -0.21056,       0.0,       0.0,
     &       0.0,       0.0,   -0.1431,       0.0,       0.0,
     &       0.0,       0.0,       0.0,   -0.1313,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     & -0.000981,   -0.3270,       0.0/
C
      DATA DGHAH /
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,   0.50155,   0.50155,
     &   0.50155,   0.50155,   0.50155,   0.50155,       0.0,
     &       0.0,       0.0,       0.0/
C
      DATA DGPCCF/
     &  -0.00058,  -0.00018,  -0.00091,       0.0,  -0.00098,
     &  -0.00099,       0.0,  -0.00084, -0.000338, -0.000338,
     &  -0.00058,       0.0,  -0.00091,       0.0,       0.0,
     &       0.0,       0.0,  -0.00084,       0.0,       0.0,
     &       0.0,  -0.00018,       0.0,  -0.00058,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,   -0.0018,   -0.0018,
     &   -0.0018,   -0.0018,   -0.0018,   -0.0018,       0.0,
     &       0.0,       0.0,       0.0/
C
C----------
C  OBSERV CONTAINS THE NUMBER OF OBSERVATIONS  BY HABITAT CLASS BY
C  SPECIES FOR THE UNDERLYING MODEL.
C  (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).
C----------
      DATA  OBSERV/
     &      650,      480,     3301,     1762,     1339,
     &     1114,        9,     1528,      372,      372,
     &      650,       84,     3301,       84,       84,
     &       84,       84,     1528,       84,       84,
     &     1000,      480,     1762,      650,       84,
     &       84,       84,      583,      583,      583,
     &      583,      583,      583,     6504,     6504,
     &     6504,     6504,     6504,     6504,      583,
     &      220,      419,      583/
C
C----------
C DGLAT CONTAINS LATITUDE CONSTANTS FOR SPECIES 5 & 9.
C----------
      DATA DGLAT5/ -0.4297, -0.4297, -0.4297, -0.2777, -0.4297/
      DATA DGLAT9/  0.1434,  0.3191,  0.1434,  0.2246,  0.1434/
C----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
C  CURRENTLY FOREST CODE WILL BE 1-6 IN THIS VARIANT.
C----------
      DATA ((MAPLOC(I,J),I=1,7),J=1,10)/
     & 1,2,2,2,1,2,1,
     & 1,1,2,1,1,1,1,
     & 1,2,3,3,1,3,1,
     & 1,1,1,1,1,1,1,
     & 1,1,2,3,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,2,2,1,2,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1/
      DATA ((MAPLOC(I,J),I=1,7),J=11,20)/
     & 1,2,2,2,1,2,1,
     & 1,1,1,1,1,1,1,
     & 1,2,3,3,1,3,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,2,2,1,2,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1/
      DATA ((MAPLOC(I,J),I=1,7),J=21,30)/
     & 1,1,1,1,1,1,1,
     & 1,1,2,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,2,2,2,1,2,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1/
      DATA ((MAPLOC(I,J),I=1,7),J=31,40)/
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1/
      DATA ((MAPLOC(I,J),I=1,7),J=41,MAXSP)/
     & 1,1,1,1,1,1,1,
     & 1,1,1,1,2,1,1,
     & 1,1,1,1,1,1,1/
C
      DATA ((DGFOR(I,J),I=1,3),J=1,10)/
     & -0.70344, -0.90272,      0.0,
     &  -0.5260,  -0.9842,  -0.7603,
     &   0.0755,  -0.3099,  -0.0440,
     & -1.69950,      0.0,      0.0,
     &  0.02786,  0.21393, -0.04927,
     & -1.81306,  -2.3037,      0.0,
     &      0.0,      0.0,      0.0,
     &  -0.8882,  -1.0712,      0.0,
     &-2.058828,      0.0,      0.0,
     &-2.058828,      0.0,      0.0/
      DATA ((DGFOR(I,J),I=1,3),J=11,20)/
     & -0.70344, -0.90272,      0.0,
     & 0.564402,      0.0,      0.0,
     &   0.0755,  -0.3099,  -0.0440,
     & 0.564402,      0.0,      0.0,
     & 0.564402,      0.0,      0.0,
     & 0.564402,      0.0,      0.0,
     & 0.564402,      0.0,      0.0,
     &  -0.8882,  -1.0712,      0.0,
     & 0.564402,      0.0,      0.0,
     & 0.564402,      0.0,      0.0/
      DATA ((DGFOR(I,J),I=1,3),J=21,30)/
     &      0.0,      0.0,      0.0,
     &  -0.5260,  -0.9842,  -0.7603,
     & -1.69950,      0.0,      0.0,
     & -0.70344, -0.90272,      0.0,
     & 0.564402,      0.0,      0.0,
     & 0.564402,      0.0,      0.0,
     & 0.564402,      0.0,      0.0,
     & -2.68349,      0.0,      0.0,
     & -2.68349,      0.0,      0.0,
     & -2.68349,      0.0,      0.0/
      DATA ((DGFOR(I,J),I=1,3),J=31,40)/
     & -2.68349,      0.0,      0.0,
     & -2.68349,      0.0,      0.0,
     & -2.68349,      0.0,      0.0,
     & -0.94563,      0.0,      0.0,
     & -0.94563,      0.0,      0.0,
     & -0.94563,      0.0,      0.0,
     & -0.94563,      0.0,      0.0,
     & -0.94563,      0.0,      0.0,
     & -0.94563,      0.0,      0.0,
     & -2.68349,      0.0,      0.0/
      DATA ((DGFOR(I,J),I=1,3),J=41,MAXSP)/
     &-0.107648,      0.0,      0.0,
     & -0.02772,   0.1327,      0.0,
     & -2.68349,      0.0,      0.0/
C----------
C  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
C  DIAMETER GROWTH EQUATION.
C  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
C  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
C  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
C  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.
C----------
      DATA DGCASP/
     &   0.01664,       0.0,   -0.1804,       0.0,       0.0,
     &  -0.16447,   -0.4919,   0.26986,       0.0,       0.0,
     &   0.01664,  0.649870,   -0.1804,  0.649870,  0.649870,
     &  0.649870,  0.649870,   0.26986,  0.649870,  0.649870,
     &       0.0,       0.0,       0.0,   0.01664,  0.649870,
     &  0.649870,  0.649870,   0.08632,   0.08632,   0.08632,
     &   0.08632,   0.08632,   0.08632,  -0.19935,  -0.19935,
     &  -0.19935,  -0.19935,  -0.19935,  -0.19935,   0.08632,
     &  0.085958,       0.0,   0.08632/
C
      DATA DGSASP/
     &  -0.00350,       0.0,   -0.1183,  -0.10656,       0.0,
     &   0.05342,       0.0,   0.09668,       0.0,       0.0,
     &  -0.00350,  0.951834,   -0.1183,  0.951834,  0.951834,
     &  0.951834,  0.951834,   0.09668,  0.951834,  0.951834,
     &       0.0,       0.0,  -0.10656,  -0.00350,  0.951834,
     &  0.951834,  0.951834,  -0.11954,  -0.11954,  -0.11954,
     &  -0.11954,  -0.11954,  -0.11954,  -0.03587,  -0.03587,
     &  -0.03587,  -0.03587,  -0.03587,  -0.03587,  -0.11954,
     &  -0.86398,       0.0,  -0.11954/
C
      DATA DGSLOP/
     &    0.7603,       0.0,       0.0,  -1.29627,       0.0,
     &  -0.05469,       0.0,   0.90804,       0.0,       0.0,
     &    0.7603,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,   0.90804,       0.0,       0.0,
     &       0.0,       0.0,  -1.29627,    0.7603,       0.0,
     &       0.0,       0.0,   0.85815,   0.85815,   0.85815,
     &   0.85815,   0.85815,   0.85815,   0.73530,   0.73530,
     &   0.73530,   0.73530,   0.73530,   0.73530,   0.85815,
     &       0.0,       0.0,   0.85815/
C
      DATA DGSLSQ/
     &   -2.2339,       0.0,       0.0,   0.87335,       0.0,
     &       0.0,       0.0,  -2.04028,       0.0,       0.0,
     &   -2.2339,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  -2.04028,       0.0,       0.0,
     &       0.0,       0.0,   0.87335,   -2.2339,       0.0,
     &       0.0,       0.0,  -1.17209,  -1.17209,  -1.17209,
     &  -1.17209,  -1.17209,  -1.17209,  -0.99561,  -0.99561,
     &  -0.99561,  -0.99561,  -0.99561,  -0.99561,  -1.17209,
     &       0.0,       0.0,  -1.17209/
C
      DATA DGEL/
     &   0.01919,   0.00489,       0.0,       0.0,  -0.00919,
     &   0.00304,       0.0,   0.00323,       0.0,       0.0,
     &   0.01919,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,   0.00323,       0.0,       0.0,
     &       0.0,   0.00489,       0.0,   0.01919,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     & -0.075986,       0.0,       0.0/
C
      DATA DGELSQ/
     &  -0.00025,       0.0,       0.0,   0.00019,   0.00019,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &  -0.00025,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,   0.00019,  -0.00025,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &  0.001193,       0.0,       0.0/
C
      DATA DGSITE/
     &    0.5827,    0.5040,    0.5260,       0.0,    0.3737,
     &   0.96412,       0.0,    0.6828,  0.566946,  0.566946,
     &    0.5827,       0.0,    0.5260,       0.0,       0.0,
     &       0.0,       0.0,    0.6828,       0.0,       0.0,
     &       0.0,    0.5040,       0.0,    0.5827,       0.0,
     &       0.0,       0.0,   0.32093,   0.32093,   0.32093,
     &   0.32093,   0.32093,   0.32093,   0.01200,   0.01200,
     &   0.01200,   0.01200,   0.01200,   0.01200,   0.32093,
     &  0.227307,    0.4401,   0.32093/
C
      DATA DGDS /
     & -0.000288, -0.000290, -0.000584,       0.0,  -0.00061,
     & -0.000222,       0.0, -0.000375,-0.0014178,-0.0014178,
     & -0.000288,       0.0, -0.000584,       0.0,       0.0,
     &       0.0,       0.0, -0.000375,       0.0,       0.0,
     &       0.0, -0.000290,       0.0, -0.000288,       0.0,
     &       0.0,       0.0, -0.000338, -0.000338, -0.000338,
     & -0.000338, -0.000338, -0.000338, -0.000373, -0.000373,
     & -0.000373, -0.000373, -0.000373, -0.000373, -0.000338,
     &       0.0, -0.000660, -0.000338/
C 
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
C----------
C  DEBUG OUTPUT: MODEL COEFFICIENTS.
C----------
      IF(DEBUG)WRITE(JOSTND,9000) DGCON
 9000 FORMAT(' DGCON =',/,11(1X,F10.5))     
C----------
C  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
C  NEEDS; COMPUTE THOSE HERE.
C----------
C COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.
C----------
      SUMD2=0.
      SUMTRE=0.
      DGQMD=0.
      IF(ITRN .EQ. 0) GO TO 1001
      DO 1000 I=1,ITRN
      SUMD2=SUMD2+DBH(I)*DBH(I)*PROB(I)
      SUMTRE=SUMTRE+PROB(I)
 1000 CONTINUE
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
        CALL SDICAL(0,SDIMAX)
        IF(SDIMAX.GT.0.)RELSDI=STDSDI/SDIMAX
        IF(DEBUG)WRITE(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',
     &  STDSDI,SDIMAX,RELSDI
        IF(RELSDI .GT. 0.85) RELSDI = 0.85
        DSTAG = 1.0
        IF(RELSDI .GT. 0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
        IF(DEBUG)WRITE(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
        IF(DEBUG)WRITE(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
      ENDIF
C----------
C  END OF SPECIAL NEEDS SECTION
C
C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      FORCON=0.0
      CONSJP=0.0
      SI = SITEAR(ISPC)
      CONSPP= DGCON(ISPC) + COR(ISPC)
      DGLDS=DGLD(ISPC)
      DGCRS=DGCR(ISPC)
      DGCRS2=DGCRSQ(ISPC)
      DGDSQS=DGDS(ISPC)
      DGDBLS=DGDBAL(ISPC)
      DGPCFS=DGPCCF(ISPC)
      DGHAHS=DGHAH(ISPC)
      DGBAS=DGBA(ISPC)
C----------
C  LOAD A CONSPP VALUE (CONSJP) FOR SMALL:
C  2=DF, 3=WF, 6=JP, 7=RF, 13=SF, 22=BD 
C----------
      SELECT CASE (ISPC)
      CASE(6)
        FORCON=0.74162
        IF(IFOR .EQ. 2) FORCON=0.59493
        IF(IGL .EQ. 2) FORCON=0.854515
        CONSJP=COR(ISPC) + 0.40657*ALOG(SI) + 0.56709*SLOPE
     &    - 0.14671*COS(ASPECT)*SLOPE + 0.26785*SIN(ASPECT)*SLOPE
     &    - 0.00164*ELEV + FORCON
        IF(LDCOR2.AND.COR2(ISPC).GT.0.0)CONSJP=CONSJP+ALOG(COR2(ISPC))
      CASE(2,3,7,13,22)
        CONSJP=COR(ISPC) + 0.233713*ALOG(SI) + 1.53962
        IF(LDCOR2.AND.COR2(ISPC).GT.0.0)CONSJP=CONSJP+ALOG(COR2(ISPC))
      END SELECT
C----------
C  BEGIN TREE LOOP.
C----------
      DO 10 I3=I1,I2
      I=IND1(I3)
      D=DIAM(I)
      IF (D.LE.0.0) GOTO 10
      BARK=BRATIO(ISPC,D,HT(I))
      IPCCF=ITRE(I)
      HOAVH=HT(I)/AVH
      CR=FLOAT(ICR(I))/100.
      ALD=ALOG(D)
      CRID = ((ICR(I)*ICR(I))/(ALOG(D+1.0)))/1000.0
      IF(D .LT. 2.0)CRID = 1.8
      BAL = (1.0 - (PCT(I)/100.)) * BA
      PBA=PTBAA(ITRE(I))
      PBAL=PBA*(1.0-(PCT(I)/100.))
      IF(PBAL .LT. 0.0)PBAL = BAL
      IF(PBA .LE. 0.0)THEN
        IF(BA .GT. 1.0)THEN
          PBA=BA
        ELSE
          PBA=1.0
        ENDIF
      ENDIF
      ALBA=0.0
      IF(BA.GT.0.0)ALBA=ALOG(BA)
C
      SELECT CASE (ISPC)
C----------
C  SPECIES USING EQUATIONS FROM THE CA VARIANT
C----------
      CASE (9:10,12,14:17,19:20,25:27)
        IF(HOAVH .GT. 1.5)HOAVH=1.5
        IF(ISPC.EQ.9 .OR. ISPC.EQ.10)THEN
          DGBAL = 0.0
        ELSE
          DGBAL = -0.000893
        ENDIF
        DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR
     &        + DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))
     &        + DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH 
     &        + DGBAS*ALBA + DGBAL*BAL
C----------
C  SPECIES USING EQUATIONS FROM THE SO VARIANT
C----------
      CASE (41)
        IF(HOAVH .GT. 1.5)HOAVH=1.5
        DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR
     &        + DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))
     &        + DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH + DGBAS*BA
C----------
C  SPECIES USING EQUATIONS FROM THE CR VARIANT (VIA UT)
C  USE SPRUCE-FIR MODEL TYPE LOWER BA LIMIT
C
C  THIS ESTIMATE IS NOT USED; TREES OF ALL SIZES ARE GROWN WITH THE
C  EQUATIONS IN **REGENT**.
C----------
      CASE (21)
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BA
        IF(BATEM .LT. 5.0) BATEM = 5.0
          DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM
     &         + 0.00177 * SI
          IF((DF-DPP) .GT. 1.0) DF = DPP + 1.0
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
C----------
C  SPECIES USING ORIGINAL WS EQUATIONS
C----------
      CASE(4,23)
        DDS = CONSPP + 1.26883*ALD - 0.35325*D*D/1000.
     &        + 0.27986*CRID - 0.79922*PBAL/(ALOG(D+1.0))/100.
C
      CASE(7)
        DDS = CONSPP + 1.53339*ALD - 0.47442*D*D/1000.
     &        + 0.35739*CRID - 0.44256*PBAL/(ALOG(D+1.0))/100.
     &        - 0.12359*ALOG(PBA)
C
      CASE DEFAULT
        DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR
     &        + DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))
     &        + DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH + DGBAS*ALBA
        IF(ISPC.EQ.3 .OR. ISPC.EQ.13) DDS = DDS - 0.15032
C----------
C  JP,DF,WF,RF  LESS THAN 10" USE SEPERATE EQUATION.
C----------
        IF(D.LT.10.0) THEN
          SELECT CASE (ISPC)
          CASE(6)
            DDS = CONSJP + 1.23864*ALD + 0.64311*CR - 0.48754*ALBA
     &           - 0.00189*BAL/(ALOG(D+1.)) - 0.00096*PCCF(IPCCF)
          CASE(2,3,7,13,22)
            DDS = CONSJP - 0.52776*ALBA + 1.64163*ALD
     &           - 0.00205*BAL/(ALOG(D+1.)) - 0.00105*PCCF(IPCCF)
          END SELECT
        ENDIF
C----------
C  BO & TO ARE 5-YR EQNS, NEED TO ADJUST TO A 10-YEAR ESTIMATE.
C---------
        IF((ISPC.GE.28 .AND. ISPC.LE.40) .OR. ISPC.EQ.43)THEN
          TDDS=EXP(DDS)
          DDS=ALOG(TDDS*2.0)
        ENDIF
      END SELECT
C----------
C  TEST RUNS SHOW DG TO SMALL, ADJUST TO MATCH GROWTH DATA. 3-24-94 GD
C  VALUE 0.3 IS LN(1.35) --- 1.35 IS AVERAGE CALIBRATION VALUE.
C----------
      IF(ISPC.EQ.5)THEN
        DDS=DDS+0.30*(0.80+0.004*(SI-50.))
      ENDIF
C
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG)THEN
      WRITE(JOSTND,9001) I,ISPC,D,PBAL,CRID,PBA,CONSPP,DDS
 9001 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &      ',  PBAL=',F7.2,',  CRID=',F7.4/
     & '  PBA=',F9.3,',   CONSPP=',F10.4,',  LN(DDS),=',F7.4)
      ENDIF
C----------
C CHECK TO SEE THAT PREDICTED DDS DOES NOT EXCEED MAX BOUND
C----------
      IF(ICR(I).LE.0.0) GO TO 9
      SELECT CASE (ISPC)
      CASE (1:8,11,13,18,22:24,28:33,40,42,43)
        DDSMAX = DDSMX1(ISPC) + DDSMX2(ISPC)*ALOG(ICR(I)*DBH(I))
        IF(DDS .GT. DDSMAX) THEN
          DDS = DDSMAX
          IF(DEBUG)WRITE(JOSTND,8)I,ISPC
    8     FORMAT(' ******WARNING DDS BOUND BEING INVOKED TREE,SPECIES=',
     &    2I5)
        ENDIF
      END SELECT
    9 CONTINUE
   10 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
      IF(DEBUG)WRITE(JOSTND,100)ICYC
  100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
      RETURN
C
C
      ENTRY DGCONS
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      ITLAT = NINT(TLAT)
      IF(ITLAT .LE. 35.) ILAT=1
      IF(ITLAT .EQ. 36.) ILAT=2
      IF(ITLAT .EQ. 37.) ILAT=3
      IF(ITLAT .EQ. 38.) ILAT=4
      IF(ITLAT .GE. 39.) ILAT=5
C
      DO 30 ISPC=1,MAXSP
      ISPFOR=MAPLOC(IFOR,ISPC)
      TEMEL=ELEV
      IF(ISPC.EQ.41 .AND. TEMEL.GT.30)TEMEL=30.
C
      SELECT CASE (ISPC)
C
      CASE(4,23)
        DGCON(ISPC) = DGLAT5(ILAT) + 0.01401*SITEAR(ISPC)
C
      CASE(7)
        DGCON(ISPC) = DGLAT9(ILAT) - 0.00700*ELEV
     &    - 0.83400*SLOPE*SLOPE + 0.00734*SITEAR(ISPC)
C
      CASE DEFAULT
        TSITE=SITEAR(ISPC)
        IF((ISPC.GE.28 .AND. ISPC.LE.40) .OR. ISPC.EQ.43)
     &    TSITE=SITEAR(2)
        DGCON(ISPC) = DGFOR(ISPFOR,ISPC)
     &              + DGEL(ISPC)*TEMEL
     &              + DGELSQ(ISPC)*TEMEL*TEMEL
     &              + DGSASP(ISPC)*SIN(ASPECT)*SLOPE
     &              + DGCASP(ISPC)*COS(ASPECT)*SLOPE
     &              + DGSLOP(ISPC)*SLOPE
     &              + DGSLSQ(ISPC)*SLOPE*SLOPE
     &              + DGSITE(ISPC)*ALOG(TSITE)
C
      END SELECT
C
      ATTEN(ISPC)=OBSERV(ISPC)
      SMCON(ISPC)=0.
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
