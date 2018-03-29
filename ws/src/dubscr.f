      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C WS $Id: dubscr.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND ARE LESS THAN 5.0 INCHES DBH.  FINALLY, IT IS
C  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
C  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.
C  THIS ROUTINE WAS MOVED FROM THE SO11 SOURCE DIRECTORY TO EC
C  WHEN THE SO11 MODEL WAS RETIRED IN JUNE 2010. IT IS USED BY
C  THE EC, NC, AND WS VARIANTS
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
COMMONS
C----------
      EXTERNAL RANN
      INTEGER ISPC
      REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),BCR5(MAXSP),
     & BCR6(MAXSP),BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP),CRSD(MAXSP)
      REAL TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
      REAL DANUW
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
      DATA BCR0/
     & -1.669490, -0.426688, -0.426688, -0.426688, -0.426688,
     & -0.426688, -0.426688, -1.669490,  6.489813,  6.489813,
     & -1.669490,  6.489813, -0.426688,  6.489813,  6.489813,
     &  6.489813,  6.489813, -1.669490,  6.489813,  6.489813,
     &  6.489813, -0.426688, -0.426688, -1.669490,  6.489813,
     &  6.489813,  6.489813, -1.669490, -1.669490, -1.669490,
     & -1.669490, -1.669490, -1.669490,  -2.19723,  -2.19723,
     &  -2.19723,  -2.19723,  -2.19723,  -2.19723, -1.669490,
     &       5.0, -1.669490, -1.669490/
C
      DATA BCR1/
     & -0.209765, -0.093105, -0.093105, -0.093105, -0.093105,
     & -0.093105, -0.093105, -0.209765,       0.0,       0.0,
     & -0.209765,       0.0, -0.093105,       0.0,       0.0,
     &       0.0,       0.0, -0.209765,       0.0,       0.0,
     &       0.0, -0.093105, -0.093105, -0.209765,       0.0,
     &       0.0,       0.0, -0.209765, -0.209765, -0.209765,
     & -0.209765, -0.209765, -0.209765,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0, -0.209765,
     &       0.0, -0.209765, -0.209765/
C
      DATA BCR2/
     &       0.0,  0.022409,  0.022409,  0.022409,  0.022409,
     &  0.022409,  0.022409,       0.0, -0.029815, -0.029815,
     &       0.0, -0.029815,  0.022409, -0.029815, -0.029815,
     & -0.029815, -0.029815,       0.0, -0.029815, -0.029815,
     & -0.029815,  0.022409,  0.022409,       0.0, -0.029815,
     & -0.029815, -0.029815,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0/
C
      DATA BCR3/
     &  0.003359,  0.002633,  0.002633,  0.002633,  0.002633,
     &  0.002633,  0.002633,  0.003359, -0.009276, -0.009276,
     &  0.003359, -0.009276,  0.002633, -0.009276, -0.009276,
     & -0.009276, -0.009276,  0.003359, -0.009276, -0.009276,
     & -0.009276,  0.002633,  0.002633,  0.003359, -0.009276,
     & -0.009276, -0.009276,  0.003359,  0.003359,  0.003359,
     &  0.003359,  0.003359,  0.003359,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.003359,
     &       0.0,  0.003359,  0.003359/
C
      DATA BCR5/
     &  0.011032,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  0.011032,       0.0,       0.0,
     &  0.011032,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  0.011032,       0.0,       0.0,
     &       0.0,       0.0,       0.0,  0.011032,       0.0,
     &       0.0,       0.0,  0.011032,  0.011032,  0.011032,
     &  0.011032,  0.011032,  0.011032,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.011032,
     &       0.0,  0.011032,  0.011032/
C
      DATA BCR6/
     &       0.0, -0.045532, -0.045532, -0.045532, -0.045532,
     & -0.045532, -0.045532,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.045532,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0, -0.045532, -0.045532,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0/
C
      DATA BCR8/
     &  0.017727,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  0.017727,       0.0,       0.0,
     &  0.017727,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,  0.017727,       0.0,       0.0,
     &       0.0,       0.0,       0.0,  0.017727,       0.0,
     &       0.0,       0.0,  0.017727,  0.017727,  0.017727,
     &  0.017727,  0.017727,  0.017727,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.017727,
     &       0.0,  0.017727,  0.017727/
C
      DATA BCR9/
     & -0.000053,  0.000022,  0.000022,  0.000022,  0.000022,
     &  0.000022,  0.000022, -0.000053,       0.0,       0.0,
     & -0.000053,       0.0,  0.000022,       0.0,       0.0,
     &       0.0,       0.0, -0.000053,       0.0,       0.0,
     &       0.0,  0.000022,  0.000022, -0.000053,       0.0,
     &       0.0,       0.0, -0.000053, -0.000053, -0.000053,
     & -0.000053, -0.000053, -0.000053,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0, -0.000053,
     &       0.0, -0.000053, -0.000053/
C
      DATA BCR10/
     &  0.014098, -0.013115, -0.013115, -0.013115, -0.013115,
     & -0.013115, -0.013115,  0.014098,       0.0,       0.0,
     &  0.014098,       0.0, -0.013115,       0.0,       0.0,
     &       0.0,       0.0,  0.014098,       0.0,       0.0,
     &       0.0, -0.013115, -0.013115,  0.014098,       0.0,
     &       0.0,       0.0,  0.014098,  0.014098,  0.014098,
     &  0.014098,  0.014098,  0.014098,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,  0.014098,
     &       0.0,  0.014098,  0.014098/
C
      DATA CRSD/
     &       0.5,    0.6957,    0.6957,    0.6957,    0.9310,
     &    0.6957,    0.6957,    0.4942,    2.0426,    2.0426,
     &       0.5,    2.0426,    0.6957,    2.0426,    2.0426,
     &    2.0426,    2.0426,    0.4942,    2.0426,    2.0426,
     &    2.0426,    0.6957,    0.6957,       0.5,    2.0426,
     &    2.0426,    2.0426,    0.6124,    0.6124,    0.6124,
     &    0.6124,    0.6124,    0.6124,       0.2,       0.2,
     &       0.2,       0.2,       0.2,       0.2,    0.6124,
     &       0.5,       0.5,    0.6124/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = TPCT
C-----------
C  CHECK FOR DEBUG.
C-----------
C     CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
C----------
C  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
C  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
C  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
C----------
      CR = BCR0(ISPC)
     *   + BCR1(ISPC)*D
     *   + BCR2(ISPC)*H
     *   + BCR3(ISPC)*BA
     *   + BCR5(ISPC)*TPCCF
     *   + BCR6(ISPC)*(AVH/H)
     *   + BCR8(ISPC)*AVH
     *   + BCR9(ISPC)*(BA*TPCCF)
     *   + BCR10(ISPC)*RMAI
C----------
C  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
C  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
C  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
C  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.
C----------
      SD=CRSD(ISPC)
   10 FCR=BACHLO(0.0,SD,RANN)
      IF(ABS(FCR).GT.SD) GO TO 10
C
      SELECT CASE (ISPC)
      CASE (9:10,12,14:17,19:20,25:27)
        CR=CR+FCR
        CR=((CR-1.0)*10.0 + 1.0)/100.
      CASE (41)
        CR=((CR-1.0)*10.0 + 1.0)/100.
      CASE DEFAULT
        IF(ABS(CR+FCR).GE.86.)CR=86.
        CR=1.0/(1.0+EXP(CR+FCR))
      END SELECT
C
      IF(CR .GT. .95) CR = .950
      IF(CR .LT. .05) CR=.05
C     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
C 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
C    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
C    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
C
C
      RETURN
      END
