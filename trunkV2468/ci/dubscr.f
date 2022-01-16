      SUBROUTINE DUBSCR(ISPC,D,H,BA,CR,TPCCF,AVH,TMAI)
      IMPLICIT NONE
C----------
C  **DUBSCR--CI   DATE OF LAST REVISION:  08/31/11
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND ARE LESS THAN 1.0 INCHES DBH.  FINALLY, IT IS
C  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
C  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.
C
C NOTE: 17=CW AND 19=OH DO NOT USE THIS ROUTINE; CROWNS FOR TREES OF
C       ALL SIZES FOR THESE SPECIES ARE DUBBED IN **CROWN**.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
COMMONS
C----------
      EXTERNAL RANN
      INTEGER ISPC
      REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),CRSD(MAXSP)
      REAL BCR5(MAXSP),BCR6(MAXSP),BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP)
      REAL TMAI,AVH,TPCCF,CR,BA,H,D,SD,FCR,BACHLO
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
C  DATA STATEMENTS
C----------
      DATA BCR0/
     &  -0.44316, -0.83965, -0.89122, -0.62646, -0.49548,
     &   0.11847, -0.32466, -0.92007, -0.89014, -0.17561,
     &  -1.66949, -1.66949,-0.426688, -2.19723,      5.0,
     &  -1.66949,      0.0, -0.49548,      0.0/
C
      DATA BCR1/
     &  -0.48446, -0.16106, -0.18082, -0.06141,  0.00012,
     &  -0.39305, -0.20108, -0.22454, -0.18026, -0.33847,
     & -0.209765,-0.209765,-0.093105,      0.0,      0.0,
     & -0.209765,      0.0,  0.00012,      0.0/
C
      DATA BCR2/
     &   0.05825,  0.04161,  0.05186,  0.02360,  0.00362,
     &   0.02783,  0.04219,  0.03248,  0.02233,  0.05699,
     &       0.0,      0.0, 0.022409,      0.0,      0.0,
     &       0.0,      0.0,  0.00362,      0.0/
C
      DATA BCR3/
     &   0.00513,  0.00602,  0.00454,  0.00505,  0.00456,
     &   0.00626,  0.00436,  0.00620,  0.00614,  0.00692,
     &  0.003359, 0.003359, 0.002633,      0.0,      0.0,
     &  0.003359,      0.0,  0.00456,      0.0/
C
      DATA BCR5/
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &  0.011032, 0.011032,      0.0,      0.0,      0.0,
     &  0.011032,      0.0,      0.0,      0.0/
C
      DATA BCR6/
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &       0.0,      0.0, -.045532,      0.0,      0.0,
     &       0.0,      0.0,      0.0,      0.0/
C
      DATA BCR8/
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &  0.017727, 0.017727,      0.0,      0.0,      0.0,
     &  0.017727,      0.0,      0.0,      0.0/
C
      DATA BCR9/
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &  -.000053, -.000053,  .000022,      0.0,      0.0,
     &  -.000053,      0.0,      0.0,      0.0/
C
      DATA BCR10/
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,      0.0,      0.0,
     &   .014098,  .014098, -.013115,      0.0,      0.0,
     &   .014098,      0.0,      0.0,      0.0/
C
      DATA CRSD/
     &    0.9476,   0.7396,   0.8706,   0.9203,   0.9450,
     &    0.8012,   0.7707,   0.9721,   0.8871,   0.8866,
     &       0.5,      0.5,   0.9310,      0.2,      0.5,
     &       0.5,      0.0,   0.9450,      0.0/
C----------
C  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, HEIGHT, AND
C  BASAL AREA.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
C  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
C----------
      CR=BCR0(ISPC) + BCR1(ISPC)*D + BCR2(ISPC)*H + BCR3(ISPC)*BA
     &   + BCR5(ISPC)*TPCCF + BCR6(ISPC)*(AVH/H) + BCR8(ISPC)*AVH
     &   + BCR9(ISPC)*(BA*TPCCF) + BCR10(ISPC)*TMAI
C----------
C  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
C  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
C  WAS USED TO FIT THE MODEL AND THE ELEMENTS OF CRSD ARE THE
C  STANDARD ERRORS FOR THE LINEARIZED MODEL BY SPECIES.
C----------
      SD=CRSD(ISPC)
   10 CONTINUE
      FCR=0.0
      IF (DGSD.GE.1.0) FCR=BACHLO(0.0,SD,RANN)
      IF(ABS(FCR).GT.SD) GO TO 10
C
      SELECT CASE (ISPC)
      CASE(15)
        CR=((CR-1.0)*10.0 + 1.0)/100.
      CASE DEFAULT
        IF(ABS(CR+FCR).GE.86.)CR=86.
        CR=1.0/(1.0+EXP(CR+FCR))
      END SELECT
C
      IF(CR.LT.0.05) CR=0.05
      IF(CR.GT.0.95) CR=0.95
C
      RETURN
      END
