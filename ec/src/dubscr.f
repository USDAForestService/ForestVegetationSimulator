      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C EC $Id: dubscr.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND ARE LESS THAN 1.0 INCH DBH.  
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
      LOGICAL DEBUG
      INTEGER ISPC
      REAL TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
      REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),
     & CRSD(MAXSP),BCR5(MAXSP),BCR6(MAXSP),
     & BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP)
      REAL DANUW
C----------
C  SPECIES LIST FOR EAST CASCADES VARIANT.
C
C   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
C   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
C   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
C   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
C   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
C   6 = GRAND FIR               (GF)    ABIES GRANDIS
C   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
C   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
C   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
C  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
C  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
C  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
C  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
C  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
C  15 = NOBLE FIR               (NF)    ABIES PROCERA
C  16 = WHITE FIR               (WF)    ABIES CONCOLOR
C  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
C  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
C  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
C  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
C  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
C  22 = RED ALDER               (RA)    ALNUS RUBRA
C  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
C  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
C  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
C  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
C  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
C  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
C  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
C  30 = WILLOW SPECIES          (WI)    SALIX sp.
C  31 = OTHER SOFTWOODS         (OS)
C  32 = OTHER HARDWOODS         (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE EC VARIANT:
C      USE 6(GF) FOR 16(WF)
C      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
C
C  FROM THE WC VARIANT:
C      USE 19(WH) FOR 11(WH)
C      USE 33(PY) FOR 13(PY)
C      USE 31(WB) FOR 14(WB)
C      USE  7(NF) FOR 15(NF)
C      USE 30(LL) FOR 17(LL)
C      USE  8(YC) FOR 18(YC)
C      USE 29(WJ) FOR 19(WJ)
C      USE 21(BM) FOR 20(BM) AND 21(VN)
C      USE 22(RA) FOR 22(RA)
C      USE 24(PB) FOR 23(PB)
C      USE 25(GC) FOR 24(GC)
C      USE 34(DG) FOR 25(DG)
C      USE 26(AS) FOR 26(AS) AND 32(OH)
C      USE 27(CW) FOR 27(CW)
C      USE 28(WO) FOR 28(WO)
C      USE 36(CH) FOR 29(PL)
C      USE 37(WI) FOR 30(WI)
C----------
      DATA BCR0/
     & -1.669490, -1.669490,  -.426688,  -.426688,  -.426688,
     &  -.426688, -1.669490,  -.426688,  -.426688, -1.669490,
     &  7.558538,  -2.19723,  6.489813,  6.489813,  8.042774,
     &  -.426688,  6.489813,  7.558538,       9.0,       5.0,
     &       5.0,       5.0,       5.0,       5.0,       5.0,
     &       5.0,       5.0,       5.0,       5.0,       5.0,
     &  -2.19723,       5.0/
C
      DATA BCR1/
     &  -.209765,  -.209765,  -.093105,  -.093105,  -.093105,
     &  -.093105,  -.209765,  -.093105,  -.093105,  -.209765,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &  -.093105,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR2/
     &       0.0,       0.0,   .022409,   .022409,   .022409,
     &   .022409,       0.0,   .022409,   .022409,       0.0,
     & -0.015637,       0.0, -0.029815, -0.029815,  0.007198,
     &   .022409, -0.029815, -0.015637,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR3/
     &   .003359,   .003359,   .002633,   .002633,   .002633,
     &   .002633,   .003359,   .002633,   .002633,   .003359,
     & -0.009064,       0.0, -0.009276, -0.009276, -0.016163,
     &   .002633, -0.009276, -0.009064,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR5/
     &   .011032,   .011032,       0.0,       0.0,       0.0,
     &       0.0,   .011032,       0.0,       0.0,   .011032,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR6/
     &       0.0,       0.0,  -.045532,  -.045532,  -.045532,
     &  -.045532,       0.0,  -.045532,  -.045532,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &  -.045532,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR8/
     &   .017727,   .017727,       0.0,       0.0,       0.0,
     &       0.0,   .017727,       0.0,       0.0,   .017727,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR9/
     &  -.000053,  -.000053,   .000022,   .000022,   .000022,
     &   .000022,  -.000053,   .000022,   .000022,  -.000053,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &   .000022,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA BCR10/
     &   .014098,   .014098,  -.013115,  -.013115,  -.013115,
     &  -.013115,   .014098,  -.013115,  -.013115,   .014098,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &  -.013115,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0/
C
      DATA CRSD/
     &    0.5000,    0.5000,    0.6957,    0.6957,    0.6957,
     &    0.9310,    0.6124,    0.6957,    0.6957,    0.4942,
     &    1.9658,       0.2,    2.0426,    2.0426,    1.3167,
     &    0.9310,    2.0426,    1.9658,       0.5,       0.5,
     &       0.5,       0.5,       0.5,       0.5,       0.5,
     &       0.5,       0.5,       0.5,       0.5,       0.5,
     &       0.2,       0.5/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = TPCT
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
C----------
      SELECT CASE (ISPC)
C-----------
C  SPECIES USING EC VARIANT EQUATIONS
C----------
      CASE(1:10,12,16,31)
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
   10   FCR=BACHLO(0.0,SD,RANN)
        IF(ABS(FCR).GT.SD) GO TO 10
        IF(ABS(CR+FCR).GE.86.)CR=86.
        CR=1.0/(1.0+EXP(CR+FCR))
        IF(CR .GT. .95) CR = .950
        IF(CR .LT. .05) CR=.05
C       IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
C 600   FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
C    &   ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
C    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
C
C-----------
C  SPECIES USING WC VARIANT EQUATIONS
C----------
      CASE(11,13:15,17:30,32)
C----------
C  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, HEIGHT,
C  AND BASAL AREA. THE EQUATION RETURNS A CROWN CODE VALUE 0-9
C----------
        CR=BCR0(ISPC) + BCR2(ISPC)*H + BCR3(ISPC)*BA
C----------
C  ASSIGN A RANDOM ERROR TO THE CROWN RATIO PREDICTION
C  VALUES OF CRSD ARE THE STANDARD ERROR VALUES FROM REGRESSION
C----------
        SD=CRSD(ISPC)
   20   FCR=BACHLO(0.0,SD,RANN)
        IF(ABS(FCR).GT.SD) GO TO 20
        IF(DEBUG)WRITE(JOSTND,*)' IN DUBSCR H,BA,CR,FCR= ',
     &  H,BA,CR,FCR
        CR=CR+FCR
C----------
C  CONVERT CODE VALUE TO A CROWN RATIO 0-100.
C----------
        CR=((CR-1.0)*10.0 + 1.0) / 100.
        IF(CR .GT. .95) CR=.95
        IF(CR .LT. .05) CR=.05
        IF(DEBUG)WRITE(JOSTND,602)ISPC,H,BA,CR
  602   FORMAT(' IN DUBSCR, ISPC= ',I2,' H= ',F5.1,' BA= ',F9.4,
     &  ' CR= ',F4.3)
C
      END SELECT
C
      RETURN
      END
