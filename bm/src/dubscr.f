      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C BM $Id: dubscr.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND ARE LESS THAN 5.0 INCHES DBH.  FINALLY, IT IS
C  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
C  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.
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
      REAL D,H,CR,TPCT,TPCCF,BACHLO,FCR,SD
      REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),
     & CRSD(MAXSP),BCR5(MAXSP),BCR6(MAXSP),
     & BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP)
      REAL DANUW
C
      DATA BCR0/
     & -1.669490, -1.669490,  -.426688,  -.426688,  -.426688,  -2.19723,
     & -1.669490,  -.426688,  -.426688, -1.669490,  -1.66949,  -1.66949,
     &  6.489813,  7.558538,  -.426688,       5.0, -1.669490,       5.0/
      DATA BCR1/
     &  -.209765,  -.209765,  -.093105,  -.093105,  -.093105,       0.0,
     &  -.209765,  -.093105,  -.093105,  -.209765,  -.209765,  -.209765,
     &       0.0,       0.0,  -.093105,       0.0,  -.209765,       0.0/
      DATA BCR2/
     &   .000000,   .000000,   .022409,   .022409,   .022409,       0.0,
     &   .000000,   .022409,   .022409,   .000000,       0.0,       0.0,
     & -0.029815, -0.015637,   .022409,       0.0,   .000000,       0.0/
      DATA BCR3/
     &   .003359,   .003359,   .002633,   .002633,   .002633,       0.0,
     &   .003359,   .002633,   .002633,   .003359,   .003359,   .003359,
     & -0.009276, -0.009064,   .002633,       0.0,   .003359,       0.0/
      DATA BCR5/
     &   .011032,   .011032,   .000000,   .000000,   .000000,       0.0,
     &   .011032,   .000000,   .000000,   .011032,   .011032,   .011032,
     &       0.0,       0.0,       0.0,       0.0,   .011032,       0.0/
      DATA BCR6/
     &   .000000,   .000000,  -.045532,  -.045532,  -.045532,       0.0,
     &   .000000,  -.045532,  -.045532,   .000000,       0.0,       0.0,
     &       0.0,       0.0,  -.045532,       0.0,   .000000,       0.0/
      DATA BCR8/
     &   .017727,   .017727,   .000000,   .000000,   .000000,       0.0,
     &   .017727,   .000000,   .000000,   .017727,   .017727,   .017727,
     &       0.0,       0.0,       0.0,       0.0,   .017727,       0.0/
      DATA BCR9/
     &  -.000053,  -.000053,   .000022,   .000022,   .000022,       0.0,
     &  -.000053,   .000022,   .000022,  -.000053,  -.000053,  -.000053,
     &       0.0,       0.0,   .000022,       0.0,  -.000053,       0.0/
      DATA BCR10/
     &   .014098,   .014098,  -.013115,  -.013115,  -.013115,       0.0,
     &   .014098,  -.013115,  -.013115,   .014098,   .014098,   .014098,
     &       0.0,       0.0,  -.013115,       0.0,   .014098,       0.0/
      DATA CRSD/
     &  .5000, .5000, .6957, .6957, .6957, 0.200,
     &  .6124, .6957, .6957, .4942, .5000, .5000,
     & 2.0426,1.9658, .9310, 0.500, .4942, 0.500/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = TPCT
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DUBSCR  CYCLE =',I5)
C----------
C  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
C  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
C  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
C----------
      CR = BCR2(ISPC)*H
     *   + BCR1(ISPC)*D
     *   + BCR5(ISPC)*TPCCF
     *   + BCR6(ISPC)*(AVH/H)
     *   + BCR8(ISPC)*AVH
     *   + BCR3(ISPC)*BA
     *   + BCR9(ISPC)*(BA*TPCCF)
     *   + BCR10(ISPC)*RMAI
     *   + BCR0(ISPC)
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
C
      CASE(1:12,15,17)
      IF(ABS(CR+FCR).GE.86.)CR=86.
      CR=1.0/(1.0+EXP(CR+FCR))
C
      CASE(13,14,16,18)
        CR=CR+FCR
        CR=((CR-1.0)*10.0 + 1.0)/100.
      END SELECT
C
      IF(CR .GT. .95) CR = .95
      IF(CR .LT. .05) CR=.05
      IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,BA,TPCCF,CR,FCR,RMAI,AVH
  600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
     & ' BA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
     &   ' RAN ERR = ',F9.4,' RMAI= ',F9.4,' AVH=',F9.4)
      RETURN
      END
