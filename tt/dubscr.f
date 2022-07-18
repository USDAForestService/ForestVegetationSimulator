      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C TT $Id$
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
      REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),
     & BCR5(MAXSP),BCR6(MAXSP),
     & BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP),CRSD(MAXSP)
      REAL TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
      REAL RDANUW
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C----------
      DATA BCR0/
     & -1.669490, -1.669490,  -.426688,  -2.19723,  -.426688,
     &  -.426688, -1.669490,  -.426688,  -.426688,  -0.17561,
     &  -2.19723,  -2.19723,       5.0,  -.426688,  -.426688,
     &       5.0,  -2.19723,  -.426688/
C
      DATA BCR1/
     &  -.209765,  -.209765,  -.093105,       0.0,  -.093105,
     &  -.093105,  -.209765,  -.093105,  -.093105,  -0.33847,
     &       0.0,       0.0,       0.0,  -.093105,  -.093105,
     &       0.0,       0.0,  -.093105/
C
      DATA BCR2/
     &       0.0,       0.0,   .022409,       0.0,   .022409,
     &   .022409,       0.0,   .022409,   .022409,   0.05699,
     &       0.0,       0.0,       0.0,   .022409,   .022409,
     &       0.0,       0.0,   .022409/
C
      DATA BCR3/
     &   .003359,   .003359,   .002633,       0.0,   .002633,
     &   .002633,   .003359,   .002633,   .002633,   0.00692,
     &       0.0,       0.0,       0.0,   .002633,   .002633,
     &       0.0,       0.0,   .002633/
C
      DATA BCR5/
     &   .011032,   .011032,       0.0,       0.0,       0.0,
     &       0.0,   .011032,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0/
C
      DATA BCR6/
     &       0.0,       0.0,  -.045532,       0.0,  -.045532,
     &  -.045532,       0.0,  -.045532,  -.045532,       0.0,
     &       0.0,       0.0,       0.0,  -.045532,  -.045532,
     &       0.0,       0.0,  -.045532/
C
      DATA BCR8/
     &   .017727,   .017727,       0.0,       0.0,       0.0,
     &       0.0,   .017727,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0/
C
      DATA BCR9/
     &  -.000053,  -.000053,   .000022,       0.0,   .000022,
     &   .000022,  -.000053,   .000022,   .000022,       0.0,
     &       0.0,       0.0,       0.0,   .000022,   .000022,
     &       0.0,       0.0,   .000022/
C
      DATA BCR10/
     &   .014098,   .014098,  -.013115,       0.0,  -.013115,
     &  -.013115,   .014098,  -.013115,  -.013115,       0.0,
     &       0.0,       0.0,       0.0,  -.013115,  -.013115,
     &       0.0,       0.0,  -.013115/
C
      DATA CRSD/
     &     .5000,     .5000,     .6957,     .2000,     .6957,
     &     .9310,     .6124,     .6957,     .6957,     .8866,
     &     .2000,     .2000,     .5000,     .9310,     .6957,
     &     .5000,       0.2,     .6957/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = TPCT
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
   10 CONTINUE
      FCR=0.0
      IF (DGSD.GE.1.0) FCR=BACHLO(0.0,SD,RANN)
      IF(ABS(FCR).GT.SD) GO TO 10
C
      SELECT CASE (ISPC)
      CASE(13,16)
        CR=((CR-1.0)*10.0 + 1.0)/100.
      CASE DEFAULT
        IF(ABS(CR+FCR).GE.86.)CR=86.
        CR=1.0/(1.0+EXP(CR+FCR))
      END SELECT
C
      IF(CR .GT. .95) CR = .950
      IF(CR .LT. .05) CR=.05
      IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TPCCF,CR,FCR,RMAI
  600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
     & ' TPCCF=',F8.4,' CR=',F4.3,
     &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4)
C
      RETURN
      END
