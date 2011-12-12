      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C  **DUBSCR--EM   DATE OF LAST REVISION:  03/26/09
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND ARE LESS THAN 1.0 (3.0 FOR SOME SPECIES) INCHES DBH.  
C  FINALLY, IT ISUSED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
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
C----------
C  SPECIES ORDER:
C   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
C  17=PB, 18=OS, 19=OH
C
C  SPECIES EXPANSION
C  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
C  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
C  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
C  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
C  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)
C----------
C  NOTE: THE ORIGINAL SOURCE OF MOST OF THESE EQUATIONS GOES BACK
C  TO THE 11 SPECIES VERSIONS OF THE SO AND NI VARIANTS.
C  LL IS THE NI AF EQUATION;
C  RM USES THE UT JU EQUATION;
C  ALL OTHER SPECIES USE EQUATIONS FROM SO. 
C
C  ALSO NOTE THAT THE STANDARD DEVIATIONS HAVE BEEN MANIPULATED TO
C  GET SOME SPECIES DIFFERENCES (FOR EXAMPLE, IN SO, WB, WL, LP, AND
C  PP ALL USE THE SAME EQUATION BUT THE STANDARD DEVIATIONS HAVE BEEN
C  MODIFIED TO BETTER REPRESENT THE SPECIES.)
C----------
      DATA BCR0/
     & -1.669490, -1.669490,  -.426688,  -1.66949,   -.89014,        0.,
     & -1.669490,  -.426688,  -.426688, -1.669490,        0.,  -.426688,
     &        0.,        0.,        0.,        0.,  -.426688,  -2.19723,
     &        0./
      DATA BCR1/
     &  -.209765,  -.209765,  -.093105,  -.209765,  -0.18026,        0.,
     &  -.209765,  -.093105,  -.093105,  -.209765,        0.,  -.093105,
     &        0.,        0.,        0.,        0.,  -.093105,        0.,
     &        0./
      DATA BCR2/
     &        0.,        0.,   .022409,        0.,    .02233,        0.,
     &        0.,   .022409,   .022409,        0.,        0.,   .022409,
     &        0.,        0.,        0.,        0.,   .022409,        0.,
     &        0./
      DATA BCR3/
     &   .003359,   .003359,   .002633,   .003359,    .00614,        0.,
     &   .003359,   .002633,   .002633,   .003359,        0.,   .002633,
     &        0.,        0.,        0.,        0.,   .002633,        0.,
     &        0./
      DATA BCR5/
     &   .011032,   .011032,        0.,   .011032,        0.,        0.,
     &   .011032,        0.,        0.,   .011032,        0.,        0.,
     &        0.,        0.,        0.,        0.,        0.,        0.,
     &        0./
      DATA BCR6/
     &        0.,        0.,  -.045532,        0.,        0.,        0.,
     &        0.,  -.045532,  -.045532,        0.,        0.,  -.045532,
     &        0.,        0.,        0.,        0.,  -.045532,        0.,
     &        0./
      DATA BCR8/
     &   .017727,   .017727,        0.,   .017727,        0.,        0.,
     &   .017727,        0.,        0.,   .017727,        0.,        0.,
     &        0.,        0.,        0.,        0.,        0.,        0.,
     &        0./
      DATA BCR9/
     &  -.000053,  -.000053,   .000022,  -.000053,        0.,        0.,
     &  -.000053,   .000022,   .000022,  -.000053,        0.,   .000022,
     &        0.,        0.,        0.,        0.,   .000022,        0.,
     &        0./
      DATA BCR10/
     &   .014098,   .014098,  -.013115,   .014098,        0.,        0.,
     &   .014098,  -.013115,  -.013115,   .014098,        0.,  -.013115,
     &        0.,        0.,        0.,        0.,  -.013115,        0.,
     &        0./
      DATA CRSD/
     & .5000, .5000, .6957, .5000, .8871,    0., .6124, .6957, .6957,
     & .4942,    0., .9310,    0.,    0.,    0.,    0., .9310,  .200,
     &    0./
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
      CR=BCR0(ISPC) + BCR1(ISPC)*D + BCR2(ISPC)*H + BCR3(ISPC)*BA
     &   + BCR5(ISPC)*TPCCF + BCR6(ISPC)*(AVH/H) + BCR8(ISPC)*AVH
     &   + BCR9(ISPC)*(BA*TPCCF) + BCR10(ISPC)*RMAI
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
      IF(ABS(CR+FCR).GE.86.)CR=86.
      CR=1.0/(1.0+EXP(CR+FCR))
      IF(CR .GT. .95) CR = .950
      IF(CR .LT. .05) CR=.05
C     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
C 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
C    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
C    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
      RETURN
      END
