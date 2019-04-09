      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND MEET CERTAIN CRITERIA.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      EXTERNAL RANN
C
      INTEGER ISPC
C
      REAL BACHLO,CR,D,FCR,H,RDANUW,SD,TPCCF,TPCT
C
      REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP)
      REAL BCR5(MAXSP),BCR6(MAXSP),BCR8(MAXSP),BCR9(MAXSP)
      REAL BCR10(MAXSP),CRSD(MAXSP)
C----------
C  DATA STATEMENTS:
C
C----------
C SPECIES LIST FOR ALASKA VARIANT.
C
C Number Code  Common Name         FIA  PLANTS Scientific Name
C   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4     TA   tamarack            071  LALA   Larix laricina
C   5     WS   white spruce        094  PIGL   Picea glauca
C   6     LS   Lutz’s spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TE
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TD
C
C----------
      DATA BCR0/
     & -1.669490, -1.669490,  -.426688,  -.426688,  -.426688,  -.426688,
     & -1.669490,  -.426688,  -.426688,  5.,  5., -1.669490,  -2.19723/
C
      DATA BCR1/
     &  -.209765,  -.209765,  -.093105,  -.093105,  -.093105,  -.093105,
     &  -.209765,  -.093105,  -.093105, .0, .0,     -.209765,   .000000/
C
      DATA BCR2/
     &  .000000,  .000000,  .022409,  .022409,  .022409,  .022409,
     &  .000000,  .022409,  .022409,  .0, .0,   .000000,  .000000/
C
      DATA BCR3/
     &   .003359,   .003359,   .002633,   .002633,   .002633,   .002633,
     &   .003359,   .002633,   .002633,   .0, .0, .003359,   .000000/
C
      DATA BCR5/
     &   .011032,   .011032,   .000000,   .000000,   .000000,   .000000,
     &   .011032,   .000000,   .000000,  .0, .0,  .011032,   .000000/
C
      DATA BCR6/
     &   .000000,   .000000,  -.045532,  -.045532,  -.045532,  -.045532,
     &   .000000,  -.045532,  -.045532,   .0, .0, .000000,   .000000/
C
      DATA BCR8/
     &   .017727,   .017727,   .000000,   .000000,   .000000,   .000000,
     &   .017727,   .000000,   .000000,   .0, .0, .017727,   .000000/
C
      DATA BCR9/
     &  -.000053,  -.000053,   .000022,   .000022,   .000022,   .000022,
     &  -.000053,   .000022,   .000022,  .0, .0, -.000053,   .000000/
C
      DATA BCR10/
     &   .014098,   .014098,  -.013115,  -.013115,  -.013115,  -.013115,
     &   .014098,  -.013115,  -.013115,   .0, .0, .014098,   .000000/
C
      DATA CRSD/
     &  .5000,.5000,.6957,.6957,.6957,.9310,
     &  .6124,.6957,.6957,.5,.5,.4942,0.200/
C-----------
C  CHECK FOR DEBUG.
C-----------
C     CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = TPCT
C----------
C  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
C  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
C  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
C  EXCEPT RED ALDER & COTTONWOOD WHICH PREDICTS CODE 0-9
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
      IF(ISPC.EQ.10 .OR. ISPC.EQ.11) THEN
        CR=CR+FCR
        CR=((CR-1.0)*10.0 +1.0)/100.
      ELSE
        CR=1.0/(1.0+EXP(CR+FCR))
      ENDIF
      IF(CR .GT. .95) CR = .950
      IF(CR .LT. .05) CR=.05
C     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
C 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
C    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
C    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
C
      RETURN
      END
