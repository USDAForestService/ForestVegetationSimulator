      SUBROUTINE DUBSCR(ISPC,D,H,PRD,QMDPLT,CR)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL. IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND MEET CERTAIN CRITERIA (DEAD OR DBH < 1 INCH).
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
      INCLUDE 'CRCOEF.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
      LOGICAL DEBUG
C
      EXTERNAL RANN
C
      INTEGER ISPC
C
      REAL BACHLO,CR,D,FCR,H,SD,PRD,QMDPLT
C
      REAL CRSD(MAXSP)

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
C   6     LS   Lutz�s spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TN
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TB
C
C----------
C
C    STANDARD ERROR OF REGRESSIONS FOR EACH SPECIES
      DATA CRSD/
     &  0.1719, 0.1719, 0.159,  0.1645, 0.1542,
     &  0.1542, 0.1894, 0.1719, 0.1568, 0.1811,
     &  0.1582, 0.155,  0.1542, 0.1447, 0.1447,
     &  0.144,  0.144,  0.1605, 0.1356, 0.1605,
     &  0.0938, 0.0938, 0.1605/
C
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK(DEBUG,'DUBSCR',6,ICYC)

C----------
C  CALCULATE CROWN RATIO USING THE FOLLOWING LOGISITC EQUATION 
C  FORM:

C  CR = 1/(1 + EXP(X)))
C  X = B1 + B2*HDR + B3*PRD + B4*DBH/QMD

C  WHERE
C  CR: CROWN RATIO
C  HDR: HEIGHT DIAMETER RATIO OF TREE RECORD
C  PRD: POINT LEVEL RELATIVE DENSITY
C  DBH: DIAMETER AT BREAST HEIGHT
C  QMDPLT: POINT LEVEL QUADRATIC MEAN DIAMETER.
C----------

      CR = CRINT(ISPC)
     &   + CRHDR(ISPC) * LOG((H*12/D))
     &   + CRRD(ISPC) * PRD
     &   + CRDQMD(ISPC) * (D/QMDPLT)

C----------
C  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
C  PRIOR TO THE LOGISTIC TRANSFORMATION. THE ELEMENTS OF CRSD
C  ARE THE STANDARD ERRORS FROM THE FINAL CROWN RATIO EQUATIONS.
C----------
C     EXTRACT STANDARD ERROR AND DETERMINE RANDOM DRAW FROM
C     NORMAL DISTRIBUTION
      SD=CRSD(ISPC)
   10 FCR=BACHLO(0.0,SD,RANN)
      IF(ABS(FCR).GT.SD) GO TO 10
C
C     CALCULATE CROWN RATIO
      CR=1.0/(1.0+EXP(CR + FCR))
C
C     DETERMINE IF CROWN RATIO SHOULD BE CONSTRAINED
      IF(CR .GT. .95) CR = 0.95
      IF(CR .LT. .05) CR= 0.05

C  DO SOME DEBUG
      IF(DEBUG)WRITE(JOSTND,*)'IN DUBSCR',' ISPC=',ISPC,' D=',D,' H=',H,
     &  ' PRD=',PRD,' QMDPLT=',QMDPLT,' FCR=',FCR,' CR=',CR

C     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
C 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
C    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
C    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
C
      RETURN
      END
