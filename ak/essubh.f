      SUBROUTINE ESSUBH (I,HHT,DELAY,GENTIM,TRAGE,JOSTND,DEBUG)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
C     CREATED BY THE ESTABLISHMENT MODEL.
C     ALL SPECIES EXCEPT OTHER SOFTWOODS AND OTHER HARDWOODS USE
C     ORIGINAL EQUATION TO ESTIMATE HEIGHT BASED ON TIME AND A 
C     RANDOM NUMBER.
C     OTHER SOFTWOODS AND OTHER HARDWOODS SET TO MINIMUM HEIGHT.
C
C     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
C     THE PLANT OR NATURAL KEYWORD. LEAVING ESSUBH, TRAGE IS THE NUMBER
C     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
C     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
C     THE TREE.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOM2.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'ESHAP.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DEFINITIONS:
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
C    I      -- SPECIES NUMBER
C   BB      -- INTERMEDIATE VARIABLE
C    X      -- RANDOM NUMBER DRAW   
C  HHT      -- HEIGHT OF BEST TREE
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL DEBUG
C
      INTEGER I,IAGE,ITIME,N,JOSTND,MODE1
C
      REAL AGE,DELAY,GENTIM,HHT,TRAGE
      REAL H,HTGR,HTMAX
C
C----------
      IF(DEBUG)WRITE(JOSTND,*)' IN ESSUBH (1)',' DELAY=',DELAY,' N=',N,
     & ' TRAGE=',TRAGE,' GENTIM=',GENTIM,' TIME=',TIME

      N = INT(DELAY+0.5)
      IF(N.LT.-3) N=-3
      DELAY=REAL(N)
      ITIME = INT(TIME+0.5)
      IF(N.GT.ITIME) DELAY=TIME
      AGE=TIME-DELAY-GENTIM
      IAGE = INT(AGE+0.5)
      IF(IAGE.LT.1) IAGE=1
      AGE=AGE+TRAGE
      IF(AGE.LT.1.0) AGE=1.0
      TRAGE=TIME-DELAY

C  INITALIZE VARIABLES NEEDED FOR HTCALC
      MODE1= 1
      HTGR=0.0
      H=0.0
      HTMAX=0.0

C  CALL HTCALC TO RETRIEVE A HEIGHT BASED ON INPUT AGE AND SITE INDEX
C  SITE INDEX IS EXTRACTED WITHIN HTCALC
      CALL HTCALC(MODE1,I,AGE,H,HTMAX,HTGR,JOSTND,DEBUG)
      HHT=H

      IF(DEBUG)WRITE(JOSTND,*)' IN ESSUBH (2)',' DELAY=',DELAY,' N=',N,
     & ' TRAGE=',TRAGE,' GENTIM=',GENTIM,' AGE=',AGE,' TIME=',TIME,
     & ' IAGE=',IAGE,' ITIME=',ITIME,' HHT=',HHT

C----------
C  HEIGHTS TO TALL, TEMPORARY FIX 12-21-20 MC.
C  SET HTT TO VALUE IN XMIN
C----------
      IF(HHT.LT.XMIN(I))HHT=XMIN(I)
      RETURN
      END
