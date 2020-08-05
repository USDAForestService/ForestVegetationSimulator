      SUBROUTINE ESADVH (I,HHT,DELAY,GENTIM,TRAGE)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     CALCULATES HEIGHTS OF ADVANCE TREES CREATED BY THE 
C     ESTABLISHMENT MODEL. 
C     ALL SPECIES EXCEPT OTHER SOFTWOODS AND OTHER HARDWOODS USE
C     ORIGINAL EQUATION TO ESTIMATE HEIGHT BASED ON TIME AND A 
C     RANDOM NUMBER.
C     OTHER SOFTWOODS AND OTHER HARDWOODS SET TO MINIMUM HEIGHT.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ESPARM.F77'

      INCLUDE 'ESCOM2.F77'

      INCLUDE 'ESCOMN.F77'

      INCLUDE 'ESHAP.F77'
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
C  AGE      -- AGE OF BEST TREE
C    I      -- SPECIES NUMBER
C   BB      -- INTERMEDIATE VARIABLE
C    X      -- RANDOM NUMBER DRAW   
C  HHT      -- HEIGHT OF BEST TREE
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER I,N
C
      REAL AGE,BB,DELAY,GENTIM,HHT,TRAGE,X
C
C----------
      N = INT(DELAY+0.5)
      IF(N.GT.2) N=1
      DELAY=REAL(N)
      TRAGE=3.0-DELAY
      AGE=3.0-DELAY-GENTIM
      IF(AGE.LT.1.0) AGE=1.0
C     SELECT SPECIES
      SELECT CASE (I)
        CASE (1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22)
          BB = -0.26203 + 0.44249*TIME
   11     CALL ESRANN(X)
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 11
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 11
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB  
        CASE (13)  
          HHT = 0.5
        CASE (23)  
          HHT = 1.0         
      END SELECT 
C     MAKE SURE HEIGHT IS ABOVE MINIMUM HEIGHT (XMIN SET IN BLKDAT.F)
      IF(HHT.LT.XMIN(I))HHT=XMIN(I)
      RETURN
      END
