      SUBROUTINE ESADVH (I,HHT,DELAY,GENTIM,TRAGE,WMAX)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     CALCULATES HEIGHTS OF ADVANCE TREES FOR REGENERATION MODEL
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
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER I,N
C
      REAL AGE,BB,DELAY,GENTIM,HHT,TRAGE,WMAX,X
C
C----------
      N = INT(DELAY+0.5)
      IF(N.GT.2) N=1
      DELAY=REAL(N)
      TRAGE=3.0-DELAY
      AGE=3.0-DELAY-GENTIM
      IF(AGE.LT.1.0) AGE=1.0
C
C     NEW SPECIES IN AK REFIT TO 23 SPECIES ARE ASSIGNED TO 
C     SURROGATE SPECIES THAT WERE ALREADY DEFINED IN THE 13 SPECIES
C     VERSION HERE. 
C             1   2   3   4   5   6   7   8   9   10  11  12  13
C             WS  RC  SF  MH  WH  YC  LP  SS  AF  RA  CW  OH  OS
C     GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,100,100,110),I
C
C     BELOW IS NEW LIST AND MAPPING.
C
C             1   2   3   4   5   6   7   8   9  10  11  12
C            SF  AF  YC  TA  WS  LS  BE  SS  LP  RC  WH  MH
      GO TO ( 30, 90, 60, 10, 10, 10, 10, 80, 70, 20, 50, 40,
     &       110,100,100,100,100,100,100,100,100,100,100),I
C            13  14  15  16  17  18  19  20  21  22  23
C            OS  AD  RA  PB  AB  BA  AS  CW  WI  SU  OH

C----------
C     HEIGHT OF TALLEST ADVANCE WHITE SPRUCE (WS).
C----------
   10 CONTINUE
      HHT = 1.0
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE WESTERN RED CEDAR (RC).
C----------
   20 CONTINUE
      BB = -0.26203 + 0.44249*TIME
   21 CALL ESRANN(X)
      IF(X .GT. WMAX) GO TO 21
      IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 21
      IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 21
      HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE PACIFIC SILVER FIR (SF).
C----------
   30 CONTINUE
      HHT = 1.0
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE MOUNTAIN HEMLOCK (MH).
C----------
   40 CONTINUE
      HHT = 1.0
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE WESTERN HEMLOCK (WH).
C----------
   50 CONTINUE
      BB = -0.26203 + 0.44249*TIME
   51 CALL ESRANN(X)
      IF(X .GT. WMAX) GO TO 51
      IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 51
      IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 51
      HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE ALASKA CEDAR (YC).
C----------
   60 CONTINUE
      BB = -0.26203 + 0.44249*TIME
   61 CALL ESRANN(X)
      IF(X .GT. WMAX) GO TO 61
      IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 61
      IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 61
      HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE LODGEPOLE PINE (LP).
C----------
   70 CONTINUE
      HHT = 1.0
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE SITKA SPRUCE (SS).
C----------
   80 CONTINUE
      BB = -0.26203 + 0.44249*TIME
   81 CALL ESRANN(X)
      IF(X .GT. WMAX) GO TO 81
      IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 81
      IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 81
      HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE SUBALPINE FIR (AF).
C----------
   90 CONTINUE
      HHT = 1.0
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE HARDWOOD.(RA, CW, OH)
C----------
  100 CONTINUE
      HHT = 1.0
      GO TO 120
C----------
C     HEIGHT OF TALLEST ADVANCE OTHER SOFTWOODS.
C----------
  110 CONTINUE
      HHT = 1.0
C
  120 CONTINUE
C----------
C  HEIGHTS TO TALL --- TEMPORARY FIX  11-20-93
C----------
      HHT=HHT*0.25
      IF(HHT.LT.1.0)HHT=1.0
      RETURN
      END
