      SUBROUTINE MAICAL
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND.
C  CALLED FROM CRATET.
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
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
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
      INTEGER IERR,ISICD
C
      INTEGER ISPNUM(MAXSP)
C
      REAL ADJMAI,SSSI
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
C----------
C
C  DATA STATEMENTS:
C
C----------
C  INITIALIZE INTERNAL VARIABLES:
C  (ISPNUM CONTAINS THE FIA CODE FOR THE PROXY EQUATION FOR THE CALL TO
C   SUBROUTINE **ADJMAI**)
C----------
      DATA ISPNUM/ 011, 019, 042, 071, 094, 094, 095, 098, 108, 242,
     &             263, 264, 298, 350, 351, 375, 376, 741, 746, 747,
     &             920, 928, 998/



C     CALCULATE ADJUSTED MAI AND LIMIT TO 128 MAX.
C     DEFAULT SITE SPECIES IS WESTERN HEMLOCK.

      IF (ISISP .EQ. 0) ISISP=11
      SSSI=SITEAR(ISISP)
      IF (SSSI .EQ. 0.) SSSI=80.0
      ISICD=ISPNUM(ISISP)
      RMAI=ADJMAI(ISICD,SSSI,10.0,IERR)
      IF(RMAI .GT. 128.0)RMAI=128.0
      RETURN
      END
