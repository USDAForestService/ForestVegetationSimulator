      SUBROUTINE ESADVH (EMSQR,I,HHT,DELAY,ELEV,DILATE,IHTSER,
     &  GENTIM,TRAGE)
      IMPLICIT NONE
C----------
C CI $Id$
C----------
C     CALCULATES HEIGHTS OF ADVANCE TREES FOR REGENERATION MODEL
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
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'ESCOM2.F77'
C
C
COMMONS
C
C----------
C
      INTEGER IHTSER,I,N,ITIME
      REAL TRAGE,GENTIM,DILATE,ELEV,DELAY,HHT,EMSQR,TPHY(5,MAXSP)
      REAL AGE,AGELN,BNORM,PN,TPRE(4,MAXSP),THAB(5,MAXSP)
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C----------
C  DATA STATEMENTS
C     CONSTANTS FOR ADVANCE HEIGHTS BY HABITAT TYPE GROUP
C     HAB.TYPE--> WET DFIR  DRY DFIR  GRANDFIR  WRC/WH   SAF
C----------
      DATA THAB/
     1           5*0.0,
     2           5*0.0,
     3           -0.00683,  0.12521, 0.16327,  0.26886,   0.0,
     4             0.0,      0.0,    0.20183,  0.31082,   0.0,
     5           5*0.0,
     6           5*0.0,
     7           5*0.0,
     8           5*0.0,
     9           5*0.0,
     O           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     A           5*0.0,
     &           5*0.0/
C
C     CONSTANTS FOR ADVANCE HEIGHTS BY SITE PREP
C     SITE PREP--> NONE      MECH      BURN      ROAD
C
      DATA TPRE/
     1           4*0.0,
     2           4*0.0,
     3           4*0.0,
     4           4*0.0,
     5             0.0,   -0.10356, -1.23036, -0.40522,
     6           4*0.0,
     7           4*0.0,
     8           4*0.0,
     9             0.0,  -0.20770, -0.12903,  0.18322,
     O           4*0.0,
     &           4*0.0,
     &           4*0.0,
     &           4*0.0,
     &           4*0.0,
     &           4*0.0,
     &           4*0.0,
     &           4*0.0,
     A             0.0,   -0.10356, -1.23036, -0.40522,
     &           4*0.0/
C
C     CONSTANTS FOR ADVANCE HEIGHTS BY PHYSIOGRAPHIC POSITION
C     PHY.POS--> BOTTOM   LOWER     MID      UPPER   RIDGE
C
      DATA TPHY/
     1           5*0.0,
     2           5*0.0,
     3           0.04770, 0.41224, 0.25028, 0.23537,  0.0,
     4           5*0.0,
     5           5*0.0,
     6           0.32413, 0.39404, 0.25123, 0.23419,  0.0,
     7          -0.28223,-0.99702,-0.47684,-0.20872,  0.0,
     8           5*0.0,
     9           5*0.0,
     O          -0.18689, 0.27119, 0.70375, 0.65555,  0.0 ,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     &           5*0.0,
     A           5*0.0,
     &           5*0.0/
C----------
      N=INT(DELAY+0.5)
      IF(N.GT.2) N=1
      DELAY=REAL(N)
      TRAGE=3.0-DELAY
      AGE=3.0-DELAY-GENTIM
      IF(AGE.LT.1.0) AGE=1.0
      AGELN=ALOG(AGE)
      ITIME=INT(TIME+0.5)
      BNORM=BNORML(ITIME)
C
      SELECT CASE(I)
C----------
C  HEIGHT OF TALLEST ADV. WWP. 6JAN88 CARLSON
C----------
      CASE(1)
        PN=  0.05585 +0.84765*AGELN -0.003824*BAA -0.02835*ELEV
     &    -0.79565*XCOS +0.39278*XSIN -0.68673*SLO
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.51878)
C----------
C  HEIGHT OF TALLEST ADV. WESTERN LARCH.
C----------
      CASE(2)
        PN= -1.80559 +1.24136*AGELN
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.54325)
C----------
C  HEIGHT OF TALLEST ADV. D-FIR. 8JAN88 CARLSON
C----------
      CASE(3)
        PN= -1.15433 +1.09480*AGELN +TPHY(IPHY,3) +THAB(IHTSER,3)
     &    -0.04804*ELEV +0.0004225*ELEV*ELEV
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.63678)
C----------
C  HEIGHT OF TALLEST ADV. GRAND FIR. 6JAN88 CARLSON
C----------
      CASE(4)
        PN= -1.96040 +1.02403*AGELN -0.00233*BAA +THAB(IHTSER,3)
     &    +0.04315*XCOS +0.13456*XSIN -0.21468*SLO
     &    -0.05224*BWB4 -0.01898*BWAF
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.61195)
C----------
C  HEIGHT OF TALLEST ADV. W HEMLOCK. 7JAN88 CARLSON
C----------
      CASE(5)
        PN= -0.43269 +0.77433*AGELN -0.00378*BAA +TPRE(IPREP,5)
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.54794)
C----------
C  HEIGHT OF TALLEST ADV. WRC. 7JAN88 CARLSON
C----------
      CASE(6)
        PN=  2.11552 +0.71766*AGELN +TPHY(IPHY,6) -0.17259*ELEV
     &    +0.12506*XCOS +0.63747*XSIN -0.35258*SLO +0.0022033*ELEV*ELEV
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.62044)
C----------
C  HEIGHT OF TALLEST ADV. LPP. 7JAN88 CARLSON
C----------
      CASE(7)
        PN= -0.59267 +0.88997*AGELN +TPHY(IPHY,7) +0.79158*XCOS
     &    +0.49060*XSIN +0.49071*SLO
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.68842)
C----------
C  HEIGHT OF TALLEST ADV. E. SPRUCE. 7JAN88 CARLSON
C----------
      CASE(8)
        PN= -2.19638 +1.12147*AGELN -0.002270*BAA
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.59475)
C----------
C  HEIGHT OF TALLEST ADV. SAF.  6JAN88 CARLSON
C----------
      CASE(9)
        PN= -1.69509 +0.87242*AGELN -0.001107*BAA +TPRE(IPREP,9)
     &    -0.06402*BWB4 +0.02299*BWAF -0.01189*XCOS +0.15379*XSIN
     &    +0.44637*SLO
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.59957)
C----------
C  HEIGHT OF TALLEST ADV. PONDEROSA PINE.  7JAN88 CARLSON
C----------
      CASE(10)
        PN= -6.33095 +0.79936*AGELN +TPHY(IPHY,10) +0.06347*BWAF
     &    +0.19305*ELEV -0.0020058*ELEV*ELEV
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.53813)
C----------
C  HEIGHT OF TALLEST ADV. WHITEBARK PINE
C----------
      CASE(11)
        PN = 0.0
        HHT = 0.5
C----------
C  HEIGHT OF TALLEST ADV. PACIFIC YEW
C----------
      CASE(12)
        PN = 0.0
        HHT = 0.5
C----------
C  HEIGHT OF TALLEST ADV. QUAKING ASPEN
C----------
      CASE(13)
        PN = 0.0
        HHT = 5.0
C----------
C  HEIGHT OF TALLEST ADV. WESTERN JUNIPER
C----------
      CASE(14)
        PN = 0.0
        HHT = 0.5
C----------
C  HEIGHT OF TALLEST ADV. CURLLEAF MOUNTAIN-MAHOGANY
C----------
      CASE(15)
        PN = 0.0
        HHT = 0.5
C----------
C  HEIGHT OF TALLEST ADV. LIMBER PINE
C----------
      CASE(16)
        PN = 0.0
        HHT = 0.5
C----------
C  HEIGHT OF TALLEST ADV. BLACK COTTONWOOD
C----------
      CASE(17)
        PN = 0.0
        HHT = 5.0
C----------
C  HEIGHT OF TALLEST ADV. OTHER SOFTWOODS (USES WH EQUATION).
C----------
      CASE(18)
        PN= -0.43269 +0.77433*AGELN -0.00378*BAA +TPRE(IPREP,11)
        HHT = EXP(PN +EMSQR*DILATE*BNORM*0.54794)
C----------
C  HEIGHT OF TALLEST ADV. OTHER HARDWOODS
C----------
      CASE(19)
        PN = 0.0
        HHT = 5.0
C
      END SELECT
C
      RETURN
      END
