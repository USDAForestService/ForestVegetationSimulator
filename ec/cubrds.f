      BLOCK DATA CUBRDS
      IMPLICIT NONE
C----------
C EC $Id$
C----------
C  DEFAULT PARAMETERS FOR THE CUBIC AND BOARD FOOT VOLUME EQUATIONS.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
COMMONS
C----------
C  SPECIES LIST FOR EAST CASCADES VARIANT.
C
C   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
C   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
C   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
C   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
C   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
C   6 = GRAND FIR               (GF)    ABIES GRANDIS
C   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
C   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
C   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
C  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
C  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
C  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
C  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
C  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
C  15 = NOBLE FIR               (NF)    ABIES PROCERA
C  16 = WHITE FIR               (WF)    ABIES CONCOLOR
C  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
C  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
C  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
C  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
C  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
C  22 = RED ALDER               (RA)    ALNUS RUBRA
C  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
C  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
C  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
C  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
C  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
C  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
C  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
C  30 = WILLOW SPECIES          (WI)    SALIX sp.
C  31 = OTHER SOFTWOODS         (OS)
C  32 = OTHER HARDWOODS         (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE EC VARIANT:
C      USE 6(GF) FOR 16(WF)
C      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
C
C  FROM THE WC VARIANT:
C      USE 19(WH) FOR 11(WH)
C      USE 33(PY) FOR 13(PY)
C      USE 31(WB) FOR 14(WB)
C      USE  7(NF) FOR 15(NF)
C      USE 30(LL) FOR 17(LL)
C      USE  8(YC) FOR 18(YC)
C      USE 29(WJ) FOR 19(WJ)
C      USE 21(BM) FOR 20(BM) AND 21(VN)
C      USE 22(RA) FOR 22(RA)
C      USE 24(PB) FOR 23(PB)
C      USE 25(GC) FOR 24(GC)
C      USE 34(DG) FOR 25(DG)
C      USE 26(AS) FOR 26(AS) AND 32(OH)
C      USE 27(CW) FOR 27(CW)
C      USE 28(WO) FOR 28(WO)
C      USE 36(CH) FOR 29(PL)
C      USE 37(WI) FOR 30(WI)
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN 
C  THE TRANSITION SIZE
C----------
      DATA CFVEQS/
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     & 0.030288,      0.0,     0.0,0.002213,     0.0,     0.0,    0.0,
     &   35*0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &   98*0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &    7*0.0/
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN 
C  THE TRANSITION SIZE
C----------
      DATA CFVEQL/
     &      0.0,      0.0,     0.0, 0.00233,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00184,     0.0,     0.0,    0.0,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00234,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00205,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &-1.557103,      0.0,     0.0,0.002474,     0.0,     0.0,    0.0,
     &    7*0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &   21*0.0,
     &      0.0,      0.0,     0.0, 0.00205,     0.0,     0.0,    0.0,
     &   98*0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &    7*0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA ICTRAN/
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA CTRAN/
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,  6000.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0/
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN 
C  THE TRANSITION SIZE
C----------
      DATA BFVEQS/
     &  -26.729,      0.0,     0.0, 0.01189,     0.0,     0.0,    0.0,
     &  -29.790,      0.0,     0.0, 0.00997,     0.0,     0.0,    0.0,
     &  -25.332,      0.0,     0.0, 0.01003,     0.0,     0.0,    0.0,
     &  -34.127,      0.0,     0.0, 0.01293,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -10.742,      0.0,     0.0, 0.00878,     0.0,     0.0,    0.0,
     &   -8.085,      0.0,     0.0, 0.01208,     0.0,     0.0,    0.0,
     &  -11.851,      0.0,     0.0, 0.01149,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  -50.340,      0.0,     0.0, 0.01201,     0.0,     0.0,    0.0,
     &    7*0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &   21*0.0,
     &  -10.742,      0.0,     0.0, 0.00878,     0.0,     0.0,    0.0,
     &   98*0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &    7*0.0/
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN 
C  THE TRANSITION SIZE
C----------
      DATA BFVEQL/
     &  -32.516,      0.0,     0.0, 0.01181,     0.0,     0.0,    0.0,
     &   85.150,      0.0,     0.0, 0.00841,     0.0,     0.0,    0.0,
     &   -9.522,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &   10.603,      0.0,     0.0, 0.01218,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &   -4.064,      0.0,     0.0, 0.00799,     0.0,     0.0,    0.0,
     &   14.111,      0.0,     0.0, 0.01103,     0.0,     0.0,    0.0,
     &    1.620,      0.0,     0.0, 0.01158,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     & -298.784,      0.0,     0.0, 0.01595,     0.0,     0.0,    0.0,
     &    7*0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &   21*0.0,
     &   -4.064,      0.0,     0.0, 0.00799,     0.0,     0.0,    0.0,
     &   98*0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &    7*0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA IBTRAN/
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA BTRAN/
     &     20.5,     20.5,    20.5,    20.5,    20.5,
     &     20.5,     20.5,    20.5,    20.5,    20.5,
     &      0.0,     20.5,     0.0,     0.0,     0.0,
     &     20.5,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &     20.5,      0.0/
      END
