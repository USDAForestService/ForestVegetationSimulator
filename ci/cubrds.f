      BLOCK DATA CUBRDS
      IMPLICIT NONE
C----------
C CI $Id$
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
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
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
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/
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
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA ICTRAN/
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA CTRAN/
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0,  6000.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &      0.0,      0.0,     0.0,     0.0/
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
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0/
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
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA IBTRAN/
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 1, 0, 0, 0, 0, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA BTRAN/
     &     20.5,     20.5,    20.5,    20.5,    20.5,
     &     20.5,     20.5,    20.5,    20.5,    20.5,
     &     20.5,     20.5,    20.5, 42800.0,    20.5,
     &     20.5,     20.5,    20.5,    20.5/
      END
