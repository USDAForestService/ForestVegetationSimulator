      SUBROUTINE ESPSUB (IFT)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICTS THE PROBABILITY OF SUBSEQUENT SPECIES.
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
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'ESHAP.F77'
C
C 
      INCLUDE 'ESHAP2.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C
C  IFT      -- STAND FOREST TYPE CATEGORY WHERE:
C                1 = 122
C                2 = 125
C                3 = 270
C                4 = 271
C                5 = 281
C                6 = 301
C                7 = 304
C                8 = 305
C                9 = 703
C               10 = 901
C               11 = 902
C               12 = 904
C               13 = 911
C               14 = OTHER (NO ADVANCED REGENERATION)
C  MAXSP    -- MAXIMUM NUMBER OF SPECIES, PASSED IN PRGPRM.F77
C  PNFT     -- SPECIES X FOREST TYPE COEFFICIENT (B1)
C  PNRDA    -- RELATIVE DENSITY COEFFICIENT (B2)
C  PBAPER   -- SPECIES BASAL AREA PERCENTAGE COEFFICIENT (B3)
C  PNEVEL   -- ELEVATION COEFFICIENT (B4)
C  PNSLO    -- SLOPE COEFFICIENT (B5)
C  PNTLAT   -- STAND LATTITUDE COEFFICIENT (B6)
C  PN       -- USED IN DETERMINING SPECIES PROBABILITY
C  XPRD     -- PLOT RELATIVE DENSITY >= REGNBK, PASSED IN AS 
C              PRDA(MAXPLOT) IN PDEN.F77
C  BAPER    -- PLOT SPECIES BASAL AREA PROPORTION>= REGNBK, 
C              PASSED IN AS OVER(MAXSP,MAXPLOT) IN PDEN.F77
C  ELEV     -- STAND ELEVATION (100S OF FEET), PASSED IN PLOT.F77
C  SLO      -- PLOT SLOPE IN PROPORTION, PASSED IN ESCOMN.F77
C  TLAT     -- STAND LATTITUDE, PASSED IN PLOT.F77
C  NNID     -- BASE MODEL PLOT NUMBER, PASSE IN ESCOMN.F77
C  OCURFT   -- SPECIES OCCURANCE BY FOREST TYPE, 0=NO, 1=YES,
C              PASSED IN ESCOMN.F77
C  XESMLT   -- SPECIES MULTIPLIER, PASSED IN ESHAP.F77
C  PSUB     -- SPECIES PROBABILITY, PASSED TO ESCOM2.F77
C----------
C  VARIABLE DECLARATIONS:
C----------
C  
      INTEGER IFT, I, J
C
      REAL PN,PNFT(MAXSP,14),PNRDA(MAXSP),PBAPER(MAXSP),PNELEV(MAXSP), 
     & PNSLO(MAXSP),PNTLAT(MAXSP),B1,B2,B3,B4,B5,B6,XPRD,BAPER,DENOM,
     & ADJSLO, ADJELV
C
C----------
      DATA ((PNFT(I,J),I=1,MAXSP),J=1,14)/
     &   0.0,         0.0,       0.0,       -4.335977, -0.723446,       ! 122 WHITE SPRUCE
     &   0.0,       -13.022607, -5.680409,   0.0,       0.0,
     &   0.0,       -14.708911,  0.0,        0.0,       0.0,
     &   4.549219,    0.0,      -0.120385, -10.511374,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977, -1.749333,       ! 125 BLACK SPRUCE
     &   0.0,       -10.508760, -5.644951,   0.0,       0.0,
     &   0.0,       -15.864302,  0.0,        0.0,       0.0,
     &   3.851620,    0.0,      -4.056666, -11.041897,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.029745,   0.0,      -3.064843,       ! 270 MOUNTAIN HEMLOCK
     &   0.0,         0.0,      -1.697176,  13.293685, 77.55712,
     &   23.109302, -10.451391,   0.0,       0.0,       0.0,
     &   -0.207050,   0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.974575,   0.0,       0.0,            ! 271 ALASKA CEDAR
     &   0.0,         0.0,      -1.875220,  14.414891, 79.436482,
     &   23.328427,  -9.525204,  0.0,        0.0,      35.028264,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      18.092865,   0.0,       0.0,            ! 281 LODGEPOLE PINE
     &   0.0,         0.0,      -3.196397,  14.489726, 79.452189,
     &   22.68629,  -10.191152,  0.0,        0.0,      35.028264,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      15.940447,   0.0,       0.0,            ! 301 WESTERN HEMLOCK
     &   0.0,         0.0,      -1.199460,  12.059662, 78.000917,
     &   24.382184, -11.232734,  0.0,        0.0,      35.028264,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.071080,   0.0,       0.0,            ! 304 WESTERN REDCEDAR
     &   0.0,         0.0,      -1.410825,  13.550407, 79.461995,
     &   23.863861, -10.247801,  0.0,        0.0,      35.028264,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      14.973387,    0.0,       0.0,            ! 305 SITKA SPRUCE
     &   0.0,         0.0,      -0.543492,  11.206520, 78.007427,
     &   22.863489, -11.475804,  0.0,        0.0,      35.028264,
     &   -0.257252,   0.0,       0.0,        0.0,      -5.290410,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.290072,       ! 703 COTTONWOOD
     &   0.0,       -14.503453, -2.413704,   0.0,       0.0,
     &   21.724851, -14.590913,  0.0,        0.0,       0.0,
     &   3.904244,    0.0,       0.0,      -10.652676, -2.666443,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977,  0.693238,       ! 901 ASPEN
     &   0.0,       -12.960951,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   3.697750,    0.0,       0.116854, -11.019925,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977, -0.047101,       ! 902 PAPER BIRCH
     &   0.0,       -12.382828, -4.973413,   0.0,       0.0,
     &   20.605333, -15.429146,  0.0,        0.0,       0.0,
     &   5.548172,    0.0,      -0.734643, -10.153927,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977, -0.723446,       ! 904 BALSAM POPLAR (MAP TO WHITE SPRUCE)
     &   0.0,       -13.022607, -5.680409,   0.0,       0.0,
     &   0.0,       -14.708911,  0.0,        0.0,       0.0,
     &   4.549219,    0.0,      -0.120385, -10.511374,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,            ! 911 RED ALDER
     &   0.0,         0.0,      -0.018768,   0.0,       0.0,
     &   21.861333,   0.0,       0.0,        0.0,      35.028264,
     &   0.0,         0.0,       0.0,        0.0,      -2.016213,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,            ! OTHER F.T.
     &   0.0,         0.0,       0.0,        0.0,       0.0, 
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0, 
     &   0.0,         0.0,       0.0 /
      DATA PNRDA / 
     &   0.0,       0.0,      -2.727480,  0.0,        0.776703,
     &   0.0,       1.660099, -0.839137, -3.839569,  -1.443887,
     &   1.735537, -0.622608,  0.0,       0.0,        0.851437, 
     &  -1.845793,  0.0,      -0.464068, -0.397475,  -0.464068, 
     &   0.0,       0.0,       0.0 /
      DATA PBAPER / 
     &   0.0,      0.0,      3.266072, 0.0,      2.913539,
     &   0.0,     19.545532, 0.899752, 2.472569, 1.07446,
     &   5.125714, 3.072878, 0.0,      0.0,      5.115206,
     &   0.956973, 0.0,      5.334335, 3.757554, 5.334335,
     &   0.0,      0.0,      0.0 /
      DATA PNELEV /
     &   0.0,      0.0,       0.000347,   0.0,       0.000579,
     &   0.0,      0.000381, -0.000296,  -0.000699, -0.00134,
     &  -0.000998, 0.000541,  0.0,        0.0,      -0.004469,
     &  -0.000526,  0.0,     -0.000587,  -0.000492, -0.000587,
     &   0.0,      0.0,       0.0 /     
      DATA PNSLO / 
     &   0.0,       0.0,     -0.003365,  0.0,      -0.020246,
     &   0.0,      -0.044743, 0.003520, -0.002281,  0.006463, 
     &   0.007038, -0.007531, 0.0,       0.0,      -0.027257,
     &   0.021005,  0.0,      0.019293,  0.029316,  0.019293,
     &   0.0,       0.0,      0.0 /      
      DATA PNTLAT / 
     &   0.0,       0.0,      -0.314365,  0.0,        -0.025767,
     &   0.0,       0.156946,  0.022802, -0.253422,   -1.427422,
     &  -0.414381,  0.164182,  0.0,       0.0,        -0.706979,
     &  -0.078066,  0.0,      -0.035456,  0.145823,   -0.035456,
     &   0.0,       0.0,       0.0 /

C     CALCULATE PLOT DENSITIES 
      XPRD = PRDA(NNID)
      DENOM = BAAA(NNID)
      IF(BAAA(NNID).LE.0.) DENOM=1. 
C     ADJUST PLOT SLO-BASED VARIABLES TO PERCENT SLOPE.  
      ADJSLO = SLO*100
      ADJELV = ELEV*100

C----------
C  PREDICT PROBABILITY OF SUBSEQUENT SPECIES
C
C  PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV + B5 * ADJSLO +
C  B6 * TLAT
C
C  VARIABLES DEFINED IN SECTION ABOVE
C  PN IS CONVERTED TO PROBABILITY OF SUBSEQUENT SPECIES BY
C  EXP(PN)/(1 + EXP(PN))
C----------

C     CYCLE THROUGH SPECIES TO COMPUTE SPECIES PROBABILITIES
      DO 5 J=1,MAXSP
C       SET COEFFICIENTS FOR COMPUTING PN
        B1 = PNFT(J,IFT)
        B2 = PNRDA(J)
        B3 = PBAPER(J)
        B4 = PNELEV(J)
        B5 = PNSLO(J)
        B6 = PNTLAT(J)
        BAPER = OVER(J,NNID)/DENOM
        PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV + 
     &       B5 * ADJSLO + B6 * TLAT
        PSUB(J) = (EXP(PN)/(1 + EXP(PN))) * OCURFT(J,IFT) * XESMLT(J)
    5 CONTINUE
C  MAKE SURE SPECIES PROBABILITIES ARE BETWEEN 0 AND 1.
      DO 10 I=1,MAXSP
      IF(PSUB(I).LT.0.)PSUB(I)=0.
      IF(PSUB(I).GT.1.)PSUB(I)=1.
      IF(IFT.EQ.14) PSUB(I)=0.
   10 CONTINUE
C
      RETURN
      END

