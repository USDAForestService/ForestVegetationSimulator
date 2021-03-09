      SUBROUTINE ESPXCS (IFT)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICTS THE PROBABILITY OF EXCESS SPECIES.
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
C  SLO      -- PLOT SLOPE IN PROPORITON, PASSED IN ESCOMN.F77
C  TLAT     -- STAND LATTITUDE, PASSED IN PLOT.F77
C  NNID     -- BASE MODEL PLOT NUMBER, PASSE IN ESCOMN.F77
C  OCURFT   -- SPECIES OCCURANCE BY FOREST TYPE, 0=NO, 1=YES,
C              PASSED IN ESCOMN.F77
C  XESMLT   -- SPECIES MULTIPLIER, PASSED IN ESHAP.F77
C  PXCS     -- SPECIES PROBABILITY, PASSED TO ESCOM2.F77
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
     &   0.0,         0.0,       0.0,        0.0,      -0.660089,       ! 122 WHITE SPRUCE
     &   0.0,       -13.048415, -5.754677,   0.0,       0.0,
     &   0.0,       -14.274116,  0.0,        0.0,       0.0,
     &   4.575201,    0.0,       0.0,      -10.549034, -0.065825,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,      -1.666924,       ! 125 BLACK SPRUCE
     &   0.0,       -10.360376, -5.718245,   0.0,       0.0,
     &   0.0,       -15.478962,  0.0,        0.0,       0.0,
     &   3.879831,    0.0,       0.0,      -11.098798, -4.211972,
     &   0.0,         0.0,       0.0, 
     &   0.0,         0.0,      17.194845,   0.0,      -3.038861,       ! 270 MOUNTAIN HEMLOCK
     &   0.0,         0.0,      -1.768278,  13.316892, 77.939276,
     &   23.017774, -10.19144,   0.0,        0.0,       0.0,
     &   -0.002905,   0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      18.143113,   0.0,       0.0,            ! 271 ALASKA CEDAR
     &   0.0,         0.0,      -1.945181,  14.438988, 79.79663,
     &   23.223099,  -9.244208,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      18.258002,   0.0,       0.0,            ! 281 LODGEPOLE PINE
     &   0.0,         0.0,      -3.262106,  14.49138,  79.797169,
     &   22.599696,  -9.902887,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      16.110082,   0.0,       0.0,            ! 301 WESTERN HEMLOCK
     &   0.0,         0.0,      -1.268606,  12.096032, 78.370411,
     &   24.277176, -10.936757,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.235882,   0.0,       0.0,            ! 304 WESTERN REDCEDAR
     &   0.0,         0.0,      -1.475124,  13.571591, 79.825599,
     &   23.7587,    -9.983417,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      15.10477,    0.0,       0.0,            ! 305 SITKA SPRUCE
     &   0.0,         0.0,      -0.602064,  11.228837, 78.371973,
     &   22.780179, -11.188737,  0.0,        0.0,       0.0,
     &   -0.226092,   0.0,       0.0,        0.0,      -5.175467,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.350078,       ! 703 COTTONWOOD
     &   0.0,       -14.559348, -2.486519,   0.0,       0.0,
     &   21.647459, -14.234057,  0.0,        0.0,       0.0,
     &   4.019134,    0.0,       0.0,      -10.668417, -2.598175,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.766646,       ! 901 ASPEN
     &   0.0,       -12.988339,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   3.732321,    0.0,       0.0,      -11.089639,  0.165815,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.020366,       ! 902 PAPER BIRCH
     &   0.0,       -12.407355, -5.051145,   0.0,       0.0,
     &   20.529615, -15.053762,  0.0,        0.0,       0.0,
     &   5.584148,    0.0,       0.0,      -10.189137, -0.667452,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,      -0.660089,       ! 904 BALSAM POPLAR (MAPPED TO WHITE SPRUCE)
     &   0.0,       -13.048415, -5.754677,   0.0,       0.0,
     &   0.0,       -14.274116,  0.0,        0.0,       0.0,
     &   4.575201,    0.0,       0.0,      -10.549034, -0.065825,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,            ! 911 RED ALDER
     &   0.0,         0.0,      -0.083139,   0.0,       0.0,
     &   21.744534,   0.0,       0.0,        0.0,      -5.462809,
     &   0.0,         0.0,       0.0,        0.0,      -2.058945,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,            ! OTHER F.T.
     &   0.0,         0.0,       0.0,        0.0,       0.0, 
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0, 
     &   0.0,         0.0,       0.0 /
      DATA PNRDA / 
     &   0.0,       0.0,      -2.753695,  0.0,        0.772376,
     &   0.0,       1.667227, -0.822708, -3.839074,  -1.389829,
     &   1.781454, -0.661553,  0.0,       0.0,        0.735086, 
     &  -1.819681,  0.0,       0.0,      -0.403056,  -0.459111, 
     &   0.0,       0.0,       0.0 /
      DATA PBAPER / 
     &   0.0,      0.0,      3.282042, 0.0,      2.926946,
     &   0.0,     19.708483, 0.896178, 2.473768, 1.07333,
     &   5.111553, 3.153337, 0.0,      0.0,      4.820169,
     &   0.963018, 0.0,      0.0,      3.790653, 5.334335,
     &   0.0,      0.0,      0.0 /
      DATA PNELEV /
     &   0.0,      0.0,       0.000355,   0.0,       0.000576,
     &   0.0,      0.000384, -0.000299,  -0.000677, -0.001317,
     &   -0.000992,0.000518,  0.0,        0.0,       0.0,
     &   -0.00051, 0.0,       0.0,       -0.000481, -0.000585,
     &   0.0,      0.0,       0.0 /     
      DATA PNSLO / 
     &   0.0,       0.0,     -0.003589,  0.0,      -0.020014,
     &   0.0,      -0.045686, 0.003577, -0.003684,  0.005159, 
     &   0.006749, -0.00674,  0.0,       0.0,      -0.023403,
     &   0.01975,   0.0,      0.0,       0.028237,  0.019055,
     &   0.0,       0.0,      0.0 /      
      DATA PNTLAT / 
     &   0.0,       0.0,      -0.317185, 0.0,        -0.026832,
     &   0.0,       0.157293,  0.0239,  -0.253421,   -1.433828,
     &  -0.413015,  0.15915,   0.0,      0.0,         0.0,
     &  -0.078845,  0.0,       0.0,      0.146413,   -0.036439,
     &   0.0,       0.0,       0.0 /  
C     CALCULATE PLOT DENSITIES 
      XPRD = PRDA(NNID)
      DENOM = BAAA(NNID)
      IF(BAAA(NNID).LE.0.) DENOM=1.       
C     ADJUST PLOT SLO-BASED VARIABLES TO PERCENT SLOPE.  
      ADJSLO = SLO*100
      ADJELV = ELEV*100

C----------
C  PREDICT PROBABILITY OF EXCESS SPECIES
C
C  PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV + B5 * ADJSLO +
C  B6 * TLAT
C
C  VARIABLES DEFINED IN SECTION ABOVE
C  PN IS CONVERTED TO PROBABILITY OF EXCESS SPECIES BY
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
        PXCS(J) = (EXP(PN)/(1 + EXP(PN))) * OCURFT(J,IFT) * XESMLT(J)
    5 CONTINUE
C
C  MAKE SURE SPECIES PROBABILITIES ARE BETWEEN 0 AND 1.
      DO 10 I=1,MAXSP
      IF(PXCS(I).LT.0.)PXCS(I)=0.
      IF(PXCS(I).GT.1.)PXCS(I)=1.
      IF(IFT.EQ.14)PXCS(I)=0.
   10 CONTINUE
C
      RETURN
      END

