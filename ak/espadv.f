      SUBROUTINE ESPADV (IFT)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICTS THE PROBABILITY OF ADVANCE SPECIES.
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
C  PBAPER   -- SPECIES BASAL AREA PROPORTION COEFFICIENT (B3)
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
C  PADV     -- SPECIES PROBABILITY, PASSED TO ESCOM2.F77
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
     &   0.0,         0.0,       0.0,       -4.335977, -0.768725,       ! 122 WHITE SPRUCE
     &   0.0,       -13.023483,  0.0,        0.0,       0.0,
     &   0.0,       -14.636196,  0.0,        0.0,       0.0,
     &   4.543671,    0.0,      -0.129163, -10.517939,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977, -1.793084,       ! 125 BLACK SPRUCE
     &   0.0,       -10.509385,  0.0,        0.0,       0.0,
     &   0.0,       -15.787063,  0.0,        0.0,       0.0,
     &   3.846221,    0.0,      -4.067106, -11.048545,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.052464,   0.0,      -3.104481,       ! 270 MOUNTAIN HEMLOCK
     &   0.0,         0.0,      -1.727407,  13.303874, 77.557037,
     &   23.185698, -10.383977,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.998179,   0.0,       0.0,            ! 271 ALASKA CEDAR
     &   0.0,         0.0,      -1.900489,  14.426165, 79.436554,
     &   23.405695,  -9.457501,  0.0,        0.0,      35.036460,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      18.116574,   0.0,       0.0,            ! 281 LODGEPOLE PINE
     &   0.0,         0.0,      -3.220587,  14.50099,  79.452263,
     &   22.762266, -10.122027,  0.0,        0.0,      35.036460,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      15.963877,   0.0,       0.0,            ! 301 WESTERN HEMLOCK
     &   0.0,         0.0,      -1.224259,  12.070894, 78.000985,
     &   24.451408, -11.165193,  0.0,        0.0,      35.036460,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      17.094312,   0.0,       0.0,            ! 304 WESTERN REDCEDAR
     &   0.0,         0.0,      -1.434335,  13.561585, 79.462069,
     &   23.941341, -10.180572,  0.0,        0.0,      35.036460,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,      14.997227,   0.0,       0.0,            ! 305 SITKA SPRUCE
     &   0.0,         0.0,      -0.563591,  11.217778, 78.007480,
     &   22.942334, -11.404114,  0.0,        0.0,      35.036460,
     &   -0.265927,   0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.231061,       ! 703 COTTONWOOD
     &   0.0,       -14.506330, -2.473404,   0.0,       0.0,
     &   21.694368, -14.517376,  0.0,        0.0,       0.0,
     &   3.879223,    0.0,       0.0,      -10.664679, -2.676858,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977,  0.65022,       ! 901 ASPEN
     &   0.0,       -12.961809,  0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   3.691336,    0.0,       0.107820, -11.026581,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977, -0.090381,       ! 902 PAPER BIRCH
     &   0.0,       -12.383669, -5.003714,   0.0,       0.0,
     &   20.687934, -15.353202,  0.0,        0.0,       0.0,
     &   5.542455,    0.0,      -0.743739, -10.160502,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,       -4.335977, -0.768725,       ! 904 BALSAM POPLAR (MAP TO WHITE SPRUCE)
     &   0.0,       -13.023483,  0.0,        0.0,       0.0,
     &   0.0,       -14.636196,  0.0,        0.0,       0.0,
     &   4.543671,    0.0,      -0.129163, -10.517939,  0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,            ! 911 RED ALDER
     &   0.0,         0.0,      -0.042003,   0.0,       0.0,
     &   21.940216,   0.0,       0.0,        0.0,      35.03646,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,            ! OTHER F.T.
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0,        0.0,       0.0,
     &   0.0,         0.0,       0.0 /
      DATA PNRDA /
     &   0.0,       0.0,      -2.727570,  0.0,        0.774395,
     &   0.0,       1.660146, -0.838168, -3.839624,  -1.443889,
     &   1.739987, -0.620028,  0.0,       0.0,        0.851525,
     &  -1.846654,  0.0,      -0.464799, -0.397800,  -0.464799,
     &   0.0,       0.0,       0.0 /
      DATA PBAPER /
     &   0.0,      0.0,      3.266453, 0.0,      2.917051,
     &   0.0,     19.545809, 0.891633, 2.472698, 1.074455,
     &   5.056352, 3.062509, 0.0,      0.0,      5.115524,
     &   0.957204, 0.0,      5.337844, 3.757893, 5.337844,
     &   0.0,      0.0,      0.0 /
      DATA PNELEV /
     &   0.0,      0.0,       0.000346,   0.0,       0.000580,
     &   0.0,      0.000381, -0.000293,  -0.000699, -0.001340,
     &  -0.000998, 0.000542,  0.0,        0.0,      -0.004470,
     &  -0.000526,  0.0,     -0.000588,  -0.000492, -0.000588,
     &   0.0,      0.0,       0.0 /
      DATA PNSLO /
     &   0.0,       0.0,     -0.003355,  0.0,      -0.020237,
     &   0.0,      -0.044745, 0.003523, -0.002279,  0.006463,
     &   0.007062, -0.007500, 0.0,       0.0,      -0.027259,
     &   0.021042,  0.0,      0.019300,  0.029324,  0.019300,
     &   0.0,       0.0,      0.0 /
      DATA PNTLAT /
     &   0.0,       0.0,      -0.314786,  0.0,        -0.025084,
     &   0.0,       0.156958,  0.023200, -0.253624,   -1.427424,
     &  -0.415727,  0.162942,  0.0,       0.0,        -0.707129,
     &  -0.077977,  0.0,      -0.035312,  0.145927,   -0.035312,
     &   0.0,       0.0,       0.0 /

C     CALCULATE PLOT DENSITIES
      XPRD = PRDA(NNID)
      DENOM = BAAA(NNID)
      IF(BAAA(NNID).LE.0.) DENOM=1.
C     ADJUST PLOT SLO-BASED VARIABLES TO PERCENT SLOPE.
      ADJSLO = SLO*100
      ADJELV = ELEV*100

C----------
C  PREDICT PROBABILITY OF ADVANCED SPECIES
C
C  PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV + B5 * ADJSLO +
C  B6 * TLAT
C
C  VARIABLES DEFINED IN SECTION ABOVE
C  PN IS CONVERTED TO PROBABILITY OF ADVANCE SPECIES BY
C  EXP(PN)/(1 + EXP(PN))
C----------
C  IF THE FOREST TYPE IS UNDEFINED (IFT=14), THE PROBABILITY CAN NOT
C  BE CALCULATED AND PADV VALUE WILL BE ZERO.
C
      IF(IFT .EQ. 14) THEN
        DO J=1,MAXSP
          PADV(J) = 0.0
        ENDDO
      ELSE

C       CYCLE THROUGH SPECIES TO COMPUTE SPECIES PROBABILITIES
        DO J=1,MAXSP
          IF(OCURFT(J,IFT) .EQ. 0.0) THEN
            PADV(J) = 0.0
          ELSE
C           SET COEFFICIENTS FOR COMPUTING PN
            B1 = PNFT(J,IFT)
            B2 = PNRDA(J)
            B3 = PBAPER(J)
            B4 = PNELEV(J)
            B5 = PNSLO(J)
            B6 = PNTLAT(J)
            BAPER = OVER(J,NNID)/DENOM
            PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV +
     &           B5 * ADJSLO + B6 * TLAT
            PADV(J)=(EXP(PN)/(1 + EXP(PN))) * OCURFT(J,IFT) * XESMLT(J)

C           WRITE STATEMENT IS USED FOR DEBUG PURPOSES HERE. UNCOMMENT TO
C           SEE OUTPUT.
C            WRITE(16,*)' IN ESPADV',' IFT=',IFT,' ISPC=',J,' XPRD=',
C     &      XPRD,' BAPER=',BAPER,' ADJELV=',ADJELV,' ADJSLO=',ADJSLO,
C     &      ' TLAT=',TLAT,' OCCURFT=',OCURFT(J,IFT),' XESMLT=',
C     &      XESMLT(J),' PN=',PN,' PADV=',PADV(J)

C           MAKE SURE SPECIES PROBABILITIES ARE BETWEEN 0 AND 1.
            IF(PADV(J) .LT. 0.0) PADV(J) = 0.0
            IF(PADV(J) .GT. 1.0) PADV(J) = 1.0
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END
