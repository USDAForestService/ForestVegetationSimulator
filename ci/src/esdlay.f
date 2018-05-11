      SUBROUTINE ESDLAY (ISPE,IAS,DRAW,DELAY)
      IMPLICIT NONE
C----------
C CI $Id: esdlay.f 0000 2018-02-14 00:00:00Z gedixon $
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
C----------
C     CONTAINS WEIBULL MAXIMUM LIKLIHOOD FUNCTIONS FOR DETERMINING
C     THE NUMBER OF YEARS BETWEEN LAST PLOT DISTURBANCE AND
C     GERMINATION OF BEST TREES.
C-----------
      INTEGER IAS,ISPE,IT,IBW,IB,IBAA,I
      REAL BSUB(3,MAXSP),CSUB(3,MAXSP),BBW(3,4),CBW(3,4),
     &  BADV(2,MAXSP),CADV(2,MAXSP),BBW1(2,4),CBW1(2,4),BBW2(2,4),
     &  CBW2(2,4)
      REAL DELAY,DRAW,BB,CC
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
C-----------
C     PLOT AGE (YRS): 2 THRU 7   8 THRU 12   13 THRU 20
C-----------
      DATA BSUB/
     1                3.52946,   7.62339,    12.79801,
     2                5.23792,   7.38005,    10.42350,
     3                4.34376,   6.55916,     9.16226,
     4                4.17909,   5.88262,     8.49857,
     5                4.33094,   6.30802,     8.63060,
     6                4.16284,   7.52536,    10.20937,
     7                5.33757,   6.78727,     9.45827,
     8                5.36466,   7.44468,     9.69507,
     9                3.45725,   6.34975,     8.65545,
     O                3.81610,   5.74622,     9.36345,
     1                     0.,        0.,          0.,
     2                     0.,        0.,          0.,
     3                     0.,        0.,          0.,
     4                     0.,        0.,          0.,
     5                     0.,        0.,          0.,
     6                     0.,        0.,          0.,
     7                     0.,        0.,          0.,
     8                4.33094,   6.30802,     8.63060,
     9                     0.,        0.,          0./
C
      DATA BBW/       4.01218,   5.86172,    10.61297,
     4                3.67409,   6.12256,     8.74195,
     8                5.92251,   8.62548,     8.76074,
     9                4.11555,   6.19124,     8.82962/
C----------
C     PLOT AGE (YRS): 3 THRU 7   8 THRU 12   13 THRU 20
C----------
      DATA CSUB/
     1                1.71621,   2.72466,     3.98359,
     2                3.11598,   3.04038,     2.87196,
     3                2.33194,   2.63560,     2.21663,
     4                2.47058,   2.25957,     2.00065,
     5                1.97408,   2.35053,     2.00997,
     6                2.03892,   3.12279,     2.68340,
     7                4.16994,   3.59937,     2.39138,
     8                2.89777,   2.80504,     3.27745,
     9                2.06804,   2.79933,     2.28687,
     O                3.01975,   2.09376,     1.80925,
     1                     0.,        0.,          0.,
     2                     0.,        0.,          0.,
     3                     0.,        0.,          0.,
     4                     0.,        0.,          0.,
     5                     0.,        0.,          0.,
     6                     0.,        0.,          0.,
     7                     0.,        0.,          0.,
     8                1.97408,   2.35053,     2.00997,
     9                     0.,        0.,          0./
C
      DATA CBW/       2.16561,   2.16010,     2.44953,
     4                2.19139,   2.38522,     2.03146,
     8                3.31325,   5.21269,     2.30270,
     9                1.98007,   2.84149,     2.46665/
C----------
C     OVERSTORY BA:  0 TO 25  26+ SQ.FT./A.
C     NO BUDWORM BEFORE HARVEST
C----------
      DATA BADV/
     1              6.699826, 13.431179,
     2              9.768223, 27.100242,
     3             13.121021, 22.186540,
     4             11.269182, 18.245664,
     5             13.604594, 19.605485,
     6             17.779381, 24.241344,
     7              7.358880, 32.955809,
     8             13.990273, 21.779362,
     9             21.962337, 32.176727,
     O              8.986115, 10.312660,
     1                    0.,        0.,
     2                    0.,        0.,
     3                    0.,        0.,
     4                    0.,        0.,
     5                    0.,        0.,
     6                    0.,        0.,
     7                    0.,        0.,
     8             13.604594, 19.605485,
     9                    0.,        0./
C----------
C     BUDWORM 1-3 YEARS BEFORE HARVEST
C----------
      DATA BBW1/   16.856252, 18.601097,
     4             11.393035, 19.994331,
     8              8.388204, 18.023409,
     9             20.207660, 30.052681/
C----------
C     BUDWORM 4-5 YEARS BEFORE HARVEST
C----------
      DATA BBW2/   18.833076, 17.660965,
     4             16.154050, 13.936793,
     8             10.587442, 16.554662,
     9             25.512363, 20.555202/
C----------
C     NO BUDWORM BEFORE HARVEST
C----------
      DATA CADV/
     1              1.262533,  1.279302,
     2              1.152577,  1.319692,
     3              1.043254,  1.215651,
     4              1.122139,  1.082805,
     5              1.267057,  1.287445,
     6              1.337217,  1.540663,
     7              0.912295,  1.230540,
     8              1.051296,  1.416222,
     9              1.101266,  1.317022,
     O              1.068472,  1.470405,
     1                    0.,        0.,
     2                    0.,        0.,
     3                    0.,        0.,
     4                    0.,        0.,
     5                    0.,        0.,
     6                    0.,        0.,
     7                    0.,        0.,
     8              1.267057,  1.287445,
     9                    0.,        0./
C----------
C     BUDWORM 1-3 YEARS BEFORE HARVEST
C----------
      DATA CBW1/    1.165449,  1.182513,
     4              1.137030,  1.355774,
     8              1.332778,  1.502281,
     9              1.660504,  1.832537/
C----------
C     BUDWORM 4-5 YEARS BEFORE HARVEST
C----------
      DATA CBW2/    1.307195,  1.938481,
     4              1.330297,  1.311782,
     8              1.470496,  1.207402,
     9              1.871949,  1.248784/
C----------
      IT=1
      IF(TIME.GT.7.5.AND.TIME.LT.12.5) IT=2
      IF(TIME.GT.12.5) IT=3
      IBW=INT(BWB4 + BWAF + 0.5)
      IB=1
      IF(IBW.GT.2) IB=2
      IBAA=1
      IF(BAA.GT.25.5) IBAA=2
      IF(IAS.EQ.2) GO TO 20
C----------
C     ADVANCE REGENERATION
C----------
      BB=BADV(IBAA,ISPE)
      CC=CADV(IBAA,ISPE)
      IF(BWB4.LT.0.5) GO TO 40
      IF(ISPE.NE.3.AND.ISPE.NE.4.AND.ISPE.NE.8.AND.ISPE.NE.9)
     &  GO TO 40
      IF(ISPE.EQ.3) I=1
      IF(ISPE.EQ.4) I=2
      IF(ISPE.EQ.8) I=3
      IF(ISPE.EQ.9) I=4
      IF(BWB4.GT.3.5) GO TO 45
      BB=BBW1(IBAA,I)
      CC=CBW1(IBAA,I)
      GO TO 40
   45 CONTINUE
      BB=BBW2(IBAA,I)
      CC=CBW2(IBAA,I)
   40 CONTINUE
      DELAY=((-(ALOG(1.0-DRAW)))**(1.0/CC))*BB
      DELAY= (DELAY+3.0) *(-1.0)
      GO TO 50
   20 CONTINUE
C----------
C     SUBSEQUENT REGENERATION
C----------
      BB=BSUB(IT,ISPE)
      CC=CSUB(IT,ISPE)
      IF(IB.NE.2) GO TO 30
      IF(ISPE.NE.3.AND.ISPE.NE.4.AND.ISPE.NE.8.AND.ISPE.NE.9)
     &  GO TO 30
      IF(ISPE.EQ.3) I=1
      IF(ISPE.EQ.4) I=2
      IF(ISPE.EQ.8) I=3
      IF(ISPE.EQ.9) I=4
      BB=BBW(IT,I)
      CC=CBW(IT,I)
   30 CONTINUE
      DELAY=((-(ALOG(1.0-DRAW)))**(1.0/CC))*BB
      DELAY=DELAY-4.0
   50 CONTINUE
C
      IF(DELAY .GE. 10.)THEN
        DELAY=10.
      ELSEIF(DELAY .LE. 0.)THEN
        DELAY=0.
      ENDIF
C
      RETURN
      END
