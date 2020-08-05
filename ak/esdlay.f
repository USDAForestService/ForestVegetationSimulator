      SUBROUTINE ESDLAY (ISPE,IAS,DRAW,DELAY)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     CONTAINS WEIBULL MAXIMUM LIKLIHOOD FUNCTIONS FOR DETERMINING
C     THE NUMBER OF YEARS BETWEEN LAST PLOT DISTURBANCE AND
C     GERMINATION OF BEST TREES.
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
      INCLUDE 'ESCOM2.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER IAS,ISPE,IT,IBW,IB,IBAA,I
C
      REAL BB,CC,DELAY,DRAW
C
      REAL BADV(2,MAXSP),BBW(3,4),BBW1(2,4),BBW2(2,4),BSUB(3,MAXSP)
      REAL CADV(2,MAXSP),CBW(3,4),CBW1(2,4),CBW2(2,4),CSUB(3,MAXSP)
C
C----------
C     PLOT AGE (YRS): 2 THRU 7   8 THRU 12   13 THRU 20
C     ORIGINAL VALUES FROM 13 SPECIES AK MARKED WITH #
C
      DATA BSUB/
     &                4.34376,   6.55916,     9.16226, ! #  1  SF = pacific silver fir
     &                3.45725,   6.34975,     8.65545, ! #  2  AF = subalpine fir
     &                4.16284,   7.52536,    10.20937, ! #  3  YC = alaska-cedar
     &                3.52946,   7.62339,    12.79801, !    4  TA   tamarack
     &                3.52946,   7.62339,    12.79801, ! #  5  WS = white spruce
     &                3.52946,   7.62339,    12.79801, !    6  LS = Lutz's spruce
     &                3.52946,   7.62339,    12.79801, !    7  BE = black spruce
     &                5.36466,   7.44468,     9.69507, ! #  8  SS = sitka spruce
     &                5.33757,   6.78727,     9.45827, ! #  9  LP = lodgepole pine
     &                5.23792,   7.38005,    10.42350, ! # 10  RC = western redcedar
     &                4.33094,   6.30802,     8.63060, ! # 11  WH = western hemlock
     &                4.17909,   5.88262,     8.49857, ! # 12  MH = mountain hemlock
     &                4.33094,   6.30802,     8.63060, ! # 13  OS = other softwoods
     &                3.81610,   5.74622,     9.36345, !   14  AD = alder species
     &                3.81610,   5.74622,     9.36345, ! # 15  RA = red alder
     &                3.81610,   5.74622,     9.36345, !   16  PB = paper birch
     &                3.81610,   5.74622,     9.36345, !   17  AB = Alaska birch
     &                3.81610,   5.74622,     9.36345, !   18  BA = balsam poplar
     &                3.81610,   5.74622,     9.36345, !   19  AS = quaking aspen
     &                3.81610,   5.74622,     9.36345, ! # 20  CW = black cottonwood
     &                3.81610,   5.74622,     9.36345, !   21  WI = willow species
     &                3.81610,   5.74622,     9.36345, !   22  SU = Scouler's willow
     &                3.81610,   5.74622,     9.36345/ ! # 23  OH = other hardwoods
C
C     ONLY ORIGINAL SPECIES, NONE ADDED FOR 23 SPECIES EXPANSION.
      DATA BBW/       4.01218,   5.86172,    10.61297, !  1 SF = pacific silver fir
     &                4.11555,   6.19124,     8.82962, !  2 AF = subalpine fir
     &                5.92251,   8.62548,     8.76074, !  8 SS = sitka spruce
     &                3.67409,   6.12256,     8.74195/ ! 12 MH = mountain hemlock
C
C     PLOT AGE (YRS): 3 THRU 7   8 THRU 12   13 THRU 20
C     ORIGINAL VALUES FROM 13 SPECIES AK MARKED WITH #
C
      DATA CSUB/
     &                2.33194,   2.63560,     2.21663, ! #  1  SF = pacific silver fir
     &                2.06804,   2.79933,     2.28687, ! #  2  AF = subalpine fir
     &                2.03892,   3.12279,     2.68340, ! #  3  YC = alaska-cedar
     &                1.71621,   2.72466,     3.98359, !    4  TA   tamarack
     &                1.71621,   2.72466,     3.98359, ! #  5  WS = white spruce
     &                1.71621,   2.72466,     3.98359, !    6  LS = Lutz's spruce
     &                1.71621,   2.72466,     3.98359, !    7  BE = black spruce
     &                2.89777,   2.80504,     3.27745, ! #  8  SS = sitka spruce
     &                4.16994,   3.59937,     2.39138, ! #  9  LP = lodgepole pine
     &                3.11598,   3.04038,     2.87196, ! # 10  RC = western redcedar
     &                1.97408,   2.35053,     2.00997, ! # 11  WH = western hemlock
     &                2.47058,   2.25957,     2.00065, ! # 12  MH = mountain hemlock
     &                1.97408,   2.35053,     2.00997, ! # 13  OS = other softwoods
     &                3.01975,   2.09376,     1.80925, !   14  AD = alder species
     &                3.01975,   2.09376,     1.80925, ! # 15  RA = red alder
     &                3.01975,   2.09376,     1.80925, !   16  PB = paper birch
     &                3.01975,   2.09376,     1.80925, !   17  AB = Alaska birch
     &                3.01975,   2.09376,     1.80925, !   18  BA = balsam poplar
     &                3.01975,   2.09376,     1.80925, !   19  AS = quaking aspen
     &                3.01975,   2.09376,     1.80925, ! # 20  CW = black cottonwood
     &                3.01975,   2.09376,     1.80925, !   21  WI = willow species
     &                3.01975,   2.09376,     1.80925, !   22  SU = Scouler's willow
     &                3.01975,   2.09376,     1.80925/ ! # 23  OH = other hardwoods
C
C     ONLY ORIGINAL SPECIES, NONE ADDED FOR 23 SPECIES EXPANSION.
      DATA CBW/       2.16561,   2.16010,     2.44953, !  1 SF = pacific silver fir
     &                1.98007,   2.84149,     2.46665, !  2 AF = subalpine fir
     &                3.31325,   5.21269,     2.30270, !  8 SS = sitka spruce
     &                2.19139,   2.38522,     2.03146/ ! 12 MH = mountain hemlock
C
C     OVERSTORY BA:  0 TO 25  26+ SQ.FT./A.
C     NO BUDWORM BEFORE HARVEST
C     ORIGINAL VALUES FROM 13 SPECIES AK MARKED WITH #
C
      DATA BADV/
     &             13.121021, 22.186540, ! #  1  SF = pacific silver fir
     &             21.962337, 32.176727, ! #  2  AF = subalpine fir
     &             17.779381, 24.241344, ! #  3  YC = alaska-cedar
     &              6.699826, 13.431179, !    4  TA   tamarack
     &              6.699826, 13.431179, ! #  5  WS = white spruce
     &              6.699826, 13.431179, !    6  LS = Lutz's spruce
     &              6.699826, 13.431179, !    7  BE = black spruce
     &             13.990273, 21.779362, ! #  8  SS = sitka spruce
     &              7.358880, 32.955809, ! #  9  LP = lodgepole pine
     &              9.768223, 27.100242, ! # 10  RC = western redcedar
     &             13.604594, 19.605485, ! # 11  WH = western hemlock
     &             11.269182, 18.245664, ! # 12  MH = mountain hemlock
     &             13.604594, 19.605485, ! # 13  OS = other softwoods
     &              8.986115, 10.312660, !   14  AD = alder species
     &              8.986115, 10.312660, ! # 15  RA = red alder
     &              8.986115, 10.312660, !   16  PB = paper birch
     &              8.986115, 10.312660, !   17  AB = Alaska birch
     &              8.986115, 10.312660, !   18  BA = balsam poplar
     &              8.986115, 10.312660, !   19  AS = quaking aspen
     &              8.986115, 10.312660, ! # 20  CW = black cottonwood
     &              8.986115, 10.312660, !   21  WI = willow species
     &              8.986115, 10.312660, !   22  SU = Scouler's willow
     &              8.986115, 10.312660/ ! # 23  OH = other hardwoods
C
C     BUDWORM 1-3 YEARS BEFORE HARVEST
C     ONLY ORIGINAL SPECIES, NONE ADDED FOR 23 SPECIES EXPANSION.
C
      DATA BBW1/   16.856252, 18.601097, !  1 SF = pacific silver fir
     &             20.207660, 30.052681, !  2 AF = subalpine fir
     &              8.388204, 18.023409, !  8 SS = sitka spruce
     &             11.393035, 19.994331/ ! 12 MH = mountain hemlock
C
C     BUDWORM 4-5 YEARS BEFORE HARVEST
C     ONLY ORIGINAL SPECIES, NONE ADDED FOR 23 SPECIES EXPANSION.
C
      DATA BBW2/   18.833076, 17.660965, !  1 SF = pacific silver fir
     &             25.512363, 20.555202, !  2 AF = subalpine fir
     &             10.587442, 16.554662, !  8 SS = sitka spruce
     &             16.154050, 13.936793/ ! 12 MH = mountain hemlock
C
C     NO BUDWORM BEFORE HARVEST
C     ORIGINAL VALUES FROM 13 SPECIES AK MARKED WITH #
C
      DATA CADV/
     &              1.043254,  1.215651, ! #  1  SF = pacific silver fir
     &              1.101266,  1.317022, ! #  2  AF = subalpine fir
     &              1.337217,  1.540663, ! #  3  YC = alaska-cedar
     &              1.262533,  1.279302, !    4  TA   tamarack
     &              1.262533,  1.279302, ! #  5  WS = white spruce
     &              1.262533,  1.279302, !    6  LS = Lutz's spruce
     &              1.262533,  1.279302, !    7  BE = black spruce
     &              1.051296,  1.416222, ! #  8  SS = sitka spruce
     &              0.912295,  1.230540, ! #  9  LP = lodgepole pine
     &              1.152577,  1.319692, ! # 10  RC = western redcedar
     &              1.267057,  1.287445, ! # 11  WH = western hemlock
     &              1.122139,  1.082805, ! # 12  MH = mountain hemlock
     &              1.267057,  1.287445, ! # 13  OS = other softwoods
     &              1.068472,  1.470405, !   14  AD = alder species
     &              1.068472,  1.470405, ! # 15  RA = red alder
     &              1.068472,  1.470405, !   16  PB = paper birch
     &              1.068472,  1.470405, !   17  AB = Alaska birch
     &              1.068472,  1.470405, !   18  BA = balsam poplar
     &              1.068472,  1.470405, !   19  AS = quaking aspen
     &              1.068472,  1.470405, ! # 20  CW = black cottonwood
     &              1.068472,  1.470405, !   21  WI = willow species
     &              1.068472,  1.470405, !   22  SU = Scouler's willow
     &              1.068472,  1.470405/ ! # 23  OH = other hardwoods
C
C     BUDWORM 1-3 YEARS BEFORE HARVEST
C     ONLY ORIGINAL SPECIES, NONE ADDED FOR 23 SPECIES EXPANSION.
C
      DATA CBW1/    1.165449,  1.182513, !  1 SF = pacific silver fir
     &              1.660504,  1.832537, !  2 AF = subalpine fir
     &              1.332778,  1.502281, !  8 SS = sitka spruce
     &              1.137030,  1.355774/ ! 12 MH = mountain hemlock
C
C     BUDWORM 4-5 YEARS BEFORE HARVEST
C     ONLY ORIGINAL SPECIES, NONE ADDED FOR 23 SPECIES EXPANSION.
C
      DATA CBW2/    1.307195,  1.938481, !  1 SF = pacific silver fir
     &              1.871949,  1.248784, !  2 AF = subalpine fir
     &              1.470496,  1.207402, !  8 SS = sitka spruce
     &              1.330297,  1.311782/ ! 12 MH = mountain hemlock
C----------
      IT=1
      IF(TIME.GT.7.5.AND.TIME.LT.12.5) IT=2
      IF(TIME.GT.12.5) IT=3
      IBW = INT(BWB4+BWAF+0.5)
      IB=1
      IF(IBW.GT.2) IB=2
      IBAA=1
      IF(BAA.GT.25.5) IBAA=2
C     ADVANCE REGENERATION
      IF(IAS .EQ. 1) THEN
        BB=BADV(IBAA,ISPE)
        CC=CADV(IBAA,ISPE)
        IF(BWB4.LT.0.5) GO TO 40
        SELECT CASE (ISPE)
        CASE(1)  ! SF
          I = 1
        CASE(2)  ! AF
          I = 2
        CASE(8)  ! SS
          I = 3
        CASE(12) ! MH
          I = 4
        CASE DEFAULT
          GO TO 40
        END SELECT
        IF(BWB4 .LE. 3.5) THEN
          BB=BBW1(IBAA,I)
          CC=CBW1(IBAA,I)
        ELSE
          BB=BBW2(IBAA,I)
          CC=CBW2(IBAA,I)
        ENDIF
   40   CONTINUE
        DELAY=((-(ALOG(1.0-DRAW)))**(1.0/CC))*BB
        DELAY= (DELAY+3.0) *(-1.0)
C     SUBSEQUENT REGENERATION
      ELSE
        BB=BSUB(IT,ISPE)
        CC=CSUB(IT,ISPE)
        IF(IB.NE.2) GO TO 30
        SELECT CASE (ISPE)
        CASE(1)  ! SF
          I = 1
        CASE(2)  ! AF
          I = 2
        CASE(8)  ! SS
          I = 3
        CASE(12) ! MH
          I = 4
        CASE DEFAULT
          GO TO 30
        END SELECT
        BB=BBW(IT,I)
        CC=CBW(IT,I)
   30   CONTINUE
        DELAY=((-(ALOG(1.0-DRAW)))**(1.0/CC))*BB
        DELAY=DELAY-4.0
      ENDIF
      IF(DELAY .GE. 10.)THEN
        DELAY=10.
      ELSEIF(DELAY .LE. 0.)THEN
        DELAY=0.
      ENDIF
C
      RETURN
      END
