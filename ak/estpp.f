      SUBROUTINE ESTPP (VAL,TPP,IFT)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  DIFFERS FROM ESTB ROUTINE BY MULTIPLE AT BOTTOM.
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
C  VARIABLE DEFINITIONS:
C----------
C
C  VAL      -- RANDOM NUMBER DRAW
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
C               12 = 911
C               13 = OTHER (NO ADVANCED REGENERATION)
C  BB AND BBIFT   -- WEIBULL SCALE PARAMETER BY FOREST TYPE
C  CC AND CCIFT   -- WEIBULL SHAPE PARAMETER BY FOREST TYPE
C  TPP      -- NUMBER OF TREES PER STOCKED PLOT
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      REAL BB,CC,TPP,VAL,BBIFT(13),CCIFT(13)
      INTEGER IFT
C
C----------
      DATA BBIFT / 
     &  9.277339,18.523823, 7.838685,15.218526,14.702989,12.588958,
     & 14.752861, 7.091332, 6.918244,10.130905, 9.125033, 7.732252,
     &  9.277339 /
      DATA CCIFT /    
     & 0.900404,1.208742,0.971332, 1.18826,1.147799,1.003261,
     & 1.145323,0.893681,0.907493,1.010962,0.910616,0.917219, 
     & 0.900404 /  
C----------
C     CALCULATION OF TREES PER STOCKED PLOT.
C----------
      BB = BBIFT(IFT)
      CC = CCIFT(IFT)
      TPP = BB * (-1*ALOG(1-VAL))**(1/CC)
C----------
C SET TO 0 IF FOREST TYPE IS OTHER (13)
C----------
      IF(IFT.EQ.13)TPP=0.
      RETURN
      END
