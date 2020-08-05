      SUBROUTINE ESTPP (VAL,IFT,TPP)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICTS THE NUMBER OF NATURAL REGENERATING TREES
C     TO ESTABLISH PER PLOT
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
C               12 = 904
C               13 = 911
C               14 = OTHER (NO ADVANCED REGENERATION)
C  BB AND BBIFT   -- WEIBULL SCALE PARAMETER BY FOREST TYPE
C  CC AND CCIFT   -- WEIBULL SHAPE PARAMETER BY FOREST TYPE
C  TPP      -- NUMBER OF TREES PER STOCKED PLOT
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      REAL BB,CC,TPP,VAL,BBIFT(14),CCIFT(14)
      INTEGER IFT
C----------
      DATA BBIFT / 
     & 9.440746, 18.438901, 7.917605, 14.978455, 14.686893, 12.604723,
     & 14.930945, 6.998232, 6.600799, 10.077218, 9.147391, 6.600799,
     & 5.64779, 9.440746 /
      DATA CCIFT /    
     & 0.90446, 1.203039, 0.962189, 1.178401, 1.152716, 0.99968, 
     & 1.161708, 0.893172, 0.906161, 1.011961, 0.911872, 0.906161, 
     & 1.01385, 0.90446 /  
C     CALCULATE TREES PER STOCKED PLOT.
      BB = BBIFT(IFT)
      CC = CCIFT(IFT)
      TPP = BB * (-1*ALOG(1-VAL))**(1/CC)
C     SET TO 0 IF FOREST TYPE IS OTHER (14)
      IF(IFT.EQ.14)TPP=0.
      RETURN
      END
