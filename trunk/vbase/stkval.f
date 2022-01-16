      SUBROUTINE STKVAL(S)
      IMPLICIT NONE
C----------
C VBASE $Id$
C----------
C     THIS ROUTINE CALCULATES THE STOCKING VALUES FOR EACH
C     TREE IN THE STAND BASED ON ARNER 2001, NATIONAL ALGORITHMS
C     FOR STOCKING, STAND SIZE, AND FOREST TYPE FOR FOREST INVENTORY
C     AND ANALYSIS PLOTS.  TO PROVIDE ADEQUATE CODE DOCUMENTATION,
C     STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
C     THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
C     END OF VALID FORTRAN STATEMENTS.
C
C     CALLED FROM FORTYP
C
C
C     S      - THIS ARRAY IS LOADED IN THIS ROUTINE WITH STOCKING VALUES
C              ACCORDING TO INITIAL FOREST TYPE.
C----------
C  INTERNAL VARIABLE DEFINITIONS
C----------
C     DMXSTD - D(IND(1)) THE MAXIMUM DBH IN THE STAND
C     DMXSS  - DIAMETER MAXIMUM FOR DIA < 5.O" TREES
C     TOTSTK - TOTAL STAND STOCKING FOR ALL TREES
C     TTST51 - SUM OF TOTAL STAND STOCKING FOR TREES
C              GREATER THAN 5 IN DBH, PROCESS STEP 1
C     TOTST5 - SUM OF TOTAL STAND STOCKING FOR TREES
C              GREATER THAN 5 IN DBH
C     TOTST3 - SUM OF TOTAL STAND STOCKING FOR TREES
C              > 0.1" & < 5.0" IN DBH (SAPLINGS)
C     TOTST1 - SUM OF TOTAL STAND STOCKING FOR TREES
C              EQUAL TO 0.1 IN DBH (SEEDLINGS)
C     SZCL   - SIZE CLASS ARRAY (3) SMALL/MEDIUM/LARGE
C     ISZCL  - SIZE CLASS CODE
C     ISTCL  - STOCKING CLASS CODE
C     SS     - SS(MAXSP) THE STOCKING VALUE FOR EACH SPECIES CALCULATED
C              IN THIS ROUTINE
C     TAB2   - COEFFICIENTS FOR THE 36 STOCKING EQUATIONS
C     TAB3   - REPRESENTS POTENTIALLY 1000 SPECIES BY FIA NUMBER.
C              TAB3(FIA,1)= INITIAL TYPE GROUP NUMBER
C              TAB3(FIA,2)= STOCKING EQUATION NUMBER
C
COMMON BLOCKS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'CONTRL.F77'
C
      INCLUDE 'ARRAYS.F77'
C
      INCLUDE 'PLOT.F77'
C----------
C  DECLARATIONS
C----------
      INTEGER IS,IFIA,I3,I2,I1,ISPC
      REAL PRE,PRA,PRG,UE,UA,EI,AI,GI,UG,STKTR2,CF,Q,D,B1,B0
      REAL DMXSS,DMXSTD,TOTST1,TOTST3,TOTST5,TTST52,TTST51,TOTSTK,DMAX
      REAL STKTR1
      INTEGER I,J
      REAL TAB2(36,2)
      INTEGER TAB3(1000,2)
      REAL SZCL(3)
      REAL SS(MAXSP),S(210)
      LOGICAL DEBUG
C----------
C  DATA STATEMENTS
C----------
      DATA ((TAB2(I,J),J= 1,2),I= 1,15) /
     &             0.00869, 1.48,
     &             0.00454, 1.73,
     &             0.01691, 1.05,
     &             0.00946, 1.59,
     &             0.00422, 1.70,
     &             0.00509, 1.81,
     &             0.00458, 1.91,
     &             0.00335, 1.73,
     &             0.01367, 1.44,
     &             0.00250, 2.00,
     &             0.00609, 1.67,
     &             0.00914, 1.67,
     &             0.00900, 1.51,
     &             0.00680, 1.72,
     &             0.00769, 1.54/
      DATA ((TAB2(I,J),J= 1,2),I= 16,30) /
     &             0.00433, 1.80,
     &             0.00313, 2.11,
     &             0.00427, 1.67,
     &             0.00333, 1.68,
     &             0.00000, 1.00,
     &             0.00000, 1.00,
     &             0.00000, 1.00,
     &             0.00000, 1.00,
     &             0.00000, 1.00,
     &             0.01105, 1.53,
     &             0.01671, 1.41,
     &             0.00694, 1.86,
     &             0.00635, 1.89,
     &             0.01119, 1.63,
     &             0.01546, 1.50/
      DATA ((TAB2(I,J),J= 1,2),I= 31,36) /
     &             0.00429, 1.87,
     &             0.01429, 1.46,
     &             0.02197, 1.13,
     &             0.00000, 1.00,
     &             0.00442, 2.02,
     &             0.00688, 1.86/
      DATA ((TAB3(I,J),J=1,1),I=  1,100)/
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  55,
     &   1,  55,   0,   2,   2,  55,   3,   4,   4,   5,
     &   5,   7,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   8,   9,  59,   0,   0,   0,   0,   0,   0,  12,
     &  12,  12,  12,  12,  12,   0,  64, 161, 161, 161,
     & 161, 161, 161,  38, 161,  63,  64,  64, 161,  70,
     &  65,  40,  13,   0,   0,   0,   0,   0,   0,   0,
     &  10,   0,   0,   0,   0,   0,   0,   0,   0,  16,
     &  70,  40,  14,  16,  17,  15,  58,  18,  16,  42/
      DATA ((TAB3(I,J),J=1,1),I=101,200)/
     &  19,  20,  21,  20,  41, 162,  44,  23,  24,  45,
     &  46,  22,  25,  22,  47,  36,  27,  22,  28,   6,
     &  48,  26,  49,  29,  42,  50, 163,  51,  53,  71,
     &  52,  54, 162, 162,  26,  70,  22,  22,  22, 162,
     &  22,  20, 162,  72,  70,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0/
      DATA ((TAB3(I,J),J=1,1),I=201,300)/
     &  30,  31,   0,   0,   0,   0,   0,   0,   0,   0,
     &  32,  33,   0,   0,   0,   0,   0,   0,   0,   0,
     &  61,  61,  61,   0,   0,   0,   0,   0,   0,   0,
     &  40,  40,   0,   0,   0,   0,   0,   0,   0,  60,
     &  60,  11,   0,   0,   0,   0,   0,   0,   0,   0,
     &  40,  40,   0,   0,   0,   0,   0,   0,   0,  66,
     &  66,  66,  34,  35,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0, 170,   0, 160/
      DATA ((TAB3(I,J),J=1,1),I=301,400)/
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 152,
     & 151, 130, 208,  96, 152,  95,  97,  96, 152, 152,
     & 159, 159, 159, 159, 159,   0,   0,   0,   0, 153,
     & 153, 153, 153, 153, 153, 153, 153,   0,   0,   0,
     & 151,   0,   0,   0, 153, 153,   0,   0,   0, 153,
     & 131, 153, 153,   0, 146, 152,   0,   0,   0,   0,
     & 132, 132, 132,   0,   0,   0, 152,   0,   0,  98,
     &  98,  98, 129, 151,  99,  99,  99,  99,  99,   0,
     & 151,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 152,   0,   0,   0,   0,   0,   0,   0,   0,  92/
      DATA ((TAB3(I,J),J=1,1),I=401,500)/
     &  90,  92,  92,  91,  90,  92,  92,  92,  92,  92,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 152, 152, 152,   0,   0,   0,   0,   0,   0, 133,
     & 133,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 152,
     & 152, 152,   0,   0,   0,   0,   0,   0,   0, 115,
     & 100, 115, 115,   0,   0,   0,   0,   0,   0,   0,
     & 152,   0,   0,   0, 156, 156, 156, 156, 156,   0,
     & 153,   0,   0,   0,   0,   0,   0,   0,   0, 153,
     & 153, 153,   0,   0,   0,   0,   0,   0,   0, 101/
      DATA ((TAB3(I,J),J=1,1),I=501,600)/
     & 101, 101,   0,   0,   0,   0,   0,   0,   0, 148,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  93,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 102,   0,   0,   0,   0,   0,   0,   0,   0, 153,
     & 103, 135, 104, 105, 153, 153, 153, 153, 153,   0,
     & 151, 101,   0,   0, 127,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 101,   0,   0,   0,   0,   0,   0,   0,   0, 153,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 106,   0,   0,   0,   0,   0,   0,   0,   0, 153/
      DATA ((TAB3(I,J),J=1,1),I=601,700)/
     & 107, 108, 153, 153, 153, 153,   0,   0,   0,   0,
     & 109,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 110,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 136,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 101,   0,   0,   0,   0,   0,   0,   0,   0, 152,
     & 152, 152, 111, 152, 152, 152, 152, 152,   0, 152,
     & 152, 152, 152, 152, 152,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 152,
     & 152, 152, 152, 152,   0,   0,   0,   0,   0, 113,
     & 112, 151, 113, 114,   0,   0,   0,   0,   0,   0/
      DATA ((TAB3(I,J),J=1,1),I=701,800)/
     & 152, 152,   0,   0,   0,   0,   0,   0,   0,   0,
     & 152, 144,   0,   0,   0,   0,   0,   0,   0,   0,
     & 127, 151,   0,   0,   0,   0,   0,   0,   0, 153,
     & 116, 153,   0,   0,   0,   0,   0,   0,   0, 118,
     & 117, 118, 119, 118, 118, 119, 137, 118, 118,   0,
     &   0, 118,   0,   0, 157, 157, 157, 157,   0, 152,
     & 152, 121, 152, 152, 152, 152,   0, 152,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 158/
      DATA ((TAB3(I,J),J=1,1),I=801,900)/
     & 138,  81, 210, 151, 142,  82, 134,  89,  86, 210,
     & 210,  88,  87, 158, 140,  89, 202, 139,  89, 201,
     & 158, 128,  83, 206,  87, 207, 204, 143, 210, 205,
     & 125,  84,  85,  87,  86,  87, 120, 203, 142,  86,
     &  89,  89, 210,   0,   0,   0,   0,   0,   0, 210,
     & 152,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 152/
      DATA ((TAB3(I,J),J=1,1),I=901,1000)/
     & 122, 160,   0,   0,   0,   0,   0,   0,   0,   0,
     & 147,   0,   0,   0,   0,   0,   0,   0, 158, 123,
     & 123, 123, 123, 123,   0,   0, 123,   0, 123,   0,
     &  93,   0,   0,   0, 152, 152, 152, 152, 152,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 124,
     & 124, 124, 124,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0, 152,
     & 209,  94, 209, 146,  94, 152,  94,   0,   0,   0,
     & 141,   0,   0,   0,   0,   0,   0,   0, 149, 160,
     & 153, 145, 146, 146, 146, 153, 153, 180, 190,   0/
      DATA ((TAB3(I,J),J=2,2),I=  1,100)/
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   1,
     &  18,   1,   0,  18,  15,   1,  18,   1,   1,  18,
     &  18,  18,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  18,  18,  16,   0,   0,   0,   0,   0,   0,  10,
     &  10,  10,  10,  10,  10,   0,   4,  10,  10,   3,
     &   3,  10,  10,  10,  10,  10,   4,   4,  10,   1,
     &   1,   2,   2,   0,   0,   0,   0,   0,   0,   0,
     &  18,   0,   0,   0,   0,   0,   0,   0,   0,   1,
     &   1,  18,   1,   1,   3,   1,   1,  18,   1,  11/
      DATA ((TAB3(I,J),J=2,2),I=101,200)/
     &  10,  10,   8,   8,   4,  10,   4,   5,  10,   6,
     &   7,  10,  10,  10,   4,  10,  10,  10,   8,  10,
     &   9,  10,   4,  10,  11,   4,  10,  12,  13,   4,
     &  14,   4,  10,  10,  10,  11,  10,  10,  10,  10,
     &  10,  10,  10,   4,   4,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0/
      DATA ((TAB3(I,J),J=2,2),I=201,300)/
     &  15,  15,   0,   0,   0,   0,   0,   0,   0,   0,
     &  19,  19,   0,   0,   0,   0,   0,   0,   0,   0,
     &  31,  31,  31,   0,   0,   0,   0,   0,   0,   0,
     &  18,  18,   0,   0,   0,   0,   0,   0,   0,  16,
     &  16,  18,   0,   0,   0,   0,   0,   0,   0,   0,
     &  18,  18,   0,   0,   0,   0,   0,   0,   0,  17,
     &  17,  17,  18,  18,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,  13,   0,  25/
      DATA ((TAB3(I,J),J=2,2),I=301,400)/
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  25,
     &  25,  25,  36,  27,  27,  25,  25,  27,  25,  25,
     &  10,  10,  10,  10,  10,   0,   0,   0,   0,  27,
     &  27,  27,  27,  27,  27,  27,  27,   0,   0,   0,
     &  25,   0,   0,   0,  36,  36,   0,   0,   0,  26,
     &  26,  26,  26,   0,  26,  25,   0,   0,   0,   0,
     &  29,  29,  29,   0,   0,   0,  25,   0,   0,  27,
     &  27,  27,  28,  28,  28,  28,  28,  28,  28,   0,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,  29/
      DATA ((TAB3(I,J),J=2,2),I=401,500)/
     &  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  25,  29,  29,   0,   0,   0,   0,   0,   0,  29,
     &  29,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  27,
     &  27,  27,   0,   0,   0,   0,   0,   0,   0,  36,
     &  36,  36,  36,   0,   0,   0,   0,   0,   0,   0,
     &  25,   0,   0,   0,  10,  33,  33,  33,  33,   0,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,  25,
     &  25,  26,   0,   0,   0,   0,   0,   0,   0,  29/
      DATA ((TAB3(I,J),J=2,2),I=501,600)/
     &  29,  29,   0,   0,   0,   0,   0,   0,   0,  15,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  29,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  27,   0,   0,   0,   0,   0,   0,   0,   0,  33,
     &  33,  33,  33,  36,  33,  33,  33,  33,  33,   0,
     &  25,  27,   0,   0,  25,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,  25,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,  30/
      DATA ((TAB3(I,J),J=2,2),I=601,700)/
     &  30,  30,  30,  30,  30,  30,   0,   0,   0,   0,
     &  31,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  33,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &  29,   0,   0,   0,   0,   0,   0,   0,   0,  33,
     &  33,  33,  25,  33,  33,  33,  33,  33,   0,  29,
     &  29,  29,  29,  29,  29,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  25,
     &  25,  25,  25,  25,   0,   0,   0,   0,   0,  31,
     &  31,  31,  31,  31,   0,   0,   0,   0,   0,   0/
      DATA ((TAB3(I,J),J=2,2),I=701,800)/
     &  25,  25,   0,   0,   0,   0,   0,   0,   0,   0,
     &  25,  27,   0,   0,   0,   0,   0,   0,   0,   0,
     &  33,  33,   0,   0,   0,   0,   0,   0,   0,  36,
     &  36,  36,   0,   0,   0,   0,   0,   0,   0,  36,
     &  32,  36,  32,  36,  36,  32,  36,  36,  36,   0,
     &   0,  36,   0,   0,  10,  10,  10,  10,   0,  25,
     &  25,  33,  25,  25,  25,  25,   0,  25,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  10/
      DATA ((TAB3(I,J),J=2,2),I=801,900)/
     &  29,  29,  10,  29,  29,  29,  29,  29,  29,  10,
     &  10,  29,  29,  10,  29,  29,  29,  29,  29,  29,
     &  25,  29,  10,  29,  29,  10,  29,  29,  10,  29,
     &  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,
     &  29,  29,  10,   0,   0,   0,   0,   0,   0,  10,
     &  25,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  29/
      DATA ((TAB3(I,J),J=2,2),I=901,1000)/
     &  29,  10,   0,   0,   0,   0,   0,   0,   0,   0,
     &  29,   0,   0,   0,   0,   0,   0,   0,  25,  25,
     &  25,  25,  25,  25,   0,   0,  25,   0,  25,   0,
     &  29,   0,   0,   0,  25,  25,  25,  25,  25,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  35,
     &  35,  35,  35,   0,   0,   0,   0,   0,   0,   0,
     &   0,   0,   0,   0,   0,   0,   0,   0,   0,  36,
     &  36,  36,  36,  36,  36,  36,  36,   0,   0,   0,
     &  29,   0,   0,   0,   0,   0,   0,   0,  25,  10,
     &  25,   1,  33,  25,  25,  25,  25,  25,  25,   0/
C----------
C REDEFINE TAB3 STOCK EQ. COEF. FOR WESTERN VARIANTS
C----------
      SELECT CASE (VARACD)
        CASE ('CS','LS','NE','SN','ON')
C
        CASE DEFAULT
         TAB3(298,2)=08                         ! WEST OTHER SOFTWOOD
         TAB3(998,2)=26                         ! WEST OTHER HARDWOOD
         TAB3(999,2)=26                         ! WEST OTHER UNKNOWN
      END SELECT
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'STKVAL',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC, ITRN
    3 FORMAT(' ENTERING SUBROUTINE STKVAL  CYCLE =',I5,' ITRN= ',I5)
      DMAX=0.
C----------
C  INITIALIZE VARIABLES
C----------
      DO 5 I= 1,MAXSP
         SS(I)= 0.0
    5 CONTINUE
      TOTSTK=0.0
      TTST51=0.0
      TTST52=0.0
      TOTST5=0.0
      TOTST3=0.0
      TOTST1=0.0
      DO I=1,3
         SZCL(I)=0.
      END DO
      ISZCL=0
      ISTCL=0
      IF (ITRN.LE.0) RETURN
C
C --- FIND DBH OF LARGEST TREE IN THE STAND
C      IF (ICYC.LE.1) THEN
          DMXSTD=0.0
          DMXSS=0.0
          DO 8 ISPC=1,MAXSP
             I1=ISCT(ISPC,1)
             IF (I1.GT.0) THEN
                I2=ISCT(ISPC,2)
                DO 7 I3=I1,I2
                   I=IND1(I3)
                   IF((IMC(I).GE.6).AND.(IMC(I).LE.9))GO TO 7
                   IF (DBH(I).GT.DMXSTD) DMXSTD= DBH(I)
                   IF (DBH(I).LT.5.0.AND.DBH(I).GT.DMXSS) DMXSS=DBH(I)
    7           CONTINUE
             END IF
    8     CONTINUE
C --- STEP 1: ASSIGN INITIAL STOCKING VALUE FOR EACH TREE
      DO 21 ISPC=1,MAXSP                                                        !SPECIES LOOP
         I1=ISCT(ISPC,1)
         IF (I1.GT.0) THEN
            I2=ISCT(ISPC,2)
            IF(FIAJSP(ISPC).EQ.'   ')THEN
              IFIA=998
            ELSE
              READ (FIAJSP(ISPC),'(I4)') IFIA                                   !FIA SPECIES CODE
            ENDIF
            IF (IFIA.EQ.0) IFIA=999
            B0=TAB2(TAB3(IFIA,2),1)                                             !STOCKING EQ. b0 COEFFICIENT
            B1=TAB2(TAB3(IFIA,2),2)                                             !STOCKING EQ. b1 COEFFICIENT
            IF (B0.EQ.0.OR.B1.EQ.0) THEN
               B0=TAB2(TAB3(999,2),1)
               B1=TAB2(TAB3(999,2),2)
            END IF
            DO 11 I3=I1,I2                                                      !TREE LOOP
               I=IND1(I3)
               IF((IMC(I).GE.6).AND.(IMC(I).LE.9))GO TO 11
               D=DBH(I)
               IF (D.GT.0.0) THEN
                  Q=1.0                                                         !STOCKABILITY PROPORTION FROM DESIGN KEYWORD, PARAMETER FIELD 7
                  STKTR1=(B0*D**B1)*PROB(I)/Q                                   !STEP 1 STOCKING VALUE
               END IF
               IF (D.GE.5.0) TTST51=TTST51+STKTR1                               !STEP 1 STOCKING TREES >= 5.0"
   11       CONTINUE
         END IF
   21 CONTINUE
      IF (DEBUG)WRITE(JOSTND,*)' TTST51,STKTR1,PROB(1)= ',
     &TTST51,STKTR1,PROB(1)
C
C --- STEP 2: ADJUST INITIAL STOCKING TO REFLECT COMPETITIVE POSITION
      DO 22 ISPC=1,MAXSP                                                        !SPECIES LOOP
         I1=ISCT(ISPC,1)
         IF (I1.GT.0) THEN
            I2=ISCT(ISPC,2)
            IF(FIAJSP(ISPC).EQ.'   ')THEN
              IFIA=998
            ELSE
              READ(FIAJSP(ISPC),'(I4)')IFIA
            ENDIF
            IF (IFIA.EQ.0) IFIA=999
            B0=TAB2(TAB3(IFIA,2),1)
            B1=TAB2(TAB3(IFIA,2),2)
            IF (B0.EQ.0.OR.B1.EQ.0) THEN
               B0=TAB2(TAB3(999,2),1)
               B1=TAB2(TAB3(999,2),2)
            END IF
            DO 12 I3=I1,I2                                                      !TREE LOOP
               I=IND1(I3)
               IF((IMC(I).GE.6).AND.(IMC(I).LE.9))GO TO 12
               D=DBH(I)
               IF (D.GT.0.0) THEN
                  IF (D.GE.5.0) THEN
                      CF=1.0                                                    !TABLE 4 (ARNER 2/13/2001) CROWN POSITION ADJUSTMENT FACTORS (CF)
                  ELSE                                                          !         CROWN CLASS                 CF FOR TREES >= 5.0"
                     IF (TTST51.GE.10) THEN                                     !  -- OPEN GROWN, DOMINANT, CODOM            1.0
                        DMAX=5.0                                                !  -- INTERMEDIATE                           0.5
                     ELSE                                                       !  -- OVERTOPPED/SUPPRESSED                  0.1
                        DMAX= DMXSS                                             !
                     END IF                                                     ! DEFAULT CF VALUE SET TO 1.0 FOR TREES WITHOUT CROWN CLASS CODED
                     CF=D/DMAX
                  END IF
                  Q=1.0
                  STKTR2=((B0*D**B1)*PROB(I)/Q)*CF                              !STEP 2 STOCKING VALUE
               END IF
               IF (D.GE.5.0) TTST52=TTST52+STKTR2                               !STEP 2 STOCKING TREES >= 5.0"
               IF (D.GE.5.0) TOTST5=TOTST5+STKTR2                               !ACCUMMULATE STOCKING TREES >= 5.0"
               IF (D.GT.0.1.AND.D.LT.5.0) TOTST3=TOTST3+STKTR2                  !ACCUMMULATE STOCKING SAPLINGS
               IF (D.EQ.0.1) TOTST1=TOTST1+STKTR2                               !ACCUMMULATE STOCKING SEEDLINGS
               TOTSTK=TOTSTK+STKTR2                                             !ACCUMMULATE TOTAL STOCKING
               SS(ISPC)=SS(ISPC)+STKTR2                                         !SUM TREE STOCKING PER SPECIES
   12       CONTINUE
         END IF
   22 CONTINUE
      IF (DEBUG) WRITE(JOSTND,*)' IN STKVAL_1: DMXSS,DMAX,CF,TTST51,',
     &'TOTST5,TOTSTK - ',DMXSS,DMAX,CF,TTST51,TOTST5,TOTSTK
C
C --- STEP 3 & 4: DETERMINE 'FUTURE STAND' OR STANDARD VALUES FOR SEEDS/SAPS
      IF (TTST52.LT.20.0) THEN                                                  !"FUTURE STAND" VALUES FOR SEEDS/SAPS
         DO 25 I= 1,MAXSP
            SS(I)= 0.0
   25    CONTINUE
         TOTSTK=0.0
         TOTST5=0.0
         TOTST3=0.0
         TOTST1=0.0
         DO 40 ISPC=1,MAXSP                                                     !SPECIES LOOP
            I1=ISCT(ISPC,1)
            IF (I1.GT.0) THEN
               I2=ISCT(ISPC,2)
               IF(FIAJSP(ISPC).EQ.'   ')THEN
                 IFIA=998
               ELSE
                 READ(FIAJSP(ISPC),'(I4)')IFIA
               ENDIF
               IF (IFIA.EQ.0) IFIA=999
               B0=TAB2(TAB3(IFIA,2),1)
               B1=TAB2(TAB3(IFIA,2),2)
               IF (B0.EQ.0.OR.B1.EQ.0) THEN
                  B0=TAB2(TAB3(999,2),1)
                  B1=TAB2(TAB3(999,2),2)
               END IF
               DO 30 I3=I1,I2                                                   !TREE LOOP
                  I=IND1(I3)
                  IF((IMC(I).GE.6).AND.(IMC(I).LE.9))GO TO 30
                  D=DBH(I)
                  IF (D.GT.0.0) THEN
                     IF (D.GE.5.0) THEN
                        CF=1.0
                     ELSE
                        IF (TTST51.GE.10) THEN
                           DMAX=5.0
                        ELSE
                           DMAX= DMXSS
                        END IF
                        CF=D/DMAX
                     END IF
                     IF (D.LT.5.0) D=5.0
                     Q=1.0
                     STKTR2=((B0*D**B1)*PROB(I)/Q)*CF
                  END IF
                  D=DBH(I)
                  IF (D.GE.5.0) TOTST5=TOTST5+STKTR2                            !ACCUMMULATE STOCKING TREES >= 5.0"
                  IF (D.GT.0.1.AND.D.LT.5.0) TOTST3=TOTST3+STKTR2               !ACCUMMULATE STOCKING SAPLINGS
                  IF (D.EQ.0.1) TOTST1=TOTST1+STKTR2                            !ACCUMMULATE STOCKING SEEDLINGS
                  TOTSTK=TOTSTK+STKTR2                                          !ACCUMMULATE TOTAL STOCKING
                  SS(ISPC)=SS(ISPC)+STKTR2                                      !SUM TREE STOCKING PER SPECIES
   30          CONTINUE
            END IF
   40    CONTINUE
      END IF
      IF (DEBUG) WRITE(JOSTND,*)' IN STKVAL_2: DMXSS,DMAX,CF,TTST51,',
     &'TOTST5,TOTSTK - ',DMXSS,DMAX,CF,TTST51,TOTST5,TOTSTK
C
C --- STEP 5: ASSURE SEEDS/SAPS DO NOT REDUCE STOCK VALUES OF LARGER TREES
      UG=120
      GI=TOTST5
      AI=TOTST3
      EI=TOTST1
      UA=MAX(UG-GI,0.0)
      UE=MAX(UA-AI,0.0)
      PRG=1.0
      PRA=1.0
      PRE=1.0
      IF (UG.GT.0.AND.GI.GT.0) PRG=MIN(UG/GI,1.0)
      IF (UA.GT.0.AND.AI.GT.0) PRA=MIN(UA/AI,1.0)
      IF (UE.GT.0.AND.EI.GT.0) PRE=MIN(UE/EI,1.0)
      DO I= 1,MAXSP
         SS(I)= 0.0
      END DO
      TOTSTK=0.0
      TOTST5=0.0
      TOTST3=0.0
      TOTST1=0.0
      DO ISPC=1,MAXSP                                                           !SPECIES LOOP
         I1=ISCT(ISPC,1)
         IF (I1.GT.0) THEN
            I2=ISCT(ISPC,2)
            IF(FIAJSP(ISPC).EQ.'   ')THEN
              IFIA=998
            ELSE
              READ(FIAJSP(ISPC),'(I4)')IFIA
            ENDIF
            IF (IFIA.EQ.0) IFIA=999
            B0=TAB2(TAB3(IFIA,2),1)
            B1=TAB2(TAB3(IFIA,2),2)
            IF (B0.EQ.0.OR.B1.EQ.0) THEN
               B0=TAB2(TAB3(999,2),1)
               B1=TAB2(TAB3(999,2),2)
            END IF
            DO I3=I1,I2                                                         !TREE LOOP
               I=IND1(I3)
               IF((IMC(I).GE.6).AND.(IMC(I).LE.9))GO TO 80
               D=DBH(I)
               IF (D.GT.0.0) THEN
                  IF (D.GE.5.0) THEN
                     CF=1.0
                  ELSE
                     IF (TTST51.GE.10) THEN
                        DMAX=5.0
                     ELSE
                        DMAX= DMXSS
                     END IF
                     CF=D/DMAX
                  END IF
                  IF (TTST52.LT.20.0.AND.D.LT.5.0) D=5.0
                  Q=1.0
                  STKTR2=((B0*D**B1)*PROB(I)/Q)*CF
               END IF
               D=DBH(I)
               IF (D.GE.5.0) TOTST5=TOTST5+STKTR2                               !ACCUMMULATE STOCKING TREES >= 5.0"
               IF (D.GT.0.1.AND.D.LT.5.0) TOTST3=TOTST3+STKTR2                  !ACCUMMULATE STOCKING SAPLINGS
               IF (D.EQ.0.1) TOTST1=TOTST1+STKTR2                               !ACCUMMULATE STOCKING SEEDLINGS
               TOTSTK=TOTSTK+STKTR2                                             !ACCUMMULATE TOTAL STOCKING
               SS(ISPC)=SS(ISPC)+STKTR2                                         !SUM TREE STOCKING PER SPECIES
               IF (D.LT.5) THEN                                                 !ACCUMMULATE STOCKING BY SIZE CLASS
                  SZCL(1)=SZCL(1)+STKTR2                                        !SMALL
               ELSE IF (IFIA.LT.300.AND.D.GE.5.AND.D.LT.9) THEN
                  SZCL(2)=SZCL(2)+STKTR2                                        !MEDIUM
               ELSE IF (IFIA.GE.300.AND.D.GE.5.AND.D.LT.11) THEN
                  SZCL(2)=SZCL(2)+STKTR2
               ELSE IF (IFIA.LT.300.AND.D.GE.9) THEN
                  SZCL(3)=SZCL(3)+STKTR2                                        !LARGE
               ELSE IF (IFIA.GE.300.AND.D.GE.11) THEN
                  SZCL(3)=SZCL(3)+STKTR2
               END IF
   80       CONTINUE
            END DO
         END IF
      END DO
      IF (DEBUG) WRITE(JOSTND,*)' IN STKVAL_3: DMXSS,DMAX,TTST51,TTST',
     &'52,TOTST5,TOTSTK - ',DMXSS,DMAX,TTST51,TTST52,TOTST5,TOTSTK
C
C --- DETERMINE STAND SIZE CLASS
      ISZCL=0
      IF (TOTSTK.LT.10) THEN
         ISZCL=5                                                        !NON-STOCKED
      ELSE IF (SZCL(1).GT.(TOTSTK*0.50)) THEN
         ISZCL=3                                                        !SEEDLING-SAPLING
      ELSE IF (SZCL(2).GT.SZCL(3)) THEN
         ISZCL=2                                                        !POLETIMBER
      ELSE IF (SZCL(3).GE.SZCL(2)) THEN
         ISZCL=1                                                        !SAWTIMBER
      END IF
C
C --- DETERMINE STOCKING CLASS
      ISTCL=0
      IF (TOTSTK.GT.100) THEN
         ISTCL=1                                                        !OVERSTOCKED
      ELSE IF (TOTSTK.GE.60.AND.TOTSTK.LT.100) THEN
         ISTCL=2                                                        !FULLY STOCKED
      ELSE IF (TOTSTK.GE.35.AND.TOTSTK.LT.60) THEN
         ISTCL=3                                                        !MODERATELY STOCKED
      ELSE IF (TOTSTK.GE.10.AND.TOTSTK.LT.35) THEN
         ISTCL=4                                                        !POORLY STOCKED
      ELSE IF (TOTSTK.GE.00.AND.TOTSTK.LT.10) THEN
         ISTCL=5                                                        !NON-STOCKED
      END IF
C
C --- LOAD INITIAL FOREST TYPE (S ARRAY) BY SPECIES (SS ARRAY)
C     FIRST LOOP THROUGH THE ITG GROUPS
C     THEN LOOP THROUGH THE SPECIES LIST
C     READ THE FIA NUMBER TO LOCATE SPECIES IN TAB3 ARRAY
C     IF SPECIES IS IN ITG GROUP THEN ADD INTO STOCKING FOR THE ITG GROUP
      DO IS=1,210
         DO ISPC=1,MAXSP
            IF(FIAJSP(ISPC) .EQ. '   ')GO TO 99
            READ (FIAJSP(ISPC),'(I4)') IFIA
            IF (TAB3(IFIA,1).EQ.IS) THEN
              S(IS)=S(IS)+SS(ISPC)
              IF(DEBUG) THEN
                 WRITE(JOSTND,*)' IN STKVAL_LOAD, IS,S(IS)= ',IS,S(IS)
              END IF
            END IF
   99       CONTINUE
         END DO
      END DO
C
C --- DEBUG
      IF(DEBUG) THEN
         WRITE(JOSTND,*)' IN STKVAL, TOTST5= ',TOTST5
         WRITE(JOSTND,*)' ISZCL,ISTCL= ',ISZCL,ISTCL
      END IF
C
      RETURN
      END
