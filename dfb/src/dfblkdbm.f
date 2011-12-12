      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C  **DFBLKD--BM   DATE OF LAST REVISION:  06/30/10
C----------
C
C     DOUGLAS-FIR BEETLE MODEL BLOCK DATA
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'DFBCOM.F77'
C
C
COMMONS
C

C.... WINSUC IS THE SPECIES DEPENDENT VALUES FOR WINDTHROW
C.... SUCEPTIBILITY.
C....
C....  1-5   WP, WL, DF, GF, WH,
C....  6-10   C, LP,  S, AF, PP,
C.... 11-15  MH, SP, WF, IC, RF,
C.... 16-20  SF, OS, OH, AS, BS,
C.... 21-25  CB, WB, LM, CW, WS,
C.... 26-30   J, OC, GS, BO, OTH,
C.... 31-35  JP, TO,  P, YC, RW,
C.... 36-39  LL, KP, PY, NF

      DATA WINSUC / 0.028, 0.083, 0.056, 0.139, 0.111,
     &              0.111, 0.028, 0.139, 0.139, 0.056,
     &              0.111, 0.042, 0.139, 0.111, 0.056,
     &              0.098, 0.028, 0.056, 0.056, 0.139,
     &              0.139,   0.0,   0.0, 0.056, 0.139,
     &                0.0, 0.028, 0.056,   0.0, 0.042,
     &              0.056,   0.0,   0.0, 0.111, 0.056,
     &              0.056,   0.0,   0.0, 0.139  /

C.... THE ARRAY IFVSSP IS USED TO INDEX THE SPECIES DEPENDENT
C.... ARRAY WINSUC. IFVSSP CAN BE MODIFIED FOR DIFFERENT VARIANTS
C.... OF FVS SO THAT SPECIES MATCH BETWEEN FVS AND THE DOUGLAS-FIR
C.... BEETLE MODEL.
C.... This IFVSSP is for the variant BM
C....              WP  WL  DF  GF  MH  WJ  LP  ES  AF
C....              PP  WB  LM  PY  YC  AS  CW  OS  OH

      DATA IFVSSP / 1,  2,  3,  4, 11, 26,  7,  8,  9, 
     &             10, 22, 23, 38, 34, 19, 24, 17, 18 /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

      DATA IDFSPC / 3 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
