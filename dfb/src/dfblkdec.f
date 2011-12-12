      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C  **DFBLKD--EC   DATE OF LAST REVISION:  06/30/10
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
C....        WP, WL, DF, GF, WH,
C....         C, LP,  S, AF, PP,
C....        MH, SP, WF, IC, RF,
C....        SF, OS, OH, AS, BS,
C....        CB, WB, LM, CW, WS,
C....         J, OC, GS, BO, OTH,
C....        JP, TO,  P, YC, RW,
C....        LL, KP, PY, NF

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
C.... This IFVSSP is for the variant EC

      DATA IFVSSP / 1, 2, 3, 16, 6, 4, 7, 8, 9, 10, 30 /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

      DATA IDFSPC / 3 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
