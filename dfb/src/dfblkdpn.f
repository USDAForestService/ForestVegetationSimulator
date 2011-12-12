      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C  **DFBLKD--PN   DATE OF LAST REVISION:  06/30/10
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
C.... This IFVSSP is for the variant PN

      DATA IFVSSP / 16, 13,  4,  9, 15,  8, 39, 34, 14,  8,  7,
     &              31, 12,  1, 10,  3, 35,  6,  5, 11, 18, 18,
     &              18, 18, 32, 19, 24, 29, 26, 36, 22, 37, 38,
     &              18, 30, 18, 30, 30, 30 /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... This value is 16 for the PN variant.

      DATA IDFSPC / 16 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
