      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C  **DFBLKD--TT   DATE OF LAST REVISION:  06/30/10
C----------
C
C     DOUGLAS-FIR BEETLE MODEL BLOCK DATA
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DFBCOM.F77'

C....
C.... WINSUC IS THE SPECIES DEPENDENT VALUES FOR WINDTHROW
C.... SUSCEPTIBILITY.
C....
C....    DFB              DFB              DFB
C....  IDX SP  WINSUC   IDX SP  WINSUC   IDX SP  WINSUC 
C....   01 WP   0.028    14 IC   0.111    27 OC   0.028 
C....   02 WL   0.083    15 RF   0.056    28 GS   0.056 
C....   03 DF   0.056    16 SF   0.098    29 BO   0.0   
C....   04 GF   0.139    17 OS   0.028    30 OTH  0.042 
C....   05 WH   0.111    18 OH   0.056    31 JP   0.056 
C....   06 RC   0.111    19 AS   0.056    32 TO   0.0   
C....   07 LP   0.028    20 BS   0.139    33 PI   0.0   
C....   08 ES   0.139    21 CB   0.139    34 YC   0.111 
C....   09 AF   0.139    22 WB   0.0      35 RW   0.056 
C....   10 PP   0.056    23 LM   0.0      36 LL   0.056 
C....   11 MH   0.111    24 CW   0.056    37 KP   0.0   
C....   12 SP   0.042    25 WS   0.139    38 PY   0.0   
C....   13 WF   0.139    26 JU   0.0      39 NF   0.139 
C....

      DATA WINSUC / 0.028, 0.083, 0.056, 0.139, 0.111,
     &              0.111, 0.028, 0.139, 0.139, 0.056,
     &              0.111, 0.042, 0.139, 0.111, 0.056,
     &              0.098, 0.028, 0.056, 0.056, 0.139,
     &              0.139,   0.0,   0.0, 0.056, 0.139,
     &                0.0, 0.028, 0.056,   0.0, 0.042,
     &              0.056,   0.0,   0.0, 0.111, 0.056,
     &              0.056,   0.0,   0.0, 0.139  /

C....
C.... THE ARRAY IFVSSP IS USED TO INDEX THE SPECIES DEPENDENT ARRAY
C.... WINSUC. IFVSSP CAN BE MODIFIED FOR DIFFERENT VARIANTS OF FVS SO
C.... THAT SPECIES MATCH BETWEEN FVS AND THE DOUGLAS-FIR BEETLE MODEL.
C.... UPDATED fSPECIES 16-APR-10 LANCE DAVID (FMSC)
C....
C.... This IFVSSP is for the variant UT 24 species
C....              1   2   3   4   5   6   7   8   9   10  -- FVS index
C....              WB  LM  DF  PM  BS  AS  LP  ES  AF  PP  -- FVS species
      DATA IFVSSP /22, 23,  3, 33, 20, 19,  7,  8,  9, 10, 
C....              11  12  13  14  15  16  17  18          -- FVS index
C....              UJ  RM  BI  MM  NC  MC  OS  OH          -- FVS species
     &             26, 26, 18, 18, 24, 30, 17, 18 /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

      DATA IDFSPC / 3 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
