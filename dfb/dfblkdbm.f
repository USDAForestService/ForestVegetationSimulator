      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C DFB $Id$
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
