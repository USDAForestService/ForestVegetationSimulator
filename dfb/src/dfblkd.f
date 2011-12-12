      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C  **DFBLKD--BASE   DATE OF LAST REVISION:  06/30/10
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
C....        WP(01), WL(02), DF(03), GF(04), WH(05),
C....         C(06), LP(07),  S(08), AF(09), PP(10),
C....        MH(11), SP(12), WF(13), IC(14), RF(15),
C....        SF(16), OS(17), OH(18), AS(19), BS(20),
C....        CB(21), WB(22), LM(23), CW(24), WS(25),
C....         J(26), OC(27), GS(28), BO(29), OTH(30),
C....        JP(31), TO(32),  P(33), YC(34), RW(35),
C....        LL(36), KP(37), PY(38), NF(39)

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
C.... This IFVSSP is for the variants NI, CI, and KT

      DATA IFVSSP / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30 /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

      DATA IDFSPC / 3 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
