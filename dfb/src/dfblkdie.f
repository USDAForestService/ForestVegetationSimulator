      BLOCK DATA DFBLKD
      IMPLICIT NONE
C----------
C  **DFBLKD--IE   DATE OF LAST REVISION:  06/30/10
C----------
C
C     DOUGLAS-FIR BEETLE MODEL BLOCK DATA FOR
C     THE INLAND EMPIRE (IE) 23 SPECIES FVS VARIANT.
C
C     Created by Lance David 13-AUG-2003
C
C------------------------------------
C
COMMONS

      INCLUDE 'PRGPRM.F77'


      INCLUDE 'DFBCOM.F77'


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

C.... THE ARRAY IFVSSP IS USED TO INDEX THE SPECIES DEPENDENT ARRAY
C.... WINSUC. IFVSSP CAN BE MODIFIED FOR DIFFERENT VARIANTS OF FVS SO
C.... THAT SPECIES MATCH BETWEEN FVS AND THE DOUGLAS-FIR BEETLE MODEL.
C....
C.... This IFVSSP is for the variant IE 23 species.
C.... IE variant species Mountain maple (20) and Paper birch (21)
C.... do not have matching species in the DFB model and are mapped
C.... to DFB Other Species (30).
C....
C....               1   2   3   4   5   6   7   8   9   10  11  12
C....               WP  WL  DF  GF  WH  RC  LP  ES  AF  PP  MH  WB
C....
C....               13  14  15  16  17  18  19  20  21  22  23
C....               LM  LL  PI  JU  PY  AS  CO  MM  PB  OH  OS
C....
      DATA IFVSSP /  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 22,
     &              23, 36, 33, 26, 38, 19, 24, 30, 30, 18, 17 /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

      DATA IDFSPC / 3 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
