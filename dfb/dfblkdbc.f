      BLOCK DATA DFBLKD
C----------
C  **DFBLKD--BC   DATE OF LAST REVISION:  08/13/03
C----------
C
C     DOUGLAS-FIR BEETLE MODEL BLOCK DATA FOR
C     THE BC (BC) 15 SPECIES FVS VARIANT.
C
C     Created by Don Robinson 20-Jan-2010 from IE variant
C     template
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

C.... This IFVSSP is for the BC variant 15 species.

C     1 = WESTERN WHITE PINE (WP)     [PW]
C     2 = WESTERN LARCH (WL)          [LW]
C     3 = DOUGLAS-FIR (DF)            [FD]
C     4 = GRAND FIR (GF)              [BG]
C     5 = WESTERN HEMLOCK (WH)        [HW]
C     6 = WESTERN REDCEDAR (RC)       [CW]
C     7 = LODGEPOLE PINE (LP)         [PL]
C     8 = ENGELMANN SPRUCE (ES)       [SE]
C     9 = SUBALPINE FIR (AF)          [BL]
C    10 = PONDEROSA PINE (PP)         [PY]
C    11 = BIRCH (PB)                  [EP] 19 in WINSUC
C    12 = ASPEN (AS)                  [AT] 19 in WINSUC
C    13 = COTTONWOOD (CW)             [AC] 24 in WINSUC
C    14 = OTHER CONIFER (OC)          [  ] ! FD
C    15 = OTHER HARDWOOD (OH)         [  ] ! EP

      DATA IFVSSP /  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     &              19, 19, 24,  3, 19  /

      DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

      DATA ROWDOM / MAXSP*80.0 /

C.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
C.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

      DATA IDFSPC / 3 /

C.... Set the variables for random number generator

      DATA S0 / 55329D0 /, SS / 55329.0 / 

      END
