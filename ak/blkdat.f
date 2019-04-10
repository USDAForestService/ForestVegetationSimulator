      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C AK $Id$
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
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ECON.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'FVSSTDCM.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'KEYCOM.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'RANCOM.F77'
C
C
      INCLUDE 'SCREEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C
C----------
C SPECIES LIST FOR ALASKA VARIANT.
C
C Number Code  Common Name         FIA  PLANTS Scientific Name
C   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4     TA   tamarack            071  LALA   Larix laricina
C   5     WS   white spruce        094  PIGL   Picea glauca
C   6     LS   Lutz’s spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TE
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TD
C
C----------
C  VARIABLE DECLARATIONS:
C----------
      INTEGER I,J
C----------
C  DATA STATEMENTS FOR COMMON CONCHR VARIABLES
C----------
      DATA TREFMT /
     &'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     &2I1,F3.0)' /

C----------
C  DATA STATEMENTS FOR COMMON CONTRL VARIABLES
C----------
      DATA RCOR2 /MAXSP*1.0/
      DATA ICCODE /0/, IRECNT /0/, YR /10.0/
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /

C----------
C  DATA STATEMENTS FOR COMMON COEFFS VARIABLES
C----------
      DATA  BKRAT /MAXSP*0./, COR2 /MAXSP*1./
C
----------
C  DATA STATEMENTS FOR COMMON HTCAL VARIABLES
C----------
      DATA  HCOR2 /MAXSP*1./

C----------
C  DATA STATEMENTS FOR COMMON ESCOMN VARIABLES
C----------
      DATA XMIN/ 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0,
     &          1.0, 1.0, 0.5, 10*0.0 /
      DATA HHTMAX/ 27.0,31.0,2*25.0,26.0,24.0,28.0,
     &             2*20.0,50.0, 20.0,18.0,26.0, 10*0.0 /
      DATA DBHMID /1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0 /,
     &  BNORML/ 3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789 /

C  REGENERATION MODELING NEEDS
C    SPROUTING SPECIES INDEX LIST
      DATA ISPSPE/ 14,15,16,17,18,19,20,21,22,23 /

C    OLD MAPPING IS COMMENTED OUT, EVENTUALLY DELETE
C      DATA IFORCD/ 1003, 1002, 1005, 701, 1004, 15*0 /
C      DATA IFORST/    1,    2,    3,   4,    1, 15*0 /
      DATA IFORCD/1004,1005,1002,1003,703,713,720,7400,
     & 7401,7402,7403,7404,7405,7406,7407,7408,8134,8135,8112,0/
      DATA IFORST/   1,   3,   2,   1,  4,  1,  1,   1,
     &    1,   1,   1,   1,   1,   1,   1,   1,   2,   4,   4,0/

C    OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C    OCURHT DIMENSIONED (16,MAXSP)
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/ 368 * 1.0 /

C    OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C    OCURNF DIMENSIONED (20,MAXSP)
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/ 460 * 1.0 /

C----------
C  DATA STATEMENTS FOR COMMONS PLTCHR AND PLOT VARIABLES
C----------
      DATA JSP /
     & 'SF ', 'AF ', 'YC ', 'TA ', 'WS ', 'LS ', 'BE ', 'SS ',
     & 'LP ', 'RC ', 'WH ', 'MH ', 'OS ', 'AD ', 'RA ', 'PB ',
     & 'AB ', 'BA ', 'AS ', 'CW ', 'WI ', 'SU ', 'OH '/

      DATA FIAJSP /
     & '011', '019', '042', '071', '094', '   ', '095', '098',
     & '108', '242', '263', '264', '298', '350', '351', '375',
     & '376', '741', '746', '747', '920', '928', '998'/

      DATA PLNJSP /
     & 'ABAM  ', 'ABLA  ', 'CANO9 ', 'LALA  ', 'PIGL  ', 'PILU  ',
     &  'PIMA  ','PISI  ', 'PICO  ', 'THPL  ', 'TSHE  ', 'TSME  ',
     & '2TE   ', 'ALNUS ', 'ALRU2 ', 'BEPA  ', 'BENE4 ', 'POBA2 ',
     & 'POTR5 ', 'POBAT ', 'SALIX ', 'SASC  ', '2TD   '/

      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /

      DATA NSP /
     & 'SF1', 'AF1', 'YC1', 'TA1', 'WS1', 'LS1', 'BE1', 'SS1',
     & 'LP1', 'RC1', 'WH1', 'MH1', 'OS1', 'AD1', 'RA1', 'PB1',
     & 'AB1', 'BA1', 'AS1', 'CW1', 'WI1', 'SU1', 'OH1',
     & 'SF2', 'AF2', 'YC2', 'TA2', 'WS2', 'LS2', 'BE2', 'SS2',
     & 'LP2', 'RC2', 'WH2', 'MH2', 'OS2', 'AD2', 'RA2', 'PB2',
     & 'AB2', 'BA2', 'AS2', 'CW2', 'WI2', 'SU2', 'OH2',
     & 'SF3', 'AF3', 'YC3', 'TA3', 'WS3', 'LS3', 'BE3', 'SS3',
     & 'LP3', 'RC3', 'WH3', 'MH3', 'OS3', 'AD3', 'RA3', 'PB3',
     & 'AB3', 'BA3', 'AS3', 'CW3', 'WI3', 'SU3', 'OH3'/

C----------
C  DATA STATEMENTS FOR COMMON COEFFS VARIABLES
C----------
C  HT1 AND HT2 CONTAIN ALDER SPECIES, RED ALDER AND COTTONWOOD
C  HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH AND LARGER.
C
      DATA HT1/ 13*0.0,  4.875,  4.875, 4*0.0,   5.152, 3*0.0 /
      DATA HT2/ 13*0.0, -8.639, -8.639, 4*0.0, -13.576, 3*0.0 /
C
C  RESIDUAL ERROR ESTIMATES WERE MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR; 5/10/91--WRW.
C
      DATA SIGMAR/
     & 0.34599, 0.34599, 0.28778, 0.34599, 0.34599, 0.34599,
     & 0.34599, 0.34599, 0.28778, 0.28119, 0.33275, 0.33275,
     & 0.34599, 0.3328,  0.3328,  0.34599, 0.34599, 0.34599,
     & 0.34599, 0.5357,  0.34599, 0.34599, 0.28778/

C----------
C  DATA STATEMENTS FOR COMMON VARCOM VARIABLES
C----------
C  HTT1 AND HTT2, DIMENSIONED (MAXSP,9)
C  USED IN THE HEIGHT DUBBING FUNCTIONS.  IN THE CASE OF RED ALDER
C  AND COTTONWOOD, THESE ARE FOR TREES LESS THAN 5.0" DBH.
C
      DATA ((HTT1(I,J),I=1,MAXSP),J=1,4)/
     & 4.8945, 4.6060, 4.8945, 4.7407, 4.8933, 4.8028,
     & 4.6150, 4.8945, 4.8945, 0.0994, 0.0994, 4.8028, 4.8945,
     & 4.8945, 4.6060, 4.8945, 4.7407, 4.8933, 4.8028,
     & 4.6150, 4.8945, 4.8945, 4.9767, 4.9767, 4.8028, 4.8945,
     & 4.8945, 4.6060, 4.8945, 4.7407, 4.8933, 4.8028,
     & 4.6150, 4.8945, 4.8945, 0.0   , 0.0   , 4.8028, 4.8945,
     & 4.9727, 4.6060, 4.9727, 4.7666, 4.9013, 4.8028,
     & 4.6150, 4.9727, 4.9727, 0.0   , 0.0   , 4.8028, 4.9727/
      DATA ((HTT1(I,J),I=1,MAXSP),J=5,9)/
     & 4.9727, 4.6060, 4.9727, 4.7666, 4.9013, 4.8028,
     & 4.6150, 4.9727, 4.9727, 0.0   , 0.0   , 4.8028, 4.9727,
     & 4.9727, 4.6060, 4.9727, 4.7666, 4.9013, 4.8028,
     & 4.6150, 4.9727, 4.9727, 0.0   , 0.0   , 4.8028, 4.9727,
     & 4.8753, 4.6060, 4.8753, 4.7666, 4.8043, 4.8028,
     & 4.6150, 4.8753, 4.8753, 0.0   , 0.0   , 4.8028, 4.8753,
     & 4.8753, 4.6060, 4.8753, 4.7666, 4.8043, 4.8028,
     & 4.6150, 4.8753, 4.8753, 0.0   , 0.0   , 4.8028, 4.8753,
     & 4.8753, 4.6060, 4.8753, 4.7666, 4.8043, 4.8028,
     & 4.6150, 4.8753, 4.8753, 0.0   , 0.0   , 4.8028, 4.8753/
      DATA ((HTT2(I,J),I=1,MAXSP),J=1,4)/
     &  -6.8703,  -7.6276,  -6.8703, -12.0745,  -8.7008, -11.2306,
     & -10.4718,  -6.8703,  -6.8703,   0.0   ,   0.0   , -11.2306,
     &  -6.8703,
     &  -6.8703,  -7.6276,  -6.8703, -12.0745,  -8.7008, -11.2306,
     & -10.4718,  -6.8703,  -6.8703,   0.0   ,   0.0   , -11.2306,
     &  -6.8703,
     &  -6.8703,  -7.6276,  -6.8703, -12.0745,  -8.7008, -11.2306,
     & -10.4718,  -6.8703,  -6.8703,   0.0   ,   0.0   , -11.2306,
     &  -6.8703,
     &  -8.9697,  -7.6276,  -8.9697, -11.8502,  -8.9024, -11.2306,
     & -10.4718,  -8.9697,  -8.9697,   0.0   ,   0.0   , -11.2306,
     &  -8.9697/
      DATA ((HTT2(I,J),I=1,MAXSP),J=5,9)/
     &  -8.9697,  -7.6276,  -8.9697, -11.8502,  -8.9024, -11.2306,
     & -10.4718,  -8.9697,  -8.9697,   0.0   ,   0.0   , -11.2306,
     &  -8.9697,
     &  -8.9697,  -7.6276,  -8.9697, -11.8502,  -8.9024, -11.2306,
     & -10.4718,  -8.9697,  -8.9697,   0.0   ,   0.0   , -11.2306,
     &  -8.9697,
     & -10.4921,  -7.6276, -10.4921, -11.8502,  -9.4649, -11.2306,
     & -10.4718, -10.4921, -10.4921,   0.0   ,   0.0   , -11.2306,
     & -10.4921,
     & -10.4921,  -7.6276, -10.4921, -11.8502,  -9.4649, -11.2306,
     & -10.4718, -10.4921, -10.4921,   0.0   ,   0.0   , -11.2306,
     & -10.4921,
     & -10.4921,  -7.6276, -10.4921, -11.8502,  -9.4649, -11.2306,
     & -10.4718, -10.4921, -10.4921,   0.0   ,   0.0   , -11.2306,
     & -10.4921/

C----------
C  DATA STATEMENTS FOR COMMON PDEN VARIABLES
C----------
      DATA REGNBK/2.999/

C----------
C  DATA STATEMENTS FOR COMMON RANCOM VARIABLES
C----------
      DATA S0/55329D0/,SS/55329./

C----------
C  DATA STATEMENTS FOR COMMON SCREEN VARIABLES
C----------
      DATA LSCRN,JOSCRN/.FALSE.,6/

C----------
C  DATA STATEMENTS FOR COMMON ECON VARIABLES
C----------
      DATA JOSUME/13/

C----------
C  DATA STATEMENTS FOR COMMON FVSSTDCM VARIABLES
C----------
      DATA KOLIST,FSTOPEN /27,.FALSE./

      END
