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
      DATA ICCODE /0/, IRECNT /0/, YR /1.0/
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /

C----------
C  DATA STATEMENTS FOR COMMON COEFFS VARIABLES
C----------
      DATA  BKRAT /MAXSP*0./, COR2 /MAXSP*1./
C----------
C  DATA STATEMENTS FOR COMMON HTCAL VARIABLES
C----------
      DATA  HCOR2 /MAXSP*1./

C----------
C  DATA STATEMENTS FOR COMMON ESCOMN VARIABLES
C----------
      DATA XMIN/ 1.00,0.50,0.50,0.50,0.50,0.50,0.50,0.50,1.00,0.50,
     &0.50,0.50,0.50,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     &1.00 /
      DATA HHTMAX/ 37.0,37.0,24.0,29.0,31.1,31.1,29.0,37.0,15.5,
     &31.6,32.6,21.0,31.1,39.9,39.9,44.9,44.9,32.5,39.2,32.5,
     &32.5,32.5,32.5 /
      DATA DBHMID /1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0 /,
     &  BNORML/ 3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789 /

C  REGENERATION MODELING NEEDS
C    SPROUTING SPECIES INDEX LIST
      DATA ISPSPE/ 14,15,16,17,18,19,20,21,22 /

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
C  RESIDUAL ERROR ESTIMATES OF GROWTH MEASUREMENTS USED FOR CALIBRATION.
C  CALCULATED VALUES WERE MULTIPLIED BY .75 TO ACCOUNT FOR FIELD
C  MEASUREMENT ERROR. MARK CASTLE. 6/5/2019
C
      DATA SIGMAR/
     & 0.580500, 0.580500, 0.529200, 0.620025, 0.701025,
     & 0.701025, 0.620025, 0.580500, 0.459675, 0.507975,
     & 0.532050, 0.518100, 0.701025, 0.580875, 0.580875,
     & 0.738750, 0.738750, 0.623025, 0.636150, 0.623025,
     & 0.623025, 0.623025, 0.623025 /


C----------
C  DATA STATEMENTS FOR COMMON VARCOM VARIABLES
C----------
C  HTT* and HTT** USED IN THE HEIGHT DUBBING FUNCTIONS.
      DATA HTT1/
     & 4.96881, 4.96881, 4.66955, 4.23828, 4.47743, 4.47743, 
     & 4.27835, 4.86600, 4.44360, 4.65302, 4.82632, 4.64691,  
     & 4.47743, 4.40043, 4.40043, 4.44538, 4.44538, 4.61208, 
     & 4.42399, 4.34112, 3.76180, 3.76180, 4.34112/
      DATA HTT2/
     & -12.0542, -12.0542, -10.1234, -5.55522, -5.84601, 
     & -5.84601, -5.51495, -9.86689, -10.0751, -10.0359,
     & -9.61067, -11.2245, -5.84601, -4.79211, -4.79211,
     & -4.32227, -4.32227, -6.14911, -4.74151, -4.66284,
     & -4.33181, -4.33181, -4.66284/
      DATA HTT11/
     & 121.157, 121.157, 103.575, 52.0762, 75.2329, 75.2329,
     & 40.0453, 135.913, 51.9941, 128.642, 161.211, 94.6211,
     & 75.2329, 90.7702, 90.7702, 52.9016, 52.9016, 133.016, 
     & 58.8630, 73.0724, 133.942, 133.942, 73.0724/
      DATA HTT12/23*0.04478/
      DATA HTT13/23*0.04939/
      DATA HTT14/
     & -0.05554, -0.05554, -0.04441, -0.17123, -0.11976,
     & -0.11976, -0.25383, -0.04202, -0.11319, -0.02585,
     & -0.03169, -0.05281, -0.11976, -0.05444, -0.05444,
     & -0.27032, -0.27032, -0.02895, -0.17808, -0.09317, 
     & -0.01359, -0.01359, -0.09317/
      DATA HTT15/
     & 1.32667, 1.32667, 1.11714, 1.33784, 1.23548, 1.23548,
     & 1.52228, 1.04975, 1.56880, 0.93397, 1.03309, 1.29239, 
     & 1.23548, 0.78470, 0.78470, 1.29203, 1.29203, 0.70069,
     & 1.14308, 0.93144, 0.77442, 0.77442, 0.93144/
      DATA NOPERMH1/
     & -5.98120, -5.98120, -6.03758, -3.11433, -3.00119,
     & -3.00119, -3.11433, -5.98120, -5.59148, -5.81345,
     & -7.14687, -5.92287, -3.00119, -1.41241, -1.41241, 
     & -2.64650, -2.64650, -3.18997, -2.92460, -3.18897,
     & -3.18897, -3.18897, -3.18897/
      DATA NOPERMH2/
     & 3*-0.000056,4*-0.000153,5*-0.000056,11*-0.000153/
      DATA NOPERMH3/
     & 0.1844200, 0.1844200, 0.0136470, 0.3147670, 0.4725020,
     & 0.4725020, 0.3147670, 0.1844200, -0.072202, 0.0080040, 
     & 0.3655290, 0.0511940, 0.4725020, 0.2170190, 0.2170190,
     & 0.4797390, 0.4797390, 0.4233280, 0.436627, 4*0.423328/
      DATA NOPERMH4/
     & -0.001420, -0.001420, -0.000219, -0.000735, -0.002113,
     & -0.002113, -0.000735, -0.001420, -0.000610, -0.000653,
     & -0.000126, -0.000698, -0.002113, -0.000475, -0.000475,
     & -0.004078, -0.004078, 6*-0.000735/
      DATA NOPERMH5/
     & 0.0524150, 0.0524150, 0.1886610, 0.2998390, 0.3146590, 
     $ 0.3146590, 0.2998390, 0.0524150, 0.2420080, 0.1859510,
     & -0.052962, 0.1554370, 0.3146590, 0.2089250, 0.2089250,
     & 0.6042530, 0.6042530, 0.0552760, 0.3215640, 4*0.0552760/
      DATA NOPERMH6/3*0.398322,4*0.376771,5*0.398322,11*0.376771/
      DATA NOPERMH7/3*0,4*-0.000152,5*0,11*-0.000152/
      DATA NOPERMH8/3*0,4*-0.498307,5*0,11*-0.498307/ 
      DATA NOPERMH9/3*0.830338,4*0,5*0.830338,11*0/     
      DATA PERMH1/
     & 3*0,-3.16862,-2.98897,-2.98897,-3.16862,5*0,-2.98897, 
     & 2*0,-2.74088,-2.74088,-1.86695,-3.03278,4*-1.86695/
      DATA PERMH2/3*0,4*-0.2774,5*0,-0.2774,2*0,8*-0.2774/ 
      DATA PERMH3/3*0,4*-0.00015,5*0,-0.00015,2*0,8*-0.00015/
      DATA PERMH4/
     & 3*0,0.314314,0.464157,0.464157,0.314134,5*0,0.464157,
     & 2*0,0.482874,0.482874,0.054669,0.410380,4*0.054669/ 
      DATA PERMH5/3*0,-0.00118,2*-0.0025,-0.00118,5*0,-0.0025,
     & 2*0,2*-0.00413,6*-0.00118/
      DATA PERMH6/3*0,0.215632,2*0.257847,0.215632,5*0,0.257847,2*0,
     & 2*0.545314,0.062189,0.288289,4*0.062189/
      DATA PERMH7/3*0,4*0.369803,5*0,0.369803,2*0,8*0.369803/
      DATA PERMH8/3*0,4*-0.00013,5*0,-0.00013,2*0,8*-0.00013/
      DATA PERMH9/3*0,4*-0.39101,5*0,-0.39101,2*0,8*-0.39101/  
     
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
