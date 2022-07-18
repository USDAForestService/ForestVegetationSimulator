      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--BM   DATE OF LAST REVISION:  04/19/10
C----------
C
C     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
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
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'ECON.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'CONTRL.F77'
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
      INCLUDE 'FVSSTDCM.F77'
C
C
COMMONS
C----------
      INTEGER I,J

C----------
C     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA BKRAT/MAXSP*0./
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C   COMMON STATEMENT FOR ESCOMN VARIABLE
C----------
      DATA XMIN/0.9, 1.7, 1.0, 1.0, 0.5, 0.5, 1.3, 0.5, 0.5, 1.0,
     &          1.0, 1.0, 1.0, 1.0, 6.0, 1.0, 1.0, 1.0/
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
      DATA ISPSPE/13,15,16/
      DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,
     &  1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
      DATA HHTMAX/23., 27., 21., 21., 22., 6.0, 24., 18., 18., 17.,
     &            23., 9.0, 20., 20., 16., 20., 17., 20./
      DATA IFORCD/103, 104, 105, 106, 621, 110, 113, 114, 116, 117,
     &            118, 109, 111, 112, 412, 402, 108, 102, 115,   0/
      DATA IFORST/  3,   4,   5,   4,   7,  10,   4,  14,  16,  17,
     &              4,   9,  11,  12,  19,  20,  11,   9,  12,   4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
C     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,18)/
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     DIMENSIONED AT (20,MAXSP) WHERE THE FIRST INDEX IS A NATIONAL FOREST.
C     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,18)/
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C
C     SPECIES LIST FOR BLUE MOUNTAINS VARIANT.
C
C     1 = WESTERN WHITE PINE   119 WP  PIMO3  PINUS MONTICOLA
C     2 = WESTERN LARCH        073 WL  LAOC   LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR          202 DF  PSME   PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR            017 GF  ABGR   ABIES GRANDIS
C     5 = MOUNTAIN HEMLOCK     264 MH  TSME   TSUGA MERTENSIANA
C     6 = WESTERN JUNIPER      064 WJ  JUOC   JUNIPERUS OCCIDENTALIS (WJ FROM UT)
C     7 = LODGEPOLE PINE       108 LP  PICO   PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE     093 ES  PIEN   PICEA ENGELMANNII
C     9 = SUBALPINE FIR        019 AF  ABLA   ABIES LASIOCARPA
C    10 = PONDEROSA PINE       122 PP  PIPO   PINUS PONDEROSA
C    11 = WHITEBARK PINE       101 WB  PIAL   PINUS ALBICAULIS (WB FROM TT)
C    12 = LIMBER PINE          113 LM  PIFL2  PINUS FLEXILIS (LM FROM UT)
C    13 = PACIFIC YEW          231 PY  TABR2  TAXUS BREVIFOLIA (PY FROM WC)
C    14 = ALASKA YELLOW CEDAR  042 YC  CANO9  CALLITROPSIS NOOTKATENSIS (YC FROM WC)
C    15 = QUAKING ASPEN        746 AS  POTR5  POPULUS TREMULOIDES (AS FROM UT)
C    16 = BLACK COTTONWOOD     747 CW  POBAT  POPULUS BALSAMIFERA (CW FROM WC)
C    17 = OTHER SOFTWOODS      298 OS  2TE    (PP FROM BM)
C    18 = OTHER HARDWOODS      998 OH  2TD    (OT FROM WC)
C----------
      DATA JSP /
     & 'WP ',   'WL ',   'DF ',   'GF ',   'MH ',   'WJ ',   'LP ',
     & 'ES ',   'AF ',   'PP ',   'WB ',   'LM ',   'PY ',   'YC ',
     & 'AS ',   'CW ',   'OS ',   'OH '/
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '017',   '264',   '064',   '108',
     & '093',   '019',   '122',   '101',   '113',   '231',   '042',
     & '746',   '747',   '298',   '998'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABGR  ','TSME  ','JUOC  ','PICO  ',
     & 'PIEN  ','ABLA  ','PIPO  ','PIAL  ','PIFL2 ','TABR2 ','CANO9 ',
     & 'POTR5 ','POBAT ','2TE   ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /'WP1','WL1','DF1','GF1','MH1','WJ1','LP1','ES1','AF1',
     &          'PP1','WB1','LM1','PY1','YC1','AS1','CW1','OS1','OH1',
     &          'WP2','WL2','DF2','GF2','MH2','WJ2','LP2','ES2','AF2',
     &          'PP2','WB2','LM2','PY2','YC2','AS2','CW2','OS2','OH2',
     &          'WP3','WL3','DF3','GF3','MH3','WJ3','LP3','ES3','AF3',
     &          'PP3','WB3','LM3','PY3','YC3','AS3','CW3','OS3','OH3'/
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     &   5.035,   5.043,   4.929,   4.874,   4.874, 
     &  3.2000,   4.954,   5.035,   4.875,   4.993,
     &  4.1920,  4.1920,  5.1880,   5.143,  4.4421,
     &  5.1520,   4.993,  5.1520/
      DATA HT2/
     & -10.674,  -9.123, -10.744, -10.405, -10.405,
     & -5.0000,  -9.177, -10.674,  -9.568, -12.430,
     & -5.1651, -5.1651,-13.8010, -13.497, -6.5405,
     &-13.5760, -12.430,-13.5760/
C
      DATA SIGMAR/
     & 0.5670, 0.3383, 0.2580, 0.2548, 0.5571,
     &0.34663, 0.2820, 0.3348, 0.3249, 0.2745,
     &0.34663,0.46710, 0.4842, 0.3931,0.34663,
     & 0.5357, 0.2745, 0.5357/
C----------
C STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
C----------
C IN THE FOLLOWING DATA STATEMENTS, DF IS USED FOR OT, AND WF FOR RC
C
      DATA BB0/
     & 0.37504453,         0.0,      0.0,   -0.30935,     22.8741,
     &        0.0,     -0.0968,  2.75780,   -0.07831, 128.8952205,
     &        0.0,         0.0,   0.6192,     0.6192,         0.0,
     &     0.6192, 128.8952205,   0.6192/
      DATA BB1/ 
     &    0.92503,     1.46897, -0.37496,     1.2383,    0.950234,
     &        0.0,     0.02679,  0.83312,     0.0149,   -0.016959,
     &        0.0,         0.0,  -5.3394,    -5.3394,         0.0,
     &    -5.3394,   -0.016959,  -5.3394/
      DATA BB2/
     & -0.0207959,   0.0092466,  1.36164,   0.001762, -0.00206465,
     &        0.0, -0.00009309, 0.015701, -4.0818E-5,     1.23114,
     &        0.0,         0.0,   240.29,     240.29,         0.0,
     &     240.29,     1.23114,   240.29/
       DATA BB3/
     & -2.4881068, -0.00023957, -0.00243434, -5.4E-6,         0.5,
     &        0.0,         0.0,    22.71944,     0.0,     -0.7864,
     &        0.0,         0.0,      3368.9,  3368.9,         0.0,
     &     3368.9,     -0.7864,      3368.9/
      DATA BB4/
     &        0.0,   1.1122E-6,   -79.97,   2.046E-7,    1.365566,
     &        0.0,         0.0, -0.63557,        0.0,     2.49717,
     &        0.0,         0.0,      0.0,        0.0,         0.0,
     &        0.0,     2.49717,      0.0/
      DATA BB5/
     &        0.0,    -0.12528,  -0.2828,  -4.04E-13,    2.045963,
     &        0.0,         0.0,      0.0,        0.0,  -0.0045042,
     &        0.0,         0.0,      0.0,        0.0,         0.0,
     &        0.0,  -0.0045042,      0.0/
      DATA BB6/
     &        0.0,    0.039636,  1.87947,    -6.2056,         0.0,
     &        0.0,         0.0,      0.0,        0.0,     0.33022,
     &        0.0,         0.0,      0.0,        0.0,         0.0,
     &        0.0,     0.33022,      0.0/
      DATA BB7/
     &        0.0,  -0.0004278, -0.022399,     2.097,         0.0,
     &        0.0,         0.0,       0.0,       0.0,      100.43,
     &        0.0,         0.0,       0.0,       0.0,         0.0,
     &        0.0,      100.43,       0.0/
      DATA BB8/
     &        0.0,   1.7039E-6,  0.966998,  -0.09411,         0.0,
     &        0.0,         0.0,       0.0,       0.0,         0.0,
     &        0.0,         0.0,       0.0,       0.0,         0.0,
     &        0.0,         0.0,       0.0/
       DATA BB9/
     &        0.0,       73.57,       0.0, -0.00004382,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0/
      DATA BB10/
     &        0.0,    -0.12528,       0.0,   2.007E-11,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0/
      DATA BB11/
     &        0.0,    0.039636,       0.0,  -2.054E-17,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0/
      DATA BB12/
     &        0.0,  -0.0004278,       0.0,      -84.73,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0/
      DATA BB13/
     &        0.0,   1.7039E-6,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0,         0.0,       0.0,
     &        0.0,         0.0,       0.0/
C
      DATA REGNBK/2.999/
C
      DATA S0/55329D0/,SS/55329./
C
      DATA LSCRN,JOSCRN/.FALSE.,6/
C
      DATA JOSUME/13/
C
      DATA KOLIST,FSTOPEN /27,.FALSE./
C
      END
