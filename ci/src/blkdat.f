      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--CI   DATE OF LAST REVISION:  06/06/11
C----------
C
C     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
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
      INCLUDE 'CICOM.F77'
C
C
      INCLUDE 'FVSSTDCM.F77'
C
C
COMMONS
C----------
      INTEGER I,J
C----------
C
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
C----------
C     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA  BKRAT/
     &   0.0,   0.0,   0.0,   0.0,   0.0, 
     &   0.0,   0.0,   0.0,   0.0,   0.0,
     &   0.0,   0.0,   0.0,   0.0,   0.0,
     &   0.0,   0.0,   0.0,   0.0/
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1./
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
      DATA XMIN/
     & 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0,
     & 1.0, 1.0, 6.0, 0.5, 0.5, 1.0, 3.0, 0.5, 3.0/
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
C
      DATA ISPSPE/12,13,17/
C
      DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,
     &  1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
C
      DATA HHTMAX/
     & 23.0, 27.0, 21.0, 21.0, 22.0, 20.0, 24.0, 18.0, 18.0, 17.0,
     & 27.0, 27.0, 16.0,  6.0,  6.0, 27.0, 16.0, 22.0, 16.0/
C
      DATA IFORCD/103,104,105,106,621,110,113,114,116,117,
     &            118,109,111,112,412,402,108,102,115,  0/,
     &    IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &             4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C----------
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
C----------
      DATA ((OCURHT(I,J),I=1,16),J=1,10)/
     1  0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 0., 1., 1., 0., 1., 0.,
     2  1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 0., 1., 0.,
     3  1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 0.,
     4  0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 0.,
     5  0., 0., 0., 0., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0.,
     6  0., 0., 0., 0., 0., 0., 0., 0., 1., 1., 0., 0., 0., 0., 0., 0.,
     7  1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.,
     8  0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.,
     9  0., 0., 0., 0., 1., 1., 1., 0., 1., 1., 1., 1., 1., 1., 1., 1.,
     O  1., 1., 1., 1., 1., 1., 1., 1., 1., 0., 0., 0., 0., 0., 0., 0./
      DATA ((OCURHT(I,J),I=1,16),J=11,MAXSP)/
     1  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     2  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     3  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     4  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     5  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     6  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     7  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     8  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     9  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
C----------
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     DIMENSIONED AT (20,MAXSP) WHERE THE FIRST INDEX IS A NATIONAL FOREST.
C----------
      DATA ((OCURNF(I,J),I=1,20),J=1,5)/
     1 0., 0., 0., 1., 1., 0., 1., 0., 0., 1., 0., 0., 0., 1., 0.,
     & 1., 0., 0., 0., 0.,
     2 0., 0., 1., 1., 1., 0., 1., 0., 1., 1., 0., 0., 0., 1., 0.,
     & 1., 1., 0., 1., 1.,
     3 0., 0., 1., 1., 1., 0., 1., 0., 1., 1., 1., 1., 0., 1., 0.,
     & 1., 1., 0., 1., 1.,
     4 0., 0., 1., 1., 1., 0., 1., 0., 0., 1., 0., 0., 0., 1., 0.,
     & 1., 1., 0., 1., 1.,
     5 0., 0., 0., 1., 1., 0., 1., 0., 0., 0., 0., 0., 0., 1., 0.,
     & 1., 0., 0., 0., 0./
      DATA ((OCURNF(I,J),I=1,20),J=6,10)/
     6 0., 0., 0., 1., 1., 0., 1., 0., 0., 0., 0., 0., 0., 1., 0.,
     & 1., 1., 0., 0., 0.,
     7 0., 0., 1., 1., 1., 0., 1., 0., 1., 1., 1., 1., 0., 1., 0.,
     & 1., 1., 0., 1., 1.,
     8 0., 0., 1., 1., 1., 0., 1., 0., 1., 1., 1., 1., 0., 1., 0.,
     & 1., 1., 0., 1., 1.,
     9 0., 0., 1., 1., 1., 0., 1., 0., 1., 1., 1., 1., 0., 1., 0.,
     & 1., 1., 0., 1., 1.,
     O 0., 0., 1., 1., 1., 0., 1., 0., 1., 0., 0., 0., 0., 1., 0.,
     & 1., 1., 0., 1., 1./
      DATA ((OCURNF(I,J),I=1,20),J=11,15)/
     1 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     2 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     3 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     4 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     5 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0./
      DATA ((OCURNF(I,J),I=1,20),J=16,MAXSP)/
     6 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     7 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     8 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0.,
     9 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0./
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'WP ',   'WL ',   'DF ',   'GF ',   'WH ',
     & 'RC ',   'LP ',   'ES ',   'AF ',   'PP ',
     & 'WB ',   'PY ',   'AS ',   'WJ ',   'MC ',
     & 'LM ',   'CW ',   'OS ',   'OH '/
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '017',   '263',
     & '242',   '108',   '093',   '019',   '122',
     & '101',   '231',   '746',   '064',   '475',
     & '113',   '747',   '298',   '998'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABGR  ','TSHE  ',
     & 'THPL  ','PICO  ','PIEN  ','ABLA  ','PIPO  ',
     & 'PIAL  ','TABR2 ','POTR5 ','JUOC  ','CELE3 ',
     & 'PIFL2 ','POBAT ','2TE   ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /
     & 'WP1','WL1','DF1','GF1','WH1','RC1','LP1','ES1','AF1','PP1',
     & 'WB1','PY1','AS1','WJ1','MC1','LM1','CW1','OS1','OH1',
     & 'WP2','WL2','DF2','GF2','WH2','RC2','LP2','ES2','AF2','PP2',
     & 'WB2','PY2','AS2','WJ2','MC2','LM2','CW2','OS2','OH2',
     & 'WP3','WL3','DF3','GF3','WH3','RC3','LP3','ES3','AF3','PP3',
     & 'WB3','PY3','AS3','WJ3','MC3','LM3','CW3','OS3','OH3'/
C----------
C      COMMON STATEMENT FOR CICOM VARIABLES.
C----------
      DATA ICITYP/
     &  50,  60,  70,  80, 100, 120, 130, 140, 160, 161,
     & 162, 170, 190, 195, 200, 210, 220, 221, 222, 250,
     & 260, 262, 264, 265, 280, 290, 310, 313, 315, 320,
     & 323, 324, 325, 330, 331, 332, 334, 340, 341, 343,
     & 344, 360, 370, 371, 372, 375, 380, 385, 390, 392,
     & 393, 395, 396, 397, 398, 400, 410, 440, 490, 493,
     & 500, 505, 510, 511, 515, 520, 525, 526, 527, 580,
     & 585, 590, 591, 592, 593, 600, 605, 620, 621, 625,
     & 635, 636, 637, 638, 640, 645, 650, 651, 652, 654,
     & 655, 660, 661, 662, 663, 670, 671, 672, 690, 691,
     & 692, 694, 700, 705, 720, 721, 723, 730, 731, 732,
     & 734, 740, 745, 750, 780, 790, 791, 793, 810, 830,
     & 831, 833, 850, 870, 900, 905, 920, 940, 955, 999/
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
C   WP HT-DBH COEFFS FROM NI VARIANT, PP FROM BM VARIANT
C
      DATA HT1/
     &   5.19988,   5.16306,   4.94866,   5.02706,   5.02706,
     &   5.16306,   4.80016,   5.09964,   4.91417,     4.993,
     &   4.19200,   4.19200,   4.44210,    3.2000,    5.1520,
     &   4.19200,   4.44210,   4.80016,   4.44210/
      DATA HT2/
     &  -9.26718,  -9.25656,  -9.75378, -11.21681, -11.21681,
     &  -9.25656,  -6.51738, -10.79269,  -9.36400,   -12.430,
     &  -5.16510,  -5.16510,  -6.54050,   -5.0000,  -13.5760,
     &  -5.16510,  -6.54050,  -6.51738,  -6.54050/
C
      DATA SIGMAR/
     &     0.230,     0.206,     0.267,     0.260,     0.260,
     &     0.206,     0.203,     0.232,     0.245,     0.230,
     &    0.4671,    0.4671,    0.3750,       0.2,    0.5357,
     &    0.4671,    0.2000,     0.203,    0.2000/
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
