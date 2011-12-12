      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--EC   DATE OF LAST REVISION:  04/12/10
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
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
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
      DATA BKRAT/
     & 0.964, 0.851, 0.844, 0.903, 0.950, 0.903,
     & 0.963, 0.956, 0.903, 0.889, 0.934/
C
      DATA COR2 /11*1./, HCOR2 /11*1./,RCOR2/11*1.0/
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
      DATA XMIN/1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0, 0.5/
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  ISPSPE/NSPSPE*12/,BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,HHTMAX/23.0,27.0,2*21.0,22.0,20.0,24.0,
     &  2*18.0,17.0,22.0/,
     &  IFORCD/103,104,105,106,621,110,113,114,116,117,
     &         118,109,111,112,412,402,108,102,115,  0/,
     &  IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &           4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
      DATA ((OCURHT(I,J),I=1,16),J=1,2)/
     1  0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,1.0,1.0,0.0,1.0,0.0,
     2  13*1.0,                                             0.0,1.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=3,4)/ 15*1.0, 0.0,
     4  0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=5,6)/ 9*0.0,1.0,6*0.0,
     6  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=7,8)/  16*1.0,
     8  0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
      DATA ((OCURHT(I,J),I=1,16),J=9,11)/
     9  0.0,0.0,0.0,0.0,1.0,1.0,1.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     O  1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,7*0.0,            16*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
      DATA ((OCURNF(I,J),I=1,20),J=1,2)/
     1    0.,0.,0.,1.,1.,0.,1.,0.,0.,1.,3*0.0,   1.,0.,1.,0.,0.,0.,0.,
     2    0.,0.,3*1.0,   0.,1.,0.,1.,1.,3*0.0,   1.,0.,1.,1.,0.,1.,0./
      DATA ((OCURNF(I,J),I=1,20),J=3,4)/
     3    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1.,
     4    0.,0.,3*1.0,   0.,1.,0.,0.,1.,3*0.0,   1.,0.,1.,1.,0.,1.,1./
      DATA ((OCURNF(I,J),I=1,20),J=5,6)/
     5    3*0.0,   1.,1.,0.,1.,6*0.0,            1.,0.,1.,4*0.0,
     6    3*0.0,   1.,1.,0.,1.,6*0.0,            1.,0.,1.,1.,3*0.0/
      DATA ((OCURNF(I,J),I=1,20),J=7,8)/
     7    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1.,
     8    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1./
      DATA ((OCURNF(I,J),I=1,20),J=9,11)/
     9    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1.,
     O    0.,0.,3*1.0,0.,1.,0.,1.,4*0.0,1.,0.,1.,1.,0.,1.,1.,  20*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C
C     SPECIES LIST FOR EAST CASCADES VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)     PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)          LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)            PSEUDOTSUGA MENZIESII
C     4 = PACIFIC SILVER FIR (SF)     ABIES AMABILIS
C     5 = WESTERN REDCEDAR (RC)       THUJA PLICATA
C     6 = GRAND FIR (GF)              ABIES GRANDIS
C     7 = LODGEPOLE PINE (LP)         PINUS CONTORTA
C     8 = ENGLEMAN SPRUCE (ES)        PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)          ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)         PINUS PONDEROSA
C    11 = OTHER (OT)
C----------
      DATA JSP /
     & 'WP ',   'WL ',   'DF ',   'SF ',   'RC ',   'GF ',   'LP ',
     & 'ES ',   'AF ',   'PP ',   'OT '/
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '011',   '242',   '017',   '108',
     & '093',   '019',   '122',   '999'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABAM  ','THPL  ','ABGR  ','PICO  ',
     & 'PIEN  ','ABLA  ','PIPO  ','2TREE '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /'WP1','WL1','DF1','SF1','RC1','GF1','LP1','ES1','AF1',
     > 'PP1','OT1','WP2','WL2','DF2','SF2','RC2','GF2','LP2','ES2',
     > 'AF2','PP2','OT2',
     > 'WP3','WL3','DF3','SF3','RC3','GF3','LP3','ES3','AF3','PP3','OT3'
     > /
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     & 5.035, 4.961, 4.920, 5.032, 4.896, 5.032,
     & 4.854, 4.948, 4.834, 4.884, 3.9715/
      DATA HT2/
     & -10.674,   -8.247,  -9.003, -10.482, -8.391, -10.482,
     &  -8.296,  -9.041,  -9.042,  -9.741, -6.7145/
      DATA SIGMAR/
     & 0.5086, 0.3340, 0.3420, 0.4320, 0.5500, 0.3890,
     & 0.3060, 0.3970, 0.4690, 0.3660, 0.3220/
C----------
C   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
C----------
      DATA BB0/
     & 0.37504453, 0.0, 0.0, -0.30935, 0.0, -0.30935, 9.89331,
     & 2.75780, -0.07831, 128.8952205, 22.8741/
      DATA BB1/
     & 0.92503, 1.46897, -0.37496, 1.2383, 1.3283 ,1.2383, -0.19177,
     & 0.83312,0.0149,-0.016959,0.950234/
      DATA BB2/
     & -0.0207959, 0.0092466, 1.36164, 0.001762, -0.0174, 0.001762,
     & 0.00124, 0.015701, -4.0818E-5, 1.23114, -0.00206465/
       DATA BB3/
     & -2.4881068, -0.00023957, -0.00243434, -5.4E-6, 1.4711, -5.4E-6,
     & -0.00082, 22.71944, 0.0, -0.7864, 0.5/
      DATA BB4/
     & 0.0, 1.1122E-6, -79.97, 2.046E-7, 0.0, 2.046E-7,
     & 0.01387 ,-0.63557, 0.0, 2.49717, 1.365566/
      DATA BB5/
     & 0.0, -0.12528, -0.2828, -4.04E-13, 0.0, -4.04E-13, -0.0000455,
     & 2*0.0, -0.004504, 2.045963/
      DATA BB6/
     & 0., 0.039636, 1.87947, -6.2056, 0., -6.2056, 3*0., 0.33022, 0./
      DATA BB7/
     & 0., -0.0004278, -0.022399, 2.097, 0., 2.097, 3*0., 100.43, 0./
      DATA BB8/
     & 0.0,1.7039E-6,0.966998,-0.09411,0.0,-0.09411,4*0.0,0.0/
       DATA BB9/
     & 0.0,73.57,0.0,-0.00004382,0.0,-0.00004382,5*0.0/
      DATA BB10/
     & 0.0,-0.12528,0.0,2.007E-11,0.0,2.007E-11,5*0.0/
      DATA BB11/
     & 0.0,0.039636,0.0,-2.054E-17,0.0,-2.054E-17,5*0.0/
      DATA BB12/
     & 0.0,-0.0004278,0.0,-84.73,0.0,-84.73,5*0.0/
      DATA BB13/
     & 0.0,1.7039E-6,9*0.0/
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
