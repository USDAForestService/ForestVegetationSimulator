      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C IE $Id$
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
      INCLUDE 'FVSSTDCM.F77'
C
C
COMMONS
C----------
      INTEGER I,J
C----------
      DATA  BKRAT/MAXSP*0./
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C   COMMON STATEMENT FOR ESCOMN VARIABLE
C----------
      DATA XMIN/1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0, 0.5,
     &     1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 6.0, 3.0, 6.0, 6.0, 3.0, 0.5/
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  ISPSPE/17,18,19,20,21/,
     &  BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,
     &  HHTMAX/23.0, 27.0, 2*21.0, 22.0, 20.0, 24.0, 2*18.0, 17.0, 22.0,
     &  2*27.0, 18.0, 2*6.0, 27.0, 5*16.0, 22.0/,
     &  IFORCD/103,104,105,106,621,110,113,114,116,117,
     &         118,109,111,112,412,402,108,102,115,  0/,
     &  IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &           4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
C     FOR SPECIES EXPANSION, NONE OF THE ADDED SPECIES HAVE NATURAL REGEN.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,2)/
     & 0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,1.0,1.0,0.0,1.0,0.0,
     & 13*1.0,0.0,1.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=3,4)/
     & 15*1.0, 0.0,
     & 0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=5,6)/
     & 9*0.0,1.0,6*0.0,
     & 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=7,8)/
     & 16*1.0,
     & 0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
      DATA ((OCURHT(I,J),I=1,16),J=9,11)/
     & 0.0,0.0,0.0,0.0,1.0,1.0,1.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     & 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,7*0.0,
     & 16*0.0/
      DATA ((OCURHT(I,J),I=1,16),J=12,14)/
     & 16*0.0,
     & 16*0.0,
     & 16*0.0/
      DATA ((OCURHT(I,J),I=1,16),J=15,17)/
     & 16*0.0,
     & 16*0.0,
     & 16*0.0/
      DATA ((OCURHT(I,J),I=1,16),J=18,20)/
     & 16*0.0,
     & 16*0.0,
     & 16*0.0/
      DATA ((OCURHT(I,J),I=1,16),J=21,MAXSP)/
     & 16*0.0,
     & 16*0.0,
     & 16*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,2)/
     & 0.,0.,0.,1.,1.,0.,1.,0.,0.,1.,3*0.0,1.,0.,1.,0.,0.,0.,0.,
     & 0.,0.,3*1.0,0.,1.,0.,1.,1.,3*0.0,1.,0.,1.,1.,0.,1.,0./
      DATA ((OCURNF(I,J),I=1,20),J=3,4)/
     & 0.,0.,3*1.0,0.,1.,0.,4*1.0,0.,1.,0.,1.,1.,0.,1.,1.,
     & 0.,0.,3*1.0,0.,1.,0.,0.,1.,3*0.0,1.,0.,1.,1.,0.,1.,1./
      DATA ((OCURNF(I,J),I=1,20),J=5,6)/
     & 3*0.0,1.,1.,0.,1.,6*0.0,1.,0.,1.,4*0.0,
     & 3*0.0,1.,1.,0.,1.,6*0.0,1.,0.,1.,1.,3*0.0/
      DATA ((OCURNF(I,J),I=1,20),J=7,8)/
     & 0.,0.,3*1.0,0.,1.,0.,4*1.0,0.,1.,0.,1.,1.,0.,1.,1.,
     & 0.,0.,3*1.0,0.,1.,0.,4*1.0,0.,1.,0.,1.,1.,0.,1.,1./
      DATA ((OCURNF(I,J),I=1,20),J=9,11)/
     & 0.,0.,3*1.0,0.,1.,0.,4*1.0,0.,1.,0.,1.,1.,0.,1.,1.,
     & 0.,0.,3*1.0,0.,1.,0.,1.,4*0.0,1.,0.,1.,1.,0.,1.,1.,
     & 20*0.0/
      DATA ((OCURNF(I,J),I=1,20),J=12,MAXSP)/
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0,
     & 20*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C
C
C     SPECIES LIST FOR INLAND EMPIRE VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA (FR0M NI)
C     2 = WESTERN LARCH (WL)             LARIX OCCIDENTALIS (FR0M NI)
C     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII (FR0M NI)
C     4 = GRAND FIR (GF)                 ABIES GRANDIS (FR0M NI)
C     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA (FR0M NI)
C     6 = WESTERN REDCEDAR (RC)          THUJA PLICATA (FR0M NI)
C     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA (FR0M NI)
C     8 = ENGLEMAN SPRUCE (ES)           PICEA ENGELMANNII (FR0M NI)
C     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA (FR0M NI)
C    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA (FR0M NI)
C    11 = MOUNTAIN HEMLOCK (MH)          TSUGA MERTENSIANA (OT FR0M NI)
C    12 = WHITEBARK PINE (WB)            PINUS ALBICAULIS (WL FR0M NI)
C    13 = LIMBER PINE (LM)               PINUS FLEXILIS (FR0M TT)
C    14 = SUBALPINE LARCH (LL)           LARIX LYALLII (AF FR0M NI)
C    15 = SINGLELEAF PINYON (PM)         PINUS MONOPHYLLA (FR0M UT)
C    16 = ROCKY MOUNTAIN JUNIPER (RM)    JUNIPERUS SCOPULORUM (FR0M UT)
C    17 = PACIFIC YEW (PY)               TAXUS BREVIFOLIA (LM FR0M TT)
C    18 = QUAKING ASPEN (AS)             POPULUS TREMULOIDES (FR0M UT)
C    19 = COTTONWOOD (CO)                POPULUS SPP. (OH FR0M CR)
C    20 = MOUNTAIN MAPLE (MM)            ACER GLABRUM (AS FROM UT)
C    21 = PAPER BIRCH (PB)               BETULA PAPYRIFERA (AS FROM UT)
C    22 = OTHER HARDWOODS (OH)           (OH FR0M CR)
C    23 = OTHER SOFTWOODS (OS)           (FR0M NI)
C----------
      DATA JSP /
     & 'WP ',   'WL ',   'DF ',   'GF ',   'WH ',   'RC ',   'LP ',
     & 'ES ',   'AF ',   'PP ',   'MH ',   'WB ',   'LM ',   'LL ',
     & 'PM ',   'RM ',   'PY ',   'AS ',   'CO ',   'MM ',   'PB ',
     & 'OH ',   'OS '/
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '017',   '263',   '242',   '108',
     & '093',   '019',   '122',   '264',   '101',   '113',   '072',
     & '133',   '066',   '231',   '746',   '740',   '321',   '375',
     & '998',   '299'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABGR  ','TSHE  ','THPL  ','PICO  ',
     & 'PIEN  ','ABLA  ','PIPO  ','TSME  ','PIAL  ','PIFL2 ','LALY  ',
     & 'PIMO  ','JUSC2 ','TABR2 ','POTR5 ','POPUL ','ACGL  ','BEPA  ',
     & '2TB   ','2TN   '/
C
      DATA JTYPE / 10,100,110,130,140,160,170,180,190,200,
     &            210,220,230,250,260,280,290,310,320,330,
     &            340,350,360,370,380,400,410,420,430,440,
     &            450,460,470,480,500,501,502,505,506,510,
     &            515,516,520,529,530,540,545,550,555,560,
     &            565,570,575,579,590,600,610,620,630,635,
     &            640,650,660,670,675,680,685,690,700,701,
     &            710,720,730,740,750,770,780,790,800,810,
     &            820,830,840,850,860,870,890,900,910,920,
     &            925,930,940,950,999,27*0 /
C
      DATA NSP /'WP1','WL1','DF1','GF1','WH1','RC1','LP1','ES1','AF1',
     >          'PP1','MH1','WB1','LM1','LL1','PM1','RM1','PY1','AS1',
     >          'CO1','MM1','PB1','OH1','OS1',
     >          'WP2','WL2','DF2','GF2','WH2','RC2','LP2','ES2','AF2',
     >          'PP2','MH2','WB2','LM2','LL2','PM2','RM2','PY2','AS2',
     >          'CO2','MM2','PB2','OH2','OS2',
     >          'WP3','WL3','DF3','GF3','WH3','RC3','LP3','ES3','AF3',
     >          'PP3','MH3','WB3','LM3','LL3','PM3','RM3','PY3','AS3',
     >          'CO3','MM3','PB3','OH3','OS3'/
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     &   5.19988, 4.97407, 4.81519, 5.00233, 4.97331, 4.89564,
     &   4.62171, 4.92190, 4.76537, 4.92880, 4.77951, 4.97407,
     &   4.19200, 4.76537,     3.2,     3.2, 4.19200, 4.44210,
     &   4.44210, 4.44210, 4.44210, 4.44210, 4.77951/
      DATA HT2/
     &  -9.26718,-6.78347,-7.29306,-8.19365,-8.19730,-8.39057,
     &  -5.32481,-8.30289,-7.61062,-9.32795,-9.31743,-6.78347,
     &  -5.16510,-7.61062,    -5.0,    -5.0,-5.16510,-6.54050,
     &  -6.54050,-6.54050,-6.54050,-6.54050,-9.31743/
C
C  RESIDUAL ERROR ESTIMATES WERE MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR; 5/10/91--WRW.
C
      DATA SIGMAR/
     &   0.3814,  0.4243,  0.4243,  0.4166,  0.4011,  0.4125,
     &   0.3606,  0.4121,  0.4345,  0.4243,  0.3433,  0.4243,
     &   0.4671,  0.4345,  0.2000,  0.2000,  0.4671,  0.3750,
     &   0.2000,  0.3750,  0.3750,  0.2000,  0.3433/
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
