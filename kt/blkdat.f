      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--KT   DATE OF LAST REVISION:  02/22/11
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
      INCLUDE 'KOTCOM.F77'
C
C
      INCLUDE 'FVSSTDCM.F77'
C
C
COMMONS
C----------
      INTEGER I,J
C----------
      DATA KOTHAB/10,65,70,74,79,91,92,93,95,100,110,120,130,140,141,
     &            161,170,171,172,180,181,182,200,210,220,221,230,250,
     &            260,261,262,263,280,281,282,283,290,291,292,293,310,
     &            311,312,313,315,320,321,322,323,324,330,331,332,340,
     &            350,360,370,371,400,410,420,421,422,430,440,450,460,
     &            461,470,480,500,501,502,505,507,508,510,511,515,520,
     &            521,522,523,524,529,530,531,532,533,534,540,542,545,
     &            550,565,570,571,572,573,574,575,577,578,579,580,585,
     &            590,591,592,610,620,621,622,623,624,625,630,632,635,
     &            636,640,641,642,650,651,652,653,654,655,660,661,662,
     &            663,670,671,672,673,674,680,690,691,692,693,700,710,
     &            712,720,730,731,732,733,734,740,750,751,761,770,780,
     &            790,791,792,810,820,830,831,832,850,860,870,900,910,
     &            920,930,940,950/
C----------
C  TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA  BKRAT/0.964,0.851,0.867,0.915,0.934,0.950,0.969,0.956,
     &            0.937,0.890,0.934/
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
C  COMMON STATEMENT FOR ESCOMN VARIABLE
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
C
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
C
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
C  COMMON STATEMENT FOR PLOT VARIABLES.
C
C
C     SPECIES LIST FOR KOOKANTL VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)             LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                 ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)          THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA
C     8 = ENGLEMAN SPRUCE (ES)           PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA
C    11 = OTHER (OT)  grown as MOUNTAIN HEMLOCK   TSUGA MERTENSIANA
C----------
      DATA JSP /
     & 'WP ',   'WL ',   'DF ',   'GF ',   'WH ',   'RC ',   'LP ',
     & 'ES ',   'AF ',   'PP ',   'OT '/
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '017',   '263',   '242',   '108',
     & '093',   '019',   '122',   '999'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABGR  ','TSHE  ','THPL  ','PICO  ',
     & 'PIEN  ','ABLA  ','PIPO  ','2TREE '/
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
     > 'PP1','OT1','WP2','WL2','DF2','GF2','WH2','RC2','LP2','ES2',
     > 'AF2','PP2','OT2',
     > 'WP3','WL3','DF3','GF3','WH3','RC3','LP3','ES3','AF3','PP3','OT3'
     > /
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     &   5.186772,5.054504,4.876799,5.063864,4.927262,4.881348,
     &   4.777827,5.079578,4.930150,5.019903,4.77951/
      DATA HT2/
     & -10.421897,-8.618720,-9.146701,-9.892393,-8.727470,-9.628555,
     & -6.336417,-10.201550,-8.825197,-12.014787,-9.31743/
C
C  RESIDUAL ERROR ESTIMATES WERE MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR; 5/10/91--WRW.
C

      DATA SIGMAR/
     &   0.4099,  0.4263,  0.4149,  0.4069,  0.3989,  0.4003,
     &   0.3861,  0.4171,  0.4249,  0.3891,  0.3433/
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
