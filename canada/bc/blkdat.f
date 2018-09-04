      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C CANADA-BC $Id$
C----------
C
C     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'ESPARM.F77'
      INCLUDE 'ESCOMN.F77'
      INCLUDE 'PDEN.F77'
      INCLUDE 'ECON.F77'
      INCLUDE 'HTCAL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RANCOM.F77'
      INCLUDE 'SCREEN.F77'
      INCLUDE 'FVSSTDCM.F77'      
      INCLUDE 'BCPLOT.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C
C
C     birch,aspen,cottonwood - from B. Snowdon: LS uses 0.94, 0.92, 0.93
C
      INTEGER I,J
C
C     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C
      DATA  BKRAT/0.964,0.851,0.867,0.915,0.934,0.950,0.969,0.956,
     &            0.937,0.890,0.939,0.916,0.897,0.867,0.939/
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/

      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F5.1,F4.1,2F5.1,F5.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C
C   COMMON STATEMENT FOR ESCOMN VARIABLE
C
      DATA XMIN/1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0,
     >          1.0, 1.0, 1.0, 1.0, 1.0 /
      DATA  DBHMID /1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  ISPSPE /11, 12, 13, 15/,
     &  BNORML /3*1.0,1.046,1.093,1.139,1.186,1.232,
     &    1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &    1.742,1.789/,
     &  HHTMAX /23.0,27.0,2*21.0,22.0,20.0,24.0,2*18.0,17.0,3*20.0,
     &          21.0,20.0/

     &  IFORCD /20 * 0 /,
     &  IFORST /20 * 0 /

c     &  IFORCD /103,104,105,106,621,110,113,114,116,117,
c     &         118,109,111,112,412,402,  0,  0,  0,  0/,
c     &  IFORST / 3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
c     &           4,  9, 11, 12, 19, 20,  4,  4,  4,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     - SET to 0; NOT USED IN STRP VERSION
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/240*0.0/
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/300*0.0/
C
C     COMMON STATEMENT FOR PLOT VARIABLES.
C
      DATA JSP /'PW ','LW ','FD ','BG ','HW ','CW ','PL ','SE ',
     &  'BL ','PY ','EP ','AT ','AC ','OC ','OH '/
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '017',   '263',   '242',   '108',
     & '093',   '019',   '122',   '375',   '746',   '747',   '202',
     & '376'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABGR  ','TSHE  ','THPL  ','PICO  ',
     & 'PIEN  ','ABLA  ','PIPO  ','BEPA  ','POTR5 ','POBAT ','SOFT  ',
     & 'HARD  '/
C
C     JTYPE NOT USED IN BC VARIANT
C
      DATA JTYPE / 122*0 /
C
      DATA NSP /'PW1','LW1','FD1','BG1','HW1','CW1','PL1','SE1','BL1',
     > 'PY1','EP1','AT1','AC1','OC1','OH1','PW2','LW2','FD2','BG2',
     > 'HW2','CW2','PL2','SE2','BL2','PY2','EP2','AT2','AC2','OC2',
     > 'OH2','PW3','LW3','FD3','BG3','HW3','CW3','PL3','SE3','BL3',
     > 'PY3','EP3','AT3','AC3','OC3','OH3' /
C
C     SPECIES LIST FOR IBC VARIANT. 
C
C     1 = WESTERN WHITE PINE (WP)     [PW]
C     2 = WESTERN LARCH (WL)          [LW]
C     3 = DOUGLAS-FIR (DF)            [FD]
C     4 = GRAND FIR (GF)              [BG]
C     5 = WESTERN HEMLOCK (WH)        [HW]
C     6 = WESTERN REDCEDAR (RC)       [CW]
C     7 = LODGEPOLE PINE (LP)         [PL]
C     8 = ENGELMANN SPRUCE (ES)       [SE] ! aka SX
C     9 = SUBALPINE FIR (AF)          [BL]
C    10 = PONDEROSA PINE (PP)         [PY]
C    11 = BIRCH (PB)                  [EP] 24 in WC
C    12 = ASPEN (AS)                  [AT] 26 in WC
C    13 = COTTONWOOD (CW)             [AC] 27 in WC
C    14 = OTHER CONIFER (OC)          [  ] ! FD
C    15 = OTHER HARDWOOD (OH)         [  ] ! EP
C
C   COMMON STATEMENT FOR COEFFS VARIABLES
C
C
C     HT FOR EP,AT,AC TAKEN FROM WC-VARIANT
C
      DATA HT1/
     &   5.19988, 4.97407, 4.81519, 5.00233, 4.97331, 4.89564,
     &   4.62171, 4.92190, 4.76537, 4.92880, 5.152,   5.152,
     &   5.152,   4.81519, 5.152 /
      DATA HT2/
     &  -9.26718,-6.78347,-7.29306,-8.19365,-8.19730,-8.39057,
     &  -5.32481,-8.30289,-7.61062,-9.32795,-13.576, -13.576,
     &  -13.576, -7.29306,-13.576 /
C
C     BEC-SPECIFIC RESIDUAL ERROR ESTIMATES ARE PROVIDED
C     IN **DGF**
C
! divide by two here, rather than in DGF; otherwise sequential
! runs break down
!      DATA SIGMAR/
!    &   0.3814,  0.4243,  0.4243,  0.4166,  0.4011,  0.4125,
!    &   0.3606,  0.4121,  0.4345,  0.4243,  0.5357,  0.5357,
!    &   0.5357,  0.4243,  0.5357 /
      DATA SIGMAR/
     &   0.1907,  0.2122,  0.2122,  0.2083,  0.2006,  0.2063,
     &   0.1803,  0.2061,  0.2173,  0.2122,  0.2679,  0.2679,
     &   0.2679,  0.2122,  0.2679 /
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
