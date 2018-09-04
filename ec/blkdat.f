      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C EC $Id$
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
C  SPECIES LIST FOR EAST CASCADES VARIANT.
C
C   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
C   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
C   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
C   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
C   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
C   6 = GRAND FIR               (GF)    ABIES GRANDIS
C   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
C   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
C   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
C  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
C  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
C  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
C  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
C  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
C  15 = NOBLE FIR               (NF)    ABIES PROCERA
C  16 = WHITE FIR               (WF)    ABIES CONCOLOR
C  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
C  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
C  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
C  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
C  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
C  22 = RED ALDER               (RA)    ALNUS RUBRA
C  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
C  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
C  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
C  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
C  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
C  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
C  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
C  30 = WILLOW SPECIES          (WI)    SALIX sp.
C  31 = OTHER SOFTWOODS         (OS)
C  32 = OTHER HARDWOODS         (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE EC VARIANT:
C      USE 6(GF) FOR 16(WF)
C      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
C
C  FROM THE WC VARIANT:
C      USE 19(WH) FOR 11(WH)
C      USE 33(PY) FOR 13(PY)
C      USE 31(WB) FOR 14(WB)
C      USE  7(NF) FOR 15(NF)
C      USE 30(LL) FOR 17(LL)
C      USE  8(YC) FOR 18(YC)
C      USE 29(WJ) FOR 19(WJ)
C      USE 21(BM) FOR 20(BM) AND 21(VN)
C      USE 22(RA) FOR 22(RA)
C      USE 24(PB) FOR 23(PB)
C      USE 25(GC) FOR 24(GC)
C      USE 34(DG) FOR 25(DG)
C      USE 26(AS) FOR 26(AS) AND 32(OH)
C      USE 27(CW) FOR 27(CW)
C      USE 28(WO) FOR 28(WO)
C      USE 36(CH) FOR 29(PL)
C      USE 37(WI) FOR 30(WI)
C----------
C     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA BKRAT/
     &    0.,    0.,    0.,    0.,    0.,
     &    0.,    0.,    0.,    0.,    0.,
     &    0.,    0.,    0.,    0.,    0.,
     &    0.,    0.,    0.,    0.,    0.,
     &    0.,    0.,    0.,    0.,    0.,
     &    0.,    0.,    0.,    0.,    0.,
     &    0.,    0./
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
      DATA XMIN/
     & 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0,
     & 1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 1.5, 1.0, 1.0, 1.0,
     & 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     & 0.5, 1.0/
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
C
      DATA ISPSPE/20,21,22,23,24,25,26,27,28,29,30/
C
      DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,
     &  1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
C
      DATA HHTMAX/
     & 23.0, 27.0, 21.0, 21.0, 22.0, 20.0, 24.0, 18.0, 18.0, 17.0,
     & 20.0, 22.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0,
     & 20.0, 50.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0,
     & 22.0, 20.0/
C
      DATA IFORCD/103,104,105,106,621,110,113,114,116,117,
     &            118,109,111,112,412,402,108,102,115,  0/,
     &     IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &              4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C----------
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
C     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
C----------
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0/
C----------
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     DIMENSIONED AT (20,MAXSP) WHERE THE FIRST INDEX IS A NATIONAL FOREST.
C     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
C----------
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0,
     &  20*0.0, 20*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'WP ',   'WL ',   'DF ',   'SF ',   'RC ',
     & 'GF ',   'LP ',   'ES ',   'AF ',   'PP ',
     & 'WH ',   'MH ',   'PY ',   'WB ',   'NF ',
     & 'WF ',   'LL ',   'YC ',   'WJ ',   'BM ',
     & 'VN ',   'RA ',   'PB ',   'GC ',   'DG ',
     & 'AS ',   'CW ',   'WO ',   'PL ',   'WI ',
     & 'OS ',   'OH '/
C
C  NOTE: VINE MAPLE IS CONSIDERED A SHRUB BY FIA (CODE 324); THE FIA 
C  CODE FOR ROCKY MTN MAPLE (321) WAS APPARENTLY USED IN THE INVENTORY 
C  DATA FOR VINE MAPLE. HOWEVER, IN THIS VARIANT, FIA CODE 321 IS BEING
C  ASSOCIATED WITH VINE MAPLE IN THE SPECIES TRANSLATOR.
C
      DATA FIAJSP /
     & '119',   '073',   '202',   '011',   '242',
     & '017',   '108',   '093',   '019',   '122',
     & '263',   '264',   '231',   '101',   '022',
     & '015',   '072',   '042',   '064',   '312',
     & '324',   '351',   '375',   '431',   '492',
     & '746',   '747',   '815',   '760',   '920',
     & '298',   '998'/
C
      DATA PLNJSP /
     & 'PIMO3 ','LAOC  ','PSME  ','ABAM  ','THPL  ',
     & 'ABGR  ','PICO  ','PIEN  ','ABLA  ','PIPO  ',
     & 'TSHE  ','TSME  ','TABR2 ','PIAL  ','ABPR  ',
     & 'ABCO  ','LALY  ','CANO9 ','JUOC  ','ACMA3 ',
     & 'ACCI  ','ALRU2 ','BEPA  ','CHCHC4','CONU4 ',
     & 'POTR5 ','POBAT ','QUGA4 ','PRUNU ','SALIX ',
     & '2TE   ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /
     & 'WP1','WL1','DF1','SF1','RC1','GF1','LP1','ES1','AF1','PP1',
     & 'WH1','MH1','PY1','WB1','NF1','WF1','LL1','YC1','WJ1','BM1',
     & 'VN1','RA1','PB1','GC1','DG1','AS1','CW1','WO1','PL1','WI1',
     & 'OS1','OH1',
     & 'WP2','WL2','DF2','SF2','RC2','GF2','LP2','ES2','AF2','PP2',
     & 'WH2','MH2','PY2','WB2','NF2','WF2','LL2','YC2','WJ2','BM2',
     & 'VN2','RA2','PB2','GC2','DG2','AS2','CW2','WO2','PL2','WI2',
     & 'OS2','OH2',
     & 'WP3','WL3','DF3','SF3','RC3','GF3','LP3','ES3','AF3','PP3',
     & 'WH3','MH3','PY3','WB3','NF3','WF3','LL3','YC3','WJ3','BM3',
     & 'VN3','RA3','PB3','GC3','DG3','AS3','CW3','WO3','PL3','WI3',
     & 'OS3','OH3'/
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
C   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
C   AND LARGER FOR SPECIES USING EQUATIONS FROM WC.
C----------
      DATA HT1/
     &   5.035,    4.961,   4.920,   5.032,   4.896,
     &   5.032,    4.854,   4.948,   4.834,   4.884,
     &   5.298,   3.9715,   5.188,   5.188,   5.327,
     &   5.032,    5.188,   5.143,   5.152,   4.700,
     &   4.700,    4.886,   5.152,   5.152,   5.152,
     &   5.152,    5.152,   5.152,   5.152,   5.152,
     &  3.9715,    5.152/
C
      DATA HT2/
     & -10.674,   -8.247,  -9.003, -10.482,  -8.391, 
     & -10.482,   -8.296,  -9.041,  -9.042,  -9.741,
     & -13.240,  -6.7145, -13.801, -13.801, -15.450,
     & -10.482,  -13.801, -13.497, -13.576,  -6.326,
     &  -6.326,   -8.792, -13.576, -13.576, -13.576,
     & -13.576,  -13.576, -13.576, -13.576, -13.576,
     & -6.7145,  -13.576/
C
      DATA SIGMAR/
     &  0.5086,   0.3340,  0.3420,  0.4320,  0.5500, 
     &  0.3890,   0.3060,  0.3970,  0.4690,  0.3660,
     &  0.4104,   0.3220,  0.4842,  0.4842,  0.4275,
     &  0.3890,   0.4842,  0.3931,  0.5357,  0.5107,
     &  0.5107,   0.7487,  0.5357,  0.5357,  0.5357,
     &  0.5357,   0.5357,   0.236,  0.5357,  0.5357,
     &  0.3220,   0.5357/
C----------
C   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
C----------
C   HTT1 IS USED TO STORE THE HEIGHT DUBBING COEFFICIENTS FOR TREES
C   LESS THAN 5.0" DBH FOR SPECIES USING EQUATIONS FROM WC.
C   DIMENSIONED AT (MAXSP,9)
C
      DATA HTT1/
C
C   HTT1(ISPC,1) IS USED TO STORE THE CONSTANT COEFFICIENT.
C
     & 10*0.0,
     & 1.3608,    0.0, 1.5907, 1.5907, 1.7100,
     &    0.0, 1.5907, 1.5907, 0.0994, 0.0994,
     & 0.0994, 0.0994, 0.0994, 0.0994, 0.0994,
     & 0.0994, 0.0994, 0.0994, 0.0994, 0.0994,
     &    0.0, 0.0994,
C
C   HTT1(ISPC,2) IS USED TO STORE THE DBH COEFFICIENT.
C
     & 10*0.0,
     & 0.6151,    0.0, 0.3040, 0.3040, 0.2943,
     &    0.0, 0.3040, 0.3040, 4.9767, 4.9767,
     & 4.9767, 4.9767, 4.9767, 4.9767, 4.9767,
     & 4.9767, 4.9767, 4.9767, 4.9767, 4.9767,
     &    0.0, 4.9767,
C
C   HTT1(ISPC,3) IS USED TO STORE THE CR COEFFICIENT.
C
     & 32*0.0,
C
C   HTT1(ISPC,4) IS USED TO STORE THE DBH SQUARED COEFFICIENT.
C
     & 10*0.0,
     &-0.0442,     0.,     0.,     0.,     0.,
     & 17*0.0,
C
C   HTT1(ISPC,5) IS USED TO STORE THE DUMMY VARIABLE FOR
C   MANAGED/UNMANAGED STANDS.
C
     & 10*0.0,
     & 0.0829,    0.0,    0.0,    0.0, 0.1054,
     & 17*0.0,
C
C   HTT1(ISPC,6) THRU HTT1(ISPC,9) ARE NOT USED. SET TO 0.0
C
     & 128*0.0/
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
