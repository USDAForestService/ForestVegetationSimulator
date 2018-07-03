      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C CA $Id: blkdat.f 0000 2018-02-14 00:00:00Z gedixon $
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
      DATA COR2/49*1./, HCOR2/49*1./, RCOR2/49*1.0/, BKRAT/49*1.0/
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
      DATA XMIN/ 0.5, 0.5, 0.3, 0.8, 0.8, 0.8, 0.8, 0.3, 0.5, 1.2,
     &           1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 1.0, 0.8, 1.2,
     &           1.0, 0.5, 1.0, 0.3, 0.8, 1.0, 0.5, 1.0, 1.0, 0.8,
     &           1.0, 0.8, 1.0, 0.5, 0.8, 0.8, 0.5, 0.8, 0.5, 0.8,
     &           1.0, 0.5, 1.0, 1.2, 1.2, 1.0, 0.3, 0.5, 0.75 /
      DATA ISPSPE/24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     &            41,42,43,44,45,46,47,48/
      DATA HHTMAX/ 49*20.0 /
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,1.371,
     &  1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/,
     &  IFORCD/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0/,
     &  IFORST/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/784*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/980*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C
C     SPECIES LIST FOR ICASCA VARIANT
C
C     1 = PORT ORFORD CEDAR (PC)          CHAMAECYPARIS LAWSONIANA
C     2 = INCENSE CEDAR (IC)              LIBOCEDRUS DECURRENS
C     3 = WESTERN REDCEDAR (RC)           THUJA PLICATA
C     4 = WHITE FIR (WF)                  ABIES CONCOLOR
C     5 = CALIFORNIA RED FIR (RF)         ABIES MAGNIFICA (MAGNIFICA)
C     6 = SHASTA RED FIR (SH)             ABIES MAGNIFICA (SHASTENSIS)
C     7 = DOUGLAS-FIR (DF)                PSEUDOTSUGA MENZIESII
C     8 = WESTERN HEMLOCK (WH)            TSUGA HETEROPHYLLA
C     9 = MOUNTAIN HEMLOCK (MH)           TSUGA MERTENSIANA
C    10 = WHITEBARK PINE (WB)             PINUS ALBICAULIS
C    11 = KNOBCONE PINE (KP)              PINUS ATTENUATA
C    12 = LODGEPOLE PINE (LP)             PINUS CONTORTA
C    13 = COULTER PINE (CP)               PINUS COULTERI
C    14 = LIMBER PINE (LM)                PINUS FLEXILIS (FLEXILIS)
C    15 = JEFFREY PINE (JP)               PINUS JEFFREYI
C    16 = SUGAR PINE (SP)                 PINUS LAMBERTIANA
C    17 = WESTERN WHITE PINE (WP)         PINUS MONTICOLA
C    18 = PONDEROSA PINE (PP)             PINUS PONDEROSA
C    19 = MONTEREY PINE (MP)              PINUS RADIATA
C    20 = GRAY PINE (GP)                  PINUS SABINIANA
C    21 = WESTERN JUNIPER (WJ)            JUNIPERUS OCCIDENTALIS
C    22 = BREWER SPRUCE (BR)              PICEA BREWERIANA
C    23 = GIANT SEQUOIA (GS)              SEQUOIADENDRON GIGANTEUM
C    24 = PACIFIC YEW (PY)                TAXUS BREVIFOLIA
C    25 = OTHER SOFTWOODS (OS)
C    26 = COAST LIVE OAK (LO)             QUERCUS AGRIFOLIA
C    27 = CANYON LIVE OAK (CY)            QUERCUS CHRYSOLEPSIS
C    28 = BLUE OAK (BL)                   QUERCUS DOUGLASII
C    29 = ENGELMANN OAK (EO)              QUERCUS ENGELMANNI
C    30 = OREGON WHITE OAK (WO)           QUERCUS GARRYANA
C    31 = CALIFORNIA BLACK OAK (BO)       QUERCUS KELLOGGII
C    32 = VALLEY WHITE OAK (VO)           QUERCUS LOBATA
C    33 = INTERIOR LIVE OAK (IO)          QUERCUS WISLIZENII
C    34 = BIGLEAF MAPLE (BM)              ACER MACROPHYLLUM
C    35 = CALIFORNIA BUCKEYE (BU)         AESCULUS CALIFORNICA
C    36 = RED ALDER (RA)                  ALNUS RUBRA
C    37 = PACIFIC MADRONE (MA)            ARBUTUS MENZIESII
C    38 = GIANT CHINQUAPIN (GC)           CHRYSOLEPIS CHRYSOPHYLLA
C    39 = PACIFIC DOGWOOD (DG)            CORNUS NUTTALLII
C    40 = OREGON ASH (FL)                 FRAXINUS LATIFOLIA
C    41 = WALNUT (WN)                     JUGLANS sp.
C    42 = TANOAK (TO)                     LITHOCARPUS DENSIFLORUS
C    43 = CALIFORNIA SYCAMORE (SY)        PLATANUS RACEMOSA
C    44 = QUAKING ASPEN (AS)              POPULUS TREMULOIDES
C    45 = BLACK COTTONWOOD (CW)           POPULUS TRICHOCARPA
C    46 = WILLOW (WI)                     SALIX sp.
C    47 = CALIFORNIA NUTMEG (CN)          TORREYA CALIFORNICA
C    48 = CALIFORNIA LAUREL (CL)          UMBELLULARIA CALIFORNICA
C    49 = OTHER HARDWOODS (OH)
C----------
      DATA JSP /
     & 'PC ',   'IC ',   'RC ',   'WF ',   'RF ',   'SH ',   'DF ',
     & 'WH ',   'MH ',   'WB ',   'KP ',   'LP ',   'CP ',   'LM ',
     & 'JP ',   'SP ',   'WP ',   'PP ',   'MP ',   'GP ',   'WJ ',
     & 'BR ',   'GS ',   'PY ',   'OS ',   'LO ',   'CY ',   'BL ',
     & 'EO ',   'WO ',   'BO ',   'VO ',   'IO ',   'BM ',   'BU ',
     & 'RA ',   'MA ',   'GC ',   'DG ',   'FL ',   'WN ',   'TO ',
     & 'SY ',   'AS ',   'CW ',   'WI ',   'CN ',   'CL ',   'OH '/
C
      DATA FIAJSP /
     & '041',   '081',   '242',   '015',   '020',   '021',   '202',
     & '263',   '264',   '101',   '103',   '108',   '109',   '113',
     & '116',   '117',   '119',   '122',   '124',   '127',   '064',
     & '092',   '212',   '231',   '298',   '801',   '805',   '807',
     & '811',   '815',   '818',   '821',   '839',   '312',   '333',
     & '351',   '361',   '431',   '492',   '542',   '600',   '631',
     & '730',   '746',   '747',   '920',   '251',   '981',   '998'/
C
      DATA PLNJSP /
     & 'CHLA  ','CADE27','THPL  ','ABCO  ','ABMA  ','ABSH  ','PSME  ',
     & 'TSHE  ','TSME  ','PIAL  ','PIAT  ','PICO  ','PICO3 ','PIFL2 ',
     & 'PIJE  ','PILA  ','PIMO3 ','PIPO  ','PIRA2 ','PISA2 ','JUOC  ',
     & 'PIBR  ','SEGI2 ','TABR2 ','2TE   ','QUAG  ','QUCH2 ','QUDO  ',
     & 'QUEN  ','QUGA4 ','QUKE  ','QULO  ','QUWI2 ','ACMA3 ','AECA  ',
     & 'ALRU2 ','ARME  ','CHCHC4','CONU4 ','FRLA  ','JUGLA ','LIDE3 ',
     & 'PLRA  ','POTR5 ','POBAT ','SALIX ','TOCA  ','UMCA  ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA (NSP(I,1),I=1,MAXSP)/
     &'PC1','IC1','RC1','WF1','RF1','SH1','DF1','WH1','MH1','WB1',
     &'KP1','LP1','CP1','LM1','JP1','SP1','WP1','PP1','MP1','GP1',
     &'WJ1','BR1','GS1','PY1','OS1','LO1','CY1','BL1','EO1','WO1',
     &'BO1','VO1','IO1','BM1','BU1','RA1','MA1','GC1','DG1','FL1',
     &'WN1','TO1','SY1','AS1','CW1','WI1','CN1','CL1','OH1'/
      DATA (NSP(I,2),I=1,MAXSP)/
     &'PC2','IC2','RC2','WF2','RF2','SH2','DF2','WH2','MH2','WB2',
     &'KP2','LP2','CP2','LM2','JP2','SP2','WP2','PP2','MP2','GP2',
     &'WJ2','BR2','GS2','PY2','OS2','LO2','CY2','BL2','EO2','WO2',
     &'BO2','VO2','IO2','BM2','BU2','RA2','MA2','GC2','DG2','FL2',
     &'WN2','TO2','SY2','AS2','CW2','WI2','CN2','CL2','OH2'/
      DATA (NSP(I,3),I=1,MAXSP)/
     &'PC3','IC3','RC3','WF3','RF3','SH3','DF3','WH3','MH3','WB3',
     &'KP3','LP3','CP3','LM3','JP3','SP3','WP3','PP3','MP3','GP3',
     &'WJ3','BR3','GS3','PY3','OS3','LO3','CY3','BL3','EO3','WO3',
     &'BO3','VO3','IO3','BM3','BU3','RA3','MA3','GC3','DG3','FL3',
     &'WN3','TO3','SY3','AS3','CW3','WI3','CN3','CL3','OH3'/
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
C   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
C   AND LARGER.
C
      DATA HT1/
     &   4.7874,   5.2052,   4.7874,   5.2180,   5.2973,   5.2973,
     &   5.3076,   4.7874,   4.7874,   4.7874,   4.6843,   4.8358,
     &   4.7874,   4.7874,   5.1419,   5.3371,   5.2649,   5.3820,
     &   4.7874,   4.6236,   4.7874,   4.7874,   4.7874,   4.7874,
     &   4.7874,   4.6618,   4.6618,   4.6618,   4.6618,   3.8314,
     &   4.4907,   4.6618,   4.6618,   4.6618,   4.6618,   4.6618,
     &   4.4809,   4.6618,   4.6618,   4.6618,   4.6618,   4.6618,
     &   4.6618,   4.6618,   4.6618,   4.6618,   4.6618,   4.6618,
     &   4.6618 /
C
      DATA HT2/
     &  -7.3170, -20.1443,  -7.3170, -14.8682, -17.2042, -17.2042,
     & -14.4740,  -7.3170,  -7.3170,  -7.3170,  -6.5516,  -9.2077,
     &  -7.3170,  -7.3170, -19.8143, -19.3151, -15.5907, -20.4097,
     &  -7.3170, -13.0049,  -7.3170,  -7.3170,  -7.3170,  -7.3170,
     &  -7.3170,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -4.8221,
     &  -7.7030,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312,
     &  -7.5989,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312,
     &  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312,
     &  -8.3312 /
C
C  RESIDUAL ERROR ESTIMATES MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR
C  GIANT SEQUOIA VALUE (#23) MAY BE OFF, NEED TO FIND REG RUN TO SEE
C
      DATA SIGMAR/
     & 0.4936, 0.4936, 0.4936, 0.4391, 0.4112, 0.4112, 0.4791,
     & 0.4687, 0.4687, 0.4169, 0.4392, 0.4169, 0.4392, 0.4392,
     & 0.4458, 0.4687, 0.4745, 0.4458, 0.4458, 0.4392, 0.4392,
     & 0.4391, 0.4408, 0.4392, 0.4458, 0.5998, 0.5998, 0.5998,
     & 0.5998, 0.5998, 0.5998, 0.5998, 0.5998, 0.5998, 0.5998,
     & 0.5998, 0.6608, 0.6608, 0.6608, 0.5998, 0.5998, 0.5166,
     & 0.5998, 0.5998, 0.5998, 0.5998, 0.4392, 0.5998, 0.5998/
C----------
C   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK.
C----------
C   HTT1(ISPC,1)
C
      DATA HTT1/ 49*0.,
C
C   HTT1(ISPC,2)
C
     & 49*0.,
C
C   HTT1(ISPC,3)
C
     & 49*0.,
C
C   HTT1(ISPC,4)
C
     & 49*0.,
C
C   HTT1(ISPC,5)
C
     & 49*0.,
C
C   HTT1(ISPC,6) THRU HTT1(ISPC,9) ARE NOT USED. SET TO 0.0
C
     & 196*0.0/
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
