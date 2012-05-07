      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--PN   DATE OF LAST REVISION:  03/07/12
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
C TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/,
     &     BKRAT/MAXSP*0./
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C COMMON STATEMENT FOR ESCOMN VARIABLE
C----------
      DATA XMIN/ 1.0, 2*1.5, 7*1.0, 1.4, 3*1.0, 1.3, 1.5,
     &           13*1.0, 1.5, 9*1.0/
      DATA ISPSPE/ 17,21,22,23,24,25,26,27,28,33,34,35,36,37/
      DATA HHTMAX/ 21*20.0,50.0,17*20.0 /
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
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/624*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/780*0.0/
C----------
C COMMON STATEMENT FOR PLOT VARIABLES.
C----------
C     SPECIES LIST FOR PACIFIC COAST VARIANT
C
C     1 = PACIFIC SILVER FIR (SF)      ABIES AMABILIS
C     2 = WHITE FIR (WF)               ABIES CONCOLOR
C     3 = GRAND FIR (GF)               ABIES GRANDIS
C     4 = SUBALPINE FIR (AF)           ABIES LASIOCARPA
C     5 = CALIFORNIA RED FIR (RF)/     ABIES MAGNIFICA
C         SHASTA RED FIR
C     6 = SITKA SPRUCE (SS)            PICEA SITCHENSIS
C     7 = NOBLE FIR (NF)               ABIES PROCERA
C     8 = ALASKA CEDAR (YC)/           CALLITROPSIS NOOTKATENSIS
C         WESTERN LARCH                LARIX OCCIDENTALIS
C     9 = INCENSE CEDAR (IC)           LIBOCEDRUS DECURRENS
C    10 = ENGELMANN SPRUCE (ES)        PICEA ENGELMANNII
C    11 = LODGEPOLE PINE (LP)          PINUS CONTORTA
C    12 = JEFFREY PINE (JP)            PINUS JEFFREYI
C    13 = SUGAR PINE (SP)              PINUS LAMBERTIANA
C    14 = WESTERN WHITE PINE (WP)      PINUS MONTICOLA
C    15 = PONDEROSA PINE (PP)          PINUS PONDEROSA
C    16 = DOUGLAS-FIR (DF)             PSEUDOTSUGA MENZIESII
C    17 = COAST REDWOOD (RW)           SEQUOIA SEMPERVIRENS
C    18 = WESTERN REDCEDAR (RC)        THUJA PLICATA
C    19 = WESTERN HEMLOCK (WH)         TSUGA HETEROPHYLLA
C    20 = MOUNTAIN HEMLOCK (MH)        TSUGA MERTENSIANA
C    21 = BIGLEAF MAPLE (BM)           ACER MACROPHYLLUM
C    22 = RED ALDER (RA)               ALNUS RUBRA
C    23 = WHITE ALDER (WA) /           ALNUS RHOMBIFOLIA
C         PACIFIC MADRONE              ARBUTUS MENZIESII
C    24 = PAPER BIRCH (PB)             BETULA PAPYRIFERA
C    25 = GIANT CHINKAPIN (GC) /       CASTANOPSIS CHRYSOPHYLLA
C         TANOAK                       LITHOCARPUS DENSIFLORUS
C    26 = QUAKING ASPEN (AS)           POPULUS TREMULOIDES
C    27 = BLACK COTTONWOOD (CW)        POPULUS TRICHOCARPA
C    28 = OREGON WHITE OAK (WO) /      QUERCUS GARRYANA
C         CALIFORNIA BLACK OAK         QUERCUS KELLOGGII
C    29 = WESTERN JUNIPER (WJ)         JUNIPERUS OCCIDENTALIS
C    30 = SUBALPINE LARCH (LL)         LARIX LYALLII
C    31 = WHITEBARK PINE (WB)          PINUS ALBICAULIS
C    32 = KNOBCONE PINE (KP)           PINUS ATTENUATA
C    33 = PACIFIC YEW (PY)             TAXUS BREVIFOLIA
C    34 = PACIFIC DOGWOOD (DG)         CORNUS NUTTALLII
C    35 = HAWTHORN (HT)                CRATAEGUS sp.
C    36 = BITTER CHERRY (CH)           PRUNUS EMARGINATA
C    37 = WILLOW (WI)                  SALIX sp.
C    38 = ---
C    39 = OTHER (OT)
C----------
      DATA JSP /
     & 'SF ',   'WF ',   'GF ',   'AF ',   'RF ',   'SS ',   'NF ',
     & 'YC ',   'IC ',   'ES ',   'LP ',   'JP ',   'SP ',   'WP ',
     & 'PP ',   'DF ',   'RW ',   'RC ',   'WH ',   'MH ',   'BM ',
     & 'RA ',   'WA ',   'PB ',   'GC ',   'AS ',   'CW ',   'WO ',
     & 'WJ ',   'LL ',   'WB ',   'KP ',   'PY ',   'DG ',   'HT ',
     & 'CH ',   'WI ',   '   ',   'OT '/
C
      DATA FIAJSP /
     & '011',   '015',   '017',   '019',   '020',   '098',   '022',
     & '042',   '081',   '093',   '108',   '116',   '117',   '119',
     & '122',   '202',   '211',   '242',   '263',   '264',   '312',
     & '351',   '352',   '375',   '431',   '746',   '747',   '815',
     & '064',   '072',   '101',   '103',   '231',   '492',   '500',
     & '768',   '920',   '   ',   '999'/
C
      DATA PLNJSP /
     & 'ABAM  ','ABCO  ','ABGR  ','ABLA  ','ABMA  ','PISI  ','ABPR  ',
     & 'CANO9 ','CADE27','PIEN  ','PICO  ','PIJE  ','PILA  ','PIMO3 ',
     & 'PIPO  ','PSME  ','SESE3 ','THPL  ','TSHE  ','TSME  ','ACMA3 ',
     & 'ALRU2 ','ALRH2 ','BEPA  ','CHCHC4','POTR5 ','POBAT ','QUGA4 ',
     & 'JUOC  ','LALY  ','PIAL  ','PIAT  ','TABR2 ','CONU4 ','CRATA ',
     & 'PREM  ','SALIX ','      ','2TREE '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /  'SF1','WF1','GF1','AF1','RF1','SS1','NF1','YC1',
     &'IC1','ES1','LP1','JP1','SP1','WP1','PP1','DF1','RW1','RC1',
     &'WH1','MH1','BM1','RA1','WA1','PB1','GC1','AS1','CW1','WO1','WJ1',
     &'LL1','WB1','KP1','PY1','DG1','HT1','CH1','WI1','__1','OT1',
     &            'SF2','WF2','GF2','AF2','RF2','SS2','NF2','YC2',
     &'IC2','ES2','LP2','JP2','SP2','WP2','PP2','DF2','RW2','RC2',
     &'WH2','MH2','BM2','RA2','WA2','PB2','GC2','AS2','CW2','WO2','WJ2',
     &'LL2','WB2','KP2','PY2','DG2','HT2','CH2','WI2','__2','OT2',
     &            'SF3','WF3','GF3','AF3','RF3','SS3','NF3','YC3',
     &'IC3','ES3','LP3','JP3','SP3','WP3','PP3','DF3','RW3','RC3',
     &'WH3','MH3','BM3','RA3','WA3','PB3','GC3','AS3','CW3','WO3','WJ3',
     &'LL3','WB3','KP3','PY3','DG3','HT3','CH3','WI3','__3','OT3'/
C----------
C COMMON STATEMENT FOR COEFFS VARIABLES
C----------
C   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
C   AND LARGER.
C----------
      DATA HT1/
     & 5.487, 2*5.308, 2*5.313, 5.517, 5.327, 5.143, 2*5.188, 4.865,
     & 5.333, 2*5.382, 5.333, 5.563, 5.188, 5.233, 5.355, 5.081,
     & 4.700, 4.875, 7*5.152, 4*5.188, 6*5.152/
C
      DATA HT2/
     & -16.701, 2*-13.624, 2*-15.321, -17.944, -15.450, -13.497,
     & 2*-13.801, -9.305, -17.762, 2*-15.866, -17.762, -16.475,
     & -13.801, -14.737, -13.878, -13.430, -6.326, -8.639,
     & 7*-13.576, 4*-13.801, 6*-13.576/
C
C  SIGMAR VALUES FOR SF,SS,DF,RC,WH,RA MULTIPLIED BY .75
C  TO CORRECT FOR BIAS IN THE FITTING PROCEDURE.
C  **REFIT CHANGES SPCS SS,DF,RC,WH (1,6,16,18,19) DMD 060396
C  **REFIT OF WO(28) by GOULD&HARRINGTON ESM 041910
C
      DATA SIGMAR/
     & 0.3428, 2*0.4390, 0.3960, 0.3102, 0.3769, 0.4275,
     & 0.3931, 2*0.4842, 0.3690, 0.3222, 2*0.5494, 0.3222,
     & 0.2679, 0.4842, 0.3625, 0.3402, 0.3751, 0.5107,
     & 0.3328, 5*0.5357, 0.236, 0.5357, 4*0.4842, 6*0.5357/
C----------
C DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK.
C----------
C   HTT1 IS USED TO STORE THE HEIGHT DUBBING COEFFICIENTS FOR TREES
C   LESS THAN 5.0" DBH.
C
      DATA HTT1/
C
C   HTT1(ISPC,1) IS USED TO STORE THE CONSTANT COEFFICIENT.
C
     & 1.3134, 2*1.4769, 1.4261, 2*1.3526, 1.7100,
     & 3*1.5907, 0.9717, 1.0756, 2*0.9717, 1.0756, 7.1391, 1.5907,
     & 2.3115, 1.3608, 1.2278, 9*0.0994, 4*1.5907, 6*0.0994,
C
C   HTT1(ISPC,2) IS USED TO STORE THE DBH COEFFICIENT.
C
     & 0.3432, 2*0.3579, 0.3334, 2*0.3335, 0.2943, 3*0.3040,
     & 0.3934, 0.4369, 2*0.3934, 0.4369, 4.2891, 0.3040, 0.2370,
     & 0.6151, 0.4000, 9*4.9767, 4*0.3040, 6*4.9767,
C
C   HTT1(ISPC,3) IS USED TO STORE THE CR COEFFICIENT.
C
     & 0.0366, 3*0.0, 2*0.0367, 0.0, 3*0.0, 0.0339, 0.0,
     & 2*0.0339, 0.0, -0.7150, 0.0, -0.0556, 21*0.0,
C
C   HTT1(ISPC,4) IS USED TO STORE THE DBH SQUARED COEFFICIENT.
C
     & 15*0.0, 0.2750, 2*0.0, -0.0442, 20*0.0,
C
C   HTT1(ISPC,5) IS USED TO STORE THE DUMMY VARIABLE FOR
C   MANAGED/UNMANAGED STANDS.
C
     & 6*0.0, 0.1054, 3*0.0, 0.3044, 0.0, 2*0.3044, 0.0,
     & 2.0393, 0.0, 0.3218, 0.0829, 20*0.0,
C
C   HTT1(ISPC,6) THRU HTT1(ISPC,9) ARE NOT USED. SET TO 0.0
C
     & 156*0.0/
C
      DATA BB0/
     &  0.0071839, -0.30935  , -0.30935  ,  2.75780 ,  0.0      ,
     & -0.2050542, -564.38   ,  0.6192   ,128.8952205,  2.75780 ,
     & -0.0968   ,128.8952205, -4.62536  , -4.62536  ,128.8952205,
     & -0.954038 ,  0.6192   , -0.2050542, -1.7307   , 22.8741   ,
     &  0.6192   , 59.5864   ,  0.6192   ,  0.6192   ,  0.6192   ,
     &  0.6192   ,  0.6192   , -0.954038  ,  0.6192   ,  0.0      ,
     &  0.6192   ,  0.6192   ,  0.6192   ,  0.6192   ,  0.6192   ,
     &  0.6192   ,  0.6192   ,  0.6192   ,  0.6192    /
C
      DATA BB1/
     &  0.0000571,  1.2383   ,  1.2383   ,  0.83312 ,  1.51744  ,
     &  1.449615 , 22.25     , -5.3394   , -0.016959 ,  0.83312 ,
     &  0.02679  , -0.016959 ,  1.346399 ,  1.346399 , -0.016959 ,
     &  0.109757 , -5.3394   ,  1.449615 ,  0.1394   ,  0.950234 ,
     & -5.3394   ,  0.7953   , -5.3394   , -5.3394   , -5.3394   ,
     & -5.3394   , -5.3394   , 0.109757      , -5.3394   ,  1.46897  ,
     & -5.3394   , -5.3394   , -5.3394   , -5.3394   , -5.3394   ,
     & -5.3394   , -5.3394   , -5.3394   , -5.3394    /
C
      DATA BB2/
     &  1.39005  ,  0.001762 ,  0.001762 ,  0.015701 ,  1.4151E-6,
     &-0.01780992,  0.04995  , 240.29    ,  1.23114  ,  0.015701 ,
     & -9.309E-5 ,  1.23114  ,-135.354483,-135.354483,  1.23114  ,
     & 5.58178E-2, 240.29    ,-0.01780992, -0.0616   , -2.06465E-3,
     & 240.29    ,  0.00194  , 240.29    , 240.29    , 240.29    ,
     & 240.29    , 240.29    ,5.58178E-2 , 240.29    ,  0.0092466,
     & 240.29    , 240.29    , 240.29    , 240.29    , 240.29    ,
     & 240.29    , 240.29    , 240.29    , 240.29     /
C
       DATA BB3/
     &  0.0      , -5.4E-6   , -5.4E-6   , 22.71944  , -0.0440853,
     &6.519748E-5,  6.80     , 3368.9    , -0.7864   , 22.71944  ,
     &  0.0      , -0.7864   ,  0.0      ,  0.0      , -0.7864   ,
     & 7.92236E-3, 3368.9    ,6.519748E-5,  0.0137   ,  0.5      ,
     & 3368.9    , -0.0007403, 3368.9    , 3368.9    , 3368.9    ,
     & 3368.9    , 3368.9    ,7.92236E-3 , 3368.9    , -2.3957E-4,
     & 3368.9    , 3368.9    , 3368.9    , 3368.9    , 3368.9    ,
     & 3368.9    , 3368.9    , 3368.9    , 3368.9     /
C
       DATA BB4/
     &  0.0        ,  2.046E-7 ,  2.046E-7   , -0.63557  , -3.0495E6 ,
     &-1.095593E-23, 2843.21   ,  0.0        ,  2.49717  , -0.63557  ,
     &  0.0        ,  2.49717  ,  0.0        ,  0.0      ,  2.49717  ,
     &-7.33819E-4  ,  0.0      ,-1.095593E-23,  0.00192  ,  1.365566 ,
     &  0.0        ,  0.9198   ,  0.0        ,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,-7.33819E-4  ,  0.0      ,  1.1122E-6,
     &  0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,  0.0        ,  0.0       /
C
      DATA BB5/
     &  0.0      , -4.04E-13 , -4.04E-13 ,  0.0      , 5.72474E-4,
     & -5.611879 , 34735.54  ,  0.0      , -0.0045042,  0.0      ,
     &  0.0      , -0.0045042,  0.0      ,  0.0      , -0.0045042,
     & 1.97693E-4,  0.0      , -5.611879 ,  0.00007  ,  2.045963 ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,1.97693E-4 ,  0.0      , -0.12528,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0       /
C
      DATA BB6/
     &  0.0      , -6.2056   , -6.2056   ,  0.0      ,  0.0      ,
     &  2.418604 ,  0.0      ,  0.0      ,  0.33022  ,  0.0      ,
     &  0.0      ,  0.33022  ,  0.0      ,  0.0      ,  0.33022  ,
     &  0.0      ,  0.0      ,  2.418604 ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.039636 ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0       /
C
      DATA BB7/
     &  0.0      ,  2.097    ,  2.097    ,  0.0      ,  0.0      ,
     & -0.2593110,  0.0      ,  0.0      ,100.43     ,  0.0      ,
     &  0.0      ,100.43     ,  0.0      ,  0.0      ,100.43     ,
     &  0.0      ,  0.0      , -0.2593110,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      , -4.278E-4 ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0       /
C
      DATA BB8/
     &  0.0      , -0.09411  , -0.09411  ,  0.0      ,  0.0      ,
     &1.351445E-4,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,1.351445E-4,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  1.7039E-6,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0       /
C
       DATA BB9/
     &  0.0        , -4.382E-5 , -4.382E-5   ,  0.0      ,  0.0      ,
     &-1.701139E-12,  0.0      ,  0.0        ,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,-1.701139E-12,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,  0.0        ,  0.0      , 73.57     ,
     &  0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      ,
     &  0.0        ,  0.0      ,  0.0        ,  0.0       /
C
      DATA BB10/
     &  0.0       ,  2.007E-11,  2.007E-11 ,  0.0      ,  0.0      ,
     &7.964197E-27,  0.0      ,  0.0       ,  0.0      ,  0.0      ,
     &  0.0       ,  0.0      ,  0.0       ,  0.0      ,  0.0      ,
     &  0.0       ,  0.0      ,7.964197E-27,  0.0      ,  0.0      ,
     &  0.0       ,  0.0      ,  0.0       ,  0.0      ,  0.0      ,
     &  0.0       ,  0.0      ,  0.0       ,  0.0      , -0.12528,
     &  0.0       ,  0.0      ,  0.0       ,  0.0      ,  0.0      ,
     &  0.0       ,  0.0      ,  0.0       ,  0.0       /
C
      DATA BB11/
     &  0.0      ,  2.054E-17,  2.054E-17,  0.0      ,  0.0      ,
     &-86.43     ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,-86.43     ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.039636,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0       /
C
      DATA BB12/
     &  0.0      ,-84.93     ,-84.93     ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      , -4.278E-4 ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      ,
     &  0.0      ,  0.0      ,  0.0      ,  0.0       /
C
      DATA BB13/
     & 29*0.0, 1.7039E-6,  9*0.0/
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
