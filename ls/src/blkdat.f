      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--LS  DATE OF LAST REVISION:  12/20/11
C----------
C
C     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
C
C     COMMON STATEMENT FOR MODEL COEFFICIENTS WHICH ARE HABITAT
C     AND SITE DEPENDENT.
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
COMMONS
C----------
C
      INTEGER I,J
C----------
C  SPECIES SPECIFIC BARK RATIOS.  FROM RAILE, G.K. 1982. ESTIMATING
C  STUMP VOLUME. RES. PAP. NC-224, 4 P.
C----------
      DATA BKRAT/5*.91,2*.95,3*.94,.95,3*.91,2*.94,.93,3*.94,3*.93,
     &           .94,.92,3*.94,.92,4*.91,3*.92,3*.94,3*.92,6*.94,
     &           20*.92/
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,
     >2I3,2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/15,2,3,16,4,8 /
C----------
C   COMMON STATEMENT FOR ESCOMN VARIABLE
C----------
C   HHTMAX, IFORST, OCURHT, AND OCURNF HAVE ALL BEEN SET TO ONE
C   VALUE FOR THE LS VARIANT.
C
      DATA XMIN/
     &0.33, 0.33, 0.25, 0.25, 0.33, 0.25, 0.25, 0.33, 0.25, 0.50,
     &0.33, 0.25, 0.25, 0.33, 0.33, 0.42, 0.42, 0.42, 1.00, 0.42,
     &0.33, 0.33, 0.50, 0.42, 0.33, 0.25, 0.25, 0.25, 0.42, 0.33,
     &0.33, 0.25, 0.33, 0.42, 0.33, 1.40, 0.50, 0.33, 0.33, 0.42,
     &0.42, 0.42, 0.42, 0.50, 0.33, 0.33, 0.42, 0.58, 0.33, 0.33,
     &0.25, 2.10, 0.42, 0.50, 0.25, 0.25, 0.25, 0.42, 0.33, 0.58,
     &0.33, 2.10, 0.33, 4.70, 1.00, 2.10, 0.50, 2.10/
C
      DATA HHTMAX/
     &14., 20., 18., 18., 20., 18., 18., 20., 16., 24.,
     &16., 16., 16., 16., 18., 24., 24., 18., 20., 26.,
     &16., 12., 20., 22., 16., 16., 16., 14., 24., 16.,
     &16., 14., 12., 20., 16., 20., 20., 14., 14., 20.,
     &20., 24., 18., 20., 18., 20., 20., 24., 10., 16.,
     &18., 20., 20., 20., 12., 18., 16., 20., 16., 24.,
     &30., 20., 20., 20., 32., 20., 18., 20./
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,IFORST/20*1/,
     &  IFORCD/902,903,904,906,907,909,910,913,12*0/,
     &  ISPSPE/15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
     &         33,34,35,36,37,38,39,40,41,42,43,45,46,47,48,50,51,52,
     &         53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C

      DATA ((OCURHT(I,J),I=1,16),J=1,68)/1088*1.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,68)/1360*1.0/
C----------
C  COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'JP ',   'SC ',   'RN ',   'RP ',   'WP ',   'WS ',   'NS ',
     & 'BF ',   'BS ',   'TA ',   'WC ',   'EH ',   'OS ',   'RC ',
     & 'BA ',   'GA ',   'EC ',   'SV ',   'RM ',   'BC ',   'AE ',
     & 'RL ',   'RE ',   'YB ',   'BW ',   'SM ',   'BM ',   'AB ',
     & 'WA ',   'WO ',   'SW ',   'BR ',   'CK ',   'RO ',   'BO ',
     & 'NP ',   'BH ',   'PH ',   'SH ',   'BT ',   'QA ',   'BP ',
     & 'PB ',   'CH ',   'BN ',   'WN ',   'HH ',   'BK ',   'NC ',
     & 'BE ',   'ST ',   'MM ',   'AH ',   'AC ',   'HK ',   'DW ',
     & 'HT ',   'AP ',   'BG ',   'SY ',   'PR ',   'CC ',   'PL ',
     & 'WI ',   'BL ',   'DM ',   'SS ',   'MA '/
C
      DATA FIAJSP /
     & '105',   '130',   '125',   '125',   '129',   '094',   '091',
     & '012',   '095',   '071',   '241',   '261',   '298',   '068',
     & '543',   '544',   '742',   '317',   '316',   '762',   '972',
     & '975',   '977',   '371',   '951',   '318',   '314',   '531',
     & '541',   '802',   '804',   '823',   '826',   '833',   '837',
     & '809',   '402',   '403',   '407',   '743',   '746',   '741',
     & '375',   '---',   '601',   '602',   '701',   '901',   '---',
     & '313',   '315',   '319',   '391',   '421',   '462',   '491',
     & '500',   '660',   '693',   '731',   '761',   '763',   '760',
     & '920',   '922',   '923',   '931',   '935'/
C
      DATA PLNJSP /
     & 'PIBA2 ','PISY  ','PIRE  ','PIRE  ','PIST  ','PIGL  ','PIAB  ',
     & 'ABBA  ','PIMA  ','LALA  ','THOC2 ','TSCA  ','2TE   ','JUVI  ',
     & 'FRNI  ','FRPE  ','PODE3 ','ACSA2 ','ACRU  ','PRSE2 ','ULAM  ',
     & 'ULRU  ','ULTH  ','BEAL2 ','TIAM  ','ACSA3 ','ACNI5 ','FAGR  ',
     & 'FRAM2 ','QUAL  ','QUBI  ','QUMA2 ','QUMU  ','QURU  ','QUVE  ',
     & 'QUEL  ','CACO15','CAGL8 ','CAOV2 ','POGR4 ','POTR5 ','POBA2 ',
     & 'BEPA  ','------','JUCI  ','JUNI  ','OSVI  ','ROPS  ','------',
     & 'ACNE2 ','ACPE  ','ACSP2 ','CACA18','CADE12','CEOC  ','COFL2 ',
     & 'CRATA ','MALUS ','NYSY  ','PLOC  ','PRPE2 ','PRVI  ','PRUNU ',
     & 'SALIX ','SANI  ','SAERF ','SAAL5 ','SOAM3 '/
C
      DATA JTYPE /122*0/
C
      DATA NSP /'JP1','SC1','RN1','RP1','WP1','WS1','NS1','BF1','BS1',
     &  'TA1','WC1','EH1','OS1','RC1','BA1','GA1','EC1','SV1',
     &  'RM1','BC1','AE1','RL1','RE1','YB1','BW1','SM1','BM1','AB1',
     &  'WA1','WO1','SW1','BR1','CK1','RO1','BO1','NP1','BH1','PH1',
     &  'SH1','BT1','QA1','BP1','PB1','CH1','BN1','WN1','HH1','BK1',
     &  'NC1','BE1','ST1','MM1','AH1','AC1','HK1','DW1','HT1','AP1',
     &  'BG1','SY1','PR1','CC1','PL1','WI1','BL1','DM1','SS1','MA1',
     &  'JP2','SC2','RN2','RP2','WP2','WS2','NS2','BF2','BS2',
     &  'TA2','WC2','EH2','OS2','RC2','BA2','GA2','EC2','SV2',
     &  'RM2','BC2','AE2','RL2','RE2','YB2','BW2','SM2','BM2','AB2',
     &  'WA2','WO2','SW2','BR2','CK2','RO2','BO2','NP2','BH2','PH2',
     &  'SH2','BT2','QA2','BP2','PB2','CH2','BN2','WN2','HH2','BK2',
     &  'NC2','BE2','ST2','MM2','AH2','AC2','HK2','DW2','HT2','AP2',
     &  'BG2','SY2','PR2','CC2','PL2','WI2','BL2','DM2','SS2','MA2',
     &  'JP3','SC3','RN3','RP3','WP3','WS3','NS3','BF3','BS3',
     &  'TA3','WC3','EH3','OS3','RC3','BA3','GA3','EC3','SV3',
     &  'RM3','BC3','AE3','RL3','RE3','YB3','BW3','SM3','BM3','AB3',
     &  'WA3','WO3','SW3','BR3','CK3','RO3','BO3','NP3','BH3','PH3',
     &  'SH3','BT3','QA3','BP3','PB3','CH3','BN3','WN3','HH3','BK3',
     &  'NC3','BE3','ST3','MM3','AH3','AC3','HK3','DW3','HT3','AP3',
     &  'BG3','SY3','PR3','CC3','PL3','WI3','BL3','DM3','SS3','MA3'/
C----------
C=================================
C     SPECIES LIST FOR LAKE STATES
C=================================
C     1 = JACK PINE (JP)
C     2 = SCOTCH PINE (SC)
C     3 = RED PINE NATURAL (RN)
C     4 = RED PINE PLANTATION (RP)
C     5 = EASTERN WHITE PINE (WP)
C     6 = WHITE SPRUCE (WS)
C     7 = NORWAY SPRUCE (NS)
C     8 = BALSAM FIR (BF)
C     9 = BLACK SPRUCE (BS)
C    10 = TAMARACK (TA)
C    11 = NORTHERN WHITE-CEDAR (WC)
C    12 = EASTERN HEMLOCK (EH)
C    13 = OTHER SOFTWOOD SPECIES (OS)
C    14 = EASTERN REDCEDAR (RC)
C    15 = BLACK ASH (BA)
C    16 = GREEN ASH (GA)
C    17 = EASTERN COTTONWOOD (EC)
C    18 = SILVER MAPLE (SV)
C    19 = RED MAPLE (RM)
C    20 = BLACK CHERRY (BC)
C    21 = AMERICAN ELM (AE)
C    22 = SLIPPERY ELM (RL)
C    23 = ROCK ELM (RE)
C    24 = YELLOW BIRCH (YB)
C    25 = AMERICAN BASSWOOD (BW)
C    26 = SUGAR MAPLE (SM)
C    27 = BLACK MAPLE (BM)
C    28 = AMERICAN BEECH (AB)
C    29 = WHITE ASH (WA)
C    30 = WHITE OAK (WO)
C    31 = SWAMP WHITE OAK (SW)
C    32 = BUR OAK (BR)
C    33 = CHINKAPIN OAK (CK)
C    34 = NORTHERN RED OAK (RO)
C    35 = BLACK OAK (BO)
C    36 = NORTHERN PIN OAK (NP)
C    37 = BITTERNUT HICKORY (BH)
C    38 = PIGNUT HICKORY (PH)
C    39 = SHAGBARK HICKORY (SH)
C    40 = BIGTOOTH ASPEN (BT)
C    41 = QUAKING ASPEN (QA)
C    42 = BALSAM POPLAR (BP)
C    43 = PAPER BIRCH (PB)
C    44 = COMMERCIAL HARDWOOD SPECIES (CH)
C    45 = BUTTERNUT (BN)
C    46 = BLACK WALNUT (WN)
C    47 = EASTERN HOPHORNBEAM (HH)
C    48 = BLACK LOCUST (BK)
C    49 = NON-COMMERCIAL HARDWOOD SPECIES (NC)
C    50 = BOXELDER (BE)
C    51 = STRIPED MAPLE (ST)
C    52 = MOUNTAIN MAPLE (MM)
C    53 = AMERICAN HORNBEAM (AH)
C    54 = AMERICAN CHESTNUT (AC)
C    55 = HACKBERRY (HK)
C    56 = FLOWERING DOGWOOD (DW)
C    57 = HAWTHORN SPECIES(HT)
C    58 = APPLE SPECIES (AP)
C    59 = BLACKGUM (BG)
C    60 = SYCAMORE (SY)
C    61 = PIN CHERRY (PR)
C    62 = CHOKECHERRY (CC)
C    63 = PLUMS, CHERRIES (PL)
C    64 = WILLOW SPECIES(WI)
C    65 = BLACK WILLOW (BL)
C    66 = DIAMOND WILLOW (DM)
C    67 = SASSAFRAS (SS)
C    68 = AMERICAN MOUNTAIN ASH (MA)
C----------
C  SIGMAR ARE THE ERROR TERMS FOR DIAMETER GROWTH.  THESE COEFFICIENTS
C  ARE THE ROOT MEAN SQUARE ERROR FOR EACH SPECIES.
C----------
      DATA SIGMAR/2*.205,2*.270,.161,2*.197,.149,.080,.108,.128,.118,
     &            2*.205,2*.145,4*.178,3*.234,.128,.201,3*.240,.186,
     &            4*.105,.130,2*.114,3*.140,.153,2*.211,.156,25*.23/
C----------
C     DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK IN THE
C     FOLLOWING DATA STATEMENTS
C----------
      DATA HT1/
     &  4.5084,   4.5457,   4.5084,   4.5084,   4.6090,   4.5084,
     &  4.5084,   4.5084,   4.5084,   4.5084,   4.5084,   4.5084,
     &  4.0374,   4.4718,   4.6155,   4.6155,   4.9396,   4.5991,
     &  4.3379,   4.3286,   4.6008,   4.6238,   4.3744,   4.4388,
     &  4.5820,   4.4834,   4.4834,   4.4772,   4.5959,   4.5463,
     &  4.7342,   4.5225,   4.3420,   4.5202,   4.4747,   4.5225,
     &  4.5128,   4.5128,   4.5128,   4.9396,   4.5128,   4.5959,
     &  4.4388,   4.4834,   4.5018,   4.5018,   4.0322,   4.4299,
     &  4.4207,   4.5018,   4.4207,   4.4207,   4.0322,   4.9396,
     &  4.4207,   3.7301,   4.4207,   3.9678,   4.3802,   4.6355,
     &  4.4207,   4.4207,   3.9678,   4.4911,   4.4911,   4.4207,
     &  4.3383,   4.4207/
C
      DATA HT2/
     & -6.0116,  -6.8000,  -6.0116,  -6.0116,  -6.1896,  -6.0116,
     & -6.0116,  -6.0116,  -6.0116,  -6.0116,  -6.0116,  -6.0116,
     & -4.2964,  -5.0078,  -6.2945,  -6.2945,  -8.1838,  -6.6706,
     & -3.8214,  -4.0922,  -7.2732,  -7.4847,  -4.5257,  -4.0872,
     & -5.0903,  -4.5431,  -4.5431,  -4.7206,  -6.4497,  -5.2287,
     & -6.2674,  -4.9401,  -5.1193,  -4.8896,  -4.8698,  -4.9401,
     & -4.9918,  -4.9918,  -4.9918,  -8.1838,  -4.9918,  -6.4497,
     & -4.0872,  -4.5431,  -5.6123,  -5.6123,  -3.0833,  -4.9920,
     & -5.1435,  -5.6123,  -5.1435,  -5.1435,  -3.0833,  -8.1838,
     & -5.1435,  -2.7758,  -5.1435,  -3.2510,  -4.7903,  -5.2776,
     & -5.1435,  -5.1435,  -3.2510,  -5.7928,  -5.7928,  -5.1435,
     & -4.5018,  -5.1435/
C
C
      DATA BB0/68*0.0/
      DATA BB1/68*0.0/
      DATA BB2/68*0.0/
      DATA BB3/68*0.0/
      DATA BB4/68*0.0/
      DATA BB5/68*0.0/
      DATA BB6/68*0.0/
      DATA BB7/68*0.0/
      DATA BB8/68*0.0/
      DATA BB9/68*0.0/
      DATA BB10/68*0.0/
      DATA BB11/68*0.0/
      DATA BB12/68*0.0/
      DATA BB13/68*0.0/
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
