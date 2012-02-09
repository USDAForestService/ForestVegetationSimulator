      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--CS  DATE OF LAST REVISION: 12/20/11
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
C
COMMONS
C----------
      INTEGER I,J
C----------
C
      DATA BKRAT/2*.95,3*.93,4*.91,4*.95,11*.93,3*.95,.91,4*.95,8*.93,
     &          .95,6*.93,3*.91,34*.93,12*.95/
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
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
     &0.33, 2.10, 0.25, 0.42, 0.25, 0.25, 0.33, 0.33, 0.33, 0.33,
     &3.59, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33,
     &0.33, 0.33, 0.33, 0.25, 0.33, 0.42, 0.50, 0.42, 1.00, 0.33,
     &0.42, 0.42, 0.33, 0.33, 0.25, 0.50, 0.33, 0.50, 0.33, 0.50,
     &0.42, 0.33, 0.25, 0.42, 0.42, 0.42, 0.33, 0.42, 0.33, 0.33,
     &0.33, 2.80, 0.33, 0.33, 0.25, 0.33, 0.25, 2.80, 0.33, 1.40,
     &0.33, 0.25, 0.50, 0.33, 1.40, 0.25, 0.50, 1.40, 0.50, 0.55,
     &0.63, 0.25, 5.00, 0.42, 0.42, 0.42, 0.58, 1.40, 0.58, 1.40,
     &0.33, 0.33, 4.70, 1.00, 0.33, 0.42, 2.10, 0.25, 0.25, 0.50,
     &0.25, 0.33, 0.42, 2.10, 0.42, 0.33/
C
      DATA HHTMAX/
     &16., 27., 14., 14., 14., 16., 20., 20., 18., 16.,
     &20., 20., 16., 14., 14., 14., 18., 14., 14., 14.,
     &14., 14., 14., 14., 18., 28., 20., 24., 20., 16.,
     &18., 26., 16., 14., 12., 20., 16., 20., 12., 20.,
     &24., 16., 16., 24., 24., 24., 16., 20., 16., 16.,
     &16., 20., 12., 16., 14., 12., 12., 20., 16., 20.,
     &14., 14., 20., 16., 20., 14., 20., 20., 18., 20.,
     &20., 12., 20., 24., 20., 20., 24., 20., 24., 20.,
     &18., 18., 20., 32., 10., 20., 20., 18., 16., 20.,
     &12., 20., 20., 20., 20., 16./
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,IFORST/20*1/,
     &  IFORCD/905,908,911,17*0/,
     &  ISPSPE/8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     &    27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
     &    46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,
     &    65,66,67,69,70,71,72,73,74,75,76,77,79,80,81,82,83,84,86,
     &    87,88,89,90,91,92,93,94,95,96/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/1536*1.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/1920*1.0/
C----------
C  COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'RC ',   'JU ',   'SP ',   'VP ',   'LP ',   'OS ',   'WP ',
     & 'WN ',   'BN ',   'TL ',   'TS ',   'WT ',   'BG ',   'HS ',
     & 'SH ',   'SL ',   'MH ',   'PH ',   'HI ',   'WH ',   'BH ',
     & 'PE ',   'BI ',   'AB ',   'BA ',   'PA ',   'UA ',   'EC ',
     & 'RM ',   'BE ',   'SV ',   'BC ',   'AE ',   'SG ',   'HK ',
     & 'WE ',   'EL ',   'SI ',   'RL ',   'RE ',   'YP ',   'BW ',
     & 'SM ',   'AS ',   'WA ',   'GA ',   'WO ',   'RO ',   'SK ',
     & 'BO ',   'SO ',   'BJ ',   'CK ',   'SW ',   'BR ',   'SN ',
     & 'PO ',   'DO ',   'CO ',   'PN ',   'CB ',   'QI ',   'OV ',
     & 'WK ',   'NK ',   'WL ',   'QS ',   'UH ',   'SS ',   'OB ',
     & 'CA ',   'PS ',   'HL ',   'BP ',   'BT ',   'QA ',   'BK ',
     & 'OL ',   'SY ',   'BY ',   'RB ',   'SU ',   'WI ',   'BL ',
     & 'NC ',   'AH ',   'RD ',   'DW ',   'HT ',   'KC ',   'OO ',
     & 'CT ',   'MV ',   'MB ',   'HH ',   'SD '/
C
      DATA FIAJSP /
     & '068',   '057',   '110',   '132',   '131',   '298',   '129',
     & '602',   '601',   '690',   '694',   '691',   '693',   '---',
     & '407',   '405',   '409',   '403',   '400',   '401',   '402',
     & '404',   '408',   '531',   '543',   '545',   '546',   '742',
     & '316',   '313',   '317',   '762',   '972',   '461',   '462',
     & '971',   '970',   '974',   '975',   '977',   '621',   '951',
     & '318',   '540',   '541',   '544',   '802',   '833',   '812',
     & '837',   '806',   '824',   '826',   '804',   '823',   '825',
     & '835',   '836',   '832',   '830',   '813',   '817',   '822',
     & '827',   '828',   '831',   '834',   '---',   '931',   '331',
     & '450',   '521',   '552',   '741',   '743',   '746',   '901',
     & '---',   '731',   '221',   '373',   '611',   '920',   '922',
     & '---',   '391',   '471',   '491',   '500',   '571',   '641',
     & '651',   '653',   '680',   '701',   '711'/
C
      DATA PLNJSP /
     &'JUVI  ','JUNIP ','PIEC2 ','PIVI2 ','PITA  ','2TE   ','PIST  ',
     &'JUNI  ','JUCI  ','NYSSA ','NYBI  ','NYAQ2 ','NYSY  ','------',
     &'CAOV2 ','CALA21','CAAL27','CAGL8 ','CARYA ','CAAQ2 ','CACO15',
     &'CAIL2 ','CATE9 ','FAGR  ','FRNI  ','FRPR  ','FRQU  ','PODE3 ',
     &'ACRU  ','ACNE2 ','ACSA2 ','PRSE2 ','ULAM  ','CELA  ','CEOC  ',
     &'ULAL  ','ULMUS ','ULPU  ','ULRU  ','ULTH  ','LITU  ','TIAM  ',
     &'ACSA3 ','FRAXI ','FRAM2 ','FRPE  ','QUAL  ','QURU  ','QUFA  ',
     &'QUVE  ','QUCO2 ','QUMA3 ','QUMU  ','QUBI  ','QUMA2 ','QUMI  ',
     &'QUST  ','QUSI2 ','QUPR2 ','QUPA2 ','QUPA5 ','QUIM  ','QULY  ',
     &'QUNI  ','QUNU  ','QUPH  ','QUSH  ','------','SAAL5 ','AEGL  ',
     &'CATAL ','DIVI5 ','GLTR  ','POBA2 ','POGR4 ','POTR5 ','ROPS  ',
     &'------','PLOC  ','TADI2 ','BENI  ','LIST2 ','SALIX ','SANI  ',
     &'------','CACA18','CECA4 ','COFL2 ','CRATA ','GYDI  ','MAPO  ',
     &'MAAC  ','MAVI2 ','MORUS ','OSVI  ','OXAR  '/
C
      DATA JTYPE /122*0/
C
      DATA NSP /'RC1','JU1','SP1','VP1','LP1','OS1','WP1','WN1','BN1',
     > 'TL1','TS1','WT1','BG1','HS1','SH1','SL1','MH1','PH1','HI1',
     > 'WH1','BH1','PE1','BI1','AB1','BA1','PA1','UA1','EC1','RM1','BE1'
     >,'SV1','BC1','AE1','SG1','HK1','WE1','EL1','SI1','RL1','RE1',
     > 'YP1','BW1','SM1','AS1','WA1','GA1','WO1','RO1','SK1','BO1',
     > 'SO1','BJ1','CK1','SW1','BR1','SN1','PO1','DO1','CO1','PN1',
     > 'CB1','QI1','OV1','WK1','NK1','WL1','QS1','UH1','SS1','OB1',
     > 'CA1','PS1','HL1','BP1','BT1','QA1','BK1','OL1','SY1','BY1',
     > 'RB1','SU1','WI1','BL1','NC1','AH1','RD1','DW1','HT1','KC1',
     > 'OO1','CT1','MV1','MB1','HH1','SD1','RC2','JU2','SP2','VP2',
     > 'LP2','OS2','WP2','WN2','BN2','TL2','TS2','WT2','BG2','HS2',
     > 'SH2','SL2','MH2','PH2','HI2','WH2','BH2','PE2','BI2','AB2',
     > 'BA2','PA2','UA2','EC2','RM2','BE2','SV2','BC2','AE2','SG2',
     > 'HK2','WE2','EL2','SI2','RL2','RE2','YP2','BW2','SM2','AS2',
     > 'WA2','GA2','WO2','RO2','SK2','BO2','SO2','BJ2','CK2','SW2',
     > 'BR2','SN2','PO2','DO2','CO2','PN2','CB2','QI2','OV2','WK2',
     > 'NK2','WL2','QS2','UH2','SS2','OB2','CA2','PS2','HL2','BP2',
     > 'BT2','QA2','BK2','OL2','SY2','BY2','RB2','SU2','WI2','BL2',
     > 'NC2','AH2','RD2','DW2','HT2','KC2','OO2','CT2','MV2','MB2',
     > 'HH2','SD2','RC3','JU3','SP3','VP3','LP3','OS3','WP3','WN3',
     > 'BN3','TL3','TS3','WT3','BG3','HS3','SH3','SL3','MH3','PH3',
     > 'HI3','WH3','BH3','PE3','BI3','AB3','BA3','PA3','UA3','EC3',
     > 'RM3','BE3','SV3','BC3','AE3','SG3','HK3','WE3','EL3','SI3',
     > 'RL3','RE3','YP3','BW3','SM3','AS3','WA3','GA3','WO3','RO3',
     > 'SK3','BO3','SO3','BJ3','CK3','SW3','BR3','SN3','PO3','DO3',
     > 'CO3','PN3','CB3','QI3','OV3','WK3','NK3','WL3','QS3','UH3',
     > 'SS3','OB3','CA3','PS3','HL3','BP3','BT3','QA3','BK3','OL3',
     > 'SY3','BY3','RB3','SU3','WI3','BL3','NC3','AH3','RD3','DW3',
     > 'HT3','KC3','OO3','CT3','MV3','MB3','HH3','SD3'/
C----------
C=====================================
C     SPECIES LIST FOR CENTRAL STATES
C=====================================
C     1 = EASTERN REDCEDAR (RC)
C     2 = JUNIPER SPECIES (JU)
C     3 = SHORTLEAF PINE (SP)
C     4 = VIRGINIA PINE (VP)
C     5 = LOBLOLLY PINE (LP)
C     6 = OTHER SOFTWOOD SPECIES (OS)
C     7 = EASTERN WHITE PINE (WP)
C     8 = BLACK WALNUT (WN)
C     9 = BUTTERNUT (BN)
C    10 = TUPELO SPECIES(TL)
C    11 = SWAMP TUPELO (TS)
C    12 = WATER TUPELO (WT)
C    13 = BLACKGUM / BLACK TUPELO (BG)
C    14 = SELECT HICKORY (HS)
C    15 = SHAGBARK HICKORY (SH)
C    16 = SHELLBARK HICKORY (SL)
C    17 = MOCKERNUT HICKORY (MH)
C    18 = PIGNUT HICKORY (PH)
C    19 = HICKORY SPECIES (HI)
C    20 = WATER HICKORY (WH)
C    21 = BITTERNUT HICKORY (BH)
C    22 = PECAN (PE)
C    23 = BLACK HICKORY (BI)
C    24 = AMERICAN BEECH (AB)
C    25 = BLACK ASH (BA)
C    26 = PUMPKIN ASH (PA)
C    27 = BLUE ASH (UA)
C    28 = EASTERN COTTONWOOD (EC)
C    29 = RED MAPLE (RM)
C    30 = BOXELDER (BE)
C    31 = SILVER MAPLE (SV)
C    32 = BLACK CHERRY (BC)
C    33 = AMERICAN ELM (AE)
C    34 = SUGARBERRY (SG)
C    35 = HACKBERRY (HK)
C    36 = WINGED ELM (WE)
C    37 = ELM SPECIES (EL)
C    38 = SIBERIAN ELM (SI)
C    39 = SLIPPERY (RED) ELM (RL)
C    40 = ROCK ELM (RE)
C    41 = YELLOW-POPLAR (YP)
C    42 = AMERICAN BASSWOOD (BW)
C    43 = SUGAR MAPLE (SM)
C    44 = ASH SPECIES(AS)
C    45 = WHITE ASH (WA)
C    46 = GREEN ASH (GA)
C    47 = WHITE OAK (WO)
C    48 = NORTHERN RED OAK (RO)
C    49 = SOUTHERN RED OAK (SK)
C    50 = BLACK OAK (BO)
C    51 = SCARLET OAK (SO)
C    52 = BLACKJACK OAK (BJ)
C    53 = CHINKAPIN OAK (CK)
C    54 = SWAMP WHITE OAK (SW)
C    55 = BUR OAK (BR)
C    56 = SWAMP CHESTNUT OAK (SN)
C    57 = POST OAK (PO)
C    58 = DELTA POST OAK (DO)
C    59 = CHESTNUT OAK (CO)
C    60 = PIN OAK (PN)
C    61 = CHERRYBARK OAK (CB)
C    62 = SHINGLE OAK (QI)
C    63 = OVERCUP OAK (OV)
C    64 = WATER OAK (WK)
C    65 = NUTTALL OAK (NK)
C    66 = WILLOW OAK (WL)
C    67 = SHUMARD OAK (QS)
C    68 = OTHER UPLAND HARDWOOD SPECIES (UH)
C    69 = SASSAFRAS (SS)
C    70 = OHIO BUCKEYE (OB)
C    71 = CATALPA (CA)
C    72 = COMMON PERSIMMON (PS)
C    73 = HONEYLOCUST (HL)
C    74 = BALSAM POPLAR (BP)
C    75 = BIGTOOTH ASPEN (BT)
C    76 = QUAKING ASPEN (QA)
C    77 = BLACK LOCUST (BK)
C    78 = OTHER LOWLAND SPECIES (OL)
C    79 = SYCAMORE (SY)
C    80 = BALDCYPRESS (BY)
C    81 = RIVER BIRCH (RB)
C    82 = SWEETGUM (SU)
C    83 = WILLOW SPECIES(WI)
C    84 = BLACK WILLOW (BL)
C    85 = NON-COMMERCIAL HARDWOOD SPECIES (NC)
C    86 = AMERICAN HORNBEAM (AH)
C    87 = EASTERN REDBUD (RD)
C    88 = FLOWERING DOGWOOD (DW)
C    89 = HAWTHORN SPECIES(HT)
C    90 = KENTUCKY COFFEETREE (KC)
C    91 = OSAGE-ORANGE (OO)
C    92 = CUCUMBERTREE (CT)
C    93 = SWEETBAY (MV)
C    94 = MULBERRY SPECIES (MB)
C    95 = EASTERN HOPHORNBEAM (HH)
C    96 = SOURWOOD (SD)
C----------
C  SIGMAR ARE THE ERROR TERMS FOR DIAMETER GROWTH.  THESE VALUES
C  ARE THE ROOT MEAN SQUARE ERROR FOR THE 10-YR BAI MODEL
C  EQUATION.
C----------
      DATA SIGMAR/2*.044, .071, 2*.071, 2*.071, 2*.126, 4*.052, 10*.067,
     &            .123, 3*.127, .324, 3*.315, .123, 8*.156, .217, .123,
     &            .093, 3*.127, .086, 2*.122,
     &            .091, .082, .053, 4*.154, 2*.056, .145, 8*.186,
     &            10*.123, 7*.324, 12*.057/
C----------
C     DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK IN THE
C     FOLLOWING DATA STATEMENTS, DF IS USED FOR OT, AND WF FOR RC
C----------
      DATA HT1/
     &  4.4718,   4.0374,   4.6271,   4.4718,   4.6897,   4.0374,
     &  4.6090,   4.5018,   4.5018,   4.3802,   4.4334,   4.4330,
     &  4.3802,   4.5128,   4.5128,   4.5128,   4.5128,   4.5128,
     &  4.5128,   4.5128,   4.5128,   4.5128,   4.5128,   4.4772,
     &  4.6155,   4.4819,   4.4819,   4.9396,   4.3379,   4.5018,
     &  4.5991,   4.3286,   4.6008,   4.5128,   4.4207,   4.5992,
     &  4.3744,   4.3744,   4.6238,   4.3744,   4.6892,   4.5820,
     &  4.4834,   4.4819,   4.5959,   4.6155,   4.5463,   4.5202,
     &  4.5142,   4.4747,   4.5225,   3.9191,   4.3420,   4.7342,
     &  4.5225,   4.6135,   4.2496,   4.2496,   4.4618,   4.5225,
     &  4.7342,   4.4618,   4.5710,   4.5577,   4.5225,   4.9396,
     &  4.6106,   4.5463,   4.3383,   4.5820,   4.9396,   4.4207,
     &  4.3734,   4.5959,   4.5959,   4.5128,   4.4299,   4.3379,
     &  4.6355,   4.6171,   4.4388,   4.5920,   4.4911,   4.4911,
     &  4.4207,   4.0322,   3.7512,   3.7301,   4.4207,   4.4772,
     &  4.0322,   4.6067,   4.3609,   3.9613,   4.0322,   4.1352/
C
      DATA HT2/
     & -5.0078,  -4.2964,  -6.4095,  -5.0078,  -6.8801,  -4.2964,
     & -6.1896,  -5.6123,  -5.6123,  -4.7903,  -4.5709,  -4.5383,
     & -4.7903,  -4.9918,  -4.9918,  -4.9918,  -4.9918,  -4.9918,
     & -4.9918,  -4.9918,  -4.9918,  -4.9918,  -4.9918,  -4.7206,
     & -6.2945,  -4.5314,  -4.5314,  -8.1838,  -3.8214,  -5.6123,
     & -6.6706,  -4.0922,  -7.2732,  -4.9918,  -5.1435,  -7.7428,
     & -4.5257,  -4.5257,  -7.4847,  -4.5257,  -4.9605,  -5.0903,
     & -4.5431,  -4.5314,  -6.4497,  -6.2945,  -5.2287,  -4.8896,
     & -5.2205,  -4.8698,  -4.9401,  -4.3503,  -5.1193,  -6.2674,
     & -4.9401,  -5.7613,  -4.8061,  -4.8061,  -4.8786,  -4.9401,
     & -6.2674,  -4.8786,  -6.0922,  -4.9595,  -4.9401,  -8.1838,
     & -5.4380,  -5.2287,  -4.5018,  -5.0903,  -8.1838,  -5.1435,
     & -5.3135,  -6.4497,  -6.4497,  -4.9918,  -4.9920,  -3.8214,
     & -5.2776,  -6.2684,  -4.0872,  -5.1719,  -5.7928,  -5.7928,
     & -5.1435,  -3.0833,  -2.5539,  -2.7758,  -5.1435,  -4.7206,
     & -3.0833,  -5.2030,  -4.1423,  -3.1993,  -3.0833,  -3.7450/
C
C
      DATA BB0/96*0.0/
      DATA BB1/96*0.0/
      DATA BB2/96*0.0/
      DATA BB3/96*0.0/
      DATA BB4/96*0.0/
      DATA BB5/96*0.0/
      DATA BB6/96*0.0/
      DATA BB7/96*0.0/
      DATA BB8/96*0.0/
      DATA BB9/96*0.0/
      DATA BB10/96*0.0/
      DATA BB11/96*0.0/
      DATA BB12/96*0.0/
      DATA BB13/96*0.0/
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

