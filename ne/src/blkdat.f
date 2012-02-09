      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C  **BLKDAT--NE  DATE OF LAST REVISION:  02/09/2012
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
C  SPECIES SPECIFIC BARK RATIOS
C
      DATA  BKRAT/.9349,.9349,.956,4*.9324,.920,.920,.890,.964,
     &            4*.950,2*.934,8*.964,.950,3*.920,3*.948,2*.948,
     &            5*.900,.950,5*.900,3*.900,5*.900,.940,5*.910,4*.900,
     &            3*.880,2*.900,2*.900,27*.900,11*.900/
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
C   VALUE FOR THE NE VARIANT.
C
      DATA XMIN/
     & .33, .50, .25, .25, .25, .25, .25, .25, .33, .25,
     & .42, .33, .33, .33, .33, .25, .25, .25, .33, .25,
     & .25, .42, .25, .33, .25, 1.0, .25, .25, .42, .42,
     & .42, .33, .42, .42, .33, .33, .33, .33, .33, .25,
     & .42, .42, .33, .42, .42, .42, .33, .33, .42, .42,
     & .42, .42, .42, .42, .33, .25, .33, .25, .33, .33,
     & .25, .33, .25, .33, .33, .33, .42, .33, .33, .33,
     & .33, .25, .25, .25, .25, .25, .25, .33, .33, .25,
     & .42, .42, .42, .33, .33, .33, .33, .58, .25, .58,
     & 1.0, .50, .33, .33, .33, .33, .33, .33, .33, .25,
     & 1.0, .42, .42, .25, .25, .42, .33, .33/
      DATA HHTMAX/
     & 20., 24., 18., 16., 18., 16., 16., 18., 20., 14.,
     & 14., 16., 16., 16., 16., 16., 16., 16., 14., 14.,
     & 16., 18., 12., 20., 16., 20., 16., 16., 18., 22.,
     & 20., 18., 18., 18., 14., 14., 14., 14., 18., 14.,
     & 24., 24., 18., 24., 28., 24., 18., 20., 20., 24.,
     & 24., 20., 20., 26., 16., 14., 12., 12., 16., 16.,
     & 14., 16., 14., 16., 16., 12., 20., 16., 16., 14.,
     & 16., 12., 12., 12., 12., 12., 12., 18., 20., 12.,
     & 20., 20., 20., 20., 16., 16., 16., 24., 14., 24.,
     & 32., 18., 16., 16., 16., 16., 12., 10., 16., 18.,
     & 30., 20., 20., 18., 16., 20., 20., 30./
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,IFORST/20*1/,
     &  IFORCD/911,919,920,921,922,15*0/,
     &  ISPSPE/26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,
     &         44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,
     &         62,63,64,65,66,67,68,69,70,72,73,74,75,76,77,78,79,80,
     &         81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,99,
     &         100,101,102,103,104,105,106,107,108/

C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/1728*1.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/2160*1.0/
C----------
C COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'BF ',   'TA ',   'WS ',   'RS ',   'NS ',   'BS ',   'PI ',
     & 'RN ',   'WP ',   'LP ',   'VP ',   'WC ',   'AW ',   'RC ',
     & 'OC ',   'EH ',   'HM ',   'OP ',   'JP ',   'SP ',   'TM ',
     & 'PP ',   'PD ',   'SC ',   'OS ',   'RM ',   'SM ',   'BM ',
     & 'SV ',   'YB ',   'SB ',   'RB ',   'PB ',   'GB ',   'HI ',
     & 'PH ',   'SL ',   'SH ',   'MH ',   'AB ',   'AS ',   'WA ',
     & 'BA ',   'GA ',   'PA ',   'YP ',   'SU ',   'CT ',   'QA ',
     & 'BP ',   'EC ',   'BT ',   'PY ',   'BC ',   'WO ',   'BR ',
     & 'CK ',   'PO ',   'OK ',   'SO ',   'QI ',   'WK ',   'PN ',
     & 'CO ',   'SW ',   'SN ',   'RO ',   'SK ',   'BO ',   'CB ',
     & 'OH ',   'BU ',   'YY ',   'WR ',   'HK ',   'PS ',   'HY ',
     & 'BN ',   'WN ',   'OO ',   'MG ',   'MV ',   'AP ',   'WT ',
     & 'BG ',   'SD ',   'PW ',   'SY ',   'WL ',   'BK ',   'BL ',
     & 'SS ',   'BW ',   'WB ',   'EL ',   'AE ',   'RL ',   'NC ',
     & 'BE ',   'ST ',   'AI ',   'SE ',   'AH ',   'DW ',   'HT ',
     & 'HH ',   'PL ',   'PR '/
C
      DATA FIAJSP /
     & '012',   '071',   '094',   '097',   '091',   '095',   '090',
     & '125',   '129',   '131',   '132',   '241',   '043',   '068',
     & '057',   '261',   '260',   '100',   '105',   '110',   '123',
     & '126',   '128',   '130',   '298',   '316',   '318',   '314',
     & '317',   '371',   '372',   '373',   '375',   '379',   '400',
     & '403',   '405',   '407',   '409',   '531',   '540',   '541',
     & '543',   '544',   '545',   '621',   '611',   '651',   '746',
     & '741',   '742',   '743',   '744',   '762',   '802',   '823',
     & '826',   '835',   '800',   '806',   '817',   '827',   '830',
     & '832',   '804',   '825',   '833',   '812',   '837',   '813',
     & '998',   '330',   '332',   '374',   '462',   '521',   '591',
     & '601',   '602',   '641',   '650',   '653',   '660',   '691',
     & '693',   '711',   '712',   '731',   '831',   '901',   '922',
     & '931',   '951',   '952',   '970',   '972',   '975',   '   ',
     & '313',   '315',   '341',   '356',   '391',   '491',   '500',
     & '701',   '760',   '761'/
C
      DATA PLNJSP /
     & 'ABBA  ','LALA  ','PIGL  ','PIRU  ','PIAB  ','PIMA  ','PICEA ',
     & 'PIRE  ','PIST  ','PITA  ','PIVI2 ','THOC2 ','CHTH2 ','JUVI  ',
     & 'JUNIP ','TSCA  ','TSUGA ','PINUS ','PIBA2 ','PIEC2 ','PIPU5 ',
     & 'PIRI  ','PISE  ','PISY  ','2TE   ','ACRU  ','ACSA3 ','ACNI5 ',
     & 'ACSA2 ','BEAL2 ','BELE  ','BENI  ','BEPA  ','BEPO  ','CARYA ',
     & 'CAGL8 ','CALA21','CAOV2 ','CAAL27','FAGR  ','FRAXI ','FRAM2 ',
     & 'FRNI  ','FRPE  ','FRPR  ','LITU  ','LIST2 ','MAAC  ','POTR5 ',
     & 'POBA2 ','PODE3 ','POGR4 ','POHE4 ','PRSE2 ','QUAL  ','QUMA2 ',
     & 'QUMU  ','QUST  ','QUERC ','QUCO2 ','QUIM  ','QUNI  ','QUPA2 ',
     & 'QUPR2 ','QUBI  ','QUMI  ','QURU  ','QUFA  ','QUVE  ','QUPA5 ',
     & '2TD   ','AESCU ','AEFL  ','BEOC2 ','CEOC  ','DIVI5 ','ILOP  ',
     & 'JUCI  ','JUNI  ','MAPO  ','MAGNO ','MAVI2 ','MALUS ','NYAQ2 ',
     & 'NYSY  ','OXAR  ','PATO2 ','PLOC  ','QUPH  ','ROPS  ','SANI  ',
     & 'SAAL5 ','TIAM  ','TIAMH ','ULMUS ','ULAM  ','ULRU  ','------',
     & 'ACNE2 ','ACPE  ','AIAL  ','AMELA ','CACA18','COFL2 ','CRATA ',
     & 'OSVI  ','PRUNU ','PRPE2 '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /'BF1','TA1','WS1','RS1','NS1','BS1','PI1','RN1','WP1',
     > 'LP1','VP1','WC1','AW1','RC1','OC1','EH1','HM1','OP1','JP1',
     > 'SP1','TM1','PP1','PD1','SC1','OS1','RM1','SM1','BM1','SV1',
     > 'YB1','SB1','RB1','PB1','GB1','HI1','PH1','SL1','SH1','MH1',
     > 'AB1','AS1','WA1','BA1','GA1','PA1','YP1','SU1','CT1','QA1',
     > 'BP1','EC1','BT1','PY1','BC1','WO1','BR1','CK1','PO1','OK1',
     > 'SO1','QI1','WK1','PN1','CO1','SW1','SN1','RO1','SK1','BO1',
     > 'CB1','OH1','BU1','YY1','WR1','HK1','PS1','HY1','BN1','WN1',
     > 'OO1','MG1','MV1','AP1','WT1','BG1','SD1','PW1','SY1','WL1',
     >'BK1','BL1','SS1','BW1','WB1','EL1','AE1','RL1','NC1','BE1','ST1',
     >'AI1','SE1','AH1','DW1','HT1','HH1','PL1','PR1','BF2','TA2','WS2',
     > 'RS2','NS2','BS2','PI2','RN2','WP2','LP2','VP2','WC2','AW2',
     > 'RC2','OC2','EH2','HM2','OP2','JP2','SP2','TM2','PP2','PD2',
     > 'SC2','OS2','RM2','SM2','BM2','SV2','YB2','SB2','RB2','PB2',
     > 'GB2','HI2','PH2','SL2','SH2','MH2','AB2','AS2','WA2','BA2',
     > 'GA2','PA2','YP2','SU2','CT2','QA2','BP2','EC2','BT2','PY2',
     > 'BC2','WO2','BR2','CK2','PO2','OK2','SO2','QI2','WK2','PN2',
     > 'CO2','SW2','SN2','RO2','SK2','BO2','CB2','OH2','BU2','YY2',
     > 'WR2','HK2','PS2','HY2','BN2','WN2','OO2','MG2','MV2','AP2',
     > 'WT2','BG2','SD2','PW2','SY2','WL2','BK2','BL2','SS2','BW2',
     >'WB2','EL2','AE2','RL2','NC2','BE2','ST2','AI2','SE2','AH2','DW2',
     >'HT2','HH2','PL2','PR2','BF3','TA3','WS3','RS3','NS3','BS3','PI3',
     > 'RN3','WP3','LP3','VP3','WC3','AW3','RC3','OC3','EH3','HM3',
     > 'OP3','JP3','SP3','TM3','PP3','PD3','SC3','OS3','RM3','SM3',
     > 'BM3','SV3','YB3','SB3','RB3','PB3','GB3','HI3','PH3','SL3',
     > 'SH3','MH3','AB3','AS3','WA3','BA3','GA3','PA3','YP3','SU3',
     > 'CT3','QA3','BP3','EC3','BT3','PY3','BC3','WO3','BR3','CK3',
     > 'PO3','OK3','SO3','QI3','WK3','PN3','CO3','SW3','SN3','RO3',
     > 'SK3','BO3','CB3','OH3','BU3','YY3','WR3','HK3','PS3','HY3',
     > 'BN3','WN3','OO3','MG3','MV3','AP3','WT3','BG3','SD3','PW3',
     > 'SY3','WL3','BK3','BL3','SS3','BW3','WB3','EL3','AE3','RL3',
     > 'NC3','BE3','ST3','AI3','SE3','AH3','DW3','HT3','HH3','PL3',
     > 'PR3'/
C----------
C================================================
C     SPECIES LIST FOR NORTHEASTERN UNITED STATES
C================================================
C     1 = BALSAM FIR (BF)
C     2 = TAMARACK (TA)
C     3 = WHITE SPRUCE (WS)
C     4 = RED SPRUCE (RS)
C     5 = NORWAY SPRUCE (NS)
C     6 = BLACK SPRUCE (BS)
C     7 = OTHER SPRUCE SPECIES(PI)
C     8 = RED PINE (RN)
C     9 = EASTERN WHITE PINE (WP)
C    10 = LOBLOLLY PINE (LP)
C    11 = VIRGINIA PINE (VP)
C    12 = NORTHERN WHITE CEDAR (WC)
C    13 = ATLANTIC WHITE CEDAR (AW)
C    14 = EASTERN REDCEDAR (RC)
C    15 = OTHER CEDAR SPECIES (OC)
C    16 = EASTERN HEMLOCK (EH)
C    17 = OTHER HEMLOCK SPECIES (HM)
C    18 = OTHER PINE SPECIES (OP)
C    19 = JACK PINE (JP)
C    20 = SHORLEAF PINE (SP)
C    21 = TABLE MOUNTAIN PINE (TM)
C    22 = PITCH PINE (PP)
C    23 = POND PINE (PD)
C    24 = SCOTCH PINE (SC)
C    25 = OTHER SOFTWOOD SPECIES (OS)
C    26 = RED MAPLE (RM)
C    27 = SUGAR MAPLE (SM)
C    28 = BLACK MAPLE (BM)
C    29 = SILVER MAPLE (SV)
C    30 = YELLOW BIRCH (YB)
C    31 = SWEET BIRCH (SB)
C    32 = RIVER BIRCH (RB)
C    33 = PAPER BIRCH (PB)
C    34 = GRAY BIRCH (GB)
C    35 = HICKORY SPECIES (HI)
C    36 = PIGNUT HICKORY (PH)
C    37 = SHELLBARK HICKORY (SL)
C    38 = SHAGBARK HICKORY (SH)
C    39 = MOCKERNUT HICKORY (MH)
C    40 = AMERICAN BEECH (AB)
C    41 = ASH SPECIES (AS)
C    42 = WHITE ASH (WA)
C    43 = BLACK ASH (BA)
C    44 = GREEN ASH (GA)
C    45 = PUMPKIN ASH (PA)
C    46 = YELLOW-POPLAR (YP)
C    47 = SWEETGUM (SU)
C    48 = CUCUMBERTREE (CT)
C    49 = QUAKING ASPEN (QA)
C    50 = BALSAM POPLAR (BP)
C    51 = EASTERN COTTONWOOD (EC)
C    52 = BIGTOOTH ASPEN (BT)
C    53 = SWAMP COTTONWOOD (PY)
C    54 = BLACK CHERRY (BC)
C    55 = WHITE OAK (WO)
C    56 = BUR OAK (BR)
C    57 = CHINKAPIN OAK (CK)
C    58 = POST OAK (PO)
C    59 = OTHER OAK SPECIES (OK)
C    60 = SCARLET OAK (SO)
C    61 = SHINGLE OAK (QI)
C    62 = WATER OAK (WK)
C    63 = PIN OAK (PN)
C    64 = CHESTNUT OAK (CO)
C    65 = SWAMP WHITE OAK (SW)
C    66 = SWAMP CHESTNUT OAK (SN)
C    67 = NORTHERN RED OAK (RO)
C    68 = SOUTHERN RED OAK (SK)
C    69 = BLACK OAK (BO)
C    70 = CHERRYBARK OAK (CB)
C    71 = OTHER HARDWOOD SPECIES (OH)
C    72 = BUCKEYE SPECIES (BU)
C    73 = YELLOW BUCKEYE (YY)
C    74 = WATER BIRCH (WR)
C    75 = HACKBERRY (HK)
C    76 = COMMON PERSIMMON (PS)
C    77 = AMERICAN HOLLY (HY)
C    78 = BUTTERNUT (BN)
C    79 = BLACK WALNUT (WN)
C    80 = OSAGE-ORANGE (OO)
C    81 = MAGNOLIA SPECIES(MG)
C    82 = SWEETBAY (MV)
C    83 = APPLE SPECIES (AP)
C    84 = WATER TUPELO (WT)
C    85 = BLACKGUM (BG)
C    86 = SOURWOOD (SD)
C    87 = PAULOWNIA (PW)
C    88 = SYCAMORE (SY)
C    89 = WILLOW OAK (WL)
C    90 = BLACK LOCUST (BK)
C    91 = BLACK WILLOW (BL)
C    92 = SASSAFRAS (SS)
C    93 = AMERICAN BASSWOOD (BW)
C    94 = WHITE BASSWOOD (WB)
C    95 = OTHER ELM SPECIES (EL)
C    96 = AMERICAN ELM (AE)
C    97 = SLIPPERY ELM (RL)
C    98 = NON-COMMERCIAL HARDWOOD SPECIES (NC)
C    99 = BOXELDER (BE)
C   100 = STRIPED MAPLE (ST)
C   101 = AILANTHUS (AI)
C   102 = SERVICEBERRY (SE)
C   103 = AMERICAN HORNBEAM (AH)
C   104 = FLOWERING DOGWOOD (DW)
C   105 = HAWTHORN SPECIES (HT)
C   106 = EASTERN HOPHORNBEAM (HH)
C   107 = PLUMS, CHERRIES (PL)
C   108 = PIN CHERRY (PR)
C----------
C  SIGMAR ARE THE ERROR TERMS FOR DIAMETER GROWTH. THESE VALUES
C  ARE THE ANNUAL ROOT MEAN SQUARE ERROR FOR EACH SPECIES EXPANDED
C  TO A 10-YR BASIS BY TAKING THE RATIO OF ANNUAL ERROR TO 10-YR
C  ERROR BY SPECIES IN THE CS-TWIGS PUBLICATIONS, AND EXPANDING
C  THE NE-TWIGS ANNUAL VALUES BY THAT FACTOR.
C----------
      DATA SIGMAR/.016,.025,.025,4*.022,.058,.071,.053,.036,4*.019,
     &    2*.025,8*.036,.112,3*.093,3*.102,2*.074,5*.029,.054,5*.063,
     &    3*.189,5*.070,.075,5*.066,4*.091,3*.069,2*.153,2*.108,
     &    27*.040,11*.027/
C----------
C  THE HT-DBH COEFFICIENTS (HT1 AND HT2 ARRAYS) ARE 
C  ASSIGNED IN **SITSET
C----------
      DATA BB0/108*0.0/
      DATA BB1/108*0.0/
      DATA BB2/108*0.0/
      DATA BB3/108*0.0/
      DATA BB4/108*0.0/
      DATA BB5/108*0.0/
      DATA BB6/108*0.0/
      DATA BB7/108*0.0/
      DATA BB8/108*0.0/
      DATA BB9/108*0.0/
      DATA BB10/108*0.0/
      DATA BB11/108*0.0/
      DATA BB12/108*0.0/
      DATA BB13/108*0.0/
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
