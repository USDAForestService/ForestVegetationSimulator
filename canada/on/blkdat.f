      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
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
      INCLUDE 'VARCOM.F77'
      INCLUDE 'FVSSTDCM.F77'      
C
      INTEGER I,J
C
COMMONS
C----------
C  SPECIES SPECIFIC BARK RATIOS.  FROM RAILE, G.K. 1982. ESTIMATING
C  STUMP VOLUME. RES. PAP. NC-224, 4 P.
C----------
      DATA BKRAT/5*.91,2*.95,3*.94,.95,3*.91,2*.94,.93,3*.94,3*.93,
     &           .94,.92,3*.94,.92,4*.91,3*.92,3*.94,3*.92,6*.94,
     &           20*.92,2*.91,.95,.94/
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F5.1,F4.1,2F5.1,F5.1,I1,3(I2,I2),2I1,I2,
     >2I3,2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/15,2,3,16,4,8 /
C
C   COMMON STATEMENT FOR ESCOMN VARIABLE
C   HHTMAX, IFORST, OCURHT, AND OCURNF HAVE ALL BEEN SET TO ONE
C   VALUE FOR THE LS & ON VARIANTS
C
      DATA XMIN/
     &0.33, 0.33, 0.25, 0.25, 0.33, 0.25, 0.25, 0.33, 0.25, 0.50,
     &0.33, 0.25, 0.25, 0.33, 0.33, 0.42, 0.42, 0.42, 1.00, 0.42,
     &0.33, 0.33, 0.50, 0.42, 0.33, 0.25, 0.25, 0.25, 0.42, 0.33,
     &0.33, 0.25, 0.33, 0.42, 0.33, 1.40, 0.50, 0.33, 0.33, 0.42,
     &0.42, 0.42, 0.42, 0.50, 0.33, 0.33, 0.42, 0.58, 0.33, 0.33,
     &0.25, 2.10, 0.42, 0.50, 0.25, 0.25, 0.25, 0.42, 0.33, 0.58,
     &0.33, 2.10, 0.33, 4.70, 1.00, 2.10, 0.50, 2.10, 0.33, 0.33,
     &0.25, 0.25/

      DATA HHTMAX/
     &14., 20., 18., 18., 20., 18., 18., 20., 16., 24.,
     &16., 16., 16., 16., 18., 24., 24., 18., 20., 26.,
     &16., 12., 20., 22., 16., 16., 16., 14., 24., 16.,
     &16., 14., 12., 20., 16., 20., 20., 14., 14., 20.,
     &20., 24., 18., 20., 18., 20., 20., 24., 10., 16.,
     &18., 20., 20., 20., 12., 18., 16., 20., 16., 24.,
     &30., 20., 20., 20., 32., 20., 18., 20., 14., 20.,
     &18., 16./
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,IFORST/20*1/,
     &  IFORCD/902,903,904,906,907,909,910,913,915,916,10*0/,
     &  ISPSPE/15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
     &         33,34,35,36,37,38,39,40,41,42,43,45,46,47,48,50,51,
     &         52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,72)/1152*1.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,72)/1440*1.0/
C
C         COMMON STATEMENT FOR PLOT VARIABLES.
C
C     OLD LS VERSION SPECIES CODES
c     DATA JSP     /'JP ','SC ','RN ','RP ','WP ','WS ','NS ','BF ',
c    &  'BS ','TA ','WC ','EH ','OS ','RC ','BA ','GA ','CW ','SV ',
c    &  'RM ','BC ','AE ','RL ','RE ','YB ','BW ','SM ','BM ','AB ',
c    &  'WA ','WO ','SW ','BR ','CK ','RO ','BO ','NP ','BH ','PH ',
c    &  'SH ','BT ','QA ','BP ','PB ','CH ','BN ','WN ','HH ','BK ',
c    &  'NC ','BE ','ST ','MM ','AH ','AC ','HB ','DW ','HT ','AP ',
c    &  'BG ','SY ','PR ','CC ','PL ','WI ','BL ','DM ','SS ','MA '/
c
C     ONTARIO SPECIES CODES. 
C        ADD IN THE FOUR NEW PLANTATION SPECIES
      DATA JSP     /'PJ ','PS ','RN ','RP ','PW ','SW ','SN ','BF ', ! 8
     &  'SB ','TA ','CE ','HE ','SO ','CR ','AB ','AR ','CW ','MV ', !18
     &  'MR ','CB ','EW ','ES ','ER ','BY ','BD ','MH ','MB ','BE ', !28
     &  'AW ','OW ','OP ','OB ','OC ','OR ','BO ','PN ','HB ','HP ', !38
     &  'HU ','PG ','PT ','PB ','BW ','CH ','BT ','WB ','IW ','LB ', !48
     &  'NC ','MM ','MS ','MT ','BB ','CA ','BH ','DF ','HT ','ML ', !58
     &  'GB ','SY ','CP ','CC ','PL ','WI ','WI ','WI ','SS ','AM ', !68
     &  'JP ','WP ','SP ','BP '/                                     !72
C
      DATA FIAJSP /
     & '105',   '130',   '125',   '125',   '129',   '094',   '091',
     & '012',   '095',   '071',   '241',   '261',   '298',   '068',
     & '543',   '544',   '742',   '317',   '316',   '762',   '972',
     & '975',   '977',   '371',   '951',   '318',   '314',   '531',
     & '541',   '802',   '804',   '823',   '826',   '833',   '837',
     & '809',   '402',   '403',   '407',   '743',   '746',   '741',
     & '375',   '   ',   '601',   '602',   '701',   '901',   '   ',
     & '313',   '315',   '319',   '391',   '421',   '462',   '491',
     & '500',   '660',   '693',   '731',   '761',   '763',   '760',
     & '920',   '922',   '923',   '931',   '935',   '105',   '129',
     & '094',   '095'/
C
      DATA PLNJSP /
     & 'PIBA2 ','PISY  ','PIRE  ','PIRE  ','PIST  ','PIGL  ','PIAB  ',
     & 'ABBA  ','PIMA  ','LALA  ','THOC2 ','TSCA  ','2TE   ','JUVI  ',
     & 'FRNI  ','FRPE  ','PODE3 ','ACSA2 ','ACRU  ','PRSE2 ','ULAM  ',
     & 'ULRU  ','ULTH  ','BEAL2 ','TIAM  ','ACSA3 ','ACNI5 ','FAGR  ',
     & 'FRAM2 ','QUAL  ','QUBI  ','QUMA2 ','QUMU  ','QURU  ','QUVE  ',
     & 'QUEL  ','CACO15','CAGL8 ','CAOV2 ','POGR4 ','POTR5 ','POBA2 ',
     & 'BEPA  ','      ','JUCI  ','JUNI  ','OSVI  ','ROPS  ','      ',
     & 'ACNE2 ','ACPE  ','ACSP2 ','CACA18','CADE12','CEOC  ','COFL2 ',
     & 'CRATA ','MALUS ','NYSY  ','PLOC  ','PRPE2 ','PRVI  ','PRUNU ',
     & 'SALIX ','SANI  ','SAERF ','SAAL5 ','SOAM3 ','PIBA2 ','PIST  ',
     & 'PIGL  ','PIMA  '/
C
      DATA JTYPE /122*0/
C
c     DATA NSP /'JP1','SC1','RN1','RP1','WP1','WS1','NS1','BF1','BS1',
c    &  'TA1','WC1','EH1','OS1','RC1','BA1','GA1','CW1','SV1',
c    &  'RM1','BC1','AE1','RL1','RE1','YB1','BW1','SM1','BM1','AB1',
c    &  'WA1','WO1','SW1','BR1','CK1','RO1','BO1','NP1','BH1','PH1',
c    &  'SH1','BT1','QA1','BP1','PB1','CH1','BN1','WN1','HH1','BK1',
c    &  'NC1','BE1','ST1','MM1','AH1','AC1','HB1','DW1','HT1','AP1',
c    &  'BG1','SY1','PR1','CC1','PL1','WI1','BL1','DM1','SS1','MA1',
c    &  'JP2','SC2','RN2','RP2','WP2','WS2','NS2','BF2','BS2',
c    &  'TA2','WC2','EH2','OS2','RC2','BA2','GA2','CW2','SV2',
c    &  'RM2','BC2','AE2','RL2','RE2','YB2','BW2','SM2','BM2','AB2',
c    &  'WA2','WO2','SW2','BR2','CK2','RO2','BO2','NP2','BH2','PH2',
c    &  'SH2','BT2','QA2','BP2','PB2','CH2','BN2','WN2','HH2','BK2',
c    &  'NC2','BE2','ST2','MM2','AH2','AC2','HB2','DW2','HT2','AP2',
c    &  'BG2','SY2','PR2','CC2','PL2','WI2','BL2','DM2','SS2','MA2',
c    &  'JP3','SC3','RN3','RP3','WP3','WS3','NS3','BF3','BS3',
c    &  'TA3','WC3','EH3','OS3','RC3','BA3','GA3','CW3','SV3',
c    &  'RM3','BC3','AE3','RL3','RE3','YB3','BW3','SM3','BM3','AB3',
c    &  'WA3','WO3','SW3','BR3','CK3','RO3','BO3','NP3','BH3','PH3',
c    &  'SH3','BT3','QA3','BP3','PB3','CH3','BN3','WN3','HH3','BK3',
c    &  'NC3','BE3','ST3','MM3','AH3','AC3','HB3','DW3','HT3','AP3',
c    &  'BG3','SY3','PR3','CC3','PL3','WI3','BL3','DM3','SS3','MA3'/
c
      DATA NSP /'PJ1','PS1','RN1','RP1','PW1','SW1','SN1','BF1','SB1',
     &  'TA1','CE1','HE1','SO1','CR1','AB1','AR1','CW1','MV1',
     &  'MR1','CB1','EW1','ES1','ER1','BY1','BD1','MH1','MB1','BE1',
     &  'AW1','OW1','OP1','OB1','OC1','OR1','BO1','PN1','HB1','HP1',
     &  'HU1','PG1','PT1','PB1','BW1','CH1','BT1','WB1','IW1','LB1',
     &  'NC1','MM1','MS1','MT1','BB1','CA1','BH1','DF1','HT1','ML1',
     &  'GB1','SY1','CP1','CC1','PL1','WI1','WI1','WI1','SS1','AM1',
     &  'JP1','WP1','SP1','BP1',  
     &  'PJ2','PS2','RN2','RP2','PW2','SW2','SN2','BF2','SB2',
     &  'TA2','CE2','HE2','SO2','CR2','AB2','AR2','CW2','MV2',
     &  'MR2','CB2','EW2','ES2','ER2','BY2','BD2','MH2','MB2','BE2',
     &  'AW2','OW2','OP2','OB2','OC2','OR2','BO2','PN2','HB2','HP2',
     &  'HU2','PG2','PT2','PB2','BW2','CH2','BT2','WB2','IW2','LB2',
     &  'NC2','MM2','MS2','MT2','BB2','CA2','BH2','DF2','HT2','ML2',
     &  'GB2','SY2','CP2','CC2','PL2','WI2','WI2','WI2','SS2','AM2',
     &  'JP2','WP2','SP2','BP2',  
     &  'PJ3','PS3','RN3','RP3','PW3','SW3','SN3','BF3','SB3',
     &  'TA3','CE3','HE3','SO3','CR3','AB3','AR3','CW3','MV3',
     &  'MR3','CB3','EW3','ES3','ER3','BY3','BD3','MH3','MB3','BE3',
     &  'AW3','OW3','OP3','OB3','OC3','OR3','BO3','PN3','HB3','HP3',
     &  'HU3','PG3','PT3','PB3','BW3','CH3','BT3','WB3','IW3','LB3',
     &  'NC3','MM3','MS3','MT3','BB3','CA3','BH3','DF3','HT3','ML3',
     &  'GB3','SY3','CP3','CC3','PL3','WI3','WI3','WI3','SS3','AM3',
     &  'JP3','WP3','SP3','BP3'/
C
C=================================
C     SPECIES LIST FOR ONTARIO - 
C=================================
C     1 = JACK PINE NATURAL (PJ)
C     2 = SCOTCH PINE (PS)
C     3 = RED PINE NATURAL (RN)
C     4 = PLANTED RED PINE (RP) (was red pine (plantation))
C     5 = EASTERN WHITE PINE NATURAL (PW)
C     6 = WHITE SPRUCE NATURAL (SW)
C     7 = NORWAY SPRUCE (SN)
C     8 = BALSAM FIR (BF)
C     9 = BLACK SPRUCE NATURAL (SB)
C    10 = TAMARACK (TA)
C    11 = NORTHERN WHITE-CEDAR (CE)
C    12 = EASTERN HEMLOCK (HE)
C    13 = OTHER SOFTWOOD SPECIES (SO)
C    14 = EASTERN REDCEDAR (CR)
C    15 = BLACK ASH (AB)
C    16 = GREEN ASH (AR)
C    17 = EASTERN COTTONWOOD (CW)
C    18 = SILVER MAPLE (MV)
C    19 = RED MAPLE (MR)
C    20 = BLACK CHERRY (CB)
C    21 = AMERICAN ELM (EW)
C    22 = SLIPPERY ELM (ES)
C    23 = ROCK ELM (ER)
C    24 = YELLOW BIRCH (BY)
C    25 = AMERICAN BASSWOOD (BD)
C    26 = SUGAR MAPLE (MH)
C    27 = BLACK MAPLE (MB)
C    28 = AMERICAN BEECH (BE)
C    29 = WHITE ASH (AW)
C    30 = WHITE OAK (OW)
C    31 = SWAMP WHITE OAK (OP)
C    32 = BUR OAK (OB)
C    33 = CHINKAPIN OAK (OC)
C    34 = NORTHERN RED OAK (OR)
C    35 = BLACK OAK (BO)
C    36 = NORTHERN PIN OAK (PN)
C    37 = BITTERNUT HICKORY (HB)
C    38 = PIGNUT HICKORY (HP)
C    39 = SHAGBARK HICKORY (HU)
C    40 = LARGETOOTH ASPEN (PG) (was bigtooth aspen)
C    41 = TREMBLING ASPEN (PT) (was quaking aspen)
C    42 = BALSAM POPLAR (PB)
C    43 = PAPER BIRCH (BW)
C    44 = COMMERCIAL HARDWOOD SPECIES (CH)
C    45 = BUTTERNUT (BT)
C    46 = BLACK WALNUT (WB)
C    47 = IRONWOOD (IW) (was eastern hophornbeam)
C    48 = BLACK LOCUST (LB)
C    49 = NON-COMMERCIAL HARDWOOD SPECIES (NC)
C    50 = BOXELDER (MM)
C    51 = STRIPED MAPLE (MS)
C    52 = MOUNTAIN MAPLE (MT)
C    53 = AMERICAN HORNBEAM (BB)
C    54 = AMERICAN CHESTNUT (CA)
C    55 = HACKBERRY (BH)
C    56 = FLOWERING DOGWOOD (DF)
C    57 = HAWTHORN SPECIES (HT)
C    58 = APPLE SPECIES (ML)
C    59 = BLACKGUM (GB)
C    60 = SYCAMORE (SY)
C    61 = PIN CHERRY (CP)
C    62 = CHOKECHERRY (CC)
C    63 = PLUMS, CHERRIES (PL)
C    64 = WILLOW SPECIES (WI)
C    65 = BLACK WILLOW (WI)
C    66 = DIAMOND WILLOW (WI)
C    67 = SASSAFRAS (SS)
C    68 = AMERICAN MOUNTAIN ASH (AM)
C    69 = PLANTED JACK PINE (JP) (was jack pine (plantation))
C    70 = PLANTED WHITE PINE (WP) (was white pine (plantation))
C    71 = PLANTED WHITE SPRUCE (SP) (was white spruce (plantation))
C    72 = PLANTED BLACK SPRUCE (BP) (was black spruce (plantation))

C----------
C  SIGMAR ARE THE ERROR TERMS FOR DIAMETER GROWTH.  THESE COEFFICIENTS
C  ARE THE ROOT MEAN SQUARE ERROR FOR EACH SPECIES.
C----------
      DATA SIGMAR/2*.205,2*.270,.161,2*.197,.149,.080,.108,.128,.118,
     &            2*.205,2*.145,4*.178,3*.234,.128,.201,3*.240,.186,
     &            4*.105,.130,2*.114,3*.140,.153,2*.211,.156,25*.23,
     &            .205,.161,.197,.080/
C----------
C     DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK IN THE
C     FOLLOWING DATA STATEMENTS, DF IS USED FOR OT, AND WF FOR RC
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
     &  4.3383,   4.4207,   4.5084,   4.6090,   4.5084,   4.5084/

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
     & -4.5018,  -5.1435,  -6.0116,  -6.1896,  -6.0116,  -6.0116/
C
      DATA REGNBK/2.999/

      DATA S0/55329D0/,SS/55329./

      DATA LSCRN,JOSCRN/.FALSE.,6/

      DATA JOSUME/13/
      
      DATA KOLIST,FSTOPEN /27,.FALSE./      

      END
