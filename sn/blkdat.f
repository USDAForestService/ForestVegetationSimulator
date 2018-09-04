      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C SN $Id$
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
C TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/,
     &     BKRAT/MAXSP*0./
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR / 5.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C COMMON STATEMENT FOR ESCOMN VARIABLE
C----------
      DATA XMIN/
     &  0.50, 2.08, 0.50, 1.00, 1.32, 2.51, 0.50, 2.53, 2.75, 0.50,
     &  5.05, 0.50, 4.70, 0.50, 1.33, 1.33, 0.66, 2.40, 1.35, 1.35,
     &  2.03, 0.50, 0.50, 0.50, 0.50, 2.08, 0.51, 0.63, 2.08, 2.08,
     &  2.08, 2.08, 0.50, 0.50, 0.50, 0.92, 0.50, 5.98, 0.94, 2.08,
     &  0.50, 3.28, 3.28, 1.33, 0.89, 1.53, 1.38, 3.59, 3.59, 3.59,
     &  2.08, 2.08, 4.15, 3.59, 3.59, 2.08, 2.08, 2.08, 0.89, 0.50,
     &  0.50, 0.50, 1.38, 1.38, 1.38, 0.50, 2.75, 2.75, 0.50, 2.75,
     &  0.50, 1.38, 0.50, 1.38, 1.38, 0.50, 2.75, 1.38, 0.50, 5.98,
     &  4.70, 2.08, 0.55, 0.50, 0.50, 0.50, 0.50, 2.08, 2.08, 2.08/
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  ISPSPE/5,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
     &         34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,
     &         52,53,54,55,56,57,59,60,61,62,63,64,65,66,67,68,69,70,
     &         71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87/
     &  ,BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,
     & HHTMAX/23.0,27.0,2*21.0,22.0,20.0,24.0,
     &  2*18.0,17.0,22.0,
     &  9*20.0,
     &  10*20.0,
     &  10*20.0,
     &  10*20.0,
     &  10*20.0,
     &  10*20.0,
     &  10*20.0,
     &  10*20.0/,
C
     &  IFORCD/103,104,105,106,621,110,113,114,116,117,
     &         118,109,111,112,412,402,108,102,115,  0/,
     &  IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &           4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C
      DATA ((OCURHT(I,J),I=1,16),J=1,2)/ 32*0.0 /

C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C
      DATA ((OCURNF(I,J),I=1,20),J=1,2)/ 40*0. /
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'FR ',   'JU ',   'PI ',   'PU ',   'SP ',   'SA ',   'SR ',
     & 'LL ',   'TM ',   'PP ',   'PD ',   'WP ',   'LP ',   'VP ',
     & 'BY ',   'PC ',   'HM ',   'FM ',   'BE ',   'RM ',   'SV ',
     & 'SM ',   'BU ',   'BB ',   'SB ',   'AH ',   'HI ',   'CA ',
     & 'HB ',   'RD ',   'DW ',   'PS ',   'AB ',   'AS ',   'WA ',
     & 'BA ',   'GA ',   'HL ',   'LB ',   'HA ',   'HY ',   'BN ',
     & 'WN ',   'SU ',   'YP ',   'MG ',   'CT ',   'MS ',   'MV ',
     & 'ML ',   'AP ',   'MB ',   'WT ',   'BG ',   'TS ',   'HH ',
     & 'SD ',   'RA ',   'SY ',   'CW ',   'BT ',   'BC ',   'WO ',
     & 'SO ',   'SK ',   'CB ',   'TO ',   'LK ',   'OV ',   'BJ ',
     & 'SN ',   'CK ',   'WK ',   'CO ',   'RO ',   'QS ',   'PO ',
     & 'BO ',   'LO ',   'BK ',   'WI ',   'SS ',   'BW ',   'EL ',
     & 'WE ',   'AE ',   'RL ',   'OS ',   'OH ',   'OT '/
C
      DATA FIAJSP /
     & '010',   '057',   '090',   '107',   '110',   '111',   '115',
     & '121',   '123',   '126',   '128',   '129',   '131',   '132',
     & '221',   '222',   '260',   '311',   '313',   '316',   '317',
     & '318',   '330',   '370',   '372',   '391',   '400',   '450',
     & '460',   '471',   '491',   '521',   '531',   '540',   '541',
     & '543',   '544',   '552',   '555',   '580',   '591',   '601',
     & '602',   '611',   '621',   '650',   '651',   '652',   '653',
     & '654',   '660',   '680',   '691',   '693',   '694',   '701',
     & '711',   '721',   '731',   '740',   '743',   '762',   '802',
     & '806',   '812',   '813',   '819',   '820',   '822',   '824',
     & '825',   '826',   '827',   '832',   '833',   '834',   '835',
     & '837',   '838',   '901',   '920',   '931',   '950',   '970',
     & '971',   '972',   '975',   '298',   '998',   '999'/
C
      DATA PLNJSP /
     & 'ABIES ','JUNIP ','PICEA ','PICL  ','PIEC2 ','PIEL  ','PIGL2 ',
     & 'PIPA2 ','PIPU5 ','PIRI  ','PISE  ','PIST  ','PITA  ','PIVI2 ',
     & 'TADI2 ','TAAS  ','TSUGA ','ACBA3 ','ACNE2 ','ACRU  ','ACSA2 ',
     & 'ACSA3 ','AESCU ','BETUL ','BELE  ','CACA18','CARYA ','CATAL ',
     & 'CELTI ','CECA4 ','COFL2 ','DIVI5 ','FAGR  ','FRAXI ','FRAM2 ',
     & 'FRNI  ','FRPE  ','GLTR  ','GOLA  ','HALES ','ILOP  ','JUCI  ',
     & 'JUNI  ','LIST2 ','LITU  ','MAGNO ','MAAC  ','MAGR4 ','MAVI2 ',
     & 'MAMA2 ','MALUS ','MORUS ','NYAQ2 ','NYSY  ','NYBI  ','OSVI  ',
     & 'OXAR  ','PEBO  ','PLOC  ','POPUL ','POGR4 ','PRSE2 ','QUAL  ',
     & 'QUCO2 ','QUFA  ','QUPA5 ','QULA2 ','QULA3 ','QULY  ','QUMA3 ',
     & 'QUMI  ','QUMU  ','QUNI  ','QUPR2 ','QURU  ','QUSH  ','QUST  ',
     & 'QUVE  ','QUVI  ','ROPS  ','SALIX ','SAAL5 ','TILIA ','ULMUS ',
     & 'ULAL  ','ULAM  ','ULRU  ','2TE   ','2TD   ','2TREE '/
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
      DATA (NSP(I,1),I= 1,90) /
     & 'FR1','JU1','PI1','PU1','SP1','SA1','SR1','LL1',
     & 'TM1','PP1','PD1','WP1','LP1','VP1','BY1','PC1','HM1','FM1',
     & 'BE1','RM1','SV1','SM1','BU1','BB1','SB1','AH1','HI1','CA1',
     & 'HB1','RD1','DW1','PS1','AB1','AS1','WA1','BA1','GA1','HL1',
     & 'LB1','HA1','HY1','BN1','WN1','SU1','YP1','MG1','CT1','MS1',
     & 'MV1','ML1','AP1','MB1','WT1','BG1','TS1','HH1','SD1','RA1',
     & 'SY1','CW1','BT1','BC1','WO1','SO1','SK1','CB1','TO1','LK1',
     & 'OV1','BJ1','SN1','CK1','WK1','CO1','RO1','QS1','PO1','BO1',
     & 'LO1','BK1','WI1','SS1','BW1','EL1','WE1','AE1','RL1','OS1',
     & 'OH1','OT1'/
      DATA (NSP(I,2),I= 1,90) /
     & 'FR2','JU2','PI2','PU2','SP2','SA2','SR2','LL2',
     & 'TM2','PP2','PD2','WP2','LP2','VP2','BY2','PC2','HM2','FM2',
     & 'BE2','RM2','SV2','SM2','BU2','BB2','SB2','AH2','HI2','CA2',
     & 'HB2','RD2','DW2','PS2','AB2','AS2','WA2','BA2','GA2','HL2',
     & 'LB2','HA2','HY2','BN2','WN2','SU2','YP2','MG2','CT2','MS2',
     & 'MV2','ML2','AP2','MB2','WT2','BG2','TS2','HH2','SD2','RA2',
     & 'SY2','CW2','BT2','BC2','WO2','SO2','SK2','CB2','TO2','LK2',
     & 'OV2','BJ2','SN2','CK2','WK2','CO2','RO2','QS2','PO2','BO2',
     & 'LO2','BK2','WI2','SS2','BW2','EL2','WE2','AE2','RL2','OS2',
     & 'OH2','OT2'/
      DATA (NSP(I,3),I= 1,90) /
     & 'FR3','JU3','PI3','PU3','SP3','SA3','SR3','LL3',
     & 'TM3','PP3','PD3','WP3','LP3','VP3','BY3','PC3','HM3','FM3',
     & 'BE3','RM3','SV3','SM3','BU3','BB3','SB3','AH3','HI3','CA3',
     & 'HB3','RD3','DW3','PS3','AB3','AS3','WA3','BA3','GA3','HL3',
     & 'LB3','HA3','HY3','BN3','WN3','SU3','YP3','MG3','CT3','MS3',
     & 'MV3','ML3','AP3','MB3','WT3','BG3','TS3','HH3','SD3','RA3',
     & 'SY3','CW3','BT3','BC3','WO3','SO3','SK3','CB3','TO3','LK3',
     & 'OV3','BJ3','SN3','CK3','WK3','CO3','RO3','QS3','PO3','BO3',
     & 'LO3','BK3','WI3','SS3','BW3','EL3','WE3','AE3','RL3','OS3',
     & 'OH3','OT3'/
C----------
C     SPECIES LIST FOR SOUTHERN VARIANT.
C
C    1 = FIR SPECIES (FR)              ABIES SP.
C    2 = REDCEDAR SPECIES (JU)         JUNIPERUS SP.
C    3 = SPRUCE SPECIES (PI)           PICEA SP.
C    4 = SAND PINE (PU)                PINUS CLAUSA
C    5 = SHORTLEAF PINE (SP)           PINUS ECHINATA
C    6 = SLASH PINE (SA)               PINUS ELLIOTTII
C    7 = SPRUCE PINE (SR)              PINUS GLABRA
C    8 = LONGLEAF PINE (LL)            PINUS PALUSTRIS
C    9 = TABLE MOUNTAIN PINE (TM)      PINUS PUNGENS
C    10 = PITCH PINE (PP)              PINUS RIGIDA
C    11 = POND PINE (PD)               PINUS SEROTINA
C    12 = EASTERN WHITE PINE (WP)      PINUS STROBUS
C    13 = LOBLOLLY PINE (LP)           PINUS TAEDA
C    14 = VIRGINIA PINE (VP)           PINUS VIRGINIANA
C    15 = BALDCYPRESS (BY)             TAXODIUM DISTICHUM
C    16 = PONDCYPRESS (PC)             TAXODIUM DISTICHUM VAR. NUTANS
C    17 = HEMLOCK SPECIES (HM)         TSUGA SP.
C    18 = FLORIDA MAPLE (FM)           ACER BARBATUM
C    19 = BOXELDER (BE)                ACER NEGUNDO
C    20 = RED MAPLE (RM)               ACER RUBRUM
C    21 = SILVER MAPLE (SV)            ACER SACCHARINUM
C    22 = SUGAR MAPLE (SM)             ACER SACCHARUM
C    23 = BUCKEYE, HORSECHESTNUT SPECIES(BU)  AESCULUS SP.
C    24 = BIRCH SPECIES (BB)           BETULA SP.
C    25 = SWEET BIRCH, BLACK BIRCH (SB)BETULA LENTA
C    26 = AMERICAN HORNBEAM (AH)       CARPINUS CAROLINIANA
C    27 = HICKORY SPECIES (HI)         CARYA SP.
C    28 = CATALPA (CA)                 CATALPA SP.
C    29 = HACKBERRY SPECIES (HB)       CELTIS SP.
C    30 = EASTERN REDBUD (RD)          CERCIS CANADENSIS
C    31 = FLOWERING DOGWOOD (DW)       CORNUS FLORIDA
C    32 = COMMON PERSIMMON (PS)        DIOSPYROS VIRGINIANA
C    33 = AMERICAN BEECH (AB)          FAGUS GRANDIFOLIA
C    34 = ASH SPECIES(AS)              FRAXINUS SP.
C    35 = WHITE ASH (WA)               FRAXINUS AMERICANA
C    36 = BLACK ASH (BA)               FRAXINUS NIGRA
C    37 = GREEN ASH (GA)               FRAXINUS PENNSYLVANICA
C    38 = HONEYLOCUST (HL)             GLEDITSIA TRIACANTHOS
C    39 = LOBLOLLY-BAY (LB)            GORDONIA LASIANTHUS
C    40 = SILVERBELL (HA)              HALESIA SP.
C    41 = AMERICAN HOLLY (HY)          ILEX OPACA
C    42 = BUTTERNUT (BN)               JUGLANS CINEREA
C    43 = BLACK WALNUT (WN)            JUGLANS NIGRA
C    44 = SWEETGUM (SU)                LIQUIDAMBAR STYRACIFLUA
C    45 = YELLOW-POPLAR (YP)           LIRIODENDRON TULIPIFERA
C    46 = MAGNOLIA SPECIES (MG)        MAGNOLIA SP.
C    47 = CUCUMBERTREE (CT)            MAGNOLIA ACUMINATA
C    48 = SOUTHERN MAGNOLIA (MS)       MAGNOLIA GRANDIFLORA
C    49 = SWEETBAY (MV)                MAGNOLIA VIRGINIANA
C    50 = BIGLEAF MAGNOLIA (ML)        MAGNOLIA MACROPHYLLA
C    51 = APPLE SPECIES (AP)           MALUS SP.
C    52 = MULBERRY SPECIES (MB)        MORUS SP.
C    53 = WATER TUPELO (WT)            NYSSA AQUATICA
C    54 = BLACKGUM/ BLACK TUPELO (BG)  NYSSA SYLVATICA
C    55 = SWAMP TUPELO(TS)             NYSSA SYLVATICA VAR. BIFLORA
C    56 = EASTERN HOPHORNBEAM, (HH)    OSTRYA VIRGINIANA
C    57 = SOURWOOD (SD)                OXYDENDRUM ARBOREUM
C    58 = REDBAY (RA)                  PERSEA BORBONIA
C    59 = SYCAMORE (SY)                PLATANUS OCCIDENTALIS
C    60 = COTTONWOOD SPECIES (CW)      POPULUS SP.
C    61 = BIGTOOTH ASPEN (BT)          POPULUS GRANDIDENTATA
C    62 = BLACK CHERRY (BC)            PRUNUS SEROTINA
C    63 = WHITE OAK (WO)               QUERCUS ALBA
C    64 = SCARLET OAK (SO)             QUERCUS COCCINEA
C    65 = SOUTHERN RED OAK (SK)        QUERCUS FALCATA VAR. FALCATA
C    66 = CHERRYBARK OAK               QUERCUS FALCATA
C    67 = TURKEY OAK (TO)              QUERCUS LAEVIS
C    68 = LAUREL OAK (LK)              QUERCUS LAURIFOLIA
C    69 = OVERCUP OAK (OV)             QUERCUS LYRATA
C    70 = BLACKJACK OAK (BJ)           QUERCUS MARILANDICA
C    71 = SWAMP CHESTNUT OAK (SN)      QUERCUS MICHAUXII
C    72 = CHINKAPIN OAK (CK)           QUERCUS MUEHLENBERGII
C    73 = WATER OAK (WK)               QUERCUS NIGRA
C    74 = CHESTNUT OAK (CO)            QUERCUS PRINUS
C    75 = NORTHERN RED OAK (RO)        QUERCUS RUBRA
C    76 = SHUMARD OAK (QS)             QUERCUS SHUMARDII
C    77 = POST OAK (PO)                QUERCUS STELLATA
C    78 = BLACK OAK (BO)               QUERCUS VELUTINA
C    79 = LIVE OAK (LO)                QUERCUS VIRGINIANA
C    80 = BLACK LOCUST (BK)            ROBINIA PSUEDOACACIA
C    81 = WILLOW SPECIES (WI)          SALIX SP.
C    82 = SASSAFRAS (SS)               SASSAFRAS ALBIDUM
C    83 = BASSWOOD SPECIES (BW)        TILIA SP.
C    84 = ELM SPECIES(EL)              ULMUS SP.
C    85 = WINGED ELM (WE)              ULMUS ALATA
C    86 = AMERICAN ELM (AE)            ULMUS AMERICANA
C    87 = SLIPPERY ELM (RL)            ULMUS RUBRA
C    88 = OTHER SOFTWOOD SPECIES (OS)
C    89 = OTHER HARDWOOD SPECIES (OH)
C    90 = OTHER SPECIES(OT)
C----------
C   THE HT-DBH COEFFICIENTS ARE ASSIGNED IN THE **SITSET ROUTINE
C----------
C  RESIDUAL ERROR ESTIMATES WERE MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR; 5/10/91--WRW.
C----------
      DATA SIGMAR/
     & 0.451100,   0.529700,   0.451100,   0.542800,   0.498700,
     & 0.525100,   0.436700,   0.441000,   0.469300,   0.552500,
     & 0.592100,   0.493700,   0.468700,   0.469300,   0.551100,
     & 0.626700,   0.451100,   0.563200,   0.560800,   0.593000,
     & 0.593000,   0.475500,   0.537300,   0.569600,   0.569600,
     & 0.603200,   0.499300,   0.440100,   0.527600,   0.545300,
     & 0.538200,   0.516000,   0.480500,   0.595800,   0.422800,
     & 0.485600,   0.485600,   0.468200,   0.590800,   0.599600,
     & 0.546900,   0.571000,   0.571000,   0.577900,   0.518100,
     & 0.572700,   0.512600,   0.570300,   0.572700,   0.570300,
     & 0.505600,   0.505600,   0.569800,   0.535600,   0.588800,
     & 0.577300,   0.504700,   0.568700,   0.557000,   0.440100,
     & 0.440100,   0.578100,   0.440700,   0.382700,   0.430500,
     & 0.400000,   0.495700,   0.558600,   0.465900,   0.464900,
     & 0.491300,   0.505600,   0.466400,   0.429300,   0.404800,
     & 0.407400,   0.485300,   0.421900,   0.663500,   0.518700,
     & 0.450800,   0.450400,   0.549600,   0.644700,   0.528600,
     & 0.536400,   0.535000,   0.529700,   0.577300,   0.557600/
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
