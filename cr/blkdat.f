      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C CR $Id$
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
      INCLUDE 'GGCOM.F77'
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
      INCLUDE 'KEYCOM.F77'
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
C
C----------
      INTEGER I,J
C----------
C  SPECIES ORDER:
C   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
C  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
C  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
C  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
C
C  SPECIES EXPANSION:
C  UJ,AJ,RM,OJ,ER USE CR JU                              
C  NC,PW USE CR CO
C  GO,AW,EM,BK,SO USE CR OA                             
C  PB USES CR AS                              
C  PM,PD,AZ USE CR PI
C  CI USES CR PP                              
C----------
C    AL COMMON                  FIA SCIENTIFIC                         
C  # CD NAME                    CD  NAME                               
C -- -- ---------------------   --- -----------------------------------
C  1 AF SUBALPINE FIR           019 ABIES LASIOCARPA 
C  2 CB CORKBARK FIR            018 ABIES LASIOCARPA var. ARIZONICA
C  3 DF DOUGLAS-FIR             202 PSEUDOTSUGA MENZIESII
C  4 GF GRAND FIR               017 ABIES GRANDIS
C  5 WF WHITE FIR               015 ABIES CONCOLOR
C  6 MH MOUNTAIN HEMLOCK        264 TSUGA MERTENSIANA
C  7 RC WESTERN REDCEDAR        242 THUJA PLICATA
C  8 WL WESTERN LARCH           073 LARIX OCCIDENTALIS
C  9 BC BRISTLECONE PINE        102 PINUS ARISTATA
C 10 LM LIMBER PINE             113 PINUS FLEXILIS 
C 11 LP LODGEPOLE PINE          108 PINUS CONTORTA
C 12 PI TWONEEDLE PINYON        106 PINUS EDULIS
C 13 PP PONDEROSA PINE          122 PINUS PONDEROSA
C 14 WB WHITEBARK PINE          101 PINUS ALBICAULIS
C 15 SW SOUTHWESTERN WHITE PINE 114 PINUS STROBIFORMUS
C 16 UJ UTAH JUNIPER            065 JUNIPERUS OSTEOSPERMA
C 17 BS BLUE SPRUCE             096 PICEA PUNGENS
C 18 ES ENGELMANN SPRUCE        093 PICEA ENGELMANNII
C 19 WS WHITE SPRUCE            094 PICEA GLAUCA
C 20 AS QUAKING ASPEN           746 POPULUS TREMULOIDES
C 21 NC NARROWLEAF COTTONWOOD   749 POPULUS ANGUSTIFOLIA
C 22 PW PLAINS COTTONWOOD       745 POPULUS DELTOIDES var. MONOLIFERA
C 23 GO GAMBEL OAK              814 QUERCUS GAMBELII
C 24 AW ARIZONA WHITE OAK       803 QUERCUS ARIZONICA
C 25 EM EMORY OAK               810 QUERCUS EMORYI
C 26 BK BUR OAK                 823 QUERCUS MACROCARPA
C 27 SO SILVERLEAF OAK          843 QUERCUS HYPOLEUCOIDES
C 28 PB PAPER BIRCH             375 BETULA PAPYRIFERA
C 29 AJ ALLIGATOR JUNIPER       063 JUNIPERUS DEPPEANA
C 30 RM ROCKY MOUNTAIN JUNIPER  066 JUNIPERUS SCOPULORUM
C 31 OJ ONESEED JUNIPER         069 JUNIPERUS MONOSPERMA
C 32 ER EASTERN REDCEDAR        068 JUNIPERUS VIRGINIANA
C 33 PM SINGLELEAF PINYON       133 PINUS MONOPHYLLA
C 34 PD BORDER PINYON           134 PINUS DISCOLOR
C 35 AZ ARIZONA PINYON PINE     143 PINUS MONOPHYLLA var. FALLAX
C 36 CI CHIHUAHUAN PINE         118 PINUS LEIOPHYLLA 
C 37 OS OTHER SOFTWOOD          299
C 38 OH OTHER HARDWOOD          998
C----------
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/,
     &     BKRAT/MAXSP*0./
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C     DATA STATEMENT FOR ESCOMN
C----------
      DATA XMIN/ 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5,
     &           1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 0.5, 3.0,
     &           3.0, 3.0, 0.5, 0.5, 0.5, 0.5, 0.5, 3.0, 0.5, 0.5,
     &           0.5, 0.5, 0.5, 0.5, 0.5, 1.0, 1.0, 0.5 /
C
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
C
      DATA ISPSPE/20,21,22,23,24,25,26,27,28,29,36/
C
      DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,
     &  1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
C
      DATA HHTMAX/ 7.0, 7.0,10.0, 7.0, 7.0,10.0, 9.0,10.0, 9.0, 9.0,
     &            10.0, 6.0,10.0, 9.0, 9.0, 6.0, 7.0, 7.0, 7.0,16.0,
     &            16.0,16.0,10.0,10.0,10.0,10.0,10.0,16.0, 6.0, 6.0,
     &             6.0, 6.0, 6.0, 6.0, 6.0,10.0, 9.0,12.0/
C
      DATA IFORCD/ 202, 203, 204, 206, 207, 209, 210, 211, 212, 213,
     & 214, 215, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 312/
C
      DATA IFORST/  23*1 /
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     (DIMENSIONED (16,MAXSP) WITH THE 16 BEING THE HABITAT TYPE GROUP 
C      AS SHOWN IN TABLE 3, PG 6, GTR INT-279) WHICH DOES NOT PERTAIN
C      TO THE CR VARIANT)
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0,
     &  16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     (DIMENSIONED 23,MAXSP WITH THE 23 BEING 23 FOREST CODES SHOWN
C      IN ARRAY IFORCD ABOVE AND MAPPED AS SHOWN IN ARRAY IFORST)
C
      DATA ((OCURNF(I,J),I=1,23),J=1,MAXSP)/
     &  23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0,
     &  23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0,
     &  23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0,
     &  23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0,
     &  23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'AF ',   'CB ',   'DF ',   'GF ',   'WF ',   'MH ',   'RC ',
     & 'WL ',   'BC ',   'LM ',   'LP ',   'PI ',   'PP ',   'WB ',
     & 'SW ',   'UJ ',   'BS ',   'ES ',   'WS ',   'AS ',   'NC ',
     & 'PW ',   'GO ',   'AW ',   'EM ',   'BK ',   'SO ',   'PB ',
     & 'AJ ',   'RM ',   'OJ ',   'ER ',   'PM ',   'PD ',   'AZ ',
     & 'CI ',   'OS ',   'OH '/
C
      DATA FIAJSP /
     & '019',   '018',   '202',   '017',   '015',   '264',   '242',
     & '073',   '102',   '113',   '108',   '106',   '122',   '101',
     & '114',   '065',   '096',   '093',   '094',   '746',   '749',
     & '745',   '814',   '803',   '810',   '823',   '843',   '375',
     & '063',   '066',   '069',   '068',   '133',   '134',   '143',
     & '118',   '299',   '998'/
C
      DATA PLNJSP /
     & 'ABLA  ','ABLAA ','PSME  ','ABGR  ','ABCO  ','TSME  ','THPL  ',
     & 'LAOC  ','PIAR  ','PIFL2 ','PICO  ','PIED  ','PIPO  ','PIAL  ',
     & 'PIST3 ','JUOS  ','PIPU  ','PIEN  ','PIGL  ','POTR5 ','POAN3 ',
     & 'PODEM ','QUGA  ','QUAR  ','QUEM  ','QUMA2 ','QUHY  ','BEPA  ',
     & 'JUDE2 ','JUSC2 ','JUMO  ','JUVI  ','PIMO  ','PIDI3 ','PIMOF ',
     & 'PILE  ','2TN   ','2TB   '/
C
      DATA JTYPE /122*0/
C
      DATA NSP    /'AF1','CB1','DF1','GF1','WF1','MH1','RC1','WL1',
     &             'BC1','LM1','LP1','PI1','PP1','WB1','SW1','UJ1',
     &             'BS1','ES1','WS1','AS1','NC1','PW1','GO1','AW1',
     &             'EM1','BK1','SO1','PB1','AJ1','RM1','OJ1','ER1',
     &             'PM1','PD1','AZ1','CI1','OS1','OH1',
C
     &             'AF2','CB2','DF2','GF2','WF2','MH2','RC2','WL2',
     &             'BC2','LM2','LP2','PI2','PP2','WB2','SW2','UJ2',
     &             'BS2','ES2','WS2','AS2','NC2','PW2','GO2','AW2',
     &             'EM2','BK2','SO2','PB2','AJ2','RM2','OJ2','ER2',
     &             'PM2','PD2','AZ2','CI2','OS2','OH2',
C
     &             'AF3','CB3','DF3','GF3','WF3','MH3','RC3','WL3',
     &             'BC3','LM3','LP3','PI3','PP3','WB3','SW3','UJ3',
     &             'BS3','ES3','WS3','AS3','NC3','PW3','GO3','AW3',
     &             'EM3','BK3','SO3','PB3','AJ3','RM3','OJ3','ER3',
     &             'PM3','PD3','AZ3','CI3','OS3','OH3'/
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     &   4.4717,   4.4717,   4.5879,   5.0271,   4.3008,   4.8740,
     &   5.1631,   5.1631,   4.1920,   4.1920,   4.3767,   4.1920,
     &   4.6024,   4.1920,   5.1999,   4.1920,   4.5293,   4.5293,
     &   4.5293,   4.4421,   4.4421,   4.4421,   4.1920,   4.1920,
     &   4.1920,   4.1920,   4.1920,   4.4421,   4.1920,   4.1920,
     &   4.1920,   4.1920,   4.1920,   4.1920,   4.1920,   4.6024,
     &   4.2597,   4.4421/
      DATA HT2/
     &  -6.7387,  -6.7387,  -8.9277, -11.2168,  -6.8139, -10.4050,
     &  -9.2566,  -9.2566,  -5.1651,  -5.1651,  -6.1281,  -5.1651,
     & -11.4693,  -5.1651,  -9.2672,  -5.1651,  -7.7725,  -7.7725,
     &  -7.7725,  -6.5405,  -6.5405,  -6.5405,  -5.1651,  -5.1651,
     &  -5.1651,  -5.1651,  -5.1651,  -6.5405,  -5.1651,  -5.1651,
     &  -5.1651,  -5.1651,  -5.1651,  -5.1651,  -5.1651, -11.4693,
     &  -9.3949,  -6.5405/
C
      DATA REGNBK/0.499/
C
      DATA S0/55329D0/,SS/55329./
C
      DATA LSCRN,JOSCRN/.FALSE.,6/
C
      DATA JOSUME/13/
C
      DATA KOLIST,FSTOPEN /27,.FALSE./
C
C----------
C   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
C   VARIABLES B0ACCF B1ACCF B0BCCF B1BCCF B0ASTD B1BSTD
C   ARE ONLY USED IN THE EM & TT VARIANTS
C----------
      DATA B0ACCF/ MAXSP*0.0 /
      DATA B1ACCF/ MAXSP*0.0 /
      DATA B0BCCF/ MAXSP*0.0 /
      DATA B1BCCF/ MAXSP*0.0 /
      DATA B0ASTD/ MAXSP*0.0 /
      DATA B1BSTD/ MAXSP*0.0 /
C----------
C COMMON STATEMENT FOR GGCOM VARIABLES
C----------
      DATA IGFOR/ 13 /
      DATA BREAK/
     &   1.,   1.,   1.,   3.,   1.,   3.,   3.,   3.,  99.,   2.,
     &   1.,  99.,   1.,   3.,   1.,  99.,   1.,   1.,   1.,   1.,
     &   1.,   1.,  99.,  99.,  99.,  99.,  99.,   1.,  99.,  99.,
     &  99.,  99.,  99.,  99.,  99.,   1.,   1.,   1./
      DATA SITELO/
     &  40.,  30.,  40.,  30.,  40.,  40.,  20.,  40.,  20.,  10.,
     &  30.,   6.,  30.,  20.,  30.,   6.,  30.,  40.,  30.,  20.,
     &  30.,  30.,   6.,   6.,   6.,   6.,   6.,  20.,   6.,   6.,
     &   6.,   6.,   6.,   6.,   6.,  30.,  30.,  20./
      DATA SITEHI/
     & 105., 100., 120., 130., 105.,  70., 125., 120.,  60.,  60.,
     &  95.,  40., 100.,  60., 130.,  30., 110., 120.,  85., 100.,
     & 120., 120.,  40.,  40.,  40.,  40.,  40., 100.,  30.,  30.,
     &  30.,  30.,  40.,  40.,  40., 100.,  95., 100./
C
      END
