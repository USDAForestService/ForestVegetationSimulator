      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C WS $Id$
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
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'COEFFS.F77'
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
C     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
C
C     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
C     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
C     3 = WHITE FIR (WF)                    ABIES CONCOLOR
C     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
C     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
C     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
C     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
C     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
C     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
C    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
C    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
C    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
C    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
C    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
C    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
C    16 = COULTER PINE (CP)                 PINUS COULTERI
C    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
C    18 = MONTEREY PINE (MP)                PINUS RADIATA
C    19 = GRAY PINE (GP)                    PINUS SABINIANA
C         (OR CALIFORNIA FOOTHILL PINE)
C    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
C    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
C    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
C    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
C    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
C    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
C    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
C    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
C    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
C    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
C    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
C    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
C    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
C         (OR CALIFORNIA WHITE OAK)
C    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
C    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
C    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
C    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
C    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
C    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
C    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
C    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
C    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
C    42 = OTHER SOFTWOODS (OS)
C    43 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C    FROM EXISTING WS EQUATIONS --
C      USE 1(SP) FOR 11(WP) AND 24(MH) 
C      USE 2(DF) FOR 22(BD)
C      USE 3(WF) FOR 13(SF)
C      USE 4(GS) FOR 23(RW)
C      USE 8(PP) FOR 18(MP)
C      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
C      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
C                     43(OH)
C
C    FROM CA VARIANT --
C      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE), 
C                       25(WJ), 26(WJ), AND 27(CJ)
C      USE CA12(LP) FOR 9(LP) AND 10(WB)
C
C    FROM SO VARIANT --
C      USE SO30(MC) FOR 41(MC)
C
C    FROM UT VARIANT --
C      USE UT17(GB) FOR 21(GB)
C----------
C TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C----------
      DATA BKRAT/MAXSP*0./
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
C COMMON STATEMENT FOR ESCOMN VARIABLES
C----------
      DATA XMIN/
     & 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 1.0,
     & 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0,
     & 0.5, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0,
     & 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
     & 1.0, 2.0, 2.0/
      
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
C
      DATA ISPSPE/23,28,29,30,31,32,33,34,35,36,37,38,39,40/
C
      DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,
     &  1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
C
      DATA HHTMAX/
     & 27.0, 21.0, 21.0, 22.0, 20.0, 18.0, 18.0, 17.0, 20.0, 20.0, 
     & 27.0, 20.0, 21.0, 20.0, 20.0, 20.0, 20.0, 17.0, 20.0, 20.0, 
     &  9.0, 21.0, 22.0, 27.0, 20.0, 20.0, 20.0, 24.0, 24.0, 24.0, 
     & 24.0, 24.0, 24.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0, 24.0, 
     & 20.0, 23.0, 24.0/ 
C
      DATA IFORCD/103,104,105,106,621,110,113,114,116,117,
     &            118,109,111,112,412,402,108,102,115,  0/
C
      DATA IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &              4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     (DIMENSIONED (16,MAXSP) WITH THE 16 BEING THE HABITAT TYPE GROUP 
C      AS SHOWN IN TABLE 3, PG 6, GTR INT-279) WHICH DOES NOT PERTAIN
C      TO THE WS VARIANT)
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/ 688*0.0 /
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     (DIMENSIONED 20,MAXSP WITH THE 20 BEING 20 FOREST CODES SHOWN
C      IN ARRAY IFORCD ABOVE AND MAPPED AS SHOWN IN ARRAY IFORST)
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/ 860*0.0 /
C----------
C COMMON STATEMENT FOR PLOT VARIABLES.
C----------
      DATA JSP /
     & 'SP ',   'DF ',   'WF ',   'GS ',   'IC ',   
     & 'JP ',   'RF ',   'PP ',   'LP ',   'WB ',   
     & 'WP ',   'PM ',   'SF ',   'KP ',   'FP ',   
     & 'CP ',   'LM ',   'MP ',   'GP ',   'WE ',   
     & 'GB ',   'BD ',   'RW ',   'MH ',   'WJ ',   
     & 'UJ ',   'CJ ',   'LO ',   'CY ',   'BL ',   
     & 'BO ',   'VO ',   'IO ',   'TO ',   'GC ',   
     & 'AS ',   'CL ',   'MA ',   'DG ',   'BM ',   
     & 'MC ',   'OS ',   'OH '/
C
      DATA FIAJSP /
     & '117',   '202',   '015',   '212',   '081',   
     & '116',   '020',   '122',   '108',   '101',   
     & '119',   '133',   '011',   '103',   '104',   
     & '109',   '113',   '124',   '127',   '137',   
     & '142',   '201',   '211',   '264',   '064',   
     & '065',   '062',   '801',   '805',   '807',   
     & '818',   '821',   '839',   '631',   '431',   
     & '746',   '981',   '361',   '492',   '312',   
     & '475',   '298',   '998'/
C
      DATA PLNJSP /
     & 'PILA  ','PSME  ','ABCO  ','SEGI2 ','CADE27',   
     & 'PIJE  ','ABMA  ','PIPO  ','PICO  ','PIAL  ',   
     & 'PIMO3 ','PIMO  ','ABAM  ','PIAT  ','PIBA  ',   
     & 'PICO3 ','PIFL2 ','PIRA2 ','PISA2 ','PIWA  ',   
     & 'PILO  ','PSMA  ','SESE3 ','TSME  ','JUOC  ',   
     & 'JUOS  ','JUCA7 ','QUAG  ','QUCH2 ','QUDO  ',   
     & 'QUKE  ','QULO  ','QUWI2 ','LIDE3 ','CHCHC4',   
     & 'POTR5 ','UMCA  ','ARME  ','CONU4 ','ACMA3 ',   
     & 'CELE3 ','2TE   ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /
     & 'SP1','DF1','WF1','GS1','IC1','JP1','RF1','PP1','LP1','WB1',
     & 'WP1','PM1','SF1','KP1','FP1','CP1','LM1','MP1','GP1','WE1',
     & 'GB1','BD1','RW1','MH1','WJ1','UJ1','CJ1','LO1','CY1','BL1',
     & 'BO1','VO1','IO1','TO1','GC1','AS1','CL1','MA1','DG1','BM1',
     & 'MC1','OS1','OH1',
     & 'SP2','DF2','WF2','GS2','IC2','JP2','RF2','PP2','LP2','WB2',
     & 'WP2','PM2','SF2','KP2','FP2','CP2','LM2','MP2','GP2','WE2',
     & 'GB2','BD2','RW2','MH2','WJ2','UJ2','CJ2','LO2','CY2','BL2',
     & 'BO2','VO2','IO2','TO2','GC2','AS2','CL2','MA2','DG2','BM2',
     & 'MC2','OS2','OH2',
     & 'SP3','DF3','WF3','GS3','IC3','JP3','RF3','PP3','LP3','WB3',
     & 'WP3','PM3','SF3','KP3','FP3','CP3','LM3','MP3','GP3','WE3',
     & 'GB3','BD3','RW3','MH3','WJ3','UJ3','CJ3','LO3','CY3','BL3',
     & 'BO3','VO3','IO3','TO3','GC3','AS3','CL3','MA3','DG3','BM3',
     & 'MC3','OS3','OH3'/
C
      DATA SIGMAR/
     &  0.347,  0.407,  0.347, 0.4408,  0.433,   
     &  0.289, 0.4182,  0.371, 0.4169, 0.4169,   
     &  0.347, 0.4392,  0.347, 0.4392, 0.4392,   
     & 0.4392, 0.4392,  0.371, 0.4392, 0.4392,   
     &    0.2,  0.407, 0.4408,  0.347, 0.4392,   
     & 0.4392, 0.4392, 0.4721, 0.4721, 0.4721,   
     & 0.4721, 0.4721, 0.4721, 0.4744, 0.4744,   
     & 0.4744, 0.4744, 0.4744, 0.4744, 0.4721,   
     & 0.5357,  0.313, 0.4721/   
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     & 4.86039, 4.86039, 4.86039, 4.86039, 4.86039, 
     & 4.86039, 4.86039, 4.86039,  4.8358,  4.8358, 
     & 4.86039,  4.6843, 4.86039,  4.6843,  4.6843, 
     &  4.6843,  4.6843, 4.86039,  4.6843,  4.6843, 
     &  4.1920, 4.86039, 4.86039, 4.86039,  4.6843, 
     &  4.6843,  4.6843, 4.80420, 4.80420, 4.80420, 
     & 4.80420, 4.80420, 4.80420, 4.80420, 4.80420, 
     & 4.80420, 4.80420, 4.80420, 4.80420, 4.80420, 
     &  5.1520, 4.86039, 4.80420/
C 
      DATA HT2/
     & -9.32795, -9.32795, -9.32795, -9.32795, -9.32795, 
     & -9.32795, -9.32795, -9.32795,  -9.2077,  -9.2077, 
     & -9.32795,  -6.5516, -9.32795,  -6.5516,  -6.5516, 
     &  -6.5516,  -6.5516, -9.32795,  -6.5516,  -6.5516, 
     &  -5.1651, -9.32795, -9.32795, -9.32795,  -6.5516, 
     &  -6.5516,  -6.5516, -9.92422, -9.92422, -9.92422, 
     & -9.92422, -9.92422, -9.92422, -9.92422, -9.92422, 
     & -9.92422, -9.92422, -9.92422, -9.92422, -9.92422, 
     & -13.5760, -9.32795, -9.92422/
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
