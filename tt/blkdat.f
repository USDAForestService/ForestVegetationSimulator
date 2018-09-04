      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C TT $Id$
C----------
C
C     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
C
COMMONS
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
      INCLUDE 'GGCOM.F77'
C
C
      INCLUDE 'FVSSTDCM.F77'
C
C
COMMONS
C----------
      INTEGER I,J
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C
C  1 = WHITEBARK PINE              WB   PINUS ALBICAULIS
C  2 = LIMBER PINE                 LM   PINUS FLEXILIS
C  3 = DOUGLAS-FIR                 DF   PSEUDOTSUGA MENZIESII
C  4 = SINGLELEAF PINYON           PM   PINUS MONOPHYLLA
C  5 = BLUE SPRUCE                 BS   PICEA PUNGENS
C  6 = QUAKING ASPEN               AS   POPULUS TREMULOIDES
C  7 = LODGEPOLE PINE              LP   PINUS CONTORTA
C  8 = ENGELMANN SPRUCE            ES   PICEA ENGELMANNII
C  9 = SUBALPINE FIR               AF   ABIES LASIOCARPA
C 10 = PONDEROSA PINE              PP   PINUS PONDEROSA
C 11 = UTAH JUNIPER                UJ   JUNIPERUS OSTEOSPERMA
C 12 = ROCKY MOUNTAIN JUNIPER      RM   JUNIPERUS SCOPULORUM
C 13 = BIGTOOTH MAPLE              BI   ACER GRANDIDENTATUM
C 14 = ROCKY MOUNTAIN MAPLE        MM   ACER GLABRUM
C 15 = NARROWLEAF COTTONWOOD       NC   POPULUS ANGUSTIFOLIA
C 16 = CURLLEAF MOUNTAIN-MAHOGANY  MC   CERCOCARPUS LEDIFOLIUS
C 17 = OTHER SOFTWOODS             OS 
C 18 = OTHER HARDWOODS             OH
C----------
      DATA  BKRAT/MAXSP*0./
C
C     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
C
      DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./, RCOR2/MAXSP*1.0/
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C COMMON STATEMENT FOR ESCOMN VARIABLE.
C----------
      DATA XMIN/
     & 1.0, 1.0, 1.0, 0.5, 0.5, 6.0, 1.0, 0.5, 0.5, 1.0,
     & 0.5, 0.5, 0.5, 6.0, 3.0, 0.5, 0.5, 3.0/
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     & ISPSPE/6,13,14,15/,
     & BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,
     & HHTMAX/
     & 23.0, 27.0, 21.0,  6.0, 18.0, 20.0, 24.0, 18.0, 18.0, 17.0,
     &  6.0,  6.0,  6.0, 16.0, 16.0,  6.0, 22.0, 16.0/,
     & IFORCD/103,104,105,106,621,110,113,114,116,117,
     &        118,109,111,112,412,402,108,102,115,  0/,
     & IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &          4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
C     (DIMENSIONED (16,MAXSP) WITH THE 16 BEING THE HABITAT TYPE GROUP 
C      AS SHOWN IN TABLE 3, PG 6, GTR INT-279) WHICH DOES NOT PERTAIN
C      TO THE TT VARIANT)
C
      DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/ 288*0.0 /
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
C     (DIMENSIONED 20,MAXSP WITH THE 20 BEING 20 FOREST CODES SHOWN
C      IN ARRAY IFORCD ABOVE AND MAPPED AS SHOWN IN ARRAY IFORST)
C
      DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/ 360*0.0 /
C----------
C COMMON STATEMENT FOR PLOT VARIABLES.
C----------
C
      DATA JSP /
     & 'WB ',   'LM ',   'DF ',   'PM ',   'BS ',
     & 'AS ',   'LP ',   'ES ',   'AF ',   'PP ',
     & 'UJ ',   'RM ',   'BI ',   'MM ',   'NC ',   
     & 'MC ',   'OS ',   'OH '/
C
      DATA FIAJSP /
     & '101',   '113',   '202',   '133',   '096',
     & '746',   '108',   '093',   '019',   '122',
     & '065',   '066',   '322',   '321',   '749',   
     & '475',   '298',   '998'/
C
      DATA PLNJSP /
     & 'PIAL  ','PIFL2 ','PSME  ','PIMO  ','PIPU  ',
     & 'POTR5 ','PICO  ','PIEN  ','ABLA  ','PIPO  ',
     & 'JUOS  ','JUSC2 ','ACGR3 ','ACGL  ','POAN3 ',
     & 'CELE3 ','2TE   ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /
     > 'WB1','LM1','DF1','PM1','BS1','AS1','LP1','ES1','AF1','PP1',
     > 'UJ1','RM1','BI1','MM1','NC1','MC1','OS1','OH1',
     > 'WB2','LM2','DF2','PM2','BS2','AS2','LP2','ES2','AF2','PP2',
     > 'UJ2','RM2','BI2','MM2','NC2','MC2','OS2','OH2',
     > 'WB3','LM3','DF3','PM3','BS3','AS3','LP3','ES3','AF3','PP3',
     > 'UJ3','RM3','BI3','MM3','NC3','MC3','OS3','OH3'/
C----------
C COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     &   4.1920,  4.1920,  4.5175,  3.2000,  4.5822,
     &   4.4625,  4.4625,  4.5822,  4.3603,   4.993,
     &   3.2000,  3.2000,  4.7000,  4.4421,  4.4421,  
     &   5.1520,  4.1920,  4.4421/
      DATA HT2/
     &  -5.1651, -5.1651, -6.5129, -5.0000, -6.4818,
     &  -5.2223, -5.2223, -6.4818, -5.2148, -12.430,
     &  -5.0000, -5.0000, -6.3260, -6.5405, -6.5405,
     & -13.5760, -5.1651, -6.5405/
C
C  RESIDUAL ERROR ESTIMATES WERE MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR.
C
      DATA SIGMAR/
     &  0.46710, 0.46710, 0.48664,     0.2, 0.47645,
     &  0.48657, 0.48657, 0.47645, 0.44065,   0.230,
     &      0.2,     0.2,  0.5107,  0.3750,     0.2,  
     &   0.5357, 0.46710,      0.2/
C
      DATA BREAK/
     &   3.,   3.,   3.,  99.,   3.,   3.,   3.,   3.,   3.,   3.,
     &  99.,  99.,  99.,   3.,   1.,  99.,   3.,  1./
C----------
C DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
C----------
      DATA B0ACCF/
     &  1.17527,  1.17527, -4.35709,       0., -0.55052,
     &       0., -0.90086, -0.55052, -4.35709,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,  1.17527,       0./
      DATA B1ACCF/
     & -0.42124, -0.42124,  0.67307,       0., -0.02858,
     &       0.,  0.16996, -0.02858,  0.67307,       0.,
     &       0.,       0.,       0.,       0.,       0., 
     &       0., -0.42124,       0./
      DATA B0BCCF/
     & -2.56002, -2.56002, -2.49682,       0., -2.26007,
     &       0., -1.50963, -2.26007, -2.49682,       0.,
     &       0.,       0.,       0.,       0.,       0., 
     &       0., -2.56002,       0./
      DATA B1BCCF/
     & -0.58642, -0.58642, -0.51938,       0., -0.67115,
     &       0., -0.61825, -0.67115, -0.51938,       0.,
     &       0.,       0.,       0.,       0.,       0.,  
     &       0., -0.58642,       0./
      DATA B0ASTD/
     &  1.08720,  1.08720,  1.13785,       0.,  1.09730,
     &       0.,  1.00749,  1.09730,  1.13785,       0.,
     &       0.,       0.,       0.,       0.,       0., 
     &       0.,  1.08720,       0./
      DATA B1BSTD/
     & -0.00230, -0.00230, -0.00185,       0., -0.00130,
     &       0., -0.00435, -0.00130, -0.00185,       0.,
     &       0.,       0.,       0.,       0.,       0., 
     &       0., -0.00230,       0./
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
