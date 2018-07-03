      BLOCK DATA BLKDAT
      IMPLICIT NONE
C----------
C NC $Id: blkdat.f 0000 2018-02-14 00:00:00Z gedixon $
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
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ECON.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'KEYCOM.F77'
C
C
      INCLUDE 'PDEN.F77'
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
      DATA COR2 /MAXSP*1./,HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/,
     &     BKRAT/MAXSP*0./
C
      DATA TREFMT /
     >'(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,
     >2I1,F3.0)' /
C
      DATA YR /  5.0 /, IRECNT/ 0 /,ICCODE/0/
C
      DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
C----------
C     DATA STATEMENT FOR ESCOMN
C----------
      DATA XMIN/ 1., 1., 1., .5, 1., .5, .5, 1., .5, 1., 1./
      DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/,
     &  ISPSPE/5,7,8/,BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,
     &  1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,
     &  1.742,1.789/,HHTMAX/ 27., 31., 25., 25., 26., 24., 28., 20.,
     &                       20., 18., 26./,
     &  IFORCD/103,104,105,106,621,110,113,114,116,117,
     &         118,109,111,112,412,402,108,102,115,  0/,
     &  IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17,
     &           4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
C
C     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
      DATA ((OCURHT(I,J),I=1,16),J=1,2)/
     1  0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,1.0,1.0,0.0,1.0,0.0,
     2  13*1.0,                                             0.0,1.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=3,4)/ 15*1.0, 0.0,
     4  0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=5,6)/ 9*0.0,1.0,6*0.0,
     6  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA ((OCURHT(I,J),I=1,16),J=7,8)/  16*1.0,
     8  0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
      DATA ((OCURHT(I,J),I=1,16),J=9,11)/
     9  0.0,0.0,0.0,0.0,1.0,1.0,1.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     O  1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,7*0.0,            16*0.0/
C
C     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
      DATA ((OCURNF(I,J),I=1,20),J=1,2)/
     1    0.,0.,0.,1.,1.,0.,1.,0.,0.,1.,3*0.0,   1.,0.,1.,0.,0.,0.,0.,
     2    0.,0.,3*1.0,   0.,1.,0.,1.,1.,3*0.0,   1.,0.,1.,1.,0.,1.,0./
      DATA ((OCURNF(I,J),I=1,20),J=3,4)/
     3    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1.,
     4    0.,0.,3*1.0,   0.,1.,0.,0.,1.,3*0.0,   1.,0.,1.,1.,0.,1.,1./
      DATA ((OCURNF(I,J),I=1,20),J=5,6)/
     5    3*0.0,   1.,1.,0.,1.,6*0.0,            1.,0.,1.,4*0.0,
     6    3*0.0,   1.,1.,0.,1.,6*0.0,            1.,0.,1.,1.,3*0.0/
      DATA ((OCURNF(I,J),I=1,20),J=7,8)/
     7    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1.,
     8    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1./
      DATA ((OCURNF(I,J),I=1,20),J=9,11)/
     9    0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1.,
     O    0.,0.,3*1.0,0.,1.,0.,1.,4*0.0,1.,0.,1.,1.,0.,1.,1.,  20*0.0/
C----------
C     COMMON STATEMENT FOR PLOT VARIABLES.
C----------
C     SPECIES LIST FOR KLAMATH MOUNTAINS VARIANT.
C
C     1 = OTHER CONIFERS (OS)
C     2 = SUGAR PINE (SP)                PINUS LAMBERTIANA
C     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII
C     4 = WHITE FIR (WF)                 ABIES CONCOLOR
C     5 = MADRONE (MA)                   ARBUTUS MENZIESII
C     6 = INCENSE CEDAR (IC)             LIBOCEDRUS DECURRENS
C     7 = CALIFORNIA BLACK OAK (BO)      QUERQUS KELLOGGII
C     8 = TANOAK (TO)                    LITHOCARPUS DENSIFLORUS
C     9 = RED FIR (RF)                   ABIES MAGNIFICA
C    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA
C    11 = OTHER HARDWOOD (OH)
C----------
      DATA JSP /
     & 'OS ',   'SP ',   'DF ',   'WF ',   'MA ',   'IC ',   'BO ',
     & 'TO ',   'RF ',   'PP ',   'OH '/
C
      DATA FIAJSP /
     & '298',   '117',   '202',   '015',   '361',   '081',   '818',
     & '631',   '020',   '122',   '998'/
C
      DATA PLNJSP /
     & '2TE   ','PILA  ','PSME  ','ABCO  ','ARME  ','CADE27','QUKE  ',
     & 'LIDE3 ','ABMA  ','PIPO  ','2TD   '/
C
      DATA JTYPE /130,170,250,260,280,290,310,320,330,420,
     &            470,510,520,530,540,550,570,610,620,640,
     &            660,670,680,690,710,720,730,830,850,999,92*0 /
C
      DATA NSP /'OS1','SP1','DF1','WF1','MA1','IC1','BO1','TO1','RF1',
     > 'PP1','OH1','OS2','SP2','DF2','WF2','MA2','IC2','BO2','TO2',
     > 'RF2','PP2','OH2',
     > 'OS3','SP3','DF3','WF3','MA3','IC3','BO3','TO3','RF3','PP3','OH3'
     > /
C----------
C   COMMON STATEMENT FOR COEFFS VARIABLES
C----------
      DATA HT1/
     & 4.78737,4.74961,4.78737,
     & 4.80268,4.73881,4.89619,
     & 4.80420,4.66181,4.83642,
     & 4.23251,4.66181/
      DATA HT2/
     & -7.31698,-7.19103,-7.31698,-8.40657,-9.44913,
     &-12.55873,-9.92422,-8.33117,-7.04795,-8.31711,
     & -8.33117/
C----------
C  RESIDUAL ERROR ESTIMATES MULTIPLIED BY 0.75 TO APPROXIMATE
C  CORRECTION FOR MEASUREMENT ERROR:  6/11/91 WRW.
C----------
      DATA SIGMAR/
C    & 0.4400 , 0.3617 , 0.4400 , 0.4400 , 0.4408 , 0.4684,
C    & 0.4721 , 0.4744 , 0.4182 , 0.2606 , 0.4744 /
     & 0.3300,  0.2713,  0.3300,  0.3300,  0.3306,  0.3513,
     & 0.3541,  0.3558,  0.3136,  0.1954,  0.3558 /
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
