      SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
      IMPLICIT NONE
C----------
C CR $Id: dubscr.f 2121 2018-02-28 23:37:24Z gedixon $
C----------
C  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
C  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
C  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
C  MEASUREMENTS AND ARE LESS THAN 5.0 INCHES DBH.  FINALLY, IT IS
C  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
C  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
COMMONS
C----------
      EXTERNAL RANN
      REAL BCR0(11),BCR1(11),BCR2(11),BCR3(11),
     & CRSD(11),BCR5(11),BCR6(11),
     & BCR8(11),BCR9(11),BCR10(11)
      INTEGER MAP1(MAXSP),MAP2(MAXSP),MAP3(MAXSP),MAP4(MAXSP),
     &          MAP5(MAXSP)
      INTEGER ISPC,IMAP
      REAL TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
      REAL DANUW
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
      DATA MAP1/
     &  9,  9,  3,  9,  4,  2,  2,  2,  1,  1,
     &  2, 11, 10,  1,  1, 11,  7,  8,  8,  6,
     &  5,  5,  5,  5,  5,  5,  5,  6, 11, 11,
     & 11, 11, 11, 11, 11, 10,  2,  5/
      DATA MAP2/
     &  4,  4,  3,  4,  4,  2,  2,  2,  1,  1,
     &  2,  9, 10,  1,  1, 11,  7,  7,  7,  6,
     &  5,  5,  8,  8,  8,  8,  8,  6, 11, 11,
     & 11, 11,  9,  9,  9, 10,  2,  5/
      DATA MAP3/
     &  2,  2,  3,  2,  2,  2,  2,  2,  2,  2,
     &  7,  2, 10,  2,  2, 11,  8,  8,  8,  6,
     &  1,  1,  5,  5,  5,  5,  5,  6, 11, 11,
     & 11, 11,  2,  2,  2, 10,  2,  5/
      DATA MAP4/
     &  9,  9,  3,  9,  9,  2,  2,  2,  2,  2,
     &  7,  2,  2,  2,  2,  2,  8,  8,  8,  6,
     &  5,  5,  5,  5,  5,  5,  5,  6,  2,  2,
     &  2,  2,  2,  2,  2,  2,  2,  5/
      DATA MAP5/
     &  9,  9,  3,  9,  9,  2,  2,  2,  2,  2,
     &  7,  2, 10,  2,  2,  2,  8,  8,  8,  6,
     &  5,  5,  5,  5,  5,  5,  5,  6,  2,  2,
     &  2,  2,  2,  2,  2, 10,  2,  5/
C
      DATA BCR2/
     &   .000000,   .000000,   .022409,   .022409,   .022409,   .022409,
     &   .000000,   .022409,   .022409,   .000000,   .000000/
      DATA BCR1/
     &  -.209765,  -.209765,  -.093105,  -.093105,  -.093105,  -.093105,
     &  -.209765,  -.093105,  -.093105,  -.209765,   .000000/
      DATA BCR5/
     &   .011032,   .011032,   .000000,   .000000,   .000000,   .000000,
     &   .011032,   .000000,   .000000,   .011032,   .000000/
      DATA BCR6/
     &   .000000,   .000000,  -.045532,  -.045532,  -.045532,  -.045532,
     &   .000000,  -.045532,  -.045532,   .000000,   .000000/
      DATA BCR8/
     &   .017727,   .017727,   .000000,   .000000,   .000000,   .000000,
     &   .017727,   .000000,   .000000,   .017727,   .000000/
      DATA BCR3/
     &   .003359,   .003359,   .002633,   .002633,   .002633,   .002633,
     &   .003359,   .002633,   .002633,   .003359,   .000000/
      DATA BCR9/
     &  -.000053,  -.000053,   .000022,   .000022,   .000022,   .000022,
     &  -.000053,   .000022,   .000022,  -.000053,   .000000/
      DATA BCR10/
     &   .014098,   .014098,  -.013115,  -.013115,  -.013115,  -.013115,
     &   .014098,  -.013115,  -.013115,   .014098,   .000000/
      DATA BCR0/
     & -1.669490, -1.669490,  -.426688,  -.426688,  -.426688,  -.426688,
     & -1.669490,  -.426688,  -.426688, -1.669490,  -2.19723/
      DATA CRSD/
     &  .5000,.5000,.6957,.6957,.6957,.9310,
     &  .6124,.6957,.6957,.4942,0.200/
C-----------
C  CHECK FOR DEBUG.
C-----------
C     CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = TPCT
C----------
C  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
C  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
C  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
C----------
      IF(IMODTY .EQ. 1) THEN
        IMAP=MAP1(ISPC)
      ELSEIF(IMODTY .EQ. 2) THEN
        IMAP=MAP2(ISPC)
      ELSEIF(IMODTY .EQ. 3) THEN
        IMAP=MAP3(ISPC)
      ELSEIF(IMODTY .EQ. 4) THEN
        IMAP=MAP4(ISPC)
      ELSE
        IMAP=MAP5(ISPC)
      ENDIF
      CR = BCR2(IMAP)*H
     *   + BCR1(IMAP)*D
     *   + BCR5(IMAP)*TPCCF
     *   + BCR6(IMAP)*(AVH/H)
     *   + BCR8(IMAP)*AVH
     *   + BCR3(IMAP)*BA
     *   + BCR9(IMAP)*(BA*TPCCF)
     *   + BCR10(IMAP)*RMAI
     *   + BCR0(IMAP)
C----------
C  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
C  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
C  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
C  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.
C----------
      SD=CRSD(IMAP)
   10 FCR=BACHLO(0.0,SD,RANN)
      IF(ABS(FCR).GT.SD) GO TO 10
      IF(ABS(CR+FCR).GE.86.)CR=86.
      CR=1.0/(1.0+EXP(CR+FCR))
      IF(CR .GT. .95) CR = .950
      IF(CR .LT. .05) CR=.05
C     IF(DEBUG)WRITE(JOSTND,600)IMAP,D,H,TTBA,TPCCF,CR,FCR,RMAI,TAVH
C 600 FORMAT(' IN DUBSCR, IMAP=',I2,' DBH=',F4.1,' H=',F5.1,
C    & ' TTBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
C    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
      RETURN
      END
