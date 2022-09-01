      SUBROUTINE MORTS
      IMPLICIT NONE
C----------
C OP $Id$
C----------
C  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR
C  EACH TREE RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE
C  REPRESENTED BY THE TREE RECORD.
C  THIS ROUTINE IS CALLED FROM **GRINCR** WHEN CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **MORCON** IS CALLED TO LOAD SITE DEPENDENT
C  CONSTANTS.
C
C  IF THERE ARE SOME VALID ORGANON TREES IN THE STAND, THEN
C  ORGANON MORTALITY RATES FOR EACH TREE RECORD (REGARDLESS OF IORG(i)
C  VALUE) WERE LOADED IN THE CALL TO **EXECUTE** FROM **DGDRIV**. 
C  USE ORGANON RATES FOR ALL TREES; APPLY USER ADJUSTMENTS.
C
C  IF THERE ARE NO VALID ORGANON TREES, THEN ORGANON MORTALITY RATES HAVE 
C  NOT BEEN CALCULATED (**EXECUTE** WAS NOT CALLED); USE FVS MORTALITY LOGIC
C----------
C  SF , WF, GF, AF, RF,SS, NF, ES:     USE GF FROM NW ORGANON
C  YC , IC:                            USE IC FROM SW ORGANON
C  LP, JP, SP, WP, PP, WJ, LL, WB, KP: USE PP FROM SW ORGANON
C  DF, OT:                             USE DF FROM NW ORGANON
C  WH, MH, RC:                         USE WH FROM NW ORGANON
C  BM:                                 USE BM FROM NW ORGANON
C  RA, WA, PB, AS, CW, WI:             USE RA FROM NW ORGANON
C  GC:                                 USE GC FROM SW ORGANON
C  WO:                                 GOULD&HARRINGTON
C  PY:                                 USE PY FROM SW ORGANON
C  DG, HT, CH:                         USE DG FROM NW ORGANON
C  RW                                  USE CASTLE 2021 EQUATION
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
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'ESTREE.F77'
C
C
      INCLUDE 'MULTCM.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'ORGANON.F77'
C
COMMONS
C----------
C  DEFINITIONS:
C
C     PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE MORTALITY
C             RATE EQUATION.
C        I -- TREE SUBSCRIPT.
C        D -- TREE DIAMETER.
C      RIP -- ESTIMATED ANNUAL MORTALITY RATE BASED ON ORGANON EQNS
C             MODEL.
C        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
C             OF TREE MORTALITY (TREES/ACRE).
C    MRTEQ -- MORTALITY EQUATION NUMBER
C----------
      LOGICAL DEBUG,LINCL
      REAL PMSC(MAXSP),PRM(6),PSP(MAXSP),BM0(MAXSP)
      REAL BM1(MAXSP),BM2(MAXSP),BM3(MAXSP),BM4(MAXSP),BM5(MAXSP)
      INTEGER MYACTS(2),MORTMAP(39),PASS,MCLASS(MAXSP),ACLASS
      REAL CREDIT,TEMP,XMORE,VLOS,PRES,XCHECK,X,RIP
      REAL DGT,RELDBH,WKI,D1,D2,XMORT,AVED,WPROB,DSUM
      REAL DQ10,G,BRATIO,BARK,CIOBDS,D,P,SD2SQ,T,CRADJ,SUMDR10
      REAL RELHT,XSITE1,XSITE2,BAL,CR,WKIT2,RELDBH1(MAXTRE),G1(MAXTRE)
      REAL BAA,TA,SD2SQA,SUMDR10A,DQ10A,SDIA,CIOBDS1(MAXTRE)
      REAL WKII(MAXTRE),MVALUES(5),AVALUE, ALPHA(3),BETA(MAXSP)
      REAL DBHA,HBH,PTBAL
      REAL SMORMT,RBM0,RBM1,RBM2,RBM3,RBM4,RBM5
      INTEGER NTODO,I,NP,IACTK,IDATE,ISPCC,IS,ISPC,I1,I2,I3,IP
      INTEGER IDMFLG,ITODO,KPOINT,KBIG,IGRP,IULIM,IG,IX,J
      INTEGER IMRTMP
C----------
C  DATA STATEMENTS.
C----------
      DATA ALPHA/ -4.4384, 0.0053, -0.6001/
      DATA MVALUES/ 1.00, 1.500, 2.250, 3.375, 5.062/
      DATA MCLASS/ 1, 2, 2, 2, 2, 2, 3, 2, 3, 2, 4, 4, 3,
     & 3, 4, 3, 1, 1, 1, 1, 1, 4, 3, 4, 3, 5, 5, 5, 5, 4,
     & 4, 5, 1, 1, 3, 3, 5, 1, 5/  
C
       DATA BETA /
     & 0.247354, 0.217481, 0.179705, 0.205647, 0.216823,
     & 0.216823, 0.282203, 0.216823, 0.281542, 0.170425,
     & 0.168227, 0.216823, 0.216823, 0.216823, 0.236925,
     & 0.163506, 0.216823, 0.182940, 0.172690, 0.302866,
     & 0.216823, 0.216823, 0.216823, 0.216823, 0.216823,
     & 0.216823, 0.216823, 0.216823, 0.216823, 0.216823,
     & 0.216823, 0.216823, 0.216823, 0.216823, 0.216823,
     & 0.216823, 0.216823, 0.216823, 0.216823/ 
C
      DATA MYACTS/94,97/
      DATA PSP/ 10*0.0, 6*1.0, 14*0.0, 2*1.0, 7*0.0 /
      DATA PMSC/ 39*.317888 /
C----------
C THESE SETTINGS ARE FOR NWO MODEL TYPE; IF SMC MODEL TYPE THEN
C CHANGE SOME OF THE SETTINGS FOR DF. THIS IS DONE BELOW.
C----------
      DATA MORTMAP/ 
     & 2,2,2,2,2,2,2,4,4,2,
     & 4,4,4,4,4,1,6,3,3,3,                              ! DF NWO
C SMC     & 4,4,4,4,4,4,1,3,3,3,                         ! DF SMC
     & 4,4,4,4,4,4,4,5,4,4,
     & 4,4,4,4,4,4,4,1,1/
C
      DATA BM0/
     & -7.60159    ,-7.60159    ,-7.60159    ,-7.60159    ,-7.60159   ,
     & -7.60159    ,-7.60159    ,-1.922689902,-1.922689902,-7.60159   , 
     & -1.050000682,-1.050000682,-1.050000682,-1.050000682,-1.050000682,
     & -4.13142    ,-4.13142    ,-0.761609   ,-0.761609   ,-0.761609  , 
C SMC     & -3.12161659 ,-4.13142    ,-0.761609   ,-0.761609   ,-0.761609  ,     
     & -2.976822456,-2.0        ,-2.0        ,-2.0        ,-4.317549852,
     & -2.0        ,-2.0        ,-0.0        ,-1.050000682,-1.050000682,
     & -1.050000682,-1.050000682,-4.072781265,-3.020345211,-3.020345211,
     & -3.020345211,-2.0,        -4.13142    ,-4.13412/
C     
      DATA BM1/
     & -0.200523   ,-0.200523   ,-0.200523   ,-0.200523   ,-0.200523  ,
     & -0.200523   ,-0.200523   ,-0.136081990,-0.136081990,-0.200523  , 
     & -.194363402 ,-.194363402 ,-.194363402 ,-.194363402 ,-.194363402,
     & -1.13736    ,-1.13736    ,-0.529366   ,-0.529366   ,-0.529366  ,
C SMC     & -0.44724396 ,-1.13736    ,-0.529366   ,-0.529366   ,-0.529366  , 
     & 0.0         ,-0.5        ,-0.5        ,-0.5        ,-0.057696253,
     & -0.5        ,-0.5        ,-0.0        ,-.194363402 ,-.194363402,
     & -.194363402 ,-.194363402 ,-0.176433475,0.0         ,0.0        ,
     & 0.0         ,-0.5        ,-1.13736    ,-1.13736/
C      
      DATA BM2/
     & 0.0         ,0.0         ,0.0         ,0.0         ,0.0        ,
     & 0.0         ,0.0         ,0.002479863 ,0.002479863 ,0.0        ,
     & 0.003803100 ,0.003803100 ,0.003803100 ,0.003803100 ,0.003803100,
     & 0.0         ,0.0         ,0.0         ,0.0         ,0.0        ,
     & 0.0         ,0.015       ,0.015       ,0.015       ,0.0        ,
     & 0.015       ,0.015       ,0.0         ,0.003803100 ,0.003803100,
     & 0.003803100 ,0.003803100 ,0.0         ,0.0         ,0.0        ,
     & 0.0         ,0.015       ,0.0         ,0.0/
C      
      DATA BM3/
     & 0.0         ,0.0         ,0.0         ,0.0         ,0.0       ,
     & 0.0         ,0.0         ,-3.178123293,-3.178123293,0.0       , 
     & -3.557300286,-3.557300286,-3.557300286,-3.557300286,-3.557300286,
     & -0.823305   ,-0.823305   ,-4.74019    ,-4.74019    ,-4.74019   ,
C SMC     & -2.48387172 ,-0.823305   ,-4.74019    ,-4.74019    ,-4.74019   ,
     & -6.223250962,-3.0        ,-3.0        ,-3.0        ,0.0        ,
     & -3.0        ,-3.0        ,0.0         ,-3.557300286,-3.557300286,
     & -3.557300286,-3.557300286,-1.729453975,-8.467882343,-8.467882343,
     & -8.467882343,-3.0        ,-0.823305   ,-0.823305/
C      
      DATA BM4/
     & 0.0441333   ,0.0441333   ,0.0441333   ,0.0441333   ,0.0441333  ,
     & 0.0441333   ,0.0441333   ,0.0         ,0.0         ,0.0441333  , 
     & 0.003971638 ,0.003971638 ,0.003971638 ,0.003971638 ,0.003971638,
     & 0.0307749   ,0.0307749   ,0.0119587   ,0.0119587   ,0.0119587  ,
C SMC     & 0.01843137  ,0.0307749   ,0.0119587   ,0.0119587   ,0.0119587  ,     
     & 0.0         ,0.015       ,0.015       ,0.015       ,0.004861355,
     & 0.015       ,0.015       ,0.0         ,0.003971638 ,0.003971638,
     & 0.003971638 ,0.003971638 ,0.0         ,0.013966388 ,0.013966388,
     & 0.013966388 ,0.015       ,0.0307749   ,0.0307749/
C
      DATA BM5/                                                       
     & 0.00063849  ,0.00063849  ,0.00063849  ,0.00063849  ,0.00063849  ,
     & 0.00063849  ,0.00063849  ,0.004684133 ,0.004684133 ,0.00063849  ,
     & 0.005573601 ,0.005573601 ,0.005573601 ,0.005573601 ,0.005573601,
     & 0.00991005  ,0.00991005  ,0.00756365  ,0.00756365  ,0.00756365  ,
C SMC     & 0.01353918  ,0.00991005  ,0.00756365  ,0.00756365  ,0.00756365  ,
     & 0.0         ,0.01        ,0.01        ,0.01        ,0.00998129,
     & 0.01        ,0.01        ,0.0         ,0.005573601 ,0.005573601,
     & 0.005573601 ,0.005573601 ,0.012525642 ,0.009461545 ,0.009461545,
     & 0.009461545 ,0.01        ,0.00991005  ,0.00991005/                     
C 
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'MORTS',5,ICYC)
C---------
C  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
C  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
C  MULTIPLIERS, SEE SUBROUTINE MULTS).
C---------
      CALL OPFIND (1,MYACTS(1),NTODO)
      IF (NTODO.EQ.0) GOTO 12
      DO 9 I=1,NTODO
      CALL OPGET(I,4,IDATE,IACTK,NP,PRM)
      IF (IACTK.LT.0) GOTO 9
      CALL OPDONE(I,IY(ICYC))
      ISPCC=IFIX(PRM(1))
      IF(ISPCC.EQ.0) GOTO 2
      XMMULT(ISPCC)=PRM(2)
      XMDIA1(ISPCC)=PRM(3)
      XMDIA2(ISPCC)=PRM(4)
      GOTO 9
    2 CONTINUE
      DO 5 ISPCC=1,MAXSP
      XMMULT(ISPCC)=PRM(2)
      XMDIA1(ISPCC)=PRM(3)
      XMDIA2(ISPCC)=PRM(4)
    5 CONTINUE
    9 CONTINUE
   12 CONTINUE
C----------
C IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
C PROPORTION IN CRATET, ADJUST THEM HERE.
C----------
      IF(PMSDIL .GT. 1.0)PMSDIL = PMSDIL/100.
      IF(PMSDIU .GT. 1.0)PMSDIU = PMSDIU/100.
C
      IF (ITRN.LE.0) GOTO 100
C----------
C SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
C BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
C WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
C FOR MODELING CLIMATE CHANGE.
C----------
      CALL SDICAL(0,SDIMAX)
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS CYCLE= ',ICYC,'  BAMAX= ',
     &BAMAX,'  SDIMAX= ',SDIMAX 
C----------
C  ESTIMATE QUADRATIC MEAN DIAMETER 5 YEARS HENCE.
C----------
      T=0.0
      SD2SQ=0.0
      SUMDR10=0.0
      DQ10=0.0
      DO 20 I=1,ITRN
      P=PROB(I)
      IS=ISP(I)
      D=DBH(I)
      BARK=BRATIO(IS,D,HT(I))
      G=DG(I)/BARK
      G1(I)=G
      IF(LZEIDE.AND.(D.LT.DBHZEIDE))GO TO 20       ! BRANCH IF D IS LT MIN DBH
      IF(.NOT.LZEIDE.AND.(D.LT.DBHSTAGE))GO TO 20
      CIOBDS=(2.0*D*G+G*G)
      CIOBDS1(I)=CIOBDS
      SD2SQ=SD2SQ+P*(D*D+CIOBDS)
      IF(LZEIDE)THEN
        SUMDR10=SUMDR10+P*(D+G)**1.605
      ENDIF 
      T=T+P
   20 CONTINUE
      IF(T. EQ. 0.0) T= 0.0001
      DQ10=SQRT(SD2SQ/T)
      IF(LZEIDE)THEN
        DQ10=(SUMDR10/T)**(1/1.605)
      ENDIF 
      IF(DEBUG)WRITE(JOSTND,*)'SD2SQ,SUMDR10,DQ10= ',
     &SD2SQ,SUMDR10,DQ10
C----------
C  CALCULATE AVERAGE DBH USED IN INDIV. MORT EQUATION BELOW
C----------
      DSUM=0.0
      WPROB=0.0
      DO I=1,ITRN
        WPROB=WPROB+PROB(I)
        DSUM=DSUM+DBH(I)*PROB(I)
      ENDDO
      AVED=DSUM/WPROB
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS DSUM,WPROB,AVED= ',DSUM,WPROB,
     &AVED
C----------
C  SEE IF ORGANON MORTALITY HAS BEEN ESTIMATED. IF YES, USE ORGANON
C  RATES FOR ALL TREE RECORDS. IF NO, THEN ESTIMATE FVS MORTALITY.
C----------
      SMORMT = 0.0
      DO I=1,ITRN
        SMORMT = SMORMT + MORTEXP(I)
      ENDDO
      IF(DEBUG)WRITE(JOSTND,*)' IN MORTS SMORMT= ',SMORMT
C----------
C  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
C  ONE AT A TIME WITHIN A SPECIES.
C----------
      DO 50 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.LE.0) GO TO 50
      I2=ISCT(ISPC,2)
      XMORT = XMMULT(ISPC)
      D1 = XMDIA1(ISPC)
      D2 = XMDIA2(ISPC)
C----------
C  START TREE LOOP WITHIN SPECIES.
C----------
      TA=0.0
      SD2SQA=0.0
      SUMDR10A=0.0
      PASS=1
      DO 40 I3=I1,I2
C----------
C  INITIALIZE FOR NEXT TREE.
C----------
      I=IND1(I3)
      P=PROB(I)
      RIP=0.0
      WKI=0.0
      WK2(I)=0.0
      D=DBH(I)
      BARK=BRATIO(ISPC,D,HT(I))
      IF(P.LE.0.0) GO TO 40
C----------
C  IF ORGANON MORTALITY HAS BEEN ESTIMATED, USE IT
C----------
      IF(LORGANON .AND. (SMORMT .GT. 0.))THEN
        WK2(I) = MORTEXP(I)*(FINT/5.)
        WKI = MORTEXP(I)*(FINT/5.)
        IF(DEBUG) WRITE(JOSTND,*) ' IN MORTS I,ISPC,D,WK2,IORG= ',
     *  I,ISPC,D,WK2(I),IORG(I) 
        GO TO 39
      ENDIF
C
      RELDBH=D/AVED
      RELDBH1(I)=RELDBH
      IF(D.LE.0.5)D=0.5
      DGT=WK1(I)/OLDFNT
      IF(D.LE.1.0 .AND. DGT.LT.0.05) DGT=0.05
      IF(D.LE.5.0 .AND. D.GT.1.0 .AND. DGT.LT.0.05)
     &     DGT=0.05*(5.0-D)/4.0
      G=WK1(I)/(BARK*OLDFNT)
      IF(WK1(I)/OLDFNT .LT. DGT)G=DGT/BARK
C----------
C  COMPUTE MORTALITY RATE
C----------
C----------
C WC AND PN MORTALITY FUNCTIONS WERE REPLACED WITH ORGANON NWO AND A
C FEW FROM ORGANON SWO BECAUSE OF ISSUES THAT WERE FOUND THAT THE 
C OLD EQUATIONS WERE APPLYING MORTALITY TOO EQUALLY BY SIZE, TREE DBH
C COEFFICIENT IS MORE SIGNIFICANT IN THE ORGANON EQUATIONS AND
C WORK BETTER. ESM07/10/2014
C IF USERS WANT TO TEST SMC EQUATION FOR DF SEE COMMENTED OUT
C CODE IN DATA STATEMETNS.
C OREGON WHITE OAK USES A DIFFERENT EQUATION BUILT 
C BY HARRINGTON AND GOULD.
C SMALL TREES USE EQUATIONS BY GOULD AND HARRINGTON
C----------
      CR=ICR(I)*0.01
      BAL=(1.0 - (PCT(I)/100.)) * BA
      PTBAL=PTBALT(I)
      CRADJ = 1.0
      IF(CR .LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
      XSITE2=SITEAR(19)
      XSITE1=SITEAR(16)
C----------
C IF SMC MODEL TYPE USE MORTMAP(26)=4 INSTEAD OF 1
C----------    
      IMRTMP = MORTMAP(ISPC)
      RBM0=BM0(ISPC)
      RBM1=BM1(ISPC)
      RBM2=BM2(ISPC)
      RBM3=BM3(ISPC)
      RBM4=BM4(ISPC) 
      RBM5=BM5(ISPC)    
      IF(IMODTY .EQ. 3 .AND. ISPC .EQ.16) THEN
        IMRTMP=4
        RBM0 = -3.12161659
        RBM1 = -0.44724396
        RBM3 = -2.48387172
        RBM4 = 0.01843137
        RBM5 = 0.01353918
      ENDIF
C 
      SELECT CASE(IMRTMP)
        CASE(1)  
          RIP = RBM0 + RBM1*D**.5 + RBM3*CR**.25 +
     &        RBM4*(XSITE1+4.5) + RBM5*BAL
          RIP = (1.0/(1.0+EXP(-(RIP))))                         !5 yr RIP
          RIP = (1.0-RIP)**(0.2)                                !annual survival
          RIP = 1.0-RIP*CRADJ                                   !annual RIP
        CASE(2)
          RIP = RBM0 + RBM1*D + RBM4*(XSITE1+4.5) +  RBM5*(BAL/D)
          RIP = (1.0/(1.0+EXP(-(RIP))))                         !5 yr RIP
          RIP = (1.0-RIP)**(0.2)                                !annual survival
          RIP = 1.0-RIP*CRADJ                                   !annual RIP
        CASE(3)
          RIP = RBM0 + RBM1*D + RBM2*D**2 + RBM3*CR +
     &        RBM4*(XSITE2+4.5) + RBM5*BAL
          RIP = (1.0/(1.0+EXP(-(RIP))))                         !5 yr RIP
          RIP = (1.0-RIP)**(0.2)                                !annual survival
          RIP = 1.0-RIP*CRADJ                                   !annual RIP
        CASE(4)
          RIP = RBM0 + RBM1*D + RBM2*D**2 + RBM3*CR +
     &        RBM4*(XSITE1+4.5) + RBM5*BAL
          RIP = (1.0/(1.0+EXP(-(RIP))))                         !5 yr RIP
          RIP = (1.0-RIP)**(0.2)                                !annual survival
          RIP = 1.0-RIP*CRADJ                                   !annual RIP
        CASE(5)
          RELHT = 0.0
          IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
          IF(RELHT .GT. 1.5)RELHT=1.5
          RIP = -6.6707 + 0.5105*ALOG(5+BA) - 1.3183*RELHT
          RIP = (1.0/(1.0+EXP(RIP)))
          RIP = 1.0-RIP                                          !annual RIP
C NEW REDWOOD EQUATION
        CASE(6)
          RIP = 2.901447 + (0.578694*D) + (-0.001793*PTBAL)
          RIP = 1.0/(1.0 + EXP(RIP))                           !annual RIP
          IF(RIP .LT. 0.0001) RIP = 0.0001                     !constrain RIP
          IF(DEBUG) WRITE(JOSTND,*)'IN MORTS RW DEBUG',' DBH=',D,
     & ' PTBAL=',PTBAL,' RIP=',RIP
      END SELECT
C----------
C SMALL-TREE MORTALITY MODEL DEVELOPED BY GOULD AND HARRINGTON
C SMALL-TREE LOGIC IS BYPASSED FOR REDWOOD
C----------  
      IF (D .LT. 3.0 .AND. ISPC .NE. 17) THEN
        RELHT = 0.0
        IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
        IF(RELHT .GT. 1.5)RELHT=1.5
        HBH = HT(I)
        IF (HBH .GE. 4.5) HBH = 4.5
        DBHA = D+BETA(ISPC)*HBH  
        ACLASS = MCLASS(ISPC)
        AVALUE = MVALUES(ACLASS)
        RIP = PTBAL*AVALUE/SQRT(DBHA+1)
        RIP = ALPHA(1) + ALPHA(2)*RIP
        RIP = RIP + ALPHA(3)*RELHT
        RIP=(1.0/(1.0+EXP(RIP)))
        RIP=1.0-RIP
        IF(DEBUG)WRITE(JOSTND,*)'IN MORTS SMALL EQNS: I,RIP,HBH,
     &  DBHA,PTBAL,ACLASS,AVALUE,ALPHA1,ALPHA2,ALPHA3 =   ',I,RIP,HBH,
     &  DBHA,PTBAL,ACLASS,AVALUE,ALPHA(1),ALPHA(2),ALPHA(3)
      ENDIF
C----------
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS EQNS IMRTMP,I,ISPC,RIP,D,CR,
     &XSITE1,XSITE2,BAL,CRDJ=',IMRTMP,I,ISPC,RIP,D,CR,XSITE1,
     &XSITE2,BAL,CRADJ
C----------
C  APPLY MORTALITY MULTIPLIERS WITHIN USER-PROVIDED DBH LIMITS
C  AND STORE TREES PER ACRE DYING IN THE ARRAY WK2.
C----------
      X=1.0
      IF (D.GE.D1 .AND. D.LT.D2) X=XMORT
C----------
C  FROM HERE TO STATEMENT 29 IS ESTABLISHMENT MODEL CODE.
C  "BEST" TREES NOT SUBJECT TO MORTALITY FOR 20 YRS
C  AFTER DISTURBANCE DATE.
C----------
      IF(IESTAT(I).LE.0) GO TO 29
      IF(IY(ICYC).GE.IESTAT(I)) IESTAT(I)=0
      XCHECK= FLOAT(IESTAT(I)-IY(ICYC))/FINT
      IF(XCHECK.GT.1.0) XCHECK=1.0
      IF(XCHECK.LT.0.0) XCHECK=0.0
      X=X* (1.0-XCHECK)
   29 CONTINUE
      IF (RIP.LT.0.00001) RIP=0.00001
      WKI=P*(1.0-(1.0-RIP)**FINT)*X
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS EQ I,XCHECK,X,P,RIP,FINT,WKI= ',
     &I,XCHECK,X,P,RIP,FINT,WKI
C
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      IDMFLG=IFIX(SIZCAP(ISPC,3))
      IF((D+G).GE.SIZCAP(ISPC,1) .AND. IDMFLG.NE.1) THEN
         WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/5.0))
        IF(DEBUG)WRITE(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',
     &  'I,ISPC,D,P,SIZCAP 1-3,WKI = ',
     &  I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
      ENDIF
   39 CONTINUE
      IF(WKI.GT.P) WKI=P
C----------
C  IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
C  SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.
C----------
      IF(SDIMAX .LT. 5)THEN
        WKI=P
      ENDIF
C----------
C  RECALCULATE VALUES AFTER MORTALITY HAS BEEN APPLIED
C---------- 
      TA=TA+(PROB(I)-WKI)
      WK2(I)=WKI
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS INITIAL PASS: IXX WKI WK2=',
     &I,WKI,WK2(I)
   40 CONTINUE 
   50 CONTINUE
      GOTO 59
C----------
C     RECALCULATE SDI AND BA FOR THE STAND AND MAKE SURE THEY ARE BELOW THE MAXIMUM
C----------
   55 CONTINUE
      DQ10A=SQRT(SD2SQA/TA)
      BAA=.005454154*DQ10A*DQ10A*TA
      IF(LZEIDE)THEN
        DQ10A=(SUMDR10A/TA)**(1./1.605)
      ENDIF
      SDIA=TA*(DQ10A/10)**1.605
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS:PASS,TA,SD2SQA,SUMDR10A,DQ10A
     &,BAA,SDIA=',PASS,TA,SD2SQA,SUMDR10A,DQ10A,BAA,SDIA
C----------
C     LOOP BACK THROUGH TREES RECORDS IF STAND IF OVER MAXIMUM SDI.
C     BUT BECAUSE BA CAN CREEP UP TO UNREALISTIC VALUES EVEN WHILE
C     HOLDING SDI BELOW MAXIMUM AN ADDITIONAL MAXIMUM OF 550FT2/ACRE.
C     THIS WAS SET BASED ON MAXIMUM BA FOUND ON 400 CVS PLOTS ON WESTSIDE
C----------
      IF(((SDIA.LT.SDIMAX).AND.(BAA.LT.550)).OR.(PASS.GT.100)) GO TO 70
      PASS=PASS+1
      SD2SQA=0
      SUMDR10A=0   
      TA=0
  59  CONTINUE
      DO 60 I=1,ITRN
      WKI=WK2(I)*(PASS)
      IF(WKI.GT.PROB(I)) WKI=PROB(I)
      WKIT2=WKI+WKIT2
      WKII(I)=WKI
      IF(LZEIDE.AND.(DBH(I).LT.DBHZEIDE))GO TO 60         ! BRANCH IF D IS LT MIN DBH
      IF(.NOT.LZEIDE.AND.(DBH(I).LT.DBHSTAGE))GO TO 60
      SD2SQA=SD2SQA+(PROB(I)-WKI)*(DBH(I)*DBH(I)+CIOBDS1(I))
      IF(LZEIDE)THEN
        SUMDR10A=SUMDR10A+(PROB(I)-WKI)*(DBH(I)+G1(I))**1.605
      ENDIF 
      TA=TA+(PROB(I)-WKI)
      IF(DEBUG)WRITE(JOSTND,*)'IN MORT PASS,I,DBH,WKI,WKIT2,SD2SQA,
     &SUMDR10A,TA=',PASS,I,DBH(I),WKI,WKIT2,SD2SQA,SUMDR10A,TA
   60 CONTINUE
      GOTO 55
C----------
C  END OF TREE LOOPING.  PRINT DEBUG INFO IF DESIRED.
C----------
   70 CONTINUE
      DO 80 I=1,ITRN 
      IF (PASS.GT.1) WK2(I)=WKII(I)
      IF(WK2(I).GT.PROB(I)) WK2(I)=PROB(I)
      IF(.NOT.DEBUG) GO TO 80
      PRES=PROB(I)-WK2(I)
      VLOS=WK2(I)*CFV(I)/FINT
      WRITE(JOSTND,9000) I,ISPC,DBH(I),G1(I),RELDBH1(I),BA,PROB(I),
     &WK2(I),PRES,VLOS
 9000 FORMAT('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,
     &       ',  INIT PROB=',F9.3/
     &       ', TREES DYING=',F9.3,'  RES PROB=',F9.3,
     &       ',  VOL LOST=',F9.3)
   80 CONTINUE
      IF(.NOT.DEBUG) GO TO 100
      WRITE(JOSTND,9001)  ISPC
 9001 FORMAT('IN MORTS,  ISPC=',I3,
     &       11X)
  100 CONTINUE
C
C----------
C  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES
C---------
      CALL CLMORTS
C
C
C----------
C  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORTS
C  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT
C----------
      CALL OPFIND (1,MYACTS(2),NTODO)
      IF (NTODO.GT.0) THEN
        IF(DEBUG)WRITE(JOSTND,*)'FIXMORT PROCESSING, ITODO= ',ITODO
         DO 300 ITODO=1,NTODO
         CALL OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
         IF (IACTK.LT.0) GOTO 300
         CALL OPDONE(ITODO,IY(ICYC))
         ISPCC=IFIX(PRM(1))
         IF(NP .LE. 4)THEN
           IF(PRM(2).GT. 1.0)PRM(2)=1.0
         ENDIF
         IF(PRM(3).LT. 0.0)PRM(3)=0.0
         IF(PRM(4).LE. 0.0)PRM(4)=999.
         IP=1
         IF (NP.GT.4) THEN
            IF(PRM(5).LT.3.)THEN
               IF(PRM(2).GT. 1.0)PRM(2)=1.0
               IF(PRM(2).LT. 0.0)PRM(2)=0.0
            ENDIF 
            IF (PRM(5).EQ.1.0) THEN
               IP=2
            ELSEIF (PRM(5).EQ.2.0) THEN
               IP=3
            ELSEIF (PRM(5).EQ.3.) THEN
               IP=4
            ENDIF
         ENDIF
C----------
C  SET FLAG FOR POINT MORTALITY, OR KILLING FROM ABOVE
C    PRM(6)    POINT      SIZE   KBIG     KILL DIRECTION
C      0         NO        NO     0                       DEFAULT CONDITION
C      1         YES       NO     0
C     10         NO        YES    1       BOTTOM UP
C     11         YES       YES    1       BOTTOM UP
C     20         NO        YES    2       TOP DOWN
C     21         YES       YES    2       TOP DOWN
C----------
         KPOINT=0
         KBIG=0
         IF(PRM(6).GT.0.)THEN
           IF(PRM(6) .EQ. 1)THEN
             KPOINT=1
           ELSEIF(PRM(6) .EQ. 10)THEN
             KBIG=1
           ELSEIF(PRM(6) .EQ. 11)THEN
             KPOINT=1
             KBIG=1
           ELSEIF(PRM(6) .EQ. 20)THEN
             KBIG=2
           ELSEIF(PRM(6) .EQ. 21)THEN
             KPOINT=1
             KBIG=2
           ENDIF
         ENDIF
         IF (ITRN.GT.0) THEN
C----------
C IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
C EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
C   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
C   ARE IN EFFECT.
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
C   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.
C----------
            IF(KBIG.GE.1 .OR. (KPOINT.EQ.1 .AND. IPTINV.GT.1)) THEN
              XMORE=0.
              DO 199 I=1,ITRN
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 90 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 91
                ENDIF
   90           CONTINUE
              ENDIF
   91         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                GOTO (191,192,193,194),IP
  191           CONTINUE
                XMORE=XMORE+PROB(I)*PRM(2)
                WK2(I)=0.
                GOTO 199
  192           CONTINUE
                XMORE=XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                GOTO 199
  193           CONTINUE
                TEMP=AMAX1(WK2(I),(PROB(I)*PRM(2)))
                IF(TEMP .GT. WK2(I)) THEN
                  XMORE=XMORE+TEMP-WK2(I)
                ENDIF
                GOTO 199
  194           CONTINUE
                XMORE=XMORE+WK2(I)*PRM(2)
                WK2(I)=0.
                GOTO 199
              ENDIF
  199         CONTINUE
              IF(DEBUG)WRITE(JOSTND,*)'KPOINT,KBIG,ITRN,XMORE= ',
     &                 KPOINT,KBIG,ITRN,XMORE
              CREDIT=0.
              DO 201 I=1,ITRN
              IWORK1(I)=IND1(I)
              IF(KBIG .EQ. 1)THEN
                WORK3(I)=(-1.0)*
     &                  (DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
              ELSE
                WORK3(I)=DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
              ENDIF
  201         CONTINUE
              CALL RDPSRT(ITRN,WORK3,IWORK1,.FALSE.)
              IF(DEBUG)WRITE(JOSTND,*)'DBH= ',(DBH(IG),IG=1,ITRN)
              IF(DEBUG)WRITE(JOSTND,*)'IWORK1= ',(IWORK1(IG),IG=1,ITRN)
              IF(DEBUG)WRITE(JOSTND,*)'WK2= ',(WK2(IG),IG=1,ITRN)
C
              IF(KBIG.GE.1 .AND. KPOINT.EQ.0)THEN
C
C  CONCENTRATION BY SIZE ONLY
C
                DO 310 I=1,ITRN
                IX=IWORK1(I)
                LINCL = .FALSE.
                IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX))THEN
                  LINCL = .TRUE.
                ELSEIF(ISPCC.LT.0)THEN
                  IGRP = -ISPCC
                  IULIM = ISPGRP(IGRP,1)+1
                  DO 92 IG=2,IULIM
                  IF(ISP(IX) .EQ. ISPGRP(IGRP,IG))THEN
                    LINCL = .TRUE.
                    GO TO 93
                  ENDIF
   92             CONTINUE
                ENDIF
   93           CONTINUE
                IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                  TEMP=CREDIT+PROB(IX)-WK2(IX)
                  IF((TEMP .LE. XMORE).OR.
     >               (ABS(TEMP-XMORE).LT.0.0001))THEN
                    CREDIT=CREDIT+PROB(IX)-WK2(IX)
                    WK2(IX)=PROB(IX)
                  ELSE
                    WK2(IX)=WK2(IX)+XMORE-CREDIT
                    CREDIT=XMORE
                    GO TO 295
                  ENDIF
                ENDIF
  310           CONTINUE
                GO TO 295
C
              ELSEIF(KPOINT.EQ.1 .AND. KBIG.EQ.0)THEN
C
C  CONCENTRATION ON POINTS ONLY
C
              DO 205 J=1,IPTINV
              DO 204 I=1,ITRN
              IF(ITRE(I) .NE. J)GO TO 204
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 94 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 95
                ENDIF
   94           CONTINUE
              ENDIF
   95         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(I)-WK2(I)
                IF((TEMP .LE. XMORE).OR.
     >             (ABS(TEMP-XMORE).LT.0.0001))THEN
                  CREDIT=CREDIT+PROB(I)-WK2(I)
                  WK2(I)=PROB(I)
                ELSE
                  WK2(I)=WK2(I)+XMORE-CREDIT
                  CREDIT=XMORE
                  GO TO 295
                ENDIF
              ENDIF
  204         CONTINUE
  205         CONTINUE
              GO TO 295
C
C  CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
C  WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
C  POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.
              ELSE
              DO 312 J=1,IPTINV
              DO 311 I=1,ITRN
              IX=IWORK1(I)
              IF(ITRE(IX) .NE. J)GO TO 311
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 96 IG=2,IULIM
                IF(ISP(IX) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 97
                ENDIF
   96           CONTINUE
              ENDIF
   97         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(IX)-WK2(IX)
                IF((TEMP .LE. XMORE).OR.
     >             (ABS(TEMP-XMORE).LT.0.0001))THEN
                  CREDIT=CREDIT+PROB(IX)-WK2(IX)
                  WK2(IX)=PROB(IX)
                ELSE
                  WK2(IX)=WK2(IX)+XMORE-CREDIT
                  CREDIT=XMORE
                  GO TO 295
                ENDIF
              ENDIF
  311         CONTINUE
  312         CONTINUE
              GO TO 295
              ENDIF
C
            ENDIF
C----------
C  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
C  IS NOT IN EFFECT.
C----------
            DO 290 I=1,ITRN
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 98 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 99
                ENDIF
   98           CONTINUE
              ENDIF
   99         CONTINUE
            IF (LINCL .AND.
     >         (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
               GOTO (610,620,630,640),IP
  610          CONTINUE
               WK2(I)=PROB(I)*PRM(2)
               GOTO 290
  620          CONTINUE
               WK2(I)=WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
               GOTO 290
  630          CONTINUE
               WK2(I)=AMAX1(WK2(I),(PROB(I)*PRM(2)))
               GOTO 290
  640          CONTINUE
               WK2(I)=AMIN1(PROB(I),WK2(I)*PRM(2))
               GOTO 290
            ENDIF
  290       CONTINUE
  295    CONTINUE
         IF(DEBUG)WRITE(JOSTND,*)'ITODO,WK2= ',
     &    ITODO,(WK2(IG),IG=1,ITRN)
         ENDIF
  300    CONTINUE
      ENDIF
      RETURN
C
      ENTRY MORCON
      RETURN
      END

