      SUBROUTINE MORTS
      IMPLICIT NONE
C----------
C VWC $Id$
C----------
C  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
C  RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
C  TREE RECORD.  MORTALITY RATE FOR TREES IS PREDICTED FROM DBH
C  AND BASAL AREA IN TREES LARGER (BAL). IF STAND GETS ABOVE MAXSDI OR BAMAX,
C  MORTALITY IS REPEATEDLY APPLIED UNTIL STAND IS BELOW MAX. IF THIS ROUTINE
C  IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH PREDICTION. ENTRY 
C  **MORCON** IS ACCESSED TO LOAD SITE DEPENDENT CONSTANTS.
C----------
C SPECIES LIST FOR ALASKA VARIANT.
C
C Number Code  Common Name         FIA  PLANTS Scientific Name
C   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4     TA   tamarack            071  LALA   Larix laricina
C   5     WS   white spruce        094  PIGL   Picea glauca
C   6     LS   Lutz’s spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TE
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TD
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
COMMONS
C----------
C  DEFINITIONS:
C      BM1 --  ARRAY CONTAINING INTERCEPT COEFFICIENTS FOR SURVIVAL EQUATION
C      BM2 --  ARRAY CONTAINING DBH COEFFICIENTS FOR SURVIVAL EQUATION
C      BM3 --  ARRAY CONTAINING DBH^2 COEFFICIENTS FOR SURVIVAL EQUATION
C      BM4 --  ARRAY CONTAINING BAL COEFFICIENTS FOR SURVIVAL EQUATION
C      BM5 --  ARRAY CONTAINING BAL/LN(DBH + 1) COEFFICIENTS FOR SURVIVAL EQUATION
C    DSURV --  MINIMUM DIAMETER LIMITS USED FOR SURVIVAL CALCULATIONS
C        D --  TREE DIAMETER
C    PTBAL --  POINT BASAL AREA
C      RIP --  ESTIMATED ANNUAL MORTALITY RATE
C        P --  NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C      WKI --  SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
C              OF TREE MORTALITY (TREES/ACRE).
C----------
      LOGICAL DEBUG,LINCL
      REAL PRM(6)
      REAL BM1(MAXSP),BM2(MAXSP),BM3(MAXSP),BM4(MAXSP),BM5(MAXSP)
      REAL CREDIT,TEMP,XMORE,VLOS,PRES,XCHECK,X,RIP
      REAL WKI,D1,D2,XMORT
      REAL DQ10,G,BRATIO,BARK,CIOBDS,D,P,SD2SQ,T,SUMDR10
      REAL WKIT2,G1(MAXTRE)
      REAL BAA,TA,SD2SQA,SUMDR10A,DQ10A,SDIA,CIOBDS1(MAXTRE)
      REAL WKII(MAXTRE),PTBAL
      INTEGER MYACTS(2),PASS
      INTEGER NTODO,I,NP,IACTK,IDATE,ISPCC,IS,ISPC,I1,I2,I3,IP
      INTEGER IDMFLG,ITODO,KPOINT,KBIG,IGRP,IULIM,IG,IX,J
      REAL DSURV(MAXSP),DTEMP,SDIUPR
C----------
C  DATA STATEMENTS.
C----------
      DATA MYACTS/94,97/
C
C     SURVIVAL INTERCEPT COEFFICIENTS
      DATA BM1/
     & 5.104464, 5.104464, 5.334589, 5.129246, 4.585373,
     & 4.585373, 5.129246, 5.104464, 4.934186, 6.09668,
     & 5.159052, 5.441808, 4.585373, 2.651991, 2.987803,
     & 3.255821, 3.255821, 2.651991, 2.833541, 2.651991,
     & 2.651991, 2.651991, 2.651991/
C      
C     SURVIVAL DBH COEFFICIENTS
      DATA BM2/
     & 0.036219, 0.036219, 0.036219, 0.402707, 0.402707,
     & 0.402707, 0.402707, 0.036219, 0.036219, 0.036219,
     & 0.036219, 0.036219, 0.402707, 0.402707, 0.402707,
     & 0.402707, 0.402707, 0.402707, 0.402707, 0.402707,
     & 0.402207, 0.402207, 0.402207/
C
C     SURVIVAL DBH^2 COEFFICIENTS
      DATA BM3/
     & -0.000634, -0.000634, -0.00104, -0.051774, -0.020537,
     & -0.020537, -0.051774, -0.000634, -0.000847, -0.000768,
     & -0.00105,  -0.000952, -0.020537, -0.011288, -0.014038,
     & -0.018658, -0.018658, -0.011288, -0.015719, -0.011288,
     & -0.011288, -0.011288, -0.011288/
C
C     SURVIVAL BAL COEFFICIENTS
C     NOT USED (ZERO VALUE) FOR SF, AF, YC, SS, LP, RC, WH, MH
      DATA BM4/
     &  0.0,       0.0,       0.0,      -0.004365, -0.004414,
     & -0.004414, -0.004365,  0.0,       0.0,       0.0,
     &  0.0,       0.0,      -0.004414, -0.002557, -0.005462,
     & -0.007307, -0.007307, -0.002557, -0.010755, -0.002557,
     & -0.002557, -0.002557, -0.002557/
C
C     SURVIVAL BAL/DBH COEFFICIENTS
C     NOT USED (ZERO VALUE) FOR TA, WS, LS, BE, OS, AD, RA, PB, AB,
C     BA, AS, BC, WI, SU, OH
      DATA BM5/
     & -0.010721, -0.010721, -0.004567,  0.0,       0.0,
     &  0.0,       0.0,      -0.010721, -0.008424, -0.005742,
     & -0.00512,  -0.005388,  0.0,       0.0,       0.0,
     &  0.0,       0.0,       0.0,       0.0,       0.0,
     &  0.0,       0.0,       0.0/

C     MINIMUM DIAMETER USED FOR SURVIVAL CALCULATIONS
      DATA DSURV/
     & 0.50, 0.50, 0.50, 0.10, 0.10, 0.10, 0.10, 0.50, 0.50, 0.50,
     & 0.50, 0.50, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10,
     & 0.10, 0.10, 0.10/
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
      IF (NTODO.GT.0) THEN
        DO I=1,NTODO
          CALL OPGET(I,4,IDATE,IACTK,NP,PRM)
          IF (IACTK.LT.0) GOTO 9
          CALL OPDONE(I,IY(ICYC))
          ISPCC=IFIX(PRM(1))
          IF (ISPCC.EQ.0) GOTO 2
          XMMULT(ISPCC)=PRM(2)
          XMDIA1(ISPCC)=PRM(3)
          XMDIA2(ISPCC)=PRM(4)
          GOTO 9
    2     CONTINUE
          DO ISPCC=1,MAXSP
            XMMULT(ISPCC)=PRM(2)
            XMDIA1(ISPCC)=PRM(3)
            XMDIA2(ISPCC)=PRM(4)
          ENDDO
    9     CONTINUE
        ENDDO
      ENDIF
C----------
C IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
C PROPORTION IN CRATET, ADJUST THEM HERE.
C----------
      IF (PMSDIL .GT. 1.0)PMSDIL = PMSDIL/100.
      IF (PMSDIU .GT. 1.0)PMSDIU = PMSDIU/100.
C
      IF (ITRN.LE.0) GOTO 100
C----------
C SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
C BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
C WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
C FOR MODELING CLIMATE CHANGE.
C----------
      CALL SDICAL(0,SDIMAX)
      IF (DEBUG)WRITE(JOSTND,*)'IN MORTS CYCLE= ',ICYC,'  BAMAX= ',
     &  BAMAX,'  SDIMAX= ',SDIMAX
C----------
C ADJUST SDIMAX BASED ON PMSDIU. SDIUPR VARIABLE WILL LATER BE USED
C TO DETERMINE NUMBER OF PASSES IN MORTALITY PROCESSING.
C----------
      SDIUPR= SDIMAX * PMSDIU
      IF (DEBUG)WRITE(JOSTND,*)'IN MORTS CYCLE= ',ICYC,' SDIMAX= ',
     &  SDIMAX,' PMSDIU=',PMSDIU,' SDIUPR=',SDIUPR
C----------
C  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.
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
      IF (LZEIDE.AND.(D.LT.DBHZEIDE)) GOTO 20       ! BRANCH IF D IS LT MIN DBH
      IF (.NOT.LZEIDE.AND.(D.LT.DBHSTAGE)) GOTO 20
      CIOBDS=(2.0*D*G+G*G)
      CIOBDS1(I)=CIOBDS
      SD2SQ=SD2SQ+P*(D*D+CIOBDS)
      IF (LZEIDE) THEN
        SUMDR10=SUMDR10+P*(D+G)**1.605
      ENDIF 
      T=T+P
   20 CONTINUE
      IF (T. EQ. 0.0) T= 0.0001
      DQ10=SQRT(SD2SQ/T)
      IF (LZEIDE) THEN
        DQ10=(SUMDR10/T)**(1/1.605)
      ENDIF 
      IF (DEBUG)WRITE(JOSTND,*)'SD2SQ,SUMDR10,DQ10= ',
     &  SD2SQ,SUMDR10,DQ10
C----------
C  START LOOP TO ESTIMATE MORTALITY RATE. TREES ARE PROCESSED
C  ONE AT A TIME WITHIN A SPECIES.
C----------
C  START SPECIES LOOP
      DO 50 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF (I1.LE.0) GOTO 50
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
      PTBAL=PTBALT(I)
      IF (P.LE.0.0) GOTO 40

C----------
C  COMPUTE SURVIVAL RATE
C
C  INDIVIDUAL TREE PROBABLITY OF SURVIVAL (ANNUAL) IS CALCULATED USING A MODIFIED
C  LOGISTIC EQUATION FORM:
C
C  PR(SURV) = EXP(X)/(1 + EXP(X))
C  X = B1 + B2*DBH + B3*DBH^2 + B4*BAL + B5*BAL/DBH
C
C  WHERE 
C  PR(SURV): IS ANNUAL PROBABILITY OF SURVIVAL
C  DBH: DIAMETER AT BREAST HEIGHT
C  BAL: POINT BASAL AREA IN TREES LARGER THAN SUBJECT TREE
C----------

C DETERMINE DIAMETER TO USE FOR SURVIVAL CALCULATION
C VALUE IN DSURV ARRAY IS USED WHEN DBH OF RECORD IS BELOW THE DSURV VALUE.
C THESE VALUES ARE USED TO AVOID MASS SEEDLING MORTALITY.
      DTEMP = D
      IF(DTEMP .LT. DSURV(ISPC)) DTEMP = DSURV(ISPC)

C CALCULATE SURVIVAL
      RIP = BM1(ISPC) + BM2(ISPC)*DTEMP + BM3(ISPC)*DTEMP*DTEMP +
     &  BM4(ISPC)*PTBAL + BM5(ISPC)*PTBAL/DTEMP

      RIP = EXP(RIP)/(1 + EXP(RIP))

C DETERMINE IF DEBUG IS NEEDED
      IF (DEBUG)WRITE(JOSTND,*)'IN MORTS',' I=',I,' ISPC=', ISPC,
     & ' D=',D,' DTEMP=',DTEMP,' PTBAL=',PTBAL,' RIP=',RIP
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
      IF (IESTAT(I).LE.0) GOTO 29
      IF (IY(ICYC).GE.IESTAT(I)) IESTAT(I)=0
      XCHECK= FLOAT(IESTAT(I)-IY(ICYC))/FINT
      IF (XCHECK.GT.1.0) XCHECK=1.0
      IF (XCHECK.LT.0.0) XCHECK=0.0
      X=X* (1.0-XCHECK)
   29 CONTINUE
C
C CONSTRAIN SURVIVAL PREDICTION
      IF (RIP.GT.0.99999) RIP=0.99999
      IF (RIP.LT.0.001) RIP=0.0
C
C PROBABILITY OF SURVIVAL IS CONVERTED TO PROBABILITY OF MORTALITY ADJUSTED BY
C CYCLE LENGTH THROUGH THE FOLLOWING: 1 - PR(SURV)**FINT
      WKI= P*(1.0 - (RIP**FINT))*X
C
C DETERMINE IF DEBUG IS NEEDED
      IF (DEBUG)
     &  WRITE(JOSTND,*)'IN MORTS EQ I,X,P,RIP,FINT,WKI= ',
     &  I,X,P,RIP,FINT,WKI
C
C DETERMINE IF PREDICTED MORTALITY IS GREATER THAN SIZE CAP IMPOSED 
C MORTALITY RATE
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/10.0)
      IDMFLG=IFIX(SIZCAP(ISPC,3))
      IF ((D+G).GE.SIZCAP(ISPC,1) .AND. IDMFLG.NE.1) THEN
         WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
        IF (DEBUG)WRITE(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',
     &  'I,ISPC,D,P,SIZCAP 1-3,WKI = ',
     &  I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
      ENDIF
      IF (WKI.GT.P) WKI=P
C----------
C  IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
C  SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.
C----------
      IF (SDIMAX .LT. 5) THEN
        WKI=P
      ENDIF
C----------
C  RECALCULATE VALUES AFTER MORTALITY HAS BEEN APPLIED
C---------- 
      TA=TA+(PROB(I)-WKI)
      WK2(I)=WKI
      IF (DEBUG)WRITE(JOSTND,*)'IN MORTS INITIAL PASS: IXX WKI WK2=',
     &  I,WKI,WK2(I)
   40 CONTINUE 
   50 CONTINUE
      GOTO 59
C----------
C  RECALCULATE SDI AND BA FOR THE STAND AND MAKE SURE THEY ARE BELOW THE MAXIMUM
C----------
   55 CONTINUE
      DQ10A=SQRT(SD2SQA/TA)
      BAA=.005454154*DQ10A*DQ10A*TA
      IF (LZEIDE) THEN
        DQ10A=(SUMDR10A/TA)**(1./1.605)
      ENDIF
      SDIA=TA*(DQ10A/10)**1.605
      IF (DEBUG)WRITE(JOSTND,*)'IN MORTS:PASS,TA,SD2SQA,SUMDR10A,DQ10A',
     &  ',BAA,SDIA=',PASS,TA,SD2SQA,SUMDR10A,DQ10A,BAA,SDIA
C----------
C  LOOP BACK THROUGH TREES RECORDS IF STAND IS OVER MAXIMUM SDI.
C  BECAUSE BA CAN CREEP UP TO UNREALISTIC VALUES EVEN WHILE
C  HOLDING SDI BELOW MAXIMUM, ANOTHER PASS IS MADE IF A STAND'S
C  BASAL AREA EXCEEDS THE BASAL AREA MAXIMUM OF THE STAND
C----------
      IF(DEBUG)WRITE(JOSTND,*)' IN MORTS',' SDIA=',SDIA,' SDIUPR=',
     & SDIUPR,' BAA=',BAA,' BAMAX=',BAMAX,' PASS=',PASS
      IF(((SDIA.LT.SDIUPR).AND.(BAA.LT.BAMAX)).OR.(PASS.GT.100)) GOTO 70
      PASS=PASS+1
      SD2SQA=0
      SUMDR10A=0   
      TA=0
  59  CONTINUE
      WKIT2=0.0
      DO 60 I=1,ITRN
      WKI=WK2(I)*(PASS)
      IF (WKI.GT.PROB(I)) WKI=PROB(I)
      WKIT2=WKI+WKIT2
      WKII(I)=WKI
      IF (LZEIDE.AND.(DBH(I).LT.DBHZEIDE)) GOTO 60         ! BRANCH IF D IS LT MIN DBH
      IF (.NOT.LZEIDE.AND.(DBH(I).LT.DBHSTAGE)) GOTO 60
      SD2SQA=SD2SQA+(PROB(I)-WKI)*(DBH(I)*DBH(I)+CIOBDS1(I))
      IF (LZEIDE) THEN
        SUMDR10A=SUMDR10A+(PROB(I)-WKI)*(DBH(I)+G1(I))**1.605
      ENDIF 
      TA=TA+(PROB(I)-WKI)
      IF (DEBUG) WRITE(JOSTND,*)'IN MORT PASS,I,DBH,WKI,WKIT2,SD2SQA,
     &  SUMDR10A,TA=',PASS,I,DBH(I),WKI,WKIT2,SD2SQA,SUMDR10A,TA
   60 CONTINUE
      GOTO 55
C----------
C  END OF TREE LOOPING.  PRINT DEBUG INFO IF DESIRED.
C----------
   70 CONTINUE
      DO I=1,ITRN 
        IF (PASS.GT.1) WK2(I)=WKII(I)
        IF (WK2(I).GT.PROB(I)) WK2(I)=PROB(I)
        IF (DEBUG) THEN
          PRES=PROB(I)-WK2(I)
          VLOS=WK2(I)*CFV(I)/FINT
          WRITE(JOSTND,9000) I,ISPC,DBH(I),G1(I),BA,PROB(I),
     &    WK2(I),PRES,VLOS
 9000     FORMAT('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &    ',  DBH INCREMENT=',F7.4,',  BA=',F7.2,
     &    ',  INIT PROB=',F9.3/
     &    ', TREES DYING=',F9.3,'  RES PROB=',F9.3,
     &    ',  VOL LOST=',F9.3)
        ENDIF
      ENDDO
      IF (DEBUG) WRITE(JOSTND,*) 'IN MORTS,  ISPC=', ISPC
  100 CONTINUE
C
C----------
C  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES
C---------
      CALL CLMORTS
C----------
C  BEGIN FIXMORT ACTIVITY PROCESS IF SCHEDULED
C  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
C  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT
C----------
      CALL OPFIND (1,MYACTS(2),NTODO)
      IF (NTODO.GT.0) THEN
        IF (DEBUG) WRITE(JOSTND,*)'FIXMORT PROCESSING, ITODO= ',ITODO
        DO ITODO=1,NTODO
          CALL OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
          IF (IACTK.LT.0) GOTO 300
          CALL OPDONE(ITODO,IY(ICYC))
          ISPCC=IFIX(PRM(1))
          IF (NP .LE. 4) THEN
            IF (PRM(2).GT. 1.0) PRM(2)=1.0
          ENDIF
          IF (PRM(3).LT. 0.0) PRM(3)=0.0
          IF (PRM(4).LE. 0.0) PRM(4)=999.
          IP=1
          IF (NP.GT.4) THEN
            IF (PRM(5).LT.3.) THEN
              IF (PRM(2).GT. 1.0) PRM(2)=1.0
              IF (PRM(2).LT. 0.0) PRM(2)=0.0
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
          IF (PRM(6).GT.0.) THEN
            IF (PRM(6) .EQ. 1) THEN
              KPOINT=1
            ELSEIF (PRM(6) .EQ. 10) THEN
              KBIG=1
            ELSEIF (PRM(6) .EQ. 11) THEN
              KPOINT=1
              KBIG=1
            ELSEIF (PRM(6) .EQ. 20) THEN
              KBIG=2
            ELSEIF (PRM(6) .EQ. 21) THEN
              KPOINT=1
              KBIG=2
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C----------
C END OF FIXMORT ACTIVITY PROCESS      
C----------
      IF (ITRN.GT.0 .AND. (KBIG .NE.0 .OR. KPOINT.NE. 0)) THEN
C----------
C IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
C EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
C   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
C   ARE IN EFFECT.
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
C   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.
C----------
        IF (KBIG.GE.1 .OR. (KPOINT.EQ.1 .AND. IPTINV.GT.1)) THEN
          XMORE=0.0
          DO I=1,ITRN
            LINCL = .FALSE.
            IF (ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I)) THEN
              LINCL = .TRUE.
            ELSEIF (ISPCC.LT.0) THEN
              IGRP = -ISPCC
              IULIM = ISPGRP(IGRP,1)+1
              DO IG=2,IULIM
                IF (ISP(I) .EQ. ISPGRP(IGRP,IG)) THEN
                  LINCL = .TRUE.
                  GOTO 91
                ENDIF
              ENDDO
            ENDIF
   91       CONTINUE
            IF (LINCL .AND.
     >        (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN

              SELECT CASE (IP)
              CASE (1)
                XMORE=XMORE+PROB(I)*PRM(2)
                WK2(I)=0.
              CASE (2)
                XMORE=XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
              CASE (3)
                TEMP=AMAX1(WK2(I),(PROB(I)*PRM(2)))
                IF (TEMP .GT. WK2(I)) THEN
                  XMORE=XMORE+TEMP-WK2(I)
                ENDIF
              CASE (4)
                XMORE=XMORE+WK2(I)*PRM(2)
                WK2(I)=0.
              END SELECT
            ENDIF

          ENDDO
          IF (DEBUG) WRITE(JOSTND,*)'KPOINT,KBIG,ITRN,XMORE= ',
     &               KPOINT,KBIG,ITRN,XMORE
          CREDIT=0.
          DO I=1,ITRN
            IWORK1(I)=IND1(I)
            IF (KBIG .EQ. 1) THEN
              WORK3(I)=(-1.0)*
     &               (DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
            ELSE
              WORK3(I)=DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
            ENDIF
          ENDDO
          CALL RDPSRT(ITRN,WORK3,IWORK1,.FALSE.)
          IF (DEBUG) THEN
            WRITE(JOSTND,*)'DBH= ',(DBH(IG),IG=1,ITRN)
            WRITE(JOSTND,*)'IWORK1= ',(IWORK1(IG),IG=1,ITRN)
            WRITE(JOSTND,*)'WK2= ',(WK2(IG),IG=1,ITRN)
          ENDIF

          IF (KBIG.GE.1 .AND. KPOINT.EQ.0) THEN
C
C           CONCENTRATION BY SIZE ONLY
C
            DO I=1,ITRN
              IX=IWORK1(I)
              LINCL = .FALSE.
              IF (ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX)) THEN
                LINCL = .TRUE.
              ELSEIF (ISPCC.LT.0) THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO IG=2,IULIM
                  IF (ISP(IX) .EQ. ISPGRP(IGRP,IG)) THEN
                    LINCL = .TRUE.
                    GOTO 93
                  ENDIF
                ENDDO
              ENDIF
   93         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(IX)-WK2(IX)
                IF ((TEMP .LE. XMORE).OR.
     >            (ABS(TEMP-XMORE).LT.0.0001)) THEN
                  CREDIT=CREDIT+PROB(IX)-WK2(IX)
                  WK2(IX)=PROB(IX)
                ELSE
                  WK2(IX)=WK2(IX)+XMORE-CREDIT
                  CREDIT=XMORE
                  GOTO 295
                ENDIF
              ENDIF
            ENDDO
            GOTO 295

          ELSEIF (KPOINT.EQ.1 .AND. KBIG.EQ.0) THEN
C
C           CONCENTRATION ON POINTS ONLY
C
            DO J=1,IPTINV
              DO I=1,ITRN
                IF (ITRE(I) .NE. J) GOTO 204
                LINCL = .FALSE.
                IF (ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I)) THEN
                  LINCL = .TRUE.
                ELSEIF (ISPCC.LT.0) THEN
                  IGRP = -ISPCC
                  IULIM = ISPGRP(IGRP,1)+1
                  DO IG=2,IULIM
                    IF (ISP(I) .EQ. ISPGRP(IGRP,IG)) THEN
                      LINCL = .TRUE.
                      GOTO 95
                    ENDIF
                  ENDDO
                ENDIF
   95           CONTINUE
                IF (LINCL .AND.
     >            (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                  TEMP=CREDIT+PROB(I)-WK2(I)
                  IF ((TEMP .LE. XMORE).OR.
     >              (ABS(TEMP-XMORE).LT.0.0001)) THEN
                    CREDIT=CREDIT+PROB(I)-WK2(I)
                    WK2(I)=PROB(I)
                  ELSE
                    WK2(I)=WK2(I)+XMORE-CREDIT
                    CREDIT=XMORE
                    GOTO 295
                  ENDIF
                ENDIF
  204           CONTINUE 
              ENDDO
            ENDDO
            GOTO 295

          ELSE
C
C           CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
C           WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
C           POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.
C
            DO J=1,IPTINV
              DO I=1,ITRN
                IX=IWORK1(I)
                IF (ITRE(IX) .NE. J) GOTO 311
                LINCL = .FALSE.
                IF (ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX)) THEN
                  LINCL = .TRUE.
                ELSEIF (ISPCC.LT.0) THEN
                  IGRP = -ISPCC
                  IULIM = ISPGRP(IGRP,1)+1
                  DO IG=2,IULIM
                    IF (ISP(IX) .EQ. ISPGRP(IGRP,IG)) THEN
                      LINCL = .TRUE.
                      GOTO 97
                    ENDIF
                  ENDDO
   97             CONTINUE
                ENDIF
                IF (LINCL .AND.
     >            (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                  TEMP=CREDIT+PROB(IX)-WK2(IX)
                  IF ((TEMP .LE. XMORE).OR.
     >              (ABS(TEMP-XMORE).LT.0.0001)) THEN
                    CREDIT=CREDIT+PROB(IX)-WK2(IX)
                    WK2(IX)=PROB(IX)
                  ELSE
                    WK2(IX)=WK2(IX)+XMORE-CREDIT
                    CREDIT=XMORE
                    GOTO 295
                  ENDIF
                ENDIF
  311           CONTINUE
              ENDDO
            ENDDO
            GOTO 295
          ENDIF
C
        ENDIF
      ELSEIF (ITRN .GT. 0 .AND. KBIG .EQ. 0 .AND. KPOINT .EQ. 0) THEN
C----------
C  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
C  IS NOT IN EFFECT.
C----------
        DO I=1,ITRN
          LINCL = .FALSE.
          IF (ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I)) THEN
            LINCL = .TRUE.
          ELSEIF (ISPCC.LT.0) THEN
            IGRP = -ISPCC
            IULIM = ISPGRP(IGRP,1)+1
            DO IG=2,IULIM
              IF (ISP(I) .EQ. ISPGRP(IGRP,IG)) THEN
                LINCL = .TRUE.
                GOTO 99
              ENDIF
            ENDDO
          ENDIF
   99     CONTINUE
          IF (LINCL .AND.
     >      (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN

            SELECT CASE (IP)
            CASE (1)
              WK2(I)=PROB(I)*PRM(2)
            CASE (2)
              WK2(I)=WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
            CASE (3)
              WK2(I)=AMAX1(WK2(I),(PROB(I)*PRM(2)))
            CASE (4)
              WK2(I)=AMIN1(PROB(I),WK2(I)*PRM(2))
            END SELECT
          ENDIF
        ENDDO
      ENDIF
  295 CONTINUE
  300 CONTINUE
      IF (DEBUG)WRITE(JOSTND,*)
     &   'ITODO,WK2= ',ITODO,(WK2(IG),IG=1,ITRN)

      RETURN
C
      ENTRY MORCON
      RETURN
      END
