      SUBROUTINE MORTS
      IMPLICIT NONE
C----------
C  **MORTS--WC  DATE OF LAST REVISION:  09/17/10
C----------
C  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
C  RECORDS AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
C  TREE RECORD.  MORTALITY RATE IS PREDICTED FROM DBH, DBH INCREMENT,
C  RELATIVE DIAMETER, AND STAND BASAL AREA. AN ADJUSTMENT IS MADE TO
C  INCREASE MORTALITY RATE AS THE STAND BASAL AREA APPROACHES THE
C  THEORETICAL MAXIMUM THAT THE HABITAT TYPE CAN SUPPORT.  THIS ROUTINE
C  IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH PREDICTION.  ENTRY
C  **MORCON** IS ACCESSED TO LOAD SITE DEPENDENT CONSTANTS.
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
COMMONS
C----------
C  DEFINITIONS:
C
C     PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE MORTALITY
C             RATE EQUATION.
C        I -- TREE SUBSCRIPT.
C        D -- TREE DIAMETER.
C      RIP -- ESTIMATED ANNUAL MORTALITY RATE BASED ON HAMILTON'S
C             MODEL.
C       RZ -- RATE SUCH THAT BASAL AREA IN 10 YEARS IS EQUAL TO BA
C             PLUS (BAMAX-BA)/BAMAX PROPORTION OF THE PREDICTED BA
C             INCREMENT.
C     RIPP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON BA, BAMAX,
C             RIP, AND RZ.  THIS IS THE RATE THAT IS USED TO PREDICT
C             TREE MORTALITY.  IT IS AN ANNUAL RATE THAT WILL BE
C             COMPOUNDED TO OBTAIN A FINT-YEAR RATE.
C        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
C             OF TREE MORTALITY (TREES/ACRE).
C----------
      LOGICAL DEBUG,LINCL
      REAL PMSC(MAXSP),PRM(6),PSP(MAXSP)
      INTEGER MYACTS(2)
      REAL CREDIT,TEMP,XMORE,VLOS,PRES,XCHECK,X,RIPP,POTENT,RIP,CRC
      REAL DGT,RELDBH,WKI,B5,D1,D2,XMORT,AVED,WPROB,DSUM,RZ,TTB
      REAL TB,BA10,DELTBA,DQ10,CIODBS,G,BRATIO,BARK,CIOBDS,D,P,SD2SQ,T
      REAL RELHT
      INTEGER NTODO,I,NP,IACTK,IDATE,ISPCC,IS,ISPC,I1,I2,I3,IP,IICR
      INTEGER IDMFLG,ITODO,KPOINT,KBIG,IGRP,IULIM,IG,IX,J,IPT,IPT2
C----------
C  DATA STATEMENTS.
C----------
      DATA MYACTS/94,97/
      DATA PSP/ 10*0.0, 6*1.0, 14*0.0, 2*1.0, 7*0.0 /
      DATA PMSC/ 39*.317888 /
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
      CALL SDICAL(SDIMAX)
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS CYCLE= ',ICYC,'  BAMAX= ',
     &BAMAX,'  SDIMAX= ',SDIMAX 
C----------
C  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.
C----------
      T=0.0
      SD2SQ=0.0
      DO 20 I=1,ITRN
      P=PROB(I)
      IS=ISP(I)
      D=DBH(I)
      BARK=BRATIO(IS,D,HT(I))
      G=DG(I)/BARK
      CIOBDS=(2.0*D*G+G*G)
      SD2SQ=SD2SQ+P*(D*D+CIOBDS)
      T=T+P
   20 CONTINUE
      DQ10=SQRT(SD2SQ/T)
      DELTBA=.005454154*DQ10*DQ10*T-BA
C----------
C  PROJECT BASAL AREA FORWARD 10 YEARS ASSUMING THAT BA/BAMAX
C  PROPORTION OF THE PREDICTED BASAL AREA INCREMENT WILL BE LOST TO
C  MORTALITY.  AT BA=BAMAX, MORTALITY WILL ABSORB ALL INCREMENT.
C----------
      BA10=BA+((BAMAX-BA)/BAMAX)*DELTBA
      TB=BA10/(.005454154*DQ10*DQ10)
C----------
C  NOW CONVERT TO AN ESTIMATE ON ANNUAL TPA MORTALITY RATE.
C----------
      TTB=(T-TB)/T
      IF(TTB.GT.0.9999) TTB=0.9999
      RZ=1.0-(1.0-(TTB))**0.1
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS T,SD2SQ,DQ10,BA,DELTBA,BAMAX,'
     &,'BA10,TB,TTB,RZ= ',T,SD2SQ,DQ10,BA,DELTBA,BAMAX,BA10,TB,TTB,RZ
C----------
C  CALCULATE AVERAGE DBH
C----------
      DSUM=0.0
      WPROB=0.0
      DO 51 I=1,ITRN
      WPROB=WPROB+PROB(I)
   51 DSUM=DSUM+DBH(I)*PROB(I)
      AVED=DSUM/WPROB
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS DSUM,WPROB,AVED= ',DSUM,WPROB,
     &AVED
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
      B5=PMSC(ISPC)
C----------
C  START TREE LOOP WITHIN SPECIES.
C----------
      DO 40 I3=I1,I2
C----------
C  INITIALIZE FOR NEXT TREE.
C----------
      I=IND1(I3)
      P=PROB(I)
      WKI=0.0
      WK2(I)=0.0
      D=DBH(I)
      BARK=BRATIO(ISPC,D,HT(I))
      IF(P.LE.0.0) GO TO 40
      RELDBH=D/AVED
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
      IF((ICYC.EQ.1 .OR. WK1(I).EQ.0.0) .AND. DG(I).GT.0.5)
     &        G=DG(I)/(BARK*10.0)
      IP=1
      IF(D.LE.5.0)IP=2
C----------
C WC MORTALITY FUNCTION. PREDICTS 12-YEAR SURVIVAL RATE, SO TAKE
C 1-RATE TO GET MORTALITY RATE AND COVERT TO ANNUAL RATE WITH
C A DISCOUNT FORMULA. EXCEPT FOR OREGON WHITE OAK WHICH HAS A 
C DIFFERENT EQUATION BUILT BY HARRINGTON AND GOULD.
C ESM 081210- CORRECTION MADE TO PSP COEFFICIENT FROM NEGATIVE TO POSITIVE
C----------
      IF (ISPC .EQ. 28)THEN
        RELHT = 0.0
        IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
        IF(RELHT .GT. 1.5)RELHT=1.5
        RIP=-6.6707 + 0.5105*ALOG(5+BA) - 1.3183*RELHT
        RIP=(1.0/(1.0+EXP(RIP)))
        RIP=1.0-RIP
      ELSE
        IICR=((ICR(I)-1)/10)+1
        CRC=FLOAT(IICR)
        RIP= -2.10198 - 0.036595*D - 0.165802*CRC + 0.987133*PSP(ISPC)
        RIP=(1.0/(1.0+EXP(RIP)))
        RIP=1.0-RIP
        RIP=EXP(ALOG(RIP+1.0)/12.)-1.0
      ENDIF
C----------
      POTENT=REIN(IP)
      RIP=RIP*POTENT
      RIPP=BA*RZ
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS I,ISPC,IICR,D,PSP,RIP,POTENT,
     &BA,RZ,RIPP=',I,ISPC,IICR,D,PSP(ISPC),RIP,POTENT,BA,RZ,RIPP
C----------
C  MAKE ADJUSTMENT FOR APPROACH TO MAXIMUM BASAL AREA.
C----------
      IF(BA.GT.BAMAX)GO TO 27
      RIPP=RIPP+(BAMAX-BA)*RIP
   27 RIPP=RIPP/BAMAX
      IF(RIPP.LT.RIP)RIPP=RIP
      IF(RIPP.GT.1.0)RIPP=1.0
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
      WKI=P*(1.0-(1.0-RIPP)**FINT)*X
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS I,XCHECK,X,P,RIPP,FINT,WKI= ',
     &I,XCHECK,X,P,RIPP,FINT,WKI
C
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/10.0)
      IDMFLG=IFIX(SIZCAP(ISPC,3))
      IF((D+G).GE.SIZCAP(ISPC,1) .AND. IDMFLG.NE.1) THEN
        WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
        IF(DEBUG)WRITE(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',
     &  'I,ISPC,D,P,SIZCAP 1-3,WKI = ',
     &  I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
      ENDIF
      IF(WKI.GT.P) WKI=P
C----------
C IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
C SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.
C----------
      IF(SDIMAX .LT. 5)THEN
        WKI=P
      ENDIF
      WK2(I)=WKI
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(.NOT.DEBUG) GO TO 40
      PRES=P-WKI
      VLOS=WKI*CFV(I)/FINT
      WRITE(JOSTND,9000) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
 9000 FORMAT('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,
     &       ',  INIT PROB=',F9.3/
     &       ', TREES DYING=',F9.3,'  RES PROB=',F9.3,
     &       ',  VOL LOST=',F9.3)
   40 CONTINUE
C----------
C  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(.NOT.DEBUG) GO TO 50
      WRITE(JOSTND,9001)  ISPC,B5
 9001 FORMAT('IN MORTS,  ISPC=',I3,
     &       11X,'B5=',F8.6)
   50 CONTINUE
C
  100 CONTINUE
C
C----------
C  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES
C---------
      CALL CLMORTS
C
C
C----------
C  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
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
C----------
C  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT REQUIRE ONE-
C  TIME RESOLUTION.  ITYPE IS A HABITAT INDEX THAT IS COMPUTED IN
C  **HABTYP** AND BAMAXA IS AN ARRAY CONTAINING ESTIMATES OF MAXIMUM
C  BASAL AREA BY HABITAT.  BAMAX IS LOADED WITH MAXIMUM BASAL AREA FOR
C  THE APPROPRIATE HABITAT TYPE.
C----------
C
C VALUES ARE SET HERE FOR THE ST.JOE, 260 TYPE FOR NOW.  NEED VALUES
C FOR WC FROM LIGHTNER TO SET THIS CORRECTLY.
C
      IPT=15
      POTEN=0.95
      REIN(1)=(1-(POTEN/20.+1.)**(-1.605))/.06821
      IPT2=49
      POTEN=2.65
      REIN(2)=(1-(POTEN+1.)**(-1.605))/.86610
      RETURN
      END
