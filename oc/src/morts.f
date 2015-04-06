      SUBROUTINE MORTS
      IMPLICIT NONE
C----------
C  **MORTS--OC   DATE OF LAST REVISION:  04/06/15
C----------
C  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR
C  EACH TREE RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE
C  REPRESENTED BY THE TREE RECORD.
C  THIS ROUTINE IS CALLED FROM **GRINCR** WHEN CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **MORCON** IS CALLED TO LOAD SITE DEPENDENT
C  CONSTANTS.
C
C  ORGANON MORTALITY RATES FOR EACH TREE RECORD (REGARDLESS OF IORG(i)
C  VALUE) WERE LOADED IN THE CALL TO **EXECUTE** FROM **DGDRIV**. 
C  USE ORGANON RATES FOR ALL TREES; APPLY USER ADJUSTMENTS.
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
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
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
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
      INCLUDE 'ORGANON.F77'
C
C
C
COMMONS
C----------
      REAL PRM(6)
      INTEGER MYACTS(2)
      INTEGER IDMFLG,NTODO,I,NP,IACTK,IDATE,ISPCC
      INTEGER I1,I2,I3,IS,KNT2,ISPC,ITODO,IP
      INTEGER KPOINT,KBIG,IGRP,IULIM,IG,IX,J
      REAL P,D,BARK,BRATIO,G,BADEAD
      REAL TNEW,BANEW,QMDNEW,ADJFAC,XMORE,TEMP,CREDIT
      REAL XMORT,D1,D2
      REAL WKI,RI,RIP,PRES,VLOS
      LOGICAL DEBUG,LINCL
C----------
C  DATA STATEMENTS.
C----------
      DATA MYACTS/94,97/
C----------
C SPECIES ORDER IN CA VARIANT:
C  1=PC  2=IC  3=RC  4=GF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
C 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
C 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
C 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
C 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH
C
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'MORTS',5,ICYC)
      IF(DEBUG)WRITE(JOSTND,9000)ICYC
 9000 FORMAT(' ENTERING SUBROUTINE MORTS  CYCLE =',I4)
C-----------
C  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
C  MULTIPLIERS AND BRANCH TO END.
C-----------
      CALL OPFIND(1,MYACTS(1),NTODO)
      IF(NTODO .EQ. 0)GO TO 25
      DO 24 I=1,NTODO
      CALL OPGET(I,4,IDATE,IACTK,NP,PRM)
      CALL OPDONE(I,IY(ICYC))
      ISPCC=IFIX(PRM(1))
      IF(ISPCC .EQ. 0)GO TO 21
      XMMULT(ISPCC)=PRM(2)
      XMDIA1(ISPCC)=PRM(3)
      XMDIA2(ISPCC)=PRM(4)
      GO TO 24
   21 CONTINUE
      DO 22 ISPCC=1,MAXSP
      XMMULT(ISPCC)=PRM(2)
      XMDIA1(ISPCC)=PRM(3)
      XMDIA2(ISPCC)=PRM(4)
   22 CONTINUE
   24 CONTINUE
   25 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9010)ICYC,RMSQD
 9010 FORMAT(1H0,'IN MORTS 9010 ICYC,RMSQD= ',
     & I5,5X,F6.2)
C----------
C  START LOOP TO LOAD ORGANON MORTALITY RATES.
C  TREES ARE PROCESSED ONE AT A TIME WITHIN A SPECIES.
C  APPLY ANY MODIFIERS ENTERED WITH THE MORTMULT KEYWORD.
C----------
      IF (ITRN.LE.0) GOTO 100
C
      DO 50 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.LE.0) GO TO 50
      I2=ISCT(ISPC,2)
      XMORT = XMMULT(ISPC)
      D1=XMDIA1(ISPC)
      D2=XMDIA2(ISPC)
C----------
C  START TREE LOOP WITHIN SPECIES.
C----------
      DO 40 I3=I1,I2
C----------
C  INITIALIZE FOR NEXT TREE.
C----------
      I=IND1(I3)
      P=PROB(I)
      WK2(I)= MORTEXP(I) ! FROM ORGANON COMMON BLOCK
      IF(P.LE.0.0) GO TO 40
      D=DBH(I)
      IF(DEBUG) WRITE(JOSTND,*) ' IN MORTS I,ISPC,D,WK2,IORG= ',
     *I,ISPC,D,WK2(I),IORG(I) 
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG) THEN
        PRES=P-WK2(I)
        VLOS=WK2(I)*CFV(I)/FINT
        WRITE(JOSTND,9080) I,ISPC,D,P,WK2,PRES,VLOS
 9080   FORMAT(' IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &       ',  INIT PROB=',F9.3,
     &       ',  TREES DYING=',F9.3,'  RES PROB=',F9.3,
     &       ',  VOL LOST=',F9.3)
      ENDIF
   40 CONTINUE
C----------
C  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
   50 CONTINUE
C----------
C  LOOP THROUGH TREES AND CHECK FOR SIZE (aka AGE) CAP RESTRICTIONS
C----------
      DO 354 I=1,ITRN
      IS = ISP(I)
      D = DBH(I)
      P = PROB(I)
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      IDMFLG=IFIX(SIZCAP(IS,3))
      IF((D+G).GE.SIZCAP(IS,1) .AND. IDMFLG.NE.1) THEN
        WK2(I) = AMAX1(WK2(I),(P*SIZCAP(IS,2)*FINT/5.0))
        IF(WK2(I) .GT. P)WK2(I)=P
        IF(DEBUG)WRITE(JOSTND,*)' SIZE CAP RESTRICTION IMPOSED, ',
     &  'I,IS,D,P,SIZCAP 1-3,WK2 = ',
     &  I,IS,D,P,SIZCAP(IS,1),SIZCAP(IS,2),SIZCAP(IS,3),WK2(I)
      ENDIF
  354 CONTINUE
C----------
C  CHECK TO SEE IF BA IS STILL WITHIN LIMITS. IF OUT OF BOUNDS,
C  ADJUST THE MORTALITY VALUES PROPORTIONATELY ACROSS ALL TREE RECORDS.
C----------
      KNT2=0
9001  CONTINUE
      TNEW=0.
      BANEW=0.
      QMDNEW=0.
      BADEAD=0.
      DO 36 I=1,ITRN
      P=PROB(I)-WK2(I)
      D=DBH(I)
      BARK=BRATIO(ISP(I),D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      TNEW=TNEW+P
      BANEW=BANEW+(0.0054542*(D+G)**2.)*P
      BADEAD=BADEAD+(0.0054542*(D+G)**2.)*WK2(I)
      QMDNEW=QMDNEW + ((D+G)**2.)*P
      IF(DEBUG)WRITE(JOSTND,*)' I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD= ',
     &I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD 
   36 CONTINUE
      IF(TNEW .GT. 0.) THEN
        QMDNEW=SQRT(QMDNEW/TNEW)
      ELSE
        QMDNEW = 0.
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' ICYC,BANEW,BAMAX,TNEW,QMDNEW,KNT2= ',
     &ICYC,BANEW,BAMAX,TNEW,QMDNEW,KNT2
      IF((BANEW-BAMAX) .GT. 1.) THEN
C----------
C       CALCULATE ADJUSTMENT FACTOR NEEDED TO GET RESIDUAL BA WITHIN
C       THE BA MAXIMUM LIMIT.
C----------
        ADJFAC = ((BANEW-BAMAX)/BADEAD)
        IF(DEBUG)WRITE(JOSTND,*)' BANEW,BAMAX,ADJFAC= ',
     &  BANEW,BAMAX,ADJFAC
C----------
C       LOOP THROUGH THE TREE LIST AND ADJUST THE MORTALITY VALUES.
C----------
        TNEW=0.
        DO 1500 I=1,ITRN
        P=PROB(I)
        WKI=WK2(I)*(1+ADJFAC)
        IF(WKI.GT.P) WKI=P
        WK2(I)=WKI
        IF(DEBUG)WRITE(JOSTND,*)' ADJUSTING FOR BAMAX I,P,WKI= ',
     &  I,P,WKI
C----------
C       PRINT DEBUG INFO IF DESIRED.
C----------
        TNEW=TNEW+P-WKI
        IF(.NOT.DEBUG) GO TO 1500
        PRES=P-WKI
        VLOS=WKI*CFV(I)/FINT
        WRITE(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
 1500   CONTINUE
C----------
C LOOP BACK AND SEE IF THE BAMAX TARGET HAS BEEN ACHIEVED YET.
C (I.E. IF THE COMPUTED MORTALITY RATE EXCEEDED THE PROB, AND WE 
C  HAD TO LIMIT MORTALITY TO THE PROB VALUE FOR SOME TREE RECORDS,
C  THEN WE MAY NOT REACH THE BA LIMIT IN ONE PASS.) 
C----------
        KNT2=KNT2+1
        IF(KNT2 .LT. 100)GO TO 9001
C
        IF(DEBUG)WRITE(JOSTND,*)' AFTER BA ADJUSTMENT RESIDUAL TPA = ',
     &  TNEW
      ENDIF
C
  100 CONTINUE
C
C----------
C  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES
C---------
      CALL CLMORTS
C----------
C  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
C  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT
C----------
      CALL OPFIND (1,MYACTS(2),NTODO)
      IF (NTODO.GT.0) THEN
        IF(DEBUG)WRITE(JOSTND,*)' FIXMORT PROCESSING, ITODO= ',ITODO
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
              IF(DEBUG)WRITE(JOSTND,*)' KPOINT,KBIG,ITRN,XMORE= ',
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
              IF(DEBUG)WRITE(JOSTND,*)' DBH= ',(DBH(IG),IG=1,ITRN)
              IF(DEBUG)WRITE(JOSTND,*)' IWORK1= ',(IWORK1(IG),IG=1,ITRN)
              IF(DEBUG)WRITE(JOSTND,*)' WK2= ',(WK2(IG),IG=1,ITRN)
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
         IF(DEBUG)WRITE(JOSTND,*)' ITODO,WK2= ',
     &    ITODO,(WK2(IG),IG=1,ITRN)
         ENDIF
  300    CONTINUE
      ENDIF
      RETURN



C
      ENTRY MORCON
C----------
C  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT
C  REQUIRE ONE-TIME RESOLUTION.
C----------
      RETURN
      END
