!== last modified  09-15-2016
C 01/11/2013 added volume calculation for stump vol(14) and tip vol(15)
C 11/06/2012 Changed errflag to 12 when NUMLOGS > 20
C 09/15/2016 Added output variable LOGDIA,LOGLEN,LOGVOL to the routine
C 03/20/2017 Added BOLHT variable and modified volume calculation for 
C            topwood/tip volume (it was overestimated)
      SUBROUTINE R4VOL(REGN,VOLEQ,MTOPP,HTTOT,DBHOB,HT1PRD,VOL,NOLOGP,
     >           NOLOGS,LOGDIA,LOGLEN,LOGVOL,BOLHT,
     +           CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG)
      IMPLICIT NONE
C**********************************************************************
      CHARACTER*10 VOLEQ
      
      INTEGER II,L,M,NUM,NUMLGS,ERRFLAG,I,REGN,J
      INTEGER CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
C                            BUT MAY BE USED IN RMSTAND, FVS, ETC
      REAL B,BFGRS,BUTTCF,CF0,CFGRS,BFINT,BFSCR,CFVOL,THT,HT1PRD
      REAL CFCOEF(20,7),CORDS,DBHOB,DLG(20),MTOPP,TOPDIA
      REAL DSM(20),D67,F,HTTOT,HT67,LEN,MERLEN,SCRIBC(70,10)
      REAL STUMPD,TOPLEN,TOTLGS,TRM,VOL(15),VOLR4(3)
      REAL NOLOGP,NOLOGS,DRATIO
      REAL LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20)
      REAL DIBSM(20),DIBLG(20),BOLHT(21),TOPVOL,HTUP,DIB

C
C  COEFFICIENTS TO DETERMINE HEIGHT TO 2/3 DBHOB (OUTSIDE BARK),
C  STUMP DIAMETER,  AND DIAMETER (INSIDE BARK) AT 2/3 TOTAL HTTOT.
C  comment out 03/21/2017 YW
c      DATA CFCOEF/0.5563,0.4496,0.2359,0.8435,1.1152,0.5405,1.2552,
c     + 0.4302,0.4727,2.0461,0.6294,1.2467,0.5334,0.5301,0.8315,
c     + 0.5028,0.7572,0.6310,0.4850,0.6076,-0.0636,-0.3956,
c     + -0.7237,-0.2800,-0.3678,-0.5908,-0.1727,-0.5229,-0.0508,-0.5080,
c     + -0.0939,-0.1615,-0.0071,0.2327,0.0967,-0.0942,-0.4527,-0.1026,
c     + -0.3636,-0.1730,0.9900,1.2756,1.6343,1.0264,0.9828,
c     + 1.3374,0.9435,1.3824,1.0615,0.9113,0.9886,0.9013,1.0127,0.8748,
c     + 0.8293,1.0600,1.1502,1.0230,1.2324,1.0585,
c     + -0.2,-0.041,-0.041,-0.041,-0.191,-0.191,-0.131,-0.131, 
c     + -0.159,-0.041,-0.488,-0.200,-0.200,-0.365,-0.365,-0.365,-0.143,
c     + -0.143,0.000,-0.153,0.964,0.884,0.884,0.884,0.943,0.943,
c     + 0.886,0.886,0.832,0.884,0.894,0.964,0.964,0.887,0.887,0.887,
c     + 0.933,0.933,0.887,0.883,
c     + -0.201,-0.041,-0.041,-0.041,-0.192,-0.192,-0.159,
c     + -0.159,-0.055,-0.041,-0.445,-0.201,-0.201,-0.367,-0.367,-0.367,
c     + -0.144,-0.144,0.000,-0.143,0.968,0.888,0.888,0.888,
c     + 0.947,0.947,0.891,0.891,0.837,0.888,0.897,0.968,0.968,
c     + 0.891,0.891,0.891,0.937,0.937,0.893,0.886/
C
C  SCRIBNER DECIMAL C TABLE FOR LENGTHS 2' THRU 20', DIA 6" TO 75"
C
      DATA SCRIBC/3*0.,5*1.,4*2.,2*3.,3*4.,3*5.,6.,7.,3*8.,2*9.,2*10.,
     + 2*11.,12.,2*14.,15.,16.,17.,2*18.,19.,20.,2*21.,23.,2*24.,25.,
     + 26.,27.,28.,29.,30.,32.,33.,34.,35.,36.,37.,39.,40.,41.,42.,
     + 44.,45.,47.,48.,49.,51.,52.,54.,
C !1/94 FOLLOWING LINE WAS 5*1.,3*2.,2*3.,....
     + 3*0.,2*1.,3*2.,2*3.,2*4.,2*6.,7.,2*8.,2*10.,11.,13.,14.,2*15.,
     + 16.,2*18.,2*20.,22.,23.,26.,27.,28.,30.,32.,33.,35.,37.,38.,39.,
     + 41.,43.,45.,47.,48.,50.,52.,54.,56.,59.,61.,63.,65.,67.,70.,72.,
     + 74.,77.,79.,82.,85.,87.,90.,93.,96.,98.,101.,104.,107.,
     * 4*1.,2*2.,3.,2*4.,5.,6.,7.,8.,9.,11.,12.,13.,14.,15.,17.,
     * 19.,21.,22.,23.,25.,27.,28.,29.,30.,33.,35.,39.,40.,42.,45.,48.,
     * 50.,52.,56.,57.,59.,62.,65.,67.,70.,73.,76.,79.,82.,85.,88.,91.,
     * 95.,98.,101.,105.,108.,112.,116.,119.,123.,127.,131.,135.,139.,
     * 144.,148.,152.,157.,161.,
     +  3*1.,2.,2*3.,4.,5.,6.,7.,8.,9.,11.,12.,14.,15.,17.,19.,21.,23.,
     + 25.,27.,29.,31.,33.,36.,37.,39.,40.,44.,46.,51.,54.,56.,60.,64.,
     + 67.,70.,74.,76.,79.,83.,86.,90.,94.,97.,101.,105.,109.,113.,
     * 118.,122.,126.,131.,135.,140.,145.,149.,154.,159.,164.,170.,175.,
     * 180.,186.,192.,197.,203.,209.,215.,
     +  2*1.,2.,2*3.,4.,5.,6.,7.,9.,10.,12.,13.,15.,17.,19.,21.,23.,25.,
     + 29.,31.,34.,36.,38.,41.,44.,46.,49.,50.,55.,58.,64.,67.,70.,75.,
     * 79.,84.,87.,93.,95.,99.,104.,108.,112.,117.,122.,127.,132.,137.,
     * 142.,147.,152.,158.,163.,169.,175.,181.,187.,193.,199.,206.,212.,
     * 219.,226.,232.,240.,247.,254.,261.,269.,
     + 1.,2*2.,2*3.,4.,6.,7.,9.,11.,12.,14.,16.,18.,21.,23.,25.,28.,30.,
     * 34.,37.,41.,44.,46.,49.,53.,55.,59.,60.,66.,69.,77.,80.,84.,90.,
     = 95.,101.,105.,111.,114.,119.,124.,130.,135.,140.,146.,152.,158.,
     = 164.,170.,176.,183.,189.,196.,203.,210.,217.,224.,232.,239.,247.,
     = 254.,262.,271.,279.,287.,296.,305.,314.,323.,
     =  1.,2*2.,3.,4.,5.,7.,8.,10.,12.,14.,16.,19.,21.,24.,27.,29.,33.,
     = 35.,40.,44.,48.,51.,53.,57.,62.,64.,69.,70.,77.,81.,90.,93.,98.,
     = 105.,111.,117.,122.,129.,133.,139.,145.,151.,157.,164.,170.,177.,
     = 184.,191.,198.,206.,213.,221.,229.,237.,245.,253.,261.,270.,279.,
     = 288.,297.,306.,316.,325.,335.,345.,356.,366.,377.,
     = 2.,2*3.,4.,6.,7.,8.,10.,11.,14.,16.,18.,21.,24.,28.,30.,33.,38.,
     = 40.,46.,50.,55.,58.,61.,66.,71.,74.,78.,80.,88.,92.,103.,107.,
     = 112.,120.,127.,134.,140.,148.,152.,159.,166.,173.,180.,187.,195.,
     = 202.,210.,218.,227.,235.,244.,252.,261.,270.,280.,289.,299.,309.,
     = 319.,329.,339.,350.,361.,372.,383.,395.,406.,418.,430.,
     * 2.,2*3.,4.,6.,8.,9.,11.,13.,16.,18.,21.,24.,27.,31.,34.,38.,42.,
     * 45.,52.,56.,62.,65.,68.,74.,80.,83.,88.,90.,98.,104.,116.,120.,
     & 126.,135.,143.,151.,157.,166.,171.,178.,186.,194.,202.,211.,219.,
     * 228.,237.,246.,255.,264.,274.,284.,294.,304.,315.,325.,336.,348.,
     = 358.,370.,381.,393.,406.,419.,430.,444.,457.,471.,484.,
     & 2.,2*3.,4.,7.,8.,10.,12.,14.,18.,20.,23.,27.,30.,35.,38.,42.,47.,
     & 50.,57.,62.,68.,73.,76.,82.,89.,92.,98.,100.,109.,115.,129.,133.,
     & 140.,150.,159.,168.,174.,185.,190.,198.,207.,216.,225.,234.,243.,
     & 253.,263.,273.,283.,294.,304.,315.,327.,338.,350.,362.,373.,387.,
     & 398.,412.,423.,437.,452.,465.,478.,493.,508.,523.,538./
C    DBHOB=IDBH/10     !1/94 NOT USED THIS PGM. MAY BE USED BY OTHER PGMS
C    HTTOT=IHT          !1/94 NOT USED THIS PGM. MAY BE USED BY OTHER PGMS
      CFGRS=0.
      BFGRS=0.
      CORDS=0.
      ERRFLAG = 0
      do 13, i=1,15
         vol(i) = 0.0
   13 continue

C*********************************************************
C  DETERMINE INDEX 'II' FOR HT67, STUMPD, AND DIB67
C  BASED ON TAPER EQUATION NUMBER (VOLEQ)
C comment out 03/21/2017 YW
c      IF(VOLEQ(8:10).EQ.'746') THEN
c        II=1
c      ELSEIF(VOLEQ(8:10).EQ.'202'.AND.VOLEQ(1:3).EQ.'400') THEN
c        II=2
c      ELSEIF(VOLEQ(8:10).EQ.'202'.AND.VOLEQ(1:3).EQ.'405') THEN
c        II=3
c      ELSEIF(VOLEQ(8:10).EQ.'202'.AND.VOLEQ(1:3).EQ.'401') THEN
c        II=4
c      ELSEIF(VOLEQ(8:10).EQ.'019'.AND.VOLEQ(1:3).EQ.'400') THEN
c        II=5
c      ELSEIF(VOLEQ(8:10).EQ.'019'.AND.VOLEQ(1:3).EQ.'405') THEN
c        II=6
c      ELSEIF(VOLEQ(8:10).EQ.'015'.AND.VOLEQ(1:3).EQ.'400') THEN
c        II=7
c      ELSEIF(VOLEQ(8:10).EQ.'015'.AND.VOLEQ(1:3).EQ.'401') THEN
c        II=8
c      ELSEIF(VOLEQ(8:10).EQ.'081') THEN
c        II=9
c      ELSEIF(VOLEQ(8:10).EQ.'073') THEN
c        II=10
c      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'403') THEN
c        II=11
c      ELSEIF(VOLEQ(8:10).EQ.'108'.AND.VOLEQ(1:3).EQ.'400') THEN
c        II=12
c      ELSEIF(VOLEQ(8:10).EQ.'108'.AND.VOLEQ(1:3).EQ.'401') THEN
c        II=13
c      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'401') THEN
c        II=14
c      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'402') THEN
c        II=15
c      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'400') THEN
c        II=16
c      ELSEIF(VOLEQ(8:10).EQ.'093'.AND.VOLEQ(1:3).EQ.'400') THEN
c        II=17
c      ELSEIF(VOLEQ(8:10).EQ.'093'.AND.VOLEQ(1:3).EQ.'407') THEN
c        II=18
c      ELSEIF(VOLEQ(8:10).EQ.'020') THEN
c        II=19
c      ELSEIF(VOLEQ(8:10).EQ.'117') THEN
c        II=20
c      ELSE
c        ERRFLAG = 1
c        RETURN
c      ENDIF

      IF(DBHOB .LT. 1.0)THEN
        ERRFLAG = 3
        RETURN
      ENDIF
      IF(HTTOT .LE. 4.5) THEN
        ERRFLAG = 4
        RETURN
      ENDIF
C*********************************************************
c  sutract one foot from height for mathis equations
      THT = HTTOT-1.0
      if(THT.LE.5.0)then         !6/2002  If small tree use smailians and return
          VOL(1) = DBHOB*DBHOB*HTTOT*0.00272708
          RETURN
      endif
      
c      HT67=CFCOEF(II,1) * DBHOB**CFCOEF(II,2) * THT**CFCOEF(II,3)
C !1/94 NEW VARIABLE.  WAS STUMPD(3) CF & STUMPD(2) CORDS
c      BUTTCF= CFCOEF(II,5) * DBHOB + CFCOEF(II,4)
                              
c      STUMPD=(BUTTCF**2*THT/(THT-4.))**.5
c      D67 = CFCOEF(II,7) * DBHOB  * (2./3.) + CFCOEF(II,6)
c      CF0 = .002727 * (HT67 * STUMPD**2 + D67**2 * THT)
c      F = CF0/(.005454 * STUMPD**2 * THT)
c      B = (1.0-F) / (2.0*F)
      
      TRM=0.5    !TRIM
      
      HTUP = 0.0
      DIB = 0.0
      CALL R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,
     + HTUP,DIB,ERRFLAG )
C*****************************************************************
C  LOOP TO COMPUTE GROSS VOLS
C  !LIMIT BF TOP DIAMETER TO 6 INCHES MINIMUM
C  M=1 FOR BF; M=2 FOR CORDWOOD; M=3 FOR CF
C  NUM = LOG NUMBER - UP TO 20 PER TREE
      DO 500 M = 1,3
        VOLR4(M)=0.
        DO 300 NUM = 1,20
          DSM(NUM) = 0.0
          DLG(NUM) = 0.0
          DIBSM(NUM) = 0.0
          DIBLG(NUM) = 0.0
          BOLHT(NUM) = 0.0
300     CONTINUE
        NUMLGS = 0
        MERLEN = 0.0
        TOTLGS = 0.0
        TOPLEN = 0.0
C  IF THE TOP DIAMETER IS LESS THAN THE BUTTDIB, PROCEED
C  MERLEN IS CALCULATED FOR BOARD FT (M=1), CORDWOOD (M=2),CUBIC FT (M=3)
C  !1/94 BUTTCF NEW VAR

        IF(MTOPP.LT.6.0.AND. M.EQ.1) THEN
           TOPDIA=6.0
        ELSEIF(MTOPP.LE.0) THEN
           TOPDIA = 1.0
        ELSE
           TOPDIA = MTOPP
        ENDIF
        IF((M.EQ.2 .OR. M.EQ.3) .AND. TOPDIA.GE. BUTTCF) GOTO 500
        IF(M.EQ.1.AND.TOPDIA.GE.STUMPD) GOTO 500
        DRATIO = TOPDIA/STUMPD
        IF(DRATIO .LE. 0.0) DRATIO = 0.0001
        MERLEN = (THT - THT * (DRATIO)**(1.0/B))
        
C     NEW BROKEN TOP LOGIC FOR FVS
        IF(HT1PRD.GT.0 .AND. HT1PRD.LT.MERLEN)THEN
           MERLEN = HT1PRD
C          FIND THE DIAMETER TO THE MERCH HEIGHT
           TOPDIA=INT((STUMPD*((THT-MERLEN)/THT)**B+.0499)*10)/10.0
           IF(TOPDIA.LT.6.0.AND.BFPFLG.EQ.1) TOPDIA=6.0
C           IF((M.EQ.2 .OR. M.EQ.3) .AND. TOPDIA.GE. BUTTCF) GOTO 500
           IF(M.EQ.1.AND.TOPDIA.GE.STUMPD) GOTO 500
        ENDIF

        IF(MERLEN.LT.2.5) GOTO 500
C  TOTLGS = TOTAL NUMBER LOGS IN THE MERCHANTABLE LENGTH OF THE TREE
C  TOPLEN = LENGTH OF THE TOPLOG OF THE MERCH STEM
          TOTLGS = MERLEN/16.5
          TOPLEN = (TOTLGS - INT(TOTLGS)) * 16.5
          IF(TOPLEN.LT.2.5)THEN
            IF(TOTLGS.GE.1.0)THEN
              TOPLEN = 16.5
            ELSE
              TOPLEN = 0.0
            ENDIF
            TOTLGS = INT(TOTLGS)
            GOTO 600
          ENDIF
C  SET ACTUAL LENGTH OF THE TOPLOG TO A 2' INCREMENT + TRIM
C  IF TOPLOG IS GREATER THAN 16' SET IT TO 16' + TRIM
          DO 1000 L = 4,16,2
            IF(TOPLEN.LT.FLOAT(L)+TRM)THEN
              TOPLEN = FLOAT(L-2) + TRM
              GOTO 600
            ENDIF
1000      CONTINUE
          TOPLEN = 16.5
C  DETERMINE WHAT THE TOTAL NUMBER OF LOGS ACTUALLY IS
600       IF(TOTLGS.GT.0.0)THEN
            IF(TOPLEN.LT.16.5)THEN
              TOTLGS = INT(TOTLGS) + 1.
            ELSE
              TOTLGS = INT(TOTLGS)
            ENDIF
            MERLEN = (TOTLGS - 1.)*16.5 + TOPLEN
            LEN = 16.
C  BUTT LOG CALCULATIONS, IF THERE IS MORE THAN 1 LOG IN THE TREE
C  NUMLGS IS INTEGER VALUE OF TOTLGS
            NUMLGS = TOTLGS
            IF(NUMLGS.GT.1)THEN
              NUM=1
C  !1/94 BUTTCF NEW VAR. WAS STUMPD(M)
              IF(M.EQ.2 .OR. M.EQ.3) THEN
                DLG(1)=INT(BUTTCF + .499)
                DIBLG(1)=BUTTCF
              ENDIF
              IF(M.EQ.1) THEN
                DLG(1) = INT(STUMPD + .499)
                DIBLG(1)=STUMPD
              ENDIF
       
              DSM(1) = INT(STUMPD*((THT - 16.5)/THT)**B + .499)
              DIBSM(1) = STUMPD*((THT - 16.5)/THT)**B
C     CUBIC VOLUME              
              IF(M.EQ.3)THEN
                 CFVOL = INT(.002727*(DLG(1)**2+
     +                        DSM(1)**2)*16.*10.+.499)/10.
c                 LOGVOL(3,I) = CFVOL
                 LOGVOL(4,NUM) = CFVOL
                 VOLR4(M)=VOLR4(M)+CFVOL
	        ENDIF
              
C     BOARD FOOT VOLUME
              IF(M.EQ.1) THEN
	           
                 IF(REGN.EQ.7) THEN
                    CALL SCRIB (DSM(1),16.0,'N',BFSCR)
                    LOGVOL(1,NUM) = BFSCR
                 ELSE
                    IF(INT(DSM(1)-5.) .GT. 70)THEN
                      ERRFLAG = 4
                      DO 555, J = 1,15
                        VOL(J) = 0.0
555                   CONTINUE
                     RETURN
                    ENDIF
                    BFSCR = SCRIBC(INT(DSM(1)-5.),INT((16./2.)))
                    LOGVOL(1,NUM) = BFSCR*10
	           ENDIF
c                 LOGVOL(1,I) = BFSCR
                 VOLR4(M)=VOLR4(M) + BFSCR

                 CALL INTL14(DSM(1),16.0,BFINT)
c                 LOGVOL(7,I) = BFINT
                 LOGVOL(7,NUM) = BFINT
                 VOL(10) = VOL(10) + BFINT
              ENDIF
C  NOTE THAT CORDWOOD (M=2) ADDS TRIM INTO VOLR4
              IF(M.EQ.2)VOLR4(M)=VOLR4(M)+INT(.002727*(DLG(1)**2+
     +        DSM(1)**2)*(16.+TRM)*10.+.499)/10. !1/94 NEW ROUNDING

              IF(NUMLGS.GT.2)THEN
C  FOR LOGS 2 TO TOTLGS - 1
                 IF (NUMLGS .GT. 20) THEN
C                   ERRFLAG = 4
C Changed errflag to 12 (YW 11/06/2012)
                   ERRFLAG = 12
                   DO 666, J = 1,15
                     VOL(J) = 0.0
666                CONTINUE
                   RETURN
                ENDIF

                DO 1100 NUM = 2,NUMLGS-1
                  DLG(NUM) = DSM(NUM-1)
                  DIBLG(NUM) = DIBSM(NUM-1)
                  DSM(NUM)=INT(STUMPD*((THT-16.5*FLOAT(NUM))/THT)**B
     +            +.499)
                  DIBSM(NUM)=STUMPD*((THT-16.5*FLOAT(NUM))/THT)**B
C     CUBIC VOLUME              
                  IF(M.EQ.3)THEN
                    CFVOL = INT(.002727*(DLG(NUM)**2+DSM(NUM)**2)*
     +                          16.*10.+.499)/10.
c                    LOGVOL(3,I) = CFVOL
                    LOGVOL(4,NUM) = CFVOL
                    VOLR4(M)=VOLR4(M)+CFVOL
                  ENDIF
C     BOARD FOOT VOLUME
                  IF(M.EQ.1) THEN

                    IF(REGN.EQ.7) THEN
                       CALL SCRIB (DSM(NUM),16.0,'N',BFSCR)
                       LOGVOL(1,NUM) = BFSCR
                    ELSE
                     IF (INT(DSM(NUM)-5.) .GT. 70) THEN
                       ERRFLAG = 4
                       DO 777, J = 1,15
                         VOL(J) = 0.0
777                    CONTINUE
                       RETURN
                     ENDIF
                       BFSCR=SCRIBC(INT(DSM(NUM)-5.),INT((16./2.)))
                       LOGVOL(1,NUM) = BFSCR*10
                    ENDIF
c                    LOGVOL(1,I) = BFSCR
                    VOLR4(M)=VOLR4(M)+ BFSCR
                  
                    CALL INTL14(DSM(NUM),16.0,BFINT)
c                    LOGVOL(7,I) = BFINT
                    LOGVOL(7,NUM) = BFINT
                    VOL(10) = VOL(10) + BFINT
                  ENDIF
C  NOTE THAT CORDWOOD (M=2) ADDS TRIM INTO VOLR4
                  IF(M.EQ.2)VOLR4(M)=VOLR4(M)+INT(.002727*(DLG(NUM)**2
     +            +DSM(NUM)**2)*(16.+TRM)*10.+.499)/10.
1100            CONTINUE
              ENDIF
            ENDIF
C  TOP LOGS, AND/OR SINGLE LOG TREE
            NUM=NUMLGS
            IF(NUMLGS.EQ.1)THEN
              IF(M.EQ.2 .OR. M.EQ.3) THEN
                DLG(1)=INT(BUTTCF + .499)
                DIBLG(1)=BUTTCF
              ENDIF
              IF(M.EQ.1)THEN
                DLG(1) = INT(STUMPD + .499)
                DIBLG(1)=STUMPD
              ENDIF
            ELSE
              DLG(NUMLGS) = DSM(NUMLGS-1)
              DIBLG(NUMLGS) = DIBSM(NUMLGS-1)
            ENDIF
            LEN = TOPLEN-TRM
            DSM(NUMLGS)=INT(STUMPD*((THT-MERLEN)/THT)**B+.499)
            DIBSM(NUMLGS)=STUMPD*((THT-MERLEN)/THT)**B
C     CUBIC VOLUME              
            IF(M.EQ.3)THEN
               CFVOL =INT(.002727*(DLG(NUMLGS)**2+DSM(NUMLGS)**2)*
     +                    LEN*10.+.499)/10.
c               LOGVOL(3,I) = CFVOL
               LOGVOL(4,NUM) = CFVOL
               VOLR4(M)=VOLR4(M)+CFVOL
	      ENDIF
C     BOARD FOOT VOLUME
            IF(M.EQ.1) THEN
               IF(REGN.EQ.7) THEN
                  CALL SCRIB (DSM(NUMLGS),LEN,'N',BFSCR)
                  LOGVOL(1,NUM) = BFSCR
               ELSE
                     IF (INT(DSM(NUMLGS)-5.) .GT. 70) THEN
                       ERRFLAG = 4
                       DO 888, J = 1,15
                         VOL(J) = 0.0
888                    CONTINUE
                       RETURN
                     ENDIF
                  BFSCR = SCRIBC(INT(DSM(NUMLGS)-5.),INT((LEN/2)))
                  LOGVOL(1,NUM) = BFSCR*10
	         ENDIF
c               LOGVOL(1,I) = BFSCR
               VOLR4(M)=VOLR4(M) + BFSCR

               CALL INTL14(DSM(NUM),16.0,BFINT)
c               LOGVOL(7,I) = BFINT
               LOGVOL(7,NUM) = BFINT
               VOL(10) = VOL(10) + BFINT
            ENDIF
C  NOTE THAT CORDWOOD (M=2) ADDS TRIM INTO VOLR4
            IF(M.EQ.2)VOLR4(M)=VOLR4(M)+INT(.002727*
     +      (DLG(NUMLGS)**2+DSM(NUMLGS)**2)*(LEN+TRM)*10.+.499)/10.
          ENDIF
  500 CONTINUE
C**********************************************************************
C  CONVERT & STORE VOLUMES IN DESCRIPTIVE VARIABLES
      CFGRS=VOLR4(3)
	IF(REGN .EQ. 7) THEN
	   BFGRS = VOLR4(1)
	ELSE
	   BFGRS=VOLR4(1)*10
	ENDIF
      CORDS=VOLR4(2)/90
C  CONVERT TO NAT CRUISE VOL STANDARDS
      IF(CUTFLG .EQ. 1) VOL(1) = CF0
      IF(BFPFLG .EQ. 1) VOL(2) = BFGRS
      IF(CUPFLG .EQ. 1)THEN
        VOL(4) = CFGRS
C       added calc tip volume. YW 20130111        
        VOL(15) = CF0-CFGRS
      ENDIF
      IF(CDPFLG .EQ. 1) VOL(6)=CORDS
      IF(SPFLG.EQ.1 .AND. CUPFLG.EQ.1)THEN
        VOL(7)=CF0-CFGRS
C       the tip volume included in secondary prod volume (vol(7))
C       so set tip vol to 0. YW 20130111
        VOL(15)=0.0
      ENDIF
      IF(SPFLG.EQ.1 .AND. CDPFLG.EQ.1) VOL(9)=CF0/90-CORDS

c calculate stump volume as 1 foot cylinder of STUMPD. YW 20130111
c      VOL(14)=STUMPD**2*0.005454154
C     IF(CUPFLG.EQ.1.OR.BFPFLG.EQ.1.OR.CDPFLG.EQ.1)NOLOGP = MERLEN/16.5
      IF(CUPFLG.EQ.1.OR.BFPFLG.EQ.1.OR.CDPFLG.EQ.1)NOLOGP = TOTLGS
      IF(SPFLG.EQ.1) NOLOGS = (THT-MERLEN)/16.5
      
	LOGDIA(1,1) = DLG(1)
	LOGDIA(1,2) = DIBLG(1)
	BOLHT(1) = 1.0
	DO 100, I=1,NUMLGS
         LOGDIA(I+1,1) = DSM(I)
         LOGDIA(I+1,2) = DIBSM(I)
	   IF(I.EQ.NUMLGS)THEN
	      LOGLEN(I) = TOPLEN - TRM
	   ELSE
            LOGLEN(I) = 16.0      
	   ENDIF
C        Get BOLHT
         BOLHT(I+1) = BOLHT(I) + LOGLEN(I) + 0.5
         
  100 CONTINUE
C     YW 03/20/2017
C     The Topwood volume VOL(7) or the Tip volume VOL(15) calculated above is 
C     not accurate. It overestimates the volume. I is recalculated below:
      IF(HTTOT.GT.0)THEN
        TOPVOL = LOGDIA(NUMLGS+1,2)**2*(HTTOT-BOLHT(NUMLGS+1))*0.002727
        IF(SPFLG.EQ.1 .AND. CUPFLG.EQ.1)THEN
          VOL(7)=TOPVOL
          VOL(15)=0.0
        ELSEIF(CUPFLG.EQ.1)THEN
          VOL(7)=0.0
          VOL(15)=TOPVOL        
        ENDIF
      ENDIF
      RETURN
      END
C THIS ROUTINE CALCULATES GROSS BF, GROSS CF, AND CORDWOOD TREE VOLUMES
C TO SEARCH FOR CHANGES IN THE SED EDITOR USE   FIND !
C
C VARIABLE DESCRIPTIONS:
C BFGRS = GROSS MAINSTEM BOARD FT VOLUME FOR A TREE
C B = COEFFICIENT USED IN TAPER EQUATION
C BF = ABBREVIATION FOR BOARD FOOT VOLUME
C BUTTCF = DIB AT 4 FT ABOVE STUMP
C CF = ABBREVIATION FOR CUBIC FOOT VOLUME
C CF0 = CUBIC FOOT OF ENTIRE TREE ABOVE THE STUMP
C CFGRS = GROSS MAIN STEM CUBIC FT VOLUME FOR A TREE
C CFCOEF = COEFFICIENTS FOR PREDICTING CF0
C CORDS = MAINSTEM CORDWOOD-CONVERTED FROM CFGRS AT 90 CF/CORD
C D67 = DIAMETER INSIDE BARK AT HT67
C DBHOB = DOB AT 4.5 FT ABOVE GROUND (PASSED FROM VOLUME)
C DLG(NUM) = DIB AT LARGE END OF LOG)
C DOB = ABBREVIATION FOR DIAMETER OUTSIDE BARK IN INCHES
C DSM(NUM) = DIB AT SMALL END OF LOG
C DIB = ABBREVIATION FOR DIAMETER INSIDE BARK IN INCHES
C VOLEQ = VOLUME EQUATION NUMBER
C F = CYLINDRICAL FORM FACTOR
C HTTOT = TOTAL TREE HEIGHT (FROM GROUND TO TIP)
C HT67 = HEIGHT TO 2/3 DBHOB OUTSIDE BARK
C II = INDEX NUMBER FOR CFCOEF
C L = COUNTER
C LEN = NOMINAL LENGTH OF LOG (NEAREST 2 FOOT W/O TRIM)
C M = UNIT OF MEASURE (M = 1 FOR BF; M = 2 FOR CORDWOOD; M=3 FOR CF)
C MERLEN = MERCHANTABLE LENGTH OF TREE BY UNIT OF MEASURE - IN FEET
C NOLOGP=MAINSTEM NO. OF  16 FT. LOGS 
C NOLOGS=TOPWOOD NO. OF LOGS 
C NUM = LOG COUNTER
C NUMLGS = TOTAL LOGS IN TREE
C SCRIBC = FACTORS FOR BF LOG VOLUMES
C STUMPD = STUMP DIB
C VOLEQ = TAPER EQUATION NUMBER
C MTOPP= MERCHANTABLE TOP DIB
C TOPLEN = LENGTH OF TOP LOG IN FEET
C TOTLGS = TOTAL LOGS IN TREE TO MERCHANTABLE TOP DIB BY UNIT OF MEAS
C TRM = TRIM ALLOWANCE ARRAY PER LOG BY UNIT OF MEASURE (IN FEET)
C UNIT OF MEASURE: 1=BF  2=CORDWOOD 3=CF
C VOLR4(M) = TEMPORARY STORAGE FOR BFGRS,CORDS,CFGRS
C VOL(M) = VOLUME ARRAY FOR NAT CRUISE PGM (PASSED BACK TO VOLUME)
C          VOL(1) = GROSS CF ENTIRE TREE    VOL(6) CORDS MAINSTEM
C          VOL(2) = GROSS BF MAINSTEM       VOL(7) GROSS CF TOPWOOD
C          VOL(3) = NET BF MAINSTEM         VOL(8) NET CF TOPWOOD
C          VOL(4) = GROSS CF MAINSTEM       VOL(9) CORDS TOPWOOD
C          VOL(5) = NET CF MAINSTEM
C ************************************************************************
      SUBROUTINE R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,
     + HTUP,DIB,ERRFLAG )
C This subroutine calculate Diameter inside bark and any given height (HTUP).
C It also calculate the height above ground and any given DIB.
      IMPLICIT NONE
C**********************************************************************
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,STUMPD,BUTTCF,B,HTUP,DIB,THT
      REAL CFCOEF(20,7)
      INTEGER II,ERRFLAG
      REAL HT67,D67,CF0,F,PHT
C
C  COEFFICIENTS TO DETERMINE HEIGHT TO 2/3 DBHOB (OUTSIDE BARK),
C  STUMP DIAMETER,  AND DIAMETER (INSIDE BARK) AT 2/3 TOTAL HTTOT.
C
      DATA CFCOEF/0.5563,0.4496,0.2359,0.8435,1.1152,0.5405,1.2552,
     + 0.4302,0.4727,2.0461,0.6294,1.2467,0.5334,0.5301,0.8315,
     + 0.5028,0.7572,0.6310,0.4850,0.6076,-0.0636,-0.3956,
     + -0.7237,-0.2800,-0.3678,-0.5908,-0.1727,-0.5229,-0.0508,-0.5080,
     + -0.0939,-0.1615,-0.0071,0.2327,0.0967,-0.0942,-0.4527,-0.1026,
     + -0.3636,-0.1730,0.9900,1.2756,1.6343,1.0264,0.9828,
     + 1.3374,0.9435,1.3824,1.0615,0.9113,0.9886,0.9013,1.0127,0.8748,
     + 0.8293,1.0600,1.1502,1.0230,1.2324,1.0585,
     + -0.2,-0.041,-0.041,-0.041,-0.191,-0.191,-0.131,-0.131, 
     + -0.159,-0.041,-0.488,-0.200,-0.200,-0.365,-0.365,-0.365,-0.143,
     + -0.143,0.000,-0.153,0.964,0.884,0.884,0.884,0.943,0.943,
     + 0.886,0.886,0.832,0.884,0.894,0.964,0.964,0.887,0.887,0.887,
     + 0.933,0.933,0.887,0.883,
     + -0.201,-0.041,-0.041,-0.041,-0.192,-0.192,-0.159,
     + -0.159,-0.055,-0.041,-0.445,-0.201,-0.201,-0.367,-0.367,-0.367,
     + -0.144,-0.144,0.000,-0.143,0.968,0.888,0.888,0.888,
     + 0.947,0.947,0.891,0.891,0.837,0.888,0.897,0.968,0.968,
     + 0.891,0.891,0.891,0.937,0.937,0.893,0.886/

      ERRFLAG = 0
      IF(DBHOB .LT. 1.0)THEN
        ERRFLAG = 3
        RETURN
      ENDIF
      IF(HTTOT .LE. 4.5) THEN
        ERRFLAG = 4
        RETURN
      ENDIF
      
C*********************************************************
C  DETERMINE INDEX 'II' FOR HT67, STUMPD, AND DIB67
C  BASED ON TAPER EQUATION NUMBER (VOLEQ)
C
      IF(VOLEQ(8:10).EQ.'746') THEN
        II=1
      ELSEIF(VOLEQ(8:10).EQ.'202'.AND.VOLEQ(1:3).EQ.'400') THEN
        II=2
      ELSEIF(VOLEQ(8:10).EQ.'202'.AND.VOLEQ(1:3).EQ.'405') THEN
        II=3
      ELSEIF(VOLEQ(8:10).EQ.'202'.AND.VOLEQ(1:3).EQ.'401') THEN
        II=4
      ELSEIF(VOLEQ(8:10).EQ.'019'.AND.VOLEQ(1:3).EQ.'400') THEN
        II=5
      ELSEIF(VOLEQ(8:10).EQ.'019'.AND.VOLEQ(1:3).EQ.'405') THEN
        II=6
      ELSEIF(VOLEQ(8:10).EQ.'015'.AND.VOLEQ(1:3).EQ.'400') THEN
        II=7
      ELSEIF(VOLEQ(8:10).EQ.'015'.AND.VOLEQ(1:3).EQ.'401') THEN
        II=8
      ELSEIF(VOLEQ(8:10).EQ.'081') THEN
        II=9
      ELSEIF(VOLEQ(8:10).EQ.'073') THEN
        II=10
      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'403') THEN
        II=11
      ELSEIF(VOLEQ(8:10).EQ.'108'.AND.VOLEQ(1:3).EQ.'400') THEN
        II=12
      ELSEIF(VOLEQ(8:10).EQ.'108'.AND.VOLEQ(1:3).EQ.'401') THEN
        II=13
      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'401') THEN
        II=14
      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'402') THEN
        II=15
      ELSEIF(VOLEQ(8:10).EQ.'122'.AND.VOLEQ(1:3).EQ.'400') THEN
        II=16
      ELSEIF(VOLEQ(8:10).EQ.'093'.AND.VOLEQ(1:3).EQ.'400') THEN
        II=17
      ELSEIF(VOLEQ(8:10).EQ.'093'.AND.VOLEQ(1:3).EQ.'407') THEN
        II=18
      ELSEIF(VOLEQ(8:10).EQ.'020') THEN
        II=19
      ELSEIF(VOLEQ(8:10).EQ.'117') THEN
        II=20
      ELSE
        ERRFLAG = 1
        RETURN
      ENDIF
      
c  sutract one foot from height for mathis equations
      THT = HTTOT-1.0
      HT67=CFCOEF(II,1) * DBHOB**CFCOEF(II,2) * THT**CFCOEF(II,3)
      BUTTCF= CFCOEF(II,5) * DBHOB + CFCOEF(II,4)
      STUMPD=(BUTTCF**2*THT/(THT-4.))**.5
      D67 = CFCOEF(II,7) * DBHOB  * (2./3.) + CFCOEF(II,6)
      CF0 = .002727 * (HT67 * STUMPD**2 + D67**2 * THT)
      F = CF0/(.005454 * STUMPD**2 * THT)
      B = (1.0-F) / (2.0*F)
      
C Calculate DIB at a given height above ground
      IF(HTUP.GT.0.AND.HTUP.LT.HTTOT)THEN
        IF(HTUP.LE.1.0)THEN
          DIB = STUMPD
        ELSE
          PHT = HTUP - 1.0
          DIB = STUMPD*((THT-PHT)/THT)**B
        ENDIF
C calculate Height to a given DIB
      ELSEIF(DIB.GT.0.AND.DIB.LT.STUMPD)THEN
        PHT = THT-THT*(DIB/STUMPD)**(1/B)
        HTUP = PHT + 1.0
      ENDIF      
      RETURN
      END