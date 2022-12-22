!== last modified  12-17-2008
      SUBROUTINE R9VOL(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,FORST,SI,BA,
     > PROD,CTYPE,BFPFLG,CUPFLG,CDPFLG,SPFLG,HTTYPE,ERRFLAG,MTOPP)

C CREATED  : 01-30-90
C YW 07/13/2018 added MTOPP variable to the routine
C PURPOSE  : THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C            USING REGION 9 VOLUME DETERMINATION ROUTINES.  
C            Gevorkiantz Model
C            ADDED A FUNCTION TO FIND MERCH HTS GIVEN TOTAL HEIGHT

C*********************************
C       DECLARE VARIBLES         *
C*********************************
      CHARACTER*10 VOLEQ
      CHARACTER*1 CTYPE,HTTYPE
      CHARACTER*2 FORST,PROD,TMPSTR
      CHARACTER*3 VOLSPP
      INTEGER BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG,I,SI,BA,IFORST

      REAL VOL(15),DBHOB,R,VC,CF,HT1PRD,HT2PRD,HTTOT,MTOPP
      REAL GCB,P,PT,CRD
      real*8 term1,term2,term3
      
      DO 100, I=1,15
        VOL(I) = 0.0
 100  CONTINUE
      ERRFLAG = 0
      IF(DBHOB.LT.1.0)THEN
        ERRFLAG = 3
        RETURN
      ENDIF
!     ADDED CALC VOLUME FOR ENTIRE STEM INCLUDE STUMP AND TIP (YW 2018/09/11)
      IF(HTTOT.GT.0.0) THEN
        VOL(1) = 0.42*3.141592*(1.0/144.0)*(1.0/4.0)*DBHOB*DBHOB*HTTOT
!       SET A SMALL NUMBER FOR STUMP TO AVOID RECALC        
        VOL(14) = 0.001
      ENDIF
      IF(FORST(2:2) .LT. '0') THEN
	   FORST(2:2) = FORST(1:1)
	   FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF

      READ(FORST,'(i2)') IFORST
      IF(VOLEQ(10:10).LT.'0' .OR. VOLEQ(10:10).GT.'9') THEN
        TMPSTR=VOLEQ(8:9)
        VOLEQ(8:10)='0'//TMPSTR
      ENDIF
      VOLSPP = VOLEQ(8:10)

C*    CHECK FOR VALID HEIGHTS
      IF(HT1PRD.LE.0 .AND. HT2PRD.LE.0 .AND. HTTOT.LE.0)THEN
         IF(PROD.EQ.'01')THEN
            ERRFLAG = 7
         ELSE
            ERRFLAG = 8
         ENDIF
         RETURN    
      ENDIF

C     MAKE SURE MERCH HEIGHTS ARE IN LOGS
      IF(CTYPE.EQ.'F')THEN
	   IF(HT1PRD.LE.0 .AND. HT2PRD.LE.0)THEN
            CALL R9_MHTS(IFORST,VOLEQ,DBHOB,HTTOT,SI,BA,HT1PRD,HT2PRD,
     >                    PROD,ERRFLAG,MTOPP)
	   ELSE IF (HTTYPE.NE.'L' .AND. HTTYPE.NE.'l')THEN
            HT1PRD=INT((HT1PRD+3.0)/8.25)
            HT2PRD=INT((HT2PRD+3.0)/8.25)
	   ENDIF
      ELSE
        HT1PRD=INT((HT1PRD+3.0)/8.25)
        IF(HT2PRD.GT.0) THEN
          HT2PRD=INT((HT2PRD+3.0)/8.25)
        ELSEIF(HTTOT.GT.0) THEN
          HT2PRD=INT((HTTOT-12)/8.25)
        ENDIF
      ENDIF

C**********************************************************
C               BOARD FOOT MAIN STEM EQUATIONS            *
C**********************************************************
C*  SET CORRECTION FACTOR **
      CF=1.0

C*************----------------- TABLE A
      IF(BFPFLG.EQ.1.AND.HT1PRD.GT.0)THEN
C*  CALCULATE R AND VOLUME OF A TRUNCATED CONE **
         R=((HT1PRD*DBHOB-3.75)/(24.0*HT1PRD-10.5))
         VC=HT1PRD*(1.0757+3.002*R+8.3776*R**2)
         
         IF (VOLEQ(1:3).EQ.'901'.OR.IFORST.EQ.2.OR.IFORST.EQ.3
     >         .OR.IFORST.EQ.6 .OR.IFORST.EQ.7.OR.IFORST.EQ.9
     >         .OR.IFORST.EQ.10 .OR.IFORST.EQ.13)THEN
            IF (VOLSPP.EQ.'094')THEN
               CF=0.90
            ELSEIF (VOLSPP.EQ.'105') THEN
               CF=0.92
            ELSEIF (VOLSPP.EQ.'125')THEN
              IF (IFORST.EQ.3) THEN
                CF=0.91
              ELSE
                CF=0.96
              ENDIF
            ELSEIF (VOLSPP.EQ.'129') THEN
              IF (IFORST.EQ.3) THEN
                CF=0.90
              ELSE
                CF=0.96
              ENDIF
            ELSEIF (VOLSPP.EQ.'241') THEN
               CF=0.80
            ELSEIF (VOLSPP.EQ.'261') THEN
               CF=0.95
            ELSEIF (VOLSPP.EQ.'318') THEN
               CF=0.98
            ELSEIF (VOLSPP.EQ.'375') THEN
               CF=0.93
            ELSEIF (VOLSPP.EQ.'531') THEN
               CF=1.15
            ELSEIF (VOLSPP.EQ.'541') THEN
               CF=1.03
            ELSEIF (VOLSPP.EQ.'951') THEN
               CF=0.97
            ELSEIF (VOLSPP.EQ.'970') THEN
               CF=1.05
            ENDIF
            VOL(2)=5.527702-(4.22*VC)-(2.4082*DBHOB)+(5.4072*HT1PRD)
            VOL(2)=VOL(2)+(.2114*(DBHOB**2*HT1PRD))-(.48097*HT1PRD**2)-
     >              (.00605*VC**2)+(.291865*DBHOB**2)+
     >              (.00501*(DBHOB**2*HT1PRD**2))-
     >              (.00039606323*(DBHOB**2*HT1PRD**3))+
     >              (.0000013109952*(DBHOB**4*HT1PRD**2))
            VOL(2)=VOL(2)*CF
C*************----------------- TABLE B
         ELSEIF (VOLEQ(1:3).EQ.'902'.OR.IFORST.EQ.4 .OR.IFORST.EQ.5
     >            .OR.IFORST.EQ.8 .OR.IFORST.EQ.11 .OR.IFORST.EQ.12
     >            .OR.IFORST.EQ.14 .OR.IFORST.EQ.24) THEN
            IF (VOLSPP.EQ.'400') THEN
               CF=1.06
            ELSEIF (VOLSPP.EQ.'602') THEN
               CF=0.90
            ELSEIF (VOLSPP.EQ.'621') THEN
               CF=1.10
            ELSEIF (VOLSPP.EQ.'694') THEN
               CF=1.15
            ELSEIF (VOLSPP.EQ.'731') THEN
               CF=0.93
            ELSEIF (VOLSPP.EQ.'742') THEN
               CF=0.97
            ELSEIF (VOLSPP.EQ.'824') THEN
               CF=0.80
            ELSEIF (VOLSPP.EQ.'830') THEN
               CF=0.96
            ELSEIF (VOLSPP.EQ.'832') THEN
               CF=1.03
            ELSEIF (VOLSPP.EQ.'068'.AND.IFORST.NE.4) THEN
               CF=0.80
            ELSEIF (VOLSPP.EQ.'110'.AND.
     >                   (IFORST.EQ.8.OR.IFORST.EQ.5)) THEN
               CF=0.95
            ELSEIF (VOLSPP.EQ.'125'.AND.IFORST.EQ.4) THEN
               CF=0.96
            ELSEIF (VOLSPP.EQ.'129') THEN
               IF (IFORST.EQ.11 .or. IFORST.EQ.14 .or. 
     >                                IFORST.EQ.12)    CF=0.95
               IF (IFORST.EQ.4) CF=0.96
            ELSEIF (VOLSPP.EQ.'241'.AND.(IFORST.EQ.4)) THEN
               CF=0.80
            ELSEIF (VOLSPP.EQ.'802') THEN
               IF (IFORST.EQ.8) CF=1.08
               IF (IFORST.EQ.5) CF=0.96
            ELSEIF (VOLSPP.EQ.'806') THEN
               IF (IFORST.EQ.5) THEN
                  CF=1.03
               ELSE
                  CF=1.10
               ENDIF
            ELSEIF (VOLSPP.EQ.'833') THEN
               IF (IFORST.EQ.8) THEN
                  CF=1.11
               ELSE
                  CF=1.06
               ENDIF
            ELSEIF (VOLSPP.EQ.'835') THEN
               IF (IFORST.EQ.5) THEN
                  CF=0.94
               ELSE
                  CF=0.98
               ENDIF
            ELSEIF (VOLSPP.EQ.'837') THEN
              IF (IFORST.EQ.8) CF=1.05
              IF (IFORST.EQ.5) CF=0.96
              IF (IFORST.EQ.4) CF=0.95
            ENDIF

            VOL(2)=(-.092685)-(5.98*VC)-(2.9715*DBHOB)+(16.7022*HT1PRD)
            VOL(2)=VOL(2)+(.2471*(DBHOB**2*HT1PRD))-(.91751*HT1PRD**2)-
     >              (.00876*VC**2)+(.351046*DBHOB**2)+
     >              (.00451*(DBHOB**2*HT1PRD**2))-
     >              (.00030183475*(DBHOB**2*HT1PRD**3))+
     >              (.0000019222413*(DBHOB**4*HT1PRD**2))
            VOL(2)=VOL(2)*CF
            VOL(10) = VOL(2)
C*************----------------- TABLE C
         ELSEIF (VOLEQ(1:3).EQ.'903'.OR.IFORST.EQ.19) THEN
            IF(VOLSPP.EQ.'129') THEN
               CF = 1.06
            ELSEIF(VOLSPP.EQ.'261') THEN
               CF=0.88
            ELSEIF(VOLSPP.EQ.'531') THEN
               CF=1.09
            ELSEIF(VOLSPP.EQ.'541'.OR.VOLSPP.EQ.'543'
     &         .OR.VOLSPP.EQ.'621'.OR.VOLSPP.EQ.'951') THEN
               CF=0.91
            ELSEIF(VOLSPP.EQ.'806'.OR.VOLSPP.EQ.'813'
     &         .OR.VOLSPP.EQ.'823'.OR.VOLSPP.EQ.'824'
     &         .OR.VOLSPP.EQ.'830'.OR.VOLSPP.EQ.'833'
     &         .OR.VOLSPP.EQ.'835'.OR.VOLSPP.EQ.'837') THEN
               CF=0.97
            ENDIF

            VOL(2)=12.900801+(4.48*VC)+(1.2218*DBHOB)-(18.267*HT1PRD)
            VOL(2)=VOL(2)+(.1993*(DBHOB**2*HT1PRD))-(.37290*HT1PRD**2)+
     >              (.02714*VC**2)-(.15071*DBHOB**2)-
     >              (.01703*(DBHOB**2*HT1PRD**2))+
     >              (.00039033000*(DBHOB**2*HT1PRD**3))-
     >              (.0000066236*(DBHOB**4*HT1PRD**2))
            VOL(2)=VOL(2)*CF
            VOL(10) = VOL(2)
C*************----------------- TABLE D
         ELSEIF (VOLEQ(1:3).EQ.'904'.OR.IFORST.EQ.20.OR.IFORST.EQ.22
     >            .OR.IFORST.EQ.30) THEN
            IF(VOLSPP.EQ.'012'.OR.VOLSPP.EQ.'261') THEN
               CF=0.86
            ELSEIF(VOLSPP.EQ.'094'.OR.VOLSPP.EQ.'097'
     >         .OR.VOLSPP.EQ.'125'.OR.VOLSPP.EQ.'129') THEN
               CF=0.97
            ELSEIF(VOLSPP.EQ.'316'.OR.VOLSPP.EQ.'746'
     >         .OR.VOLSPP.EQ.'762'.OR.VOLSPP.EQ.'833'
     >         .OR.VOLSPP.EQ.'951'.OR.VOLSPP.EQ.'970') THEN
               CF=0.94
            ELSEIF(VOLSPP.EQ.'375'.OR.VOLSPP.EQ.'541'
     >         .OR.VOLSPP.EQ.'376') THEN
               CF=0.92
            ELSEIF(VOLSPP.EQ.'371') THEN
               IF(IFORST.EQ.20) THEN
                  CF=0.92
               ELSEIF(IFORST.EQ.22) THEN
                  CF=0.97
               ENDIF
            ENDIF
            VOL(2)=(-3.592279)-(2.74*VC)-(.6274*DBHOB)+(1.5333*HT1PRD)
            VOL(2)=VOL(2)+(.2697*(DBHOB**2*HT1PRD))+(.10400*HT1PRD**2)+
     >              (.00009*VC**2)+(.14129*DBHOB**2)-
     >              (.01104*(DBHOB**2*HT1PRD**2))+
     >              (.00029155*(DBHOB**2*HT1PRD**3))-
     >              (.00000007493*(DBHOB**4*HT1PRD**2))
            VOL(2)=VOL(2)*CF
            VOL(10) = VOL(2)
C*************----------------- TABLE E
         ELSEIF (VOLEQ(1:3).EQ.'905'.OR.
     >               (IFORST.EQ.21 .AND. VOLSPP.GE.'300')) THEN
            IF(VOLSPP.EQ.'316'.OR.VOLSPP.EQ.'371'.OR.VOLSPP.EQ.'802'
     >                        .OR.VOLSPP.EQ.'951') THEN
               CF=1.03
            ELSEIF(VOLSPP.EQ.'318') THEN
               CF=1.09
            ELSEIF(VOLSPP.EQ.'373') THEN
               CF=0.94
            ELSEIF(VOLSPP.EQ.'531') THEN
               CF=1.12
            ELSEIF(VOLSPP.EQ.'541') THEN
               CF=0.97
            ELSEIF(VOLSPP.EQ.'621'.OR.VOLSPP.EQ.'651'
     >         .OR.VOLSPP.EQ.'261') THEN
               CF=1.06
            ELSEIF(VOLSPP.EQ.'833') THEN
               CF=0.91
            ENDIF
            VOL(2)=16.775055+(4.06*VC)+(.8709*DBHOB)-(18.343*HT1PRD)
            VOL(2)=VOL(2)+(.1940*(DBHOB**2*HT1PRD))-(.19833*HT1PRD**2)+
     >              (.02401*VC**2)-(.13224*DBHOB**2)-
     >              (.0163*(DBHOB**2*HT1PRD**2))+
     >              (.00037154*(DBHOB**2*HT1PRD**3))-
     >              (.0000057358*(DBHOB**4*HT1PRD**2))
            VOL(2)=VOL(2)*CF
            VOL(10) = VOL(2)
C*************----------------- TABLE F
         ELSEIF (VOLEQ(1:3).EQ.'906'.OR.
     >               (IFORST.EQ.21.AND.VOLSPP.LT.'300')) THEN
            IF(VOLSPP.EQ.'097') THEN
               CF=1.15
            ELSEIF(VOLSPP.EQ.'129') THEN
               CF=0.94
            ENDIF
            VOL(2)=14.522237-(9.40*VC)-(.415*DBHOB)+(15.639*HT1PRD)
            VOL(2)=VOL(2)+(.3655*(DBHOB**2*HT1PRD))+(.00563*HT1PRD**2)-
     >              (.01959*VC**2)+(.3088*DBHOB**2)-
     >              (.00844*(DBHOB**2*HT1PRD**2))+
     >              (.00030875*(DBHOB**2*HT1PRD**3))+
     >              (.0000055105*(DBHOB**4*HT1PRD**2))
            VOL(2)=VOL(2)*CF
            VOL(10) = VOL(2)
         ENDIF
      ENDIF
C**********************************************************
C           CUBIC FOOT MAIN STEM EQUATIONS                *
C**********************************************************
C*************** CUBIC VOLUME FOR AN ALL PULPWOOD TREE
      IF(CUPFLG.EQ.1) THEN
         IF ((VOLEQ(1:3).EQ.'911'.OR.PROD.NE.'01').AND.HT2PRD.GT.0) THEN

            VOL(4)=(0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-
     >                (0.009984*HT2PRD**2)+(0.04/HT2PRD))*79

C*************** CUBIC VOLUME FOR A SAWTIMBER TREE

         ELSEIF ((VOLEQ(1:3).EQ.'912'.OR.PROD.EQ.'01').AND.
     >                                           HT1PRD.GT.0) THEN
            IF (VOLSPP.EQ.'071'.OR.VOLSPP.EQ.'094'.OR.
     >            VOLSPP.EQ.'095'.OR.VOLSPP.EQ.'097'.OR.
     >            VOLSPP.EQ.'105'.OR.VOLSPP.EQ.'241'.OR.
     >            VOLSPP.EQ.'460'.OR.VOLSPP.EQ.'543'.OR.
     >            VOLSPP.EQ.'601'.OR.VOLSPP.EQ.'602'.OR.
     >            VOLSPP.EQ.'731'.OR.VOLSPP.EQ.'742'.OR.
     >            VOLSPP.EQ.'823'.OR.VOLSPP.EQ.'824')THEN
               CF=0.95
            ELSEIF (VOLSPP.EQ.'400'.OR.VOLSPP.EQ.'404'.OR.
     >              VOLSPP.EQ.'651'.OR.VOLSPP.EQ.'694'.OR.
     >              VOLSPP.EQ.'813'.OR.VOLSPP.EQ.'830') THEN
               CF=1.05
            ELSEIF (VOLSPP.EQ.'531') THEN
               CF=1.10
            ELSEIF (VOLSPP.EQ.'920') THEN
               CF=0.90
            ELSEIF (VOLSPP.EQ.'970') THEN
               CF=1.08
            ELSEIF (DBHOB.LT.13.0.AND.VOLSPP.EQ.'110') THEN
               CF=1.06
            ELSEIF (DBHOB.LT.15.0) THEN
               IF (VOLSPP.EQ.'621'.OR.VOLSPP.EQ.'746') THEN
                  CF=1.03
               ELSEIF (VOLSPP.EQ.'125') THEN
                  CF=1.04
               ELSEIF (VOLSPP.EQ.'837') THEN
                  CF=1.05
               ELSEIF (VOLSPP.EQ.'835'.OR.VOLSPP.EQ.'951') THEN
                  CF=1.06
               ELSEIF (VOLSPP.EQ.'371'.OR.VOLSPP.EQ.'833') THEN
                  CF=1.08
               ELSEIF (VOLSPP.EQ.'129'.OR.VOLSPP.EQ.'318'.OR.
     >                                    VOLSPP.EQ.'802') THEN
                  CF=1.10
               ELSEIF (VOLSPP.EQ.'806') THEN
                  CF=1.11
               ELSEIF (VOLSPP.EQ.'375') THEN
                  CF=1.12
               ELSEIF (VOLSPP.EQ.'762') THEN
                  CF=1.16
               ELSEIF (VOLSPP.EQ.'316') THEN
                  CF=1.18
	         ENDIF
            ELSE
               IF (VOLSPP.EQ.'746') THEN
                  CF=0.95
               ELSEIF (VOLSPP.EQ.'129') THEN
                  CF=0.96
               ELSEIF (VOLSPP.EQ.'835') THEN
                  CF=1.01
               ELSEIF (VOLSPP.EQ.'371') THEN
                  CF=1.03
               ELSEIF (VOLSPP.EQ.'318'.OR.VOLSPP.EQ.'375'.OR.
     >             VOLSPP.EQ.'951') THEN
                  CF=1.04
               ELSEIF (VOLSPP.EQ.'833') THEN
                  CF=1.05
               ELSEIF (VOLSPP.EQ.'837') THEN
                  CF=1.06
               ELSEIF (VOLSPP.EQ.'802') THEN
                  CF=1.07
               ELSEIF (VOLSPP.EQ.'621') THEN
                  CF=1.08
               ELSEIF (VOLSPP.EQ.'762'.OR.VOLSPP.EQ.'806') THEN
                  CF=1.09
               ELSEIF (VOLSPP.EQ.'316') THEN
                  CF=1.12
               ENDIF
            ENDIF

            term1=-1.70774+(0.051321*DBHOB)+(0.58857*HT1PRD)+
     >               (0.0193547*DBHOB**2)+(0.0237324*HT1PRD*DBHOB**2)
            term2=-(0.04821*HT1PRD**2)-(0.0002174*DBHOB**2*HT1PRD**2)-
     >              (0.0000239*DBHOB**2*HT1PRD**3)+(0.00000795*DBHOB**3*
     >                HT1PRD**2)
            term3=-(0.00000057*DBHOB**3*HT1PRD**3)-
     >              (0.000000035*DBHOB**4*HT1PRD**2)
            vol(4)=(term1+term2+term3)*cf
         ENDIF
      ENDIF
C**********************************************************
C        CORD WOOD VOLUME FOR AN ALL PULPWOOD TREE        *
C**********************************************************
      IF(CDPFLG.EQ.1.AND.HT2PRD.GT.0.AND.
     >                    (PROD.NE.'01'.OR.VOLEQ(1:3).EQ.'921')) THEN
         VOL(6)=(0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-
     >              (0.009984*HT2PRD**2)+(0.04/HT2PRD))
      ENDIF

C**********************************************************
C            CUBIC FOOT TOP WOOD EQUATIONS                *
C**********************************************************
      IF(SPFLG.EQ.1.AND.HT2PRD.GT.0.AND.PROD.EQ.'01')THEN
         GCB=(0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-
     >            (0.009984*HT2PRD**2)+(0.04/HT2PRD))*79
         IF(HT1PRD.GT.0) THEN
            P=(HT1PRD)/(HT2PRD)*100
            PT=(98.461-(1.394*P)+(0.004*P**2))*0.01
            VOL(7)=PT*GCB
         ELSE
            VOL(7) = GCB
         ENDIF
         IF (VOL(7).LT.0.0) VOL(7)=0.0
      ENDIF
C**********************************************************
C                 CORD TOP WOOD EQUATIONS                 *
C**********************************************************
      IF(SPFLG.EQ.1.AND.HT2PRD.GT.0.AND.PROD.EQ.'01')THEN
         CRD=(0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-
     >            (0.009984*HT2PRD**2)+(0.04/HT2PRD))
         IF(HT1PRD.GT.0) THEN
            P=(HT1PRD)/(HT2PRD)*100
            PT=(98.461-(1.394*P)+(0.004*P**2))*0.01
            VOL(9)=PT*CRD
         ELSE
            VOL(9)=CRD
         ENDIF
         IF (VOL(9).LT.0.0) VOL(9)=0.0
      ENDIF
      
  999 RETURN
      END
!-------------------------------------------------------------------------
      SUBROUTINE HAHN_NC250(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,SI,BA,
     > PROD,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG,MTOPP,MTOPS)
!     Created: YW  2018/07/31   
!     Hahn 1981 Tree volume and biomass equations for the Lake States  
!     USDA Forest Service North Central Forest Experiment Station Research Paper NC-250
!     NVEL Equation Number: 900DVEE***
      CHARACTER*10 VOLEQ
      REAL HTTOT,HT1PRD,HT2PRD,DBHOB,VOL(15),MTOPP,MTOPS
      CHARACTER*2 PROD
      INTEGER BFPFLG,CUPFLG,SPFLG,ERRFLAG,SI,BA
      CHARACTER*1 CTYPE
      
      INTEGER I,J,VOLSP(49),SPN,CNT,IDX,BFMIND
      REAL HTCOEF(49,7),VOLCOEF(49,6)
      REAL B1,B2,B3,B4,B5,B6,C0,C1,D0,D1,D,H,S,TOP,B,S0
      REAL THTEST,HT1EST,HT2EST,RATIO,VOL4
      DATA VOLSP/
     & 12,68,71,94,95,105,125,129,132,241,
     &261,299,313,314,316,317,318,371,373,375,
     &400,460,531,541,543,544,601,602,611,621,
     &694,731,741,742,743,746,762,802,806,812,
     &830,833,834,837,920,951,970,998,999/
!     Coefficients of height equations     
      DATA ((HTCOEF(J,I), I=1,7), J=1,49) /
     & 12,14.304,0.19894,1.4195,0.23349,0.76878,0.12399,
     & 68,8.2079,0.19672,1.3112,0.33978,0.76173,0.11666,
     & 71,13.62,0.24255,1.2885,0.25831,0.68128,0.10771,
     & 94,31.957,0.18511,1.702,0,0.68967,0.162,
     & 95,20.038,0.18981,1.2909,0.17836,0.57343,0.10159,
     & 105,16.934,0.12972,1,0.20854,0.77792,0.12902,
     & 125,36.851,0.08298,1,0.00001,0.63884,0.18231,
     & 129,16.281,0.08621,1,0.1622,0.86833,0.23316,
     & 132,36.851,0.08298,1,0.00001,0.63884,0.18231,
     & 241,8.2079,0.19672,1.3112,0.33978,0.76173,0.11666,
     & 261,5.3117,0.10357,1,0.68454,0.7141,0,
     & 299,16.934,0.12972,1,0.20854,0.77792,0.12902,
     & 313,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,
     & 314,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 316,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,
     & 317,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,
     & 318,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 371,7.1852,0.28384,1.4417,0.38884,0.82157,0.11411,
     & 373,7.2773,0.22721,1,0.41179,0.76498,0.11046,
     & 375,7.2773,0.22721,1,0.41179,0.76498,0.11046,
     & 400,6.1034,0.17368,1,0.44725,1.0237,0.1461,
     & 460,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,
     & 531,7.1852,0.28384,1.4417,0.38884,0.82157,0.11411,
     & 541,8.1782,0.27316,1.725,0.38694,0.75822,0.10847,
     & 543,11.291,0.2525,1.5466,0.35711,0.7506,0.06859,
     & 544,8.1782,0.27316,1.725,0.38694,0.75822,0.10847,
     & 601,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 602,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 611,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 621,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 694,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 731,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 741,6.4301,0.23545,1.338,0.4737,0.73385,0.08228,
     & 742,13.625,0.28668,1.6124,0.30651,1.0292,0.0746,
     & 743,5.5346,0.22637,1,0.46918,0.72456,0.11782,
     & 746,6.4301,0.23545,1.338,0.4737,0.73385,0.08228,
     & 762,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 802,9.2078,0.22208,1,0.31723,0.8256,0.13465,
     & 806,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,
     & 812,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,
     & 830,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,
     & 833,6.6844,0.19049,1,0.43972,0.82962,0.10806,
     & 834,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,
     & 837,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,
     & 920,13.625,0.28668,1.6124,0.30651,1.0292,0.0746,
     & 951,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 970,8.458,0.27527,1.9602,0.34894,0.89213,0.12594,
     & 998,6.9572,0.26564,1,0.4866,0.76954,0.01618,
     & 999,6.9572,0.26564,1,0.4866,0.76954,0.01618/
!     Cubic foot volume regression coefficients (COLUMN 2 & 3) AND   
!     Board foot volume regression coefficients (COLUMN 4 & 5)
!     Stump volume coef (column 6)
      DATA ((VOLCOEF(J,I), I=1,6), J=1,49) /
     & 12,0.2514,0.002679,4.658,0.01694,    .009967,
     & 68,0.5905,0.002168,17.167,0.01404,   .008877,
     & 71,1.4109,0.002227,17.592,0.01427,   .008877,
     & 94,0.3365,0.002599,6.81,0.01611,     .010699,
     & 95,0.2631,0.002706,0,0.01735,        .008877,
     & 105,1.2446,0.002165,19.149,0.01307,  .007017,
     & 125,2.0822,0.002046,24.848,0.01298,  .007176,
     & 129,0,0.002364,0,0.01625,            .008269,
     & 132,2.0822,0.002046,24.848,0.01298,  .007176,
     & 241,1.0406,0.002408,12.532,0.0156,   .011946,
     & 261,0,0.001856,0,0.01054,            .008579,
     & 299,0.5905,0.002168,17.167,0.01404,  .008877,
     & 313,1.7283,0.002189,63.992,0.01215,  .008476,
     & 314,1.3746,0.002206,36.859,0.01534,  .008894,
     & 316,1.7283,0.002189,63.992,0.01215,  .008476,
     & 317,1.7283,0.002189,63.992,0.01215,  .008476,
     & 318,1.3746,0.002206,36.859,0.01534,  .008894,
     & 371,0,0.00248,14.575,0.01766,        .009968,
     & 373,0,0.002485,34.832,0.01458,       .008380,
     & 375,2.3037,0.00181,37.619,0.01404,   .008380,
     & 400,2.4364,0.001881,57.449,0.01122,  .008980,
     & 460,0,0.002325,28.875,0.01466,       .010422,
     & 531,2.2793,0.002395,56.5,0.01465,    .010202,
     & 541,1.528,0.002021,65.124,0.01124,   .008728,
     & 543,2.599,0.001792,70.167,0.01015,   .011016,
     & 544,1.528,0.002021,65.124,0.01124,   .008728,
     & 601,2.6341,0.001887,67.801,0.01109,  .008980,
     & 602,2.6341,0.001887,67.801,0.01109,  .008980,
     & 611,0,0.002485,34.832,0.01458,       .008980,
     & 621,2.6341,0.001887,67.801,0.01109,  .008980,
     & 694,0,0.002485,34.832,0.01458,       .008980,
     & 731,0,0.002485,34.832,0.01458,       .008980,
     & 741,0,0.002335,17.978,0.01578,       .006594,
     & 742,0,0.002485,34.832,0.01458,       .011145,
     & 743,0.9461,0.002247,31.842,0.01483,  .006594,
     & 746,2.0756,0.001913,29.329,0.0148,   .007369,
     & 762,2.6341,0.001887,67.801,0.01109,  .008980,
     & 802,0.7316,0.001951,46.038,0.01173,  .009727,
     & 806,0.7554,0.002174,34.677,0.0137,   .009727,
     & 812,0.7554,0.002174,34.677,0.0137,   .009727,
     & 830,0.7554,0.002174,34.677,0.0137,   .009727,
     & 833,1.6378,0.002032,41.41,0.01326,   .008908,
     & 834,0.7554,0.002174,34.677,0.0137,   .009727,
     & 837,0.7554,0.002174,34.677,0.0137,   .009727,
     & 920,0,0.002485,34.832,0.01458,       .011145,
     & 951,0.9239,0.002206,36.821,0.01435,  .009639,
     & 970,0,0.002325,28.875,0.01466,       .010422,
     & 998,1.4824,0.001796,36.341,0.01339,  .008980,
     & 999,0.867,0.00194,36.341,0.01339,    .008980/
!     Total and nerch height formula     
      HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)=
     & 4.5+B1*(1-EXP(-B2*D))**B3*S**B4*(1.00001-TOP/D)**B5*B**B6
!     Cubicfoot and boardfoot volume formula     
      V(C0,C1,D,H)=C0+C1*D**2*H
     
      VOL = 0.0
      ERRFLAG = 0
      VOL4 = 0.0
      IF(DBHOB.LT.1.0)THEN
        ERRFLAG = 3
        RETURN
      ENDIF
      READ(VOLEQ(8:10),'(I3)') SPN
      CNT = 49
      IDX = 0
      CALL SEARCH(CNT,VOLSP,SPN,IDX,ERRFLAG)
      IF(IDX.LE.0) THEN
        ERRFLAG = 1
        RETURN
      ELSEIF(SPN.EQ.VOLCOEF(IDX,1))THEN
        B1 = HTCOEF(IDX,2)
        B2 = HTCOEF(IDX,3)
        B3 = HTCOEF(IDX,4)
        B4 = HTCOEF(IDX,5)
        B5 = HTCOEF(IDX,6)
        B6 = HTCOEF(IDX,7)
        C0 = VOLCOEF(IDX,2)
        C1 = VOLCOEF(IDX,3)
        D0 = VOLCOEF(IDX,4)
        D1 = VOLCOEF(IDX,5)
        S0 = VOLCOEF(IDX,6)
      ENDIF
      IF(SPN.LT.300)THEN
        BFMIND = 9.0
        IF(MTOPP.LT.0.1) MTOPP = 7.0
      ELSE
        BFMIND = 11.0
        IF(MTOPP.LT.0.1) MTOPP = 9.0
      ENDIF
      IF(MTOPS.LT.0.1) MTOPS = 4.0
      
      IF(SI.EQ.0) SI = 60
      IF(BA.EQ.0) BA = 90
      S = SI
      B = BA
      D = DBHOB
      
C*    CHECK FOR VALID HEIGHTS
      IF(HT1PRD.LE.0 .AND. HT2PRD.LE.0 .AND. HTTOT.LE.0)THEN
         IF(CTYPE.EQ.'C')THEN
            ERRFLAG = 7
            RETURN    
         ENDIF
      ENDIF
      RATIO = 1.0
      TOP = 0.0
      THTEST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)    
      IF(HTTOT.GT.0.0)THEN
        RATIO = HTTOT/THTEST
      ELSEIF(HT1PRD.GT.0.0)THEN
        TOP = MTOPP
        HT1EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
        RATIO = HT1PRD/HT1EST
      ELSEIF(HT2PRD.GT.0.0)THEN
        TOP = MTOPS
        HT2EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
        RATIO = HT2PRD/HT2EST
      ENDIF
      
      IF(HTTOT.LT.0.1) HTTOT = THTEST*RATIO
!     Compute total cubic volume from stump to tip      
      H = HTTOT
      VOL(1) = V(C0,C1,D,H)
      IF(HT2PRD.LT.0.1.AND.D.GE.5.0)THEN
        TOP = MTOPS
        HT2EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
        HT2PRD = HT2EST*RATIO
      ENDIF
      IF(HT1PRD.LT.0.1)THEN
        TOP = MTOPP
        HT1EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
        HT1PRD = HT1EST*RATIO
      ENDIF
!     Compute merch cubic volume from stump to 4 inch top      
      IF(HT2PRD.GT.0.1)THEN
        H = HT2PRD
        VOL(4) = V(C0,C1,D,H)
        VOL4 = VOL(4)
      ENDIF 
!     Compute merch cubic volume from stump to 7/9 inch top      
      IF(HT1PRD.GT.0.1.AND.PROD.EQ.'01')THEN
        H = HT1PRD
        VOL(4) = V(C0,C1,D,H)
!     Compute topwood cubic volume from 7/9 inch to 4 inch top      
        IF(VOL4.GT.0.0) VOL(7) = VOL4-VOL(4)
        VOL(10) = V(D0,D1,D,H)
!       convert International BF to Scribner using the method formerly used by NCRS-FIA       
        VOL(2) = VOL(10)*0.89
      ENDIF
      vol(14) = S0*D*D
      VOL(15) = VOL(1) - VOL(4) - VOL(7)
      IF(VOL(15).LT.0) VOL(15) = 0.0
      RETURN
      
      END SUBROUTINE HAHN_NC250
      

