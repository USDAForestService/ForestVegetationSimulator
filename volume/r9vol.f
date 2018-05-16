C----------
C VOLUME $Id: r9vol.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  12-17-2008
      SUBROUTINE R9VOL(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,FORST,SI,BA,
     >            PROD,CTYPE,BFPFLG,CUPFLG,CDPFLG,SPFLG,HTTYPE,ERRFLAG)

C CREATED  : 01-30-90

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

      REAL VOL(15),DBHOB,R,VC,CF,HT1PRD,HT2PRD,HTTOT
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
     >                    PROD,ERRFLAG)
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
         R=((HT1PRD*DBHOB-3.75)/(24*HT1PRD-10.5))
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
            vol(4)=REAL((term1+term2+term3)*cf)
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
      
      RETURN
      END


