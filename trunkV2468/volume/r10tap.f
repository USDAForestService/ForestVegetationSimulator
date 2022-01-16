C----------
C VOLUME $Id: r10tap.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  03-20-2006
      SUBROUTINE R10TAP(TAPEQU,DBHOB,HTTOT,HTUP,D2)
C----------
C----------
C
C     TAPEQU  IS THE NUMBER OF THE PROFILE EQUATION THAT WILL BE USED
C              IN THE CALCULATION
C              TAPEQU 1051 IS SPRUCE-HEMLOCK - 16 FOOT LOGS
C              TAPEQU 1052 IS SPRUCE-HEMLOCK - 32 FOOT LOGS
C              TAPEQU 1053 IS WESTERN REDCEDAR - 16 FOOT LOGS
C              TAPEQU 1054 IS WESTERN REDCEDAR - 32 FOOT LOGS
C              TAPEQU 1055 IS ALASKA CEDAR - 16 FOOT LOGS
C              TAPEQU 1056 IS ALASKA CEDAR - 32 FOOT LOGS
C     DBHOB   IS DIAMETER BREAST HEIGHT OUTSIDE BARK
C     HTTOT   IS TOTAL HEIGHT OF TREE FROM GROUND TO TIP
C     HTUP    IS THE HEIGHT ON THE TREE STEM WHERE A DIB IS TO BE ESTIMATED
C     D2      IS THE ESTIMATED DIAMETER AT HEIGHT "HTUP"
C
C     LIST REAL VARIABLES
C
      CHARACTER*10 TAPEQU
      CHARACTER*2 ISP,IAC,IRC,IRA
      REAL RH, DSI, RH32,BK,RH40
      REAL HTTOT,HTUP,DBHOB,D2,D,H,BKAYC,BKWRC
      REAL*8 BKAC,DVA,BKWR,DVR,BB,DD2MI,DVREDA
C
C
C     PROFILE EQUATION  (USED FOR DIAMETER CALCULATIONS)
C***********************************************************************
C     SPRUCE-HEMLOCK TAPER EQUATION
C
C     THIS EQUATION IS USED WHEN CALCULATING DIB'S ALONG THE TREE STEM
C     THIS EQUATION NUMBER 1051 AND 1052
C

      DD2MI(RH,RH32,D,H) = RH**1.5 + (-.0052554*H + 0.000034947*H**2 
     >    + 0.104477*H/D) * (RH**1.5 - RH**3) + (7.76807/D**2
     >    - 0.0000094852*H**2 - 0.011351*H/D) * (RH**1.5-RH32**32)

C
C     BARK THICKNESS EQUATION
C
      BB(D,H) = (0.8467 + 0.0009144*D + 0.0003568*H)
C
C*********************************************************************
C     WESTERN REDCEDAR
C
C     THIS EQUATION IS USED WHEN CALCULATING DIB'S ALONG THE TREE STEM
C     THIS EQUATION NUMBER 1053 AND 1054
C
      DVR(RH,RH32,H,DBHOB) = RH**1.5 + ((RH**1.5 - RH**3) * 
     >     (5.17703194/DBHOB**2 - 0.12516819*DBHOB + 0.02537037*H
     >     - 0.00004193*H**2 + 0.00155481 * DBHOB**2)) + ((RH**1.5
     >     - RH32**32) * (-0.00002070*H**2 + 0.24125235/DBHOB**2))
C
C     BARK THICKNESS EQUATION
C
      BKWR(DBHOB,H) = 0.86031485 + 0.00059638*H - 0.18335961/DBHOB
C
C
C**********************************************************************
C     ALASKA YELLOW CEDAR
C
C     THIS EQUATION IS USED WHEN CALCULATING DIB'S ALONG THE TREE STEM
C     THIS EQUATION NUMBER 1055 AND 1056
C
      DVA(RH,RH32,H,DBHOB)=RH**1.5+((RH**1.5-RH**3)*(-0.02834001*DBHOB
     >    + 0.00007123*H**2 + 0.06709114*H/DBHOB))
     >    + ((RH**1.5 - RH32**32) * (.00282021*DBHOB - 0.00002277*H**2
     >    + 1.06064717/DBHOB**2 - 0.00528349*H/DBHOB))
C
C    BARK THICKNESS EQUATION FOR ALASKA YELLOW CEDAR
C
      BKAC(DBHOB,H) = 0.95866817 + 0.00064402 * DBHOB - 3.1299972/H
C
C     THE FOLLOWING TAPER EQUATION IS FOR RED ALDER
C         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)
C***********************************************************************
      DVREDA(RH,RH32,RH40,H,DBHOB)=0.91274*(RH**1.5)
     >    -(1.9758*(RH**1.5-RH**3.)*(DBHOB*10**(-2.)))
     >    + (8.2375*(RH**1.5 - RH**3.)*(H)*(10**(-3.)))
     >    - (4.964*(RH**1.5 - RH32**32.)*(H*DBHOB)*(10**(-5.)))
     >    + (3.773*(RH**1.5 - RH32**32.)*(H**0.5)*(10**(-3.)))
     >    - (7.417*(RH**1.5 - RH40**40.)*(H**2.)*(10**(-6.)))
C***********************************************************************

C***********************************************************************

      IAC='AC'
      IRC='RC'
      IRA='RA'
C      ISS='SS'
      IF(TAPEQU(8:10).EQ.'042') ISP='AC'
      IF(TAPEQU(8:10).EQ.'242') ISP='RC'
      IF(TAPEQU(8:10).EQ.'098') ISP='SS'
      IF(TAPEQU(8:10).EQ.'351') ISP='RA'

C     ZERO OUT ARRAYS

      D=DBHOB
      H=HTTOT
C
C   FIND SCALING DIAMETERS FOR ALL LOG HEIGHTS 
C
C  THIS IS THE ALASKA CEDAR PROFILE EQUATION CALCULATION

        IF (ISP.EQ.IAC .AND. DBHOB.LT.38.01) THEN
           RH = (H - HTUP)/(H - 4.5)
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
           IF(RH.LE.0.0)THEN
              RH = 0
              RH32 = 0
              D2 = 0
              RETURN
           ELSEIF(RH .LT. 0.078) THEN
              RH32= 0.078
           ELSE
              RH32= RH
           ENDIF
C
           DSI=REAL(DVA(RH,RH32,H,DBHOB))
           if(dsi.lt.0.0) dsi = 0.0
           BKAYC=REAL(BKAC(DBHOB,H))
           D2=(DSI*BKAYC)**0.5*DBHOB

C  THIS IS THE WESTERN REDCEDAR PROFILE EQUATION CALCULATION

        ELSE IF (ISP.EQ.IRC .AND. DBHOB.LT.56.01) THEN
           RH=(H - HTUP)/(H - 4.5)
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVR FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVR FUNCTION.
C----------
           IF(RH.LE.0.0)THEN
              RH = 0
              RH32 = 0
              D2 = 0
              RETURN
           ELSEIF(RH .LT. 0.078) THEN
              RH32= 0.078
           ELSE
              RH32= RH
           ENDIF
C
           DSI=REAL(DVR(RH,RH32,H,DBHOB))
           if(dsi.lt.0.0) dsi = 0.0
           BKWRC=REAL(BKWR(DBHOB,H))
           D2=(DSI*BKWRC)**.5*DBHOB
 
C     THIS IS THE SPRUCE-HEMLOCK AND RED ALDER PROFILE EQUATION CALCULATION

        ELSE
          IF(ISP.EQ.IRA)THEN
            BK = 0
          ELSE
            BK = REAL(BB(D,H))
        ENDIF
           RH = (H - HTUP)/(H - 4.5)
C----------
C      IF R<0.078 (R^32<10^-38), R32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DD2MI FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DD2MI FUNCTION.
C----------
            IF(RH.LE.0.0)THEN
               RH = 0
               RH32 = 0
               RH40 = 0
               D2 = 0
               RETURN
            ELSEIF(RH .LT. 0.078) THEN
               RH40 = 0.15
               RH32= 0.078
            ELSEIF(RH .LT. 0.15) THEN
               RH40 = 0.15
               RH32= RH
            ELSE
               RH40 = RH
               RH32= RH
            ENDIF
C
            IF(ISP.EQ.IRA)THEN
              D2=REAL(DVREDA(RH,RH32,RH40,H,D))
            ELSE
              D2=REAL(DD2MI(RH,RH32,D,H))
            ENDIF
           if(d2.lt.0.0) d2 = 0.0
           IF(ISP.EQ.IRA)THEN
             D2=(D2)**.5*D
           ELSE
             D2=(D2*BK)**.5*D
           ENDIF
        ENDIF
C
C     THIS IS THE END OF THE DIAMETER CALCULATIONS
C
      RETURN
      END 
