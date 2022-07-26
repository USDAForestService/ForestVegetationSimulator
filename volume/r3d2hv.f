!== last modified  12-3-2008
      SUBROUTINE R3D2HV(VOLEQU,UNT,HTTOT,HT1PRD,DBHOB,DRC,FCLASS,
     >                  HTTFLL,VOL,ERRFLAG)


C--  THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C--  USING REGION 3 D*D*H VOLUME DETERMINATION EQUATIONS.
c==klc  June 5, 2000
C YW 2018/09/04 Added 301DVEW060, 302DVEW060, 301DVEW106, 302DVEW106, 301DVEW800  
C YW 2018/09/06 Added 301DVEW122 AND 302DVEW122 FROM FIA VOLUME EQUATION CODE
! YW 2018/09/12 Changed the FCLASS to be: 1 = sngle, others = multistem
C**************************************************************

      CHARACTER*10 VOLEQU, VOLEQTMP
      CHARACTER*3 SPC
      CHARACTER*2 PROD
      INTEGER UNT, FCLASS, HTTFLL,ERRFLAG,I

      REAL DBHOB, D2H,DRC,D2HA, HTTOT,VOL(15),UM4,UM6,TWVOL,TOPD
      REAL B1,B2,B3,B4,SCBDFT,HT1PRD,ENTIRE,GCUFT6,GCUFT4,INTBDFT

      DO 10 I=1,15
        VOL(I)=0.0
 10   CONTINUE

C--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
      ERRFLAG = 0   

      if(dbhob.lt.1 .and. drc.lt.1) then
        ERRFLAG = 3
        GO TO 1000
      ENDIF
C==
C== PREDICTED TOTAL HEIGHT, BASED ON REGRESSION OF HEIGHT TO 4-INCH TOP
C==
      IF(HTTOT.LE.0.0 .AND. HT1PRD.GT.0.0) HTTOT = 12.211+1.1342*HT1PRD
C==
      IF (HTTOT.LT.4.5) THEN
        ERRFLAG = 4
         GOTO 1000
      ENDIF

C**************************************************************
C BASIC VOLUME CALCULATIONS FOR ALL TREES, ALL UNTS OF MEASURE*
C**************************************************************

      D2H=DBHOB**2*HTTOT
      ENTIRE=0.0
      GCUFT6=0.0
      GCUFT4 = 0.0
      INTBDFT = 0.0
      SCBDFT = 0.0
      UM4 = 0.0
      UM6 = 0.0
      TWVOL = 0.0
      SPC = VOLEQU(8:10)
      TOPD = 4.0
      IF(UNT.EQ.1)THEN
        PROD = '01'
      ELSE
        PROD = '02'
      ENDIF
C----------------------- PONDEROSA PINE (EAGER MILL STUDY)
C----------------------- ARIZONA PINE
C----------------------- APACHE PINE
      IF (VOLEQU(8:10).EQ.'122'.AND.VOLEQU(1:3).EQ.'300') THEN
         
         IF (D2H.LE.31629.91964) THEN
            SCBDFT=-1.786+0.00098814*D2H
         ELSE
	      IF(HTTFLL .LE. 0) HTTFLL = 25.0

            SCBDFT=-52.897+(0.12826*HTTFLL)+(0.0017678*D2H)
     >             +(879120.0 / D2H)
         ENDIF
            SCBDFT = SCBDFT * 10.0
C==
C==---------------------- EAGER CUBIC-FOOT VOLUME
C==
         IF (D2H.LE.33590.92207) THEN
            GCUFT6 = -1.7751+0.0018897*D2H
         ELSE
            GCUFT6 = -13.542+0.00224*D2H
         ENDIF
C==
C==--- HANN & BARE CUBIC-FOOT VOLUMES, COCONINO/LINCOLN FOR ALL FORESTS
C==
         ENTIRE = 0.081072 + (0.001984 * D2H) 
         UM4 = -0.125349+(0.003604*((4.0**3 *HTTOT)/DBHOB**1.5)) +
     >           (.005406*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         UM6 = -0.125349+(0.003604*((6.0**3 *HTTOT)/DBHOB**1.5)) +
     >           (.005406*DBHOB**2)
         TWVOL = UM6 - UM4
 
! ADD 301DVEW122 AND 302DVEW122 (YW 2018/09/06)
!--Ponderosa Pine, Apache Pine, Arizona Pine
!--Blackjack Pine in Lincon, Cocinino and Tonto NF's
!--Yellow pine in R3
!--Switched to yellow pine equation at 21 inches DBH based on Rmstand code
!--INT-209, Hann and Bare, 1978
      ELSEIF (VOLEQU(8:10).EQ.'122'.AND.VOLEQU(1:3).EQ.'301')THEN
        IF(DBHOB.LT.21.0)THEN
          VOLEQTMP = '301HAB0122'
        ELSE
          VOLEQTMP = '300HAB1122'
        ENDIF
        CALL HANN_PP(VOLEQTMP,DBHOB,HTTOT,PROD,TOPD,VOL,ERRFLAG)
          ENTIRE = VOL(1)
          UM4 = VOL(15)
          IF(UNT.EQ.1)THEN
            GCUFT6=VOL(4)
            TWVOL=VOL(7)
            INTBDFT=VOL(10)
            SCBDFT=VOL(2)
          ELSE
            GCUFT4=VOL(4)
          ENDIF
!--Ponderosa Pine, Apache Pine, Arizona Pine
!--Blackjack Pine in Carson and Santa Fe NF's
      ELSEIF (VOLEQU(8:10).EQ.'122'.AND.VOLEQU(1:3).EQ.'302')THEN
        IF(DBHOB.LT.21.0)THEN
          VOLEQTMP = '302HAB0122'
        ELSE
          VOLEQTMP = '300HAB1122'
        ENDIF
        CALL HANN_PP(VOLEQTMP,DBHOB,HTTOT,PROD,TOPD,VOL,ERRFLAG)
          ENTIRE = VOL(1)
          UM4 = VOL(15)
          IF(UNT.EQ.1)THEN
            GCUFT6=VOL(4)
            TWVOL=VOL(7)
            INTBDFT=VOL(10)
            SCBDFT=VOL(2)
          ELSE
            GCUFT4=VOL(4)
          ENDIF 
C----------------------- DOUGLAS FIR
C---------------LINCOLN,COCONINO,TONTO

      ELSEIF (VOLEQU(8:10).EQ.'202'.AND.VOLEQU(1:3).EQ.'301')THEN
         ENTIRE = 0.438374 + (0.001756 * D2H)
         UM6=-.083149+(.001219*((6.0**3*HTTOT)/DBHOB))+(.005417*
     >         DBHOB**2)
         GCUFT6=ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT=GCUFT6*(6.587353-(.892716*DBHOB**(-1))-(243.514909*
     >            DBHOB**(-2)))
         SCBDFT=INTBDFT*(1.000897-(4.100072*DBHOB**(-1.177748)))
C
         UM4 = -0.083149+(0.001219*((4.0**3 *HTTOT)/DBHOB)) +
     >              (.005417*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         TWVOL = UM6 - UM4
C------  DF ADJUSTMENT TO VOLUME TABLES-- BASED ON FY 87
C------  VOLUME VALIDATION PROJECT

         SCBDFT = SCBDFT*.932

C----------------------- DOUGLAS FIR
C-------------------- CARSON,SANTA FE
      ELSEIF (VOLEQU(8:10).EQ.'202'.AND.VOLEQU(1:3).EQ.'302') THEN
         ENTIRE =0.341133+(.001918*D2H)
        
         UM6 = -0.187631 + (0.006719*((6.0**3*HTTOT)/DBHOB**1.5)) + 
     >         (.005364 * DBHOB**2)
         GCUFT6 = ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT = GCUFT6*(6.597174-(.894047*DBHOB**(-1))-(243.877967*
     >           DBHOB**(-2)))
         SCBDFT= INTBDFT*(.870260 - (19.495942*DBHOB**(-2)))
C==
         UM4 = -0.187631+(0.006719*((4.0**3 *HTTOT)/DBHOB**1.5)) +
     >                  (.005364*DBHOB**2)
         GCUFT4 = ENTIRE - UM4

         TWVOL = UM6 - UM4

C----------------------- WHITE FIR
C-------------- LINCOLN,COCONINO,TONTO
      ELSEIF (VOLEQU(8:10).EQ.'015'.AND.VOLEQU(1:3).EQ.'301') THEN
         ENTIRE = 0.210904 + (0.001840*D2H)
         UM6 = -0.182700+(0.001248*((6.0**3*HTTOT)/DBHOB))+(0.006245*
     >           DBHOB**2)
         GCUFT6 = ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT = GCUFT6*(6.246875-(7.019940*DBHOB**(-1))-(201.958728*
     >             DBHOB**(-2)))
         SCBDFT = INTBDFT*(1.0-(1.888144*DBHOB**(-1))-(8.851449*
     >              DBHOB**(-2)))

         UM4 = -0.182700+(0.001248*((4.0**3 *HTTOT)/DBHOB**1.0)) +
     >              (.006245*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         TWVOL = UM6 - UM4

C----------------------- WHITE FIR
C-------------------- CARSON,SANTA FE

      ELSEIF (VOLEQU(8:10).EQ.'015'.AND.VOLEQU(1:3).EQ.'302') THEN
         ENTIRE = 0.157777+(.002009*D2H)
         UM6 = -0.187563+(.006326*((6.0**3*HTTOT)/DBHOB**1.5))
     >       +(.006041*DBHOB**2)
         GCUFT6 = ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT = GCUFT6*(5.736445-(1.720934*DBHOB**(-1))-(74.573790*
     >          DBHOB**(-2)))
          
         SCBDFT = INTBDFT * (1.017248 - (1.870568 * DBHOB**(-1)) - 
     >            (8.514451 * DBHOB**(-2)))
C==
         UM4 = -0.187563+(0.006326*((4.0**3 *HTTOT)/DBHOB**1.5)) +
     >              (.006041*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         TWVOL = UM6 - UM4

C----------------------- CORKBARK FIR
C----------------------- SPRUCE

      ELSEIF (VOLEQU(8:10).EQ.'093') THEN
         ENTIRE = 0.225466+(.002170*D2H)
         UM6 = -0.2664752+(.006129*((6.0**3*HTTOT)/DBHOB**1.5))
     >         +(0.007431*DBHOB**2)
         GCUFT6 = ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT = GCUFT6 * (5.987363 - (9.847918 * DBHOB**(-1)) - 
     >         (-300.812808*DBHOB**(-2)) - (2855.342454*DBHOB**(-3)))
         SCBDFT = INTBDFT * (.878454 - (15.998458 * DBHOB**(-2)))

         UM4 = -0.2664752+(0.006129*((4.0**3 *HTTOT)/DBHOB**1.5)) +
     >                  (.007431*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         TWVOL = UM6 - UM4

C----------------------- SOUTHWEST WHITE PINE
C----------------------- BRISTLECONE PINE
C----------------------- LIMBER PINE
C----------------------- FOXTAIL

      ELSEIF (VOLEQU(8:10).EQ.'113') THEN
         ENTIRE = 0.160889 + (.002032*D2H)
         UM6 = -0.213005 + (.004912*((6.0**3*HTTOT)/DBHOB**1.5))
     >          +(.006061*DBHOB**2)
         GCUFT6 = ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT = GCUFT6*(6.691967-(7.520114*DBHOB**(-1))-(216.348366*
     >             DBHOB**(-2)))
         SCBDFT = INTBDFT*(1.006086-(2.384660*DBHOB**(-1)))

         UM4 = -0.213005+(0.004912*((4.0**3 *HTTOT)/DBHOB**1.5)) +
     >              (.006061*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         TWVOL = UM6 - UM4

C------------------------------ ASPEN

      ELSEIF (VOLEQU(8:10).EQ.'746') THEN
         ENTIRE = 0.0327+(.002311*D2H)
         UM6 = -0.236432+(.005802*((6.0**3*HTTOT)/DBHOB**1.5))
     >          +(.006080*DBHOB**2)
         GCUFT6 = ENTIRE-UM6
         IF(GCUFT6.LT.0) GCUFT6 = 0

         INTBDFT = GCUFT6*(6.688085-(-1.276851*DBHOB**(-1))-(-4.504804*
     >             DBHOB**(-2))-(1423.985244*DBHOB**(-3)))
         SCBDFT = INTBDFT*(.887891-(17.19374*DBHOB**(-2)))

         UM4 = -0.236432+(0.005802*((4.0**3 *HTTOT)/DBHOB**1.5)) +
     >              (.006080*DBHOB**2)
         GCUFT4 = ENTIRE - UM4
         TWVOL = UM6 - UM4

C--------------- JUNIPERS, ARIZONA CYPRESS, AND MISC PINES. (PULPWOOD ONLY)

      ELSEIF (VOLEQU(8:10).EQ.'060') THEN
         IF (DBHOB.GT.3.0 .OR. DRC.GT.3.0) THEN
          IF(DRC .GT. 0)D2H = DRC*DRC*HTTOT

          D2HA = D2H/1000.0
C Added VOLEQ(2:3).EQ.'01' and '02' YW 2018/09/04          
          IF(VOLEQU(2:3).EQ.'01') THEN
C--From INT-363, Chojnacky, 1986
C--Pinyon-Juniper Volume Equations for Arizona Hualapai and Havasupai Indian Reservations
            IF(D2HA.LE.5.0) THEN
              GCUFT4 = -.05+2.48*D2HA+.057*D2HA*D2HA
            ELSE
              GCUFT4 = 4.24+2.48*D2HA-14.29/D2HA
            ENDIF
          ELSEIF(VOLEQU(2:3).EQ.'02') THEN  
C--From INT-379, Chojnacky, 1988
C--Woodland Volume Equations for Arizona Fort Apache and San Carlos Indian Reservations
            IF(FCLASS.NE.1) THEN
              IF(D2HA.LE.5) THEN
                GCUFT4 = -.169+1.9246*D2HA+.053*D2HA*D2HA
              ELSE
                GCUFT4 = 3.805+1.9246*D2HA-13.249/D2HA
              ENDIF
            ELSE
              IF(D2HA.LE.5) THEN
                GCUFT4 = -.002+1.7385*D2HA+.181*D2HA*D2HA
              ELSE
                GCUFT4 = 13.572+1.7385*D2HA-45.246/D2HA
              ENDIF
            ENDIF
          
          ELSE 
C--From INT-391, Chojnacky, 1988
C--Juniper, Pinyon, Oak and Mesquite Volume Equations for Arizona
            IF (FCLASS.NE.1) THEN
               IF (D2HA.LE.6.0) THEN
                  GCUFT4 = -0.129 + (2.0255*D2HA)+(0.1011*D2HA*D2HA)
               ELSE
                  GCUFT4 = 10.786 + (2.0255 * D2HA) - (43.663/D2HA)
               ENDIF
            ELSE
               IF (D2HA.LE.6.0) THEN
                  GCUFT4 = -0.032 + (2.1076*D2HA)+(0.1454*D2HA*D2HA)
               ELSE
                  GCUFT4 = 15.675 + (2.1076 * D2HA) - (62.827/D2HA)
               ENDIF
            ENDIF
          ENDIF  
         ELSE
            GCUFT4 = 0.0
         ENDIF
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4

C---------------------- PINYON PINES (PULPWOOD ONLY)

      ELSEIF (VOLEQU(8:10).EQ.'106') THEN
         IF (DBHOB.GT.3.0 .OR. DRC.GT.3.0) THEN
            IF(DRC .GT. 0)D2H = DRC*DRC*HTTOT
            D2HA = D2H/1000.0
           IF(VOLEQU(2:3).EQ.'01') THEN 
C--From INT-363, Chojnacky, 1986
C--Pinyon-Juniper Volume Equations for Arizona Hualapai and Havasupai Indian Reservations
             IF(D2HA.LE.5) THEN
               GCUFT4 = -.07+2.51*D2HA+.098*D2HA*D2HA
             ELSE
               GCUFT4 = 7.29+2.51*D2HA-24.53/D2HA
             ENDIF
           ELSEIF(VOLEQU(2:3).EQ.'02') THEN 
C--From INT-379, Chojnacky, 1988
C--Woodland Volume Equations for Arizona Fort Apache and San Carlos Indian Reservations
             IF(D2HA.LE.5) THEN
               TCVOL = -.073+2.1608*D2HA+.0804*D2HA*D2HA
             ELSE
               TCVOL = 5.961+2.1608*D2HA-20.112/D2HA
             ENDIF
             GCUFT4 = TCVOL
           ELSE
C--INT-391, Chojnacky, 1988
C--Juniper, Pinyon, Oak and Mesquite Volume Equations for Arizona
C--TM_EQ: 300DVEQ106
            IF (D2HA.LE.3.0) THEN
                GCUFT4 = -0.060 + (2.5139*D2HA)+(0.1466*D2HA*D2HA)
            ELSE
                GCUFT4 =  3.898 + (2.5139 * D2HA) - (7.917/D2HA)
            ENDIF
           ENDIF
         ELSE
            GCUFT4 = 0.0
         ENDIF
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4

C----------------------- MISC HARDWOODS / maple (PULPWOOD ONLY)

      ELSEIF (VOLEQU(8:10).EQ.'310') THEN
         IF (DBHOB.GT.3.0 .OR. DRC.GT.3.0) THEN
            IF(DRC .GT. 0)D2H = DRC*DRC*HTTOT
            IF (FCLASS.NE.1) THEN
               B4 = 0.0
            ELSE
               B4 = 1.0
            ENDIF 
C--
C--  SET COEFFICIENTS FOR THE EQUATION
C--
            B1 = -0.29013
            B2 = 0.126114
            B3 = 0.14489
C--
            GCUFT4 = (B1 + (B2 * D2H**0.3333) + (B3*B4))**3
            ENTIRE = GCUFT4
         ELSE
            GCUFT4 = 0.0
         ENDIF
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4
           
C----------------------- OAKS (PULPWOOD ONLY)
      ELSEIF (VOLEQU(8:10).EQ.'800') THEN
         IF (DBHOB.GT.3.0 .OR. DRC.GT.3.0) THEN
            IF(DRC .GT. 0)D2H = DRC*DRC*HTTOT
            D2HA = D2H/1000.0
           IF (VOLEQU(2:3).EQ.'01') THEN 
C--From INT-379, Chojnacky, 1988
C--Woodland Volume Equations for Arizona Fort Apache and San Carlos Indian Reservations
             IF(FCLASS.NE.1) THEN
               IF(D2HA.LE.5.0) THEN
                 GCUFT4 = -.181+2.1917*D2HA+.0208*D2HA*D2HA
               ELSE
                 GCUFT4 = -1.742+2.1917*D2HA+5.205/D2HA
               ENDIF
             ELSE
               IF(D2HA.LE.5.0) THEN
                 GCUFT4 = -.02+2.1896*D2HA+.127*D2HA*D2HA
               ELSE
                 GCUFT4 = 9.502+2.1896*D2HA-31.74/D2HA
               ENDIF
             ENDIF
           ELSE
C--From INT-391, Chojnacky, 1988
C--Juniper, Pinyon, Oak and Mesquite Volume Equations for Arizona
            IF (FCLASS.NE.1) THEN
               IF (D2HA.LE.4.0) THEN
                  GCUFT4 = -0.028 + (1.9545*D2HA)+(0.1400*D2HA*D2HA)
               ELSE
                  GCUFT4 = 6.691 + (1.9545 * D2HA) - (17.918/D2HA)
               ENDIF
            ELSE
               IF (D2HA.LE.4.0) THEN
                  GCUFT4 = -0.068 + (2.4048*D2HA)+(0.1383*D2HA*D2HA)
               ELSE
                  GCUFT4 = 6.571 + (2.4048 * D2HA) - (17.704/D2HA)
               ENDIF
            ENDIF 
           ENDIF
         ELSE
            GCUFT4 = 0.0
         ENDIF
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4
C-------------------- MESQUITE  (PULPWOOD ONLY)

      ELSEIF (VOLEQU(8:10).EQ.'999') THEN
         IF (DBHOB.GT.3.0 .OR. DRC.GT.3.0) THEN
            IF(DRC .GT. 0)D2H = DRC*DRC*HTTOT
            D2HA = D2H/1000.0
            IF (FCLASS.NE.1) THEN
               IF (D2HA.LE.2.0) THEN
                  GCUFT4 = 0.020 + (1.8972*D2HA)+(0.5756*D2HA*D2HA)
               ELSE
                  GCUFT4 = 6.927 + (1.8972 * D2HA) - (9.210/D2HA)
               ENDIF
            ELSE
               IF (D2HA.LE.2.0) THEN
                  GCUFT4 = -0.043 + (2.3378*D2HA)+(0.8024*D2HA*D2HA)
               ELSE
                  GCUFT4 = 9.586 + (2.3378 * D2HA) - (12.839/D2HA)
               ENDIF
            ENDIF 
         ELSE
             GCUFT4 = 0.0
         ENDIF
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4

C--------------------- HACKBERRY, ALDERLEAF, MAHOGANY  (PULPWOOD ONLY)

      ELSEIF (VOLEQU(8:10).EQ.'314') THEN
         IF (DBHOB.GT.3.0 .OR. DRC.GT.3.0) THEN
            IF(DRC .GT. 0)D2H = DRC*DRC*HTTOT
            IF (FCLASS.NE.1) THEN
               B4 = 0.0
            ELSE
               B4 = 1.0
            ENDIF 
C--
C--  SET COEFFICIENTS FOR THE EQUATION
C--
            B1 = -0.29013
            B2 = 0.126114
            B3 = 0.14489
C--
            GCUFT4 = (B1 + (B2 * D2H**0.3333) + (B3*B4))**3
         ELSE
            GCUFT4 = 0.0
         ENDIF
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4

C*************---------- SMALIANS FORMULA (PULPWOOD ONLY)

      ELSE
         B1 = DBHOB**2 * 0.005454
         B2 = 4.**2 * 0.005454
         GCUFT4 = (B1 + B2)/2. * HT1PRD
         ENTIRE = GCUFT4
         IF(UNT.EQ.1) GCUFT6 = GCUFT4
        
      ENDIF
C===
C===     END OF VOLEQU BLOCKS
C===
C-       DBH LT 9.0 HAS NO BDFT
     
      IF(DBHOB.LT.9) THEN
         SCBDFT = 0
         INTBDFT = 0
      ENDIF

C               TOTAL CUBIC (MIGHT BE ZERO)
      VOL(1) = ENTIRE
C               SAWTIMBER TREES (PRODUCT = 01) (BDFT, CUFT, TOPWOOD)
      IF (UNT .EQ. 1) THEN
         VOL(2) = SCBDFT
         VOL(10) = INTBDFT
         VOL(4) = GCUFT6
         VOL(7) = TWVOL
C               NON-SAWTIMBER TREES (PRODUCT != 01) (CUFT, CORDS)
      ELSEIF (UNT .EQ. 3) THEN
         VOL(4) = GCUFT4
         VOL(6) = VOL(4)/79.0
      ENDIF
      VOL(15) = UM4
C==
      IF (VOL(1).LT.0.0) THEN
         VOL(1)=0.0
      ENDIF
      IF (VOL(2).LT.0.0) THEN
         VOL(2)=0.0
      ENDIF
      IF (VOL(4).LT.0.0) THEN
         VOL(4)=0.0
      ENDIF
      IF (VOL(6).LT.0.0) THEN
         VOL(6)=0.0
      ENDIF
      IF (VOL(7).LT.0.0) THEN
         VOL(7)=0.0
      ENDIF
      IF (VOL(10).LT.0.0) THEN
         VOL(10)=0.0
      ENDIF
      IF (VOL(15).LT.0.0) VOL(15) = 0.0
C==
 1000 CONTINUE

      RETURN
      END
C**************************************************************

C********VOLUME EQUATION NUMBERS********

C--   THE FOLLOWING SPECIES ARE GROUPED FOR EQUATION NUMBERS
C-
C--       300DVEW122 = APACHE PINE, CHIHUAHUA PINE, ARIZONA PINE, PONDEROSA PINE
C--       301DVEW202 = DOUGLAS FIR; LINCOLN, COCONINO, TONTO, APS, COR, GIL, PRE
C--       302DVEW202 = DOUGLAS FIR; CARSON, SANTE FE, CIB, KAI
C--       301DVEW015 = WHITE FIR; LINCOLN, COCONINO, TONTO, APS, COR, GIL, PRE
C--       302DVEW015 = WHITE FIR; CARSON, SANTE FE,  CIB, KAI
C--       300DVEW093 = CORKBARK, SUBALPINE, ENGELMANN SPRUCE, BLUE SPRUCE
C--       300DVEW113 = BRISTLECONE PINE, LIBMBER PINE, SOUTHWESTERN WHITE PINE
C--       300DVEW746 = ASPEN
C--
C--       300DVEW060 = ALLIGATOR JUNIPER, UTAH JUNIPER, ONE SEED JUNIPER, 
C--                    ROCKY MTN JUNIPER, PINCHOT JUNIPER, REDBERRY JUNIPER,
C--                    ARIZONA CYPRESS, MISC SOFTWOODS
C--       300DVEW106 = PINON PINE, SINGLE LEAF PINON PINE, BORDER PINE
C--       300DVEW310 = BIGTOOTH MAPLE, BOX ELDER, ASH, WALNUT, WILLOW,
C--                    ARIZONA SYCAMORE, COTTONWOOD, N.M. LOCUST, TAMARISK,
C--                    MISC HARDWOODS
C--       300DVEW800 = EMERY OAK, GAMBEL OAK, ARIZONA WHITE OAK, GREY OAK, 
C--                    SILVERLEAF OAK
C--       300DVEW999 = MESQUITE
C--       300DVEW314 = HACKBERRY, ALDERLEAF, MNT MAHOGANY, HAIRY MTN MAHOGANY
C--

C--  HTTOT - REAL -  **TREE HEIGHT IN FT. FROM GROUND TO TIP**
C**********************************************************************
      SUBROUTINE HANN_PP(VOLEQ,DBHOB,HTTOT,PROD,MTOPP,VOL,ERRFLAG)
C     HANN & BARE Equation for ponderosa pine young-growth  (blackjack)
C     in Lincon, Cocinino and Tonto NF and Carson and Santa Fe NF
C     and old-growth (yellow pine) in region wide
C     Volume Equation number:
C     301HAB0122 -- Young-growth (blackjack pine) in Lincon, Cocinino and Tonto
C     302HAB0122 -- Young-growth (blackjack pine) in Carson and Santa Fe
C     300HAB1122 -- Old-growth (yellow pine)   
 
      CHARACTER*10 VOLEQ
      CHARACTEr*2 PROD
      REAL DBHOB,HTTOT,MTOPP,TOP,VOL(15)
      INTEGER ERRFLAG
      REAL D2H,UM4,UM6,TWVOL,SCBDFT,ENTIRE,GCUFT6,GCUFT4,INTBDFT  
      
      DO 100 I=1,15
        VOL(I)=0.0
 100  CONTINUE

C--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
      ERRFLAG = 0   
      IF(VOLEQ(8:10).NE.'122')THEN
        ERRFLAG = 1
        RETURN
      ENDIF
      if(dbhob.lt.1.0) then
        ERRFLAG = 3
        RETURN
      ENDIF
      IF (HTTOT.LT.4.5) THEN
        ERRFLAG = 4
        RETURN
      ENDIF

      D2H=DBHOB**2*HTTOT
      ENTIRE=0.0
      GCUFT6=0.0
      GCUFT4 = 0.0
      INTBDFT = 0.0
      SCBDFT = 0.0
      UM4 = 0.0
      UM6 = 0.0
      TWVOL = 0.0 
      TOP = MTOPP
      IF(TOP.EQ.0.0) TOP = 4.0
C     valid top diameter is between 3 and 8      
      IF(TOP.LT.3.0) TOP = 3.0
      IF(TOP.GT.8.0) TOP = 8.0
C ----Blackjack pine 
      IF(VOLEQ(7:7).EQ.'0')THEN
C ------Lincon, Cocinino and Tonto      
        IF(VOLEQ(2:3).EQ.'01')THEN
          ENTIRE = 0.0810724804 + 0.00198351037 * D2H
          UM6 =-0.125349396+(0.00360421889*((6.0**3*HTTOT)/DBHOB**1.5))
     &       + (.00540634204*DBHOB**2)
          GCUFT6=ENTIRE-UM6
          IF(GCUFT6.LT.0.0) GCUFT6 = 0.0
          UM4 =-0.125349396+(0.00360421889*((TOP**3*HTTOT)/DBHOB**1.5))
     &       + (.00540634204*DBHOB**2)
          GCUFT4 = ENTIRE - UM4
          TWVOL = UM6 - UM4
          INTBDFT=GCUFT6*(6.84751736-(7.69491322*DBHOB**(-1))
     &            -(221.377226*DBHOB**(-2)))
          SCBDFT=INTBDFT*(.96579222-.40579028*DBHOB**(-1)
     &            -16.93678414*DBHOB**(-2))
C ------Carson and Santa Fe NF
        ELSEIF(VOLEQ(2:3).EQ.'02')THEN   
          ENTIRE = 0.0483082948 + 0.00204968419 * D2H
          UM6 =-0.133967845+(0.00650174839*((6.0**3*HTTOT)/DBHOB**1.5))
     &       + (.00490223789*DBHOB**2)
          GCUFT6=ENTIRE-UM6
          IF(GCUFT6.LT.0.0) GCUFT6 = 0.0
          UM4 =-0.133967845+(0.00650174839*((TOP**3*HTTOT)/DBHOB**1.5))
     &       + (.00490223789*DBHOB**2)
          GCUFT4 = ENTIRE - UM4
          TWVOL = UM6 - UM4
          INTBDFT=GCUFT6*(7.58122078-(8.51941410*DBHOB**(-1))
     &            -(245.097535*DBHOB**(-2)))
          SCBDFT=INTBDFT*(.993986685-1.463486622*DBHOB**(-1)
     &            -12.40584877*DBHOB**(-2))
        ELSE
          ERRFLAG = 1
          RETURN
        ENDIF   
C ----Yellow pine  
      ELSEIF(VOLEQ(7:7).EQ.'1')THEN   
          ENTIRE = 0.237204154 + 0.00221122919 * D2H
          UM6 =.0185465259+(0.000788175798*((6.0**3*HTTOT)/DBHOB**1.0))
     &      + (.00505513624*DBHOB**2)
          GCUFT6=ENTIRE-UM6
          IF(GCUFT6.LT.0.0) GCUFT6 = 0.0
          UM4 =.0185465259+(0.000788175798*((TOP**3*HTTOT)/DBHOB**1.0))
     &      + (.00505513624*DBHOB**2)
          GCUFT4 = ENTIRE - UM4
          TWVOL = UM6 - UM4
          INTBDFT=GCUFT6*(7.10051404-(7.97921881*DBHOB**(-1))
     &            -(229.556497*DBHOB**(-2)))
          SCBDFT=INTBDFT*(.982101210-.926027395*DBHOB**(-1)
     &            -14.49443523*DBHOB**(-2))
      ELSE
        ERRFLAG = 1
        RETURN
      ENDIF
      VOL(1) = ENTIRE
C     SAWTIMBER TREES (PRODUCT = 01) (BDFT, CUFT, TOPWOOD)
      IF (PROD.EQ.'01') THEN
         VOL(2) = SCBDFT
         VOL(10) = INTBDFT
         VOL(4) = GCUFT6
         VOL(7) = TWVOL
C     NON-SAWTIMBER TREES (PRODUCT != 01) (CUFT, CORDS)
      ELSE
         VOL(4) = GCUFT4
         VOL(6) = VOL(4)/79.0
      ENDIF
      VOL(15) = UM4
      IF (VOL(1).LT.0.0) THEN
         VOL(1)=0.0
      ENDIF
      IF (VOL(2).LT.0.0) THEN
         VOL(2)=0.0
      ENDIF
      IF (VOL(4).LT.0.0) THEN
         VOL(4)=0.0
      ENDIF
      IF (VOL(6).LT.0.0) THEN
         VOL(6)=0.0
      ENDIF
      IF (VOL(7).LT.0.0) THEN
         VOL(7)=0.0
      ENDIF
      IF (VOL(10).LT.0.0) THEN
         VOL(10)=0.0
      ENDIF
      IF (VOL(15).LT.0.0) VOL(15) = 0.0
      RETURN
      END