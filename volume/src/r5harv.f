!== last modified  09-14-2005
      SUBROUTINE  R5HARV (VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,
     >                    ERRFLAG)
C FROM THE PNW RESEARCH NOTE PNW-414
C EQUATIONS FOR TOTAL, WOOD, AND SAW-LOG VOLUME FOR FIFTEEN
C CALIFORNIA HARDWOODS - Norman H. Pillsbury and Michael L. Kirkley
C
C    INPUT - INTEGER  EQUNUM   THREE DIGIT SPECIES CODE
C            REAL     DBHOB      DIAMETER BREAST HEIGHT
C            REAL     HTTOT       HEIGHT
C            INTEGER  TOP      IF 4 USES WVOL EQUATIONS
C                              IF 8 USES SVOL EQUATIONS
C                              IF 0 USE TO TIP
C   OUTPUT   REAL  VOL(15) 
C            INTEGER ERROR     IF ERROR OCCURS VOLUMES SET TO 0.0
C                              -1 SPEC  ERROR
      CHARACTER*10 VOLEQ
      REAL*8   COFA(15,4), COFB (15,4), COFC(15,4)
      REAL*8   IV,COEFSEQB(3),COEFSEQC(3)
      REAL DBHOB, VOL(15), TARIF, CV4, CV6, CV8, CVT
      REAL CUFTGROS, BDFTGROS, INTLGROS, BALOG, DLOG, TLOG
      REAL DE, HE, BA, HTTOT,  HLOG, TERM1
      REAL B4, RS616, RS616L, RI6, XINT6, RI8
      REAL XINT8, CVTS, TOPTW, TOPC, TOPB
      REAL MTOPP
      REAL  F,V,D,RS816,SV616,SV816,R16,R18
      INTEGER SPEC,BFPFLG,CUPFLG,ERRFLAG,I

      DATA IV/10.0/

C REDALDER              71   DONE IN REDALDER.F77 ROUTINE
      DATA (COFA(1,I),I=1,4)/0.0,0.0,0.0,0.0/
      DATA (COFB(1,I),I=1,4)/0.0,0.0,0.0,0.0/
      DATA (COFC(1,I),I=1,4)/0.0,0.0,0.0,0.0/
C BIGLEAF MAPLE         76, 95
      DATA (COFA(2,I),I=1,4) /0.0034214162,2.35347,0.69586,0.0/
      DATA (COFB(2,I),I=1,4) /0.0004236332,2.10316,1.08584,0.40017/
      DATA (COFC(2,I),I=1,4) /0.0101786350,2.22462,0.57561,0.0/
C CALIFORNIA BLACK OAK  81
      DATA (COFA(3,I),I=1,4) /0.0036795695,2.12635,0.83339,0.0/
      DATA (COFB(3,I),I=1,4) /0.0012478663,2.68099,0.42441,0.28385/
      DATA (COFC(3,I),I=1,4) /0.0070538108,1.97437,0.85034,0.0/
C BLUE OAK              88
      DATA (COFA(4,I),I=1,4) /0.0042324071,2.53987,0.50591,0.0/
      DATA (COFB(4,I),I=1,4) /0.0036912408,1.79732,0.838884,0.15958/
      DATA (COFC(4,I),I=1,4) /0.0125103008,2.33089,0.46100,0.0/
C CANYON LIVE OAK       84
      DATA (COFA(5,I),I=1,4) /0.0031670596,2.32519,0.74348,0.0/
      DATA (COFB(5,I),I=1,4) /0.0006540144,2.24437,0.81358,0.43381/
      DATA (COFC(5,I),I=1,4) /0.0097438611,2.20527,0.61190,0.0/
C GIANT CHINKAPIN       93
      DATA (COFA(6,I),I=1,4) /0.0055212937,2.07202,0.77467,0.0/
      DATA (COFB(6,I),I=1,4) /0.0018985111,2.38285,0.77105,0.0/
      DATA (COFC(6,I),I=1,4) /0.0120372263,2.02232,0.68638,0.0/
C COAST LIVE OAK        82   / also 96 & 98
      DATA (COFA(7,I),I=1,4) /0.0024574847,2.53284,0.60764,0.0/
      DATA (COFB(7,I),I=1,4) /0.0006540144,2.24437,0.81358,0.43381/
      DATA (COFC(7,I),I=1,4) /0.0065261029,2.31958,0.62528,0.0/
C INTERIOR LIVE OAK     85
      DATA (COFA(8,I),I=1,4) /0.0041192264,2.14915,0.77843,0.0/
      DATA (COFB(8,I),I=1,4) /0.0006540144,2.24437,0.81358,0.43381/
      DATA (COFC(8,I),I=1,4) /0.0136818837,2.02989,0.63257,0.0/
C CALIFORNIA LAUREL     91
      DATA (COFA(9,I),I=1,4) /0.0016380753,2.05910,1.05293,0.0/
      DATA (COFB(9,I),I=1,4) /0.0007741517,2.23009,1.037,  0.0/
      DATA (COFC(9,I),I=1,4) /0.0057821322,1.94553,0.88389,0.0/
C PACIFIC MADRONE       94
      DATA (COFA(10,I),I=1,4) /0.0025616425,1.99295,1.01532,0.0/
      DATA (COFB(10,I),I=1,4) /0.000618153, 1.72635,1.26462,0.37867/
      DATA (COFC(10,I),I=1,4) /0.0067322665,1.96628,0.83458,0.0/
C OREGON WHITEOAK       86
      DATA (COFA(11,I),I=1,4) /0.0024277027,2.25575,0.87108,0.0/
      DATA (COFB(11,I),I=1,4) /0.0008281647,2.10651,0.91215,0.32652/
      DATA (COFC(11,I),I=1,4) /0.0072695058,2.14321,0.74220,0.0/
C TANOAK                87     / also 72 73 75
      DATA (COFA(12,I),I=1,4) /0.000577497, 2.19576,1.14078,0.0/
      DATA (COFB(12,I),I=1,4) /0.0002526443,2.30949,1.21069,0.0/
      DATA (COFC(12,I),I=1,4) /0.0058870024,1.94165,0.86562,0.0/
C CALIFORNIA WHITE OAK  83
      DATA (COFA(13,I),I=1,4) /0.0009684363,2.39565,0.98878,0.0/
      DATA (COFB(13,I),I=1,4) /0.0001880044,1.87346,1.62443,0.0/
      DATA (COFC(13,I),I=1,4) /0.0042870077,2.33631,0.74872,0.0/
C NOT TO BE USED
      DATA (COFA(14,I),I=1,4)/0.0,0.0,0.0,0.0/
      DATA (COFB(14,I),I=1,4)/0.0,0.0,0.0,0.0/
      DATA (COFC(14,I),I=1,4)/0.0,0.0,0.0,0.0/
C ENGLEMANN OAK         79   
      DATA (COFA(15,I),I=1,4)/.0053866353,2.61268,.31103,0.0/
      DATA (COFB(15,I),I=1,4)/0.0,0.0,0.0,0.0/
      DATA (COFC(15,I),I=1,4)/.0191453191,2.40248,.28060,0.0/
C GIANT SEQUOIA
      DATA (COEFSEQB(I),I=1,3) /0.001682608,1.755956,1.490641/
      DATA (COEFSEQC(I),I=1,3) /0.002438339,1.694874,1.098957/


          
C CHECK FOR VALID SPECIES AND IF NOT SET VOLUME TO -1 AND RETURN
      DO 10,I=1,15
        VOL(I) = 0
   10 CONTINUE

      ERRFLAG = 0
      if(dbhob .lt. 1.0)then
         errflag = 3
         goto 999
      endif    
      
      IF (BFPFLG.EQ.1 .OR. CUPFLG.EQ.1) THEN
        IF(VOLEQ(8:10).EQ.'060') THEN      
          SPEC = 0
        ELSEIF(VOLEQ(8:10).EQ.'351') THEN      
          SPEC = 1
        ELSEIF(VOLEQ(8:10).EQ.'312') THEN      
          SPEC = 2
        ELSEIF(VOLEQ(8:10).EQ.'818') THEN      
          SPEC = 3
        ELSEIF(VOLEQ(8:10).EQ.'807') THEN      
          SPEC = 4
        ELSEIF(VOLEQ(8:10).EQ.'805') THEN      
          SPEC = 5
        ELSEIF(VOLEQ(8:10).EQ.'431') THEN      
          SPEC = 6
        ELSEIF(VOLEQ(8:10).EQ.'801') THEN      
          SPEC = 7
        ELSEIF(VOLEQ(8:10).EQ.'839') THEN      
          SPEC = 8
        ELSEIF(VOLEQ(8:10).EQ.'981') THEN      
          SPEC = 9
        ELSEIF(VOLEQ(8:10).EQ.'361') THEN      
          SPEC = 10
        ELSEIF(VOLEQ(8:10).EQ.'815') THEN      
          SPEC = 11
        ELSEIF(VOLEQ(8:10).EQ.'631') THEN      
          SPEC = 12
        ELSEIF(VOLEQ(8:10).EQ.'821') THEN      
          SPEC = 13
        ELSEIF(VOLEQ(8:10).EQ.'212') THEN      
          SPEC = 14
        ELSEIF(VOLEQ(8:10).EQ.'811') THEN      
          SPEC = 15
        ELSE
          SPEC = -1
        ENDIF
      ENDIF       

C END CHECK

C JUNIPER SPECIES

      IF (SPEC .EQ. 0) THEN
         D = DBHOB
         TOPC = MTOPP
         IF(D.LT.5 .OR. HTTOT.LT.10) THEN
           CVTS = 0.00272708 * D * D * HTTOT
            V = 0
        ELSE
            F = 0.307 + 0.00086*HTTOT - 0.0037*D*HTTOT / (HTTOT - 4.5)
            BA = 0.005454154 * D * D
            CVTS = BA * F * HTTOT * (HTTOT / (HTTOT - 4.5))**2
C  IF TOP NOT EQUAL TO 0 THEN DEFAULT TO 4 INCH TOP
            IF(TOPC.GT.0) THEN
               V=(CVTS + 3.48)/(1.18052+0.32736 * EXP(-0.1 * D))-2.948
            ELSE
               V = CVTS
            ENDIF
        ENDIF
         VOL(4) = ANINT(V*10 + 0.5)/10.0
         vol(1) = cvts
C REDALDER SPECIES

      ELSEIF (SPEC .EQ. 1) THEN
        DE = DBHOB
        HE = HTTOT
        BA = DE * DE * 0.005454154

        TERM1 = (1.033 * (1.0 + 1.382937 * EXP(-4.015292 *
     >           (DE / 10.0)))) * (BA + 0.087266) - 0.174533
        DLOG = LOG10(DE)
        HLOG = LOG10(HE)

        CVTS = 10.0**(-2.672775 + 1.920617 * DLOG + 1.074024 * HLOG)

        TARIF = (CVTS * 0.912733) / TERM1

        IF(TARIF.LE.0.0) THEN
           TARIF = 0.01
        ENDIF

C**************************************************
C   CUBIC FOOT VOLUMES FOR TIP, 4", 6", 8"  TOPS **
C**************************************************

        TOPC = MTOPP
        CV4 = TARIF * (BA - 0.087266) / 0.912733
        CV8 = CV4 * (0.983 - 0.983 * 0.65**(DE - 8.6))

        CVT = TARIF * (0.9679 - 0.1051 * 0.5523**(DE - 1.5)) * 
     >        TERM1 / 0.912733
        CV6 = CV4 * (0.993 - 0.993 * 0.62**(DE - 6.0))

        IF (TOPC.ge.3 .and. topc.lt. 5) THEN
           VOL(4) = CV4
        ELSEIF (TOPC.ge.5 .and. topc.lt.7) THEN
           VOL(4) = CV6
        ELSEIF (TOPC.ge.7 .and. topc.le.9) THEN
           VOL(4) = CV8
        ELSEIF (TOPC.lt.3) THEN
           VOL(4) = cvt
        ENDIF
        
        vol(1)=cvt
        
        IF (VOL(4).LE.0.0) THEN
           VOL(4) = 0.0
        ENDIF

C*************************************************
C             SCRIBNER BOARD FOOT VOLUMES       **
C*************************************************

        IF (DBHOB .GE. 7.0) THEN

          TOPB = MTOPP
          B4 = TARIF / 0.912733
          BALOG = LOG10(B4)
          RS616L = 0.174439 + 0.117594 * DLOG * BALOG - 8.210585 /
     >             DE**2 + 0.236693 * BALOG - 0.00001345 * B4**2 -
     >             0.00001937 * DE**2

          RS616 = 10.0**RS616L
          SV616 = RS616 * CV6
          RS816 = 0.99 - 0.58 * (0.484**(DE - 9.5))
          SV816 = RS816 * SV616

          IF (TOPB.ge.5 .and. topb.lt.7) THEN
             VOL(2) = SV616
          ELSEIF (TOPB.ge.7 .and. topb.le.9) THEN
             VOL(2) = SV816
          ELSE
             VOL(2) = 0.0
          ENDIF

C*************************************************
C                 INTERNATIONAL 1/4"            **
C*************************************************

          TLOG = LOG10(DE * TARIF)
          R16 = -2.904157 + 3.466328 * TLOG - 0.02765985 * DE -
     >           0.00008025 * TARIF**2 + 11.29598 / DE**2
          XINT6 = R16 * CV6
          R18 = 0.99 - 0.55 * (0.485**(DE - 9.5))
          XINT8 = R18 * XINT6

          IF (TOPB.ge.5 .and. topb.lt.7) THEN
            VOL(10) = XINT6
          ELSEIF (TOPB.ge.7 .and. topb.le.9) THEN
            VOL(10) = XINT8
          ELSE
            VOL(10) = 0.0
          ENDIF

C*************************************************
C                  TOPWOOD LOGIC                **
C*************************************************

          TOPTW = MTOPP
          IF (TOPTW.LE.0.0) THEN
            VOL(7) = 0.0
          ELSE
            IF (TOPTW.EQ.6) THEN
              VOL(7) = CV4 - CV6
            ELSEIF (TOPTW.EQ.8) THEN
              VOL(7) = CV4 - CV8
            ELSEIF (TOPTW.EQ.4) THEN
              VOL(7) = 0.0  
            ELSE
              VOL(7) = 0.0
            ENDIF
          ENDIF
        ENDIF   ! DBHOB >= 7.0
C*************************************
C    END RED ALDER ROUTINE         ***
C*************************************

C*******************************************
C   GIANT SEQUOIA (pillsbury et al (1991) **
C*******************************************

      ELSE IF (SPEC.EQ.14) THEN

        VOL(2) = COEFSEQB(1)*(DBHOB**COEFSEQB(2))*HTTOT ** COEFSEQB(3)
        VOL(4) = COEFSEQC(1)*(DBHOB**COEFSEQC(2))*HTTOT ** COEFSEQC(3)
        vol(1) = vol(4)
C************************************************
C--  MISC HARDWOOD SPECIES                   ****
C************************************************
    
      ELSE 
        TOPC = MTOPP
        D = DBHOB
        BA = D**2 * 0.005454154
        CV4 = COFA(SPEC,1) * DBHOB**COFA(SPEC,2)*HTTOT**COFA(SPEC,3) * 
     >        IV**COFA(SPEC,4)
        CV8 = COFB(SPEC,1) * DBHOB**COFB(SPEC,2)*HTTOT**COFB(SPEC,3) * 
     >        IV**COFB(SPEC,4)
        IF (CV4.GT.0.AND.CV8.GT.0) THEN
          CV6 = CV4-((CV4-CV8)*.4)
        ELSE
          CV6=0
        ENDIF
        CVT = COFC(SPEC,1) * DBHOB**COFC(SPEC,2)*HTTOT**COFC(SPEC,3) *
     >        IV**COFC(SPEC,4)
        IF (TOPC.ge.3 .and. topc.lt. 5) THEN
           CUFTGROS = CV4
        ELSEIF (TOPC.ge.5 .and. topc.lt.7) THEN
           CUFTGROS = CV6
        ELSEIF (TOPC.ge.7 .and. topc.le.9) THEN
           CUFTGROS = CV8
        ELSEIF(TOPC.lt.3)THEN
           CUFTGROS = CVT
        ELSE
           CUFTGROS = 0.0
        ENDIF
        VOL(4) = CUFTGROS
        vol(1) = cvt
                               
C END CUFT CALCULATIONS
C
C
C DOCUMENTATION FOR CONVERTING CUBIC TO SCRIBNER AND INTL 1/4
C   IS CONTAINED IN THE SUBROUTINE REDALDER.F77
C
        TOPB = MTOPP

        IF (DBHOB.GE.5.0) THEN

C FOR 5" <= DBHOB < 11", FIND SCRIBNER AND INTERNATIONAL VOLUME BASED
C ON BOARD FOOT/CUBIC FOOT RATIO (CUBIC * 4 FOR SCRIBNER,
C CUBIC * 5 FOR I-1/4" RULE)
          IF (DBHOB.LT.11.0) THEN
            IF (TOPB.ge.5 .and. topb.lt.7) THEN
              BDFTGROS = CV6 * 4
              INTLGROS = CV6 * 5
            ELSEIF (TOPB.ge.7 .and. topb.le.9) THEN
              BDFTGROS = CV8 * 4
              INTLGROS = CV8 * 5
            ELSE
              BDFTGROS = 0.0
              INTLGROS = 0.0
            ENDIF
            VOL(2) = BDFTGROS

          ELSE
C START SCRIBNER CALCULATIONS
            TARIF=(CV8*.912733)/((.983-.983*.65**(D-8.6))*(BA-.087266))

            IF(TARIF.LE.0.0) TARIF=0.01

            B4=TARIF/.912733
            DLOG = LOG10(D)
            BALOG = LOG10(B4)
            RS616L = 0.174439 + 0.117594 * DLOG * BALOG - 8.210585 /
     >               D**2 + 0.236693 * BALOG - 0.00001345 * B4**2 -
     >               0.00001937 * D**2
            RS616 = 10.0**RS616L
            SV616 = RS616 * CV6
            RS816 = 0.99 - 0.58 * (.484**(D - 9.5))
            SV816 = RS816 * SV616
            IF (TOPB.ge.5 .and. topb.lt.7) THEN
              BDFTGROS = SV616
            ELSEIF (TOPB.ge.7 .and. topb.le.9) THEN
              BDFTGROS = SV816
            ELSE
              BDFTGROS = 0.0
            ENDIF
            VOL(2) = BDFTGROS
C END SCRIBNER

C NOW GET INTERNATIONAL
            TLOG = LOG10(D*TARIF)
            RI6 = -2.904154 + 3.466328 * TLOG - 0.02765985 * D -
     >            0.00008025 * TARIF**2 + 11.29598 / D**2
            XINT6 = RI6 * CV6
            RI8 = 0.99 - 0.55 * (0.485**(D - 9.5))
            XINT8 = RI8 * XINT6

            IF (TOPB.ge.5 .and. topb.lt.7) THEN
               INTLGROS = XINT6
            ELSEIF (TOPB.ge.7 .and. topb.le.9) THEN
              INTLGROS = XINT8
            ELSE
              INTLGROS = 0.0
            ENDIF

            VOL(10) = INTLGROS
          ENDIF   ! END INTL CALC
       ENDIF ! END DBH CHECK
      ENDIF   ! END MISC HARDWOODS
C CHECK MERCHANTABLITY OPTIONS
c        check for top diameter greater then dbh; no merch volume
      IF(MTOPP .GT. DBHOB) VOL(2) = 0.0
      IF(MTOPP .GT. DBHOB) VOL(10) = 0.0
      IF(MTOPP .GT. DBHOB) VOL(4) = 0.0


 999  RETURN
      END
