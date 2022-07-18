C----------
C VOLUME $Id: scrib.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  10-30-2003
      SUBROUTINE SCRIB(DIA,LEN,COR,VOL)
C     DIA-REAL  LEN-REAL COR-CHAR  VOL-REAL

C     COMPUTES SCRIBNER BOARD FOOT VOLUME USING FACTORS AND
C     MAKING CORRECTIONS TO THE FACTOR VOLUMES IF DESIRED

C     COR - FLAG FOR FACTOR OR CORRECTED FACTOR VOLUMES
C     DIA - DIAMETER CLASS OF LOG
C     EXCEPT - EXCEPTIONS BETWEEN FACTOR AND SCRIBNER VOLUMES
C     FACTOR - FACTORS USED TO GENERATE SCRIBNER VOLUME
C     LEN - LENGTH OF LOG NOT INCLUDING TRIM
C     VOL - SCRIBNER DECIMAL C VOLUME (10 BDFT)

      INTEGER ILOW,IHIGH,ISCRPT,COMPAR
      REAL FACTOR(132),ANUM,XXX
      REAL EXCEPT(149)
      REAL DIA,LEN,VOL,VOLFAC
      INTEGER Q9
      CHARACTER*1 AGAIN,COR

C ###############################################################
C   THE SECTION BETWEEN THE LINES OF "###" SHOULD NORMALLY BE
C   COMMENTED OUT AS IT IS ONLY NECESSARY WHEN CHECKING FACTORS

C      CHARACTER*3 TNIRP /'YES'/
C      CHARACTER*1 NEWPG

C ###############################################################

C     COEFFICIENTS FOR FACTOR VOLUMES
      DATA (FACTOR(I),I=1,66)/0.000,0.143,0.390,0.676,
     > 1.070,1.160,1.400,1.501,2.084,3.126,3.749,4.900,
     > 6.043,7.140,8.880,10.000,11.528,13.290,14.990,
     > 17.499,18.990,20.880,23.510,25.218,28.677,31.249,
     > 34.220,36.376,38.040,41.060,44.376,45.975,48.990,
     > 50.000,54.688,57.660,64.319,66.730,70.000,75.240,
     > 79.480,83.910,87.190,92.501,94.990,99.075,103.501,
     > 107.970,112.292,116.990,121.650,126.525,131.510,
     > 136.510,141.610,146.912,152.210,157.710,163.288,
     > 168.990,174.850,180.749,186.623,193.170,199.120,
     > 205.685/
      DATA (FACTOR(I),I=67,132)/ 211.810,218.501,225.685,
     > 232.499,239.317,246.615,254.040,261.525,269.040,
     > 276.630,284.260,292.501,300.655,308.970,317.360,
     > 325.790,334.217,343.290,350.785,359.120,368.380,
     > 376.610,385.135,393.380,402.499,410.834,419.166,
     > 428.380,437.499,446.565,455.010,464.150,473.430,
     > 482.490,491.700,501.700,511.700,521.700,531.700,
     > 541.700,552.499,562.501,573.350,583.350,594.150,
     > 604.170,615.010,625.890,636.660,648.380,660.000,
     > 671.700,683.330,695.011,
     > 1.249,1.608,1.854,2.410,3.542,4.167,
     > 1.570,1.800,2.200,2.900,3.815,4.499/
C    EXCEPTION TO FACTOR VOLUMES
C     FIRST 2 POSITIONS ARE LENGTH NEXT 3 ARE DIAMETER
C       IF EXCEPTION IS ODD ADD 1 BDFT
C       IF EXCEPTION IS EVEN SUBTRACT 1 BDFT
      DATA (EXCEPT(I),I=1,77)/ 40420.,40460.,40510.,
     > 40520.,40530.,40540.,40550.,40600.,40630.,
     > 40650.,40720.,40730.,40740.,40750.,40760.,
     > 40800.,40860.,40880.,40930.,40960.,40980.,
     > 41010.,41020.,41030.,41040.,41050.,41060.,
     > 41110.,41120.,41150.,41180.,50100.,50191.,
     > 50211.,50410.,50430.,50460.,50590.,50640.,
     > 50650.,50740.,50750.,50810.,50840.,50860.,
     > 50890.,50900.,50930.,50950.,50970.,50990.,
     > 51090.,51100.,51130.,51190.,51200.,60201.,

     > 60211.,60821.,60920.,61080.,61120.,61140.,
     > 70091.,70110.,70611.,70710.,70791.,80241.,
     > 80291.,80381.,80640.,80660.,80671.,80690.,
     > 80711.,80771./
      DATA(EXCEPT(I),I=78,149)/80790.,80831.,90291.,
     > 90431.,90511.,90611.,90741.,100091.,100230.,
     > 100711.,100740.,100771.,100831.,100960.,
     > 101071.,110091.,110250.,110581.,110611.,
     > 110641.,110731.,120100.,130060.,130470.,
     > 130521.,130611.,130661.,130691.,130770.,
     > 130990.,140060.,140440.,140800.,150060.,
     > 150280.,150451.,150511.,150611.,150741.,
     > 150801.,170461.,170611.,170641.,170751.,
     > 170801.,180440.,180701.,180710.,180770.,
     > 180811.,180830.,180911.,180931.,180951.,
     > 180981.,181001.,181071.,181111.,190080.,
     > 190090.,190131.,200080.,200090.,200621.,
     > 200641.,200661.,200670.,200691.,200710.,
     > 200770.,200791.,
     > 999990./

C ###############################################################
C   THE SECTION BETWEEN THE LINES OF "###" SHOULD NORMALLY BE
C   COMMENTED OUT AS IT IS ONLY NECESSARY WHEN CHECKING FACTORS

C      IF (TNIRP .EQ. 'YES') THEN
C      NEWPG = CHAR(12)

C   WRITE OUT FACTORS USED TO GENERATE SCRIBNER VOLUME

C      WRITE (2,10) NEWPG
C  10   FORMAT (A1,'   FACTORS FOR COMPUTING SCRIBNER ',
C    >   'BOARD FOOT LOG VOLUMES',/,10X,'DIA = INSIDE ',
C    >  ' BARK SMALL END DIAMETER',/)
C      WRITE (2,15) (I,FACTOR(I),I=1,120)
C  15  FORMAT ( 5(4X,'DIA',1X,' FACTOR',),/,
C    >         24( 5(4X,I3,1X,F7.3),/ ))
C      WRITE (2,20)
C  20  FORMAT (/,'   FACTORS FOR LOGS 16 FT. THROUGH',
C    >  ' 31 FT. IN LENGTH',/)
C      WRITE (2,25) (I,FACTOR(I+115),I=6,11)
C  25  FORMAT ( 6(4X,'DIA',1X,' FACTOR',),/,
C    >       6(4X,I3,1X,F7.3),/ )
C      WRITE (2,30)
C  30  FORMAT (/,'   FACTORS FOR LOGS 32 FT. THROUGH',
C    >  ' 40 FT. IN LENGTH',/)
C      WRITE (2,35) (I,FACTOR(I+121),I=6,11)
C  35  FORMAT ( 6(4X,'DIA',1X,' FACTOR',),/,
C    >        6(4X,I3,1X,F7.3),/ )

C   WRITE OUT EXCEPTIONS TO FACTOR VOLUMES

C      WRITE (2,40)
C  40  FORMAT (/,'   EXCEPTIONS TO THE FACTOR GENERATED ',
C    >  'VOLUMES - 40420 IS A 4 FT. LONG LOG WITH A 42 ',
C    >  'INCH DIA.',/,3X,'IF THE NUMBER ENDS IN 0 SUBTRACT 1 ',
C    >  'FROM THE FACTOR VOLUME, IF IT ENDS IN 1 ADD 1 ',
C    >  'TO THE FACTOR VOLUME.',/)
C      WRITE (2,45) (EXCEPT(I),I=1,132)
C  45  FORMAT ( 13(10(3X,F7.0),/ ),3X,F7.0,3X,F7.0)

C      TNIRP = 'NO '
C      ENDIF
C #############################################################

C *************************************************************
C    START OF MAIN LOGIC
C *************************************************************

      IF(COR .NE. 'Y' .AND. COR .NE. 'N')THEN
c         WRITE(*,46)'  ERROR : COR NOT Y OR N'
c  46     FORMAT(A24)
      ENDIF
      IF(DIA .LT. 1) THEN
         VOL = 0.0
         RETURN
      ENDIF
c     set maximum small end diameter 
      if(DIA .GT. 120) DIA = 120
      
      Q9=INT(DIA)
      IF (DIA .GT. 5.0 .AND. DIA .LE. 11) THEN
        IF ((LEN.GT.15).AND.(LEN.LT.32)) Q9=INT(DIA+115.)
        IF ((LEN.GT.31).AND.(LEN.LT.41)) Q9=INT(DIA+121.)
      ENDIF

      VOLFAC=FACTOR(Q9)
      if(cor.eq.'Y') then
         VOL=AINT((LEN*VOLFAC+5)/10)
      else if (cor.eq.'N') then
         VOL=AINT(LEN*VOLFAC+.5)
      endif
C  FIX EXCEPTIONS TO FACTOR VOLUMES

      IF (COR.EQ.'Y')THEN
        ILOW=1
        IHIGH=149
        ANUM=LEN*1000+DIA
  100     AGAIN = 'N'
          ISCRPT=(IHIGH+ILOW)/2
          COMPAR=INT(AINT(EXCEPT(ISCRPT)/10.))
          IF (ANUM.EQ.COMPAR) THEN
             XXX =(EXCEPT(ISCRPT)/2.0)-INT(EXCEPT(ISCRPT)/2.0)
             IF (XXX.GT.0) THEN
                VOL=VOL+1
               ELSE
                VOL=VOL-1
             ENDIF
           ELSE IF (ILOW.NE.IHIGH-1) THEN
             IF (ANUM.GT.COMPAR) THEN
               ILOW=ISCRPT
              ELSE
               IHIGH=ISCRPT
             ENDIF
           AGAIN = 'Y'
           ENDIF
        IF (AGAIN .EQ. 'Y') GOTO 100
      ENDIF
      RETURN
      END


C************************************************************
C      GET INTERNATIONAL 1/4 VOLUME FOR THE PIECES          *
C************************************************************

      SUBROUTINE INTL14(DIB,LENGTH,BFINT)
      
      !parameters
      REAL DIB    !small end diameter inside bark
      REAL LENGTH !log length
      REAL BFINT  !board foot international
      !local variables
      REAl FF     !fraction of 4' segment (.25, .5 or .75)
      REAL SEGVOL !segment volume (4' segments)
      REAL LOGVOL 
      REAL SEDIAM !small end diameter
      INTEGER LOGSEG,IRNDVOL,JJJ,J
      
      !if diameter inside bark is lt 4 there is no bd ft volume
      IF(DIB .LT. 4.0) THEN
        BFINT = 0
        RETURN
      ENDIF
      
      LOGVOL = 0.0
      !calculate the number of 4'segments in the passed in log
      LOGSEG = INT(LENGTH/4.0)
      
      !get the leftover piece .25, .5 or .75% of 4' piece
      FF = LENGTH/4.0 - LOGSEG

      !for each 4' segment get the small end diameter which assumes
      !1/2" taper per segment
      DO 100, J = 1,LOGSEG
        SEDIAM = DIB + (LOGSEG-J)/2.0
        SEGVOL = (0.22*(SEDIAM)**2 - 0.71*(SEDIAM)) * 0.905
        LOGVOL = LOGVOL + SEGVOL
  100 CONTINUE
  
      !if there is a leftover piece get the volume of the top
      !4' piece and scale it by the percentage of 4'
      IF (FF.GT.0.0) THEN
          SEGVOL = FF*(.22*(DIB)**2 -
     >             0.71 *(DIB))*0.905
          LOGVOL = LOGVOL + SEGVOL
      ENDIF
      
      !logic for rounding to the nearest 5 board feet.
      IF(LOGVOL .LT. 7.5) THEN
          LOGVOL = 5
      ELSE
        !get the largest multiple of 10
        IRNDVOL=INT(LOGVOL/10.0)
        !get the remaining ones and tenths places in integer
         JJJ=INT(((LOGVOL/10.0) - IRNDVOL)*100)
        !if you have lt 2.5  board feet remainder round down 10
        IF(JJJ .LT. 25) THEN
           LOGVOL = IRNDVOL*10
        !if you have ge 7.5 board feet remainder round up 10
        ELSE IF(JJJ .GE. 75) THEN
           LOGVOL = (IRNDVOL+1) * 10
        !else round to 5
        ELSE
           LOGVOL = IRNDVOL*10 + 5
        ENDIF
      ENDIF
      
     
      BFINT = LOGVOL

c    ----------- end of INTERNATIONAL 1/4 routine
      RETURN
      END
