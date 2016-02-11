!== last modified  2-21-2012
      SUBROUTINE r10d2h(VOLEQ,DBHOB,HTTOT,VOL,CUTFLG,CUPFLG,BFPFLG,
     >                                                          ERRFLAG)

C    subroutine implements volume equations developed by Larsen and Winterberger.
c            PNW-RN-478.
c    aug, 2000.  KLC
C    2/21/2012 Y. Wang
C    Modified A00DVEW094 and added the following equation based on document from R10
C    A01DVEW094, A00DVEW375, A01DVEW375, A00DVEW747, A01DVEW747,
C    A00DVEW108, A00DVEW310, and A00DVEW351
C    A00 is used by costal Alaska and Interior Alaska south and east of the Alaska Range
C    A01 is used by Interior Alaska north and west of the Alaska Range
C
C    A00DVEW094, A00DVEW375, and A00DVEW747 are from PNW-RN-478 and PNW-RN-495 by Larson et al 1988 and 1990
C    A01DVEW094, A01DVEW375, and A01DVEW747 are from NOR-5 by Haack 1963 and NOR-6 by Gregory et al 1964
C    A00DVEW108, A00DVEW310, and A00DVEW351 are from Brackett 1973
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15)
      INTEGER CUTFLG,CUPFLG,BFPFLG,ERRFLAG,spn
      CHARACTER*2 EQN

      REAL D2H,VOLM
      REAL A,B,C, pi,BA,CVTS,TATS,TARIF,TV4,CV4,R64,CV6,B4,BCU1,SV6
      REAL BCU2,IV6,R84,CV8,RS86,SV8,RI86,IV8,CVSL,SCFT,XINTT
      
      ERRFLAG = 0
      IF(DBHOB.LE.1.0)THEN
        ERRFLAG = 3
        RETURN
      ENDIF
      IF(HTTOT.LE.0)THEN
        ERRFLAG = 4
        RETURN
      ENDIF
    
      IF((voleq(3:3) .eq. '0') .or. (voleq(3:3) .eq. 'O') .or.
     +   (voleq(3:3) .eq. 'o')) THEN
         EQN = '00'
      ELSEIF((voleq(3:3) .eq. '1') .or. (voleq(3:3) .eq. 'l')
     +   .or. (voleq(3:3) .eq. 'I') .or. (voleq(3:3) .eq. 'L')
     +   .or. (voleq(3:3) .eq. 'i')) THEN
         EQN = '01'
      ELSE
         ERRFLAG = 1
         RETURN
      ENDIF
      D2H = DBHOB*DBHOB * HTTOT

      if(voleq(8:10).eq.'094' .or. voleq(8:10).eq.'095') then
c  A00DVEW094
        IF(EQN .eq. '00') THEN
          IF(CUTFLG .EQ. 1)THEN
             VOL(1) = 0.65559+0.00191*D2H
          ENDIF

          IF(CUPFLG .EQ. 1 .AND. DBHOB .GT. 4.0)THEN
             VOL(4) = -0.21849 + 0.00189*D2H
          ENDIF

	    IF(BFPFLG.EQ. 1 .AND. DBHOB .GT. 6.0)THEN
	      VOL(2) = 0.000136 * (D2H**1.40338)
C XINTT from equation 6 (PNW-RN-495)	      
	      VOL(10) = 0.00078*(D2H**1.26485)
          ENDIF
        ELSE
C A01DVEW094        
C NOR-6
          IF(CUTFLG .EQ. 1)THEN
             VOL(1) = -1.1843+0.205*DBHOB+0.01639*HTTOT+0.00187*D2H
          ENDIF
         
C Equations for Subdivision 2 and Unit 4 (NOR-5)
          IF(CUPFLG .EQ. 1 .AND. DBHOB .GT. 4.0)THEN
             VOL(4) = -2.0555+(0.2982*DBHOB)+(0.00181*D2H)
          ENDIF

	    IF(BFPFLG.EQ. 1 .AND. DBHOB .GT. 6.0)THEN
	      VOL(2) = 98.7701+(0.02022*D2H) - 0.77651*DBHOB**2
     &                - 1.63023*HTTOT
	      VOL(10) = -67.1116+(0.013663011*D2H) + (3344.33/(DBHOB**2))
          ENDIF
        ENDIF  
      ELSEIF(voleq(8:10) .eq. '375' .or.
     +       voleq(8:10) .eq. '746' .or. 
     +       voleq(8:10) .eq. '920') THEN
C A00DVEW375
        IF(EQN .eq. '00') THEN
          IF(CUTFLG .EQ. 1)THEN
             VOL(1) = 0.64456+0.00206*D2H
          ENDIF
        
          IF(CUPFLG .EQ. 1 .AND. DBHOB .GT. 4.0)THEN
             VOL(4) = -0.7126+(0.00211*D2H)
          ENDIF

	    IF(BFPFLG.EQ. 1 .AND. DBHOB .GT. 6.0)THEN
	      VOL(2) = 0.000081*(D2H**1.48459)
	      VOL(10) = 0.00043*(D2H**1.34294)
          ENDIF
C A01DVEW375
        ELSE
          IF(CUTFLG .EQ. 1)THEN
             VOL(1) = -0.01408*DBHOB**2+0.00815*HTTOT+0.00227*D2H
          ENDIF        
        
          IF(CUPFLG .EQ. 1 .AND. DBHOB .GT. 4.0)THEN
             VOL(4) = -1.02411+(0.0022034075*D2H)
          ENDIF

	    IF(BFPFLG.EQ. 1 .AND. DBHOB .GT. 6.0)THEN
	      VOL(2) = -27.163+(0.00995*D2H)
	      VOL(10) = -29.8848+(0.011913048*D2H)
          ENDIF
        ENDIF
      ELSEIF(voleq(8:10) .eq. '747') THEN     
C A00DVEW747
        IF(EQN .eq. '00') THEN
          IF(CUTFLG .EQ. 1)THEN
             VOL(1) = 0.9864+0.00181*D2H
          ENDIF
        
          IF(CUPFLG .EQ. 1 .AND. DBHOB .GT. 4.0)THEN
             VOL(4) = -1.39764+(0.00188*D2H)
          ENDIF

	    IF(BFPFLG.EQ. 1 .AND. DBHOB .GT. 6.0)THEN
	      VOL(2) = -28.0674+(0.00937*D2H)
	      VOL(10) = -17.4877+(0.01119*D2H)
          ENDIF
C A01DVEW747
        ELSE
          IF(CUTFLG .EQ. 1)THEN
             VOL(1) = 0.00806*HTTOT+0.00175*D2H
          ENDIF
        
          IF(CUPFLG .EQ. 1 .AND. DBHOB .GT. 4.0)THEN
             VOL(4) = -0.8722+(0.001811522*D2H)
          ENDIF

	    IF(BFPFLG.EQ. 1 .AND. DBHOB .GT. 6.0)THEN
	      VOL(2) = -46.7415+(0.00956*D2H)
	      VOL(10) = -49.1199+(0.010941441*D2H)
          ENDIF
        ENDIF
      ELSEIF((voleq(8:10) .eq. '108') .or. 
     +       (voleq(8:10) .eq. '310') .or.
     +       (voleq(8:10) .eq. '351') .or.
     +       (voleq(8:10) .eq. '660'))THEN 
C Equation 20 for A00DVEW108, A00DVEW310, and A00DVEW351 
        READ(voleq(8:10),'(I3)')spn
        IF(voleq(8:10) .eq. '108') THEN
          A = -2.615591
          B = 1.847504
          C = 1.085772
        ELSEIF(voleq(8:10) .eq. '351') THEN
          A = -2.672775
          B = 1.920617  
          C = 1.074024
        ELSEIF(voleq(8:10).eq.'310' .or. voleq(8:10).eq.'660') THEN
          A = -2.770324
          B = 1.885813  
          C = 1.119043
        ENDIF 
        
c       basal area, squre feet     
        BA = 0.005454154*(DBHOB**2)
c       Volume, cubic feet, entire stem (incl. stump and top)
        CVTS = (10**A)*(DBHOB**B)*(HTTOT**C)
        VOL(1) = CVTS
        IF(DBHOB .gt. 4.0) THEN
c       Tarif access constants
         TATS = 0.912733/((1.033*(1+1.382937*EXP(-4.015292*(DBHOB/10))))
     +         *(BA+0.087266) - 0.174533)
         TARIF = CVTS * TATS
         TV4 = (BA-0.087266)/0.912733
c       Volume, cubic feet, stump to 4" top
         CV4 = TARIF * TV4
         IF(DBHOB .gt. 6.0) THEN 
c       Volume, cubic feet, stump to 6" top
           R64 = 0.993 - 0.993*0.62**(DBHOB-6)
           CV6 = CV4*R64
c       Volume, board feet, Scribner, stump to 6" top
           IF(((spn .ge. 300) .and. (DBHOB .ge. 11)) .or.
     +         ((spn .lt. 300) .and. (DBHOB .ge. 9))) THEN
             B4 = TARIF / 0.912733
             BCU1 = 10**(0.174439+0.117594* LOG10(DBHOB) * LOG10(B4) -
     +             8.210585/DBHOB**2 + 0.236693 * LOG10(B4) -
     +             0.00001345*B4**2 - 0.00001937*DBHOB**2)
             SV6 = CV6 * BCU1
c      Volume, bosrd feet, International 1/4", stump to 6" top
             BCU2 = -2.904154 + 3.466328 * LOG10(DBHOB * TARIF) - 
     +             0.02765985 * DBHOB - 0.00008205*TARIF**2 +
     +             11.29598/DBHOB**2
             IV6 = CV6 * BCU2
c      Volume, cubic feet, to 8" top
             R84 = 0.983 - 0.983*0.65**(DBHOB-8.6)
             CV8 = CV4 * R84
c      Volume, board feet, Scribner, to 8" top
             RS86 = 0.99-0.58*(0.484**(DBHOB-9.5))
             SV8 = SV6 * RS86
c      Volume, board feet International 1/4", to 8" top
             RI86 = 0.99 - 0.55*(0.485**(DBHOB-9.5))
             IV8 = IV6 * RI86
           ENDIF
         ENDIF
       ENDIF
       IF(spn.ge.300) THEN
         CVSL = CV8
         SCFT = SV8
         XINTT = IV8
       ELSE
         SCFT = SV6
         XINTT = IV6
         CVSL = CV6
       ENDIF
       
       IF(CUTFLG .EQ. 1)THEN
          VOL(1) = CVTS
       ENDIF
       IF(CUPFLG .EQ. 1)THEN
         VOL(4) = CVSL
       ENDIF

	 IF(BFPFLG.EQ. 1)THEN
	   VOL(2) = SCFT
	   VOL(10) = XINTT
       ENDIF           
           
      ENDIF

      IF(VOL(2) .LT. 0) VOL(2) = 0
      IF(VOL(4) .LT. 0) VOL(4) = 0
      IF(VOL(1) .LT. 0) VOL(1) = 0
      IF(VOL(10) .LT. 0) VOL(10) = 0
      RETURN
      END
