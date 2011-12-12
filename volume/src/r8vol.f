!== last modified  02-16-2007
      SUBROUTINE R8VOL(VOLEQ,DBHOB,HTTOT,UPSHT1,HT1PRD,MTOPP,PROD,VOL,
     >                 FORST,SI,BA,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG)

C  CREATED:   06-03-91
C  PURPOSE:   THIS ROUTINE IS THE DRIVER PROGRAM FOR CALCULATING
C             R8 VOLUMES.
C  DECLARE VARIABLES
      CHARACTER*1 CTYPE
      CHARACTER*2 PROD,FORST
      character*10 VOLEQ

      INTEGER BFPFLG,CUPFLG,SPFLG
      INTEGER ERRFLAG,I,SI,BA

      REAL DBHOB, HT1PRD, UPSHT1, VOL(15), HTTOT, MTOPP

      LOGICAL VOL1, VOL2

      do 100, i=1,15
         vol(i) = 0
 100  continue
      
      VOL1 = .FALSE.
      VOL2 = .FALSE.

      if(dbhob .lt. 1.0) then
         errflag = 3
         goto 999
      endif
    	
	IF(UPSHT1.LE.0 .AND. HT1PRD.GT.0)UPSHT1=HT1PRD

      IF (VOLEQ(4:6).EQ.'DVE'.OR.VOLEQ(4:6).EQ.'dve')then
        if(BFPFLG.EQ.1) VOL1 = .TRUE.
        if(CUPFLG.EQ.1) VOL1 = .TRUE.
        IF(SPFLG.EQ.1) VOL1 = .TRUE.
      ELSEIF (VOLEQ(4:6).EQ.'CLK'.OR.VOLEQ(4:6).EQ.'clk') THEN
        if(BFPFLG.EQ.1) VOL2 = .TRUE.
        if(CUPFLG.EQ.1) VOL2 = .TRUE.
        IF(SPFLG.EQ.1) VOL2 = .TRUE.
      ELSE
       ERRFLAG = 1
        RETURN
      ENDIF

      IF(VOL2) CALL R8VOL2 (VOLEQ,VOL,DBHOB,HT1PRD,UPSHT1,MTOPP,HTTOT,
     >                      CTYPE, ERRFLAG)

	IF(HT1PRD.LE.0)THEN
	    IF(CTYPE.EQ.'F' .AND. HTTOT.GT.0)THEN
	       CALL R8_MHTS(FORST,VOLEQ,DBHOB,HTTOT,SI,BA,HT1PRD,ERRFLAG)
	    ENDIF
	ENDIF
      IF(VOL1) CALL R8VOL1 (VOLEQ,DBHOB,HT1PRD,UPSHT1,PROD,VOL,ERRFLAG)

C     ROUND THE VOLUMES 10TH CUBIC FOOT, NEAREST BDFT
	VOL(2) = ANINT(VOL(2))
	VOL(4) = ANINT(VOL(4)*10.0)/10.0
	VOL(7) = ANINT(VOL(7)*10.0)/10.0
	VOL(10) = ANINT(VOL(10))
	VOL(12) = ANINT(VOL(12))

      IF(VOL(4) .LT. 0.5) VOL(4) = 0.5

 999  RETURN
      END

C***********************************************************************
C***********************************************************************
      SUBROUTINE R8_MHTS(FORST,VOLEQ,DBH,HTTOT,SI,BA,HT1PRD,ERRFLAG)
C***********************************************************************
C CALCULATES THE MERCH HTS BASED ON TOTAL TREE HEIGHT BY FORST AND SPECIES
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SI,BA,VFLAG,ERRFLAG,IFOR,ISPEC
	REAL DBH,HTTOT,HT1PRD,B(6),BFMIND,DBHMIN,TOPD
      REAL FACTOR,MHT,ESTCHT,ESTTHT

      IF(FORST(2:2) .LT. '0') THEN
	   FORST(2:2) = FORST(1:1)
	   FORST(1:1) = '0'
         IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      
      READ(FORST,'(i2)') IFOR

C     CHECK EQUATION NUMBER FOR INFORMATION
	IF(VOLEQ(4:4) .EQ. 'D' .OR. VOLEQ(4:4).EQ.'D')THEN
         READ(VOLEQ(8:10),'(I3)')ISPEC
         IF(ISPEC .LT. 300)THEN
            TOPD = 7.0
         ELSE
            TOPD = 9.0
         ENDIF
      ELSE
         IF(VOLEQ(3:3) .EQ. '0') THEN
		   HT1PRD = HTTOT
	      GO TO 100
	   ELSEIF(VOLEQ(3:3) .EQ. '4')THEN
		   TOPD = 4.0
	   ELSEIF(VOLEQ(3:3) .EQ. '7')THEN
  	      TOPD = 7.0
	   ELSEIF(VOLEQ(3:3) .EQ. '9')THEN
		   TOPD = 9.0
	   ENDIF
      ENDIF

      IF(BA.LE.0) BA = 90
      IF(IFOR.EQ.1 .OR. IFOR.EQ.3 .OR. IFOR.EQ.12) THEN
C     SE
        IF(SI.LE.0)SI = 80
	  VFLAG = 1
        CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)

	ELSE
C     SN      
        IF(SI.LE.0)SI = 70
	  VFLAG = 2
        CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)
      ENDIF
      
C----------
C  COMPUTE ESTIMATED TOTAL TREE HEIGHT.
C----------
      IF(DBH .LT. DBHMIN .OR. ERRFLAG.GT.0) THEN
         HT1PRD=0
         GO TO 100
      ELSE
         FACTOR = 0.0
         ESTTHT = 4.5+B(1)*(1.0-EXP(-1.0*B(2)*DBH))
     &         **B(3)*SI**B(4)*(1.00001-FACTOR)
     &         **B(5)*BA**B(6)
	ENDIF
C----------
C  COMPUTE THE MERCH HEIGHT TO A 4 INCH TOP DIAMETER (PULPWOOD)
C----------
	IF(TOPD.EQ.4.0 .OR. DBH.GE.BFMIND) THEN
         FACTOR = TOPD/DBH
         IF(FACTOR .GT. 1.0) FACTOR=1.0
         ESTCHT = 4.5+B(1)*(1.0-EXP(-1.0*B(2)*DBH))
     &         **B(3)*SI**B(4)*(1.00001-FACTOR)
     &         **B(5)*BA**B(6)
         MHT=ESTCHT*(HTTOT/ESTTHT)
         HT1PRD=INT(MHT)
	ELSE
	   HT1PRD = 0
      ENDIF

  100 CONTINUE
      RETURN
      END

