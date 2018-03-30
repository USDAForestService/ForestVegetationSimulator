      SUBROUTINE R8VOL(VOLEQ,DBHOB,HTTOT,UPSHT1,HT1PRD,MTOPP,PROD,VOL,
     >                 FORST,SI,BA,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
C----------
C VOLUME $Id: r8vol.f 0000 2018-02-14 00:00:00Z gedixon $
C----------

C  CREATED:   06-03-91
!== last modified  2-7-2014
C  PURPOSE:   THIS ROUTINE IS THE DRIVER PROGRAM FOR CALCULATING
C             R8 VOLUMES.
C  10/11/2012 YW Changed to set VOL(7) to 0.5 if it is 0 for PROD 08 and for other prod set vol(4) to 0.5.
C  02/07/2014 YW modified to allow enter broken top height to HT1PRD field for pulp tree
C  06/04/2014 YW Changed the merch height calculation to use R8_MHT
C  DECLARE VARIABLES
      CHARACTER*1 CTYPE
      CHARACTER*2 PROD,FORST, VAR, DIST
      character*10 VOLEQ,VOLEQTMP

      INTEGER BFPFLG,CUPFLG,SPFLG
      INTEGER ERRFLAG,I,SI,BA,VOLSP
      REAL DANUW

      REAL DBHOB, HT1PRD, UPSHT1, VOL(15), HTTOT, MTOPP,VOLTMP(15)

      LOGICAL VOL1, VOL2
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = REAL(BA)
      DANUW = REAL(SI)
C

      do 100, i=1,15
         vol(i) = 0
 100  continue
      
      VOL1 = .FALSE.
      VOL2 = .FALSE.

      if(dbhob .lt. 1.0) then
         errflag = 3
         goto 999
      endif
C The HT1PRD field is also used by pulpwood broken top height (YW 2/7/14)     
      IF(UPSHT1.LE.0 .AND. HT1PRD.GT.0.AND.PROD.EQ.'01')UPSHT1=HT1PRD

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

      IF(VOL2) THEN
        CALL R8VOL2 (VOLEQ,VOL,DBHOB,HT1PRD,UPSHT1,MTOPP,HTTOT,
     >                      CTYPE, ERRFLAG)
      ELSE
        IF(HT1PRD.LE.0)THEN
          IF(CTYPE.EQ.'F' .AND. HTTOT.GT.0)THEN
C           R8_MHTS has problem to calculate merch height because the original species FIA code
C           is not retained in the VOLEQ. Changed to call R8VOL2 to calc merch height
C           YW 12/08/2011
c            CALL R8_MHTS(FORST,VOLEQ,DBHOB,HTTOT,SI,BA,HT1PRD,ERRFLAG)
             READ (VOLEQ(8:10),'(I3)') VOLSP
c      get the default Clark equation
             DIST='01'
             CALL R8_CEQN(FORST,DIST,VOLSP,PROD,VAR,VOLEQTMP,ERRFLAG)
             ERRFLAG=0
             CALL R8VOL2 (VOLEQTMP,VOLTMP,DBHOB,HT1PRD,UPSHT1,MTOPP,
     >                      HTTOT,CTYPE, ERRFLAG)
          ENDIF
        ENDIF
       IF(VOL1) CALL R8VOL1 (VOLEQ,DBHOB,HT1PRD,UPSHT1,PROD,VOL,ERRFLAG)
      ENDIF
C     ROUND THE VOLUMES 10TH CUBIC FOOT, NEAREST BDFT
      VOL(2) = ANINT(VOL(2))
      VOL(4) = ANINT(VOL(4)*10.0)/10.0
      VOL(7) = ANINT(VOL(7)*10.0)/10.0
      VOL(10) = ANINT(VOL(10))
      VOL(12) = ANINT(VOL(12))
c Changed to NOT reset VOL(4) to 0.5 for PROD 08 (09/06/2012)
      IF(PROD.EQ.'08') THEN
        IF(VOL(4).LT.0.5.AND.VOL(7).LT.0.5) VOL(7) = 0.5
      ELSE
        IF(VOL(4) .LT. 0.5) VOL(4) = 0.5
      ENDIF

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
C  VFLAG = 1
C        CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)

      ELSE
C     SN      
        IF(SI.LE.0)SI = 70
C  VFLAG = 2
C        CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)
      ENDIF
C     REGION 8 IS USING SAME COEFFICIENT  
      VFLAG = 1
      CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)
      
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

