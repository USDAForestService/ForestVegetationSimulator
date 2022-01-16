C----------
C VOLUME $Id: r6vol.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  01-18-2013
C 01/18/2013 added calculation for stump vol(14 and tip VOL(15)
      SUBROUTINE R6VOL(VOLEQ,FORST,DBHOB,BTR,FCLASS,MTOPP,HT1,HTTYPE,
     >           VOL,LOGVOL,NOLOGP,LOGDIA,SCALEN,DBTBH,HT1PRD,CTYPE,
     >           ERRFLAG)

C  CREATED:   06/19/90
C  PURPOSE:   THIS ROUTINE IS THE DRIVER PROGRAM FOR CALCULATING
C             R6 VOLUMES.
C  DECLARE VARIABLES

      CHARACTER*1  HTTYPE,CTYPE
      CHARACTER*2  FORST
c      CHARACTER*89 INREC
      CHARACTER*10 VOLEQ
      
      INTEGER FCLASS,ZONE,ERRFLAG
      INTEGER LOGS,I,J
      integer lggrd(20),IXXX
      REAL DBHIB,DBTBH,FC_HT,INTBF(20),D17,HT1PRD
      REAL DBHOB,BTR,MTOPP,HT1,TLH,TTH,TAPCOF
      REAL SCALEN(20),XLEN(20),LOGVOL(7,20),LOGV
      REAL VDEF(20),VOL(15),NOLOGP,LOGDIA(21,3),T,TDI1,TDI2

C FLAG FOR CRUISING METHODS, FLAGS WRITING TO THE LOG STOCK FILE
C      INITIALIZE VARIABLES

      DO 30 I=1,20
        VDEF(I)=0.0
        lggrd(i) = 0 
C        IF (I.LE.11) VOL(I)=0
  30  CONTINUE
   
      DO 35, I=1,7
        DO 33, J=1,20
            LOGVOL(I,J) = 0.0
  33     CONTINUE
  35  CONTINUE         
      
      DO 37, I=1,15
        VOL(I) = 0.0
  37  CONTINUE
      
      LOGS=0
      ERRFLAG = 0

      if(DBHOB.LT.1.0) then
         ERRFLAG = 3
         RETURN
      ENDIF
C ADD DECIMAL IF HT IS IN LOGS
      IF (HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
         HT1=HT1/10
         IF(HT1.GT.20)THEN
           ERRFLAG = 7
           RETURN
         ENDIF
      ENDIF

      IF(FCLASS .LE. 0) THEN
        IF(CTYPE .EQ. 'F') THEN
          !  GET FORMCLASS
            CALL GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
        ELSE
            ERRFLAG = 2
            RETURN
        ENDIF
      ENDIF
C  STANDARD TAPER COEFFICIENT

      TAPCOF=0.62


c DETERMINE THE ZONE

      IF (VOLEQ(1:3).EQ.'616'.or. voleq(1:3).eq.'628') THEN
        ZONE=1
        FC_HT = 17.3
      ELSEIF (VOLEQ(1:3).EQ.'632') THEN
        ZONE=2
        FC_HT = 33.6
      ELSE
       ERRFLAG = 1
        GOTO 1000
      ENDIF

C DETERMINE IF TREE HT IS EXPRESSED AS NUMBER OF LOGS OR
C HT IN FT FROM STUMP.
      IF (HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
        TLH=HT1
        TTH=0
      ELSE
        TLH=0
        TTH=HT1
      ENDIF

C  SMALL TREE CHECKS FOR HEIGHT IN FEET
      DBHIB = DBHOB-DBTBH
      IF(TTH.GT.0) THEN 
          D17 = FCLASS/100.0*DBHOB     
          IF(TTH.LE.FC_HT) THEN  
             NOLOGP = 0
C              use smailians to find total cubic volume
             VOL(1)=0.00272708*(DBHIB*DBHIB)*TTH
             go to 1000
          ELSEIF(DBHIB .LT. MTOPP) THEN
c            only total cubic volume possible
             CALL R6VOL3(DBHOB,DBTBH,FCLASS,TTH,ZONE,VOL)
             GO TO 1000
          ELSE
c            call the total cubic routines
             CALL R6VOL3(DBHOB,DBTBH,FCLASS,TTH,ZONE,VOL)
          ENDIF
      ENDIF                                  
      CALL R6DIBS(ZONE,DBHOB,BTR,FCLASS,MTOPP,TLH,TTH,NOLOGP,
     >            LOGDIA,SCALEN,XLEN,TAPCOF)

      IF(TTH.GT.0) THEN
         HT1PRD = 1.0
         DO 65, I=1,20
            HT1PRD = HT1PRD + XLEN(I)
   65    CONTINUE
      ENDIF

      IF(ZONE.EQ.2 .AND. TTH.GT.0 .AND. LOGDIA(1,1).GT.DBHOB)THEN
C          SMALL TREE CORRECTION FOR WESTSIDE TREES
         LOGDIA(1,2) = LOGDIA(2,2) + 1.0
         LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
         IF (TTH.LT.40.0) THEN
            LOGDIA(2,2) = 0.0
            LOGDIA(2,1) = 0.0
            SCALEN(2) = 0.0
            NOLOGP = 1.0
            LOGDIA(1,2) = MTOPP
            LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
         ENDIF
      ENDIF

      CALL R6VOL1(ZONE,DBHOB,FCLASS,NOLOGP,LOGDIA,LOGVOL,INTBF)           

c     PNW appraisal fix for Zone 2 bdft volume      
      IF(VOLEQ(1:3).EQ.'628') THEN
          CALL R6FIX(DBHOB,FCLASS,TLH,TTH,HTTYPE,LOGVOL)
      ENDIF

C  DETERMINE LOG-LOOP PARAMETER AND IF TOP 8 FT SEGMENT PRESENT
C  ON EASTSIDE.  LOGS VARIABLE = NO. OF OF 16 FT LOGS
      LOGS=INT(NOLOGP)
      T=NOLOGP-LOGS
      IF (T.GT.0.0) LOGS=LOGS+1
C  REMOVE THE 0.3 FEET OF TRIM ON ALL THE 16 FOOT LOGS  
      DO 100, I=1,LOGS
         IXXX = INT(SCALEN(I))
         SCALEN(I) = IXXX
  100 CONTINUE

C IF WESTSIDE - CONVERT NOLOGP VARIABLE TO NO. OF 32'S

      IF (ZONE.EQ.2) NOLOGP=NOLOGP/2.0

C ACCUMULATE LOG VOLUMES FOR THE TOTAL TREE VOLUME

C***********************
      DO 500 I=1,LOGS
C**********************
        LOGV = ANINT(LOGVOL(1,I))
        LOGVOL(1,I) = LOGV
        LOGV = ANINT(LOGVOL(4,I)*10.0)/10.0
        LOGVOL(4,I) = LOGV

c        VOL(1) = VOL(1) + LOGVOL(4,I)
        VOL(2) = VOL(2) + LOGVOL(1,I)
        VOL(4) = VOL(4) + LOGVOL(4,I)
        VOL(10) = VOL(10) + anint(INTBF(I))

c        logvol(2,i)=LOGVOL(1,I)
c        logvol(5,i)=LOGVOL(4,I)
        LOGVOL(7,I)=INTBF(I)

 500  CONTINUE
      NOLOGP = logs
      DO 600, I = LOGS,1,-1
          TDI1 = LOGDIA(I,1)
          LOGDIA(I+1,1) = TDI1
          TDI2 = LOGDIA(I,2)
          LOGDIA(I+1,2) = TDI2
  600 CONTINUE
C     calculate stump vol as 1 foot cylindar
      IF(VOL(14).LT.0.01) VOL(14)=0.005454154*LOGDIA(1,2)**2
      
      LOGDIA(1,1) = 0
      LOGDIA(1,2) = 0
c     calculate stem tip volume
      IF(VOL(4).GT.0.0) VOL(15)=VOL(1)-VOL(4)-VOL(14)
      IF(VOL(15).LT.0.0) VOL(15)=0.0
 1000 RETURN
      END


c***********************************************************************
      SUBROUTINE GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
      
      CHARACTER*2 FORST
      CHARACTER*3 SPEC
      CHARACTER*10 VOLEQ
      REAL DBHOB
      INTEGER IFORST,FCLASS

      SPEC = VOLEQ(8:10)
      IF(SPEC .EQ. '000') THEN
        FCLASS = 80
        RETURN
      ENDIF

      IF(FORST(2:2) .LT. '0') THEN
         FORST(2:2) = FORST(1:1)
         FORST(1:1) = '0'
         IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF

      READ(FORST,'(i2)') IFORST

      IF(IFORST.EQ.4 .OR. IFORST.EQ.7 .OR. IFORST.EQ.14 .OR. 
     >                                               IFORST.EQ.16)THEN
         CALL FORMCL_BM(SPEC,IFORST,DBHOB,FCLASS)
      ELSE IF(IFORST.EQ.10  .OR. IFORST.EQ.11)THEN
         CALL FORMCL_CA(SPEC,IFORST,DBHOB,FCLASS)
      ELSE IF(IFORST.EQ.8 .OR. IFORST.EQ.17 )THEN
         CALL FORMCL_EC (SPEC,IFORST,DBHOB,FCLASS)
      ELSE IF(IFORST.EQ.21)THEN
         CALL FORMCL_NI (SPEC,IFORST,DBHOB,FCLASS)
      ELSE IF(IFORST.EQ.12  .OR. IFORST.EQ.9)THEN
         CALL FORMCL_PN (SPEC,IFORST,DBHOB,FCLASS)
      ELSE IF(IFORST.EQ.1 .OR. IFORST.EQ.2 .OR. IFORST.EQ.20)THEN
         CALL FORMCL_SO (SPEC,IFORST,DBHOB,FCLASS)
      ELSE IF(IFORST.EQ.6 .OR. IFORST.EQ.15 .OR. IFORST.EQ.18 .OR.
     >                                IFORST.EQ.3 .OR. IFORST.EQ.5)THEN
         CALL FORMCL_WC (SPEC,IFORST,DBHOB,FCLASS)
      ELSE 
          FCLASS = 80
      ENDIF

      RETURN
      END

c VARIABLES DEFINED IN THE CALL LIST
c      VOLEQ - Volume equation number (10 digits)
c      DBHOB - Diameter at Breast Height Outside Bark
c      BTR  -  Bark Thickness ratio (optional)
c      FCLASS - Girard's Form Class
c      MTOPP - Merchantable Top Diameter for Main Stem
c      HT1 - Height to a merchantable height (in feet or number of logs) (optional)
c      HTTYPE - Character flag describes HT1 variable:
c                  F or blank = HT1 is in feet
c                  L = HT1 is number of logs
c      VOL(15) - Tree Volumes
C           VOL(1) - Total volume for the tree in cubic feet, ground to tip.
C              Does not include the limbs and roots.
C           VOL(2) - Gross amount of board foot as primary product - Scribner.
C           VOL(3) - Net amount of board foot as primary product - Scribner.
C           VOL(4) - Gross amount of cubic foot as primary product - Smalian.
C           VOL(5) - Net amount of cubic foot as primary product - Smalian.
C           VOL(6) - Amount of cord wood as primary product.
C           VOL(7) - Gross amount of cubic foot as secondary product - Smalian.
C           VOL(8) - Net amount of cubic foot as secondary product - Smalian.
C           VOL(9) - Amount of cord wood as secondary product.
C           VOL(10) - Gross amount of board foot as primary product - INT'L 1/4"
C           VOL(11) - Net amount of board foot as primary product - INT'L 1/4"
C           VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C           VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
c      LOGVOL(7,20) - Volumes for individual logs (X = log number)
C           LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
C           LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
C           LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
C           LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
C           LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
C           LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
C           LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
c      NOLOGP - Number of logs in the main stem
c      LOGDIA(20,3) - Log end diameters
C           LOGDIA(x,1) = scaling dib (used to compute the volumes)
C           LOGDIA(x,2) = actual dib 
c           LOGDIA(x,3) = actual dob
c      SCALEN - Scale length or log lengths
c      DBTBH  -  Double Bark Thickness at Breast Height (optional)
c      ERRFLAG - Error flag.
