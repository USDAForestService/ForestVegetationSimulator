C  DIB Calculation for the profile model tutorial
C
C  Modified version of profile.f used by the profile model tutorial
C  for calculating dib at at a given height.  NOTE: height where dib
C  is requested is passed in the DRCOB field.  DIB is returned in DIBO

C  created TDH 10/30/10  
C
c  revised 06/23/11
C  Cleaned up code and added comments
C
C_______________________________________________________________________

        SUBROUTINE PMTPROFILE (FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,DIBO,
     >   HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,
     >   AVGZ1,AVGZ2,HTREF,DBTBH,BTR,VOL,
     >   CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,
     >   CTYPE,FCLASS,PROD,ERRFLAG)
C_______________________________________________________________________
   
       !DEC$ ATTRIBUTES DLLEXPORT::PMTPROFILE

        USE DEBUG_MOD
        
C***********************************************************************

c     MERCH VARIABLES
      CHARACTER*1 COR,HTTYPE,CTYPE
      CHARACTER*2 FORST,PROD
      CHARACTER*10 VOLEQ
      INTEGER EVOD,OPT,REGN,HTFLG,FCLASS
      INTEGER CUTFLG,BFPFLG,CUPFLG,SPFLG,ERRFLAG,CDPFLG
      REAL LENGTH,DRCOB,MINBFD,MHT,slope,TOPD
      REAL MAXLEN,MINLEN,minlent,MERCHL,MTOPP,MTOPS,STUMP,TRIM,TRM
C     TREE VARIABLES
      REAL DBHOB,HTTOT,DBTBH,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL BTR,LMERCH,LENMS,HT1PRD,HT2PRD,TOP6
      INTEGER HTREF,HTLOG,I,J

C     OUTPUT VARIABLES
      REAL VOL(15),LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3)
      REAL BOLHT(21),NOLOGP,NOLOGS,LOGVOL32(20),LVOL32(2,20)
      REAL LLEN32(20),LDIA32(21,2)
      INTEGER NLCNT,LCNT

C     PROGRAM VARIABLES
      REAL LOGV,LHT,DIBL,DIBS,HT2,CUVOL,DOB
      INTEGER LOGST,NUMSEG,NEXTRA,MFLAG,MFLG,TLOGS
      REAL DIB,LOGLENG(20),BFINT,TOPV16,BOTV16,R,DIBO

C       Variables to hold flewellings coefficients
      INTEGER SETOPT(6),JSP
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL HEX(2),dex(2), ZEX(2)
C       Temp variables for Region 1


      REAL UHT,VMER,VMER1,VMER2,TEMPVOL,TCVOL,TCVOL1,TCVOL2

C***********************************************************************

      !zero out values
      MFLAG = 0       
      NOLOGP = 0.0
      NOLOGS = 0.0
      NUMSEG = 0
      JSP = 0
      ERRFLAG = 0
      HTFLG = 0
	LMERCH = 0

      DO 102,I=1,20
        DO 103, J=1,7
          LOGVOL(J,I) = 0.0
  103   CONTINUE        
        LOGDIA(I,1) = 0.0
        LOGDIA(I,2) = 0.0
        LOGDIA(I,3) = 0.0
  102 CONTINUE        

      DO 104, I=1,15
       VOL(I) = 0.0
  104 CONTINUE

C--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
      IF (DBHOB.LT.1.0 .OR. (HTTOT.LT.5 .AND. HT1PRD.LE.0)) THEN
         ERRFLAG = 3
         IF(HTTOT.GT.0)ERRFLAG = 0
         GO TO 1000
      ENDIF 
c test 1/29/14
      IF (VOLEQ(4:6).EQ.'BEH' .OR. VOLEQ(4:6).EQ.'beh') THEN
C     added DIB calculation for Behr equation
         CALL BEHTAP(VOLEQ,DBHOB,HTTOT,LMERCH,DRCOB,FCLASS,MTOPP,DIBO)  
         RETURN
      ENDIF  
c end test 1/29/14
c test r8 Clark
      IF (VOLEQ(1:1).EQ.'8'.AND.VOLEQ(3:3).NE.'1'.AND.
     &   (VOLEQ(4:6).EQ.'CLK' .OR. VOLEQ(4:6).EQ.'clk')) THEN
         CALL R8CLKDIB(VOLEQ,FORST,DBHOB,HTTOT,UPSHT1,DRCOB,DIBO, 
     &                    ERRFLAG)
         RETURN
      ENDIF  

      IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f' .OR.
     >    VOLEQ(4:6).EQ.'DEM' .OR. VOLEQ(4:6).EQ.'dem' .OR.
     >    VOLEQ(4:6).EQ.'CUR' .OR. VOLEQ(4:6).EQ.'cur')  THEN

        TOPD = MTOPP        

        IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f') THEN
C--   Initialize Flewelling model for this tree
          
          CALL FWINIT(VOLEQ,DBHOB,HTTOT,MHT,TOPD,UPSHT1,UPSHT2,UPSD1,
     >      UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,
     >      TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,
     >      ERRFLAG)

           AVGZ1 = ZEX(1)
           AVGZ2 = ZEX(2)

           IF(ERRFLAG .EQ. 1 .or. errflag.eq.6)  GO TO 1000

        ELSE
C          region 10 call to determine total height or merch height
           CALL R10HTS(VOLEQ,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,MTOPP,
     >          LMERCH)
        ENDIF

      ELSEIF (VOLEQ(4:6).EQ.'CZ3' .OR. VOLEQ(4:6).EQ.'cz3') THEN
C        initialize Czaplewski three point model
         IF(HTTOT.LE.4.5)THEN
	      ERRFLAG = 4
	      GOTO 1000
         ENDIF
         UHT = HTTOT * 0.95
         if(UPSHT1.LE.0 .or. UPSD1.LE.0) THEN 
            ERRFLAG = 9
            GO TO 1000
         endif
         if(UPSHT1.LE.4.5 .or. UPSHT1.GT.UHT) then
            ERRFLAG = 10
            GO TO 1000
         endif      
         HEX(1) = UPSHT1
         DEX(1) = UPSD1
         CALL TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,6.0,
     >                TOP6,DBTBH,errflag)
   
      ELSE
C       CHECK FOR TOTAL TREE HEIGHT
        IF(HTTOT.LE.4.5)THEN
	     ERRFLAG = 4
	     GOTO 1000
        ENDIF
        
      ENDIF      
      
      !DRCOB is passed in and is where the use whats DIB
      HT2 = DRCOB
      
      CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >           DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >           HT2,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
     
      DIBO = DIB    

1000  CONTINUE

      RETURN
      END

C##################################################################



