!     Subroutine to determine tree profile for APSS

!     Created ??? ??/??/??
!     Revised TDH 06/18/09 - created profile2 for APSS
!     YW 06/20/2014 Add logic to reset MINLEN for region 10 32 foot log equation
C     YW 07/02/2014 Added errflag to call VOLINTRP routine
C     YW 03/17/2017 Modified PROFILE2 to call PROFILE to avoid duplicated codes
      SUBROUTINE PROFILE2 (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     >   HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,
     >   AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
     >   LOGST,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,
     >   CTYPE,FCLASS,PROD,ERRFLAG, MERRULES)
     
      USE MRULES_MOD
      USE DEBUG_MOD
      USE VOLINPUT_MOD
     
!...  This is a minor modification to profile.f for use with the APSS
C--   This subroutine determines the cubic and board foot volume of a tree
C--     using species and area specific profile models developed by
C--     Jim Flewelling, Ray Czaplewski, Wensel and Olson, and John Byrne.
C--     All merchandising logic was developed by the Forest Management 
C--     Service Center in accordance with existing FS Handbook direction.

C--   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
C--     MRULES - EXTERNAL - No calls
C--     NUMLOG - EXTERNAL - No calls
C--     SCRIB -  EXTERNAL - No calls
C--     INTL14 - EXTERNAL - No calls (part of scrib.for)
C--     SEGMNT - EXTERNAL - No calls
C--------
C--     GETDIB   - INTERNAL - CALLS TAPERMODEL
C--     MERLEN   - INTERNAL - CALLS TAPERMODEL
C--     TCUBIC   - INTERNAL - CALLS TAPERMODEL
C--     TOP6LEN  - INTERNAL - CALLS TAPERMODEL
C--     VOLRATIO - INTERNAL - CALLS R1TAP
C--     TAPERMODEL - INTERNAL - CALLS PROFILE EQUATION ROUTINES
C--     FWSMALL  - INTERNAL - CALLS SF_DS, BRK_UP

C**************************************************************
C**************************************************************

c     MERCH VARIABLES
      CHARACTER*1 COR,HTTYPE,CTYPE
      CHARACTER*2 FORST,PROD
      CHARACTER*10 VOLEQ
      INTEGER EVOD,OPT,REGN,HTFLG,FCLASS, N16SEG
      INTEGER CUTFLG,BFPFLG,CUPFLG,SPFLG,ERRFLAG,CDPFLG
      REAL LENGTH,DRCOB,MINBFD,MHT,slope,TOPD
      REAL MAXLEN,MINLEN,minlent,MERCHL,MTOPP,MTOPS,STUMP,TRIM,TRM
C     TREE VARIABLES
      REAL DBHOB,HTTOT,DBTBH,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL BTR,LMERCH,LENMS,HT1PRD,HT2PRD,TOP6
      INTEGER HTREF,HTLOG,I,J
      TYPE(MERCHRULES)::MERRULES

C     OUTPUT VARIABLES
      REAL VOL(15),LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3)
      REAL BOLHT(21),NOLOGP,NOLOGS,LOGVOL32(20),LVOL32(2,20)
      REAL LLEN32(20),LDIA32(21,2)
      INTEGER NLCNT,LCNT

C     PROGRAM VARIABLES
      REAL LOGV,LHT,DIBL,DIBS,HT2,CUVOL,DOB
      INTEGER LOGST,NUMSEG,NEXTRA,MFLAG,MFLG,TLOGS
      REAL DIB,LOGLENG(20),BFINT,TOPV16,BOTV16,R

C       Variables to hold flewellings coefficients
      INTEGER SETOPT(6),JSP
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL HEX(2),dex(2), ZEX(2)
C       Temp variables for Region 1


      REAL UHT,VMER,VMER1,VMER2,TEMPVOL,TCVOL,TCVOL1,TCVOL2
C     Temp variable for BEH equation
      INTEGER ZONE
      REAL FC_HT,D17,TTH,DBHIB
!=====================================================================
		IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 2) ' -->Enter PROFILE2'
    2    FORMAT (A)   
   		END IF

      MRULEMOD = 'Y'
      NEWCOR = MERRULES%COR
      NEWEVOD = MERRULES%EVOD
      NEWOPT = MERRULES%OPT
      NEWMAXLEN = MERRULES%MAXLEN
      NEWMINLEN = MERRULES%MINLEN
      NEWMERCHL = MERRULES%MERCHL
      NEWMINLENT = MERRULES%MINLENT
      NEWMTOPP = MERRULES%MTOPP
      NEWMTOPS = MERRULES%MTOPS
      NEWSTUMP = MERRULES%STUMP
      NEWTRIM = MERRULES%TRIM
      NEWBTR = MERRULES%BTR
      NEWDBTBH = MERRULES%DBTBH
      NEWMINBFD = MERRULES%MINBFD
        CALL PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,
     +      HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,
     +      AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
     +      TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,
     +      DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)

      GOTO 1000
! The following codes are duplicated from PROFILE subroutine and will be deleted!
! ********************************************************************************      

c      LOGST = 0      ! commented out for variable log cruising
C     changed to set LOGST = 0 for NOT variable log cruising (CTYPE = V)
C     YW 05/10/2016
      IF(CTYPE.NE.'V') LOGST = 0
      ! calc secondary product    
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

!...  don't need to call mrules for apss, it is passed in as a
!...  defined type and initialized in the calling app.
      
c     MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
!      CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,
!     >         MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD)

!  this was for testing apss i think
!      WRITE  (LUDBG, 106)'  COR EVOD OPT MAXLEN MINLEN MERCHL'
!  106    FORMAT (A)
!         WRITE  (LUDBG, 107) MERRULES%COR(1:1), MERRULES%EVOD, 
!     +   MERRULES%OPT, MERRULES%MAXLEN, MERRULES%MINLEN, MERRULES%MERCHL
!  107    FORMAT (2X, A, 3X, I2, 3X, I3,3X, F7.1, F7.1, F7.1)


      COR = MERRULES%COR
      EVOD = MERRULES%EVOD
      OPT = MERRULES%OPT
      MAXLEN = MERRULES%MAXLEN
      MINLEN = MERRULES%MINLEN
      MERCHL = MERRULES%MERCHL
      MINLENT = MERRULES%MINLENT
      MTOPP = MERRULES%MTOPP
      MTOPS = MERRULES%MTOPS
      STUMP = MERRULES%STUMP
      TRIM = MERRULES%TRIM
      BTR = MERRULES%BTR
      DBTBH = MERRULES%DBTBH
      MINBFD = MERRULES%MINBFD
C     Add 1.5 to HTTOT to make it same as it in blmvol (YW 09/17/13)
      IF (VOLEQ(1:1).EQ.'B' .OR. VOLEQ(1:1).EQ.'b') THEN
        HTTOT=HTTOT+1.5
      ELSEIF (VOLEQ(1:1).EQ.'6') THEN
        TTH=HTTOT
        HTTOT=HTTOT+1.0
      ENDIF
 
      IF(FCLASS .LE. 0 .AND. VOLEQ(4:6).EQ.'BEH') THEN
            ERRFLAG = 2
            RETURN
      ENDIF
      		
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 108)'  COR EVOD OPT MAXLEN MINLEN MERCHL'
  108    FORMAT (A)
         WRITE  (LUDBG, 110) COR, EVOD, OPT, MAXLEN, MINLEN, MERCHL
  110    FORMAT (2X, A, 3X, I2, 3X, I3,3X, F7.1, F7.1, F7.1)
   		END IF

      IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f' .OR.
     >    VOLEQ(4:6).EQ.'DEM' .OR. VOLEQ(4:6).EQ.'dem' .OR.
     >    VOLEQ(4:6).EQ.'CUR' .OR. VOLEQ(4:6).EQ.'cur')  THEN

        TOPD = MTOPP
        IF(HTTOT.LE.0) THEN
	! no total height
           HTFLG = 1  
	!do not calc secondary product    
           IF(HT2PRD.LE.0) MFLAG = 1    
           IF(HTTYPE.EQ.'l' .or. HTTYPE.EQ.'L') THEN
C--           CHECK FOR VARIABLE LOG CRUISE
              IF(CTYPE.EQ.'V' .AND. LOGST.GT.0) then
                ! SUM UP LOG LENGTHS
                 MHT = 0
                 DO 120, I=1,LOGST
                    MHT = MHT + LOGLEN(I)
                    IF(LOGLEN(I) .GE. 12) MHT = MHT + 0.5 
  120            CONTINUE
              ELSE    
                 !logs are in 10s of logs
                 LHT = HT1PRD/10.0
                 IF(LHT.LT.1.0 .AND. LHT.NE.0.5) THEN
                    ERRFLAG = 7
                    GOTO 1000
                 ENDIF
                 IF(HTLOG .LE. 0)THEN
	              IF(VOLEQ(1:3).EQ.'A32' .OR. VOLEQ(1:3).EQ.'a32')THEN
	                  HTLOG = 32
	              ELSE
	                  HTLOG = 16
	              ENDIF
                 ENDIF
	           IF(HTLOG .GE. 32) THEN
                    TRM = TRIM * 2
	           ELSE
	              TRM = TRIM
	           ENDIF
                 MHT = (LHT * (HTLOG + TRM)) + STUMP
              ENDIF
           ELSE
              if(ht2prd .gt. 0)then
	          mht = ht2prd + stump
	          topd = mtops
     	          if(ht1prd .gt. 0) then
	             UPSHT1 = ht1prd + stump
	             UPSD1 = MTOPP
            	 else
	             UPSHT1 = 0
	          endif

	        else
                MHT = HT1PRD + STUMP
	          topd = mtopp
	        endif
           ENDIF
        endif

        IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f') THEN
C--   Initialize Flewelling model for this tree
          
          CALL FWINIT(VOLEQ,DBHOB,HTTOT,MHT,TOPD,UPSHT1,UPSHT2,UPSD1,
     >      UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,
     >      TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,
     >      ERRFLAG)

           AVGZ1 = ZEX(1)
           AVGZ2 = ZEX(2)

          IF(ERRFLAG .EQ. 1 .or. errflag.eq.6)  GO TO 1000
	  
          IF(ERRFLAG .EQ. 11)THEN
C         USE ITERPOLATION ROUTINE 11/02
	      CALL VOLINTRP(REGN,VOLEQ,DBHOB,LHT,MHT,MTOPP,HTTYPE,DBTBH,
     >           LOGVOL,LOGDIA,HTLOG,LOGLEN,LOGST,NOLOGP,VOL,CTYPE,PROD,
     >           ERRFLAG)
            GO TO 1000  
          ENDIF
        ELSE
C          region 10 call to determine total height or merch height
           CALL R10HTS(VOLEQ,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,MTOPP,
     >          LMERCH)

C           IF(HT1PRD.LE.0) HT1PRD = LMERCH + STUMP
        
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

C*************************************************************
C*************************************************************
C--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *
C*************************************************************

C--   SUBROUTINE "TCUBIC" IS INTERNAL AND USES PROFILE MODEL
      IF (CUTFLG .EQ. 1) THEN
C     For Behre equation 616BEH***, 628BEH***, 632BEH*** use R6VOL3 to calculate total cubic
        IF (VOLEQ(1:1).EQ.'6')THEN
           IF(VOLEQ(1:3).EQ.'632')THEN
             ZONE=2
             FC_HT=33.6
           ELSE
             ZONE=1
             FC_HT=16.3
           ENDIF
           DBHIB = DBHOB-DBTBH
           IF(TTH.GT.0) THEN 
              D17 = FCLASS/100.0*DBHOB     
              IF(TTH.LE.FC_HT) THEN  
C                use smailians to find total cubic volume
                 VOL(1)=0.00272708*(DBHIB*DBHIB)*TTH
              ELSE
c                call the total cubic routines
                 CALL R6VOL3(DBHOB,DBTBH,FCLASS,TTH,ZONE,VOL)
              ENDIF
           ENDIF                                  
        ELSE        
        CALL TCUBIC (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,
     >           MTOPP,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >           TCVOL,slope,errflag,VOL,MTOPS)
        VOL(1) = NINT(TCVOL * 10.0) * 1E-1       
	  if(drcob.le.0 .and. ctype.eq.'F')then
	      drcob = dex(2)
	  endif
	  ENDIF
      ENDIF

C--  ******************************************************
      IF(BFPFLG.EQ.1 .OR. CUPFLG.EQ.1) THEN

C--   MERCHANDISE THE TREE BOLE FOR PRIMARY PRODUCT
C--      CHECK FOR VARIABLE LOG CRUISE
        IF(CTYPE .EQ. 'V') THEN
          NUMSEG = LOGST
          LOGST = 0
          NOLOGP = NUMSEG
        ELSE
          do 150, i=1,20
              loglen(i) = 0
  150     continue
          IF(HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
              LMERCH = MHT - STUMP
          ELSE IF (LMERCH .LE. 0) THEN
              LOGST = 0
               !MERCH HEIGHT ENTERED
              IF(HT1PRD.GT.0) THEN  
                 LMERCH = HT1PRD - STUMP
	            ! TOTAL HEIGHT ENTERED, FIND TOP DIAMETER
                 IF(HTFLG.EQ.0) THEN  
                   CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,
     >              HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >              PINV_Z,TOP6,HT1PRD,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
                  
                     MTOPP = DIB
	           ENDIF
	            !NO MERCH HT, TOTAL HEIGHT
              ELSE IF (HT1PRD.EQ.0 .AND. HTFLG.EQ.0) THEN  
C--           SUBROUTINE "MERLEN" IS INTERNAL AND USES PROFILE MODEL
C--           LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO SPECIFIED TOP, INCLUDING TRIM.
                CALL MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >            DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >            STUMP,MTOPP,LMERCH,errflag)

                   HT1PRD = LMERCH + STUMP
              ENDIF
          ENDIF

C--   CHECK FOR REGION 5 FIREWOOD LOGIC
          IF((REGN.EQ.5 .OR. REGN.EQ.3) .AND. PROD.EQ.'07') THEN
            CALL FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >           LMERCH,DBTBH,MTOPP,STUMP,HEX,DEX,ZEX,RHFW,RFLW,
     >           TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)
	      VOL(4) = NINT(TCVOL * 10.0) * 1E-1
	      GO TO 500
	    ENDIF

C--    CHECK FOR A MINIMUM MERCH STEM LENGTH - IF NOT MERCH DO NOT
C--            CACULATE PRODUCT VOLUMES
           IF (LMERCH.LT.MERCHL) THEN
              GO TO 500
           ENDIF

C--   For Region 10 32 foot log equation, it needs to reset MINLEN in some 
C--   situation in order to NOT miss 18, 20, 22 foot logs. (YW 06/20/2014)
          IF((voleq(4:5).eq.'f3' .or. voleq(4:5).eq.'F3' .or. 
     >        voleq(2:3).eq.'61' .or. voleq(2:3).eq.'62'.OR. 
     >        voleq(2:3).eq.'32') .AND. CTYPE.NE.'V') THEN
              N16SEG = INT(LMERCH/(MAXLEN+TRIM))
              IF(MOD(N16SEG,2).EQ.1) MINLEN = 2
          ENDIF

C--         SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
C--         MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
C--         OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
C--         RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.

           CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)

C--         SUBROUTINE "SEGMNT" WILL DETERMINE THE LENGTH OF EACH
C--         SEGMENT, GIVEN A MERCHANTABLE LENGTH OF TREE STEM AND
C--         THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
C--         "NUMLOG").  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
C--         ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
C--         ESTIMATOR HANDBOOK FSH ???.
C--         NOTE - THE VARIABLE LMERCH WHEN PASSED IS THE TOTAL
C--           MERCH LENGTH, BUT IS RETURNED AS THE MERCH LENGTH
C--           WITH TRIM AND UNUSABLE PORTION OF THE TOP SUBTRACTED.

           CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,
     >               LOGLEN)

C--         SET NOLOGP TO THE NUMBER OF 16 FOOT SEGMENTS
c           NOLOGP = LMERCH/16.0
            NOLOGP = NUMSEG
         ! ENDIF FOR VARIABLE LOG LENGTH CHECK
        ENDIF     

C--     SUBROUTINE "GETDIB" IS INTERNAL AND USES PROFILE MODEL
        CALL GETDIB(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,MTOPP,
     >      STUMP,TRIM,DBTBH,LOGST,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >      PINV_Z,TOP6,NUMSEG,LOGLEN,LOGDIA,BOLHT,errflag)

C--     TREE BOLE HAS BEEN MERCHANDISED FOR PRIMARY PRODUCT
      ENDIF

C
C**********************************************
C**********************************************
C     GET BOARD FOOT PRODUCT VOLUMES          *
C**********************************************

C--   DETERMINE IF A BOARD FOOT PRODUCT REPRESTNTATION IS NEEDED

      IF(BFPFLG.EQ.1 .and. DBHOB.GE.MINBFD) THEN

        DO 250 I=1,NUMSEG
           DIB=LOGDIA(I+1,1)
           LENGTH=LOGLEN(I)
c removed scribner check for small diameter logs Jan, 98
           CALL SCRIB (DIB,LENGTH,COR,LOGV)

           if(cor.eq.'Y') then
              LOGVOL(1,I) = LOGV * 10
           else
              LOGVOL(1,I) = ANINT(LOGV)
           endif
           VOL(2) = VOL(2) + LOGVOL(1,I)

C          WRITE(*,*)" 16FT ",NUMSEG,I,LOGV
C-- GET INTERNATIONAL 1/4 VOLUME FOR THE PIECES 

           CALL INTL14(DIB,LENGTH,BFINT)
           VOL(10) = VOL(10) + BFINT
           LOGVOL(7,I) = BFINT
           
C-- END OF INTERNATIONAL 1/4 SECTION

 250    CONTINUE

C-- CONVERT FROM SCRIBNER DECIMAL C TO SCRIBNER
c      if 32 foot volume is asked for do the following
c****************************************************************
c     find volume for 32 foot logs (Flewelling & Demars equation only)

      IF((voleq(4:5).eq.'f3' .or. voleq(4:5).eq.'F3' .or. 
     >    voleq(2:3).eq.'61' .or. voleq(2:3).eq.'62'.OR. 
     >    voleq(2:3).eq.'32') .AND. CTYPE.NE.'V') THEN
         LCNT = 0
         DO 260 I = 2, NUMSEG,2
c           truncate diameter for 32foot board 6/2001
            LCNT = LCNT + 1
            DIB = INT (LOGDIA(I+1,2))
            LENGTH = LOGLEN(I) + loglen(I-1)
            CALL SCRIB (DIB,LENGTH,COR,LOGV)
            LOGVOL32(LCNT) = LOGV
	      LVOL32(1,LCNT) = LOGV*10
	      LDIA32(LCNT,1)= DIB
	      LDIA32(LCNT,2) = LOGDIA(I+1,2)
	      LLEN32(LCNT) = LENGTH
  260    CONTINUE
C    check for a 16 foot top log
	   IF(((NUMSEG+1)/2) .GT. (NUMSEG/2)) THEN
            LCNT = LCNT + 1
            DIB = INT(LOGDIA(NUMSEG+1,2))
            LENGTH = LOGLEN(NUMSEG)
            IF(REGN.EQ.7 .AND. LENGTH.LT.16) THEN
	         LOGV = 0
	      ELSE
               CALL SCRIB (DIB,LENGTH,COR,LOGV)
	      ENDIF
            LOGVOL32(LCNT) = LOGV
	      LVOL32(1,LCNT) = LOGV*10
	      LDIA32(LCNT,1)= DIB
	      LDIA32(LCNT,2) = LOGDIA(NUMSEG+1,2)
	      LLEN32(LCNT) = LENGTH
         ENDIF	      
            
         IF(REGN.EQ.10 .AND. HTLOG.EQ.32)THEN
	      DO 263 I=1,NUMSEG
	         LOGVOL(1,I) = 0
  263 	   CONTINUE
            NLCNT = (NUMSEG + 1) / 2
	      DO 264 I=1,NLCNT
	         LOGVOL(1,I) = LVOL32(1,I)
  264       CONTINUE
	   ELSE
c    prorate 32 volumes into 16 foot pieces
            LCNT = 0
             DO 261, i=2,numseg,2
	         LCNT = LCNT + 1
  	         if(regn .eq. 7) then
                  logvol(1,I-1) = ANINT(logvol32(lcnt)/2.0)
                  logvol(1,I) = logvol32(lcnt) - logvol(1,i-1) 
	         else
                 topv16 = logvol(1,i)
                 botv16 = logvol(1,i-1)
                 R = TOPV16 / (TOPV16 + BOTV16)
	           if(R .EQ. 0.5)THEN
                   logvol(1,I) = INT(logvol32(LCNT) * R)
	           else
                   logvol(1,I) = ANINT(logvol32(LCNT) * R)
	           endif
      	         IF(LOGVOL(1,I) .LE. 0) LOGVOL(1,I) = 1     
                 logvol(1,I-1) = ANINT(logvol32(LCNT) - logvol(1,i)) 
                 IF(cor.eq.'Y') THEN
                   logvol(1,I-1) = logvol(1,I-1) * 10
                   logvol(1,I) = logvol(1,I) * 10 
                 ENDIF
	         endif
  261        CONTINUE
c     check for top log
  	       IF(((NUMSEG+1)/2) .GT. (NUMSEG/2)) THEN
	         LCNT = LCNT + 1
               LOGVOL(1,NUMSEG) = LOGVOL32(LCNT)
               IF(cor.eq.'Y') LOGVOL(1,NUMSEG) = LOGVOL(1,NUMSEG)*10
             ENDIF
         ENDIF
         VOL(2) = 0
         DO 265, i = 1,numseg
            VOL(2) = VOL(2) + LOGVOL(1,I)
C            WRITE(*,*)" 32FT ",NUMSEG,I,LOGVOL(1,I)
  265    CONTINUE
      ENDIF
c************** 32 foot logs endif ******************************

C***************************************************************

C----- REGION 1 BYRNE BOARD FOOT LOGIC (bdft to cuft rato)
        IF(VOLEQ(4:4) .EQ. 'J' .OR. VOLEQ(4:4) .EQ. 'j') THEN
           IF (VOLEQ(8:10) .NE. '108') THEN
              CALL VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPP,LOGDIA,
     >                              LOGLEN,NUMSEG,TCVOL,VMER)
              TEMPVOL = VOL(2)
              VOL(2) = VMER*(TEMPVOL/TCVOL)
              TEMPVOL = VOL(10)
              VOL(10) = VMER*(TEMPVOL/TCVOL)
           ENDIF
        ENDIF

        IF (VOL(10) .LT. 0.0) THEN
           VOL(10) = 0.0
        ENDIF
        IF (VOL(2) .LT. 0.0) THEN
           VOL(2) = 0.0
        ENDIF
C--   (ENDIF FOR BOARD FOOT EQUATIONS)
      ENDIF
C-------END OF BOARD FOOT SECTION            

C**************************************************************
C*      CUBIC FOOT MAIN STEM EQUATIONS  ALL TREES             *
C**************************************************************
C--   DETERMINE IF A CUBIC FOOT PRODUCT REPRESTNTATION IS NEEDED
      IF(CUPFLG .EQ. 1 ) THEN

C--   USE DIB AT DBHOB FOR LARGE END BUTT LOG
        DIBL=LOGDIA(1,1)
        DO 350 I=1,NUMSEG
          DIBS=LOGDIA(I+1,1)
          LENGTH=LOGLEN(I)
          LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
          
          LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0
          
          VOL(4)=VOL(4) + LOGVOL(4,I)
          
          DIBL = DIBS
  350   CONTINUE

C     CHECK FOR 32 FOOT LOGS
        IF(REGN.EQ.10 .AND. HTLOG.EQ.32 .and. (voleq(4:5).eq.'f3' .or. 
     >      voleq(4:5).eq.'F3' .or. voleq(2:3).eq.'61' .or. 
     >      voleq(2:3).eq.'62' .OR. voleq(2:3).eq.'32'))THEN
          LCNT = 0
          DO 351 I = 2, NUMSEG,2
            LCNT = LCNT + 1
	      LVOL32(2,LCNT) = LOGVOL(4,I) + LOGVOL(4,I-1)
C           if boardfoot flag is zero, need to compute the following
            IF(BFPFLG.EQ.0)THEN
               DIB = INT (LOGDIA(I+1,2))
               LENGTH = LOGLEN(I) + loglen(I-1)
	         LDIA32(LCNT,1)= DIB
	         LDIA32(LCNT,2) = LOGDIA(I+1,2)
	         LLEN32(LCNT) = LENGTH
	      ENDIF
  351     CONTINUE
C    check for a 16 foot top log
	    IF(((NUMSEG+1)/2) .GT. (NUMSEG/2)) THEN
            LCNT = LCNT + 1
	      LVOL32(2,LCNT) = LOGVOL(4,NUMSEG)
            IF(BFPFLG.EQ.0)THEN
      	      LDIA32(LCNT,1)= INT(LOGDIA(NUMSEG+1,1))
	         LDIA32(LCNT,2) = LOGDIA(NUMSEG+1,2)
	         LLEN32(LCNT) = LOGLEN(NUMSEG)
	      ENDIF
          ENDIF	      
        ENDIF    

C----- REGION 1 BYRNE CUBIC FOOT LOGIC
        IF(VOLEQ(4:4) .EQ. 'J' .OR. VOLEQ(4:4) .EQ. 'j') THEN
           IF (VOLEQ(8:10) .NE. '108') THEN
              MFLG = 1
              CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >           DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >           HT2,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

              VOL(4) = CUVOL
           ENDIF
        ENDIF
        IF (VOL(4) .LT. 0.0) THEN
               VOL(4) = 0.0
        ENDIF
C--  (ENDIF FOR CUBIC FOOT MAIN STEM EQUATIONS)
      ENDIF

C*********************************************
C     RETURN 32 FOOT LOGS
      IF(REGN.EQ.10 .AND. HTLOG.EQ.32 .and. (voleq(4:5).eq.'f3' .or. 
     >      voleq(4:5).eq.'F3' .or. voleq(2:3).eq.'61' .or. 
     >      voleq(2:3).eq.'62' .OR. voleq(2:3).eq.'32'))THEN
      	DO 352 I = 1,NUMSEG
	      LOGVOL(1,I) = 0
      	   LOGVOL(4,I) = 0
	      LOGDIA(I+1,1) = 0
      	   LOGDIA(I+1,2) = 0
	      LOGDIA(I+1,3) = 0
      	   LOGLEN(I) = 0
  352	   CONTINUE
         NLCNT = (NUMSEG + 1) / 2
         DO 353 J=1,NLCNT
            LOGVOL(1,J) = LVOL32(1,J)
	      LOGVOL(4,J) = LVOL32(2,J)
            LOGLEN(J) = LLEN32(J)
            LOGDIA(J+1,1) = LDIA32(J,1)
     	      LOGDIA(J+1,2) = LDIA32(J,2)
  353   CONTINUE
        NUMSEG = NLCNT
        NOLOGP = NLCNT
      ENDIF
C**************************************************************
C*      CORD WOOD MAIN STEM EQUATIONS  ALL TREES             *
C**************************************************************

      IF (CDPFLG .EQ. 1) THEN
C--   PUT LOGIC HERE FOR CORDS  (SOURCE:  DON MARTINEZ)
        VOL(6) = ANINT((VOL(4)/90.0)*10)/10.0
      ENDIF

C**************************************************************
C--   SET LOGST TO NUMBER OF SEGMENTS MAIN STEM

      LOGST = NUMSEG

C--   SET COMMON VARIABLE LOGS TO LOGST TO BE USED IN LOG STOCK FILE
C      LOGS = LOGST

C****************************************************************
C****************************************************************
C--  DETERMINE SECONDARY PRODUCTS (TOP WOOD) ALL TREES          *
C****************************************************************

C--  ******************************************************
C--   MERCHANDISE THE UPPER TREE BOLE FOR SECONDARY PRODUCTS

 500  IF (SPFLG.EQ.1 .AND. MFLAG.EQ.0) THEN

C--     LENMS IS THE LENGTH OF MAIN STEM FOR WHICH A
C--        PRODUCT WAS DETERMINED.
        IF(REGN.EQ.5 .AND. PROD.EQ.'07') THEN
	    LENMS = LMERCH
	  ELSE
          LENMS = 0.0
          DO 400 I=1,NUMSEG
	      !32 FOOT LOGS CONTAIN TWICE THE TRIM
	      IF(LOGLEN(I) .GE. 32)THEN  
	         LENMS = LENMS + TRIM + TRIM + LOGLEN(I)
	      ELSE
               LENMS=LENMS+TRIM+LOGLEN(I)
	      ENDIF
  400     CONTINUE
        ENDIF
        
        IF(HT2PRD.GT.0)THEN
          IF(HT2PRD .LT. LENMS)THEN
            VOL(7) = 0
            GOTO 1000
          ENDIF
          IF(HTFLG.EQ.0) THEN  
              CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,
     >          HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >          PINV_Z,TOP6,HT2PRD,MTOPS,MFLG,CUVOL,DIB,DOB,errflag)
                  
              MTOPS = DIB
	    ENDIF
          LMERCH = HT2PRD - LENMS 
        ELSE
C--     SUBROUTINE "MERLEN" IS INTERNAL AND USES PROFILE MODEL
           CALL MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,
     >       HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,STUMP,
     >       MTOPS,LMERCH,errflag)     

C--     LMERCH IS CHANGED TO BE THE MERCH LENGTH OF THE
C--       TOP WOOD PORTION ONLY.  THERE MIGHT BE A SMALL
C--       DIFFERENCE IN TOP WOOD LENGTH DEPENDING ON THE
C--       SEGMENTATION LOGIC USED FOR THE MAIN STEM.
C--   CHECK FOR REGION 5 FIREWOOD LOGIC

           LMERCH = LMERCH - LENMS
C--  SEE DEFINITION OF SUBROUTINE "NUMLOG" UNDER BDFT CACULATIONS

        ENDIF

        IF(REGN.EQ.5 .AND. PROD.EQ.'07') THEN
          CALL FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >         LMERCH,DBTBH,MTOPS,LENMS,HEX,DEX,ZEX,RHFW,RFLW,
     >         TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)
	    VOL(7) = NINT(TCVOL * 10.0) * 1E-1
	    GO TO 1000
        ENDIF

c       check for mainstem piece, if not, topwood must meet minimum merch length
        IF(LENMS.LE.0)THEN
           IF (LMERCH.LT.MERCHL) THEN
              GO TO 1000
           ENDIF
	  ENDIF

        CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG)

C--  SEE DEFINITION OF SUBROUTINE "SEGMNT" UNDER BDFT CACULATIONS

        CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,LOGLENG)
        DO 50 I = 1, NUMSEG
           LOGLEN(I+LOGST) = LOGLENG (I)
 50     CONTINUE

C--  SET NOLOGS TO NUMBER OF 16 FOOT PIECES

        NOLOGS=LMERCH/16.0

C--   SUBROUTINE "GETDIB" IS INTERNAL AND USES PROFILE MODEL

        LENMS = LENMS + STUMP
        CALL GETDIB(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,MTOPS,
     >      LENMS,TRIM,DBTBH,LOGST,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >      PINV_Z,TOP6,NUMSEG,LOGLEN,LOGDIA,BOLHT,errflag)

C--  UPPER TREE BOLE HAS BE MERCHANDISED FOR SECONDARY PRODUCT
C--  ******************************************************

C************************************************************
C       GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA     *
C************************************************************

         DIBL=LOGDIA(1 + LOGST,1)

         DO 450 I = 1 + LOGST, NUMSEG + LOGST
            DIBS = LOGDIA(I+1,1)
            LENGTH = LOGLEN(I)
            IF(CUPFLG.EQ.1) THEN
               LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
           
               LOGVOL(4,I)=ANINT(LOGV*10.0)/10.0
           
               VOL(7)=VOL(7)+LOGVOL(4,I)
	      ENDIF
            IF(BFPFLG.EQ.1)THEN
               CALL SCRIB (DIBS,LENGTH,COR,LOGV)
               if(cor.eq.'Y') then
                 LOGVOL(1,I) = LOGV * 10
               else
                 LOGVOL(1,I) = ANINT(LOGV)
               endif
               VOL(12) = VOL(12) + LOGVOL(1,I)
	      ENDIF
C           LOGGRS(I,3) = LOGV
            DIBL = DIBS
  450   CONTINUE
C----- REGION 1 BYRNE CUBIC FOOT LOGIC
        IF(VOLEQ(4:4).EQ.'J' .OR. VOLEQ(4:4).EQ.'j') THEN
           IF(CUPFLG.EQ.1)THEN
              IF (VOLEQ(8:10) .NE. '108') THEN
                 MFLG = 1
                 CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,
     >              HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >              PINV_Z,TOP6,HT2,MTOPS,MFLG,CUVOL,DIB,DOB,errflag)

                 VOL(7) = CUVOL - VOL(4)
              ENDIF
	     ENDIF
	     IF(BFPFLG.EQ.1)THEN
C--            REGION 1 BYRNE BOARD FOOT LOGIC (bdft to cuft rato)
              IF (VOLEQ(8:10) .NE. '108') THEN
                 CALL VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPP,LOGDIA,
     >                          LOGLEN,LOGST,TCVOL1,VMER1)
	           TLOGS = LOGST + NUMSEG
                 CALL VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPS,LOGDIA,
     >                          LOGLEN,TLOGS,TCVOL2,VMER2)
	           VMER = VMER2 - VMER1
	           TCVOL = TCVOL2 - TCVOL1
	           TCVOL = NINT(TCVOL * 10.0) * 1E-1    
                 TEMPVOL = VOL(12)
                 VOL(12) = VMER*(TEMPVOL/TCVOL)
              ENDIF
           ENDIF
        ENDIF
          
C***************************************************************

        IF (VOL(7) .LT. 0.0) VOL(7) = 0.0
        IF (VOL(12).LT. 0.0) VOL(12) = 0.0
C--   PUT LOGIC HERE FOR CORDS  (SOURCE:  DON MARTINEZ)
        IF(CDPFLG.EQ.1 .AND. CUPFLG.EQ.1) THEN
            VOL(9) = ANINT((VOL(7)/90.0)*10)/10.0
        ENDIF
C     UPDATE TOTAL LOGS        
        LOGST = LOGST + NUMSEG  
	!ENDIF FOR TOP WOOD EQUATIONS
      ENDIF    

1000  CONTINUE

		IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 20) ' <--Exit PROFILE2'
   20    FORMAT (A)   
   		END IF

      MERRULES%MTOPS = MTOPS

      RETURN
      END SUBROUTINE PROFILE2

C##################################################################