C----------
C VOLUME $Id$
C----------
      SUBROUTINE PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     >   HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,
     >   AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
     >   LOGST,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,
     >   CTYPE,FCLASS,PROD,ERRFLAG)

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

        USE DEBUG_MOD
        
!REV  Revised TDH 03/08/10
!REV  See notes in GETDIB subroutine below
C YW 11/06/2012 Changed the errflag to 12 for NUMSEG > 20
C YW 01/18/2013 Added calculation for stump VOL(14) and tip VOL(15)
C YW 03/25/2014 added PROD to call MRULES
C YW 06/20/2014 added logic for region 10 32-foot log equation to reset MINLEN 
C               to correct the missing 18, 20, and 22 foot logs
C YW 07/02/2014 added errflag (13) for top diameter greater than DBH in VOLINTRP routine
C YW 08/19/2015 Added VOL to SUBROUTINE TCUBIC and modified TCUBIC to calculate stump and tip vol
C               and save value in VOL(14) and VOL(15)
C YW 10/15/2015 Modified TAPERMODEL for call R1tap to calc vol.
C**************************************************************
C**************************************************************

c     MERCH VARIABLES
      CHARACTER*1 COR,HTTYPE,CTYPE
      CHARACTER*2 FORST,PROD
      CHARACTER*10 VOLEQ
      INTEGER EVOD,OPT,REGN,HTFLG,FCLASS,N16SEG
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
      REAL DIB,LOGLENG(20),BFINT,TOPV16,BOTV16,R

C       Variables to hold flewellings coefficients
      INTEGER SETOPT(6),JSP
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL HEX(2),dex(2), ZEX(2)
C       Temp variables for Region 1

      REAL UHT,VMER,VMER1,VMER2,TEMPVOL,TCVOL,TCVOL1,TCVOL2

c      LOGST = 0      ! commented out for variable log cruising
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
      
      
c     MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
      CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,
     >           MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >           PROD)
      
      IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f' .OR.
     >    VOLEQ(4:6).EQ.'DEM' .OR. VOLEQ(4:6).EQ.'dem' .OR.
     >    VOLEQ(4:6).EQ.'CUR' .OR. VOLEQ(4:6).EQ.'cur')  THEN
       
        TOPD = MTOPP
        MHT = 0
        IF(HTTOT.LE.0) THEN
C  no total height
           HTFLG = 1  
C  do not calc secondary product    
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
        CALL TCUBIC (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,
     >           MTOPP,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >           TCVOL,slope,errflag,VOL,MTOPS)
        VOL(1) = NINT(TCVOL * 10.0) * 1E-1       
        if(drcob.le.0 .and. ctype.eq.'F')then
          drcob = dex(2)
        endif
      ENDIF

      IF (DEBUG%MODEL) THEN
        WRITE  (LUDBG, 125)'Profile - VOL(1)=', VOL(1)
  125   FORMAT (A, 2x, F8.4)
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
C                TOTAL HEIGHT ENTERED, FIND TOP DIAMETER
                 IF(HTFLG.EQ.0) THEN  
                   CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,
     >              HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >              PINV_Z,TOP6,HT1PRD,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
                  
                     MTOPP = DIB
              ENDIF
C             NO MERCH HT, TOTAL HEIGHT
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
            VOL(4) = TCVOL
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
     >         voleq(2:3).eq.'61' .or. voleq(2:3).eq.'62'.OR. 
     >         voleq(2:3).eq.'32') .AND. CTYPE.NE.'V') THEN
               N16SEG = INT(LMERCH/(MAXLEN+TRIM))
               IF(MOD(N16SEG,2).EQ.1) MINLEN = 2
           ENDIF

C--         SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
C--         MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
C--         OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
C--         RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.
           
           CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)
           
           if(NUMSEG .GT. 20) THEN
C               ERRFLAG = 4
C    Changed ERRFLAG to 12 (YW 11/06/2012)
               ERRFLAG = 12
               DO 151, I=1,15
                   VOL(I) = 0.0
  151          CONTINUE
               RETURN
           ENDIF
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
  263      CONTINUE
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
  352     CONTINUE
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
C         32 FOOT LOGS CONTAIN TWICE THE TRIM
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
          VOL(7) = TCVOL
          GO TO 1000
        ENDIF

c       check for mainstem piece, if not, topwood must meet minimum merch length
        IF(LENMS.LE.0)THEN
           IF (LMERCH.LT.MERCHL) THEN
              GO TO 1000
           ENDIF
        ENDIF

        CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG)

        if((NUMSEG + LOGST) .GT. 20) THEN
           DO 401, I=1,15
              VOL(I) = 0.0
  401      CONTINUE
c           ERRFLAG = 4
c changed errflag to 12 (YW 11/06/2012)
           ERRFLAG = 12
           RETURN
        ENDIF

C--  SEE DEFINITION OF SUBROUTINE "SEGMNT" UNDER BDFT CACULATIONS

        CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,LOGLENG)
        DO 405 I = 1, NUMSEG
           LOGLEN(I+LOGST) = LOGLENG (I)
 405    CONTINUE

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
C     ENDIF FOR TOP WOOD EQUATIONS
      ENDIF    

1000  CONTINUE
      
      RETURN
      END

C##################################################################
C##################################################################
C   THE LOGIC BETWEEN THE DOUBLE ROWS OF "#" MUST BE DUPLICATED
C   IN EACH REGIONAL VOLUME ROUTINE THAT USES FLEWELLING'S PROFILE
C   MODELS.  ALL CALLS ARE PRECEDED BY A SHORT ROW OF "#".
C     THERE ARE FOUR INTERNAL SUBROUTINES IN THIS SECTION.
C     THESE INTERNAL SUBROUTINES CONTAIN ALL OF THE CALLS TO
C     FLEWELLINGS STEM PROFILE ROUTINES.
C     SUBROUTINE "TCUBIC" WILL COMPUTE TOTAL CUBIC VOLUME.
C     SUBROUTINE "MERLEN" WILL FIND THE MERCHANTABLE LENGTH TO
C         A SPECIFIED TOP.
C     SUBROUTINE "GETDIB" WILL RETURN THE END DIAMETERS OF LOGS
C         ONCE SEGMENTATION HAS OCCURED.


C**************************************************************
C**************************************************************
      SUBROUTINE TCUBIC(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,
     >     MTOPP,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,
     >     slope,errflag, VOL, MTOPS) 
C**************************************************************

C--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
C--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
C--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
C--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
C--   TO THE TIP.

C--   VARIABLES OF INTEREST ARE:
C--   HTTOT - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
C--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
C--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

      character*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER HTLOOP,JSP,NEXTRA,SETOPT(6),errflag,I,MFLG
      REAL D2OLD, HTTOT, HT2, TCVOL, VOLTMP,MTOPP,DBTBH
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL DBHOB, HEX(2), ZEX(2),TOP6,DEX(2),slope
      REAL DIB,DOB,R,CUVOL
      REAL TIPV, TIPL, TIPTMP, VOL(15)
      REAL MTOPS, TIPD
      
      TIPD = MTOPS
      IF(TIPD.GT.4.0 .OR. TIPD.LE.0.0) TIPD = 4.0
      TIPV = 0.0
      TIPTMP = 0.0                                              
C      geosub=voleq(2:3)
       HTLOOP = INT((HTTOT + .5 - 1.0)/4.0)
       HT2=1.0
C##########
      MFLG = 0
      IF(JSP.GT.0 .AND. HTTOT.LE.15)THEN
C                  SMALL TREE FIX FOR FLEWELLING STUMP DIAMETERS
        CALL FWSMALL(JSP,NEXTRA,SETOPT,slope,DBHOB,HTTOT,DBTBH,
     >       HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB,errflag) 
      ELSE
      
        CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >    DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >    MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
      
        IF(VOLEQ(4:6).EQ.'JB2' .OR. VOLEQ(4:6).EQ.'jb2') THEN
          IF(VOLEQ(8:10) .EQ. '108') THEN
             TCVOL = 0.005454154 * CUVOL * DBHOB * DBHOB * HTTOT
          ELSE
             TCVOL = CUVOL
          ENDIF
          RETURN
        ENDIF
      ENDIF     
C--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
      R=DIB/2.0
      TCVOL = (3.1416*R*R)/144.0
C     Save stump vol to VOL(14) (YW 2015/08/19)
      VOL(14) = TCVOL      
      dex(2) = dib
      DO 10 I = 1,HTLOOP
         D2OLD = DIB
         HT2=HT2+4.0
C##########
         CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >     DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >     MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
c       WRITE(*,*) DBHOB, HTTOT, HT2, DIB

C--     USE SMALIAN FOR NON STUMP SEGMENTS
         VOLTMP = .00272708*(D2OLD*D2OLD+DIB*DIB)*4.0
C       Check if DIB < 4 for tip volume
         IF(D2OLD.GT.TIPD .AND. DIB.LT.TIPD)THEN
           TIPL = 4.0-(D2OLD*D2OLD-TIPD*TIPD)/(D2OLD*D2OLD-DIB*DIB)*4.0
           TIPTMP = .00272708*(TIPD*TIPD+DIB*DIB)*TIPL
         ELSEIF(D2OLD.LE.TIPD)THEN
           TIPTMP = VOLTMP
         ENDIF   
         TCVOL = TCVOL + VOLTMP
         TIPV = TIPV + TIPTMP
   10 CONTINUE
c--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
      IF ( (HTTOT-HT2) .GT. 0.0 ) THEN
         VOLTMP = .00272708*(DIB*DIB)*(HTTOT-HT2)
         TCVOL = TCVOL + VOLTMP
         TIPV = TIPV + VOLTMP
      ENDIF
      VOL(15) = TIPV
      RETURN
      END

C**************************************************************
C**************************************************************
      SUBROUTINE MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,
     >                  HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >                  STUMP,TOP,LMERCH,errflag)
C**************************************************************

C--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
C--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
C--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
C--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
C--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.

      character*2 geosub,FORST
      CHARACTER*10 VOLEQ
      INTEGER JSP, NEXTRA, SETOPT(6),half,first,errflag
      INTEGER LAST,TOPLOP,I,MFLG
      REAL LMERCH, STUMP, TOP, DBHOB, HTTOT, DBTBH, F
      REAL DS, HS,DEX(2),TOP6, HT2, CVOL
      REAL HEX(2),ZEX(2),RHFW(4),RFLW(6),TAPCOE(12),FMOD(3),PINV_Z(2)
      REAL TOP1,CUVOL,DIB,DOB
      geosub = voleq(2:3)
      
C--   SET TOP1 EQUAL TO DESIRED TOP
      DS = TOP
      HS = 0
      IF(VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f') THEN
         CALL SF_HS(JSP,geosub,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,HEX,
     >              ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DS,HS)
         LMERCH = HS - STUMP

      ELSEIF(VOLEQ(4:4).EQ.'J' .OR. VOLEQ(4:4).EQ.'j') THEN
         CALL R1TAP('100JB2W108',FORST,DBHOB,HTTOT,TOP,HS,1,CVOL,DS)
         LMERCH = HS - STUMP

      ELSE
C--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED
         TOP1=ANINT((DS +.005)*10.00)

         FIRST = 1
C--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
         LAST=INT(HTTOT+.5)*10
         TOPLOP = LAST

         DO 10 I = 1,TOPLOP
           IF (FIRST .EQ. LAST) THEN
             GO TO 100
           ENDIF
           HALF = (FIRST + LAST + 1 ) / 2
           HT2=FLOAT(HALF)/10.0

C##########
           CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >       DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >       TOP,MFLG,CUVOL,DIB,DOB,errflag)

C--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
C--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
C--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

           DIB=INT((DIB+.005)*10.0)
           IF (TOP1 .LE. DIB) THEN
C--          MOVE UP STEM
             FIRST = HALF
           ELSE
C--        MOVE DOWN THE STEM
             LAST = HALF - 1
           ENDIF
   10    CONTINUE

  100   CONTINUE

        LMERCH = FLOAT(FIRST)/10.0 - STUMP
      
      ENDIF 

      IF (LMERCH .LT. 0.0) LMERCH=0.0
      
      RETURN
      END

C**************************************************************
C**************************************************************
      SUBROUTINE GETDIB(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,MTOPP,
     >     STUMP,TRIM,DBTBH,LOGST,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >     PINV_Z,TOP6,NUMSEG,LOGLEN,LOGDIA,BOLHT,errflag)
C**************************************************************

C--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
C--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)
C--   LOGDIA(*,1) - REAL - SCALED DIAMETER
C--   LOGDIA(*,2) - REAL - DIAMETER INSIDE BARK
C--   LOGDIA(*,3) - REAL - DIAMETER OUTSIDE BARK
C--   BOLHT(*)   - REAL - HEIGHT UP STEM WHERE LOGDIA WAS PREDICTED

      USE DEBUG_MOD

      character*2 FORST
      CHARACTER*10 VOLEQ 
      INTEGER NUMSEG,LOGST,JSP,NEXTRA,SETOPT(6)
      INTEGER errflag,MFLG,I,DIBCLS
      REAL MTOPP, HT2,LOGLEN(20),BOLHT(21) 
      REAL LOGDIA(21,3), STUMP, TRIM,CUVOL
      REAL DBHOB,HEX(2),DEX(2),ZEX(2),PINV_Z(2),FMOD(3),F,TOP6
      REAL RHFW(4),RFLW(6),TAPCOE(12)
      REAL HTTOT,DBTBH,DIB,DOB,XXX
C      geosub=voleq(2:3)

      !debug info
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 2) ' -->Enter GETDIB (profile.f)'
    2    FORMAT (A)    
      
         WRITE  (LUDBG, 5)'DBHOB HTTOT  MTOPP  STUMP  NUMSEG 
     &    TRIM  LOGST'
  5      FORMAT (A)
         WRITE  (LUDBG, 66)DBHOB,HTTOT,MTOPP,STUMP,NUMSEG,TRIM,
     &           LOGST
 66      FORMAT(F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 2X, I4, 4X,
     &          F5.2, 4X, I6)
     
         WRITE  (LUDBG, 76)'LOGDIA(2,2) ', LOGDIA(2,2)
 76      FORMAT (A, 2x, F8.4)
       END IF

      MFLG = 2
      IF (NUMSEG .EQ. 0 ) GO TO 1000

      IF (STUMP .GT. 4.5 ) THEN
C--     USE FOR TOP WOOD
C###########
!  TDH commented out this code on 3/8/2010 as it was causing the
!  following problem - when topwood is calcuated the large end of
!  the 1st topwood log is the same as the small end of the last
!  main stem log.  The code below would reset this value and the
!  rounding check (if less than MTOPP) would not apply. 
        
!          CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
!     >      DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,STUMP,
!     >      MTOPP,MFLG,CUVOL,DIB,DOB,errflag)


!         LOGDIA(1+LOGST,2)= DIB
!         LOGDIA(1+LOGST,3)= DOB 
!         BOLHT(1+LOGST) = STUMP
         
          IF (DEBUG%MODEL) THEN
       
            WRITE  (LUDBG, 96)'LOGDIA(5,2) ', LOGDIA(5,2)
  96         FORMAT (A, 2x, F8.4)
  
          endif
      ELSE

C--     USE FOR MAIN STEM
C###########
       ht2 = 4.5
c     find DBHOB inside bark
         CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >      DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,ht2,
     >      MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

        LOGDIA(1,2)= DIB
        LOGDIA(1,3)= DOB
        BOLHT(1) = 4.5
      ENDIF

       HT2 = STUMP
       DO 20 I=1+LOGST,NUMSEG+LOGST
          HT2=HT2+TRIM+LOGLEN(I)
          CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >       DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >       MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
          LOGDIA(I+1,2)= DIB
          LOGDIA(I+1,3)= DOB
          BOLHT(I+1) = HT2
  20  CONTINUE

C--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
C--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
C--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
C--  MINIMUM SPECIFIED TOP.

       IF (DEBUG%MODEL) THEN
       
         WRITE  (LUDBG, 25)'LOGDIA(NUMSEG+LOGST+1,2)  MTOPP '
  25     FORMAT (A)
         
        WRITE  (LUDBG, 26)LOGDIA(NUMSEG+LOGST+1,2), MTOPP
 26      FORMAT(F5.1, 1X, F5.1)
 
       endif
       
       
       IF( LOGDIA(NUMSEG+LOGST+1,2) .LT. MTOPP) THEN
          LOGDIA(NUMSEG + LOGST + 1,2) = MTOPP
       ENDIF
C--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
      DO 30 I=1+LOGST,NUMSEG+LOGST+1
         DIBCLS=INT(LOGDIA(I,2))
         XXX=LOGDIA(I,2)-DIBCLS
c          IF( XXX .GT. 0.55) DIBCLS=DIBCLS+1.0
         IF( XXX .GT. 0.501) DIBCLS=DIBCLS+1
         LOGDIA(I,1)=DIBCLS
  30  CONTINUE


C--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
C--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
C--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
C--  MINIMUM SPECIFIED TOP.

!  TDH - why is this code here and also above???       
       IF( LOGDIA(NUMSEG+LOGST+1,2) .LT. MTOPP) THEN
          LOGDIA(NUMSEG + LOGST + 1,2) = MTOPP
       ENDIF

C--   NEED TO CHECK TOP DIB 
       DIBCLS=INT(LOGDIA(NUMSEG+LOGST+1,2))
       XXX=LOGDIA(NUMSEG+LOGST+1,2)-DIBCLS
c       IF( XXX .GT. 0.55) DIBCLS=DIBCLS+1.0
       IF( XXX .GT. 0.501) DIBCLS=DIBCLS+1
       LOGDIA(NUMSEG+LOGST+1,1)=DIBCLS

      IF (DEBUG%MODEL) THEN
       
        WRITE  (LUDBG, 600)'LOGDIA(5,2) ', LOGDIA(5,2)
  600   FORMAT (A, 2x, F8.4)
  
      ENDIF

 1000 CONTINUE
 
      !debug info
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 1100) ' <--Exit GETDIB (profile.f)'
 1100    FORMAT (A)   
      END IF
       
      RETURN
      END

C**************************************************************
C**************************************************************
      SUBROUTINE TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,TOP,TOP6,
     >                   DBTBH,errflag)
C**************************************************************

C--   FIX FOR CZAPLEWSKI 3 POINT MODEL.  DETERMINES THE HEIGHT TO A
C--   6-INCH TOP.

C      CHARACTER*3 SPEC
      INTEGER  I,FIRST,HALF,LAST,TOPLOP
      REAL HTTOT,STUMP,TOP
      character*2 FORST
      CHARACTER*10 VOLEQ 
      INTEGER JSP,NEXTRA,SETOPT(6)
      INTEGER errflag,MFLG
      REAL DIB,DOB,HT2,DBTBH 
      REAL CUVOL,TOP1
      REAL DBHOB,HEX(2),DEX(2),ZEX(2),PINV_Z(2),FMOD(3),F,TOP6
      REAL RHFW(4),RFLW(6),TAPCOE(12)
C      SPEC = VOLEQ(8:10)
      

      TOP6 = 0.0
                                
      IF (DEX(1).LT.6.0) THEN
        TOP6 = HEX(1)
        GO TO 500
      ENDIF

C--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED

      TOP1=INT(TOP*10.0)

      FIRST = 1
C--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
      LAST=INT(HTTOT+.5)*10
      TOPLOP = LAST

      DO 10 I = 1,TOPLOP
        IF (FIRST .EQ. LAST) THEN
          GO TO 100
        ENDIF
        HALF = (FIRST + LAST + 1 ) / 2
        HT2=FLOAT(HALF)/10.0

C###########
      CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >       DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >       TOP,MFLG,CUVOL,DIB,DOB,errflag)

C--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
C--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
C--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

        DIB=INT((DIB+.005)*10.0)
        IF (TOP1 .LE. DIB) THEN
C--        MOVE UP STEM
           FIRST = HALF
        ELSE
C--        MOVE DOWN THE STEM
           LAST = HALF - 1
        ENDIF
   10 CONTINUE

  100 CONTINUE

      TOP6 = FLOAT(FIRST)/10.0 - STUMP
  500 IF (TOP6 .LT. 0.0) TOP6 = 0.0
 
      RETURN
      END


C********************************************************************
C**************************************************************
      SUBROUTINE VOLRATIO(VOLEQ,FORST,DBHOB,HTTOT,TOPD,LOGDIB,LOGLEN,
     >                     NUMSEG,CUBV,VMER)
C**************************************************************

C--   SUBROUTINE WILL DETERMINE CALCULATE SMALIANS CUBIC VOLUME
C--   FOR LODGEPOLE AND THE CUBIC VOLUME FOR THE SPECIES OF
C--   INTEREST.  BOARD FOOT FOR SPECIES OF INTEREST IS CALCULATED
C--   USING A BDFT TO CUBIC FOOT RATIO BASED ON LODGEPOLE.
      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER  NUMSEG,I,LENGTH
      REAL DBHOB, HTTOT, LOGV, HTUP, TOPD
      REAL LOGDIB(21,3),LOGLEN(20),CUBV,VMER,CVOL,DIB,DIBL,DIBS

      CUBV = 0.0
      HTUP = 0.0
    
      CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,TOPD,HTUP,1,CVOL,DIB)

      VMER = CVOL

      DIBL=LOGDIB(1,1)

      DO 10 I=1,NUMSEG
         DIBS=LOGDIB(I+1,1)
         LENGTH=INT(LOGLEN(I))
         LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
         CUBV = CUBV + LOGV
         DIBL = DIBS
  10  CONTINUE

      RETURN
      END

C********************************************************************
C**************************************************************
      SUBROUTINE TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >    DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >    MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
c calls taper equations or profile models.
      USE VOLINPUT_MOD
      IMPLICIT NONE
      
      character*2 geosub,FORST
      CHARACTER*10 VOLEQ 
      INTEGER JSP,NEXTRA,SETOPT(6)
      INTEGER ineedsl,errflag,MFLG,FCLASS
      REAL DIB,DOB,HT2,DBTBH,DBT,HTTOT,TLH
      REAL slope,CUVOL,MTOPP,TOP
      REAL DBHOB,HEX(2),DEX(2),ZEX(2),PINV_Z(2),FMOD(3),F,TOP6
      REAL RHFW(4),RFLW(6),TAPCOE(12)
      
      FCLASS=FORMCLASS
      ineedsl = 0
      DOB = 0.0
      geosub=voleq(2:3)
C###########
      IF(VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f') THEN
         CALL SF_DS(JSP,GEOSUB,NEXTRA,SETOPT,ineedsl,slope,DBHOB,
     >           HTTOT,HT2,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB)
c         IF(JSP.GE.22 .AND. JSP.LE.30) THEN
            CALL BRK_UP(JSP,geosub,DBHOB,HTTOT,DBTBH,HT2,DIB,DOB,DBT)
c         ENDIF
      ELSEIF(VOLEQ(4:6).EQ.'CZ2' .OR. VOLEQ(4:6).EQ.'cz2' .OR. 
     >            VOLEQ(4:6).EQ.'CZ3' .OR. VOLEQ(4:6).EQ.'cz3') THEN
         CALL R2TAP(VOLEQ,DBHOB,DEX(1),HEX(1),TOP6,HTTOT,HT2,DIB,DBTBH,
     >                errflag)
      
      ELSEIF(VOLEQ(4:6).EQ.'WO2' .OR. VOLEQ(4:6).EQ.'wo2') THEN
         CALL R5TAP(VOLEQ,DBHOB,HTTOT,HT2,DIB,errflag)

      ELSEIF(VOLEQ(4:6).EQ.'DEM' .OR. VOLEQ(4:6).EQ.'dem') THEN
        CALL R10TAP(VOLEQ,DBHOB,HTTOT,HT2,DIB)

      ELSEIF(VOLEQ(4:6).EQ.'CUR' .OR. VOLEQ(4:6).EQ.'cur') THEN
        CALL R10TAP(VOLEQ,DBHOB,HTTOT,HT2,DIB)

      ELSEIF(VOLEQ(4:6).EQ.'JB2' .OR. VOLEQ(4:6).EQ.'jb2') THEN
        IF(MFLG .EQ. 0)THEN
            TOP = 0.0
            HT2 = 0.0
c   The following function should be called for volume calc.(10/15/2015)!            
            CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,TOP,HT2,MFLG,
     >                CUVOL,DIB)
            
        ELSE IF(MFLG .EQ. 1) THEN
            CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,HT2,MFLG,
     >                CUVOL,DIB)
        ELSE       
            CALL R1TAP('100JB2W108',FORST,DBHOB,HTTOT,MTOPP,HT2,MFLG,
     >                CUVOL,DIB)
        ENDIF
C Added to test for BEH equation in profile(09/12/13 YW)        
      ELSEIF(VOLEQ(4:6).EQ.'BEH' .OR. VOLEQ(4:6).EQ.'beh') THEN
        TLH=0.0
        CALL BEHTAP(VOLEQ,DBHOB,HTTOT,TLH,HT2,FCLASS,MTOPP,DIB)
      ENDIF

      RETURN
      END


C**************************************************************
C********************************************************************
      SUBROUTINE FWSMALL(JSP,NEXTRA,SETOPT,slope,DBHOB,HTTOT,
     >     DBTBH,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB1,errflag) 
C--******************************************************************
C     FIND TOTAL CUBIC VOLUME OF TREES BETWEEN 5 AND 15 FEET TALL
      character*2 geosub
      INTEGER JSP,NEXTRA,SETOPT(6)
      INTEGER ineedsl,errflag
      REAL HTTOT,DBTBH,SLOPE,DIB,DOB,DBT
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL DBHOB, HEX(2),ZEX(2)

      REAL H1,H5,H15,HR_5,HR_15LO,HR_15HI,HRATIO,HR_MIN,HR_MAX
      REAL DR_MIN,DR,DIB1
      INTEGER IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = ERRFLAG
C
c              set the parameters of the special modifiers
      ineedsl = 0
      H1=1.0
      H5  =5.0
      H15 =15.0
      HR_5 = .18
      HR_15LO= .11
      HR_15HI= .25
C           Ponderosa Pine has slightly higher values
      IF (JSP .eq. 14) then
         HR_5= .25
         HR_15LO = .21
         HR_15HI = .27
      end if
C                For the original eqns (published hemlock) only 2 limits allowed
      CALL SF_DS(JSP,GEOSUB,NEXTRA,SETOPT,ineedsl,slope,DBHOB,
     >           HTTOT,H1,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB)
      IF(JSP.GE.22 .AND. JSP.LE.30) THEN
         CALL BRK_UP(JSP,geosub,DBHOB,HTTOT,DBTBH,H1,DIB,DOB,DBT)
      ENDIF
c                    Compute and possible modify ratio DIB(1 ft) /(TOTALH -1)
      HRATIO = DIB / ( HTTOT-H1 )    
      HR_MIN = HR_5 + (HTTOT - H5)/(H15-H5)* (HR_15LO - HR_5)
      HR_MAX = HR_5 + (HTTOT - H5)/(H15-H5)* (HR_15HI - HR_5) 
      
      IF (HRATIO .lt. HR_MIN) HRATIO=HR_MIN
      IF (HRATIO .gt. HR_MAX) HRATIO=HR_MAX

      DIB1 = HRATIO *(HTTOT - H1)
c                 Impose a limit of DOB(1 ft)/DBHob > DR_MIN
c                      where DR_MIN=1 at totalH=15   , 1.3 at TOTALH = 5
c                                 
      DR_MIN = 1.0
      if (HTTOT .lt. H15) DR_MIN = DR_MIN + 0.3*(H15-HTTOT)/(H15 - H5)
      DR = DIB1/(DBHOB - DBTBH)
      
      if (DR .lt. DR_MIN) DIB1 = DR_MIN*(DBHOB - DBTBH)

      RETURN
      END


C********************************************************************
C********************************************************************
C********************************************************************
C********************************************************************
      SUBROUTINE VOLINTRP(REGN,VOLEQ,DBH,LHT,MHT,MTOPP,HTTYPE,DBTBH,
     >        LOGVOL,LOGDIA,HTLOG,LOGLEN,LOGST,NOLOGP,VOL,CTYPE,PROD,
     >        ERRFLAG)  
c     total height could not be predicted, interpolate the diameters for logs
c     in the merch piece.      
      CHARACTER*1 HTTYPE,COR,CTYPE
      CHARACTER*2 FORST, PROD
      CHARACTER*10 VOLEQ 
      INTEGER LOGST,HTLOG,REGN,EVOD,OPT,I,LCNT,ERRFLAG
      REAL DBH, LHT, MTOPP, DBTBH, VOL(15),NOLOGP,MHT,HTUP
      REAL LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),D2,LEN,MINBFD
      REAL DBHIB,MAXLEN,MINLEN,MERCHL,MINLENT,MTOPS,STUMP,TRIM,BTR
      REAL DIBL,DIBS,LENGTH,LOGV,LOGVOL32(20),TOPV16,BOTV16,R,LMERCH

      DBHIB = DBH-DBTBH
c     check top diameter not greater than DBH inside bark. (YW 2014/07/02)      
      IF(MTOPP.GE.DBHIB)THEN
         ERRFLAG = 13
         RETURN
      ENDIF
c       MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
      CALL MRULES(REGN,FORST,VOLEQ,DBH,COR,EVOD,OPT,MAXLEN,MINLEN,
     >           MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >           PROD)
      IF(HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l')THEN
          LOGDIA(1,1) = ANINT(DBHIB)
          LOGDIA(1,2) = DBHIB
C         IF ONE LOG, SET DIAMETER AND LENGTH
          IF(LHT.EQ.1)then
            LOGST = 1
            NOLOGP = 1
            LOGDIA(2,1) = ANINT(MTOPP)
            LOGDIA(2,2) = MTOPP
            LOGLEN(1) = HTLOG
          ELSE
C         IF MULTIPLE LOGS, LOOP THROUGH, FIND DIAMETERS AND LENGTHS
            LOGST = INT (LHT + 0.5)
            NOLOGP = LHT
            LEN = MHT - 4.5
            HTUP = 1.0
            DO 100, I=1,INT(LHT)-1
              LOGLEN(I) = HTLOG
              HTUP = HTUP + HTLOG
              D2 = DBHIB*DBHIB - (DBHIB*DBHIB-MTOPP*MTOPP)*
     >                                              ((HTUP-4.5)/LEN)
              LOGDIA(I+1,1) = ANINT(SQRT(D2))
              LOGDIA(I+1,2) = SQRT(D2)
  100       CONTINUE
            HTUP = HTUP + LOGLEN(INT(LHT))
            LOGDIA(INT(LHT)+1,1) = ANINT(MTOPP)
            LOGDIA(INT(LHT)+1,2) = MTOPP
          ENDIF
      ELSE
         LMERCH = MHT - STUMP
         IF (CTYPE.NE.'V') THEN
            CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,LOGST)
            CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,LOGST,LOGLEN)
C--     SET NOLOGP TO THE NUMBER OF 16 FOOT SEGMENTS
            NOLOGP = LOGST
C           NOLOGP = LMERCH/16.0
         ELSE
            NOLOGP = LOGST
         ENDIF
         LOGDIA(1,1) = ANINT(DBHIB)
         LOGDIA(1,2) = DBHIB
C        CHECK FOR A ONE LOG TREE, SET DIAMETER AND LENGTH
         IF(LOGST.EQ.1)THEN
             LOGDIA(2,1) = ANINT(MTOPP)
             LOGDIA(2,2) = MTOPP
C        IF MULTIPLE SEGMENTS, LOOP THROUGH, FIND DIAMETERS AND LENGTHS
         ELSE
            LEN = MHT - 4.5
            HTUP = 1.0
            DO 200, I=1,LOGST-1
               HTUP = HTUP + LOGLEN(I)
               D2 = DBHIB*DBHIB - (DBHIB*DBHIB-MTOPP*MTOPP)*
     >                                             ((HTUP-4.5)/LEN)
               LOGDIA(I+1,1) = ANINT(SQRT(D2))
               LOGDIA(I+1,2) = SQRT(D2)
 200       CONTINUE
           HTUP = HTUP + LOGLEN(LOGST)
           LOGDIA(LOGST+1,1) = ANINT(MTOPP)
           LOGDIA(LOGST+1,2) = MTOPP
         ENDIF
      ENDIF
C     CALL THE SCRIB ROUTINE
      DIBL=LOGDIA(1,1)
      DO 250 I=1,LOGST
         DIBS=LOGDIA(I+1,1)
         LENGTH=LOGLEN(I)
         CALL SCRIB (DIBS,LENGTH,COR,LOGV)
         if(cor.eq.'Y') then
            LOGVOL(1,I) = LOGV * 10
         else
            LOGVOL(1,I) = ANINT(LOGV)
         endif
         VOL(2) = VOL(2) + LOGVOL(1,I)
C     FIND SMAILIAN'S VOLUME
         LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
         LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0
         VOL(4)=VOL(4) + LOGVOL(4,I)
         DIBL = DIBS
  250 CONTINUE
c     find volume for 32 foot logs (Flewelling & Demars equation only)
      IF((VOLEQ(4:5).EQ.'f3' .OR. VOLEQ(4:5).EQ.'F3' .OR.
     >                   VOLEQ(2:3).EQ.'61') .AND. LOGST.GT.1) THEN
         LCNT = 0
         DO 260 I = 2, LOGST,2
            LCNT = LCNT + 1
c           truncate diameter for 32foot board 6/2001
            DIBS = INT (LOGDIA(I+1,2))
            LENGTH = LOGLEN(I) + loglen(I-1)
            CALL SCRIB (DIBS,LENGTH,COR,LOGV)
            LOGVOL32(LCNT) = LOGV
  260    CONTINUE
C    check for a 16 foot top log
         IF(((LOGST+1)/2) .GT. (LOGST/2)) THEN
            LCNT = LCNT + 1
            DIBS = INT(LOGDIA(LOGST+1,2))
            LENGTH = LOGLEN(LOGST)
            CALL SCRIB (DIBS,LENGTH,COR,LOGV)
            LOGVOL32(LCNT) = LOGV
         ENDIF
c    prorate 32 volumes into 16 foot pieces
         LCNT = 0
         DO 261, i=2,LOGST,2
            LCNT = LCNT + 1
            topv16 = logvol(1,i)
            botv16 = logvol(1,i-1)
            R = TOPV16 / (TOPV16 + BOTV16)
            logvol(1,I) = ANINT(logvol32(LCNT) * R)
            logvol(1,I-1) = ANINT(logvol32(LCNT) - logvol(1,i)) 
            IF(cor.eq.'Y') THEN
               logvol(1,I-1) = logvol(1,I-1) * 10
               logvol(1,I) = logvol(1,I) * 10 
            ENDIF
  261    CONTINUE
c     check for top log
         IF(((LOGST+1)/2) .GT. (LOGST/2)) THEN
            LCNT = LCNT + 1
            LOGVOL(1,LOGST) = LOGVOL32(LCNT)
            IF(cor.eq.'Y') LOGVOL(1,LOGST) = LOGVOL(1,LOGST)*10
         ENDIF

         VOL(2) = 0
         DO 265, i = 1,LOGST
            VOL(2) = VOL(2) + LOGVOL(1,I)
C            WRITE(*,*)" 32FT ",NUMSEG,I,LOGVOL(1,I)
  265    CONTINUE
      ENDIF
c************** 32 foot logs endif ******************************
c     calculate stump and stem tip volume
      IF(STUMP.LT.0.01) STUMP=1.0
      VOL(14)=0.005454154*LOGDIA(1,2)*LOGDIA(1,2)*STUMP
      IF(VOL(4).GT.0.0) VOL(15)=VOL(1)-VOL(14)-VOL(4)-VOL(7)
      IF(VOL(15).LT.0.01) VOL(15)=0.0
      
      RETURN
      END


C*******************************************************************
C*******************************************************************
      SUBROUTINE R10HTS(EQNUM,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,TOP,
     >                   LMERCH)
C*******************************************************************
C
C     TAPEQU IS THE PROFILE EQUATION NUMBER THAT WILL BE USED IN THE 
C             CALCULATIONS
C             TAPEQU 1051 IS SPRUCE-HEMLOCK - 16 FOOT LOGS
C             TAPEQU 1052 IS SPRUCE-HEMLOCK - 32 FOOT LOGS
C             TAPEQU 1053 IS WESTERN REDCEDAR - 16 FOOT LOGS
C             TAPEQU 1054 IS WESTERN REDCEDAR - 32 FOOT LOGS
C             TAPEQU 1055 IS ALASKA CEDAR - 16 FOOT LOGS
C             TAPEQU 1056 IS ALASKA CEDAR - 32 FOOT LOGS
C     HTTOT    IS THE TOTAL HEIGHT OF TREE FROM GROUND TO TIP
C     DBHOB    IS DIAMETER BREAST HEIGHT OUTSIDE BARK
C     STUMP  IS THE HEIGHT OF THE STUMP
C     TOP    IS SPECIFIED TOP DIAMETER
C     LMERCH IS THE MERCHANTABLE LENTH FROM STUMP TO SPECIFIED
C             TOP DIAMETER, INCLUDING TRIM.
C      
      REAL DST(2),DSLO(2),DSHI(2),XLL(20),DS(20),HH(20)
C
C     LIST REAL VARIABLES
C
      CHARACTER*1 HTTYPE      
      CHARACTER*10 EQNUM
      CHARACTER*2 ISP,IAC,IRC,ISS,IRA
      REAL LIMD,LOWD,LUPD,LMERCH,TAPER,HITOP,RH,DSX,BKWRC,DSI
      REAL HTTOT, HT1PRD, DBHOB, STUMP, TOP,RXL,DXL,HTEST
      REAL BKAYC,H,HEST,D,C,HTD,BK,RH32,RH40
      REAL*8 BKAC,DVA,BKWR,DVR,BB,DD2MI,DVREDA
      INTEGER I,K,KNOL,IMO
C
C     SPRUCE-HEMLOCK TAPER EQUATION
C     PROFILE EQUATION USED FOR DIAMETER CALCULATIONS
C*********************************************************************** 
      DD2MI(RH,RH32,D,H) = RH**1.5 + (-.0052554*H + 0.000034947*H**2 
     >    + 0.104477*H/D) * (RH**1.5 - RH**3) + (7.76807/D**2
     >    - 0.0000094852*H**2 - 0.011351*H/D) * (RH**1.5-RH32**32)
C***********************************************************************
C     BARK THICKNESS EQUATION
C
      BB(D,H) = (0.8467 + 0.0009144*D + 0.0003568*H)
C
C     THE FOLLOWING TAPER EQUATION IS FOR WESTERN RED CEDAR
C         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)**2
C***********************************************************************
      DVR(RH,RH32,H,DBHOB) = RH**1.5 + ((RH**1.5 - RH**3) * 
     >     (5.17703194/DBHOB**2 - 0.12516819*DBHOB + 0.02537037*H
     >     - 0.00004193*H**2 + 0.00155481 * DBHOB**2)) + ((RH**1.5
     >     - RH32**32) * (-0.00002070*H**2 + 0.24125235/DBHOB**2))
C***********************************************************************
C     BARK THICKNESS EQUATION FOR WESTERN RED CEDAR
C
      BKWR(DBHOB,H) = 0.86031485 + 0.00059638*H - 0.18335961/DBHOB
C
C     THE FOLLOWING TAPER EQUATION IS FOR ALASKA YELLOW CEDAR
C         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)
C***********************************************************************
      DVA(RH,RH32,H,DBHOB)=RH**1.5+((RH**1.5-RH**3)*(-0.02834001*DBHOB
     >    + 0.00007123*H**2 + 0.06709114*H/DBHOB))
     >    + ((RH**1.5 - RH32**32) * (.00282021*DBHOB - 0.00002277*H**2
     >    + 1.06064717/DBHOB**2 - 0.00528349*H/DBHOB))
C***********************************************************************
C     BARK THICKNESS EQUATION FOR ALASKA YELLOW CEDAR
C
      BKAC(DBHOB,H) = 0.95866817 + 0.00064402*DBHOB - 3.1299972/H
C
C     THE FOLLOWING TAPER EQUATION IS FOR RED ALDER
C         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)
C***********************************************************************
      DVREDA(RH,RH32,RH40,H,DBHOB)=0.91274*(RH**1.5)
     >    -(1.9758*(RH**1.5-RH**3.)*(DBHOB*10.**(-2.)))
     >    + (8.2375*(RH**1.5 - RH**3.)*(H)*(10.**(-3.)))
     >    - (4.964*(RH**1.5 - RH32**32.)*(H*DBHOB)*(10.**(-5.)))
     >    + (3.773*(RH**1.5 - RH32**32.)*(H**0.5)*(10.**(-3.)))
     >    - (7.417*(RH**1.5 - RH40**40.)*(H**2.)*(10.**(-6.)))
C***********************************************************************
C     BARK THICKNESS EQUATION FOR RED ALDER
C
C
      H=HTTOT
      HEST=0.0
      D=DBHOB
      DBHOB=D
C
C SET HTD TO MERCHANTABLE HEIGHT, CONVERTING LOGS TO FEET IF NECESSARY
C
      IF(HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
         !logs are in 10s of logs. So added /10 to get number of logs. YW 9/9/13
         IF (EQNUM(1:3).EQ.'A16') THEN
            LMERCH = HT1PRD/10*16.3
            HTD = LMERCH +STUMP
         ELSE IF (EQNUM(1:3).EQ.'A32') THEN
            LMERCH = HT1PRD/10*32.6
            HTD = LMERCH + STUMP
         ENDIF
      ELSE
         LMERCH = HT1PRD
         HTD = LMERCH + STUMP
      ENDIF

      IF(EQNUM(8:10).EQ.'042') ISP='AC'
      IF(EQNUM(8:10).EQ.'242') ISP='RC'
      IF(EQNUM(8:10).EQ.'098') ISP='SS'
      IF(EQNUM(8:10).EQ.'351') ISP='RA'
      IAC='AC'
      IRC='RC'
      ISS='SS'
      IRA='RA'

      LIMD = TOP
      LUPD = LIMD + 0.001
      LOWD = LIMD - 0.001
C
C     ZERO OUT ARRAYS
C
      DO 15 I=1,20
         DS(I)=0.0
         XLL(I)=0.0
   15 CONTINUE

      I=1
      IF(LMERCH.GT.0.01)  THEN
          H=0.0
          HH(I)=HTD
      ENDIF
       
      IF(ISP.EQ.ISS .OR. ISP.EQ.IRA .OR. (ISP.EQ.IAC .AND. D.GT.38.01)
     >                         .OR. (ISP.EQ.IRC .AND. D.GT.56.01)) THEN
C///////////////////////////////////////////////////////////////////////
C     THE FOLLOWING ESTIMATES HI AT TOP DIAMETER OR TOTAL TREE HEIGHT
C       DEPENDING ON WHICH VARIABLE IS MISSING (FOR SPRUCE-HEMLOCK EQ.)
C       SPRUCE-HEMLOCK, RED ALDER, AND LARGE ALASKA AND WESTERN RED CEDAR
C///////////////////////////////////////////////////////////////////////
         I = 1
       
         IF(H.GE.0.01) THEN
         IF(ISP.EQ.IRA)THEN
            BK = 0
         ELSE
            BK = REAL(BB(D,H))
         ENDIF
C            FV = 0.005454154*D**2*(H - 4.5)*BK
         ENDIF
         
         XLL(I) = 1.0 - (2./3.)*(LIMD/D)
       
         IF(H.LT.0.01)  HEST = HH(I)/XLL(I)

         IF(HEST.GT.0.01) BK = REAL(BB(D,HEST))

         IF(ISP.EQ.IRA)THEN
            DST(I) = (LIMD**2)/(D**2)
         ELSE
            DST(I) = (LIMD**2)/(BK*D**2)
         ENDIF
         
         DSLO(I) = DST(I) - 0.0001
         DSHI(I) = DST(I) + 0.0001

         IF(H.LE.0.01) THEN
            RH = (HEST - HH(I))/(HEST - 4.5)
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
            IF(RH.LE.0.0)THEN
               RH = 0
               RH32 = 0
               RH40 = 0
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
            
            IF(ISP.EQ.IRA)THEN
               DS(I) = REAL(DVREDA(RH,RH32,RH40,HEST,D))
            ELSE
               DS(I) = REAL(DD2MI(RH,RH32,D,HEST))
            ENDIF
            if(ds(i).lt.0.0) ds(i) = 0.0

            RXL = 0.9*RH
            IF(RXL.LE.0.0)THEN
               RH32 = 0
               RH40 = 0
            ELSEIF(RXL .LT. 0.078) THEN
               RH40 = 0.15
               RH32= 0.078
            ELSEIF(RXL .LT. 0.15) THEN
               RH40 = 0.15
               RH32= RXL
            ELSE
               RH40 = RXL
               RH32= RXL
            ENDIF
            IF(ISP.EQ.IRA)THEN
               DXL = REAL(DVREDA(RXL,RH32,RH40,HEST,D))
            ELSE
               DXL = REAL(DD2MI(RXL,RH32,D,HEST))
            ENDIF
            if(dxl.lt.0.0) dxl = 0.0
            TAPER = (DS(I) - DXL)/(.1*RH)
         ELSE
            HH(I) = H*XLL(I)
            RH = (H - HH(I))/(H - 4.5)
            IF(RH.LE.0.0)THEN
               RH = 0
               RH32 = 0
               RH40 = 0
            ELSEIF(RH .LT. 0.078) THEN
               RH40 = 0.15
               RH32= 0.078
            ELSEIF(RH .LT. 0.15) THEN
               RH40 = 0.13
               RH32= RH
            ELSE
               RH40 = RH
               RH32= RH
            ENDIF
            IF(ISP.EQ.IRA)THEN
               DS(I) = REAL(DVREDA(RH,RH32,RH40,H,D))
            ELSE
               DS(I) = REAL(DD2MI(RH,RH32,D,H))
            ENDIF
            if(ds(i).lt.0.0) ds(i) = 0.0

            RXL = 0.9*RH
            IF(RXL.LE.0.0)THEN
               RXL = 0
               RH32 = 0
               RH40 = 0
            ELSEIF(RXL .LT. 0.078) THEN
               RH40 = 0.15
               RH32= 0.078
            ELSEIF(RXL .LT. 0.15) THEN
               RH40 = 0.15
               RH32= RXL
            ELSE
               RH40 = RXL
               RH32= RXL
            ENDIF
            IF(ISP.EQ.IRA)THEN
               DXL = REAL(DVREDA(RXL,RH32,RH40,H,D))
            ELSE
               DXL = REAL(DD2MI(RXL,RH32,D,H))
            ENDIF

            if(dxl.lt.0.0) dxl = 0.0
            TAPER = (DS(I) - DXL)/(.1*RH)
         ENDIF

         DO 245, K=1,10
            IF(DS(I).GT.DSLO(I) .AND. DS(I).LT.DSHI(I))  GO TO 40
            IF(TAPER.NE.0.0) THEN
               RXL = RH + (DST(I) - DS(I))/TAPER
            ELSE
               RXL = RH
            ENDIF
            RH = RXL
            IF(RH.LE.0.0)THEN
               RH = 0
               RH32 = 0
               RH40 = 0
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
            IF(H.LE.0.01) THEN
               HEST = (HH(I) - 4.5*RH)/(1.0 - RH)
               IF(ISP.EQ.IRA)THEN
                  DS(I) = REAL(DVREDA(RH,RH32,RH40,HEST,D))
               ELSE
                  DS(I) = REAL(DD2MI(RH,RH32,D,HEST))
               ENDIF
               if(ds(i).lt.0.0) ds(i) = 0.0
            ELSE
               IF(ISP.EQ.IRA)THEN
                  DS(I) = REAL(DVREDA(RH,RH32,RH40,H,D))
               ELSE
                  DS(I) = REAL(DD2MI(RH,RH32,D,H))
               ENDIF
               if(ds(i).lt.0.0) ds(i) = 0.0
            ENDIF 
 245     CONTINUE
       
   40    CONTINUE
         IF(HTD.GT.0.01 .AND. H.LT.0.01)  GO TO 27
         IF(H.LT.0.01)  GO TO 27
 
         HH(I) = H - (RH*(H - 4.5))
         LMERCH = HH(I) - STUMP

   27    CONTINUE

         IF(H.LT.0.01)  H = HEST
         HTTOT = H
C
      ELSE
C////////////////////////////////////////////////////////////////////////////
C     THE FOLLOWING ROUTINE IS DESIGNED TO ESTIMATE HT AT TOP DIAMETER
C       OR TOTAL TREE HEIGHT DEPENDING ON WHICH VARIABLE IS MISSING
C       SMALL ALASKA AND WESTERN RED CEDAR
C////////////////////////////////////////////////////////////////////////////
         I=1
         IF(H.GT.0.001)  THEN

C THIS PART OF THE PROGRAM ESTIMATES HT TO MERCHANTABLE TOP DIAMETER

            KNOL = 0
            HITOP = STUMP

 4541       CONTINUE

            HITOP = HITOP + 16.3
            IF(HITOP.LE.H)  THEN
      
               KNOL = KNOL + 1
               RH = (H - HITOP)/(H - 4.5)
               IF(RH.LE.0.0)THEN
                    RH = 0
                  RH32 = 0
              ELSEIF(RH .LT. 0.078) THEN
                  RH32= 0.078
               ELSE
                  RH32= RH
               ENDIF
C               HX(KNOL) = HITOP

C  WESTERN REDCEDAR MERCHANTABLE LENGTH
               IF(ISP.EQ.IRC)  THEN
                  DSX = REAL(DVR(RH,RH32,H,DBHOB))
                  BKWRC = REAL(BKWR(DBHOB,H))
                  if(dsx.lt.0) dsx = 0
                  DSI = (DSX*BKWRC)**.5 * DBHOB
              
C  ALASKA CEDAR MERCHANTABLE LENGTH
               ELSE IF(ISP.EQ.IAC) THEN 
                  DSX = REAL(DVA(RH,RH32,H,DBHOB))
                  BKAYC = REAL(BKAC(DBHOB,H))
                  if(dsx.lt.0) dsx = 0
                  DSI = (DSX*BKAYC)**.5 * DBHOB
               ELSE
                  GO TO 900
               ENDIF

               DS(KNOL) = DSI

               IF(DS(KNOL).GE.LIMD)  GO TO 4541
            ENDIF
            
            IF(DS(KNOL).LT.LIMD)  KNOL = KNOL - 1
            HITOP = HITOP - 16.3
C            HX(KNOL+2) = H
            DS(KNOL+2) = 0.0
            IMO = KNOL + 1
            C = 1.0

 5566       CONTINUE
            HITOP = HITOP + C
              
            RH = (H - HITOP)/(H - 4.5)
            IF(RH.LE.0.0)THEN
               RH = 0
               RH32 = 0
           ELSEIF(RH .LT. 0.078) THEN
               RH32= 0.078
            ELSE
               RH32= RH
            ENDIF
              
C  WESTERN RED CEDAR TAPER EQUATION
            
            IF(ISP.EQ.IRC)  THEN
               DSX = REAL(DVR(RH,RH32,H,DBHOB))
               BKWRC = REAL(BKWR(DBHOB,H))
               if(dsx.lt.0) dsx = 0
               DSI = (DSX*BKWRC)**.5 * DBHOB

C  ALASKA YELLOW CEDAR TAPER EQUATION
            ELSE
               DSX = REAL(DVA(RH,RH32,H,DBHOB))  
               BKAYC = REAL(BKAC(DBHOB,H))
               if(dsx.lt.0) dsx = 0
               DSI = (DSX*BKAYC)**.5 * DBHOB
            ENDIF 

            IF(DSI.GT.LOWD.AND.DSI.LT.LUPD)  GO TO 5543

            IF(DSI.GT.LIMD)  GO TO 5566

            HITOP = HITOP - C
            C = C * 0.1

            GO TO 5566
 
 5543       CONTINUE

            DS(IMO) = DSI
            HH(1) = HITOP
            LMERCH = HITOP - STUMP
C            HX(IMO) = HITOP

C     THIS PART OF THE PROGRAM ESTIMATES THE TOTAL HEIGHT OF THE TREE
C     THIS PART USES ALASKA CEDAR OR WESTERN REDCEDAR EQUATIONS
C
         ELSE

            HITOP = STUMP + LMERCH
            HTEST = HITOP
            C = 1.0

 7566       CONTINUE

            HTEST = HTEST + C
            H = HTEST
            RH = (H - HITOP)/(H - 4.5)
            IF(RH.LE.0.0)THEN
               RH = 0
               RH32 = 0
           ELSEIF(RH .LT. 0.078) THEN
               RH32= 0.078
            ELSE
               RH32= RH
            ENDIF

C   WESTERN RED CEDAR TAPER EQUATION
            IF(ISP.EQ.IRC)  THEN
               DSX = REAL(DVR(RH,RH32,H,DBHOB))
               BKWRC = REAL(BKWR(DBHOB,H))
               if(dsx.lt.0) dsx = 0
               DSI = (DSX*BKWRC)**.5 * DBHOB

C  ALASKA YELLOW CEDAR TAPER EQUATION
            ELSE
               DSX = REAL(DVA(RH,RH32,H,DBHOB))
               BKAYC = REAL(BKAC(DBHOB,H))
               if(dsx.lt.0) dsx = 0
               DSI = (DSX*BKAYC)**.5 * DBHOB
            ENDIF

            IF(DSI.GT.LOWD.AND.DSI.LT.LUPD)  GO TO 7543
            IF(DSI.LT.LIMD)  GO TO 7566

            HTEST = HTEST - C
            C = C * 0.1

            GO TO 7566

 7543       CONTINUE
            H = HTEST
            HTTOT = H
C
        ENDIF 
C
      ENDIF

  900 RETURN
      END 

C********************************************************************
C********************************************************************
      SUBROUTINE FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >        LMERCH,DBTBH,MTOPP,STUMP,HEX,DEX,ZEX,RHFW,RFLW,
     >        TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)

C--   DETERMINE CUBIC FIBER CONTENT TO TOP DIAMETER.  THE VOLUME WILL BE
C--   DETERMINED BY USING THE SMALIAN FORMULA TO COMPUTE VOLUMES FOR 4 
c--   FOOT SECTIONS FROM THE STUMP TO THE TOP DIAMETER, NO TRIM.

C--   VARIABLES OF INTEREST ARE:
C--   TCVOL - REAL - VOLUME FOR THE TREE IN CUBIC FOOT,
C--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER HTLOOP,JSP,NEXTRA,SETOPT(6),errflag,I,MFLG
      REAL D2OLD, HTTOT, HT2, TCVOL, VOLTMP,MTOPP,DBTBH
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL DBHOB, HEX(2), ZEX(2),TOP6,DEX(2),slope
      REAL DIB,DOB,CUVOL,LMERCH,MERCHHT,STUMP
                                                    
C     GET MERCH HEIGHT

      HTLOOP = INT((LMERCH + .5 - 1.0)/4.0)
      MERCHHT = LMERCH + STUMP

      HT2=STUMP
C##########
      IF(JSP.GT.0 .AND. HTTOT.LE.15)THEN
C                  SMALL TREE FIX FOR FLEWELLING STUMP DIAMETERS
        CALL FWSMALL(JSP,NEXTRA,SETOPT,slope,DBHOB,HTTOT,DBTBH,
     >       HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB,errflag) 
      ELSE
      
        CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >    DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >    MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
      
C        IF(VOLEQ(4:6).EQ.'JB2' .OR. VOLEQ(4:6).EQ.'jb2') THEN
C          IF(VOLEQ(8:10) .EQ. '108') THEN
C             TCVOL = 0.005454154 * CUVOL * DBHOB * DBHOB * HTTOT
C          ELSE
C             TCVOL = CUVOL
C          ENDIF
C          RETURN
C        ENDIF
      ENDIF     

      TCVOL = 0
      DO 10 I = 1,HTLOOP
         D2OLD = DIB
         HT2=HT2+4.0
C##########
         CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >     DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,
     >     MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

C--     USE SMALIAN FOR NON STUMP SEGMENTS
         VOLTMP = .00272708*(D2OLD*D2OLD+DIB*DIB)*4.0
         TCVOL = TCVOL + VOLTMP
   10 CONTINUE
c--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF MTOPP
      IF ((MERCHHT-HT2) .GT. 0.0 ) THEN
         VOLTMP = .00272708*(DIB*DIB)*(MERCHHT - HT2)
         TCVOL = TCVOL + VOLTMP
      ENDIF
      TCVOL = NINT(TCVOL * 10.0) * 1E-1
      RETURN
      END

C********************************************************************
C********************************************************************
C--  VARIABLES OF INTEREST ARE:
C--******************************************************************
C--  Following variables have been be passed to the model *******
C--            optional variables are identified
c
c--  MERCHANDIZING VARIABLES
C***************************
c--  REGION - INTEGER - Region number used to set Regional Merchandizing Rules
C--  INTEGERS - Indicating which volumes are desired
C--    BFPFLG - Board foot primary product
C--    CUPFLG - Cubic foot primary product
C--    CDPFLG - Cord wood primary product
C--    SPFLG - Secondary product
C
C****** End of Merchandising parameters.
C
C    TREE CHARACTERISTICS
C************************
C--  VOLEQ - INTEGER - The volume equation number for this tree
C
C--  DBHOB - REAL - Diameter Breast Height Outside Bark
c--  DRCOB - REAL - Diameter Root Collar Outside Bark
c
c--  HTTYPE - CHAR - Tree height measured in feet (F) or logs (L)
C--  HTTOT - REAL - Tree height from the ground to the tip
C--  LOGHT - REAL - Tree height in number of logs to MTOPP
c--  HT1PRD - REAL - Tree height in feet to MTOPP
c--  HT2PRD - REAL - Tree height in feet to MTOPS
c
C--  Note measurements of bark and form are optional entries
C--  DBTBH - REAL - Double bark thickness at breast height
C--  FCLASS - INTEGER - Girard form class
C--  BTR - INTEGER - Bark thickness ratio
C--  UPSHT1 - REAL - The height of the first upper stem form measurement
C--  UPSHT2 - REAL - The height of the second upper stem form measurement
C--  UPSD1 - REAL - The measured diameter outisde bark at point UPSHT1.
C--  UPSD2 - REAL - The measured diameter outisde bark at point UPSHT2 .
c--  HTREF - REAL - The percent reference height for AVGZ1
C--  AVGZ1 - REAL - An average Z correction factor to use for point UPSHT1.
C--  AVGZ2 - REAL - An average Z correction factor to use for point UPSHT2.
C
C--  PCTDF - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
C--          IN THE MAIN STEM PORTION OF A TREE - THIS DEFECT %
C--          WILL BE APPLIED TO BOTH BDFT AND CUFT PRODUCTS.
C--  PCTDF2 - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
C--          IN THE TOPWOOD PORTION OF A TREE - THIS DEFECT %
C--          WILL BE APPLIED TO CUFT PRODUCTS.
c
C--  RETURNED VARIABLES
C--********************
C--  Following variables will be returned to the calling routine ******
C--             when requested
C--  VOL - REAL
C--    VOL(1) - Total volume for the tree in cubic feet, ground to tip.
C--              Does not include the limbs and roots.
C--    VOL(2) - Gross amount of board foot as primary product - Scribner.
C--    VOL(3) - Net amount of board foot as primary product - Scribner.
C--    VOL(4) - Gross amount of cubic foot as primary product - Smalian.
C--    VOL(5) - Net amount of cubic foot as primary product - Smalian.
C--    VOL(6) - Amount of cord wood as primary product.
C--    VOL(7) - Gross amount of cubic foot as secondary product - Smalian.
C--    VOL(8) - Net amount of cubic foot as secondary product - Smalian.
C--    VOL(9) - Amount of cord wood as secondary product.
C--    VOL(10) - Gross amount of board foot as primary product - INT'L 1/4"
C--    VOL(11) - Net amount of board foot as primary product - INT'L 1/4"
C--    VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C--    VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C--    VOL(14) - Stump inside bark cubic foot volume
C--    VOL(15) - Stump outside bark cubic foot volume
C--
C--  LOGLEN - REAL(20) - Log segment lengths computed by
C--                      subroutine "SEGMENT".  Does not include trim.
C--  LOGVOL(7,20) - REAL - The product volume of a log
C--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
C
C--  LOGDIA - REAL(21,3) - Log end diameters 
C       LOGDIA(x,1) = scaling dib
C       LOGDIA(x,2) = actual dib 
c       LOGDIA(x,3) = actual dob
c
C--  BOLHT - REAL(21) - point on the bole where diameters were estimated
C--  *** Note LOGDIA and BOLHT are dimensioned 1 larger then the number of
C--     logs.  This is to hold the values for the small end of the top log.
c
C--  NOLOGP - REAL - Average number of 16 ft logs, primary product.
C--  NOLOGS - REAL - Average number of 16 ft logs, secondary product.
c
C--******************************************************************
c
C--  VARIABLES OF INTEREST USED WITHIN THIS ROUTINE ARE:
c
C--  LENMS - REAL - Length of primary product if a secondary product
C--                 is desired.
C--  LMERCH - REAL - Total merchantable length of stem returned
C--             from subroutine "MERLEN" and the merchantable
C--             length of stem with trim and unusable top
C--             removed returned from subroutine "SEGMNT"
C--  NUMSEG - INTEGER - The computed number of segments from 
C--           subroutine "NUMSEG"
C--
C**************************************************************
C**************************************************************

C--  
