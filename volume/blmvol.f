!== last modified  09-18-2013
C 01/18/2013 Added calculation for stump VOL(14) and tip VOL(15)
C 09/18/2013 Correct XINT to sum all logs
C 03/26/2015 Modified SUBROUTINE DOUBLE_BARK to include TAPEQN 14 and also catch error for no bark equation (YW)
C 04/13/2017 Removed stump and tip vol calc. Now they are calced in voinit.
      SUBROUTINE BLMVOL(VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,
     >        VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,BFPFLG,
     >        CUPFLG,ERRFLAG)
C
C   THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C   USING BLM, OREGON, VOLUME DETERMINATION ROUTINES.

C   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
C     BLMGDIB - INTERNAL - CALLS BLMTAP
C     BLMMLEN - INTERNAL - CALLS BLMTAP
C     BLMTCUB - INTERNAL - CALLS BLMTAP
C     NUMLOG  - EXTERNAL - NO CALLS
C     BLMOLDV - EXTERNAL - NO CALLS
C     SCRIB   - EXTERNAL - NO CALLS
C     SEGMNT  - EXTERNAL - NO CALLS

C **************************************************************
C **************************************************************

      USE DEBUG_MOD

      CHARACTER*10 VOLEQ
      CHARACTER*1 COR, HTTYPE

      INTEGER EVOD,NUMSEG,OPT,TAPEQU,FCLASS,ERRFLAG,I,J
      INTEGER PROFILE, bfpflg,cupflg,tlogs
C      INTEGER ITAPER

      REAL DBHOB,DIB,DBHIB,LOGVOL(7,20),MTOPP,D2
      REAL HTTOT,TTH,D17,TLH,LENGTH,MERCHL,HALFLOG,DIBL,DIBS
      REAL LMERCH,LOGV,HGT2,HT1PRD,MAXLEN,MINLEN,NOLOGP,NOLOGS
      REAL STUMP,TRIM,VOL(15),LOGDIA(21,3),LOGLEN(20),HTLOG
      REAL BLMBUTT(3,9),LOGVOL32(20),TOTCUB,SMD_17,TCVOL
      REAL BFINT
C     COEFFICIENTS FOR THE BUTT LOGS (B1,B2,B3)

C     DOUGLAS FIR, COAST
      DATA ((BLMBUTT(I,J),I=1,3),J=1,9)
     >               / 0.8936,  0.2810,  -0.0407,
C     DOUGLAS FIR, CASCADE
     >                 0.8737,  0.3359,  -0.0581,
C     DOUGLAS FIR, SOUTHWEST,EAST,CALIFORNIA & W LARCH
     >                 0.7111,  0.3433,  -0.0339,
C     PONDEROSA, SUGAR, JEFFREY & W WHITE PINE
     >                 0.6232,  0.3388,   0.0,
C     GRAND, WHITE FIR, SPRUCES EAST & WEST SIDE
     >                 0.7082,  0.3490,  -0.0238,
C     SHASTA, SILVER, & NOBLE FIR
     >                 0.8304,  0.1901,   0.0,
C     HEMLOCK
     >                 0.5393,  0.4447,   0.0,
C     CEDARS
     >                 0.9305,  0.2003,   0.0,
C     ALL OTHER SPECIES
     >                 0.7150,  0.294,    0.0  /

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 90) ' -->Enter BLMVOL'
   90    FORMAT (A)   
   		   
         WRITE  (LUDBG, 100)'  VOLEQ     MTOPP HTTOT HT1PRD DBHOB 
     &    HTTYPE FCLASS'
100      FORMAT (A)
  		   WRITE  (LUDBG, 104)VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,
     &                       FCLASS
104      FORMAT(A, 1X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 5X, A, 
     &          1X, I5)
         
         WRITE  (LUDBG, 300)'TLOGS NOLOGP NOLOGS BFPFLG CUPFLG
     &                        ERRFLAG'
  300    FORMAT (A)
  		   WRITE  (LUDBG, 320)TLOGS,NOLOGP,NOLOGS,BFPFLG,CUPFLG,
     &                         ERRFLAG
  320    FORMAT(1X, I2, 2X, F5.1, 2X, F5.1, 2X, A, 1X, I5, I5, 1X,I5)
      ENDIF
      
C   SET ALL POTENTIAL VOLUMES AND AVG NUM OF LOGS TO ZERO
      DO 10 I = 1, 15
          VOL(I) = 0.0
  10  CONTINUE

      DO 15 I = 1, 21
          LOGDIA(I,1) = 0.0
          LOGDIA(I,2) = 0.0
          LOGDIA(I,3) = 0.0
  15  CONTINUE

      DO 21, I = 1,7
        DO 20 J = 1, 20
          logvol(I,J) = 0.0
  20    CONTINUE
  21  CONTINUE

      TLOGS = 0
      STUMP = 0.0
      NOLOGP = 0.0
      NOLOGS = 0.0
      NUMSEG = 0
      ERRFLAG = 0
     

C   IF DBHOB OR HT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME

      IF(DBHOB.LE. 0.0) THEN
          ERRFLAG = 3
      ENDIF
     
      IF(FCLASS .LE. 0) THEN
          ERRFLAG = 2
	    GO TO 1000
      ENDIF


      IF (HTTYPE.EQ.'L'.OR.HTTYPE.EQ.'l') THEN
         IF (HT1PRD.LT.10 .OR. HT1PRD.GT.200) THEN
           ERRFLAG = 7
            GO TO 1000
         ENDIF

         TLH = HT1PRD/10.0            ! logs are in 10s of logs, 11/2000
         TTH = 0.0
                                             ! find number of 16 foot logs
c         IF(MODEL.EQ.'B32' .OR. MODEL.EQ.'b32') TLH = TLH * 2
         
      ELSE
        IF(HTTOT .LE. 0) THEN
           ERRFLAG = 4
            GO TO 1000
          ENDIF
         TTH = HTTOT + 1.5
         TLH = 0
      ENDIF

C      Set the Taper Equation Indicator for the current tree.
C      First, place the Taper Equation number, in <EQNUM(1)>, in the
C      integer scalar variable <TAPEQU>.
C      For the BLM, the taper equation number is the two-digit Species Code
C      plus 700 AND is the same for Scribner and CUBIC volume computations.
C
      IF (BFPFLG.EQ.1 .OR. CUPFLG.EQ.1)then
         CALL BLMTAPEQ(VOLEQ,PROFILE,TAPEQU)
C      The following codes are moved to SUBROUTINE BLMTAPEQ
c         itaper = 0        
c         IF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B01'
c     >               .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
c            TAPEQU = ITAPER
c	      PROFILE = 1
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B01'
c     >               .AND. ITAPER .EQ. 0) THEN
c            TAPEQU = 1
c	      PROFILE = 1
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B02'
c     >               .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
c            TAPEQU = ITAPER
c	      PROFILE = 2
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B02'
c     >               .AND. ITAPER .EQ. 0) THEN
c            TAPEQU = 2
c	      PROFILE = 2
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B03'
c     >                .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
c            TAPEQU = ITAPER
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B03'
c     >               .AND. ITAPER .EQ. 0) THEN
c            TAPEQU = 3
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B04')THEN
c            TAPEQU = 4
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'211')THEN
c            TAPEQU = 5
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B05')THEN
c            TAPEQU = 6
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'B01')THEN
c            TAPEQU = 10
c	      PROFILE = 3
c         ELSEIF(VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'B00')THEN
c            TAPEQU = 11 
c	      PROFILE = 3
c         ELSEIF(VOLEQ(8:10).EQ.'116')THEN
c            TAPEQU = 12 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'117')THEN
c            TAPEQU = 13 
c	      PROFILE = 4
c         ELSEIF(VOLEQ(8:10).EQ.'119')THEN
c            TAPEQU = 14 
c	      PROFILE = 5
c         ELSEIF(VOLEQ(8:10).EQ.'108')THEN
c            TAPEQU = 15 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'231')THEN
c            TAPEQU = 20 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'631')THEN
c            TAPEQU = 21 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'351')THEN
c            TAPEQU = 22 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'998')THEN
c            TAPEQU = 23 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'312')THEN
c            TAPEQU = 24 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'361')THEN
c            TAPEQU = 25 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'431')THEN
c            TAPEQU = 26 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'542')THEN
c            TAPEQU = 27 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'747')THEN
c            TAPEQU = 28 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'800')THEN
c            TAPEQU = 29 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'015' .AND. VOLEQ(1:3).EQ.'B01')THEN
c            TAPEQU = 30
c	      PROFILE = 6
c         ELSEIF(VOLEQ(8:10).EQ.'015' .AND. (VOLEQ(1:3).EQ.'B00' .OR. 
c     >                  VOLEQ(1:3).EQ.'B02'))THEN
c            TAPEQU = 31 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'021')THEN
c            TAPEQU = 32
c	      PROFILE = 7
c         ELSEIF(VOLEQ(8:10).EQ.'017')THEN
c            TAPEQU = 33 
c	      PROFILE = 6
c         ELSEIF(VOLEQ(8:10).EQ.'011')THEN
c            TAPEQU = 34 
c	      PROFILE = 7
c         ELSEIF(VOLEQ(8:10).EQ.'022')THEN
c            TAPEQU = 35 
c	      PROFILE = 7
c         ELSEIF(VOLEQ(8:10).EQ.'093')THEN
c            TAPEQU = 41 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'098')THEN
c            TAPEQU = 42 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'260' .OR. VOLEQ(8:10).EQ.'263')THEN
c            TAPEQU = 48 
c	      PROFILE = 8
c         ELSEIF(VOLEQ(8:10).EQ.'081')THEN
c            TAPEQU = 51 
c	      PROFILE = 9
c         ELSEIF(VOLEQ(8:10).EQ.'042')THEN
c            TAPEQU = 52             
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'041')THEN
c            TAPEQU = 53 
c	      PROFILE = 10
c         ELSEIF(VOLEQ(8:10).EQ.'242')THEN
c            TAPEQU = 54 
c	      PROFILE = 9
c         ELSEIF(VOLEQ(8:10).EQ.'073')THEN
c            TAPEQU = 55 
c	      PROFILE = 9
c         ELSE
c            TAPEQU = 56
c	      PROFILE = 10
c         ENDIF
      ENDIF

C     Second, set the taper equation array subscript, <PROFILE>, according to
C     the (Species Code plus 700), provided in the VOLUME EQUATIONs given
C     in the Key File, which, for the BLM, is the Equation Number,
C     and is now the value of <TAPEQU>:

c      IF (TAPEQU.EQ.1) THEN
c        PROFILE = 1
c      ELSEIF (TAPEQU.EQ.2) THEN
c        PROFILE = 2
c      ELSEIF (TAPEQU.EQ.3.OR.TAPEQU.EQ.4.OR.
c     >        TAPEQU.EQ.6.OR.TAPEQU.EQ.55) THEN
c        PROFILE = 3
c      ELSEIF (TAPEQU.GE.10.AND.TAPEQU.LE.14) THEN
c        PROFILE = 4
c      ELSEIF (TAPEQU.EQ.30.OR.TAPEQU.EQ.31.OR.
c     >        TAPEQU.EQ.33.OR.TAPEQU.EQ.41.OR.
c     >        TAPEQU .EQ. 42) THEN
c        PROFILE = 5
c      ELSEIF (TAPEQU.EQ.32.OR.TAPEQU.EQ.34.OR.
c     >        TAPEQU.EQ.35) THEN
c        PROFILE = 6
c      ELSEIF (TAPEQU.EQ.48) THEN
c        PROFILE = 7
c      ELSEIF (TAPEQU.GE.51.AND.TAPEQU.LE.54) THEN
c        PROFILE = 8
c      ELSE
c        PROFILE = 9
c      ENDIF
      
      CALL DOUBLE_BARK (TAPEQU, DBHOB, DBHIB)
c     Added arror catch for equations without Double bark equation (03/26/2015)      
      IF(DBHIB.LE.0.0001) THEN
        ERRFLAG = 14
        RETURN
      ENDIF
      IF (MTOPP.LE.0)THEN
         MTOPP = anint((0.184*DBHOB)+2.24)
      ENDIF
      
c     small tree fixes for FVS
c     Tree must be measured in feet.
      IF (tth.gt.0.0) THEN

          IF(TTH.LE.17.8) THEN  
                TOTCUB=0.00272708*(DBHIB*DBHIB)*TTH
                VOL(1) = TOTCUB
                goto 1000
          ELSE
            
            SMD_17 = AINT(SQRT(DBHIB*DBHIB-(DBHIB*DBHIB)*17.3/TTH)+0.5)
            
            IF(SMD_17.LT.MTOPP) THEN
                TOTCUB=0.00272708*(DBHIB*DBHIB)*TTH
                VOL(1) = TOTCUB
                goto 1000
            ENDIF
          ENDIF
      ENDIF  
         
c      if(VOLEQ(4:6).eq.'b32' .or. VOLEQ(4:6).EQ.'B32') THEN
c          HTLOG = 32.6
c      ELSE
          HTLOG = 16.3
c      ENDIF
C **************************************************************
C **************************************************************
c      total cubic volume calculations
C **************************************************************
c

      if(cupflg.eq.1) then
         D17 = ANINT((DBHOB*FCLASS) / 100.0)
         call BLMTCUB (PROFILE, DBHOB, TTH, TLH, D17, MTOPP, HTLOG,
     >                    TCVOL)
         vol(1) = tcvol
      endif


C **************************************************************
C **************************************************************
C           BOARD FOOT CACULATIONS  (DBHOB GE 7.0)              *
C **************************************************************

C   DETERMINE IF A BOARD FOOT PRODUCT REPRESENTATION IS NEEDED

      IF (BFPFLG.EQ.1 .OR. CUPFLG.EQ.1) THEN
C *************** USE STEM PROFILE MODEL TO DETERMINE VOLUME
         COR ='N'
         EVOD =  2
         MAXLEN = 16.0
c              MAXLEN = 32.0
         MINLEN = 8.0
         OPT = 23              !changed from 21, 10/11/2001 klc
         STUMP =  1.0

         TRIM = 0.3
         MERCHL = 8
C         ROUNDING IN ACCORDANCE WITH BLM STANDARDS.
          D17 = ANINT ((DBHOB*FCLASS) / 100)

         IF (HTTYPE.EQ.'L'.OR.HTTYPE.EQ.'l') THEN
c check for half logs
            NUMSEG = INT(TLH)
            HALFLOG = TLH - NUMSEG
            LOGDIA(1,2) = 0.0
            DO 102, I = 1, NUMSEG
               HGT2 = FLOAT(I)
           
               CALL BLMTAP (DBHOB,TTH,TLH,HGT2,D17,MTOPP,HTLOG,D2,
     >                         PROFILE)


            IF (DEBUG%MODEL) THEN
              WRITE  (LUDBG, 400)'  DBHOB   TTH    TLH     HGT2   D17'
     &        //'   MTOPP  HTLOG  D2   PROFILE'
400            FORMAT (A)
  		         WRITE  (LUDBG, 420)DBHOB,TTH,TLH,HGT2,D17,MTOPP,HTLOG,
     >         D2, PROFILE
420            FORMAT(9F7.1) 
            ENDIF



C               PUTS DIAMETERS IN PROPER DIAMETER CLASS
C               ROUNDING CONDITION USED WAS .49 WHICH IS IN
C               ACCORDANCE WITH BLM'S STANDARDS.
               LOGDIA(I+1,2) = D2
               LOGLEN(I) = 16.0
  102       CONTINUE
            IF(HALFLOG.GT.0.0) THEN
               HGT2 = FLOAT(NUMSEG) + HALFLOG
               CALL BLMTAP (DBHOB,TTH,TLH,HGT2,D17,MTOPP,HTLOG,D2,
     >                         PROFILE)

               NUMSEG = NUMSEG + 1
               LOGDIA(NUMSEG+1,2) = D2
               if(VOLEQ(4:6).eq.'b32' .or. VOLEQ(4:6).EQ.'B32') THEN
                  LOGLEN(NUMSEG) = 16.0
               ELSE
                  LOGLEN(NUMSEG) = 8.0
               ENDIF
            ENDIF
            NOLOGP = NUMSEG
         ELSE

C             LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO
C             SPECIFIED TOP, INCLUDING TRIM.
            CALL BLMMLEN (PROFILE,TTH,TLH,DBHOB,D17,STUMP,MTOPP,
     >                        HTLOG,LMERCH)
            HT1PRD = LMERCH + STUMP
C             CHECK FOR A MINIMUM MERCH STEM LENGTH
            IF (LMERCH.LT.MERCHL) GO TO 1000

C             SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
C             MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
C             OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
C             RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.

            CALL NUMLOG (OPT, EVOD, LMERCH, MAXLEN, MINLEN,
     >                       TRIM,NUMSEG)


C             SUBROUTINE "SEGMNT" WILL DETERMINE THE LENGTH OF EACH
C             SEGMENT, GIVEN A MERCHANTABLE LENGTH OF TREE STEM AND
C             THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
C             "NUMLOG").  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
C             ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
C             ESTIMATOR HANDBOOK FSH ???.
C             NOTE - THE VARIABLE LMERCH WHEN PASSED IS THE TOTAL
C                    MERCH LENGTH, BUT IS RETURNED AS THE MERCH LENGTH
C                    WITH TRIM AND UNUSABLE PORTION OF THE TOP SUBTRACTED.

            CALL SEGMNT (OPT, EVOD, LMERCH, MAXLEN, MINLEN,
     >                       TRIM, NUMSEG, LOGLEN)

C           NOLOGP = LMERCH / 16.0
           NOLOGP = NUMSEG

C             SUBROUTINE "BLMGDIB" IS INTERNAL AND USES PROFILE MODEL

            CALL BLMGDIB (PROFILE,MTOPP,TTH,TLH,DBHOB,DBHIB,D17,STUMP,
     >                     TRIM, NUMSEG,HTLOG,LOGLEN, LOGDIA)

         ENDIF


C************************************************************
C      GET SCRIBNER VOLUME FOR THE PIECES                   *
C************************************************************
         IF(BFPFLG.EQ.1) THEN
      
            DO 250 I = 1, NUMSEG
C Ref:         DIB = LOGDIA(I+1,2)
               DIB = ANINT (LOGDIA(I+1,2))
               LOGDIA(I+1,1)= DIB
               LENGTH = LOGLEN(I)
C   Move to Scribner Factor volumes       11/2000 klc
C               IF (DIB.EQ.5.0)  THEN
C                 LOGV = 16.0 * (LENGTH/16.0)
C               ELSEIF (DIB.EQ.6.0)  THEN
C                 LOGV = 21.0 * (LENGTH/16.0)
C               ELSEIF (DIB.EQ.7.0)  THEN
C                 LOGV = 25.0 * (LENGTH/16.0)
C               ELSE
C                 LOGV = (0.79*(DIB**2)-2*DIB-4.0)*(LENGTH/16.0)
C               ENDIF
               CALL SCRIB (DIB,LENGTH,COR,LOGV)
      
               LOGVOL(1,I) = ANINT(LOGV)        !ROUND EACH LOG VOLUME
!               write(LUDBG, '(F8.1)')LOGVOL(1,I)
c               VOL(2) = VOL(2) + LOGVOL(1,I)

c  international 1/4                 
               call INTL14(DIB,LENGTH,BFINT)
               vol(10) = bfint + VOL(10)     !Added +VOL(10) YW 09/18/13
 250        CONTINUE
c      if 32 foot volume is asked for do the following
c****************************************************************
c     find volume for 32 foot logs
            if((voleq(4:6).eq.'b32' .or. 
     >                        voleq(4:6).eq.'B32')) then

                  DO 260 I = 2, NUMSEG,2
C Ref:            DIB = LOGDIA(I+1,2)
                  DIB = INT (LOGDIA(I+1,2))
                  LENGTH = LOGLEN(I) + loglen(I-1)

C                  IF (DIB.EQ.5.0)  THEN
C                      LOGV = 16.0 * (LENGTH/16.0)
C                  ELSEIF (DIB.EQ.6.0)  THEN
C                      LOGV = 21.0 * (LENGTH/16.0)
C                  ELSEIF (DIB.EQ.7.0)  THEN
C                      LOGV = 25.0 * (LENGTH/16.0)
C                  ELSE
C                      LOGV = (0.79*(DIB**2)-2*DIB-4.0)*(LENGTH/16.0)
C                  ENDIF
                  CALL SCRIB (DIB,LENGTH,COR,LOGV)
                  LOGVOL32(I) = ANINT(LOGV)
 260           CONTINUE
c    DIVIDE 32 volumes into 16 foot pieces
C
               DO 261, i=2,numseg,2
C                  topv16 = logvol(1,i)
C                  botv16 = logvol(1,i-1)
C                   R = TOPV16 / (TOPV16 + BOTV16)
                  logvol(1,I-1) = ANINT(logvol32(i)/2.0)
                  logvol(1,I) = logvol32(i) - logvol(1,i-1) 
 261           continue
            endif
c ************** 32 foot logs endif ******************************

C  ROUND LOG VOLUMES BEFORE SUMMING THE TREE
            DO 265, i = 1,numseg
               LOGV = ANINT(LOGVOL(1,I))
               LOGVOL(1,I) = LOGV
               VOL(2) = VOL(2) + LOGVOL(1,I)
  265       CONTINUE
C                         !  MINIMUM STEM LENGTH
C                         !  EQNUM(1) EQUAL TO A VALID EQUATION
         ENDIF

         IF (VOL(2).LT.0.0) THEN
            VOL(2) = 0.0
         ENDIF

C************************************************************
C      GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA      *
C************************************************************
         IF (CUPFLG.EQ.1) THEN
C    USE DIB AT DBHOB FOR LARGE END BUTT LOG
            LOGDIA(1,2) = DBHIB
C Ref:      DIBL = LOGDIB(1)
            DIBL = ANINT (LOGDIA(1,2))
            LOGDIA(1,1) = DIBL
            DO 350 I = 1,NUMSEG
c           truncate diameter for 32foot board 6/2001
               DIBS = ANINT (LOGDIA(I+1,2))
               LOGDIA(I+1,1) = DIBS
               LENGTH = LOGLEN(I)
C             IF (I.EQ.1) THEN
C     ===============================================================
C     Removed  20 Sep 93, RJM, OSO 931.5, as requested by J. Alegria.
C **            LOGVOL = 0.005454 * (16.0 *
C ** >                   (BLMBUTT(1,PROFILE) * D17**2 +
C ** >                    BLMBUTT(2,PROFILE) * (DBHOB**2)) *
C ** >                   DBHOB**BLMBUTT(3,PROFILE))
C     ===============================================================
C     Included 20 Sep 93, RJM, OSO 931.5, as requested by J. Alegria.:
C     ===============================================================
C               LOGVOL = 0.00272708 * (DBHIB**2 + D17**2  ) * LENGTH
C             ELSE
               LOGV = .00272708 * (DIBL*DIBL+DIBS*DIBS) * LENGTH
C     ===============================================================
C             ENDIF
               LOGVOL(4,I) = (ANINT(LOGV*10.0)/10.0)
               VOL(4) = VOL(4) + LOGVOL(4,I)
               DIBL = DIBS
  350       CONTINUE
         ENDIF
C--       (ENDIF FOR VALID EQUATION)
         IF (VOL(4).LT.0.0) THEN
            VOL(4) = 0.0
         ENDIF

      ENDIF
C********************************************************

C--   SET TLOGS TO NUMBER OF SEGMENTS MAIN STEM

      TLOGS = NUMSEG

C--   NUMSEG IS ZEROED OUT FOR NOW UNTIL TOPWOOD LOGIC IS IMPLEMENTED
      NUMSEG = 0

C*************************************************************
C*************************************************************
C--    DETERMINE TOP WOOD CUBIC FOOT PRODUCTS (ALL TREES)    *
C*************************************************************
C--     (ENDIF FOR CUBIC FOOT  TOP WOOD EQUATIONS)
C       ENDIF


C      DO 630 NNN = TLOGS + 1, TLOGS + NUMSEG
C          VOL(8) = VOL(8)+LOGVOL(6,NNN)
C 630  CONTINUE

      TLOGS = TLOGS + NUMSEG

 1000 CONTINUE

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, '(A)')'<--Exit BLMVOL'
      ENDIF
      RETURN
      END



C#############################################################
C#############################################################

C   THE LOGIC BETWEEN THE DOUBLE ROWS OF "#" MUST BE DUPLICATED
C   IN EACH REGIONAL VOLUME ROUTINE WITH THE CALLS TO THE STEM
C   PROFILE MODELS MODIFIED TO POINT TO THE APPROPRIATE PROFILE
C   MODEL.  ALL CALLS ARE PRECEDED BY A SHORT ROW OF "#".

C     THERE ARE THREE INTERNAL SUBROUTINES IN THIS SECTION.
C     THESE INTERNAL SUBROUTINES CONTAIN ALL OF THE CALLS TO
C     STEM PROFILE MODELS.

C     SUBROUTINE "BLMTCUB" WILL COMPUTE TOTAL CUBIC VOLUME.

C     SUBROUTINE "BLMMLEN" WILL FIND THE MERCHANTABLE LENGTH TO
C     A SPECIFIED TOP.

C     SUBROUTINE "LOGDIA" WILL RETURN THE END DIAMETERS OF LOGS
C     ONCE SEGMENTATION HAS OCCURED.

C##############################################################
C##############################################################




C**************************************************************
C**************************************************************
      SUBROUTINE BLMTCUB (PROFILE, DBHOB, TTH, TLH, D17, TOP, HTLOG,
     >                    TCVOL)
C**************************************************************

C--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
C--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
C--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
C--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
C--   TO THE TIP.

C--   VARIABLES OF INTEREST ARE:
C--   DBHOB - REAL - DIAMETER BREAST HEIGHT
C--   TTH - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
C--   TLH - REAL - **NUMBER OF 16 FOOT LOGS IN TREE.**
C--   PROFILE - INTEGER - EQUATION NUMBER TO USE
C--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
C--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

      INTEGER HTLOOP, PROFILE,I
      REAL DBHOB, D2, D2OLD, TTH, HGT2, TCVOL, VOL, STUMPDIB
      REAL TLH, TLH1,D17,TOP,HTLOG,R
      REAL RDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = TLH
C

      IF (TTH.GT.0.0) THEN

        HTLOOP = INT ((TTH + .5 - 1.0) / 4.0)
        HGT2 = 1.0
        TLH1 = 0.0

C##########
        CALL BLMTAP (DBHOB,TTH,TLH1,HGT2,D17,TOP,HTLOG,D2,PROFILE)
C Ref:STUMPDIB = D2
        STUMPDIB = ANINT ( D2 )

C--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
        R = D2 / 2.0
        TCVOL = (3.1416 * R*R) / 144.0

        DO 10 I = 1,HTLOOP
          D2OLD = D2
          HGT2 = HGT2 + 4.0

C##########
          CALL BLMTAP (DBHOB,TTH,TLH1,HGT2,D17,TOP,HTLOG,D2,PROFILE)

C--     USE SMALIAN FOR NON STUMP SEGMENTS
          VOL = .00272708 * (D2OLD*D2OLD + D2*D2) * 4.0
          TCVOL =  TCVOL + VOL

   10   CONTINUE

C--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
        IF ((TTH - HGT2).GT.0.0) THEN
          VOL = .00272708 * (D2*D2) * (TTH - HGT2)
          TCVOL = TCVOL + VOL
        ENDIF
      ENDIF
      RETURN
      END


C**************************************************************
C**************************************************************
      SUBROUTINE BLMMLEN (PROFILE, TTH, TLH, DBHOB, D17, STUMP, TOP,
     >                    HTLOG, LMERCH)
C**************************************************************

C--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
C--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
C--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
C--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
C--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.

      INTEGER  I, FIRST, HALF, LAST, PROFILE, TOPLOP,TOP1
      REAL DBHOB, D2, TTH, HGT2, LMERCH, STUMP, TOP, D17, TLH
      REAL HTLOG
     

C--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED
      TOP1 = INT(AINT (TOP*10.0))

      FIRST = 1
C--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
      LAST = INT (TTH + 0.5) * 10
      TOPLOP = LAST


      DO 10 I = 1,TOPLOP
        IF (FIRST.EQ.LAST) GO TO 100
        HALF = (FIRST + LAST + 1) / 2
        HGT2 =  FLOAT(HALF) / 10.0

C###########
        CALL BLMTAP (DBHOB,TTH,TLH,HGT2,D17,TOP,HTLOG,D2,PROFILE)

C--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
C--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
C--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

        D2 = AINT ((D2 + 0.005) * 10.0)
        IF (TOP1.LE.D2) THEN
C--        MOVE UP STEM
           FIRST = HALF
        ELSE
C--        MOVE DOWN THE STEM
           LAST = HALF - 1
        ENDIF
   10 CONTINUE

  100 CONTINUE

      LMERCH = FLOAT (FIRST) / 10.0 - STUMP
      IF (LMERCH.LT.0.0) LMERCH = 0.0

      RETURN
      END

 

C**************************************************************
C**************************************************************
      SUBROUTINE BLMGDIB (PROFILE,TOP,TTH,TLH,DBHOB,DBHIB,D17,STUMP,
     >                    TRIM,NUMSEG,HTLOG,LOGLEN,LOGDIA)
C**************************************************************

C--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
C--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)

      INTEGER NUMSEG, PROFILE, I

      REAL TOP, DBHOB, D2, TTH, HGT2, LOGLEN(20),DBHIB
      REAL LOGDIA(21,3), STUMP, TRIM, D17, TLH, HTLOG

      DO 10 I = 1, 21
         LOGDIA(I,2) = 0.0
   10 CONTINUE

      IF (NUMSEG.EQ.0) GO TO 1000

      IF (STUMP.GT.4.5) THEN

C--     USE FOR TOP WOOD

C###########
        CALL BLMTAP (DBHOB,TTH,TLH,STUMP,D17,TOP,HTLOG,D2,PROFILE)
        LOGDIA(1,2) = D2
      ELSE

C--     USE FOR MAIN STEM

C###########
C        CALL BLMTAP (DBHOB,TTH,TLH,4.5,D17,TOP,HTLOG,D2,PROFILE)
        LOGDIA(1,2) = DBHIB
      ENDIF

      HGT2 = STUMP
      DO 20 I = 1, NUMSEG
         HGT2 = HGT2 + TRIM + LOGLEN(I)

C###########
         CALL BLMTAP (DBHOB,TTH,TLH,HGT2,D17,TOP,HTLOG,D2,PROFILE)
         LOGDIA(I+1,2) = D2
   20 CONTINUE

C--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
C--   ROUNDING CONDITION USED WAS .49 WHICH IS IN ACCORDANCE WITH
C--   BLM'S STANDARDS.
C Ref:DO 30 I = 1, NUMSEG+1
C Ref:    DIBCLS = INT (LOGDIB(I))
C Ref:    XXX = LOGDIB(I) - DIBCLS
C Ref:    IF (XXX .GT. 0.49) DIBCLS = DIBCLS + 1.0
C Ref:    LOGDIB(I) = DIBCLS
C Ref:   30 CONTINUE

C--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
C--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
C--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
C--  MINIMUM SPECIFIED TOP.

      IF (LOGDIA(NUMSEG+1,2).LT.TOP) THEN
          LOGDIA(NUMSEG+1,2) = TOP
      ENDIF

 1000 CONTINUE
      RETURN
      END



C**************************************************************
C**************************************************************
      SUBROUTINE DOUBLE_BARK (TAPEQU, DBHOB, DBHIB)
C**************************************************************

      INTEGER TAPEQU
      REAL DBHOB, DBHIB

C     THIS IS INTENDED TO BE IN (used by {RJM}) THE BLMVOL.F77 SUBROUTINE
C     Purpose:        Compute the Double Bark Thickness, <DBHIB>, according
C                     to the given species code (= 700 - <TapEqu>) & DBHOB.

C     Input:  <TapEqu>: Taper Equation Number
C     Input:  <DBHOB>:    Diameter @ Breast Height
C     Output: <DBHIB:>  The Large End Diameter OF A BUTT LOG!

C     by      J. Alegria & R. Miller, OSO 930, 20 Sep 93

C     Note:   All of the Bark Thickness Equations are explicitly coded
C             for ease of reference.

C     Usage:
C             Prepare the Taper Equation Number
C             Call Double_Bark (TapEqu, DBHIB)
C             Use <DBHIB> as the large diameter in computing the CUBIC
C             Volume of the BUTT LOG by means of the Smalian Formula.

      IF (TAPEQU.EQ.1.OR.TAPEQU.EQ.2.OR.TAPEQU.EQ.3.OR.
     >    TAPEQU.EQ.5.OR.TAPEQU.EQ.35)THEN
C     DOUGLAS-FIR (LARSEN & HANN, 1985)
        DBHIB = 0.903563 * DBHOB**0.989388

      ELSEIF (TAPEQU .EQ. 11  .OR. TAPEQU .EQ. 12)    THEN
C     PONDEROSA AND JEFFREY PINE (LARSEN & HANN, 1985)
        DBHIB = 0.809427 * DBHOB**1.016866

      ELSEIF (TAPEQU .EQ. 13 .OR. TAPEQU .EQ. 14) THEN
C     Added TAPEQN 14 for white pine per BLM 3/26/2015 (Kristen Thompson, state cruiser)      
C     SUGAR PINE (LARSEN & HANN, 1985)
C => ?  DBHIB = 0.859045 * DBHOB**1.000000
        DBHIB = 0.859045 * DBHOB

      ELSEIF (TAPEQU .EQ. 15) THEN
C     LODGEPOLE PINE (SPADA, 1960)
        DBHIB = DBHOB - (0.3147 + 0.0274 * DBHOB)

      ELSEIF (TAPEQU.EQ.20.OR.TAPEQU.EQ.25) THEN
C     PACIFIC YEW AND PACIFIC MADRONE
        DBHIB = -0.03425 + 0.98155 * DBHOB

      ELSEIF (TAPEQU.EQ.21) THEN
C     TAN OAK (PILLSBURY, 1984)
        DBHIB = -4.36852 + 0.95354 * DBHOB + 0.18307 * 4.5

      ELSEIF (TAPEQU.EQ.22.OR.TAPEQU.EQ.23.OR.
     >         TAPEQU.EQ.24.OR.TAPEQU.EQ.26.OR.
     >         TAPEQU.EQ.27) THEN
C     RED ALDER, OREGON MYRTLE, BIG LEAF MAPLE
C     GOLDERN CHIQUAPIN AND OREGON ASH (PILLSBURY, 1984)
        DBHIB = 0.3953 4 + 0.90182 * DBHOB

      ELSEIF (TAPEQU.EQ.28.OR.TAPEQU.EQ.29) THEN
C     BLACK COTTONWOOD AND OAK SPECIES (OREGON WHITE: PILLSBURY, 1984)
        DBHIB = -0.78034 + 0.95956 * DBHOB

      ELSEIF (TAPEQU.EQ.31.OR.TAPEQU.EQ.33) THEN
C     WHITE AND GRAND FIR (LARSEN & HANN, 1985)
C => ?  DBHIB = 0.904973 * DBHOB**1.000000
        DBHIB = 0.904973 * DBHOB

      ELSEIF (TAPEQU.EQ.32.OR.TAPEQU.EQ.34) THEN
C     RED FIR & PACIFIC SILVER FIR (DOLPH, 1989)
        DBHIB = 0.86951 * DBHOB**1.00983

      ELSEIF (TAPEQU.EQ.41.OR.TAPEQU.EQ.42) THEN
C     ENGELMANN AND SITKA SPRUCE (SPADA, 1960)
        DBHIB = DBHOB - (0.2113 + 0.0445 * DBHOB)

      ELSEIF (TAPEQU.EQ.48.OR.TAPEQU.EQ.56) THEN
C     HEMLOCK AND MISCELLANEOUS (FINCH, 1948)
        DBHIB = DBHOB / 1.071

      ELSEIF (TAPEQU.EQ.52.OR.TAPEQU.EQ.54) THEN
C     ALASKA YELLOW CEDAR AND WESTERN RED CEDAR (FINCH, 1948)
        DBHIB = DBHOB / 1.053

      ELSEIF (TAPEQU.EQ.51.OR.TAPEQU.EQ.53) THEN
C     INCENSE AND PORT ORFORD CEDAR (LARSEN & HANN, 1985)
C => ?  DBHIB = 0.837291 * DBHOB**1.000000
        DBHIB = 0.837291 * DBHOB

      ELSEIF (TAPEQU.EQ.55) THEN
C     WESTERN LARCH
        DBHIB = DBHOB - (0.1231 + 0.1306 * DBHOB)

      ENDIF

      RETURN
      END

C **************************************************************
      SUBROUTINE BLMTAPEQ(VOLEQ,PROFILE,TAPEQU)
      CHARACTER*10 VOLEQ
      INTEGER PROFILE, TAPEQU,ITAPER
         itaper = 0        
         IF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B01'
     >               .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
            TAPEQU = ITAPER
	      PROFILE = 1
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B01'
     >               .AND. ITAPER .EQ. 0) THEN
            TAPEQU = 1
	      PROFILE = 1
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B02'
     >               .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
            TAPEQU = ITAPER
	      PROFILE = 2
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B02'
     >               .AND. ITAPER .EQ. 0) THEN
            TAPEQU = 2
	      PROFILE = 2
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B03'
     >                .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
            TAPEQU = ITAPER
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B03'
     >               .AND. ITAPER .EQ. 0) THEN
            TAPEQU = 3
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B04')THEN
            TAPEQU = 4
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'211')THEN
            TAPEQU = 5
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B05')THEN
            TAPEQU = 6
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'B01')THEN
            TAPEQU = 10
	      PROFILE = 3
         ELSEIF(VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'B00')THEN
            TAPEQU = 11 
	      PROFILE = 3
         ELSEIF(VOLEQ(8:10).EQ.'116')THEN
            TAPEQU = 12 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'117')THEN
            TAPEQU = 13 
	      PROFILE = 4
         ELSEIF(VOLEQ(8:10).EQ.'119')THEN
            TAPEQU = 14 
	      PROFILE = 5
         ELSEIF(VOLEQ(8:10).EQ.'108')THEN
            TAPEQU = 15 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'231')THEN
            TAPEQU = 20 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'631')THEN
            TAPEQU = 21 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'351')THEN
            TAPEQU = 22 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'998')THEN
            TAPEQU = 23 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'312')THEN
            TAPEQU = 24 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'361')THEN
            TAPEQU = 25 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'431')THEN
            TAPEQU = 26 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'542')THEN
            TAPEQU = 27 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'747')THEN
            TAPEQU = 28 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'800')THEN
            TAPEQU = 29 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'015' .AND. VOLEQ(1:3).EQ.'B01')THEN
            TAPEQU = 30
	      PROFILE = 6
         ELSEIF(VOLEQ(8:10).EQ.'015' .AND. (VOLEQ(1:3).EQ.'B00' .OR. 
     >                  VOLEQ(1:3).EQ.'B02'))THEN
            TAPEQU = 31 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'021')THEN
            TAPEQU = 32
	      PROFILE = 7
         ELSEIF(VOLEQ(8:10).EQ.'017')THEN
            TAPEQU = 33 
	      PROFILE = 6
         ELSEIF(VOLEQ(8:10).EQ.'011')THEN
            TAPEQU = 34 
	      PROFILE = 7
         ELSEIF(VOLEQ(8:10).EQ.'022')THEN
            TAPEQU = 35 
	      PROFILE = 7
         ELSEIF(VOLEQ(8:10).EQ.'093')THEN
            TAPEQU = 41 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'098')THEN
            TAPEQU = 42 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'260' .OR. VOLEQ(8:10).EQ.'263')THEN
            TAPEQU = 48 
	      PROFILE = 8
         ELSEIF(VOLEQ(8:10).EQ.'081')THEN
            TAPEQU = 51 
	      PROFILE = 9
         ELSEIF(VOLEQ(8:10).EQ.'042')THEN
            TAPEQU = 52             
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'041')THEN
            TAPEQU = 53 
	      PROFILE = 10
         ELSEIF(VOLEQ(8:10).EQ.'242')THEN
            TAPEQU = 54 
	      PROFILE = 9
         ELSEIF(VOLEQ(8:10).EQ.'073')THEN
            TAPEQU = 55 
	      PROFILE = 9
         ELSE
            TAPEQU = 56
	      PROFILE = 10
         ENDIF      
      RETURN
      END

C **************************************************************
C       As of December 14, 1993, all diameters are kept as computed
C       (UN-rounded).  Diameters are ROUNDED only for the purpose of
C       computing a segment volume.
C
C       As agreed by W. Abbey, J. Alegria, and G. Willits, on December
C       13th, 1993.  RJM. OSO 931.5.
C **************************************************************

C --  VARIABLES OF INTEREST ARE:

C --  COR    - CHARACTER - FLAG TO USE SCRIBNER OR SCRIBNER
C --                       FACTORS TO GENERATE SCRIBNER PRODUCT
C --  DBHOB    - REAL      - DIAMETER BREAST HEIGHT
C --  DBHIB  - REAL      - DIAMETER-INSIDE-BARK @ BREAST HEIGHT
C --  EVOD    - INTEGER - EVEN OR ODD LENGTH SEGMENTS ALLOWED
C --            SEGMENTATION OPTIONS 11-14 ALLOW ODD LENGTHS BY
C --            DEFINITION
C --        1 = ODD SEGMENTS ALLOWED
C --        2 = ONLY EVEN SEGMENTS ALLOWED

C --  HT      - REAL -  **TREE HEIGHT IN FT. FROM A 1 FOOT STUMP OR
C --                      TREE HEIGHT IN NUMBER OF 16 FOOT LOGS PER TREE**
C --  TTH     - REAL - TREE HEIGHT FROM GROUND TO TIP
C --  TLH     - REAL - TREE HEIGHT IN LOGS PER TREE
C --  LENMS   - REAL - LENGTH OF MAIN STEM IF A TOP WOOD PRODUCT
C --                   IS DESIRED
C --  LMERCH  - REAL - GIVEN MERCHANTABLE LENGTH OF STEM RETURNED
C --                   FROM SUBROUTINE "BLMMLEN" AND THE MERCHANTABLE
C --                   LENGTH OF STEM WITH TRIM REMOVED RETURNED FROM
C --                   SUBROUTINE "SEGMNT"

C --  LOGLEN - REAL(20) - LOG SEGMENT LENGTHS COMPUTED BY
C --           SUBROUTINE "SEGMNT". DOES NOT INCLUDE TRIM.

C --  LOGVOL - REAL  - THE PRODUCT VOLUME OF A LOG

C --  MAXLEN - REAL  - MAXIMUM SEGMENT LENGTH
C --  MINLEN - REAL  - MINIMUM SEGMENT LENGTH
C --  MERCHL - INTEGER - MINIMUM PRODUCT LENGTH FOR A MERCH TREE

C --  NOLOGP - REAL  - AVERAGE NUMBER OF 16 FOOT LOGS IN MAIN STEM
C --  NOLOGS - REAL  - AVERAGE NUMBER OF 16 FOOT LOGS IN TOP WOOD
C --  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS FROM
C --           SUBROUTINE "NUMSEG"
C --  STUMP  - REAL  - HEIGHT OF STUMP
C --  TAPEQU - Integer - TAPER EQUATION TO USE - FROM EQNUM(1)
C --  TRIM   - REAL  - TRIM LENGTH FOR EACH SEGMENT

C --  VOL    - REAL
C --    VOL(1)  - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT, DOES NOT
C --              INCLUDE LIMBS AND ROOTS.
C --    VOL(2)  - GROSS AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
C --    VOL(3)  - NET AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
C --    VOL(4)  - GROSS AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
C --    VOL(5)  - NET AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
C --    VOL(6)  - AMOUNT OF CORD WOOD PRODUCT IN MAIN STEM
C --    VOL(7)  - GROSS AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
C --    VOL(8)  - NET AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
C --    VOL(9)  - AMOUNT OF CORD WOOD PRODUCT IN TOP WOOD.
C --    VOL(10) - NOT USED

