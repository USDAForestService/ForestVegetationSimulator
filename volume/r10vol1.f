!== last modified  03-20-2006
      SUBROUTINE R10VOL1(EQNUM,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,
     >           HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,
     >           BFPFLG,CUPFLG,SPFLG,ERRFLAG)

C*********************************************************************
C*                 LOCAL VARIABLES                                   *
C*********************************************************************

      CHARACTER*10 EQNUM
      CHARACTER*1 COR,HTTYPE

      INTEGER TLOGS,EVOD,NUMSEG,OPT,PIEST,I,ITMP
      INTEGER NSEG16
      INTEGER CUPFLG,BFPFLG,SPFLG,ERRFLAG,ILOG

      REAL HTX
      REAL TOP,NOLOGS,MERCHL
      REAL DBHOB,HTTOT,HT1PRD,DIB,LENGTH,HMERCH
      REAL LMERCH,LOGDIA(21,3),LOGLEN(20),PIELEN(20),LOGV
      REAL LOGLENG(20),CUBVOL,PIEDIB(21,3),PIEVOL(20)
      REAL MAXLEN,MINLEN,NOLOGP,DUMVOL,PCT,DIBL,DIBS
      REAL STUMP,MTOPP,MTOPS,TRIM,VOL(15),LOGVOL(7,20)
     
      LOGICAL FLAG32

C--   SET ALL POTENTIAL VOLUMES AND AVG NUM OF LOGS TO ZERO
c      LOGDATA=.FALSE.

      DO 10 I=1,15
          VOL(I) = 0.0
   10 CONTINUE

      DO 15 I = 1, 21
        PIEDIB(I,1)=0.0
        PIEDIB(I,2)=0.0
   15 CONTINUE

      DO 20 I=1,20
         LOGLEN(I)=0.0
         PIELEN(I)=0.0
         PIEVOL(I)=0.0
   20 CONTINUE

      TLOGS=0
      NOLOGP=0
      NOLOGS=0
      NUMSEG=0
      NSEG16=0
      LOGV=0.0

C--   IF DBHOB OR HTTOT AND HT1PRD EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
      ERRFLAG = 0
      IF (DBHOB.LT.1.0)THEN
        ERRFLAG = 3
        GOTO 1000
      ENDIF
      IF(HTTOT.LE.0.0 .AND. HT1PRD.LE.0.0) THEN
        ERRFLAG = 4
        GOTO 1000
      ENDIF

C**************************************************************
C**************************************************************
C           BOARD FOOT CACULATIONS  (DBHOB GE 9.0)              *
C**************************************************************

C--   DETERMINE IF A BOARD FOOT PRODUCT REPRESTNTATION IS NEEDED

      IF (BFPFLG.EQ.1 .OR. CUPFLG.EQ.1) THEN
C--*************** USE STEM PROFILE MODEL TO DETERMINE VOLUME
         if(DBHOB .LE. MTOPP) THEN
             ERRFLAG = 11
            GOTO 750
         ENDIF

c   --------- FIND LOG LENGTHS

         COR='Y'
         EVOD=2
         OPT=23
         STUMP=1.0
         TOP=MTOPP

         IF (EQNUM(1:3).EQ.'A16') THEN
            MINLEN=4.0
            MAXLEN=16.0
            MERCHL=8.0
            TRIM=.3
            FLAG32=.FALSE.
         ELSE
            MINLEN=4.0
            MAXLEN=32.0
            MERCHL=16.0
            TRIM=.6
            FLAG32=.TRUE.
         ENDIF
             
C--   SUBROUTINE "R10_HTS" IS INTERNAL AND USES PROFILE MODEL

C--           WILL FIND LMERCH OR HTTOT DEPENDING ON ENTERED HEIGHT
C--           LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO
C--           SPECIFIED TOP, INCLUDING TIP

         CALL R10_HTS(EQNUM,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,TOP,LMERCH)

C--  CHECK FOR A MINIMUM MERCH STEM LENGTH
              
         IF(HT1PRD.LE.0) HT1PRD = LMERCH + STUMP

         IF(LMERCH.GE.MERCHL) THEN

C--  SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
C--  MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
C--  OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
C--  RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.

            IF (HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
               NUMSEG = HT1PRD
               NOLOGP=HT1PRD
               DO 35 I = 1, NUMSEG
                   LOGLEN(I) = MAXLEN
 35            CONTINUE
               IF (FLAG32) THEN
                     NSEG16=INT(HT1PRD*2.0)
                     DO 36 I=1,NSEG16
 36                     PIELEN(I)=16.0
               ENDIF
               itmp = anint((ht1prd-int(ht1prd))*10)
               IF (itmp .eq. 5) THEN
                     NUMSEG=NUMSEG+1
                     LOGLEN(NUMSEG)=MAXLEN/2.0
               ENDIF
            ELSE
               CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,
     >                    TRIM,NUMSEG)
               HMERCH=LMERCH
        
C--  SUBROUTINE "SEGMNT" WILL DETERMINE THE LENGTH OF EACH
C--  SEGMENT, GIVEN A MERCHANTABLE LENGTH OF TREE STEM AND
C--  THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
C--  "NUMLOG").  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
C--  ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
C--  ESTIMATOR HANDBOOK FSH ???.
C--  NOTE - THE VARIABLE LMERCH WHEN PASSED IS THE TOTAL
C--           MERCH LENGTH, BUT IS RETURNED AS THE MERCH LENGTH
C--           WITH TRIM AND UNUSABLE PORTION OF THE TOP SUBTRACTED.

               CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,
     >                       TRIM,NUMSEG,LOGLENG)

               HTX=0.0
               DO 30 I=1,NUMSEG
                    LOGLEN(I)=LOGLENG(I)
                    HTX=HTX+LOGLENG(I)
 30            CONTINUE

               IF (FLAG32) THEN
                   NSEG16=INT(HTX/16.0)
                   IF (NSEG16.LT.HTX/16.0) NSEG16=NSEG16+1
                   CALL SEGMNT(OPT,EVOD,HMERCH,16.0,4.0,
     >                                      0.3,NSEG16,LOGLENG)
                   DO 31 I=1,NSEG16
                       PIELEN(I)=LOGLENG(I)
 31                CONTINUE
               ENDIF

C--  SET NOLOGP TO THE NUMBER OF LOGS FOOT SEGMENTS
               NOLOGP = NUMSEG                    
C               NOLOGP = LMERCH/16.0
            ENDIF
     
C--   SUBROUTINE "R10GDIB" IS INTERNAL AND USES PROFILE MODEL


            CALL R10GDIB(EQNUM,TOP,HTTOT,DBHOB,STUMP,TRIM,TLOGS,
     >              NUMSEG,LOGLEN,LOGDIA)

            IF (FLAG32) THEN
                   CALL R10GDIB(EQNUM,TOP,HTTOT,DBHOB,STUMP,.3,PIEST,
     >                         NSEG16,PIELEN,PIEDIB)
            ENDIF
C************************************************************
C      GET SCRIBNER VOLUME FOR THE PIECES                   *
C************************************************************
            IF(BFPFLG.EQ.1 .AND. DBHOB .GE. 9.0) THEN
               DO 250 I=1,NUMSEG
                   DIB=LOGDIA(I+1,1)
                   LENGTH=LOGLEN(I)
                   IF (DIB.LT.6)  THEN
                     LOGV=0.0
                   ELSEIF((DIB.EQ.6).AND.(LENGTH.GT.3)
     >                      .AND.(LENGTH.LT.10)) THEN
                     LOGV=.5
                   ELSEIF((DIB.EQ.7).AND.(LENGTH.GT.3)
     >                          .AND.(LENGTH.LT.7)) THEN
                     LOGV=.5
                   ELSEIF((DIB.EQ.8).AND.(LENGTH.LT.5)
     >                       .AND.(LENGTH.GT.2)) THEN
                     LOGV=.5
                   ELSE
                     CALL SCRIB (DIB,LENGTH,COR,LOGV)
                   ENDIF

                   VOL(2)=VOL(2)+LOGV
                   LOGVOL(1,I)=LOGV*10
c              call international
                   CALL INTL14(DIB,LENGTH,LOGV)
                   VOL(10) = VOL(10) + LOGV
 250           CONTINUE

C*******************************************************
C IF 32FT EQ THEN NEED TO BREAK LOGS INTO 16FT PIECES  *
C*******************************************************

               IF (FLAG32) THEN
                  DO 251 I=1,NSEG16
                    DIB=PIEDIB(I+1,1)
                    LENGTH=PIELEN(I)
                    IF (DIB.LT.6)  THEN
                          LOGV=0.0
                    ELSEIF((DIB.EQ.6).AND.(LENGTH.GT.3)
     >                           .AND.(LENGTH.LT.10)) THEN
                          LOGV=.5
                    ELSEIF((DIB.EQ.7).AND.(LENGTH.GT.3)
     >                            .AND.(LENGTH.LT.7)) THEN
                          LOGV=.5
                    ELSEIF((DIB.EQ.8).AND.(LENGTH.LT.5)
     >                            .AND.(LENGTH.GT.2)) THEN
                          LOGV=.5
                    ELSE
                          CALL SCRIB (DIB,LENGTH,COR,LOGV)
                    ENDIF

                    PIEVOL(I)=LOGV*10

 251              CONTINUE

C************************************************
C PRO-RATE LOG VOLUMES TO PRODUCE PIECE VOLUMES *
C************************************************
                  ILOG=1
                  DO 252 I=1,NSEG16,2
                       IF (I.GT.20) GOTO 253
C                       IF (PIELEN(I+1).GT.0.0) THEN  ! REPLACED 1/10/2003 WITH PIEVOL CHECK
                       IF (PIEVOL(I+1).GT.0.0) THEN
                          DUMVOL=PIEVOL(I)+PIEVOL(I+1)

                          PCT=PIEVOL(I)/DUMVOL
                          PIEVOL(I)=LOGVOL(1,ILOG)*PCT

                          PCT=PIEVOL(I+1)/DUMVOL
                          PIEVOL(I+1)=LOGVOL(1,ILOG)*PCT
                       ELSE
                          PIEVOL(I)=LOGVOL(1,ILOG)
                          GOTO 253
                       ENDIF
                       ILOG=ILOG+1
 252              CONTINUE

 253              DO 254 I=1,NSEG16
 254                 LOGVOL(1,I)=PIEVOL(I)
               ENDIF


C--    CONVERT FROM SCRIBNER DECIMAL C TO SCRIBNER
               VOL(2)=VOL(2)*10.0

C--    (ENDIF FOR BOARD FOOT)
            ENDIF
            IF (VOL(2) .LT. 0.0) THEN
                VOL(2) = 0.0
            ENDIF
         
C************************************************************
C      GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA     *
C************************************************************

C    USE DIB AT DBHOB FOR LARGE END BUTT LOG
            IF(CUPFLG.EQ.1) THEN
               DIBL=LOGDIA(1,1)
               DO 350 I=1,NUMSEG
                  DIBS = LOGDIA(I+1,1)
                  LENGTH=LOGLEN(I)
                  LOGV=.00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH

                  LOGVOL(4,I)= ANINT(LOGV*10.0)/10.0

                  VOL(4)=VOL(4)+LOGVOL(4,I)

                  DIBL=DIBS
  350          CONTINUE
      
C              (ENDIF FOR CUBIC VOLUMES)
            ENDIF                     
C           END IF FOR MERCH LENGTH
         ENDIF   

C***************************************************************

         IF (VOL(4) .LT. 0.0) THEN
              VOL(4) = 0.0
         ENDIF

C--   (ENDIF FOR BOARD AND CUBIC FOOT EQUATIONS)
      ENDIF


C*************************************************************
C*************************************************************
C--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *
C*************************************************************

C--   ENTER LOGIC FOR TOTAL CUBIC VOLUME HERE
  750 IF ((DBHOB.GE.9.0 .AND. HTTOT.GT.40.0) .AND. CUPFLG.EQ.1) THEN
         call R10TC(EQNUM,DBHOB,HTTOT,CUBVOL)
        VOL(1) = CUBVOL
      ENDIF


C**************************************************************
C**************************************************************
C*      CORD WOOD MAIN STEM EQUATIONS  ALL TREES             *
C**************************************************************


C--   PUT LOGIC HERE FOR CORDS

C**************************************************************
C--   SET TLOGS TO NUMBER OF SEGMENTS MAIN STEM

C--   IF MORE THAN ONE STEM PROFILE EQUATION IS SPECIFIED FOR BDFT,
C--   CUFT, OR CORDS, THEN THEY MUST BE THE SAME.  OTHERWISE
C--   THE DATA IS INVALID AND THE VARIABLES ARE ZEROED OUT.  THE SAME
C--   APPLIES TO THE SPECIFIED TOP DIAMETERS.

C************************************************************
C  IF 32FT LOGS THEN FILL LOG ARRARYS WITH THE PIECE INFO   *
C************************************************************

      IF (FLAG32) THEN
         DO 399 I=1,NSEG16
            LOGDIA(I,1)=PIEDIB(I,1)
            LOGLEN(I)=PIELEN(I)
 399     CONTINUE
         LOGDIA(NSEG16+1,1)=PIEDIB(NSEG16+1,1)
         TLOGS=NSEG16
         NUMSEG=NSEG16
      ENDIF

      TLOGS = NUMSEG

C****************************************************************
C****************************************************************
C--  DETERMINE TOP WOOD CUBIC FOOT PRODUCTS (ALL TREES)
C****************************************************************

     
 1000 CONTINUE
  
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
C     SUBROUTINE "FASTGRO" WILL FIND SMALL TREE CUBIC VOLUMES
C     SUBROUTINE "SECGRO" WILL FIND SMALL TREE CUBIC VOLUMES
C     SUBROUTINE "R10MLEN" WILL FIND THE MERCHANTABLE LENGTH TO
C     A SPECIFIED TOPWOOD TOP DIAMETER.
C     SUBROUTINE "R10GDIA" WILL RETURN THE END DIAMETERS OF LOGS
C     ONCE SEGMENTATION HAS OCCURED.
C     SUBROUTINE "R10_HTS" WILL RETURN EITHER MERCHANTABLE LENGTH
C     OR TOTAL TREE HEIGHT DEPENDING ON WHAT HEIGHT IS GIVEN



C**************************************************************
C**************************************************************
      SUBROUTINE R10MLEN(EQNUM,HTTOT,DBHOB,STUMP,TOP,LMERCH)
C**************************************************************
C--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
C--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
C--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
C--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
C--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.
C--   USED ONLY FOR TOPWOOD VOLUME DETERMINATION

      CHARACTER*10 EQNUM
      INTEGER  I, FIRST, HALF, LAST, TOPLOP
      REAL DBHOB, D2, HTTOT, HTUP, LMERCH, STUMP, TOP,TOP1

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
        HTUP=FLOAT(HALF)/10.0
C###########
        CALL R10TAP(EQNUM,DBHOB,HTTOT,HTUP,D2)

C--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
C--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
C--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

        D2 = INT((D2 + 0.005)*10.0)
        IF (TOP1 .LE. D2) THEN
C--        MOVE UP STEM
           FIRST = HALF
        ELSE
C--        MOVE DOWN THE STEM
           LAST = HALF - 1
        ENDIF
   10 CONTINUE
  100 CONTINUE
      LMERCH = FLOAT(FIRST)/10.0 - STUMP
      IF (LMERCH .LT. 0.0) LMERCH=0.0
      RETURN
      END


C**************************************************************
C**************************************************************
      SUBROUTINE R10GDIB(EQNUM,TOP,HTTOT,DBHOB,STUMP,TRIM,TLOGS,
     >              NUMSEG,LOGLEN,LOGDIA)
C**************************************************************
C--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
C--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)

      CHARACTER*10 EQNUM
      INTEGER NUMSEG,TLOGS,I

      REAL TOP,DBHOB,D2,HTTOT,HTUP,LOGLEN(20)
      REAL LOGDIA(21,3),STUMP,TRIM,XXX,DIBCLS

      DO 10 I=1+TLOGS,21
         LOGDIA(I,2)=0.0
   10 CONTINUE

      IF (NUMSEG .EQ. 0) GO TO 1001
      IF (STUMP .GT. 4.5 ) THEN
C--     USE FOR TOP WOOD
C##########A#
        CALL R10TAP(EQNUM,DBHOB,HTTOT,STUMP,D2)
        LOGDIA(1+TLOGS,2)=D2
      ELSE

C--     USE FOR MAIN STEM
C###########
        CALL R10TAP(EQNUM,DBHOB,HTTOT,4.5,D2)
        LOGDIA(1,2)=D2
      ENDIF
      HTUP = STUMP
      DO 20 I=1+TLOGS,NUMSEG+TLOGS
         HTUP=HTUP+TRIM+LOGLEN(I)
C###########
         CALL R10TAP(EQNUM,DBHOB,HTTOT,HTUP,D2)
         LOGDIA(I+1,2)=D2
   20 CONTINUE
C--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
      DO 30 I=1+TLOGS,NUMSEG+TLOGS+1
          DIBCLS=INT(LOGDIA(I,2))
          XXX=LOGDIA(I,2)-DIBCLS
          IF( XXX .GT. 0.55) DIBCLS=DIBCLS+1.0
          LOGDIA(I,1)=DIBCLS
   30 CONTINUE
C--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
C--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
C--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
C--  MINIMUM SPECIFIED TOP.
      IF( LOGDIA(NUMSEG+TLOGS+1,1) .LT. TOP) THEN
          LOGDIA(NUMSEG + TLOGS + 1,1) = TOP
      ENDIF
 1001 CONTINUE
      RETURN
      END



C*******************************************************************
C*******************************************************************
      SUBROUTINE R10_HTS(EQNUM,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,TOP,
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
         IF (EQNUM(1:3).EQ.'A16') THEN
            LMERCH = HT1PRD*16.3
            HTD = LMERCH +STUMP
         ELSE IF (EQNUM(1:3).EQ.'A32') THEN
            LMERCH = HT1PRD*32.6
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
               BK = BB(D,H)
	      ENDIF
C            FV = 0.005454154*D**2*(H - 4.5)*BK
         ENDIF
         
         XLL(I) = 1.0 - (2./3.)*(LIMD/D)
       
         IF(H.LT.0.01)  HEST = HH(I)/XLL(I)

         IF(HEST.GT.0.01) BK = BB(D,HEST)

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
	         DS(I) = DVREDA(RH,RH32,RH40,HEST,D)
            ELSE
               DS(I) = DD2MI(RH,RH32,D,HEST)
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
	         DXL = DVREDA(RXL,RH32,RH40,HEST,D)
            ELSE
               DXL = DD2MI(RXL,RH32,D,HEST)
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
	         DS(I) = DVREDA(RH,RH32,RH40,H,D)
            ELSE
               DS(I) = DD2MI(RH,RH32,D,H)
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
	         DXL = DVREDA(RXL,RH32,RH40,H,D)
            ELSE
               DXL = DD2MI(RXL,RH32,D,H)
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
	            DS(I) = DVREDA(RH,RH32,RH40,HEST,D)
	         ELSE
                  DS(I) = DD2MI(RH,RH32,D,HEST)
	         ENDIF
               if(ds(i).lt.0.0) ds(i) = 0.0
            ELSE
      	      IF(ISP.EQ.IRA)THEN
	            DS(I) = DVREDA(RH,RH32,RH40,H,D)
	         ELSE
                  DS(I) = DD2MI(RH,RH32,D,H)
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
                  DSX = DVR(RH,RH32,H,DBHOB)
                  BKWRC = BKWR(DBHOB,H)
                  if(dsx.lt.0) dsx = 0
                  DSI = (DSX*BKWRC)**.5 * DBHOB
              
C  ALASKA CEDAR MERCHANTABLE LENGTH
               ELSE IF(ISP.EQ.IAC) THEN 
                  DSX = DVA(RH,RH32,H,DBHOB)
                  BKAYC = BKAC(DBHOB,H)
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
               DSX = DVR(RH,RH32,H,DBHOB)
               BKWRC = BKWR(DBHOB,H)
               if(dsx.lt.0) dsx = 0
               DSI = (DSX*BKWRC)**.5 * DBHOB

C  ALASKA YELLOW CEDAR TAPER EQUATION
            ELSE
 5546          CONTINUE
               DSX = DVA(RH,RH32,H,DBHOB)   
               BKAYC = BKAC(DBHOB,H)
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
               DSX = DVR(RH,RH32,H,DBHOB)
               BKWRC = BKWR(DBHOB,H)
               if(dsx.lt.0) dsx = 0
               DSI = (DSX*BKWRC)**.5 * DBHOB

C  ALASKA YELLOW CEDAR TAPER EQUATION
            ELSE
               DSX = DVA(RH,RH32,H,DBHOB)
               BKAYC = BKAC(DBHOB,H)
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

C**************************************************************
C**************************************************************
      SUBROUTINE R10TC(VOLEQ,DBHOB,HTTOT,TCVOL)
C**************************************************************

C--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
C--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
C--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
C--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
C--   TO THE TIP.

C--   VARIABLES OF INTEREST ARE:
C--   totht - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
C--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
C--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

      CHARACTER*10 VOLEQ
      INTEGER HTLOOP,I
      REAL D2, D2OLD, HT2, TCVOL, VOLTMP
      REAL DBHOB,HTTOT,R

      HTLOOP = INT((HTTOT - 1.0)/4.0)
      HT2=1.0
C##########
      CALL R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
C--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
      R=D2/2.0
      TCVOL = (3.1416*R*R)/144.0
      DO 10 I = 1,HTLOOP
         D2OLD = D2
         HT2=HT2+4.0
C##########
         CALL R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
C--     USE SMALIAN FOR NON STUMP SEGMENTS
         VOLTMP = .00272708*(D2OLD*D2OLD+D2*D2)*4.0
         TCVOL = TCVOL + VOLTMP
   10 CONTINUE
C--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
      IF ( (HTTOT-HT2) .GT. 0.0 ) THEN
         VOLTMP = .00272708*(D2*D2)*(HTTOT-HT2)
         TCVOL = TCVOL + VOLTMP
      ENDIF
      RETURN
      END

C--   THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE

C--   USING REGION 10 VOLUME DETERMINATION ROUTINES.
C--   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
C--     R10MERLEN - INTERNAL - R10TAP
C--     R10GETDIB - INTERNAL - R10TAP 
C--     R10_HTS   - INTERNAL - NO CALLS
C--     SECGRO    - INTERNAL - NO CALLS
C--     FASTGRO   - INTERNAL - NO CALLS
C--     R10TAP    - EXTERNAL - NO CALLS
C--     NUMLOG    - EXTERNAL - NO CALLS
C--     SCRIB     - EXTERNAL - NO CALLS
C--     SEGMNT    - EXTERNAL - NO CALLS

C**************************************************************
C**************************************************************

C--  VARIABLES OF INTEREST ARE:

C--  COR - CHARACTER - FLAG TO USE SCRIBNER OR SCRIBNER
C--                    FACTORS TO GENERATE SCRIBNER PRODUCT
C--  DBHOB - REAL - DIAMETER BREAST HEIGHT
C--  EQNUM - INTEGER - EQUATION NUMBER TO USE
C--    EQNUM(1) - BOARD FOOT MAIN STEM
C--    EQNUM(2) - CUBIC FOOT MAIN STEM
C--    EQNUM(3) - CORD WOOD MAIN STEM
C--    EQNUM(4) - CUBIC FOOT TOP WOOD
C--    EQNUM(5) - CORD WOOD TOP WOOD
     
C--  TOP DIB TO USE
C--    MTOPP - MAIN STEM
C--    MTOPS - TOP WOOD

C********BOARD FOOT VOLUME EQUATION NUMBERS********

C--    THE FOLLOWING ARE REGION 10 STEM PROFILE MODELS
C--    1051 = SPRUCE - HEMLOCK - 16 FOOT LOG LENGTH
C--    1052 = SPRUCE - HEMLOCK - 32 FOOT LOG LENGTH
C--    1053 = WESTERN RED CEDAR - 16 FOOT LOG LENGTH
C--    1054 = WESTERN RED CEDAR - 32 FOOT LOG LENGTH
C--    1055 = ALASKA CEDAR - 16 FOOT LOG LENGTH
C--    1056 = ALASKA CEDAR - 32 FOOT LOG LENGTH

C********CUBIC FOOT VOLUME EQUATION NUMBERS********

C--    THE FOLLOWING ARE REGION 10 STEM PROFILE MODELS
C--    1051 = SPRUCE - HEMLOCK - 16 FOOT LOG LENGTH
C--    1052 = SPRUCE - HEMLOCK - 32 FOOT LOG LENGTH
C--    1053 = WESTERN RED CEDAR - 16 FOOT LOG LENGTH
C--    1054 = WESTERN RED CEDAR - 32 FOOT LOG LENGTH
C--    1055 = ALASKA CEDAR - 16 FOOT LOG LENGTH
C--    1056 = ALASKA CEDAR - 32 FOOT LOG LENGTH

C--  EVOD - INTEGER - EVEN OR ODD LENGTH SEGMENTS ALLOWED
C--         SEGMENTATION OPTIONS 11-14 ALLOW ODD LENGTHS BY
C--         DEFINITION
C--        1 = ODD SEGMENTS ALLOWED
C--        2 = ONLY EVEN SEGMENTS ALLOWED

C--  HTTOT - REAL - TREE HEIGHT FROM GROUND TO TIP
C--  HT1PRD - REAL - TREE MERCHANTABLE HEIGHT IN FEET OR LOGS
C--  LENMS - REAL - LENGTH OF MAIN STEM IF A TOP WOOD PRODUCT
C--                 IS DESIRED
C--  LMERCH - REAL - GIVEN MERCHANTABLE LENGTH OF STEM EITHER INPUT
C--             IN LOGS AS HT1PRD FROM A6 RECORD OR RETURNED
C--             FROM SUBROUTINE "R10_HTS" AND THE MERCHANTABLE
C--             LENGTH OF STEM WITH TRIM REMOVED RETURNED FROM
C--             SUBROUTINE "SEGMNT"
C--  LOGLEN - REAL(20) - LOG SEGMENT LENGTHS COMPUTED BY
C--           SUBROUTINE "SEGMNT". DOES NOT INCLUDE TRIM.
C--  LOGV - REAL - THE PRODUCT VOLUME OF A LOG
C--  MAXLEN - REAL - MAXIMUM SEGMENT LENGTH
C--  MINLEN - REAL - MINIMUM SEGMENT LENGTH
C--  MERCHL - INTEGER - MINIMUM PRODUCT LENGTH FOR A MERCH TREE
C--  NOLOGP - REAL - AVERAGE NUMBER OF 16 FOOT LOGS IN MAIN STEM
C--  NOLOGS - REAL - AVERAGE NUMBER OF 16 FOOT LOGS IN TOP WOOD
C--  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS FROM
C--           SUBROUTINE "NUMSEG"
C--  OPT - INTEGER - SPECIFIED SEGMENTATION OPTION
C--        OPTION CODES ARE AS FOLLOWS:
C--        11 = 16 FT LOG SCALE (FSH 2409.11)
C--        12 = 20 FT LOG SCALE (FSH 2409.11)
C--        13 = 32 FT LOG SCALE
C--        14 = 40 FT LOG SCALE
C--        21 = NOMINAL LOG LENGTH (NLL), IF TOP LESS THAN HALF
C--             OF NLL IT IS COMBINED WITH NEXT LOWEST LOG AND
C--             SEGMENTED ACORDING TO RULES FOR NLL MAX LENGTH.
C--             IF SEGMENT IS HALF OR MORE OF NLL THEN SEGMENT
C--             STANDS ON ITS' OWN.
C--        22 = NOMINAL LOG LENGTH, TOP IS PLACED WITH NEXT LOWEST
C--             LOG AND SEGMENTED ACORDING TO RULES FOR NLL MAX
C--             LENGTH.
C--        23 = NOMINAL LOG LENGTH, TOP SEGMENT STANDS ON ITS' OWN.
C--        24 = NOMINAL LOG LENGTH, TOP SEGMENT LESS THAN 1/4 OF
C--             NNL THEN SEGMENT IS DROPED, IF SEGMENT IS
C--             1/4 TO 3/4 THEN SEGMENT IS = 1/2 OF NNL,
C--             IF SEGMENT IS GREATER THAN 3/4 OF NNL THEN
C--             SEGMENT IS = NNL.

C--  PCTDF1 - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
C--          DUE TO CULL AND BREAKAGE.IN THE MAIN STEM PORTION OF TREE
C--          THIS DEFECT % WILL BE APPLIED TO BOTH BDFT AND CUFT PROD/

C--  PCTDF2 - REAL - THE INTEGER VALUE REPRESENTING THE % OF HIDDEN DEF
C--  LOGDEF() - REAL - THE VALUE REPRESENTING THE % OF SEEN DEF BY LOG

C--  STUMP - REAL - HEIGHT OF STUMP
C--  TAPEQU - REAL - TAPER EQUATION TO USE - FROM EQNUM(?)
C--  TRIM - REAL - TRIM LENGTH FOR EACH SEGMENT
C--  VOL - REAL
C--    VOL(1) -  TOTAL VOLUME FOR THE TREE IN CUBIC FOOT, DOES NOT
C--              INCLUDE LIMBS AND ROOTS.
C--    VOL(2) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(3) - NET AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(4) - GROSS AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
C--    VOL(5) - NET AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
C--    VOL(6) - AMOUNT OF CORD WOOD PRODUCT IN MAIN STEM
C--    VOL(7) - GROSS AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
C--    VOL(8) - NET AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
C--    VOL(9) - AMOUNT OF CORD WOOD PRODUCT IN TOP WOOD.
C--    VOL(10) - GROSS AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(11) - NET AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C--    VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C--
C******
C--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
C*********************************************************************

