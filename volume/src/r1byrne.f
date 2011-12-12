c== last modified  9-6-2000
      SUBROUTINE R1BYRNE(VOLEQ,FORST,MTOPP,HTTOT,DBHOB,HT1PRD,LIVE,
     >                 PROD,VOL,CUTFLG,BFPFLG,CUPFLG)

C-- THIS SUBROUTINE WAS WRITTEN BY JIM BRICKELL, MENSURATIONIST, TCFPM,
C-- R-1, TO CALCULATE TREE VOLUMES FOR LODGEPOLE PINE FROM THE STEM
C-- PROFILE EQUATIONS THAT WERE DEVELOPED BY JOHN BYRNE, MOSCOW FSL, INT
C-- THESE STEM PROFILE EQUATIONS ARE DOCUMENTED IN THE PUBLICATION:
C--      BYRNE, JOHN. 1989. ------------------------------------.
c--
C--   THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C--   USING REGION 1 VOLUME DETERMINATION ROUTINES.
C--


C**************************************************************

      CHARACTER*1 COR,LIVE
      CHARACTER*2 PROD,FORST
      CHARACTER*10 VOLEQ

      INTEGER NATFOR,CUTFLG,BFPFLG,CUPFLG
      INTEGER EVOD, OPT

      REAL DBHOB, HTTOT, LMERCH, STUMP, TRIM, MTOPP, VOL(11)
      REAL VMER, HT1PRD, CUBV, MAXLEN, MINLEN,logs
      REAL TCVOL, D2, TEMPVOL, DIB
      REAL LENGTH,LOGDIB(21,3),LOGLEN(20),LOGVOL(7,20),LOGV

C--   SET ALL POTENTIAL VOLUMES AND AVG NUM OF LOGS TO ZERO

      DO 10 I=1,11
        VOL(I) = 0.0
   10 CONTINUE
C      NLOGMS = 0.0
C      NLOGTW = 0.0
C      NUMSEG = 0

C--   IF DBH OR HT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME

      IF ( DBHOB .LE. 0.0 .OR. HTTOT .LE. 0.0 ) THEN
        GO TO 1000
      ENDIF

C*************************************************************
C*************************************************************
C--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *
C*************************************************************

C--   CHECK TO MAKE SURE A STEM PROFILE MODEL IS AVAILABLE

      IF (CUTFLG.EQ.1) THEN

          CALL R1TCUB(VOLEQ,FORST,DBHOB,HTTOT,TCVOL)
          VOL(1) = TCVOL
      ENDIF

C**************************************************************
C**************************************************************
C           BOARD FOOT CACULATIONS  (DBH GE 7.0)              *
C**************************************************************

C--   DETERMINE IF A BOARD FOOT PRODUCT REPRESTNTATION IS NEEDED

      IF(BFPFLG.EQ.1 .OR. CUPFLG.EQ.1) THEN

C***************** USE BYRNE'S D2H ROUTINES TO DETERMINE VOLUME
C--  FOR LODGEPOLE PINE
         COR='Y'
         EVOD = 2
         MAXLEN = 20.0
         MINLEN = 10.0
         OPT = 12
         STUMP = 1.0
         TRIM = .5
         LMERCH = HT1PRD
         MERCHL = 10.0

C--   SUBROUTINE "MERLEN" IS INTERNAL AND USES PROFILE MODEL
C--           LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO
C--           SPECIFIED TOP, INCLUDING TRIM, IF MERCH LEN IS NOT GIVEN.

         IF(LMERCH .LE. 0.0) THEN
              CALL R1MLEN(VOLEQ,NF,DBHOB,HTTOT,STUMP,MTOPP,LMERCH)
         ENDIF

C--  CHECK FOR A MINIMUM MERCH STEM LENGTH

         IF(LMERCH.GE.MERCHL) THEN

C--  SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
C--  MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
C--  OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
C--  RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.

            CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,
     >             TRIM,NUMSEG)


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
     >              TRIM,NUMSEG,LOGLEN)

C               NLOGMS = LMERCH/16.0

C--   SUBROUTINE "GETDIB" IS INTERNAL AND USES PROFILE MODEL

            CALL R1GDIB(VOLEQ,NF,DBHOB,HTTOT,MTOPP,STUMP,TRIM,
     >              NUMSEG,LOGLEN,LOGDIB)


C************************************************************
C      GET SCRIBNER VOLUME FOR THE PIECES                   *
C************************************************************
            IF(BFPFLG .EQ. 1 .AND. DBHOB .GE. 7.0) THEN

               TEMPVOL = 0.0
               DO 250 I=1,NUMSEG
                 DIB=LOGDIB(I+1,1)
                 LENGTH=LOGLEN(I)

                 IF (DIB.LT.6)  THEN
                   LOGV=0.0
                 ELSEIF((DIB.EQ.6).AND.(LENGTH.GT.3)
     >                         .AND.(LENGTH.LT.10)) THEN
                   LOGV=.5
                 ELSEIF((DIB.EQ.7).AND.(LENGTH.GT.3)
     >                          .AND.(LENGTH.LT.7)) THEN
                   LOGV=.5
                 ELSEIF((DIB.EQ.8).AND.(LENGTH.LT.5)
     >                          .AND.(LENGTH.GT.2)) THEN
                   LOGV=.5
                 ELSE

                   CALL SCRIB (DIB,LENGTH,COR,LOGV)

                 ENDIF

                 LOGVOL(1,I) = LOGV
                 TEMPVOL = TEMPVOL + LOGV

250           CONTINUE

C--    CONVERT FROM SCRIBNER DECIMAL C TO SCRIBNER
               TEMPVOL = TEMPVOL * 10.0

C--    LODGEPOLE PINE IS DONE.  OTHER SPECIES REQUIRE A BDFT/CUFT RATIO
               IF (VOLEQ(8:10) .EQ. '108') THEN
                  VOL(2) = TEMPVOL
               ELSE
                  CALL VOLRATIO (VOLEQ,NF,DBHOB,HTTOT,MTOPP,LOGDIB,
     >                              LOGLEN,NUMSEG,CUBV,VMER)
                  VOL(2) = VMER*(TEMPVOL/CUBV)
C                  WRITE(*,*)VMER,TEMPVL,CUBV
               ENDIF

C--    (ENDIF FOR MINIMUM STEM LENGTH)
            ENDIF


C--       (ENDIF FOR EQNUM(1) EQUAL TO A VALID EQUATION)
         ENDIF

C***************************************************************

         IF (VOL(2) .LT. 0.0) THEN
               VOL(2) = 0.0
         ENDIF

C--   (ENDIF FOR BOARD FOOT EQUATIONS)

C**************************************************************
C**************************************************************
C*      CUBIC FOOT MAIN STEM EQUATIONS  ALL TREES             *
C**************************************************************

        IF (CUPFLG.EQ.1) THEN

C***************** USE D2H ROUTINES TO DETERMINE VOLUME

C--      IF SPECIFICATIONS ARE THE SAME FOR CUBIC AND BOARD FOOT
C--      PRODUCTS THEN DO NOT RECOMPUTE LOG LENGTHS AND DIAMETERS
C--  FOR LODGEPOLE PINE
           IF(VOLEQ(8:10).EQ.'108') THEN
              DIBL=LOGDIB(1,1)
              DO 15 I=1,NUMSEG
                 DIBS=LOGDIB(I+1,1)
                 LENGTH=LOGLEN(I)
                 LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
C                 LOGVOLC(I) = LOGVOL
                 VOL(4) = VOL(4) + ANINT(LOGV)
                 LOGVOL(4,I) = ANINT(LOGV)
                 DIBL = DIBS
  15          CONTINUE

C--  FOR OTHER SPECIES
           ELSE
              CALL R1TAP(VOLEQ,NF,DBHOB,HTTOT,MTOPP,LMERCH,1,VMER,D2)
              VOL(4) = VMER
           ENDIF

C--       (ENDIF FOR EQNUM(2) EQUAL TO A VALID EQUATION)
        ENDIF

C***************************************************************

          IF (VOL(4) .LT. 0.0) THEN
               VOL(4) = 0.0
          ENDIF

C--   (ENDIF FOR CUBIC FOOT EQUATIONS)
      ENDIF

C**************************************************************
C*************************************************************
C--    DETERMINE TOP WOOD CUBIC FOOT PRODUCTS (ALL TREES)    *
C*************************************************************


C**************************************************************

C      WRITE(15,*)' BDFT LOGS'
C      DO 105, I=1,NUMSEG
C           WRITE(15,43) LOGDIB(I+1),LOGLEN(I),LOGVOLB(I)
C  43       FORMAT(3F5.1)
C 105  CONTINUE
C
C      WRITE(15,*)' CUFT LOGS'
C      DO 107, I=1,NUMSEG
C           WRITE(15,44) LOGDIB(I+1),LOGLEN(I),LOGVOLC(I)
C  44       FORMAT(3F5.1)
C 107  CONTINUE

 1000 CONTINUE

      RETURN
      END



C**************************************************************
C**************************************************************
      SUBROUTINE R1TCUB(VOLEQ,FORST,DBHOB,HTTOT,TCVOL)
C**************************************************************

C--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
C--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
C--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
C--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
C--   TO THE TIP.

C--   VARIABLES OF INTEREST ARE:
C--   DBH - REAL - DIAMETER BREAST HEIGHT
C--   HT1 - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
C--   VOLEQ - INTEGER - EQUATION NUMBER TO USE
C--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
C--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.
C--   MFLAG - INTEGER - 0 RETURNS TOTAL CUBIC VOLUME
C--                     1 RETURNS MERCH HT OR MERCH CUBIC VOLUME
C--                     2 RETURNS DIAMETER AT HT
      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER MFLAG
      REAL DBHOB, D2, HTTOT, CVOL, TCVOL

      MFLAG = 0
      CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,0.0,0.0,MFLAG,CVOL,D2)

      IF(VOLEQ(8:10) .EQ. '108') THEN
           TCVOL = 0.005454154 * CVOL * DBHOB * DBHOB * HTTOT
      ELSE
           TCVOL = CVOL
      ENDIF

      RETURN
      END

C**************************************************************
C**************************************************************
      SUBROUTINE R1MLEN(VOLEQ,FORST,DBHOB,HTTOT,STUMP,MTOPP,LMERCH)
C**************************************************************

C--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
C--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
C--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
C--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
C--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.
      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER  MFLAG
      REAL DBHOB, D2, HTTOT, HTUP, LMERCH, STUMP, MTOPP

      MFLAG = 1
C      HT2 = 0.0

      CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,HTUP,MFLAG,CVOL,D2)

      LMERCH = HTUP - STUMP

      RETURN
      END

C**************************************************************
C**************************************************************
      SUBROUTINE R1GDIB(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,STUMP,TRIM,
     >              NUMSEG,LOGLEN,LOGDIB)
C**************************************************************

C--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
C--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)
      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER NUMSEG, MFLAG

      REAL MTOPP, DBHOB, D2, HTTOT, HTUP, LOGLEN(20)
      REAL LOGDIB(21,3), STUMP, TRIM, CVOL

      DO 10 I=1,21
         LOGDIB(I,1)=0.0
         LOGDIB(I,2)=0.0
         LOGDIB(I,3)=0.0
   10 CONTINUE

      IF (NUMSEG .EQ. 0 ) GO TO 1000

      MFLAG = 2

C      IF (STUMP .GT. 4.5 ) THEN

C--     USE FOR TOP WOOD

C###########
C        CALL R1TAP(TAPEQU,FORST,DBH,HT1,TOP,HT2,MFLAG,CVOL,D2)
C        LOGDIB(1)=D2
C      ELSE

C--     USE FOR MAIN STEM

C###########
        CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,4.5,MFLAG,CVOL,D2)

        LOGDIB(1,2)=D2
C      ENDIF

      HTUP = STUMP
      DO 20 I=1,NUMSEG
         HTUP=HTUP+TRIM+LOGLEN(I)

C###########
        CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,HTUP,MFLAG,CVOL,D2)
        LOGDIB(I+1,2) = D2
   20 CONTINUE

C--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
      DO 30 I=1,NUMSEG+1
          DIBCLS=INT(LOGDIB(I,2))
          XXX=LOGDIB(I,2)-DIBCLS
          IF( XXX .GT. 0.499) DIBCLS = DIBCLS+1.0
          LOGDIB(I,1)=DIBCLS
   30 CONTINUE

C--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
C--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
C--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
C--  MINIMUM SPECIFIED TOP.

      IF( LOGDIB(NUMSEG+1,1) .LT. MTOPP) THEN
          LOGDIB(NUMSEG+1,1) = MTOPP
      ENDIF

 1000 CONTINUE
      RETURN
      END



C**************************************************************
C**************************************************************

      SUBROUTINE VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPP,LOGDIB,LOGLEN,
     >                     NUMSEG,CUBV,VMER)
C**************************************************************

C--   SUBROUTINE WILL DETERMINE CALCULATE SMALIANS CUBIC VOLUME
C--   FOR LODGEPOLE AND THE CUBIC VOLUME FOR THE SPECIES OF
C--   INTEREST.  BOARD FOOT FOR SPECIES OF INTEREST IS CALCULATED
C--   USING A BDFT TO CUBIC FOOT RATIO BASED ON LODGEPOLE.
      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER  MFLAG, NF, NUMSEG
      REAL DBHOB, D2, HTTOT, TOP, LOGV, HTUP, MTOPP
      REAL LOGDIB(21,3), LOGLEN(20), CUBV, VMER

      CUBV = 0.0
      MFLAG = 1
      HTUP = 0.0
    
      CALL R1TAP(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,HTUP,MFLAG,CVOL,D2)

      VMER = CVOL

      DIBL=LOGDIB(1,1)

      DO 10 I=1,NUMSEG
         DIBS=LOGDIB(I+1,1)
         LENGTH=LOGLEN(I)
         LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
         CUBV = CUBV + LOGV
         DIBL = DIBS
  10  CONTINUE

      RETURN
      END
C**************************************************************

C--  HTTOT - REAL -  TREE HEIGHT IN FEET
C--  MERCHL - REAL - TREE HEIGHT FROM GROUND TO MERCHANTABLE TOP

C--  LENMS - REAL - LENGTH OF MAIN STEM IF A TOP WOOD PRODUCT
C--                 IS DESIRED
C--
C--  LMERCH - REAL - GIVEN MERCHANTABLE LENGTH OF STEM CALCULATED
C--             IF HT2 IS INPUT AS ZERO
C--
C--  LOGLEN - REAL(20) - LOG SEGMENT LENGTHS COMPUTED.

C--  LOGV - REAL - THE PRODUCT VOLUME OF A LOG

C--  NLOGMS - REAL - AVERAGE NUMBER OF 16 FOOT LOGS IN MAIN STEM
C--  NLOGTW - REAL - AVERAGE NUMBER OF 16 FOOT LOGS IN TOP WOOD
C--  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS


C--  STUMP - REAL - HEIGHT OF STUMP
C--  VOLEQ - REAL - TAPER EQUATION TO USE - FROM EQNUM(?)
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
