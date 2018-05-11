C----------
C VOLUME $Id: r12vol.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  1-18-2013
C 01/18/2013 Added calculation for tip volume VOL(15)
      SUBROUTINE R12VOL(EQNUM,MTOPP,HT1PRD,DBHOB,VOL,NOLOGP,
     >                  NOLOGS,FCLASS,CUTFLG,BFPFLG,CUPFLG,ERRFLAG)

C--   THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C--   USING VOLUME DETERMINATION ROUTINES FOR HAWAII.
C--   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
C--       R12TAP
C**************************************************************

      character*10 eqnum
      
      REAL DBHOB, HT1PRD, LOGVOL(21),NOLOGP, NOLOGS, HT2, TCVOL
      REAL MTOPP, VOL(15), LVOL

      INTEGER VTYPE,SEGNUM,FCLASS,CUTFLG,BFPFLG,CUPFLG,ERRFLAG,I

C--   SET ALL POTENTIAL VOLUMES AND AVG NUM OF LOGS TO ZERO
      DO 10 I=1,15
          VOL(I) = 0.0
   10 CONTINUE

      NOLOGP = 0.0
      NOLOGS = 0.0
      SEGNUM = 0
      ERRFLAG = 0

C--   IF DBHOB OR HT1PRD EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
      
      IF ( DBHOB .LE. 1.0)THEN
         ERRFLAG = 3
         GOTO 1000
      ENDIF
      IF(HT1PRD .LE. 0.0 ) THEN
         ERRFLAG = 4
         GO TO 1000
      ENDIF

C*************************************************************
C--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *
C*************************************************************
C--   SUBROUTINE "R12TAP" IS THE PROFILE MODEL
      IF(CUTFLG.EQ.1) THEN
        HT2 = HT1PRD
        VTYPE = 1
        CALL R12TAP(EQNUM,DBHOB,HT1PRD,HT2,MTOPP,FCLASS,VTYPE,TCVOL)

        VOL(1) = TCVOL
      ENDIF

C**************************************************************
C**************************************************************
C           BOARD FOOT CACULATIONS  (DBHOB GE 10.0)              *
C**************************************************************

C--   DETERMINE IF A BOARD FOOT PRODUCT REPRESTNTATION IS NEEDED

      IF(DBHOB .GE. 10.0) THEN
C--*************** USE STEM PROFILE MODEL TO DETERMINE VOLUME
          IF(BFPFLG.EQ.1) THEN
               VTYPE = 2
               SEGNUM = INT(HT1PRD/8.15)
               NOLOGP = SEGNUM
               DO 100, I=1,SEGNUM
                    HT2 = I
                    CALL R12TAP(EQNUM,DBHOB,HT1PRD,HT2,MTOPP,FCLASS,
     >                                                VTYPE,LVOL)
                    VOL(10) = VOL(10) + LVOL
                    LOGVOL(I) = LVOL
C                IF(I.EQ.2.OR.I.EQ.4) THEN
C                    TEMPVOL = LOGVOL(I) + LOGVOL(I-1)
C                ENDIF
  100          CONTINUE
          ENDIF   

C*****************
          IF (VOL(10) .LT. 0.0) THEN
               VOL(10) = 0.0
          ENDIF

C--   (ENDIF FOR BOARD FOOT EQUATIONS)
      ENDIF

C**************************************************************
C*      CUBIC FOOT MAIN STEM EQUATIONS  ALL TREES             *
C**************************************************************

      IF (CUPFLG.EQ.1) THEN
C--*************** USE STEM PROFILE MODEL TO DETERMINE VOLUME
          IF(DBHOB.GE.7.0) THEN
               VTYPE = 3
               SEGNUM = INT(HT1PRD/8.15)
               DO 200, I=1,SEGNUM
                  HT2 = I
                  CALL R12TAP(EQNUM,DBHOB,HT1PRD,HT2,MTOPP,FCLASS,
     >                                                 VTYPE,LVOL)
                  VOL(4) = VOL(4) + LVOL
  200          CONTINUE
C--       (ENDIF FOR EQNUM(2) EQUAL TO A VALID EQUATION)
          ENDIF
C*************
          IF (VOL(4) .LT. 0.0) THEN
               VOL(4) = 0.0
          ENDIF
C--   (ENDIF FOR CUBIC FOOT EQUATIONS)
      ENDIF

C**************************************************************
C*      CORD WOOD MAIN STEM EQUATIONS  ALL TREES             *
C**************************************************************

C--   PUT LOGIC HERE FOR CORDS

C**************************************************************

        VOL(2) = VOL(10)
        VOL(3) = VOL(11)

C****************************************************************
C--  DETERMINE TOP WOOD CUBIC FOOT PRODUCTS (ALL TREES)
C--    ONLY ONE STEM PROFILE EQUATION MAY BE SPECIFIED FOR BDFT,
C--    CUFT, AND CORDS AND ONLY ONE TOP DIAMETER MAY BE SPECIFIED
C--    BEFORE TOP WOOD VOLUME CAN BE DETERMINED.
C****************************************************************


C***************************************************************

          IF (VOL(7) .LT. 0.0) THEN
             VOL(7) = 0.0
          ENDIF
C     Calculate stem tip volume
      IF(VOL(4).GT.0.0 .AND. VOL(1).GT.0.0) VOL(15)=VOL(1)-VOL(4)
      IF(VOL(15).LT.0.01) VOL(15)=0.0
 1000 CONTINUE

      RETURN
      END

C**************************************************************
C**************************************************************

C--  VARIABLES OF INTEREST ARE:

C--  DBHOB - REAL - DIAMETER BREAST HEIGHT
C--  EQNUM - INTEGER - EQUATION NUMBER TO USE
C--    EQNUM(1) - BOARD FOOT MAIN STEM
C--    EQNUM(2) - CUBIC FOOT MAIN STEM
C--    EQNUM(3) - CORD WOOD MAIN STEM
C--    EQNUM(4) - CUBIC FOOT TOP WOOD
C--    EQNUM(5) - CORD WOOD TOP WOOD

C--  TOP DIB TO USE
C--    TOPDIA(1) - BOARD FOOT MAIN STEM
C--    TOPDIA(2) - CORD WOOD MAIN STEM
C--    TOPDIA(3) - CUBIC FOOT MAIN STEM
C--    TOPDIA(4) - CORD WOOD TOP WOOD
C--    TOPDIA(5) - CUBIC FOOT TOP WOOD

C********BOARD FOOT VOLUME EQUATION NUMBERS********
C--    THE FOLLOWING ARE REGION 5 STEM PROFILE MODELS
C      1251 = KOA  
C      1252 = OHIA
C      1253 = ROBUSTA EUCALYPTUS 
C      1254 = SALIGNA EUCALYPTUS 

C********CUBIC FOOT VOLUME EQUATION NUMBERS********

C--    THE FOLLOWING ARE REGION 5 STEM PROFILE MODELS
C      1251 = KOA  
C      1252 = OHIA
C      1253 = ROBUSTA EUCALYPTUS 
C      1254 = SALIGNA EUCALYPTUS 
C
C--  HT1PRD - REAL - TREE HEIGHT FROM GROUND TO TIP
C--  LENMS - REAL - LENGTH OF MAIN STEM IF A TOP WOOD PRODUCT
C--                 IS DESIRED
C--  MAXLEN - REAL - MAXIMUM SEGMENT LENGTH
C--  MINLEN - REAL - MINIMUM SEGMENT LENGTH
C--  MERCHL - INTEGER - MINIMUM PRODUCT LENGTH FOR A MERCH TREE
C--  NOLOGP - REAL - AVERAGE NUMBER OF 8 FOOT LOGS IN MAIN STEM
C--  NOLOGS - REAL - AVERAGE NUMBER OF 8 FOOT LOGS IN TOP WOOD
C--  SEGNUM - INTEGER - THE COMPUTED NUMBER OF SEGMENTS 


C--  PCTDF - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
C--          IN THE MAIN STEM PORTION OF A TREE - THIS DEFECT %
C--          WILL BE APPLIED TO BOTH BDFT AND CUFT PRODUCTS.
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
C**************************************************************
