      SUBROUTINE MPBIN (PASKEY,ARRAY,LNOTBK,LKECHO)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     OPTION PROCESSOR FOR MOUNTAIN PINE BEETLE MODEL
C
C Revision History:
C  Previous revision date noted was 03/18/89.
C  11/10/03 - Lance R. David (FHTET)
C    Added LFLAG to KEYRDR call statement argument list.
C  07/27/07 - Lance R. David (FHTET)
C    Improved text written for the INVMORT keyword.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C   08/22/14 Lance R. David (FMSC)
C     Function name was used as variable name.
C     changed variable INT to INCRS
C----------
C
C------------------------------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'MPBCOM.F77'
      INCLUDE 'COLCOM.F77'
C
COMMONS

C.... INTERNAL STORAGE

      CHARACTER*8 PASKEY,TABLE(40),KEYWRD
      CHARACTER*10 KARD(7)
      CHARACTER*80 PNAME

      LOGICAL LRANSD,LNOTBK(7),LACSRF,LKECHO

      INTEGER ISIZE, KODE, NUMBER, I, I1, I2, IG, J, K, KEY
      
      REAL ARRAY(7)
      
      DATA ISIZE/ 40 /

      DATA TABLE /
     >     'END','PSFOUND','DCFOUND','INVMORT','MANSTART','MPBSTART',
     >     'AGGPHERM','REPPHERM','DEBUG','PRBSCALE','RANNSEED',
     >     'BETTER','RANSTART','NUMCLASS','AMP','ACTSRF','CRITAD',
     >     'MPBECHO','BEETLES','HABSUIT','MAXYEARS','STNDSIZE',
     >     'GENOTYPE','STRONG','EMERINC','POPDYN','NOPOPDYN',
     >     'AGGTHRES','CURRMORT','EPIPROB','MPBGRF','NODEBUG',
     >     'QVALUES ','INITMORT','NOMPBGRF','PSDBHLIM',
     >     'PSPKILL','DCPKILL','LATITUDE','PARTIAL' /
C
C     Set flag for warning note concerning use of ??X.exe models
C
      CALL TXNOTE (LXNOTE)

      KEYWRD=PASKEY

   10 CONTINUE
      CALL KEYRDR (IREAD,JOSTND,DEBUIN,KEYWRD,LNOTBK,
     &             ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

C----------
C  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
C               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.
C----------
C*    IF (KODE.LT.0) THEN
C*       IPRMPT=-KODE
C*    ELSE
C*       IPRMPT=0
C*    ENDIF

      IF (KODE .EQ. 0) GOTO 30
      IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
      CALL ERRGRO (.TRUE.,6)
      GOTO 10

   30 CONTINUE
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUIN,JOSTND)

C.... RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

      IF (KODE .EQ. 0) GO TO 40
      IF (KODE .EQ. 1) CALL ERRGRO (.FALSE.,1)
      CALL ERRGRO (.TRUE.,5)
   40 CONTINUE

C.... IF VARIABLES ARE INITIALIZED, THEN: BRANCH TO OPTION PROCESSING.

      IF (LMPB1) GO TO 90

C.... ELSE: INITIALIZE VARIABLES

      LACSRF=.FALSE.
      LMPB1 = .TRUE.
      LRANSD =.FALSE.
      GO TO 90

C.... SPECIAL END-OF-FILE TARGET

   80 CONTINUE
      CALL ERRGRO (.FALSE.,2)
   90 CONTINUE

C.... PROCESS OPTIONS

      GO TO( 1000,1200,1300,1400,1500,1500,1700,1800,1900,2000
     >      ,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000
     >      ,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000
     >      ,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000
     >      ),  NUMBER

 1000 CONTINUE

C     =================  OPTION NUMBER 1  -- END =============

      IF(LKECHO) WRITE(JOSTND,1020) KEYWRD
 1020 FORMAT (/,A8,'   END OF MOUNTAIN PINE BEETLE OPTIONS.')
      RETURN

 1200 CONTINUE

C     =================  OPTION NUMBER 2  -- PSFOUND =========

      LPS = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10
      IF(LKECHO) WRITE(JOSTND,1220) KEYWRD
 1220 FORMAT (/,A8,'   PROPORTION OF SURFACE TARGETED FOR SPRAYING ',
     >  'THAT WAS ACTUALLY FOUND AND SPRAYED (BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 1250 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30

         READ (IREAD,1725,END=80) (PSPF(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (PSPF(K),K=I1,I2)
         IRECNT = IRECNT + 1
 1250 CONTINUE
      GO TO 10

 1300 CONTINUE

C     =================  OPTION NUMBER 3  -- DCFOUND =========

      LDC = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10
      IF(LKECHO) WRITE(JOSTND,1320) KEYWRD
 1320 FORMAT (/,A8,'   PROPORTION OF ATTACKED TREES SPRAYED ',
     >               '(BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 1350 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30
         READ (IREAD,1725,END=80) (DCPF(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (DCPF(K),K=I1,I2)
         IRECNT = IRECNT + 1
 1350 CONTINUE
      GO TO 10

 1400 CONTINUE

C     =================  OPTION NUMBER 4  -- INVMORT  ========

      LINVMR = .TRUE.
      LCURMR = .FALSE.
      IF(LNOTBK(1)) LDAM = .FALSE.

      IF(LKECHO) WRITE(JOSTND,1410) KEYWRD
 1410 FORMAT (/,A8,'   USE INVENTORIED CURRENT MPB ATTACK DATA '
     >       '(FVS TREE DATA DAMAGE/SEVERITY CODES).')

      IF(LKECHO .AND. LDAM) WRITE(JOSTND,1411)
 1411 FORMAT (11X,'MPB DAMAGE CODE TABLE WILL BE WRITTEN.')

      IF(LKECHO .AND. .NOT. LDAM) WRITE(JOSTND,1412)
 1412 FORMAT (11X,'MPB DAMAGE CODE TABLE WILL NOT BE WRITTEN.')
      GO TO 10

 1500 CONTINUE

C     =================  OPTION NUMBER 5  -- MANSTART  =======

C....  NOTE: BOTH MPBSTART AND MANSTART KEYWORDS GET YOU HERE !!!!!!!!

      MPBON = 1
      I = 1
      IF (LNOTBK(1)) I = IFIX(ARRAY(1))

      CALL OPNEW (KODE,I,555,0,ARRAY(2))
      IF (KODE .GT. 0) GO TO 10

      IF(LKECHO) WRITE(JOSTND,1540) KEYWRD,I
 1540 FORMAT (/,A8,'   OUTBREAK MANUALLY SCHEDULED FOR CYCLE/YEAR ',
     >       I4)

      GO TO 10

C     =================  OPTION NUMBER 6  -- MPBSTART  =======

C.... NOTE: THIS KEYWORD BEING READ IN CAUSES CONTROL TO BE TRANSFERRED
C....       TO LINE 1500 IF THE CODE FOR MANSTART.

 1700 CONTINUE

C     =================  OPTION NUMBER 7  -- AGGPHERM  =======

      LAGG = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10

      IF(LKECHO) WRITE(JOSTND,1720) KEYWRD
 1720 FORMAT (/,A8,'   POPULATION MULTIPLIERS USED TO ',
     >  'SIMULATE THE ARTIFICIAL APPLICATION OF AGGREGATION PHEREMONES',
     >  ' (BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 1750 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30
         READ (IREAD,1725,END=80) (AGGPH(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (AGGPH(K),K=I1,I2)
 1725    FORMAT (8F10.0)
 1727    FORMAT (T12,8F10.2)

         IRECNT = IRECNT + 1
 1750 CONTINUE
      GO TO 10

 1800 CONTINUE

C     =================  OPTION NUMBER 8  -- REPPHERM  =======

      LREP = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10

      IF(LKECHO) WRITE(JOSTND,1820) KEYWRD
 1820 FORMAT (/,A8,'   POPULATION MULTIPLIERS USED TO ',
     >  'SIMULATE THE ARTIFICIAL APPLICATION OF REPELLING PHEREMONES',
     >  ' (BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 1850 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30

         READ (IREAD,1725,END=80) (REPL(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (REPL(K),K=I1,I2)

         IRECNT = IRECNT + 1
 1850 CONTINUE
      GO TO 10

 1900 CONTINUE

C     =================  OPTION NUMBER 9  -- DEBUG  ==========

      IF(LKECHO) WRITE(JOSTND,1920) KEYWRD
 1920 FORMAT (/,A8,'   MOUNTAIN PINE BEETLE DEBUG ACTIVATED.')

      JOMPB = JOSTND
      DEBUIN = .TRUE.
      GO TO 10

 2000 CONTINUE

C     =================  OPTION NUMBER 10 -- PRBSCALE  =======

      IF ( LNOTBK(1) ) PRBSCL = ARRAY(1)
      IF(LKECHO) WRITE(JOSTND,2040) KEYWRD,PRBSCL
 2040 FORMAT (/,A8,'   OUTBREAK PROBABILITY SCALING FACTOR = ',F11.3)
      GO TO 10

 2100 CONTINUE

C     =================  OPTION NUMBER 11 -- RANNSEED  =======

      CALL MPRNSD (LNOTBK(1),ARRAY(1))
      IF(LKECHO) WRITE(JOSTND,2110) KEYWRD,ARRAY(1)
 2110 FORMAT (/,A8,'   RANDOM SEED IS:',F10.0)
      GOTO 10

 2200 CONTINUE

C     =================  OPTION NUMBER 12 -- BETTER  =========

      DO 2250 I = 1,4
         IF ( LNOTBK(I) ) BETTER(I)=ARRAY(I)
 2250 CONTINUE

      IF(LKECHO) WRITE(JOSTND,2260) KEYWRD,(BETTER(I),I=1,NATR)
 2260 FORMAT (/,A8,'   CLASSIFICATION IMPORTANCE VALUES',
     >       ' (ONE PER ATTRIBUTE) =',8F6.1)
      GO TO 10

 2300 CONTINUE

C     =================  OPTION NUMBER 13 -- RANSTART  =======

      LRANST = .TRUE.
      IF (LNOTBK(1)) ISTDT = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,2310) KEYWRD, ISTDT
 2310 FORMAT (/,A8,'   MOUNTAIN PINE BEETLE OUTBREAKS WILL BE ',
     >        'SCHEDULED RANDOMLY STARTING AT CYCLE/YEAR ',I4,'.')
      GO TO 10

 2400 CONTINUE

C     =================  OPTION NUMBER 14 -- NUMCLASS  =======

      IF ( ARRAY(1) .GE. 1.0 .AND. ARRAY(1) .LE. 30.0)
     >          NCLASS = IFIX (ARRAY(1))

      IF(LKECHO) WRITE(JOSTND,2440) KEYWRD,NCLASS
 2440 FORMAT (/,A8,'   NUMBER OF TREE CLASSES REPRESENTED = ',I5)
      GO TO 10

 2500 CONTINUE

C     =================  OPTION NUMBER 15 -- AMP  ============

      IF ( LNOTBK(1) ) AMP1 = ARRAY(1)
      IF ( LNOTBK(2) ) AMP2 = ARRAY(2)

      IF(LKECHO) WRITE(JOSTND,2540) KEYWRD,AMP1, AMP2
 2540 FORMAT (/,A8,'   AGGREGATING TREES HAVE ',F8.1,
     >       ' TIMES MORE SURFACE.  PARM TWO =',F8.1)
      GO TO 10

 2600 CONTINUE

C     =================  OPTION NUMBER 16 -- ACTSURF  ========

      LACSRF=.TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10

      IF(LKECHO) WRITE(JOSTND,2620) KEYWRD
 2620 FORMAT (/,A8,'   ACTUAL SURFACE KILLED PER YEAR : ',/)

      J = IFIX(ARRAY(1))
      DO 2650 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30

         READ (IREAD,1725,END=80) (ACTSRF(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (ACTSRF(K),K=I1,I2)
         IRECNT = IRECNT + 1
 2650 CONTINUE
      GO TO 10

 2700 CONTINUE

C     =================  OPTION NUMBER 17 -- CRITAD  =========

      IF ( LNOTBK(1) ) CRITAD = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,2760) KEYWRD,CRITAD
 2760 FORMAT (/,A8,'   BEETLES PER UNIT SURFACE NEEDED TO CAUSE ',
     >              'TREE DEATH = ',F11.3)
      GO TO 10

 2800 CONTINUE

C     =================  OPTION NUMBER 18 -- MPBECHO  ========

C.... SET JOMPBX TO THE UNIT NUMBER FOR THE POST PROCESSOR OUTPUT.

      JOMPBX = 29

C.... READ IN THE FILE NAME FROM THE SUPPLEMENTAL RECORD.

      READ (IREAD,2810) PNAME
 2810 FORMAT (A)
      IF (PNAME .EQ. ' ') PNAME = 'MPBOUT'

C.... TRY TO OPEN THE FILE.

      CALL MYOPEN (JOMPBX, PNAME, 1, 133, 0, 1, 1, 0, KODE)

C.... PRINT OUT KEYWORD MESSAGE BASED ON IF THE FILE OPENS OR NOT.

      IF (KODE .EQ. 1) THEN
         WRITE(JOSTND,2820) KEYWRD, PNAME
 2820    FORMAT (/,A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,
     &           11X,'WAS NOT OPENED DUE TO AN ERROR!!!!')
         JOMPBX = 0
      ELSE
         IF(LKECHO) WRITE(JOSTND,2830) KEYWRD, PNAME
 2830    FORMAT (/,A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,
     &           11X,'WAS OPENED.')
      ENDIF
      GOTO 10

 2900 CONTINUE

C     =================  OPTION NUMBER 19 -- BEETLES  ========

      IF ( LNOTBK(1) ) STRBUG = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,2940) KEYWRD, STRBUG
 2940 FORMAT (/,A8,'   STARTING BEETLE POPULATION PER ACRE = ',F11.3)
      GO TO 10

 3000 CONTINUE

C     =================  OPTION NUMBER 20 -- HABSUIT  ========

      IF ( LNOTBK(1) ) HS = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,3040) KEYWRD, HS
 3040 FORMAT (/,A8,'   HABITAT SUITABILITY = ',F11.3)
      GO TO 10

 3100 CONTINUE

C     =================  OPTION NUMBER 21 -- MAXYEARS  =======

      IF ( ARRAY(1) .GE. 1.0 .AND. ARRAY(1) .LE. 30.0 )
     >          MPMXYR = IFIX (ARRAY(1))

      IF(LKECHO) WRITE(JOSTND,3140) KEYWRD, MPMXYR
 3140 FORMAT (/,A8,'   MAXIMUM YEARS IN MPB PROJECTION = ',I4)
      GO TO 10

 3200 CONTINUE

C     =================  OPTION NUMBER 22 -- STNDSIZE  =======

      IF ( LNOTBK(1) ) EXCON = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,3240) KEYWRD, EXCON
 3240 FORMAT (/,A8,'   STAND SIZE (ACRES) = ',F11.3)
      GO TO 10

 3300 CONTINUE

C     =================  OPTION NUMBER 23 -- GENOTYPE  =======

      IF ( ARRAY(1) .GE. 1.0 .AND. ARRAY(1) .LE. 3.0 )
     >          NG = IFIX ( ARRAY(1) )
      IF ( LNOTBK(2) ) DST(1) = ARRAY(2)
      IF ( LNOTBK(3) ) DST(2) = ARRAY(3)
      IF ( LNOTBK(4) ) DST(3) = ARRAY(4)

      IF(LKECHO) WRITE(JOSTND,3340) KEYWRD,NG,(DST(IG),IG=1,NG)
 3340 FORMAT (/,A8,'   NUMBER OF GENOTYPES = ',I1,'.  DISTANCE EACH ',
     >              'ONE CAN FLY (FEET) IS ',3F8.1)
      GO TO 10

 3400 CONTINUE

C     =================  OPTION NUMBER 24 -- STRONG  =========

      IF ( LNOTBK(1) )  STRP = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,3440) KEYWRD,STRP
 3440 FORMAT (/,A8,'   PROPORTION BEETLES IN GENOTYPE ONE (STRONG) =',
     >      F8.7)
      GO TO 10

 3500 CONTINUE

C     =================  OPTION NUMBER 25 -- EMERINC  ========

      IF (LNOTBK(1)) INCRS = IFIX (ARRAY(1))

      IF(LKECHO) WRITE(JOSTND,3540) KEYWRD,INCRS
 3540 FORMAT (/,A8,'   NUMBER OF EMERGENCE INCREMENTS = ',I4)
      GO TO 10

 3600 CONTINUE

C     =================  OPTION NUMBER 26 -- POPDYN  =========

      IF(LKECHO) WRITE(JOSTND,3640) KEYWRD
 3640 FORMAT (/,A8,'   THE MOUNTAIN PINE BEETLE POPULATION DYNAMICS',
     >              ' MODEL WILL BE USED.')
      LPOPDY = .TRUE.
      GO TO 10

 3700 CONTINUE

C     =================  OPTION NUMBER 27 -- NOPOPDYN  =======

      IF(LKECHO) WRITE(JOSTND,3740) KEYWRD
 3740 FORMAT (/,A8,'   THE MOUNTAIN PINE BEETLE RATE OF LOSS MODEL',
     >              ' WILL BE USED.')

      IF (LNOTBK(1)) IBOUSE = IFIX(ARRAY(1))

      IF (IBOUSE.GE.1) THEN
         IBOUSE = 1
         IF(LKECHO) WRITE(JOSTND,3741)
 3741    FORMAT (T12,'BOUSFIELD''S ADJUSTMENTS ARE BEING USED.')
      ELSE
         IBOUSE = 0
         IF(LKECHO) WRITE(JOSTND,3742)
 3742    FORMAT (T12,'BOUSFIELD''S ADJUSTMENTS ARE NOT BEING USED.')
      ENDIF

      LPOPDY = .FALSE.
      GO TO 10

 3800 CONTINUE

C     =================  OPTION NUMBER 28 -- AGGTHRES  =======

      LCRES = .NOT. LNOTBK(1)
      IF (LNOTBK(1)) TAFAC = ARRAY(1)
      IF (LNOTBK(2)) TAMAX = ARRAY(2)
      IF (LNOTBK(3)) TAMIN = ARRAY(3)
      IF (LNOTBK(4)) TATOL = ARRAY(4)

      IF(LKECHO) WRITE(JOSTND,3840) KEYWRD,TAFAC,TAMAX,TAMIN,TATOL
 3840 FORMAT (/,A8,'   THRESHOLD OF AGGREGATION = ',F11.3,/,
     >       T12,'THRESHOLD MAXIMUM = ',F11.3,/,
     >       T12,'THRESHOLD MINIMUM = ',F11.3,/,
     >       T12,'CALIBRATION TOLERANCE = ',F11.3)
      GO TO 10

 3900 CONTINUE

C     =================  OPTION NUMBER 29 -- CURRMORT  =======

      DO 3910 I=1,7
         IF (LNOTBK(I)) CURRMR(I) = ARRAY(I)
 3910 CONTINUE

      READ (IREAD,'(3F10.0,T1,3A10)',END=80) (ARRAY(I),I=1,3),
     >                                       (KARD(I),I=1,3)
      IRECNT = IRECNT + 1

      DO 3915 I=1,3
         IF (KARD(I) .NE. ' ') CURRMR(7+I) = ARRAY(I)
 3915 CONTINUE

      IF(LKECHO) WRITE(JOSTND,3920) KEYWRD,CURRMR
 3920 FORMAT (/,A8,'   CURRENT MORTALITY BY DBH CLASS=',10F8.3)
      LCURMR = .TRUE.
      LINVMR = .FALSE.

C.... Call the Event Monitor and schedule a manual outbreak in
C.... cycle 1.

      CALL OPNEW (KODE,1,555,0,ARRAY(4))

      GO TO 10

 4000 CONTINUE

C     =================  OPTION NUMBER 30 -- EPIPROB  ========

      IF (ARRAY(1) .LT. 0.0 .OR. ARRAY(1) .GT. 1.0) GO TO 10
      LEPI = .TRUE.
      EPIPRB = ARRAY(1)

      IF(LKECHO) WRITE(JOSTND,4040) KEYWRD,EPIPRB
 4040 FORMAT (/,A8,'   SPECIFIED OUTBREAK PROBABILITY = ',F11.3)
      GO TO 10

 4100 CONTINUE

C     =================  OPTION NUMBER 31 -- MPBGRF  =========

      IF(LKECHO) WRITE(JOSTND,4140) KEYWRD
 4140 FORMAT (/,A8,'   MOUNTAIN PINE BEETLE GRAPHICS OUTPUT',
     >        ' MODEL WILL BE PRINTED.')
      MPBGRF = .TRUE.
      GO TO 10

 4200 CONTINUE

C     =================  OPTION NUMBER 32 -- NODEBUG  ========

      IF(LKECHO) WRITE(JOSTND,4240) KEYWRD
 4240 FORMAT (/,A8,'   TURN OFF DEBUG OUTPUT.')
      DEBUIN = .FALSE.
      GO TO 10

 4300 CONTINUE

C     =================  OPTION NUMBER 33 -- QVALUES  ========

      DO 4310 I=1,7
         IF (LNOTBK(I)) PRNOIN(I) = ARRAY(I)
 4310 CONTINUE

      READ (IREAD,'(3F10.0,T1,3A10)',END=80) (ARRAY(I),I=1,3),
     >                                       (KARD(I),I=1,3)
      IRECNT = IRECNT + 1

      DO 4315 I=1,3
         IF (KARD(I).NE.' ') PRNOIN(7+I)=ARRAY(I)
 4315 CONTINUE

      IF(LKECHO) WRITE(JOSTND,4320) KEYWRD,PRNOIN
 4320 FORMAT (/,A8,'   Q VALUES BY DBH CLASS=',10F8.3)
      GO TO 10

 4400 CONTINUE

C     =================  OPTION NUMBER 34 -- INITMORT  =======

      DO 4410 I=1,7
         IF (LNOTBK(I)) ZINMOR(I) = ARRAY(I)
 4410 CONTINUE

      READ (IREAD,'(3F10.0,T1,3A10)',END=80) (ARRAY(I),I=1,3),
     >                                       (KARD(I),I=1,3)
      IRECNT = IRECNT + 1

      DO 4415 I=1,3
         IF (KARD(I).NE.' ') ZINMOR(7+I)=ARRAY(I)
 4415 CONTINUE

      IF(LKECHO) WRITE(JOSTND,4420) KEYWRD,ZINMOR
 4420 FORMAT (/,A8,'   INITIAL MORTALITY BY DBH CLASS=',10F8.3)
      GO TO 10

 4500 CONTINUE

C     =================  OPTION NUMBER 35 -- NOMPBGRF  =======

      IF(LKECHO) WRITE(JOSTND,4540) KEYWRD
 4540 FORMAT (/,A8,'   MOUNTAIN PINE BEETLE GRAPHICS OUTPUT',
     >              ' MODEL WILL NOT BE PRINTED.')
      MPBGRF = .FALSE.
      GO TO 10

 4600 CONTINUE

C     =================  OPTION NUMBER 36 -- PSDBHLIM  =======

      LPS = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10

      IF(LKECHO) WRITE(JOSTND,4620) KEYWRD
 4620 FORMAT (/,A8,'   SMALLEST SIZE TREE (DBH) TREATED WITH ',
     >       'PREVENTIVE SPRAY (BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 4650 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30

         READ (IREAD,1725,END=80) (PSDL(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (PSDL(K),K=I1,I2)
         IRECNT = IRECNT + 1
 4650 CONTINUE
      GO TO 10

 4700 CONTINUE

C     =================  OPTION NUMBER 37 -- PSPKILL  ========

      LPS = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10

      IF(LKECHO) WRITE(JOSTND,4720) KEYWRD
 4720 FORMAT (/,A8,'   PROPORTION OF BEETLES KILLED LANDING ON ',
     >    'TREES TREATED WITH PREVENTIVE SPRAY (BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 4750 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30

         READ (IREAD,1725,END=80) (PSPK(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (PSPK(K),K=I1,I2)
         IRECNT = IRECNT + 1
 4750 CONTINUE
      GO TO 10

 4800 CONTINUE

C     =================  OPTION NUMBER 38 -- DCPKILL  ========

      LDC = .TRUE.
      IF (ARRAY(1) .LE. 0.) GO TO 10

      IF(LKECHO) WRITE(JOSTND,4820) KEYWRD
 4820 FORMAT (/,A8,'   PROPORTION OF BEETLES KILLED WITH DIRECT ',
     >    'CONTROL (BY YEAR):',/)

      J = IFIX(ARRAY(1))
      DO 4850 I=1,J
         I1 = (I - 1) * 8 + 1
         I2 = I1 + 7
         IF (I2 .GT. 30) I2 = 30

         READ (IREAD,1725,END=80) (DCPK(K),K=I1,I2)
         IF(LKECHO) WRITE(JOSTND,1727) (DCPK(K),K=I1,I2)
         IRECNT = IRECNT + 1
 4850 CONTINUE
      GO TO 10

 4900 CONTINUE

C     =================  OPTION NUMBER 39  -- LATITUDE  ======

      IF ( LNOTBK(1)) FORLAT = ARRAY(1)
      IF(LKECHO) WRITE(JOSTND,4940) KEYWRD,FORLAT
 4940 FORMAT (/,A8,'   FOREST LATITUDE = ',F4.1,' DEGREES.')
      GO TO 10

 5000 CONTINUE

C     =================  OPTION NUMBER 40  --  PARTIAL  ======

      IF (LNOTBK(1)) NEPIYR = IFIX(ARRAY(1))
      LSADLP = NEPIYR .GT. 0 .AND.LNOTBK(2)
      IF ( LSADLP ) SADLPP = ARRAY(2)

      IF(LKECHO) WRITE(JOSTND,5040) KEYWRD,NEPIYR
 5040 FORMAT (/,A8,'   CURRENT EPIDEMIC IS IN YEAR ',I4,', ')

      IF (LSADLP.AND.LKECHO) WRITE(JOSTND,5060) SADLPP
 5060 FORMAT (T12,'SURFACE AREA OF DEAD LODGEPOLE =',F11.3)

      IF ((.NOT.LSADLP).AND.LKECHO) WRITE(JOSTND,5080)
 5080 FORMAT (T12,'SURFACE AREA OF DEAD LODGEPOLE ',
     >                'WILL BE COMPUTED BY MODEL.')
      GO TO 10


C.... This entry point executes mountain pine beetle operations
C.... which can only be executed after all Prognosis keywords
C.... have been read, but before the Prognosis model enters its cycle
C.... loop.

      ENTRY MPBOPS

      IF (.NOT. LMPB1) RETURN

C.... Request diameter growth dubbing & save the past DG measurement
C.... period...

      NPYR = IFINT
      LDUBDG = .TRUE.
      RETURN


C.... Entry point used to find the keyword in the table of keywords.

      ENTRY MPKEY (KEY,PASKEY)
      PASKEY = TABLE(KEY)

      RETURN
      END

