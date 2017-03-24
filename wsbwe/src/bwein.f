      SUBROUTINE BWEIN(LKECHO)
      IMPLICIT NONE
C----------
C  $Id: bwein.f 1842 2016-06-30 22:20:31Z ldavid $
C  **BWEIN                  DATE OF LAST REVISION:  03/23/17
C----------
C
C     OPTION PROCESSOR FOR BUDWORM MODEL.
C
C     PART OF THE MULTI-STAND WESTERN SPRUCE BUDWORM MODEL.
C     CONVERT TO F77, N.L. CROOKSTON 12/88
C     N.L.CROOKSTON, W.P. KEMP, P.W. THOMAS, UNIV OF IDAHO,
C     FORESTRY SCIENCES LAB, MOSCOW, IDAHO. APRIL 10, 1984.
C
C     MAJOR REVISION: SERIAL LINKAGE TO STAND PROGNOSIS MODEL.
C     MAJOR REVISION: LINKAGE TO PARALLEL PROCESSING EXTENSION.
c     major revision: delete links to BW pop.dyn. model 
c       K.A.Sheehan USDA-FS,R6-FID  July 1996
C     MAJOR REVISION: CONVERT TO MENU-DRIVEN INPUT. KAS 9/96
C     Revised to remove menu system -- k.sheehan 12/21/98
C
C     CALLED FROM :
C
C       INITRE - PROGNOSIS KEYWORD PROCESSOR.
C
C     SUBROUTINES CALLED :
C
C     BWEERR - PROCESS ERRORS.
C     BWEFDK - FIND A KEYWORD IN A LOOKUP TABLE.
C     BWEKRD - READ A KEYWORD.
C     BWEMNU - HANDLE BUDWORM MENU OPTIONS
C     BWEOB  - SCHEDULE THE NEXT BUDWORM OUTBREAK
C     BWERSD - RESEED RANDOM NUMBER GENERATOR.
C     BWEUCA
C     MYOPEN
C     OPNEW  - SCHEDULE NEW OPTION.
C
C  PARAMETERS :
C
C   IBWCHK - =1 IF BUDLITE IS CALLED, ELSE = 0 (LOCAL VARIABLE)
C   IOBACT - IS MOST RECENT OUTBREAK STILL ACTIVE? 1=YES, 2=NO
C   IREAD  - LOGICAL UNIT NUMBER FOR BUDWORM KEYWORD INPUT.
C   IRECNT - NUMBER OF RECORDS READ FROM IREAD.
C   JOSTND - LOGICAL UNIT NUMBER FOR KEYWORD REPORT.
C   MOPT   - 1=USE MENU SYSTEM, 0=USE FVS KEYWORD FILE FOR BW OPTIONS,
C            2=USE BUDWORM KEYWORD FILE FOR BW OPTIONS
C   JOWE   - FILE NUMBER FOR WEATHER STATION DATA [BWEBOX]
C   WSTEA  - NAME OF FILE CONTAINING WEATHER STATION DATA [LOCAL]
C   BWEATH(10,4) - WEATHER PARAMETER ARRAY [BWECM2]
C   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
C   IYROBL - YEAR THAT THE LAST (MOST RECENT) OUTBREAK STARTED [BWECM2]
C   LBUDL  - FLAG SET TO TRUE IF A BW OUTBREAK IS (WILL BE) ACTIVE [BWECM2]
C   IOBLOC - LOCATION INDEX FOR OUTBREAK STATISTICS [BWECM2]
C   IWOPT  - WEATHER OPTION (1= INCORP. VARIATION, 2= USE MEANS ONLY) [BWECM2]
C   NEMULT(4,3) - MULTIPLIERS FOR NATURAL ENEMY EFFECTS [BWEBOX]
C   LP1-7  - BW OUTPUT FLAGS (TRUE=PRINT, FALSE=NO PRINT) [BWEBOX]
C
C  Revision History:
C    02-MAY-00 Lance David (FHTET)
C      .Added debug handling.
C      .The variable LSTRT is initialized to false to suppress the
C       printing of the heading within the body of the FVS keyword
C       report.
C    04-MAY-00 Lance David (FHTET)
C      .Replaced variable IFILE with direct usage of FVS variable IREAD
C       for logical unit number of keyword file.
C    12-MAY-00 Lance David (FHTET)
C      .Changed variable ISIZE to TABSZ.
C    16-MAY-00 Lance David (FHTET)
C      .Removed TEMPS1(1) as character storage for IYROBL year of last
C       outbreak and added ILOBYR to store initial value.
C    08-JUN-00 Lance David (FHTET)
C      .Changed to BWOUTPUT keyword section to only process fields
C       that are actually used (fields 1-7, variables LP1-LP7)
C    16-JUN-00 Lance David (FHTET)
C      .Reading of the weather station data file (STATIONS.DAT) moved
C       from BWEINT to BWEIN so that the process will only occur if
C       budworm keywords are present. Subroutine BWEINT is called by
C       FVS subroutine INITRE regardless.
C    19-JUN-00 Lance David (FHTET)
C      .Added information to what is written when keyword is processed
C       for keywords: recovery, outbrloc, bwoutput, obsched.
C      .Print control variables LP1-LP7 changed from integer to logical.
C    02-AUG-00 Nick Crookston (RMRS)
C       Added a missing comma to statement 2540. Modified MYOPEN calls
C       for the weather data files.
C    02-OCT-00 Lance David (FHTET)
C      .Reading of the weather station data file (STATIONS.DAT) moved
C       to the location where the WEAHTER keyword is processed so that
C       this file is only processed when the specific option is requested
C       that utilizes the WSLOOK array, use model's internal weather
C       paramters (field 4 = 1).
C    01-DEC-00 Lance David (FHTET)
C      .Variable IDEFPR associated with the generation of DFESUM.TAB
C       was not set along with LP3. Kathy originally had two options
C       on the generation of this table, but now only option 1 is 
C       active. The section of code for this table was originally in 
C       the menu subroutine. I don't think it was properly placed when
C       put in this subroutine. It still does not function properly, 
C       only a table of zeros is generated. This is not a critical 
C       problem, so will have to be addressed at a later date.
C    05-DEC-00 Lance David (FHTET)
C      .Removed a couple do loops that data arrays STAGE and TREESP.
C      .In BWSPRAY keyword processing:
C        - variable KSPRAY changed to ISPRAY
C        - % defoliation trigger variable TRIGGR had not been set.
C    16-AUG-01 Lance R. David (FHTET)
C       Keyword RANNSEED was lost in the smoke of reworking the model
C       and is now back in operation.
C       Minor modification in the reporting of Weather keyword parameters.
C    21-NOV-2002 Lance R. David (FHTET)
C       Changed variable ISTATE to ISTNUM.
C    30-OCT-2003 Lance R. David (FHTET)
C       Replaced real literal value (1.) with real "ARRAY" in GenDefol
C       keyword processing call to OPNEW.
C    05-MAY-2005 Lance R. David (FHTET)
C       Removed carriage control column from format statements used
C       to write to JOSTND do to change in file type to list/text.
C    12-DEC-2005 Lance R. David (FHTET)
C       Put carriage control column back into format statements. Seems
C       this FVS update has been tabled or canceled. 
C    28-DEC-2005 Lance R. David (FHTET)
C       Increased weather file name to 40 characters.
C       Modified the WEATHER keyword reporting for clarity and set
C       the file name when users select to use weather parameter files
C       provided with the model to be located in 'c:/fvsbin/' directory.
C    22-SEP-2006 Lance R. DAvid (FHTET)
C       Change random number initialization and seed handling for 
C       damage model, weather model and outbreak scheduling.
C    29-MAY-2009 Lance R. David (FMSC)
C       Added character variable TMPNAM. Prepended keyword file name
C       to output file names at BWOUTPUT keyword processing.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C    04-SEP-2013 Lance R. David (FMSC)
C       Added RAWS weather year range as supplemental record on 
C       WEATHER keyword.
C    23-MAR-2013 Lance R. David (FMSC)
C       Modified DEFOL and SETPRBIO to process numeric and alpha specie codes.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BWESTD.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'
C
COMMONS
C
C     INTERNAL STORAGE
C
      CHARACTER*40 TEMPNM,TMPNAM
      CHARACTER*4 CFOUR
      CHARACTER*3 TREESP(5)
      CHARACTER*8 KEYWRD
      CHARACTER*7 CSTAT
      CHARACTER*10 KARD(7)
      CHARACTER*6 STAGE(3)
      CHARACTER*20 WSTEA
      CHARACTER*25 CWTYP
      CHARACTER*40 STR40
      CHARACTER*80 RECORD

      INTEGER  I, IACTK, IBWCHK, IDT, IEND, IM, IOB,
     &         IP, IS, ISKIP, ISTAGE, ISTART , ISTLNB, IUNIT,
     &         IZNUL, J, K, KODE, N, NEN, NSKIP, NUMBR

      REAL ARRAY(7), RECNT, X


      LOGICAL DEBUG,LKECHO
      LOGICAL LNOTBK(7),LSTRT, LADDS, LOUTP

CLRD  EQUIVALENCE (KARD,RECORD)

      DATA STAGE/'FOURTH','FIFTH','SIXTH'/
      DATA TREESP/' WF',' DF',' GF','SAF',' ES'/
C
C.... Initialize local variables.
C
      DEBUG = .FALSE.
      LSTRT = .FALSE.
      LADDS = .FALSE.
      LOUTP = .FALSE.

      TEMPNM = ' '
      CFOUR  = ' '
      KEYWRD = ' '
      CSTAT  = ' '
      WSTEA  = ' '
      STR40  = ' '
      RECORD = ' '
      DO I=1,8
        KARD(I) = ' '
      END DO
      DO I=1,7
        LNOTBK(I) = .FALSE.
        ARRAY(I)  = 0.0
      END DO
      NUMBR=0
C
C.... Check for DEBUG
C
      CALL DBCHK(DEBUG,'BWEIN',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,*) 'ENTER BWEIN: ICYC = ',ICYC
C
C     OPEN BUDWORM STANDARD OUTPUT DEVICE AND INITIALIZE CONTROL VARIABLES.
C
      IBWCHK=0
      STR40=' '
      CALL MYOPEN (JOWSBW,STR40,4,133,1,1,1,0,KODE)
      LOUTP=.FALSE.
      LSTRT=.FALSE.
      LADDS=.FALSE.
   10 CONTINUE
      CALL BWEKRD (IREAD,JOSTND,KEYWRD,LNOTBK,ARRAY,
     >             IRECNT,KODE,KARD)
C----------
C  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
C               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.
C----------
      IF (KODE .EQ. 0) GOTO 30
      IF (KODE .EQ. 2) CALL BWEERR(IREAD,IRECNT,JOSTND,ICCODE,
     >                             .FALSE.,2)
      CALL BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.TRUE.,6)
      GOTO 10
   30 CONTINUE

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: >>>BEFORE CALL BWEFDK'
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: NUMBR =', NUMBR
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: KEYWRD =', KEYWRD
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: TABLE  =', TABLE
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: TABSZ  =', TABSZ 
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: KODE   =', KODE
      CALL BWEFDK (NUMBR,KEYWRD,TABLE,TABSZ,KODE,JOSTND)
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: >>>>AFTER CALL BWEFDK'
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: NUMBR =', NUMBR
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: KEYWRD =', KEYWRD
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: TABLE  =', TABLE
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: TABSZ  =', TABSZ 
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: KODE   =', KODE
C
C     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND.
C
      IF (KODE .EQ. 0) THEN
         GOTO 95
      ELSE
         CALL BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.TRUE.,1)
         GOTO 10
      ENDIF
C
C     SPECIAL END-OF-FILE TARGET (USED IF YOU READ PARAMETER CARDS).
C
   80 CONTINUE
      CALL BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.FALSE.,2)
C
C     TARGET USED TO WRITE ILLEGAL PARAMETERS STATEMENT.
C
   90 CONTINUE
      CALL BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.TRUE.,4)
      GOTO 10
   95 CONTINUE
C
C
C     WRITE KEYWORD TABLE HEADING.
C
      IF (.NOT.LSTRT) GOTO 99
      IF (LKECHO) WRITE(JOSTND,98)
   98 FORMAT (/,130('-'),//,T49,'OPTIONS SELECTED BY INPUT',//,
     >         130('-'),/,'KEYWORD    PARAMETERS:',/,
     >         ' --------   ',119('-'))
      LSTRT=.FALSE.
   99 CONTINUE
C
C     PROCESS OPTIONS
C
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: KEYWRD= ',KEYWRD
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: ARRAY = ',ARRAY
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: LNOTBK= ',LNOTBK

      GOTO (200,300,400,500,600,700,800,900,1000
     >     ,1100,1200,1300,1400,1500,1600,1700,1800,1900
     >     ,2000,2100,2200,2300,2400,2500,2600), NUMBR
C
C--------------------- OPTION NUMBER 2 -- END
C
C     THE END KEYWORD SIGNALS THE END OF THE MAIN BUDWORM KEYWORD SET.
C
  200 CONTINUE
      IF (LKECHO) WRITE(JOSTND,220) KEYWRD
  220 FORMAT (/,A8,'   END OF BUDWORM OPTIONS.')
C
C SKIP THE FOLLOWING SECTION IF MANUAL DEFOLIATION IS SUPPLIED
C
      IF (LDEFOL) GOTO 9000          ! exit subroutine  
C
C  READ WEATHER DATA NOW IF BUDLITE HAS BEEN SELECTED
C  IWSRC: 1 = Weather parameters provided with model
C         2 = Weather parameter file provided by user
C         3 = RAWS daily weather data to be used
C
      IF (IBWCHK .EQ. 1) THEN
         IF (IWSRC .EQ. 1) THEN
            CALL MYOPEN (JOWE,TEMPNM,3,133,1,1,1,0,KODE)
            IF (ITEMP(5).NE.1) THEN
               NSKIP=(ITEMP(5)-1)*11
               DO 250 ISKIP=1,NSKIP
               READ (JOWE,240)
  240          FORMAT (1X)
  250          CONTINUE
            ENDIF
         ELSEIF (IWSRC .EQ. 2) THEN
            CALL MYOPEN (JOWE,WFNAME,3,133,1,1,1,0,KODE)
         ELSE
            CALL BWERAWS
         ENDIF
         IF (IWSRC .EQ. 1 .OR. IWSRC .EQ. 2) THEN
            READ (JOWE,260) WSTEA
  260       FORMAT (A20)
            DO 270 I=1,10
              READ (JOWE,265) (BWEATH(I,J),J=1,4)
  265         FORMAT (4F8.3)
  270       CONTINUE
C--------------------------------
C This is an old section of code from before weather source option
C of using RAWS daily weather was added. At this point in the code,
C the RAWS data has already been processed. BUT we are still missing
C means and Std Dev values for hot Fall and precipitation during key
C periods for small and large larvae, pupae and L2 emergence.
C Lance David 07/20/2011
C 
C         ELSE
C            READ (JOWE,275) WHOTM,WHOTSD,(RAINM(I),RAINS(I),I=1,3),
C     >          RAINDM,RAINDS
C  275       FORMAT (/10F7.3)
C            READ (JOWE,280) IYEAR
C  280       FORMAT (I4)
C            IF (IYEAR.EQ.IY(1)) THEN
C               REWIND (JOWE)
C            ELSE
C               ICOUNT=0
C  285          READ (JOWE,280,END=295,IOSTAT=IOS) IYEAR
C               ICOUNT=ICOUNT+1
C               IF (IYEAR.NE.IY(ICYC)) GOTO 285
C               REWIND (JOWE)
C               NCOUNT=ICOUNT-1
C               DO 290 I=1,NCOUNT
C                  READ (JOWE,280,IOSTAT=IOS) IYEAR
C  290          CONTINUE
C               ICOUNT=0
C  295          IF (ICOUNT.NE.0) REWIND (JOWE)
C            ENDIF
C---------------------------------
         ENDIF
      ENDIF
C
C  SET UP THE TIMING OF THE NEXT OUTBREAK
C
      CALL BWEOB
      KRECVR=0
C
C  INIT. THE SPECIAL EVENTS TABLE & COUNTER, PRINT SUMMARY INFO.
C
      IF (LP4 .AND. IBWCHK.EQ.1) CALL BWEP4(1)

      GOTO 9000   ! end of keywords and initialization, exit subroutine
C
C--------------------- OPTION NUMBER 3 -- OPEN
C
  300 CONTINUE
      READ (IREAD,'(A80)',END=80) RECORD
      IRECNT=IRECNT+1
      IF (ARRAY(1).GT.0.0) GOTO 310
      CALL BWEERR(IREAD,IRECNT,JOSTND,ICCODE,.TRUE.,4)
      GOTO 10
  310 CONTINUE
      IUNIT=IFIX(ARRAY(1))
      IF (ARRAY(2).EQ.0) THEN
         IZNUL=0
         CFOUR='ZERO'
      ELSE
         IZNUL=1
         CFOUR='NULL'
      ENDIF
      IS=INT(ARRAY(3))+1
      IF (IS.GE.0.OR.IS.LE.3) GOTO 320
      CALL BWEERR(IREAD,IRECNT,JOSTND,ICCODE,.TRUE.,4)
      GOTO 10
  320 CONTINUE
      IF (IS.EQ.1) CSTAT='UNKNOWN'
      IF (IS.EQ.2) CSTAT='NEW'
      IF (IS.EQ.3) CSTAT='OLD'
      IF (IS.EQ.4) THEN
         IS=5
         CSTAT='FRESH'
      ENDIF
      IM=80
      IF (ARRAY(4).GT.0) IM=INT(ARRAY(4))
C
C     CALL MYOPEN TO OPEN THE FILE.  MACHINE SPECIFIC CODE IS IN MYOPEN
C
      CALL MYOPEN (IUNIT,RECORD(1:40),IS,IM,IZNUL,1,1,0,KODE)
C
      IF (LKECHO) WRITE(JOSTND,340) KEYWRD,IUNIT,CFOUR,CSTAT,IM,RECORD
  340 FORMAT (/,A8,'   DATA SET REFERENCE NUMBER = ',I5,'; BLANK=',A4,
     >       '; STATUS=',A7,/,T12,
     >       'MAXIMUM RECORD LENGTH (IGNORED ON SOME MACHINES) = ',I4,
     >       /,T12,'DATA SET NAME = ',A)
      IF (KODE.EQ.1) WRITE(JOSTND,350)
  350 FORMAT (/,T12,'********   OPEN FAILED   ********')

      GOTO 10
C
C--------------------- OPTION NUMBER 4 -- CLOSE
C
  400 CONTINUE
      IF (ARRAY(1).LE.0.0) THEN
         CALL BWEERR(IREAD,IRECNT,JOSTND,ICCODE,.TRUE.,4)
      ELSE
         I=INT(ARRAY(1))
         IF (LKECHO) WRITE(JOSTND,420) KEYWRD,I
  420    FORMAT (/,A8,'   DATA SET REFERENCE NUMBER = ',I5)
         CLOSE (UNIT=I)
      ENDIF
      GOTO 10

C
C--------------------- OPTION NUMBER 5 -- MGMTID 
C
  500 CONTINUE
      READ (IREAD,510,END=80) MGMIDB
  510 FORMAT (A4)
      IRECNT = IRECNT+1
      IF(LKECHO)WRITE(JOSTND,520) KEYWRD,MGMIDB
  520 FORMAT (/,A8,'   MANAGEMENT ID= ',A4)
      GOTO 10
C
C--------------------- OPTION NUMBER 6 -- RANNSEED
C
C     THE RANNSEED KEYWORD RESEEDS THE BUDWORM DAMAGE MODEL RANDOM
C     NUMBER GENERATOR.
C     IF FIELD 1 IS ZERO OR BLANK, THE CURRENT SEED IS RETRIEVED AND
C     REPORTED.
C
  600 CONTINUE
      IF (.NOT. LNOTBK(1) .OR. ARRAY(1).EQ.0.0) THEN
         CALL BWERGT (DSEEDD)
         DSEEDR = REAL(DSEEDD)
         IF(LKECHO)WRITE(JOSTND,610) KEYWRD, DSEEDR
      ELSEIF (LNOTBK(1) .AND. ARRAY(1).LE.10000.0) THEN
         IF(LKECHO)WRITE(JOSTND,612) KEYWRD, ARRAY(1)
      ELSE
         DSEEDR = ARRAY(1)
C         DSEEDD = DBLE(DSEEDR)
         CALL BWERSD(LNOTBK(1), DSEEDR)
         CALL BWERGT(DSEEDD)
         IF(LKECHO)WRITE(JOSTND,614) KEYWRD, DSEEDR
      ENDIF
      
  610 FORMAT (/,A8,'   DAMAGE MODEL RANDOM NUMBER SEED IS:',
     > F10.0)
  612 FORMAT (/,A8,'   ********   ERROR ********',/,11X,
     > 'RANDOM SEED VALUE MUST BE A 5-DIGIT ODD NUMBER. ',
     > 'VALUE PROVIDED WAS:',F10.0,/,11X,
     > 'RESEEDING OF RANDOM NUMBER GENERATOR DID NOT OCCUR.')
  614 FORMAT (/,A8,'   DAMAGE MODEL RANDOM NUMBER SEED RESET TO:',
     > F10.0)
      GOTO 10
C
C--------------------- OPTION NUMBER 7 -- STARTYR
C
C     THE STARTYR KEYWORD SELECTS THE STARTING YEAR OF A
C     BUDWORM MODEL RUN, IN STAND ALONE MODE.
C
  700 CONTINUE
      IBWYR1=1
      IF (LNOTBK(1)) IBWYR1=IFIX(ARRAY(1))
      IF (LKECHO) WRITE(JOSTND,710) KEYWRD,IBWYR1
  710 FORMAT (/,A8,'   FIRST YEAR OF PROJECTION IS',I5,'.')
      GOTO 10
C
C--------------------- OPTION NUMBER 8 -- COMMENT
C
  800 CONTINUE
      IF (LKECHO) WRITE(JOSTND,'(/1X,A8)') KEYWRD
  810 CONTINUE
      READ (IREAD,'(A80)',END=80) RECORD
      IRECNT=IRECNT+1
      CFOUR=RECORD (1:4)
      DO 820 I=1,4
      CALL BWEUCA (CFOUR(I:I))
  820 CONTINUE
      IF (CFOUR.EQ.'END ') THEN
         IF(LKECHO)WRITE(JOSTND,'(/'' END'')')
         GOTO 10
      ELSE
         IF(LKECHO)WRITE(JOSTND,'(T13,A80)') RECORD
         GOTO 810
      ENDIF
      GOTO 10
C
C--------------------- OPTION NUMBER 9 -- DEFOL
C
C     THE DEFOL KEYWORD SCHEDULES MANUAL DEFOLIATION IN THE LINKED
C     VERSION OF THE BUDWORM MODEL.
C     SPECIES IS DECODED AND SET TO APPROPIATE FVS SPECIES NUMBERIC CODE
C     BLANK DEFOLIATION FIELDS ARE SET TO ZERO.
  900 CONTINUE
      LDEFOL=.TRUE.
      IACTK=2151
      X=0.
      IDT=1
      IF (LNOTBK(1)) IDT=IFIX(ARRAY(1))

      CALL SPDECD (2,IS,JSP,JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (IS .EQ. -999 .OR. IS .EQ. 0) THEN
         ARRAY(2) = 0.0
      ELSE
        IF (IBWSPM(IS) .EQ. 7) THEN
C         NON-HOST SPECIES SPECIFIED
          IF (LKECHO) WRITE(JOSTND,905) KEYWRD,IDT,JSP(IS)
  905     FORMAT (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,
     >        '; NON-HOST - NOT SCHEDULED.')
          GOTO 10
        ELSE
          ARRAY(2)=IS
        ENDIF
      ENDIF

      IF (ARRAY(3).GT.15.) ARRAY(3)=0.0
      DO 910 I=4,7
      IF (.NOT.LNOTBK(I)) ARRAY(I)=X
  910 CONTINUE
      CALL OPNEW (KODE,IDT,IACTK,6,ARRAY(2))
      IF (KODE.GT.0) GOTO 10
      IF (LKECHO) WRITE(JOSTND,920)
     >   KEYWRD,IDT,KARD(2)(1:4),(ARRAY(I),I=3,7)
  920 FORMAT (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,
     >        '; CROWN CODE=',F3.0,'; NEW=',F7.2,'; 1-YR=',F7.2,
     >        '; 2-YR=',F7.2,'; REMAINING=',F7.2)
      GOTO 10
C
C--------------------- OPTION NUMBER 10 -- SETPRBIO
C
C     THE SETPRBIO KEYWORD ALLOWS THE USER TO SCHEDULE RECOMPUTATION
C     OF THE PROPORTION OF RETAINED BIOMASS ARRAYS IN THE LINKED VERSION
C     OF THE BUDWORM MODEL.
C
 1000 CONTINUE
      IACTK=2153
      X=1.0
      IDT=1
      IF (LNOTBK(1)) IDT=IFIX(ARRAY(1))

      CALL SPDECD (2,IS,JSP,JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (IS .EQ. -999 .OR. IS .EQ. 0) THEN
         ARRAY(2) = 0.0
      ELSE
        IF (IBWSPM(IS) .EQ. 7) THEN
C         NON-HOST SPECIES SPECIFIED
          IF (LKECHO) WRITE(JOSTND,1005) KEYWRD,IDT,JSP(IS)
 1005     FORMAT (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,
     >        '; NON-HOST - NOT SCHEDULED.')
          GOTO 10
        ELSE
          ARRAY(2)=IS
        ENDIF
      ENDIF
      IF (ARRAY(3).GT.15.) ARRAY(3)=0.0
      DO 1010 I=4,6
      IF (.NOT.LNOTBK(I)) ARRAY(I)=X
 1010 CONTINUE
      CALL OPNEW (KODE,IDT,IACTK,5,ARRAY(2))
      IF (KODE.GT.0) GOTO 10
      IF (LKECHO) WRITE(JOSTND,1020) 
     >   KEYWRD,IDT,KARD(2)(1:4),(ARRAY(I),I=3,6)
 1020 FORMAT (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,
     >        '; CROWN CODE=',F3.0,'; 1-YR=',F7.2,
     >        '; 2-YR=',F7.2,'; REMAINING=',F7.2)
      GOTO 10
C
C--------------------- OPTION NUMBER 11 -- DAMAGE
C
C     THE DAMAGE KEYWORD ACTIVATES DAMAGE OUTPUT.
C
 1100 CONTINUE
      LBWDAM=.TRUE.
      IF(LKECHO)WRITE(JOSTND,1110) KEYWRD
 1110 FORMAT (/,A8,'   PRINT BUDWORM-CAUSED DAMAGE',
     >        ' OUTPUT.')
      GOTO 10
C
C--------------------- OPTION NUMBER 12 -- NODAMAGE
C
C     THE NODAMAGE KEYWORD DEACTIVATES DAMAGE OUTPUT.
C
 1200 CONTINUE
      LBWDAM=.FALSE.
      IF(LKECHO)WRITE(JOSTND,1210) KEYWRD
 1210 FORMAT (/,A8,'   DO NOT PRINT BUDWORM-CAUSED DAMAGE',
     >        ' OUTPUT.')
      GOTO 10
C
C--------------------- OPTION NUMBER 13 -- PERDAM
C
 1300 CONTINUE
      LBWPDM=.TRUE.
      IF(LKECHO)WRITE(JOSTND,1310) KEYWRD
 1310 FORMAT (/,A8,'   PRINT BUDWORM-CAUSED PERIODIC DAMAGE',
     >        ' OUTPUT.')
      GOTO 10

C
C--------------------- OPTION NUMBER 14 -- NOPERDAM
C
 1400 CONTINUE
      LBWPDM=.FALSE.
      IF(LKECHO)WRITE(JOSTND,1410) KEYWRD
 1410 FORMAT (/,A8,'   DO NOT PRINT BUDWORM-CAUSED PERIODIC DAMAGE',
     >        ' OUTPUT.')
      GOTO 10
C
C -----------------------OPTION NUMBER 15 -- GENDEFOL
C
 1500 CONTINUE
      IACTK= 2150
      IBWCHK=1
      IOBACT=0
      IF (LNOTBK(1)) IOBACT=IFIX(ARRAY(1))
      IF (IOBACT.EQ.1) THEN
        LTEMP1(1)=.TRUE.
      ELSE
        LTEMP1(1)=.FALSE.
      ENDIF
      IYROBL = ILOBYR
      IF (LNOTBK(2)) THEN
        IYROBL = IFIX(ARRAY(2))
        ILOBYR = IYROBL
      ENDIF
      LBUDL=.TRUE.
      CALL OPNEW(KODE,IOBACT,IACTK,0,ARRAY)
      IF(LKECHO)WRITE(JOSTND,1550) KEYWRD,IOBACT,IYROBL
 1550 FORMAT (/,A8,'   CURRENT OUTBREAK (1=Y)=',I2,
     >   '; LAST OUTBREAK STARTED',
     >   ' IN ',I5, '.')
      GOTO 10 
C
C -----------------------OPTION NUMBER 16 -- OUTBRLOC
C
C   SET THE GEOGRAPHIC LOCATION INDEX FOR OUTBREAKS
C     (1=SOUTHWEST, 2=NORTHWEST, 3=MONTANA)
C
 1600 CONTINUE
      IOBLOC=2
      OBSEER=REAL(OBSEED)
      IF (LNOTBK(1)) IOBLOC=IFIX(ARRAY(1))
      ITEMP(2)=IOBLOC
      IF (LNOTBK(2)) THEN
      	OBSEER=REAL(ARRAY(2))
        CALL BWERSD(LNOTBK(2), OBSEER)
        CALL BWERGT(OBSEED)
      ENDIF
      IF(LKECHO)WRITE(JOSTND,1620) KEYWRD,IOBLOC,TEMPS2(IOBLOC),OBSEER
 1620 FORMAT (/,A8,3X,'GEOGRAPHIC LOCATION FOR OUTBREAK.',/,11X,
     > 'LOCATION OPTIONS ARE: 1=SOUTHWEST, 2=NORTHWEST, 3=MONTANA',
     > /,11X,'OUTBREAK LOCATION SPECIFIED=',I3,' (',A20,')',
     > /,11X,'RANDOM NUMBER SEED FOR OUTBREAKS=',F8.1)
      GOTO 10
C
C -----------------------OPTION NUMBER 17 -- RECOVERY
C
 1700 CONTINUE

      IF(LKECHO)WRITE(JOSTND,1720) KEYWRD
 1720 FORMAT (/,A8,3X,'THIS OPTION IS CURRENTLY UNDER DEVELOPMENT ',
     & 'AND HAS NO AFFECT.')

C     IACTK= 2154
C     CALL OPNEW(KODE,0,IACTK,0,ARRAY)
      GOTO 10
C
C -----------------------OPTION NUMBER 18 -- WEATHER
C
 1800 CONTINUE
      IWOPT=1
      JOWE=40
      WSEEDR=REAL(WSEED)
      IF (LNOTBK(1)) IWOPT=IFIX(ARRAY(1))
      IF (LNOTBK(2)) JOWE=IFIX(ARRAY(2))
      IF (LNOTBK(4)) IWSRC=IFIX(ARRAY(4))
      IF (LNOTBK(5)) ITEMP(5)=IFIX(ARRAY(5))
      IF (LNOTBK(6)) ISTNUM=IFIX(ARRAY(6))
      IF (LNOTBK(3)) THEN
      	WSEEDR=REAL(ARRAY(3))
        CALL BWERSD(LNOTBK(3), WSEEDR)
        CALL BWERGT(WSEED)
      ENDIF
      ITEMP(4)=ISTNUM
      IWYR=IY(1)
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEIN: WSEED=', WSEED

      IF (IWSRC .EQ. 1) THEN
C
C        User has chosen weather data source as that provided within
C        the model; therefor, we need to process the weather stations
C        file containing state and station names.
C
C        **********************************************************
C        ***** This code is not active in production versions *****
C        ***** of the executable.                             *****
C        ***** WSLOOK array is loaded using data statements   *****
C        ***** in bweblk the block data program unit.         *****
C        **********************************************************
C        READ WEATHER STATION NAMES
C        This process expects file stations.dat to contain 10 states
C        with 100 stations for each state.
C
C.       IF (DEBUG) IF(LKECHO)WRITE(JOSTND,*) 'IN BWEIN: STNAME=',STNAME
C.       CLOSE (JOWE)
C.       CALL MYOPEN (JOWE,STNAME,3,133,1,1,1,0,KODE)
C.
C.       DO JSTATE=1,10
C.         READ (JOWE,4) NUMSTN(JSTATE)
C.  4      FORMAT (11X,I3)
C.         READ (JOWE,5) (WSLOOK(JSTN,JSTATE),JSTN=1,100)
C.  5      FORMAT (5A16,19(/5A16))
C.       END DO
C        Write weather station lookup table to help create block data
C        statements in bweblk?? files. List of names with propper
C        formatting is written to the main FVS output file.
C        If it is desired to perform processing of the stations.dat
C        file at runtime, do activate the following 3 lines.
C
C.       IF(LKECHO)WRITE(JOSTND,6) WSLOOK
C.  6    FORMAT ((33(/,'     >"',A16,'","',A16,'","',A16,'",'),
C.   >               /,'     >"',A16,'",'))
C        **********************************************************
C        ***** End of weather station name file processing.   *****
C        *****                                                *****
C        **********************************************************

C        Skip the 2 supplemental records for the weather data file name
C        and year range because they are not utilized with this option.
C
         READ(IREAD,1830) 
 1830    FORMAT (1X,/,1X)

       ELSEIF (IWSRC .EQ. 2 .OR. IWSRC .EQ. 3) THEN
         READ (IREAD,1850) WFNAME
 1850    FORMAT (A40)
      ENDIF

C     If using RAWS data, a year range may be specified on the
C     following record. Four-digit starting and ending years
C
      IF (IWSRC .EQ. 3) THEN
         READ (IREAD,*) IYRNG(1), IYRNG(2)
      ENDIF
      
      IF (IWOPT .EQ. 1) THEN
         CWTYP = 'INCLUDES RANDOM VARIATION'
      ELSE
         CWTYP = 'BASED ON AVERAGE VALUES'
      ENDIF

      IF (IWSRC .EQ. 1) THEN
         TEMPNM='c:/fvsbin/'//STATES(ISTNUM,2)
         IF (LKECHO) WRITE (JOSTND,1819) KEYWRD,CWTYP,ITEMP(5),
     >   WSLOOK(ITEMP(5),ITEMP(4)),STATES(ISTNUM,1),TEMPNM,WSEEDR
 1819    FORMAT (/,A8,'   WEATHER ',A,';  WEATHER',
     >   ' STATION = (',I3,') ',A,' IN ',A,
     >     /,11X,'WEATHER DATA FILE IS ',A,
     >     /,11X,'RANDOM NUMBER SEED FOR WEATHER= ',F8.1)

      ELSEIF (IWSRC .EQ. 2) THEN     
         IF (LKECHO) WRITE(JOSTND,1820) KEYWRD,CWTYP,JOWE,WFNAME,WSEEDR
 1820    FORMAT (/,A8,'   WEATHER ',A,';  WEATHER DATA',
     *   ' FILE (NO.) NAME= (',I3') ',A,/,11X,'RANDOM NUMBER',
     *   ' SEED FOR WEATHER= ',F8.1)

      ELSEIF (IWSRC .EQ. 3) THEN     
         IF (LKECHO) WRITE(JOSTND,1822) KEYWRD,JOWE,WFNAME
 1822    FORMAT (/,A8,'   WEATHER IS ACTUAL RAWS DATA;  WEATHER DATA',
     *   ' FILE (NO.), NAME= (',I3') ',A)
         IF (IYRNG(1) .GE. 1900 .AND. IYRNG(2) .GE. 1900) THEN
           IF (LKECHO) WRITE(JOSTND,1823) IYRNG(1), IYRNG(2)
 1823      FORMAT (/,11X,'RANGE OF WEATHER YEARS SPECIFIED: ',
     *             I4,' TO ',I4)
         ELSE
           IF (LKECHO) WRITE(JOSTND,1824)
 1824      FORMAT (/,11X,'ALL WEATHER YEARS WILL BE USED.')
         ENDIF
      ENDIF
      GOTO 10
C
C -----------------------OPTION NUMBER 19 -- NEMULT
C
 1900 CONTINUE
      NEN=-1
      IF (LNOTBK(1)) THEN
         NEN=IFIX(ARRAY(1))
         IF (LNOTBK(2)) NEMULT(NEN,1)=ARRAY(2)
         IF (LNOTBK(3)) NEMULT(NEN,2)=ARRAY(3)
         IF (LNOTBK(4)) NEMULT(NEN,3)=ARRAY(4)
      ENDIF
      IF (NEN.LT.1 .OR. NEN.GT.4) THEN
         WRITE (JOSTND,1910) KEYWRD,NEN
 1910    FORMAT (/,A8,'   NATURAL ENEMY KEYWORD CALLED -- ERROR! ',
     >   'NEN = ',I3)
      ELSE
         IF (LKECHO) WRITE(JOSTND,1920) KEYWRD,NEN,
     >              (NEMULT(NEN,ISTAGE),ISTAGE=1,3)
 1920    FORMAT (/,A8,'   NATURAL ENEMY KEYWORD; NEN = ',I3,
     >   '  MULTIPLIERS= ',3F7.2)
      ENDIF
      GOTO 10
C
C -----------------------OPTION NUMBER 20 -- BWOUTPUT
C
C     The output options are and associated file names are:
C     FIELD 1) WITHIN.TAB, UNIT 41
C     FIELD 2) CANOPY.TAB, UNIT 42
C     FIELD 3) DEFSUM.TAB, UNIT 43
C     FIELD 4) PARAMS.TAB, UNIT 44
C     FIELD 5) ANNUAL.TAB, UNIT 45
C     FIELD 6) DEFOL.TAB,  UNIT 46
C     FIELD 7) DYNAMICS.TAB, UNIT 47
C           8) EFFECTS.TAB, UNIT 48   -- not developed ?

 2000 CONTINUE
      IF (LNOTBK(1) .AND. ARRAY(1) .EQ. 1.0) LP1 = .TRUE.
      IF (LNOTBK(2) .AND. ARRAY(2) .EQ. 1.0) LP2 = .TRUE.
      IF (LNOTBK(3) .AND. ARRAY(3) .EQ. 1.0) LP3 = .TRUE.
      IF (LNOTBK(4) .AND. ARRAY(4) .EQ. 1.0) LP4 = .TRUE.
      IF (LNOTBK(5) .AND. ARRAY(5) .EQ. 1.0) LP5 = .TRUE.
      IF (LNOTBK(6) .AND. ARRAY(6) .EQ. 1.0) LP6 = .TRUE.
      IF (LNOTBK(7) .AND. ARRAY(7) .EQ. 1.0) LP7 = .TRUE.
      IF(LKECHO)WRITE(JOSTND,2005) KEYWRD,(ARRAY(I),I=1,7)
 2005 FORMAT (/,A8,3X,'BUDWORM OUTPUT OPTIONS:',7F4.0,/,36X,
     > '(1) (2) (3) (4) (5) (6) (7)',/,11X,
     > 'REPORTS, SUMMARIES AND TABLES TO BE PRODUCED ARE:')

      IF (LP1) THEN
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(1)
        IF (LKECHO) WRITE(JOSTND,2011) TMPNAM
 2011   FORMAT (11X,'(1) ',A30,
     >    '-- WITHIN-YEAR POPULATION DYNAMICS SUMMARY')
        CALL MYOPEN(JOBWP1,TMPNAM,5,133,1,1,1,0,KODE)        
      ENDIF
      IF (LP2) THEN 
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(2)
        IF (LKECHO) WRITE(JOSTND,2012) TMPNAM
 2012   FORMAT (11X,'(2) ',A30,'-- ANNUAL DEFOLIATION BY SPECIES TABLE')
        CALL MYOPEN(JOBWP2,TMPNAM,5,133,1,1,1,0,KODE)        
      ENDIF
      IF (LP3) THEN
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(3)
        IF (LKECHO) WRITE(JOSTND,2013) TMPNAM
 2013   FORMAT (11X,'(3) ',A30,'-- DEFOLIATION SUMMARY')
        IDEFPR=1
        IBUDYR=0
        DO 2025 I=1,100
          DO 2025 J=1,5
            IDEF(I,J)=0
 2025   CONTINUE
        IF (IDEFPR.EQ.1) THEN
           CALL MYOPEN (JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
           DO 2030 IP=1,5
              DLABS(IP)='        '
              IF (IDEFSP(IP).EQ.1) DLABS(IP)=TREESP(IP)
 2030      CONTINUE
           NUMCOL=5
           WRITE (JOBWP3,2035) IDEFPR,NUMCOL,(DLABS(I),I=1,5)
 2035      FORMAT (I4,4X,I4,/,8X,5A8)
        ELSEIF (IDEFPR.EQ.2) THEN
           CALL MYOPEN(JOBWP3,TMPNAM,3,133,1,1,1,0,KODE)
           IF (KODE.EQ.0) THEN
              READ (JOBWP3,2040) IDEFPR,NUMCOL,(DLABS(N),N=1,NUMCOL)
 2040         FORMAT (I4,4X,I4,/,8X,5(A8,2X))
              IF (IDEFPR.EQ.2) THEN
                DO 2050 J=1,100
                 IF (NUMCOL.EQ.1) READ (JOBWP3,2045,END=2052) IDEF(J,1)        
                 IF (NUMCOL.EQ.2) READ (JOBWP3,2045,END=2052) 
     >             (IDEF(J,I),I=1,2)        
                 IF (NUMCOL.EQ.3) READ (JOBWP3,2045,END=2052) 
     >             (IDEF(J,I),I=1,3)        
                 IF (NUMCOL.GE.4) READ (JOBWP3,2045,END=2052) 
     >             (IDEF(J,I),I=1,4)        
 2045            FORMAT(8X,4(I4,6X))        
 2050           CONTINUE
 2052           NUMCOL=NUMCOL+1
                IF (NUMCOL.GT.5) THEN
                   WRITE (*,2055) 
 2055              FORMAT('********   ERROR - WSBW: TOO MANY COLUMNS ',
     >             'FOR DEFOLIATION TABLE -- WILL WRITE OVER ',
     >             'PREVIOUS COLUMN ')
                   NUMCOL=5
                ENDIF
                DLABS(NUMCOL)=DEFLAB
                CLOSE (JOBWP3)
                CALL MYOPEN(JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
                WRITE (JOBWP3,2040) IDEFPR,NUMCOL,(DLABS(I),I=1,NUMCOL)
              ELSE
                 CLOSE (JOBWP3)
                 CALL MYOPEN (JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
                 DLABS(1)=DEFLAB
                 DO 2060 IP=2,5
                    DLABS(IP)='        '
 2060            CONTINUE
                 NUMCOL=1
                 IDEFPR=2
                 WRITE (JOBWP3,2040) IDEFPR,NUMCOL,DLABS(1)
              ENDIF
           ELSE
              CALL MYOPEN (JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
              DLABS(1)=DEFLAB
              DO 2065 IP=2,5
                 DLABS(IP)='        '
 2065         CONTINUE
              NUMCOL=1
              WRITE (JOBWP3,2040) IDEFPR,NUMCOL,DLABS(1)
           ENDIF
c        ELSE
c           WRITE (*,2070) IDEFPR
c 2070      FORMAT ('********   ERROR - WSBW: IDEFPR = ',I6)
        ENDIF
      ENDIF
      IF (LP4) THEN
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(4)
        IF(LKECHO)WRITE(JOSTND,2014) TMPNAM
 2014   FORMAT (11X,'(4) ',A30,'-- PARAMETERS AND KEY EVENTS SUMMARY')
        CALL MYOPEN(JOBWP4,TMPNAM,5,133,1,1,1,0,KODE)        
      ENDIF
      IF (LP5) THEN
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(5)
        IF(LKECHO)WRITE(JOSTND,2015) TMPNAM
 2015   FORMAT (11X,'(5) ',A30,'-- BRIEF ANNUAL SUMMARY')
        CALL MYOPEN(JOBWP5,TMPNAM,5,133,1,1,1,0,KODE)        
      ENDIF
      IF (LP6) THEN
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(6)
        IF (LKECHO) WRITE(JOSTND,2016) TMPNAM
 2016   FORMAT (11X,'(6) ',A30,'-- ANNUAL DEFOLIATION SUMMARY')
        CALL MYOPEN(JOBWP6,TMPNAM,5,133,1,1,1,0,KODE)        
      ENDIF
      IF (LP7) THEN
        TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(7)
        IF (LKECHO) WRITE(JOSTND,2017) TMPNAM
 2017   FORMAT (11X,'(7) ',A30,'-- BUDWORM DYNAMICS SUMMARY')
        CALL MYOPEN(JOBWP7,TMPNAM,5,133,1,1,1,0,KODE)        
      ENDIF
C     IF (LP8) THEN
C       CALL MYOPEN(JOBWP8,OUTNAM(8),5,133,1,1,1,0,KODE)        
C     ENDIF
C
      GOTO 10
C
C -----------------------OPTION NUMBER 21 -- PARASITE
C
 2100 CONTINUE
      IPARA=1
      LTEMP1(2)=.TRUE.
      IF (LNOTBK(1)) IPARA=IFIX(ARRAY(1))
      IF (IPARA.EQ.2) THEN
        LTEMP1(2)=.FALSE.
        OBPHAS(1,2)=OBPHAS(1,1)
        OBPHAS(2,2)=OBPHAS(2,1)
        OBPHAS(3,2)=OBPHAS(3,1)
        IF(LKECHO)WRITE(JOSTND,2120) KEYWRD
 2120   FORMAT (/,A8,'   PARASITISM RATES DO NOT CHANGE DURING THE ',
     >     'OUTBREAK')
      ELSE
        IF(LKECHO)WRITE(JOSTND,2140) KEYWRD
 2140   FORMAT (/,A8,'   PARASITISM RATES INCREASE DURING THE ',
     >     'OUTBREAK')
      ENDIF
      GOTO 10
C
C -----------------------OPTION NUMBER 22 -- FQUALDEV
C
 2200 CONTINUE
      IQUALD=1
      LTEMP1(3)=.TRUE.
      IF (LNOTBK(1)) IQUALD=IFIX(ARRAY(1))
      IF (IQUALD.EQ.2) THEN
        LTEMP1(3)=.FALSE.
        FOLDVY(3)=FOLDVY(1)
        FOLDVY(4)=FOLDVY(1)
        IF(LKECHO)WRITE(JOSTND,2220) KEYWRD
 2220   FORMAT (/,A8,'   LARVAL DEVELOPMENT TIMES DO NOT ',
     >     'CHANGE DURING THE OUTBREAK')
      ELSE
        IF (LKECHO) WRITE(JOSTND,2240) KEYWRD
 2240   FORMAT (/,A8,'   LARVAL DEVELOPMENT TIMES INCREASE',
     >     ' DURING THE OUTBREAK')

      ENDIF
      GOTO 10
C
C -----------------------OPTION NUMBER 23 -- FQUALWT
C
 2300 CONTINUE
      IQUALW=1
      LTEMP1(4)=.TRUE.
      IF (LNOTBK(1)) IQUALW=IFIX(ARRAY(1))
      IF (IQUALW.EQ.2) THEN
        LTEMP1(4)=.FALSE.
        FOLWTY(3)=FOLWTY(1)
        FOLWTY(4)=FOLWTY(1)
        IF(LKECHO)WRITE(JOSTND,2320) KEYWRD
 2320   FORMAT (/,A8,'   PUPAL WEIGHTS DO NOT ',
     >     'CHANGE DURING THE OUTBREAK')
      ELSE
        IF (LKECHO) WRITE(JOSTND,2340) KEYWRD
 2340   FORMAT (/,A8,'   PUPAL WEIGHTS DECREASE',
     >     ' DURING THE OUTBREAK')

      ENDIF
      GOTO 10
C
C -----------------------OPTION NUMBER 24 -- TITLE
C
 2400 CONTINUE
      READ (IREAD,2410,END=80) ITITLB
 2410 FORMAT (A72)
      RECNT=RECNT+1
      IF (LKECHO) WRITE(JOSTND,2420) KEYWRD,ITITLB
 2420 FORMAT (/,A8,'   BUDWORM TITLE= ',A72)
      GOTO 10
C
C -----------------------OPTION NUMBER 25 -- BWSPRAY
C
 2500 CONTINUE
      IF (LNOTBK(1)) ISPRAY=IFIX(ARRAY(1))
      IF (LNOTBK(2)) ISPVAR=IFIX(ARRAY(2))
      IF (LNOTBK(3)) INSTSP=IFIX(ARRAY(3))
      IF (LNOTBK(4)) SPEFF=ARRAY(4)
      IF (LNOTBK(5)) LIMITS=IFIX(ARRAY(5))

      IF (ISPRAY.EQ.1) THEN
         IACTK=2157
         IDT=ISPVAR
         CALL OPNEW(KODE,IDT,IACTK,2,ARRAY(3))
         IF (KODE.GT.0) GOTO 10
         IF(LKECHO)WRITE(JOSTND,2520) KEYWRD,
     >   ISPVAR,STAGE(INSTSP),SPEFF
 2520    FORMAT (/,A8,'   INSECTICIDE TO BE APPLIED IN YEAR ',I4,
     >   /,11X,'AT PEAK ',A6,' INSTAR, RESULTING IN ',F5.1,
     >   ' PERCENT MORTALITY')
      ELSEIF (ISPRAY.EQ.2) THEN
         IACTK=2159
         IDT=0
         TRIGGR=ISPVAR
         CALL OPNEW(KODE,IDT,IACTK,2,ARRAY(3))
         IF (KODE.GT.0) GOTO 10
         IF (LKECHO)
     >      WRITE(JOSTND,2540) KEYWRD,ISPVAR,STAGE(INSTSP),SPEFF
 2540    FORMAT (/,A8,'   INSECTICIDE TO BE APPLIED WHEN ',
     >   '% DEFOLIATION FIRST RISES ABOVE ',I4,/,
     >   11X,'AT PEAK ',A6,' INSTAR, RESULTING IN ',F5.1,
     >   ' PERCENT MORTALITY')
         IF ((LIMITS.EQ.1).AND.LKECHO)WRITE(JOSTND,2550)
 2550    FORMAT (11X,'ONLY ONE SPRAY APPLICATION ALLOWED PER OUTBREAK')
      ENDIF
      GOTO 10
C
C -----------------------OPTION NUMBER 26 -- OBSCHED
C
 2600 CONTINUE
      IF (LNOTBK(1)) IOBOPT=IFIX(ARRAY(1))

      IF (IOBOPT.EQ.1) THEN
        IF (LKECHO) WRITE(JOSTND,2671) KEYWRD
 2671   FORMAT (/,A8,'   STARTING DATES GENERATED FROM FREQUENCY',
     *  ' STATISTICS;',/,11X,'OUTBREAKS END WHEN % NEW DEFOLIATION',
     *  ' IS < 10% FOR'/,11X,'3 CONSECUTIVE YEARS (5 YEAR MIN.)')

      ELSEIF (IOBOPT.EQ.2) THEN
        IF(LKECHO)WRITE(JOSTND,2672) KEYWRD
 2672   FORMAT (/,A8,'   BOTH STARTING AND ',
     >      'ENDING DATES GENERATED FROM STATISTICS')

      ELSEIF (IOBOPT.EQ.3) THEN
        IF(LKECHO)WRITE(JOSTND,2673) KEYWRD
 2673   FORMAT (/,A8,'   USER HAS SELECTED OUTBREAK',
     >       ' STARTING AND ENDING YEARS:')
        NOBSCH=0
        K=0
        DO 2620 IOB=1,3
          ISTART=0
          IEND=0
          K=K+2
          IF (LNOTBK(K)) ISTART=IFIX(ARRAY(K))
          IF (LNOTBK(K+1)) IEND=IFIX(ARRAY(K+1))
          IF (ISTART.NE.0.AND.IEND.NE.0) THEN
            NOBSCH=NOBSCH+1
            IOBSCH(NOBSCH,1)=ISTART
            IOBSCH(NOBSCH,2)=IEND
            IF (LKECHO) WRITE(JOSTND,2675) NOBSCH,ISTART,IEND
 2675       FORMAT (14X,'OUTBREAK NO. ',I1,':  START= ',I4,
     &         ', END= ',I4)
          ENDIF
 2620   CONTINUE

      ELSEIF (IOBOPT.EQ.4) THEN
        IF (LKECHO) WRITE(JOSTND,2676) KEYWRD
 2676   FORMAT (/,A8,'   FIRST OUTBREAK SCHEDULED, THEN ',
     &      'GENERAL DEFOLIATOR RUNS ',/,11X,
     &      'UNTIL THE END OF THE SIMULATION.')

      ELSE
C
C       INVALID OPTION FOR OUTBREAK SCHEDULING.
C
        WRITE(JOSTND,2678) KEYWRD
 2678   FORMAT (/,A8,'   INVALID OUTBREAK SCHEDULING OPTION. FIELD ',
     &  '1 VALUE MUST BE 1, 2, 3 OR 4.')
      ENDIF

      GOTO 10

 9000 CONTINUE
      IF (DEBUG) WRITE(JOSTND,*) 'EXIT  BWEIN: ICYC = ',ICYC
      RETURN
      END
