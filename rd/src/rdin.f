      SUBROUTINE RDIN(PASKEY,ARRAY,LNOTBK,LKECHO)
      IMPLICIT NONE
C----------
C  **RDIN                         LAST REVISION:  03/24/15
C----------
C
C  Purpose :
C     This is the master routine for reading in root disease keywords.
C
C     Format statements 606, 711, 1321, 2921 (11f5.2) and 3963 (11I5) are
C     dependent on parameter MAXSP. If the number of species (PRGPRM: MAXSP)
C     changes, so must these.
C
C  Called By :
C     INITRE  [PROGNOSIS]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     KEYRDR  (SUBROUTINE)   [PROGNOSIS]
C     ERRGRO  (SUBROUTINE)   [PROGNOSIS]
C     FNDKEY  (SUBROUTINE)   [PROGNOSIS]
C     OPNEW   (SUBROUTINE)   [PROGNOSIS]
C     RCDSET  (SUBROUTINE)   [PROGNOSIS]
C     MYOPEN  (SUBROUTINE)   [PROGNOSIS]
C     SPDECD  (SUBROUTINE)   [PROGNOSIS]
C     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
C     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
C     RDSTP   (SUBROUTINE)   [ROOT DISEASE]
C
C  Entry Points :
C     RDKEY
C
C  Arguments :
C     PASKEY - RETURNS TO THE CALLING ROUTINE THE KEYWORD FROM ARRAY
C              TABLE FOR INDEX VALUE IN PARAMETER KEY OF ENTRY POINT
C              RDKEY.
C     ARRAY  - ARRAY OF 7 ELEMENTS THAT HOLDS THE FIELD VALUES READ IN
C              FROM THE KEYWORD
C     LNOTBK - ELEMENT IS TRUE IF FIELD IS NOT BLANK
C
C  Local Variables :
C
C
C  Common Block Variables Used :
C
C  Revision History :
C     05/01/97 - Matthew K. Thompson (FHTET)
C                Fixed an error with setting the disease type in the   
C                PLOTINF keyword.
C     05/28/97 - Matthew K. Thompson (FHTET)
C                Removed keyword DATELIST and call to subroutine RDTLS.
C     05/26/98 - Robert Nathan Havis (FHTET)
C                Put in warning message print to output file when RRTYPE
C                is not used to specify disease and default disease type,
C                annosus is used. new logical (LRTYPE)
C     01/18/00 - Lance David (FHTET)
C                Inserted a keyword spaceholder in position 27 of array 
C                TABLE and increased the array size to 42. Removal of the
C                keyword that occupied this space and reducing the array
C                size (possibly mod dated 05/38/97) caused the FVS option
C                processor to report the wrong keyword in the Activity
C                Schedule and Summary reports for all Root Disease activity
C                keywords in positions 27-42 of array TABLE.
C     03/20/00 - Lance David (FHTET)
C                Replaced the literals in option processor calls with
C                references to array MYACT. 
C     08/03/00 - Nick Crookston (RMRS)
C                Fixed up some format statements. (Who coded the old
C                H edit discriptor in 907?).
C  16-MAY-01 Lance R. David (FHTET)
C     Added use of FVS subroutine UPCASE to eliminate case sensitivity in
C     COMMENT's "END" keyword processing.
C  06-JUL-01 Lance R. David (FHTET)
C     Moved several local variables (IDOBB, LBBON and LRTYPE) to RDCOM
C     and their initialization to RDINIT. 
C     The purpose of this change is to allow RD keywords to occur
C     in multiple sets/blocks within the FVS keyword set. That means the
C     that certain variables cannot be re-initialized each time this
C     routine is entered which is triggered by the RDIN keyword.
C  06-AUG-01 Lance R. David (FHTET)
C     Handling of tree specie code, number and alpha, (ISPC and KARD(2)) for
C     BBTYPEx keywords were not set when field 2 of keyword was blank. This
C     was a problem (undefined variable) for the keyword reporting statements.  
C  13-AUG-02 Lance R. David (FHTET)
C     Added call to FVS routine GETSED in processing of RSEED keyword 
C     for random seeding of the random number generator when field 1 is 0.
C  10-NOV-2003 Lance R. David (FHTET)
C     Added LFLAG to KEYRDR call statement argument list.
C  04-JAN-2006 Lance R. David (FHTET)
C     Changed error message and process for END keyword so that FVS
C     simulation continues instead of terminating when initialization
C     instructions are incomplete.
C  16-AUG-2006 Lance R. David (FHTET)
C     Change of metric conversion factors variable names to match
C     variables in new \FVS\COMMON\METRIC.F77. rd\src\metric.f77
C     will be retired. (mods courtesy of Don Robinson, ESSA)
C  17-JUL-2007 Lance R. David (FHTET)
C     Removed condition testing IREC1 to determine if tree records had already
C     been processed and terminating to run if they had. The restructuring of
C     damage code processing eliminates this requirement.
C  08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C  03/24/15 Lance R. David
C     For implementation of General Report Writer facility and addition 
C     of output to database option, BBOUT and RRDOUT keywords modified.
C
C----------------------------------------------------------------------
C
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'METRIC.F77'

C.... Local variable declarations.

      LOGICAL      DEBUG,LKECHO
      LOGICAL      LNOTBK(7), LINIT

      CHARACTER*16 RRJSP(ITOTSP)
      CHARACTER*12 DISTYP(0:ITOTRR)
      CHARACTER*10 KARD(7), T1, T2, T3
      CHARACTER*8  TABLE(42), KEYWRD, PASKEY
      CHARACTER*4  C4TMP
      CHARACTER*80 RECORD

      INTEGER  I, IC, IDI, IDSPLT, IDT, IHB, IHST(MAXSP), IPOINT, IRG,
     &         IRMAX, IRMIN, IRRPSH, ISIZE, ISL, ISPC, J, JAGE, KEY,
     &         K, KODE, KSP, M, MYACT(9), NEXT, NUMBER

      REAL     ANS, ARRAY(7), DEN, DIAM, PRMS(10), RDPRP, RTD, TT 

C.... Data Statements.
C
C.... 1-SPREAD  2-PSTUMP 3-WINDTHR 4-BBTYPE1 5-BBTYPE2
C.... 6-BBTYPE3 7-BORATE 8-SPORE   9-BBTYPE4

      DATA MYACT / 2401, 2403, 2414, 2415, 2416,
     &             2417, 2430, 2431, 2432 /

      DATA DISTYP /'NON-HOST    ','P-TYPE ANOS.','S-TYPE ANOS.',
     &             'ARMILLARIA  ','PHELLINUS   '/

      DATA RRJSP /'      WHITE PINE', '   WESTERN LARCH',
     &            '     DOUGLAS-FIR', '       GRAND FIR',
     &            ' WESTERN HEMLOCK', '     W. REDCEDAR',
     &            '  LODGEPOLE PINE', 'ENGELMANN SPRUCE',
     &            '   SUBALPINE FIR', '  PONDEROSA PINE',
     &            'MOUNTAIN HEMLOCK', '      SUGAR PINE',
     &            '       WHITE FIR', '   INCENSE CEDAR',
     &            '         RED FIR', '   P. SILVER FIR',
     &            '  OTHER SOFTWOOD', '  OTHER HARDWOOD',
     &            '           ASPEN', '     BLUE SPRUCE',
     &            '    CORKBARK FIR', '  WHITEBARK PINE',
     &            '     LIMBER PINE', '      COTTONWOOD',
     &            '    WHITE SPRUCE', '         JUNIPER',
     &            '  OTHER CONIFERS', '   GIANT SEQUOIA',
     &            '       BLACK OAK', '           OTHER',
     &            '    JEFFREY PINE', 'TANOAK/CHINKAPIN',
     &            '     PINYON PINE', '    YELLOW CEDAR',
     &            '         REDWOOD', ' SUBALPINE LARCH',
     &            '   KNOBCONE PINE', '     PACIFIC YEW',
     &            '       NOBLE FIR', '        NON-HOST'/

      DATA TABLE
     &   /'SPREAD',   'CARRY',    'PSTUMP',   'OPEN',    'RRINIT',
     &    'TTDMULT',  'INFMULT',  'INFSIMS',  'END',     'SAREA',
     &    'COMMENT',  'RRDOUT',   'INFKILL',  'WINDTHR', 'BBTYPE1',
     &    'BBTYPE2',  'BBTYPE3',  'INOCSPAN', 'RRCOMP',  'RRECHO',
     &    'STREAD',   'TDISTN',   'RRJUMP',   'RRMINK',  'RSEED',
     &    'PLREAD',   '--RDv3--', 'RRTREIN',  'INFCOLO', 'BORATE',
     &    'SPORE',    'BBTYPE4',  'DNSCALC',  'SMCOUT',  'BBOUT',
     &    'PLOTINF',  'SDIRMULT', 'TIMEDEAD', 'RRHOSTS', 'INOCLIFE',
     &    'BBCLEAR',  'RRTYPE' /

C     DATA TABLE, alphabetically by keyword
C
C     keyword  array index
C
C     --RDv3--  27    Put in as a place holder. If removed, indexing in FVS 
C                     option processor routines must also be corrected.
C     BBCLEAR   41
C     BBOUT     35
C     BBTYPE1   15
C     BBTYPE2   16
C     BBTYPE3   17
C     BBTYPE4   32
C     BORATE    30
C     CARRY      2     Not used in Annosus model
C     COMMENT   11
C     DNSCALC   33
C     END        9
C     INFCOLO   29
C     INFKILL   13
C     INFMULT    7
C     INFSIMS    8
C     INOCLIFE  40
C     INOCSPAN  18
C     OPEN       4
C     PLOTINF   36
C     PLREAD    26
C     PSTUMP     3
C     RRCOMP    19
C     RRDOUT    12
C     RRECHO    20
C     RRHOSTS   39
C     RRINIT     5
C     RRJUMP    23
C     RRMINK    24
C     RRTREIN   28
C     RRTYPE    42
C     RSEED     25
C     SAREA     10
C     SDIRMULT  37
C     SMCOUT    34
C     SPREAD     1
C     SPORE     31
C     STREAD    21
C     TDISTN    22
C     TIMEDEAD  38
C     TTDMULT    6
C     WINDTHR   14

      IROOT = 1
      ISIZE = 42

C.... Load the passed keyword into KEYWRD.

      KEYWRD = PASKEY

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'RDIN',4,ICYC)

   90 CONTINUE

C.... Call the keyword reader.

      CALL KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,
     &    LNOTBK,ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

C.... PROCESS ERRORS
C ... RETURN CODES 0=NO ERROR 1=COLUMN 1 BLANK 2=EOF

      IF (KODE .EQ. 0) GOTO 80
      IF (KODE .EQ. 2) CALL ERRGRO (.FALSE.,2)
      CALL ERRGRO (.TRUE.,6)
      GOTO 90

   80 CONTINUE
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

C.... RETURN CODES 0=NO ERROR 1=KEYWORD NOT FOUND 2=MISSPELLING

      IF (KODE .EQ. 0) GOTO 51
      IF (KODE .EQ. 1) THEN
         CALL ERRGRO (.TRUE.,1)
         GOTO 90
      ENDIF

   51 CONTINUE

C.... PROCESS THE KEYWORD

      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
     &  22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     &  41,42), NUMBER

    1 CONTINUE
C
C  ==========  OPTION NUMBER 1: SPREAD ===============================
C
C     ARG 1 = YR OF CHANGE TO THESE PARAMETERS
C     ARG 2 = 0, STATIC SPREAD MODEL
C     ARG 2 = 1, DYNAMIC SPREAD MODEL
C     ARG 2 = 2, DYNAMIC SPREAD MODEL WITH RATE AVERAGED
C           EVALUATED ONLY IF ARG 2 = 0
C     ARG 3 = X, SPREAD RATE FOR P TYPE CENTERS OR CURRENT ROOT
C           DISEASE CENTERS
C     ARG 4 = X, SPREAD RATE FOR S TYPE CENTERS
C           ONLY USED IF ARG 2 > 0
C     ARG 5 = Y  NUMBER OF MONTE CARLO SIMULATIONS FOR SPREAD
C     ARG 6 = DISEASE TYPE :     1 = P-TYPE ANNOSUS
C                                2 = S-TYPE ANNOSUS
C                                3 = ARMILLARIA
C                                4 = PHELLINUS
C
      IDT = 1
      IPOINT = MINRR 

      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

C.... SET WHICH ROOT DISEASE SPECIES SHOULD BE MODIFIED

      IF (LNOTBK(6)) IPOINT = INT(ARRAY(6))

      IRMIN = MINRR
      IRMAX = MAXRR

      IF (IPOINT .GT. 2) THEN
         IRMIN = IPOINT
         IRMAX = IPOINT
      ELSEIF (IPOINT .EQ. 0) THEN
         IRMIN = 1
         IRMAX = 2
      ENDIF

      PRMS(1) = IRSPTY
      PRMS(2) = RRRSET(IRMIN)
      PRMS(3) = RRRSET(IRMAX)
      PRMS(4) = NMONT

      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)
      IF (PRMS(4) .LT. 0.0) PRMS(4) = 0.0
      IF (PRMS(4) .GT. 50.0) PRMS(4) = 50.0

      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (ARRAY(2) .EQ. 1.0) GOTO 150
      IF (ARRAY(2) .EQ. 2.0) GOTO 175

C.... STATIC SPREAD MODEL

      IRSPTY = 0

      IF (LMTRIC) THEN
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3) * MTOFT
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4) * MTOFT
      ELSE
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      ENDIF

      IF(LKECHO)WRITE(JOSTND,100) KEYWRD, IDT 
  100 FORMAT(/A8,'   STARTING IN YEAR: ',I5,'   USE THE STATIC ',
     &        'SPREAD MODEL')

      IF (LMTRIC) THEN
         IF (IRMIN .EQ. IRMAX) THEN
            IF(LKECHO)WRITE(JOSTND,113) DISTYP(IRMIN),PRMS(2) * FTTOM
  113       FORMAT(/T12, 'SPREAD FOR ',A12,F4.2,' M/YEAR')
         ELSEIF (IRMAX .EQ. IRMIN + 1) THEN
            IF(LKECHO)WRITE(JOSTND,114) DISTYP(IRMIN),PRMS(2) * FTTOM,
     &                        DISTYP(IRMAX),PRMS(3) * FTTOM
  114       FORMAT(/T12, 'SPREAD FOR ',A12,F4.2,' M/YEAR' /
     &              T12, 'SPREAD FOR ',A12,F4.2,' M/YEAR')
         ENDIF
      ELSE
         IF (IRMIN .EQ. IRMAX) THEN
            IF(LKECHO)WRITE(JOSTND,103) DISTYP(IRMIN),PRMS(2)
  103       FORMAT(/T12, 'SPREAD FOR ',A12,F4.2,' FEET/YEAR')
         ELSEIF (IRMAX .EQ. IRMIN + 1) THEN
            IF(LKECHO)WRITE(JOSTND,104) DISTYP(IRMIN),PRMS(2),
     &                        DISTYP(IRMAX),PRMS(3)
  104       FORMAT(/T12, 'SPREAD FOR ',A12,F4.2,' FEET/YEAR' /
     &              T12, 'SPREAD FOR ',A12,F4.2,' FEET/YEAR')
         ENDIF
      ENDIF
      GOTO 180

  150 CONTINUE

C.... DYNAMIC SPREAD MODEL WITH A VARIETY OF SPREAD RATES USED

      IRSPTY = 1

      IF(LKECHO)WRITE(JOSTND,101) KEYWRD,IDT,INT(PRMS(4))
  101 FORMAT (/A8,'   STARTING IN YEAR: ',I5,'  USE THE DYNAMIC ',
     &        'SPREAD MODEL (VARIABLE RATES).',/T12,'DO ',
     &         I4,'  SPREAD SIMULATIONS')
      GOTO 180

  175 CONTINUE

C.... DYNAMIC SPREAD MODEL WITH AVERAGE SPREAD RATE APPLIED TO ALL
C.... CENTERS

      IRSPTY = 2

      IF(LKECHO)WRITE(JOSTND,102) KEYWRD,IDT,INT(PRMS(4))
  102 FORMAT (/A8,'   STARTING IN YEAR: ',I5,'  USE THE DYNAMIC ',
     &        'SPREAD MODEL (AVERAGE RATES).',/T12,'DO ',
     &         I4,'  SPREAD SIMULATIONS')
  180 CONTINUE

      CALL OPNEW (KODE,IDT,MYACT(1),4,PRMS)
      IF (KODE .GT. 0) GOTO 90

      GOTO 90

    2 CONTINUE
C
C  ==========  OPTION NUMBER 2: CARRY ================================
C
C     OPTION PROCESSING LOGIC FOR CARRYOVER MODEL IS CONTAINED IN
C     SUBROUTINE DISEAS. DEFAULT IS STATIC CARRY MODEL.
C
C     ****NOT YET METRIFIED.  IF DECIDE TO USE, MUST ADD METRIC OPTIONS*
C
C     IF(LKECHO)WRITE(JOSTND,*) ' '
C     IF(LKECHO)WRITE(JOSTND,*) 'CARRY      KEYWORD NOT USED IN GENERAL ROOT ',
C    &                 'DISEASE MODEL'
C     GOTO 90
C
C     ICRRY = 0
C     IF (ARRAY(2) .GT. 0.0) IRGEN(1) = INT(ARRAY(2))
C     IF (ARRAY(1) .GT. 0) GOTO 202
C
C     STATIC CARRYOVER MODEL, LOOK FOR PINSET, CHECK THAT IT'S IN BOUNDS
C
C     IFRRC = 0
C     IRFLAG = 1
C     IF ((LNOTBK(4)) .AND. ARRAY(4) .GE. 0.0 .AND. ARRAY(4) .LE. 100.0)
C    &    NNCENT = INT(ARRAY(4))
C     IF (ARRAY(3) .LT. 0.0 .OR. ARRAY(3) .GT. 1.0) GOTO 203
C     IF (LNOTBK(3)) PINSET = ARRAY(3)
C     GOTO 204
C
C 203 CONTINUE
C     ICRRY = 1
C
C 204 CONTINUE
C     GOTO 205
C
C 202 CONTINUE
C
C     DYNAMIC CARRYOVER MODEL, CHECK FOR DEFINITION OF MINIMUM
C     SPREAD RATE IN ARRAY(3)
C
C     IFRRC = 1
C     IRFLAG = 0
C     IF (ARRAY(3) .GT. 0.0) RRGEN(1,7) = ARRAY(3)
C
C     OUTPUT FOR CARRY KEYWORD
C
C 205 CONTINUE
C     IF (IFRRC .EQ. 1) GOTO 206
C     IF (ICRRY .EQ. 1) GOTO 207
C
C     IF(LKECHO)WRITE(JOSTND,208) KEYWRD, PINSET,NNCENT,IRGEN(1)
C 208 FORMAT(/A8, '   STATIC CARRYOVER MODEL',
C    &     2X,'; PROB. OF NEW INFECTION CENTERS BEING FORMED=',F4.2,
C    &      ';  NEW CENTERS = ',I4,/
C    &        T12,'MODEL CALLED ',I4,' CYCLES AFTER MANAGEMENT')
C     GOTO 90
C
C 207 CONTINUE
C     IF(LKECHO)WRITE(JOSTND,210) KEYWRD,ARRAY(2),PINSET
C 210 FORMAT (/A8,'*** CARRYOVER MODEL KEYWORD, PROBABILITY',
C    & ' OF INFECTION',/,
C    &      T12, '***** ASSIGNED ',F4.2,' OUT OF BOUNDS, SET TO ',F4.2)
C     GOTO 90
C
C 206 CONTINUE
C     IF(LKECHO)WRITE(JOSTND,211) KEYWRD,RRGEN(1,7),IRGEN(1)
C 211 FORMAT (/A8,'   DYNAMIC CARRYOVER MODEL, MINIMUM SPREAD= ',
C    &      F5.2, ' FT/YR  MODEL CALLED ',I4,' CYCLES AFTER MANAGEMENT')

      GOTO 90

    3 CONTINUE
C
C  ==========  OPTION NUMBER 3: PSTUMP ===============================
C
C     ARG 1 - YEAR OF STUMP PUSHING
C     ARG 2 - STUMP PUSHING EFFICIENCY.  THIS IS THE PROPORTION OF
C             STUMPS THAT WILL BE REMOVED BY THE STUMP-PUSHING ACTIVITY.
C             DEFAULT EFFICIENCY IS 1.0
C     ARG 3 - MINIMUM STUMP DIAMETER TO BE REMOVED.  ALL STUMPS OF
C             THIS DIAMETER AND GREATER WILL BE REMOVED WITH THE
C             EFFICIENCY STATED IN FIELD 2.  IF LEFT BLANK, MINIMUM
C             DBH IS SET TO 0.
C
C
      IDT = 1
      IRRPSH = 0

      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

      PRMS(1) = PPUSH
      PRMS(2) = PRREM

      IF (ARRAY(2) .LE. 1.0 .AND. ARRAY(2) .GT. 0.0) THEN
         IRRPSH = 1
         PRMS(1) = ARRAY(2)
      ENDIF

      IF (ARRAY(3) .GT. 0.0 .AND. ARRAY(3) .LE. 999.0) THEN
         IF (LMTRIC) PRMS(2) = ARRAY(3) * CMTOIN
         IF (.NOT. LMTRIC) PRMS(2) = ARRAY(3)
      ENDIF

      CALL OPNEW (KODE,IDT,MYACT(2),2,PRMS)
      IF (KODE .GT. 0) GOTO 90

C.... OUTPUT FOR PSTUMP KEYWORD

      IF (IRRPSH .EQ. 1) GOTO 301
      WRITE(JOSTND,302) KEYWRD, ARRAY(2), PPUSH
  302 FORMAT (/A8,'   ***** ',F4.2,' STUMP-PUSHING EFFICIENCY ',
     &      'OUT OF BOUNDS',/,T12,'***** SET TO ',F4.2)
      GOTO 90

  301 CONTINUE
      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,304)
     &      KEYWRD, IDT, PRMS(1),PRMS(2)*INTOCM
  304    FORMAT(/A8,'   DATE/CYCLE=',I5,';  EFFICIENCY = ',F4.2,
     &            ';   MIN SIZE = ',F6.2,' CMS DBH')
      ELSE
         IF(LKECHO)WRITE(JOSTND,303) KEYWRD, IDT, PRMS(1), PRMS(2)
  303    FORMAT(/A8,'   DATE/CYCLE=',I5,';  EFFICIENCY = ',F4.2,
     &            ';   MIN SIZE = ',F6.2,' INCHES DBH')
      ENDIF

      GO TO 90

    4 CONTINUE
C
C  ==========  OPTION NUMBER 4: OPEN    ==============================
C
C     OPEN FILE.  USED WITH KEYWORD RRECHO, BBOUT, SMCOUT, INFSIMS.
C     DEFAULT VALUES ARE: UNIT 24 (RRECHO DEFAULT)
C     SPACES TREATED AS ZEROS, STATUS FRESH, REC LENGTH 132,
C     FILE NAME MUST BE SUPPLIED AS A SUPPLEMENTAL RECORD.
C

      IF (.NOT. LNOTBK(1)) ARRAY(1) = 24
      IF (.NOT. LNOTBK(2)) ARRAY(2) = 0
      IF (.NOT. LNOTBK(3)) ARRAY(3) = 3
      IF (.NOT. LNOTBK(4)) ARRAY(4) = 132

C.... CALL PROGNOSIS SUBROUTINE KEYOPN FOR OPEN KEYWORD.

      CALL KEYOPN (IREAD,RECORD,JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
      GOTO 90

    5 CONTINUE
C
C  ==========  OPTION NUMBER 5: RRINIT ===============================
C
C     THIS KEYWORD IS USED WHEN ROOT DISEASE INITIALIZATION IS
C     PERFORMED 'MANUALLY' IE THE INITIALIZATION INFORMATION IS SUPPLIED
C     BY THE USER.
C
C     CENTERS CAN BE EITHER READ IN OR SET RANDOMLY
C
C     FIELD 7 CONTAINS THE DISEASE TYPE.  1 = P TYPE ANNOSUS
C                                         2 = S TYPE ANNOSUS
C                                         3 = ARMILLARIA
C                                         4 = PHELLINUS
C
      IDI = MINRR
      IF (LNOTBK(7)) IDI = INT(ARRAY(7))
C
C     Modification April 17, 1998 (RNH APR98) to correct setting of 
C     root disease type and number of centers (ie. RRMAX and RRMIN
C     when RRINIT keyword is used without the RRTYPE keyword.  
C
C      MAXRR= IDI
C
C     end modification (RNH APR98)

      RRMAN = .TRUE.
      IF (ARRAY(1) .LT. 1.0) GOTO 520

C.... THE ATTRIBUTES OF EACH CENTER ARE READ IN
C.... AND ARRAY(2) CONTAINS THE NUMBER OF CENTERS TO READ
C.... IN.  CHECK THAT ONLY 100, AT MOST CENTERS ARE READ IN

      IF (LNOTBK(2)) NCENTS(IDI) = INT(ARRAY(2))
      IF (ARRAY(2) .LE. 100.0) GOTO 502
      WRITE(JOSTND,503)
  503 FORMAT(T12,'**** TOO MANY CENTERS TO READ IN, SET TO 100')
      NCENTS(IDI) = 100

  502 CONTINUE
      DO 504 IC=1,NCENTS(IDI)
         READ(IREAD,505) PCENTS(IDI,IC,1), PCENTS(IDI,IC,2),
     &                   PCENTS(IDI,IC,3)
  505    FORMAT (3F7.1)

         IF (LMTRIC) THEN
            PCENTS(IDI,IC,1) = PCENTS(IDI,IC,1) * MTOFT
            PCENTS(IDI,IC,2) = PCENTS(IDI,IC,2) * MTOFT
            PCENTS(IDI,IC,3) = PCENTS(IDI,IC,3) * MTOFT
         ENDIF   

         IF (PCENTS(IDI,IC,1) .LE. DIMEN) GOTO 506

         WRITE(JOSTND,507) IC, DIMEN
  507    FORMAT (/T12,'***** ERROR IN PATCH ',I4,
     &          ' X-COORD SET TO ',F7.0)
         PCENTS(IDI,IC,1) = DIMEN

  506    CONTINUE
         IF (PCENTS(IDI,IC,2) .LE. DIMEN) GOTO 504

         WRITE(JOSTND,508) IC,DIMEN
  508    FORMAT (/T12,'***** ERROR IN PATCH ',I4,
     &          ' Y-COORD SET TO ',F7.0)
         PCENTS(IDI,IC,2) = DIMEN

  504 CONTINUE

C.... SET PAREA(IDI) TO -1 TO INDICATE THAT CENTERS WERE READ IN
C.... MANUALLY AND SET LPAREA TO TRUE SO PAREA WONT BE RESET AT END
C.... OF PROCESSING.

      PAREA(IDI) = -1.0
      LPAREA(IDI) = .TRUE.

      IPCFLG(IDI) = 1
      GOTO 530

  520 CONTINUE

C.... THE ATTRIBUTES OF THE CENTERS ARE ASSIGNED RANDOMLY
C.... THE USER MUST SET THE TOTAL NUMBER OF CENTERS IN
C.... ARRAY(2) AND THE TOTAL DISEASED AREA IN ARRAY(6)

      IF (LNOTBK(2)) NCENTS(IDI) = INT(ARRAY(2))
      IF (ARRAY(2) .LE. 100.0) GOTO 525

      WRITE(JOSTND,521)
  521 FORMAT(T12,'**** TOO MANY CENTERS TO ASSIGN, SET TO 100')

      NCENTS(IDI) = 100

  525 CONTINUE
      IF (LMTRIC) THEN
         IF (LNOTBK(6)) PAREA(IDI) = ARRAY(6) * HAtoACR
      ELSE
         IF (LNOTBK(6)) PAREA(IDI) = ARRAY(6)
      ENDIF   

      IF (LNOTBK(6)) LPAREA(IDI) = .TRUE.
      IPCFLG(IDI) = 0

C.... IF THERE IS ONLY ONE CENTER AND THE REQUESTED AREA IS THE STAND
C.... AREA THEN RUN THE STAND AS ONE CENTER

      IF (NCENTS(IDI) .EQ. 1 .AND. PAREA(IDI) .EQ. SAREA)
     &        LONECT(IDI) = 1

  530 CONTINUE

C.... SET UP OTHER PARAMETERS

      IF (LNOTBK(3) .AND. LNOTBK(4)) THEN
         IF (LMTRIC) THEN
            IF (ARRAY(3) .GE. 0.0) PRKILL(IDI) = ARRAY(3) / HAtoACR
            IF (ARRAY(4) .GE. 0.0) PRUN(IDI) = ARRAY(4) / HAtoACR
         ELSE
            IF (ARRAY(3) .GE. 0.0) PRKILL(IDI) = ARRAY(3)
            IF (ARRAY(4) .GE. 0.0) PRUN(IDI) = ARRAY(4)
         ENDIF
         LINIT = .TRUE.
      ELSE
         LINIT = .FALSE.
      ENDIF

      IF (LNOTBK(5) .AND. ARRAY(5) .GE. 0.0 .AND. ARRAY(5) .LE. 1.0)
     &   RRINCS(IDI) = ARRAY(5)

C.... OUTPUT FOR RRINIT KEYWORD

      IF (IPCFLG(IDI) .EQ. 1) GOTO 550

      IF(LKECHO)WRITE(JOSTND,541) KEYWRD, DISTYP(IDI)
  541 FORMAT (/A8,'   ROOT DISEASE IS ', A12 /
     &      T12,'ROOT DISEASE INITIALIZATION KEYWORD; ',
     &      'ROOT DISEASE CENTERS ASSIGNED RANDOMLY '/
     &         T12,'INITIALIZATION WILL BE WITH USER-SUPPLIED DATA',
     &             ' UNLESS THE RRTREIN KEYWORD IS USED IN THE',
     &             ' RUNSTREAM')

      IF (LINIT) THEN
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,572) NCENTS(IDI),
     &         PRKILL(IDI)/ACRtoHA,PRUN(IDI)/ACRtoHA
  572       FORMAT (T12,'NUMBER OF CENTERS=',I4/
     &            T12,'INFECTED TREES/HA IN DISEASED AREAS   = ',F7.2/
     &            T12,'UNINFECTED TREES/HA IN DISEASED AREAS = ',F7.2)
         ELSE
            IF(LKECHO)WRITE(JOSTND,542)
     &         NCENTS(IDI),PRKILL(IDI),PRUN(IDI)
  542       FORMAT (T12,'NUMBER OF CENTERS=',I4/
     &         T12,'INFECTED TREES/ACRE IN DISEASED AREAS   = ',F7.2/
     &         T12,'UNINFECTED TREES/ACRE IN DISEASED AREAS = ',F7.2)
         ENDIF

      ELSE
         IF(LKECHO)WRITE(JOSTND,543) NCENTS(IDI)
  543    FORMAT (T12,'NUMBER OF CENTERS=',I4)

         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,575)
  575       FORMAT (T12,'50% OF TREES/HA IN THE DISEASED AREAS ARE',
     &               ' INFECTED'/
     &              T12,'50% OF TREES/HA IN THE DISEASED AREAS ARE',
     &               ' UNINFECTED')
         ELSE
            IF(LKECHO)WRITE(JOSTND,576)
  576       FORMAT (T12,'50% OF TREES/ACRE IN THE DISEASED AREAS ARE',
     &               ' INFECTED'/
     &              T12,'50% OF TREES/ACRE IN THE DISEASED AREAS ARE',
     &               ' UNINFECTED')
         ENDIF

         IF (LNOTBK(3).OR.LNOTBK(4)) WRITE(JOSTND,544)
  544    FORMAT (T9,'** EITHER FIELD 3 OR FIELD 4 WAS NOT SPECIFIED.',
     &           ' DEFAULTS WERE USED IN BOTH FIELDS')
      ENDIF

      IF (PAREA(IDI) .LT. 0.0) THEN
         IF(LKECHO)WRITE(JOSTND,571) RRINCS(IDI)
  571    FORMAT (T12,'PROPORTION ROOTS INFECTED=',F4.2,
     &          /T12,'ROOT DISEASE AREA WILL BE CALCULATED FROM THE ',
     &          'PROPORTION OF INFECTED PLOTS IN THE STAND.')

      ELSE
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,585) RRINCS(IDI),
     &        PAREA(IDI) * ACRtoHA
  585       FORMAT (T12,'PROPORTION ROOTS INFECTED=',F4.2,
     &                  ' ;DISEASE AREA = ',F6.2,' HA')
         ELSE
            IF(LKECHO)WRITE(JOSTND,545) RRINCS(IDI), PAREA(IDI)
  545       FORMAT (T12,'PROPORTION ROOTS INFECTED=',F4.2,
     &                  ' ;DISEASE AREA = ',F6.2,' ACRES')
         ENDIF
      ENDIF

      IF (LONECT(IDI) .EQ. 1.0) THEN
        IF(LKECHO)WRITE(JOSTND,547)
  547   FORMAT (T12,'THE STAND WILL BE MODELLED AS ONE DISEASE CENTER')
      ENDIF

      GOTO 590

  550 CONTINUE
      IF(LKECHO)WRITE(JOSTND,551) KEYWRD, DISTYP(IDI)
  551 FORMAT (/A8,'   ROOT DISEASE IS ', A12 /
     &      T12,'ROOT DISEASE INITIALIZATION KEYWORD; ',
     &      ' ROOT DISEASE CENTERS READ IN '/
     &         T12,'INITIALIZATION WILL BE WITH USER-SUPPLIED DATA',
     &             ' UNLESS THE RRTREIN KEYWORD IS USED IN THE',
     &             ' RUNSTREAM')

      IF (LINIT) THEN
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,572) NCENTS(IDI),
     &         PRKILL(IDI)/ACRtoHA, PRUN(IDI)/ACRtoHA
         ELSE
            IF(LKECHO)WRITE(JOSTND,542)
     &         NCENTS(IDI),PRKILL(IDI),PRUN(IDI)
         ENDIF
      ELSE
         IF(LKECHO)WRITE(JOSTND,543) NCENTS(IDI)
         IF (LMTRIC.AND.LKECHO)WRITE(JOSTND,575)
         IF (.NOT.LMTRIC.AND.LKECHO)WRITE(JOSTND,576)
         IF ((LNOTBK(3).OR.LNOTBK(4)).AND.LKECHO)WRITE(JOSTND,544)
      ENDIF

      IF(LKECHO)WRITE(JOSTND,552) RRINCS(IDI)
  552 FORMAT (T12,'PROPORTION ROOTS INFECTED=',F4.2)

      IF(LKECHO)WRITE(JOSTND,553)
  553 FORMAT (//,T12,'X-COORD    Y-COORD   RADIUS')
      IF(LKECHO)WRITE(JOSTND,554)
  554 FORMAT (T12,'------------------------------')

      DO 556 IC=1,NCENTS(IDI)
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,555) PCENTS(IDI,IC,1) * FTTOM,
     &                         PCENTS(IDI,IC,2) * FTTOM,
     &                         PCENTS(IDI,IC,3) * FTTOM
         ELSE
            IF(LKECHO)WRITE(JOSTND,555) PCENTS(IDI,IC,1),
     &          PCENTS(IDI,IC,2), PCENTS(IDI,IC,3)
         ENDIF
  555    FORMAT (T12,F7.1,4X,F7.1,2X,F7.1)
  556 CONTINUE

  590 CONTINUE
      GO TO 90

    6 CONTINUE
C
C  ==========  OPTION NUMBER 6: TTDMULT ==============================
C
C     TIME TO DEATH MULTIPLIERS
C     CHECK THAT ROOT DISEASE TYPE IS ACCEPTABLE
C
      IPOINT = 0
      IF (LNOTBK(2)) IPOINT = INT(ARRAY(2))
      IF (ARRAY(1) .GT. 0.0) IRGEN(8) = INT(ARRAY(1))
      IHB = 1
      IF (IRGEN(8) .NE. 3000) IHB = 2
      IF (INT(ARRAY(2)) .LE. ITOTRR) GOTO 601

      WRITE(JOSTND,602)
  602 FORMAT (/, '********   ERROR IN DISEASE TYPE, TTDMULT KEYWORD')
      WRITE(JOSTND,603)
  603 FORMAT ('********   KEYWORD WILL AFFECT ALL SPECIFIED DISEASE ',
     &        'TYPES')
      IPOINT = 0

  601 CONTINUE

C.... SET WHICH ROOT DISEASE SPECIES SHOULD BE MODIFIED

      IRMIN = MINRR
      IRMAX = MAXRR
      IF (IPOINT .GT. 0) THEN
         IRMIN = IPOINT
         IRMAX = IPOINT
      ENDIF

C.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

      CALL SPDECD(3,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

C.... NOW, CHECK WHETHER ONLY ONE VALUE TO BE CHANGED, OR ALL

      IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....    ONLY ONE TREE SPECIES VALUE TO BE CHANGED
C....    (FOR ONE OR MORE RR TYPES)

         DO 670 IDI=IRMIN,IRMAX
            HABFAC(IRTSPC(ISPC),IDI,IHB) = ARRAY(4)
  670    CONTINUE

      ELSEIF (ISPC .EQ. 0) THEN

C....    ALL SPECIES GET CHANGED

         READ (IREAD,606) (HABFAC(IRTSPC(KSP),IRMIN,IHB),KSP=1,MAXSP)
         DO 672 IDI=IRMIN+1,IRMAX
            DO 671 KSP=1,MAXSP
               HABFAC(IRTSPC(KSP),IDI,IHB) =
     &               HABFAC(IRTSPC(KSP),IRMIN,IHB)
  671       CONTINUE
  672    CONTINUE
  606    FORMAT (11F5.2)

      ELSE

C....    ERROR IN READING IN SPECIES CODE

         WRITE(JOSTND,610) KEYWRD
  610    FORMAT ('********   ERROR IN KEYWORD ', A8, ', SPECIES ',
     &           'CODE NOT VALID.  KEYWORD IGNORED.')
         GOTO 90

      ENDIF

C.... OUTPUT FOR TTDMULT KEYWORD

      IF(LKECHO)WRITE(JOSTND,620) KEYWRD
  620 FORMAT (/A8,'   TIME TO DEATH MULTIPLIERS ')
      IF(LKECHO)WRITE(JOSTND,621)
  621 FORMAT (//T12,'MULTIPLIERS ARE RELATIVE TO INTERIOR',
     &        ' DOUGLAS-FIR')
      IF(LKECHO)WRITE(JOSTND,622)
  622 FORMAT (T12,'ON DOUGLAS-FIR HABITAT')
      IF(LKECHO)WRITE(JOSTND,623)
  623 FORMAT (/T12,5X,'SPECIES',14X,'MULTIPLIER')
      IF(LKECHO)WRITE(JOSTND,624)
  624 FORMAT (T12,41('-'))

      DO 675 IDI=IRMIN,IRMAX
         IF(LKECHO)WRITE(JOSTND,680) DISTYP(IDI)
         IF(LKECHO)WRITE(JOSTND,681)

         DO 630 KSP=1,MAXSP
            IF(LKECHO)WRITE(JOSTND,631) RRJSP(IRTSPC(KSP)),
     &                         HABFAC(IRTSPC(KSP),IDI,1)
  630    CONTINUE
  675 CONTINUE

  680 FORMAT (T12,A16)
  681 FORMAT (T12,16('-'))
  631 FORMAT (T12,A16,13X,F5.2)

      IF (IRGEN(8) .EQ. 3000) GOTO 640
      IF(LKECHO)WRITE(JOSTND,641) IRGEN(8)
  641 FORMAT (/T12,'TIME TO DEATH MULTIPLIERS AFTER ',I4)
      IF(LKECHO)WRITE(JOSTND,623)
      IF(LKECHO)WRITE(JOSTND,624)

      DO 676 IDI=IRMIN,IRMAX
         IF(LKECHO)WRITE(JOSTND,680) DISTYP(IDI)
         IF(LKECHO)WRITE(JOSTND,681)

         DO 642 KSP=1,MAXSP
            IF(LKECHO)WRITE(JOSTND,631) RRJSP(IRTSPC(KSP)),
     &                        HABFAC(IRTSPC(KSP),IDI,2)
  642    CONTINUE
  676 CONTINUE

  640 CONTINUE
      GOTO 90

    7 CONTINUE
C
C  ==========  OPTION NUMBER 7: INFMULT ==============================
C
C     PROBABILITY OF INFECTION MULTIPLIERS
C     CHECK FOR YEAR THAT MULTIPLIERS ARE TO TAKE EFFECT
C     CHECK THAT ROOT DISEASE TYPE IS ACCEPTABLE

      IPOINT = 0 
      IF (ARRAY(1) .GT. 0.0) IRGEN(9) = INT(ARRAY(1))
      IF (LNOTBK(2)) IPOINT = INT(ARRAY(2))

C.... CHECK TO MAKE SURE ROOT DISEASE TYPE IS ACCEPTABLE

      IF (INT(ARRAY(2)) .GT. ITOTRR) THEN
         WRITE(JOSTND,702)
         WRITE(JOSTND,703)
         IPOINT = 0
      ENDIF

  702 FORMAT (/,'********   ERROR IN DISEASE TYPE, INFMULT KEYWORD')
  703 FORMAT ('********   CHANGE WILL AFFECT ALL ACTIVE DISEASE ',
     &        'TYPES')

C.... SET THE ROOT DISEASE TYPE TO BE MODIFIED

      IRMIN = MINRR
      IRMAX = MAXRR
      IF (IPOINT .GT. 0) THEN
         IRMIN = IPOINT
         IRMAX = IPOINT
      ENDIF

C.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

      CALL SPDECD(3,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

C.... NOW, CHECK WHETHER ONLY ONE VALUE TO BE CHANGED, OR ALL

      IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....    ONLY ONE VALUE TO BE CHANGED

         DO 770 IDI=IRMIN,IRMAX
            IF (IRGEN(9) .EQ. 3000) THEN
               PNINF(IRTSPC(ISPC),IDI) = ARRAY(4)
            ELSE
               SSSFAC(IRTSPC(ISPC),IDI) =
     &               ARRAY(4) / PNINF(IRTSPC(ISPC),IDI)
            ENDIF
  770    CONTINUE

      ELSEIF (ISPC .EQ. 0) THEN

C....    ALL SPECIES GET CHANGED

         IF (IRGEN(9) .EQ. 3000) THEN
            READ (IREAD,711) (PNINF(IRTSPC(KSP),IRMIN),KSP=1,MAXSP)
         ELSE
            READ (IREAD,711) (SSSFAC(IRTSPC(KSP),IRMIN),KSP=1,MAXSP)
         ENDIF
  711    FORMAT (11F5.2)

         DO 772 IDI=IRMIN,IRMAX
            DO 712 KSP=1,MAXSP
               IF (IDI .GT. IRMIN) THEN
                  IF (IRGEN(9) .EQ. 3000) THEN
                     PNINF(IRTSPC(KSP),IDI) = PNINF(IRTSPC(KSP),IRMIN)
                  ELSE
                     SSSFAC(IRTSPC(KSP),IDI) = SSSFAC(IRTSPC(KSP),IRMIN)
                  ENDIF
               ENDIF
               IF (PNINF(IRTSPC(KSP),IDI) .GT. 0.0) THEN
                  SSSFAC(IRTSPC(KSP),IDI) = SSSFAC(IRTSPC(KSP),IDI) /
     &                                 PNINF(IRTSPC(KSP),IDI)
               ENDIF
  712       CONTINUE
  772    CONTINUE

      ELSE

C....    ERROR IN READING IN SPECIES CODE

         WRITE(JOSTND,610) KEYWRD
         GOTO 90
      ENDIF

C.... OUTPUT FOR INFMULT KEYWORD

  720 CONTINUE
      IF(LKECHO)WRITE(JOSTND,721) KEYWRD
  721 FORMAT (/A8,'   PROBABILITY OF INFECTION FOR ROOT DISEASE')
      IF(LKECHO)WRITE(JOSTND,722)
  722 FORMAT (/T12,5X,'SPECIES',14X,'PROBABILITY')
      IF(LKECHO)WRITE(JOSTND,723)
  723 FORMAT (T12,41('-'))

      DO 775 IDI=IRMIN,IRMAX
         IF(LKECHO)WRITE(JOSTND,780) DISTYP(IDI)
         IF(LKECHO)WRITE(JOSTND,781)

         DO 724 KSP=1,MAXSP
            IF(LKECHO)WRITE(JOSTND,725)
     &         RRJSP(IRTSPC(KSP)),PNINF(IRTSPC(KSP),IDI)
  724    CONTINUE
  775 CONTINUE

  780 FORMAT (T12,A16)
  781 FORMAT (T12,16('-'))
  725 FORMAT(T12,A16,13X,F5.2)

      IF (IRGEN(9) .EQ. 3000) GOTO 740
      IF(LKECHO)WRITE(JOSTND,726) IRGEN(9)
  726 FORMAT (/T12,'PROBABILITY OF INFECTION AFTER ',I4)
      IF(LKECHO)WRITE(JOSTND,722)
      IF(LKECHO)WRITE(JOSTND,723)

      DO 785 IDI=IRMIN,IRMAX
         IF(LKECHO)WRITE(JOSTND,780) DISTYP(IDI)
         IF(LKECHO)WRITE(JOSTND,781)

         DO 730 KSP=1,MAXSP
            TT = PNINF(IRTSPC(KSP),IDI) * SSSFAC(IRTSPC(KSP),IDI)
            IF(LKECHO)WRITE(JOSTND,725) RRJSP(IRTSPC(KSP)), TT
  730    CONTINUE
  785 CONTINUE

  740 CONTINUE
      GOTO 90

    8 CONTINUE
C
C  ==========  OPTION NUMBER 8: INFSIMS ==============================
C
C     SET NUMBER OF SIMULATIONS TO DO THE INSIDE CENTER INFECTION
C     SIMULATION ALSO, CAN REQUEST OUTPUT FROM THE INSIDE CENTER
C     INFECTION SIMULATION (IN REGULAR TABLE FORMAT, WITH HEADINGS)
C     (TO PRINT OUTPUT, FIELD 2 MUST CONTAIN SOME CHARATER, AND
C     FIELD 3 MUST CONTAIN THE FILE NUMBER. IF FIELD 2 IS NOT BLANK
C     AND FIELD 3 IS BLANK, THE DEFAULT FILE NUMBER WILL BE USED)
C

      IF (LNOTBK(1)) NINSIM = INT(ARRAY(1))
      IF (NINSIM .GT. 50) NINSIM = 50
      
      ISDOUT = 0
      IF (LNOTBK(2)) THEN
         ISDOUT = 25
         IF (ARRAY(3) .GT. 0.0) ISDOUT = INT(ARRAY(3))
      ENDIF

C.... OUTPUT FOR INFSIMS KEYWORD

      IF(LKECHO)WRITE(JOSTND,801) KEYWRD, NINSIM
  801 FORMAT(/A8,'   NUMBER OF TIMES TO SIMULATE THE INSIDE ',
     &       'INFECTION DYNAMICS IS: ',I4)

      IF (ISDOUT .GT. 0) THEN
         IF(LKECHO)WRITE(JOSTND,802) ISDOUT
  802    FORMAT (/T12,'   INSIDE CENTER INFECTION OUTPUT ON LOGICAL',
     &           ' UNIT ',I4)
      ENDIF

      GOTO 90

    9 CONTINUE


C
C  ==========  OPTION NUMBER 9: END ==================================
C
C     END KEYWORD. END THE ROOT DISEASE KEYWORD PROCESSING.
C     OPEN ROOT DISEASE FILES HERE. CALL IS PUT HERE TO INSURE
C     FILES ARE OPENED AFTER ANY DEFAULT UNITS HAVE BEEN MODIFIED BY
C     KEYWORD.
C
      IF(LKECHO)WRITE(JOSTND,901) KEYWRD
  901 FORMAT (/A8,'   END OF ROOT DISEASE KEYWORDS')

C.... IF NEITHER INITIALIZATION METHOD SPECIFIED,THE SIMULATION 
C.... CONTINUES WITHOUT ROOT DISEASE IMPACTS.

      IF (.NOT. RRTINV .AND. .NOT. RRMAN) THEN
         WRITE(JOSTND,905)

  905    FORMAT (/'***** FVSRD ERROR: NEITHER TREELIST OR MANUAL ',
     &            'INITIALIZATION SPECIFIED. ROOT DISEASE NOT ',
     &            'ACTIVATED.*****')
         CALL ERRGRO (.TRUE.,4)
         GOTO 9000
      ENDIF
C
C     If the RRTYPE keword has not be used the disease type for the
C     simulation will be the defalt - annosus disease. The following
C     statement checks for the use of RRTYPE and writes warning message
C     to the output file (RNH May98)
C
      IF (.NOT. LRTYPE) WRITE(JOSTND, 907)

  907 FORMAT (/'****** WARNING: THE "RRTYPE" KEYWORD WAS NOT USED,',
     &         '>> ANNOSUS ROOT DISEASE <<, WILL BE SIMULATED.')

C.... IF NO BARK BEETLES ARE SCHEDULED, AND THE USER HAS NOT SPECIFIED
C.... THAT THE CHOICE OF NO BARK BEETLES WAS INTENTIONAL, THEN
C.... TURN ON ALL BARK BEETLES USING THEIR DEFAULT VALUES.

      IF (IDOBB .LE. 0 .AND. LBBON) THEN
         WRITE(JOSTND,911)
  911    FORMAT (/'******* WARNING: NO BARK BEETLES WERE SPECIFIED ',
     &     'AND THE BBCLEAR KEYWORD WAS NOT USED.',/T18,
     &     'BARK BEETLES ARE ON USING THEIR DEFAULT VALUES AS FOLLOWS')

C....    TYPE 1 BARK BEETLES

         IDOBB = IDOBB + 1
         PRMS(1) = RROBTS(1)
         PRMS(2) = RROBSC(1)
         PRMS(3) = RROBOL(1)
         PRMS(4) = RROBMR(1)
         PRMS(5) = 0.0

         CALL OPNEW (KODE,1,MYACT(4),5,PRMS)
         IF (KODE .GT. 0) GOTO 913

         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND, 952) PRMS(1), PRMS(2)*INTOCM,
     &                          PRMS(3)/ACRtoHA, PRMS(4)
         ELSE
            IF(LKECHO)WRITE(JOSTND, 912) (PRMS(I), I=1,4)
         ENDIF

  912    FORMAT (/'BBTYPE1    TYPE 1 BARK BEETLE IS ON EVERY YEAR.',
     &           ' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,' INCHES',
     &           /T12,'MINIMUM ELIGIBLE STEMS FOR EVENT =',F7.0,
     &           ' STEMS/ACRE;   MORTALITY RATE = ',F5.3)
  952    FORMAT (/'BBTYPE1    TYPE 1 BARK BEETLE IS ON EVERY YEAR.',
     &           ' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,' CMS',
     &           /T12,'MINIMUM ELIGIBLE STEMS FOR EVENT =',F7.0,
     &           ' STEMS/HA;   MORTALITY RATE = ',F5.3)

  913    CONTINUE

C....    TYPE 3 BARK BEETLES

         IDOBB = IDOBB + 1
         IDT = 1

         PRMS(1) = RROBTS(3)
         PRMS(2) = RROBSC(3)
         PRMS(3) = RROBOL(3)
         PRMS(4) = RROBMR(3)
         PRMS(5) = RROBRD(3)
         PRMS(6) = 0.0

         CALL OPNEW (KODE,1,MYACT(6),6,PRMS)
         IF (KODE .GT. 0) GOTO 915

         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND, 954) PRMS(1), PRMS(2)*INTOCM,
     &                          PRMS(3)/ACRtoHA, PRMS(4), PRMS(5)
         ELSE
            IF(LKECHO)WRITE(JOSTND, 914) (PRMS(I), I=1,5)
         ENDIF

  914    FORMAT(/'BBTYPE3    TYPE 3 BARK BEETLE IS ON EVERY YEAR',
     &          ' SPECIES =', F3.0, ';  MINIMUM DBH=', F5.2, '  INCHES',
     &          /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.0,
     &          ' STEMS', '/ACRE   ;  MORTALITY RATE = ', F4.2,
     &          /T12, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)
  954    FORMAT(/'BBTYPE3    TYPE 3 BARK BEETLE IS ON EVERY YEAR',
     &          ' SPECIES =', F3.0, ';  MINIMUM DBH=', F5.2, '  CMS',
     &          /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.0,
     &          ' STEMS', '/HA   ;  MORTALITY RATE = ', F4.2,
     &          /T12, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)

  915    CONTINUE

C....    TYPE 4 BARK BEETLES

         IDOBB = IDOBB + 1
         PRMS(1) = RROBTS(4)
         PRMS(2) = RROBSC(4)
         PRMS(3) = RROBOL(4)
         PRMS(4) = RROBMR(4)
         PRMS(5) = RROBRD(4)
         PRMS(6) = 0.3
         PRMS(7) = 0.15
         PRMS(8) = 0.01
         PRMS(9) = 0.0

         CALL OPNEW (KODE,1,MYACT(9),9,PRMS)
         IF (KODE .GT. 0) GOTO 917

         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND, 956) PRMS(1), PRMS(2)*INTOCM,
     &                          PRMS(3)/ACRtoHA, (PRMS(I),I=4,8)
         ELSE
            IF(LKECHO)WRITE(JOSTND,  916) (PRMS(I), I=1,8)
         ENDIF

  916    FORMAT(/'BBTYPE4    TYPE 4 BARK BEETLE IS ON EVERY YEAR',
     &      ' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,'  INCHES',
     &      /T12,'MINIMUM ELIGIBLE STEMS FOR EVENT= ',F7.0,' STEMS',
     &      '/ACRE ; INFECTED MORTALITY= ',F5.3,
     &      /T12,'MINIMUM ROOT DISEASE INFECTION= ',F5.2,
     &      ' UNINFECTED-WITHIN-PATCH MORTALITY= ',F5.2,
     &      /T12,'UNINFECTED-FRINGE MORTALITY=',F5.2,
     &      ' UNINFECTED-NON-PATCH MORTALITY= ',F5.2)
  956    FORMAT(/'BBTYPE4    TYPE 4 BARK BEETLE IS ON EVERY YEAR',
     &      ' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,'  CMS',
     &      /T12,'MINIMUM ELIGIBLE STEMS FOR EVENT= ',F7.0,' STEMS',
     &      '/HA ; INFECTED MORTALITY= ',F5.3,
     &      /T12,'MINIMUM ROOT DISEASE INFECTION= ',F5.2,
     &      ' UNINFECTED-WITHIN-PATCH MORTALITY= ',F5.2,
     &      /T12,'UNINFECTED-FRINGE MORTALITY=',F5.2,
     &      ' UNINFECTED-NON-PATCH MORTALITY= ',F5.2)

  917    CONTINUE
      ENDIF

C.... IF BBTYPE II SPECIFIED BUT NOT A WINDTHROW EVENT WRITE ERROR
C.... MESSAGE.

      IF (BB2GO .AND. .NOT. WINGO) THEN
         WRITE(JOSTND,920)
  920    FORMAT(/T12,'***** WARNING, TYPE II BARK BEETLE EVENT CALLED',
     &          ' FOR BUT WINDTHROW EVENT NOT CALLED'/,
     &      T12,'TYPE II BARK BEETLE OUTBREAK CANNOT HAPPEN *****')
      ENDIF

C.... IF THERE MAY BE TOO MANY BARK BEETLE EVENTS SCHEDULED THEN
C.... PRINT A WARNING MESSAGE THAT SOME BARK BEETLES MAY NOT BE DOME

      IF (IDOBB .GT. 3*MAXSP) THEN
          WRITE(JOSTND,919)
  919     FORMAT(/T12,'***** WARNING, TOO MANY BARK BEETLE EVENTS HAVE',
     &           ' BEEN SCHEDULED.'/T12,'IF ALL OF THEM ARE ACTIVE IN',
     &           ' THE SAME YEAR NOT ALL EVENTS WILL HAPPEN *****')
      ENDIF

C.... IF REINEKE SPECIFIED BUT BBTYPE 1 NOT PRESENT

      IF (REINGO .AND. .NOT. BB1GO) THEN
         WRITE(JOSTND,921)
  921    FORMAT(/T12,'***** WARNING, REINEKE METHOD CALLED',
     &          ' WITHOUT BBTYPE 1 EVENT:'/,
     &          T12,'REINEKE WILL HAVE NO EFFECT *****')
      ENDIF

      DO 923 IDI=MINRR,MAXRR

C....    IF PAREA HAS NOT BEEN ENTERED BY KEYWORD THEN SET THE DEFAULT
C....    FOR PAREA TO 25% OF SAREA.

         IF (.NOT. LPAREA(IDI)) PAREA(IDI) = 0.25 * SAREA

C....    If the LONECT flag has not been set and if there is only one
C....    center and the requested root disease area is equal to the
C....    stand area then run the stand as one center.
C....    This code is here to catch if the SAREA keyword comes before    
C....    the RRINIT keyword.

         IF (LONECT(IDI) .EQ. 0 .AND. NCENTS(IDI) .EQ. 1 .AND.
     &       PAREA(IDI) .EQ. SAREA) THEN
            LONECT(IDI) = 1
            IF(LKECHO)WRITE(JOSTND,*)
            IF(LKECHO)WRITE(JOSTND,547)
         ENDIF

  923 CONTINUE

C.... OPEN DISK FILE FOR UNIT IRUNIT.

      CALL MYOPEN (IRUNIT, '   ', 4, 133, 0, 1, 1, 0, KODE)

C.... PRINT MESSAGE IF ERROR OCCURED IN OPEN.

      IF (KODE .EQ. 1) THEN
         WRITE(JOSTND,*) ' ERROR WHILE OPENING UNIT IRUNIT !!!!!!!'
      ENDIF

C.... OPEN DISK FILE FOR UNIT IOUNIT.

      CALL MYOPEN (IOUNIT, '   ', 4, 133, 0, 1, 1, 0, KODE)

C.... PRINT MESSAGE IF ERROR OCCURED IN OPEN.

      IF (KODE .EQ. 1) THEN
         WRITE(JOSTND,*) ' ERROR WHILE OPENING UNIT IOUNIT !!!!!!!'
      ENDIF

C.... OPEN DISK FILE FOR RRECHO (UNIT IRGEN(2)).

C      CALL MYOPEN (IRGEN(2), 'RRECHO', 1, 133, 0, 1, 1, 0, KODE)

      GOTO 9000

   10 CONTINUE
C
C  ==========  OPTION NUMBER 10: SAREA ================================
C
C     SET THE STAND AREA IN ACRES
C        (IF ENTERED AS HA, CHANGE TO ACRES)
C     CHECK THAT A POSITIVE VALUE HAS BEEN ENTERED
C
      IF (ARRAY(1) .GT. 0.0) GOTO 1010

      IF (LMTRIC) WRITE(JOSTND,1011) SAREA * ACRtoHA
 1011 FORMAT (/T12,'***** ERROR IN STAND AREA INPUT ',F5.2,
     &        ' HECTARES ASSUMED')

      IF (.NOT. LMTRIC) WRITE(JOSTND,1001) SAREA
 1001 FORMAT (/T12,'***** ERROR IN STAND AREA INPUT ',F5.2,
     &        ' ACRES ASSUMED')
      GOTO 90

 1010 CONTINUE
      IF (.NOT. LMTRIC) SAREA = ARRAY(1)
      IF (LMTRIC) SAREA = ARRAY(1) * HAtoACR
      DIMEN = SQRT(SAREA) * 208.7

C.... OUTPUT FOR SAREA KEYWORD

      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,1012) KEYWRD, SAREA * ACRtoHA
 1012    FORMAT(/ A8, '   STAND AREA =', F8.2, ' HA')
      ELSE
         IF(LKECHO)WRITE(JOSTND,1002) KEYWRD, SAREA
 1002    FORMAT(/ A8, '   STAND AREA =', F8.2, ' ACRES')
      ENDIF
      GOTO 90

   11 CONTINUE
C
C  ==========  OPTION NUMBER 11: COMMENT ==============================
C
C     COMMENT KEYWORD.  THIS ENABLES THE USER TO WRITE OUT
C     A DESCRIPTION OF THE PARTICULAR ROOT DISEASE SCENARIO
C
      IF(LKECHO)WRITE(JOSTND,1101) KEYWRD
 1101 FORMAT (/A8)

C.... READ IN COMMENT STATEMENTS UNTIL 'END' IS ENCOUNTERED

 1102 CONTINUE
      READ (IREAD,1103,END=90) RECORD
 1103 FORMAT (A80)
      IRECNT = IRECNT + 1
      C4TMP = RECORD(1:4)
      CALL UPCASE (C4TMP(1:1))
      CALL UPCASE (C4TMP(2:2))
      CALL UPCASE (C4TMP(3:3))
      IF(C4TMP .EQ. 'END ') THEN
         IF(LKECHO)WRITE(JOSTND,1105) RECORD(1:4)
 1105    FORMAT (/A4)
         GOTO 90
      ELSE
 1104    IF(LKECHO)WRITE(JOSTND,1106) RECORD
 1106    FORMAT (T12,A80)
      ENDIF
      GOTO 1102

   12 CONTINUE
C
C  ==========  OPTION NUMBER 12: RRDOUT ===============================
C
C     DETAILED OUTPUT KEYWORD
C
      IRDOUT = 1

C.... OUTPUT FOR RRDOUT KEYWORD

      IF(LKECHO)WRITE(JOSTND,1201) KEYWRD
 1201 FORMAT (/A8,'   DETAILED ROOT DISEASE OUTPUT WILL BE WRITTEN')
      GOTO 90

  13  CONTINUE
C
C  ==========  OPTION NUMBER 13: INFKILL ==============================
C
C     KEYWORD FOR INFECTION AT DEATH
C     CHECK THAT ROOT DISEASE TYPE IS ACCEPTABLE
C
      IPOINT = 0
      IF (LNOTBK(1)) IPOINT = INT(ARRAY(1))

C.... IF ROOT DISEASE TYPE IS NOT VALID THEN SET VALUES FOR ALL
C.... ROOT DISEASE TYPES.

      IF (INT(ARRAY(1)) .GT. ITOTRR) THEN
         WRITE(JOSTND,1302) ITOTRR,ARRAY(1)
         WRITE(JOSTND,1303)
         IPOINT = 0
      ENDIF

 1302 FORMAT (/,'********   ERROR IN DISEASE TYPE, INFKILL KEYWORD.',
     &        ' VALUE MUST BE LESS THAN ',I2,' VALUE ON KEYWORD=',F4.0)
 1303 FORMAT ('********   CHANGE WILL AFFECT ALL ACTIVE DISEASE ',
     &        'TYPES')

C.... SET ROOT DISEASE TYPE TO BE MODIFIED.

      IRMIN = MINRR
      IRMAX = MAXRR
      IF (IPOINT .GT. 0) THEN
         IRMIN = IPOINT
         IRMAX = IPOINT
      ENDIF

C.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

      CALL SPDECD(2,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

C.... CHECK WHETHER ONLY ONE VALUE TO BE CHANGED, OR ALL

      IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....    ONLY ONE VALUE TO BE CHANGED

         DO 1350 IDI=IRMIN,IRMAX
            IF (LNOTBK(3)) PKILLS(IRTSPC(ISPC),IDI) = ARRAY(3)
 1350    CONTINUE

      ELSEIF (ISPC .EQ. 0) THEN

C....    ALL SPECIES GET CHANGED

         READ (IREAD,1321) (PKILLS(IRTSPC(KSP),IRMIN),KSP=1,MAXSP)

         DO 1357 IDI=IRMIN+1,IRMAX
            DO 1355 KSP=1,MAXSP
               PKILLS(IRTSPC(KSP),IDI) = PKILLS(IRTSPC(KSP),IRMIN)
 1355       CONTINUE
 1357    CONTINUE
 1321    FORMAT (11F5.2)
      ELSE

C....    ERROR IN READING IN SPECIES CODE

         WRITE(JOSTND,610) KEYWRD
         GOTO 90
      ENDIF

C.... OUTPUT FOR INFKILL KEYWORD

 1330 CONTINUE

      IF(LKECHO)WRITE(JOSTND,1331) KEYWRD
 1331 FORMAT (/A8,'   PROPORTION ROOT INFECTION AT DEATH')
      IF(LKECHO)WRITE(JOSTND,1332)
 1332 FORMAT (/T12,5X,'SPECIES',8X,'PROPORTION INFECTED')
      IF(LKECHO)WRITE(JOSTND,1333)
 1333 FORMAT (T12,41('-'))

      DO 1360 IDI=IRMIN,IRMAX
         IF(LKECHO)WRITE(JOSTND,1365) DISTYP(IDI)
         IF(LKECHO)WRITE(JOSTND,1366)

         DO 1334 KSP=1,MAXSP
            IF(LKECHO)WRITE(JOSTND,1335) RRJSP(IRTSPC(KSP)),
     &            PKILLS(IRTSPC(KSP),IDI)
 1334    CONTINUE
 1360 CONTINUE

 1365 FORMAT (T12,A16)
 1366 FORMAT (T12,16('-'))
 1335 FORMAT (T12,A16,13X,F5.2)

      GOTO 90

   14 CONTINUE
C
C  ==========  OPTION NUMBER 14: WINDTHR ===============================
C
C     WINDTHROW KEYWORD
C
      WINGO = .TRUE.
      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      PRMS(1) = ROWIND
      PRMS(2) = ROWMIN

      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
      CALL OPNEW (KODE,IDT,MYACT(3),2,PRMS)
      IF (KODE .GT. 0) GOTO 90

C.... OUTPUT FOR WINDTHR KEYWORD

      IF(LKECHO)WRITE(JOSTND,1401) KEYWRD, IDT,PRMS(1),PRMS(2)
 1401 FORMAT(/A8,'   WINDTHROW EVENT TO BE ATTEMPTED IN DATE/CYCLE=',
     &       I4, ';   PROPORTION TO WINDTHROW =', F4.2,
     &       /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.0)
      GOTO 90

   15 CONTINUE
C
C  ==========  OPTION NUMBER 15: BBTYPE1 ===============================
C
C     BARK BEETLE TYPE # 1
C
      BB1GO = .TRUE.
      IDOBB = IDOBB + 1

      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

C.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MIN ELIGIBLE TPA,
C.... MORTALITY RATE, AND NUMBER OF OUTBREAKS.

      PRMS(1) = RROBTS(1)
      PRMS(2) = RROBSC(1)
      PRMS(3) = RROBOL(1)
      PRMS(4) = RROBMR(1)
      PRMS(5) = 0.0

      IF (LNOTBK(2)) THEN

C....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

         CALL SPDECD(2,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

         IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....       VALID SPECIES CODE.

            PRMS(1) = ARRAY(2)

         ELSE

C....       INVALID SPECIES CODE.

            WRITE(JOSTND,1510) KARD(2), ISPC
 1510       FORMAT ('********   SPECIES = ', A3, ' (',I2,') ',
     &              'NOT VALID. DEFAULT WILL BE USED.')

            ISPC = INT(RROBTS(1))
            KARD(2)(1:2) = NSP(ISPC,1)(1:2)
            KARD(2)(3:3) = ' '
         ENDIF
      ELSE
         ISPC = INT(RROBTS(1))
         KARD(2)(1:2) = NSP(ISPC,1)(1:2)
         KARD(2)(3:3) = ' '
      ENDIF

      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)
      IF (LNOTBK(6)) PRMS(5) = ARRAY(6)

      IF (LMTRIC) THEN
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4) / HAtoACR
      ELSE
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      ENDIF

      CALL OPNEW (KODE,IDT,MYACT(4),5,PRMS)
      IF (KODE .GT. 0) GOTO 90

C.... DIAGNOSTIC OUTPUT FOR BBTYPE1 KEYWORD: STEMS/ACRE METHOD

      IF (.NOT. REINGO) THEN
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND, 1530) KEYWRD, IDT, KARD(2), ISPC,
     &             PRMS(2) * INTOCM, PRMS(3) / ACRtoHA, PRMS(4)
         ELSE
            IF(LKECHO)WRITE(JOSTND, 1520) KEYWRD, IDT, KARD(2), ISPC,
     &             (PRMS(I), I=2,4)
         ENDIF

 1520    FORMAT (/A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &           '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &           'MINIMUM DBH=', F5.2, ' INCHES',
     &           /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,
     &           ' STEMS/ACRE;   MORTALITY RATE = ', F5.3)

 1530    FORMAT (/A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &           '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &           'MINIMUM DBH=', F5.2, ' CMS',
     &           /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,
     &           ' STEMS/HA;   MORTALITY RATE = ', F5.3)

      ENDIF

C.... DIAGNOSTIC OUTPUT FOR BBTYPE1 KEYWORD: REINEKE METHOD

      IF (REINGO) THEN
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,1550) KEYWRD, IDT, KARD(2), ISPC,
     &           PRMS(2) * INTOCM, PRMS(3) / ACRtoHA, PRMS(4)
         ELSE
            IF(LKECHO)WRITE(JOSTND,1540) KEYWRD, IDT, KARD(2), ISPC,
     &           (PRMS(I), I=2,4)
         ENDIF

 1540    FORMAT (/A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &           '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &           'MINIMUM DBH=', F5.2, ' INCHES',
     &           /T12, 'MINIMUM REINEKE SDI=', F5.3,
     &           '  MORTALITY RATE = ', F5.3)

 1550    FORMAT (/A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &           '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &           'MINIMUM DBH=', F5.2, ' CMS',
     &           /T12, 'MINIMUM REINEKE SDI=', F5.3,
     &           '  MORTALITY RATE = ', F5.3)

      ENDIF

      IF (PRMS(5) .EQ. 1.0) THEN
         IF(LKECHO)WRITE(JOSTND,1560)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1570)
      ENDIF

 1560 FORMAT (8X, '   ONLY ONE OUTBREAK WILL BE SCHEDULED FOR THIS ',
     &        'BARK BEETLE.')

 1570 FORMAT (8X, '   MULTIPLE OUTBREAKS WILL BE SCHEDULED FOR THIS ',
     &        'BARK BEETLE.')

      GOTO 90

   16 CONTINUE
C
C  ==========  OPTION NUMBER 16: BBTYPE2 =============================
C
C     TYPE 2 BARK BEETLES
C
      BB2GO = .TRUE.
      IDOBB = IDOBB + 1

      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

C.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MIN TPA WINDTHROWN,
C.... MORTALITY RATE, AND NUMBER OF OUTBREAKS.

      PRMS(1) = RROBTS(2)
      PRMS(2) = RROBSC(2)
      PRMS(3) = RROBOL(2)
      PRMS(4) = RROBMR(2)
      PRMS(5) = 0.0

      IF (LNOTBK(2)) THEN

C....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

         CALL SPDECD(2,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

         IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....       VALID SPECIES CODE.

            PRMS(1) = ARRAY(2)

         ELSE

C....       INVALID SPECIES CODE.

            WRITE(JOSTND, 1510) KARD(2), ISPC
            ISPC = INT(RROBTS(2))
            KARD(2)(1:2) = NSP(ISPC,1)(1:2)
            KARD(2)(3:3) = ' '
         ENDIF
      ELSE
         ISPC = INT(RROBTS(2))
         KARD(2)(1:2) = NSP(ISPC,1)(1:2)
         KARD(2)(3:3) = ' '
      ENDIF

      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)
      IF (LNOTBK(6)) PRMS(5) = ARRAY(6)

      IF (LMTRIC) THEN
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4) / HAtoACR
      ELSE
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      ENDIF 
      
      CALL OPNEW (KODE,IDT,MYACT(5),5,PRMS)
      IF (KODE .GT. 0) GOTO 90

C.... OUTPUT FOR BBTYPE2 KEYWORD

      IF (LMTRIC) THEN

         IF(LKECHO)WRITE(JOSTND,1611) KEYWRD, IDT, KARD(2), ISPC,
     &                       PRMS(2) * INTOCM,
     &                       PRMS(3) / ACRtoHA, PRMS(4)
 1611    FORMAT (/A8,'   TYPE 2 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &      '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &      'MINIMUM DBH=', F5.2, '  CMS',
     &      /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,
     &      ' STEMS/HA ;   MORTALITY RATE = ', F4.2)

      ELSE
         IF(LKECHO)WRITE(JOSTND, 1601) KEYWRD, IDT, KARD(2), ISPC,
     &                        (PRMS(I), I=2,4)
 1601    FORMAT (/A8,'   TYPE 2 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &      '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &      'MINIMUM DBH=', F5.2, '  INCHES',
     &      /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,
     &      ' STEMS/ACRE ;   MORTALITY RATE = ', F4.2)

      ENDIF

      IF (PRMS(5) .EQ. 1.0) THEN
         IF(LKECHO)WRITE(JOSTND,1560)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1570)
      ENDIF

      GOTO 90

   17 CONTINUE
C
C  ==========  OPTION NUMBER 17: BBTYPE3 ==============================
C
C     TYPE 3 BARK BEETLES
C
C     Changed order of paramter fields: 10/94
C

      IDOBB = IDOBB + 1

      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

C.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MINIMUM ELIGIBLE TPA,
C.... MORTALITY RATE, MINIMUM PROPORTION ROOTS INFECTED, AND
C.... NUMBER OF OUTBREAKS.

      PRMS(1) = RROBTS(3)
      PRMS(2) = RROBSC(3)
      PRMS(3) = RROBOL(3)
      PRMS(4) = RROBMR(3)
      PRMS(5) = RROBRD(3)
      PRMS(6) = 0.0

      IF (LNOTBK(2)) THEN

C....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

         CALL SPDECD(2,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

         IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....       VALID SPECIES CODE.

            PRMS(1) = ARRAY(2)

         ELSE

C....       INVALID SPECIES CODE.

            WRITE(JOSTND,1510) KARD(2), ISPC
            ISPC = INT(RROBTS(3))
            KARD(2)(1:2) = NSP(ISPC,1)(1:2)
            KARD(2)(3:3) = ' '
         ENDIF
      ELSE
         ISPC = INT(RROBTS(3))
         KARD(2)(1:2) = NSP(ISPC,1)(1:2)
         KARD(2)(3:3) = ' '
      ENDIF

      IF (LMTRIC) THEN
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4) / HAtoACR
      ELSE
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      ENDIF   

      IF (LNOTBK(5)) PRMS(5) = ARRAY(5)
      IF (LNOTBK(6)) PRMS(6) = ARRAY(6)
      IF (LNOTBK(7)) PRMS(4) = ARRAY(7)

      CALL OPNEW (KODE,IDT,MYACT(6),6,PRMS)
      IF (KODE .GT. 0) GOTO 90

C.... OUTPUT FOR BBTYPE3 KEYWORD

      IF (LMTRIC) THEN

         IF(LKECHO)WRITE(JOSTND,1711) KEYWRD, IDT, KARD(2), ISPC,
     &      PRMS(2)*INTOCM, PRMS(3)/ACRtoHA, PRMS(4), PRMS(5)
 1711    FORMAT(/A8,'   TYPE 3 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &      '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &      'MINIMUM DBH=', F5.2, '  CMS',
     &      /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,
     &      ' STEMS/HA   ;  MORTALITY RATE = ', F4.2,
     &      /T12, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)

      ELSE
         IF(LKECHO)WRITE(JOSTND, 1701) KEYWRD, IDT, KARD(2), ISPC,
     &                        (PRMS(I), I=2,5)
 1701    FORMAT(/A8,'   TYPE 3 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &      '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',
     &      'MINIMUM DBH=', F5.2, '  INCHES',
     &      /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,
     &      ' STEMS/ACRE   ;  MORTALITY RATE = ', F4.2,
     &      /T12, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)

      ENDIF

      IF (PRMS(6) .EQ. 1.0) THEN
         IF(LKECHO)WRITE(JOSTND,1560)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1570)
      ENDIF

      GO TO 90

   18 CONTINUE
C
C  ==========  OPTION NUMBER 18: INOCSPAN =============================
C
C     Minimum life span of inoculum

C.... For each disease type, if a value was entered, set the XMINLF
C.... array to the user defined value and then print out the value.

      DO 1820 IDI = 1, ITOTRR
         IF (LNOTBK(IDI)) THEN
            XMINLF(IDI) = ARRAY(IDI)
            IF(LKECHO)WRITE(JOSTND,1830)
     &         KEYWRD,DISTYP(IDI),XMINLF(IDI)
         ENDIF
 1820 CONTINUE

 1830 FORMAT (/A8,'   MINIMUM LIFE SPAN FOR ',A12,' INOCULUM IS',
     &           F4.0, ' YEARS')

      GOTO 90

   19 CONTINUE
C
C  ==========  OPTION NUMBER 19: RRCOMP  ==============================
C
      IF (ARRAY(1) .GT. 400.0 .OR. ARRAY(1) .LE. 0.0) GOTO 1910
      IRCOMP = INT(ARRAY(1))
      GOTO 1920

 1910 CONTINUE
      WRITE(JOSTND,1911) ARRAY(1)
 1911 FORMAT (/A8,'   *** ERROR, COMPRESSION TARGET READ AS ',F5.0,
     &        '.', /, T12, 'VALUE IS OUT OF BOUNDS. SET TO 400.')
      IRCOMP = 400

      GOTO 1930

C.... OUTPUT FOR RRCOMP KEYWORD

 1920 CONTINUE
      IF(LKECHO)WRITE(JOSTND,1921) KEYWRD, IRCOMP
 1921 FORMAT (/A8,'   COMPRESSION TARGET=',I5,' RECORDS')

 1930 CONTINUE
      GOTO 90

   20 CONTINUE
C
C  ==========  OPTION NUMBER 20: RRECHO ===============================
C
C     MACHINE READABLE SUMMARY OUTPUT KEYWORD.
C     IRGEN(7) IS SET TO 1 IF RRECHO IS TO BE PRINTED.
C
      IF (ARRAY(1) .GT. 0.0) IRGEN(2) = INT(ARRAY(1))
      IRGEN(7) = 1

CM    IF (ARRAY(2) .GT. 0.0) IRGEN(7) = INT(ARRAY(2))

C.... OUTPUT FOR RRECHO KEYWORD

      IF(LKECHO)WRITE(JOSTND,2001) KEYWRD, IRGEN(2)
 2001 FORMAT (/A8,'   MACHINE READABLE OUTPUT ON LOGICAL UNIT ',I4)

CM    IF (IRGEN(7) .NE. 0) IF(LKECHO)WRITE(JOSTND,2002)
CM  2002 FORMAT (T12,'OUTPUT WILL BE IN LOTUS READABLE FORMAT')

      GOTO 90

   21 CONTINUE
C
C  ==========  OPTION NUMBER 21: STREAD ===============================
C
C     Modified 10/94 to use multiple keywords instead of supplemental
C     records.
C
      IRGEN(3) = 1

C.... TEST TO MAKE SURE NONE OF THE FIRST FOUR FIELDS ARE BLANK.

      DO 2110 I=1,4
         IF (.NOT.LNOTBK(I)) THEN
            WRITE(JOSTND,2101)
 2101       FORMAT(/,'**** BLANK FIELD IN STREAD.  KEYWORD IGNORED.')
            GOTO 90
         ENDIF
 2110 CONTINUE

C.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

      CALL SPDECD(1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

      IF (ISPC .LT. 1 .OR. ISPC .GT. MAXSP) THEN

C....    INVALID SPECIES CODE.

         WRITE(JOSTND,2102) KARD(1), ISPC
 2102    FORMAT (/,'**** STREAD: INVALID SPECIES. ',
     &           'SPECIES = ', A3, ' (', I2, ').  KEYWORD IGNORED.')
         GOTO 90
      ENDIF

      ISL = INT(ARRAY(2))
      IF (ISL .LT. 1 .OR. ISL .GT. 5) THEN
         WRITE(JOSTND,2103) ISL
 2103    FORMAT (/,'**** STREAD: STUMP CLASS=',I3, ' OUT OF RANGE.',
     &           ' KEYWORD IGNORED.')
         GOTO 90
      ENDIF

      DEN = ARRAY(3)
      IF (DEN .LT. 0.0) THEN
         WRITE(JOSTND,2104) DEN
 2104    FORMAT (/,'**** STREAD: NEGATIVE NUMBER OF STUMPS=',F6.1,
     &           ' KEYWORD IGNORED.')
         GOTO 90
      ENDIF

      IRRSP = INT(ARRAY(4))
      IF (IRRSP .LT. 1 .OR. IRRSP .GT. ITOTRR) THEN
         WRITE(JOSTND,2105) IRRSP
 2105    FORMAT (/,'**** STREAD: DISEASE TYPE=',I2, ' OUT OF RANGE.',
     &           ' KEYWORD IGNORED.')
         GOTO 90
      ENDIF

      IF (LNOTBK(5)) THEN
         JAGE = INT(ARRAY(5))
      ELSE
         JAGE = 0
      ENDIF

      IF (JAGE .LT. 0) THEN
         WRITE(JOSTND,2106) JAGE
 2106    FORMAT (/,'**** STREAD: NEGATIVE YEARS SINCE DEATH=',I4,
     &           ' KEYWORD IGNORED.')
         GOTO 90
      ENDIF

      ISTFLG = 1

C.... CONVERT STUMP CLASS TO DIAMETER (INCHES)

      DIAM = STCUT(ISL) + 6.0

C.... ESTIMATE ROOT RADIUS

      CALL RDROOT(ISPC,DIAM,ANS,PROOT(IRTSPC(ISPC)),RSLOP(IRTSPC(ISPC)),
     &            0.0)
      RTD = ANS

C.... ADD TO STUMP ARRAY

      CALL RDSTP (ISL,ISPC,DEN,DIAM,RTD)

C     IF(LKECHO)WRITE(JOSTND,2159) 'AFTER', ISL, ISPC, DEN, DIAM, RTD, IRRSP
C2159 FORMAT (A10,' ISL=',I4, ' ISPC=',I4, '   DEN=',F8.2,
C    &        '   DIAM=',F8.2, '   RTD=',F8.2,
C    &        '   IRRSP=',I4)

C.... RESET AGE TO USER INPUT, IF DIFFERENT FROM THAT SET IN RDSTP

      IF (JAGE .NE. IYEAR) THEN
         JRAGED(IRRSP,ISPS(IRTSPC(ISPC)),ISL,1) = -JAGE
      ENDIF
      ISTFLG = 0

C.... WRITE OUTPUT FOR STREAD KEYWORD.

      IF(LKECHO)WRITE(JOSTND,2131) KEYWRD, KARD(1), ISPC, ISL, DEN,
     &                    DISTYP(IRRSP), JAGE
 2131 FORMAT (/A8, '   STUMPS ENTERED WITH FOLLOWING ATTRIBUTES:',
     &        /, T12, 'SPECIES = ', A3, '(', I2, ')   STUMP CLASS = ',
     &        I2, '   NUMBER OF STUMPS = ', F6.1, '   DISEASE TYPE = ',
     &        A12, '   YEARS SINCE DEATH = ', I3)

      GOTO 90

   22 CONTINUE
C
C  ==========  OPTION NUMBER 22: TDISTN ===============================
C
C     TREE DISTRIBUTION FOR SPREAD MODEL
C
      IF (ARRAY(1) .GT. 0.0) IRGEN(4) = INT(ARRAY(1))
      IF (ARRAY(2) .EQ. 0.0 .OR. ARRAY(2) .EQ. 1.0)
     &    IRSTYP = INT(ARRAY(2))
      IF (LNOTBK(3)) RRSFRN = ARRAY(3)

C.... OUPUT FOR TDISTN KEYWORD

      IF ((IRSTYP.EQ.0).AND.LKECHO)WRITE(JOSTND,2200) KEYWRD
      IF ((IRSTYP .EQ. 1).AND.LKECHO)WRITE(JOSTND,2201) KEYWRD
 2200 FORMAT (/A8,'   TREE DISTRIBUTION IS RANDOM')
 2201 FORMAT (/A8,'   TREE DISTRIBUTION IS LATTICE')

      IF (IRGEN(4) .EQ. 3000) GOTO 2230

      IF(LKECHO)WRITE(JOSTND,2202) IRGEN(4)
 2202 FORMAT (T12,'SPATIAL DISTRIBUTION WILL CHANGE',
     &      ' TO OTHER FORM IN ',I4)

 2230 CONTINUE
      IRG = IRGEN(4)
      IF (((IRSTYP .EQ. 1).OR.(IRG .NE. 3000))
     &   .AND.LKECHO)WRITE(JOSTND,2231) RRSFRN
 2231 FORMAT (T12,'STANDARD DEVIATION OF LATTICE DISTRIBUTION IS ',F7.3)

      GOTO 90

   23 CONTINUE
C
C  ==========  OPTION NUMBER 23: RRJUMP ==============================
C
C     SPECIFIES THE EXTENT TO WHICH ROOT DISEASE CENTERS WILL EXPAND
C     AFTER A CUT.
C

C.... WRITE OUT KEYWORD INFO.

      IF(LKECHO)WRITE(JOSTND,2301) KEYWRD
 2301 FORMAT (/A8,'   NUMBER OF RADII THE AREA IN SPECIFIED ROOT ',
     &      'DISEASE WILL JUMP OUT')

C.... FOR EACH DISEASE TYPE, IF A VALUE WAS ENTERED, SET THE TNJUMP
C.... ARRAY TO THE USER DEFINED VALUE AND THEN PRINT OUT THE VALUE.

      DO 2305 IDI=1,ITOTRR
         IF (LNOTBK(IDI)) THEN

C....       A VALUE WAS ENTERED FOR THIS DISEASE TYPE (IDI), SET
C....       TNJUMP ARRAY.

            TNJUMP(IDI) = ARRAY(IDI)

C....       OUTPUT VALUE FOR DISEASE TYPE (IDI)

            IF(LKECHO)WRITE(JOSTND,2312) DISTYP(IDI), TNJUMP(IDI)
         ENDIF
 2305 CONTINUE

 2312 FORMAT (T12,1X,A12,5X,F5.2)

      GOTO 90

   24 CONTINUE
C
C  ==========  OPTION NUMBER 24: RRMINK ================================
C
C     MINIMUM YEARS TO KILL TREES
C
      DO 2420 IDI=1,ITOTRR
         IF (LNOTBK(IDI)) THEN
            XMINKL(IDI) = ARRAY(IDI)

C....       OUTPUT FOR RRMINK KEYWORD

            IF(LKECHO)WRITE(JOSTND,2401)
     &         KEYWRD,XMINKL(IDI),DISTYP(IDI)
         ENDIF
 2420 CONTINUE

 2401 FORMAT (/A8,'   NO HABITAT INFLUENCE BELOW ',F4.0,
     &        ' YEARS, ',A12 )

      GOTO 90

   25 CONTINUE
C
C  ==========  OPTION NUMBER 25: RSEED ================================
C
C     KEYWORD FOR REINITIALIZING ROOT DISEASE RANDOM NUMBER GENERATOR
C
      IF (LNOTBK(1).AND.ARRAY(1).EQ.0.0) CALL GETSED (ARRAY(1))
      IF (LNOTBK(1)) DSEED = ARRAY(1)

      IF(LKECHO)WRITE(JOSTND,2501) KEYWRD, DSEED
 2501 FORMAT (/A8,'   SEED TO REINITIALIZE RANDOM NUMBER GENERATOR=',
     &   F10.2)

      GOTO 90

   26 CONTINUE
C
C  ==========  OPTION NUMBER 26: PLREAD ===============================
C
C     KEYWORD TO READ IN SUB-PLOT ID'S
C
      IDI = MINRR
      IF (LNOTBK(1)) IDI = INT(ARRAY(1))

      K = 0
      LONECT(IDI) = 2

 2601 READ (IREAD,2602) IDSPLT
 2602 FORMAT (I4)

      IF (IDSPLT .EQ. -999) GOTO 2610
      K = K + 1
      IRDPLT(IDI,K) = IDSPLT
      GOTO 2601

 2610 CONTINUE

C.... WRITE KEYWORD, DISEASE SUB-PLOTS

      IF (K .LE. 25) M = K
      IF (K .GT. 25) M = 25

      IF(LKECHO)WRITE(JOSTND,2611) KEYWRD, DISTYP(IDI),
     &      (IRDPLT(IDI,J),J=1,M)
 2611 FORMAT (/A8,'   ROOT DISEASE IS ', A12 /
     &        T12, 'DISEASE SUBPLOTS READ IN'/
     &        T12, 'DISEASED PLOTS=', 25(I4))

      IF (K .LE. 25) GOTO 90

      IF(LKECHO)WRITE(JOSTND,2612)
 2612 FORMAT (/)
      NEXT = M + 1

      IF(LKECHO)WRITE(JOSTND,2613) (IRDPLT(IDI,J),J=NEXT,K)
 2613 FORMAT (T26,25I4)
      GOTO 90

   27 CONTINUE
C
C  ==========  OPTION NUMBER 27: --RDv3-- =============================
C
C     THIS IS NOT AN ACTUAL KEYWORD. IT IS JUST A PLACEHOLDER OCCUPYING
C     THE SPACE OF A KEYWORD THAT WAS REMOVED.

C.... OUTPUT 

      IF(LKECHO)WRITE(JOSTND,2710) KEYWRD
 2710 FORMAT (/A8,'   THIS RD KEYWORD ARRAY POSITION IS NOT USED.')

      GOTO 90

   28 CONTINUE
C
C  ==========  OPTION NUMBER 28: RRTREIN  =============================
C
C     THIS KEYWORD USED WHEN ROOT DISEASE INITIALIZATION IS TAKEN
C     DIRECTLY FROM THE INPUT TREELIST.
C
      RRTINV = .TRUE.

C.... OUTPUT FOR RRTREIN KEYWORD

      IF(LKECHO)WRITE(JOSTND,2856) KEYWRD
 2856 FORMAT (/A8,'   ROOT DISEASE INITIALIZATION IS FROM TREELIST')

      GOTO 90

   29 CONTINUE
C
C  ==========  OPTION NUMBER 29: INFCOLO  =============================
C
C     USED TO CHANGE THE PROPORTION OF ROOT SYSTEMS COLONIZED AFTER
C     DEATH.
C
      IPOINT = 0
      IF (LNOTBK(3)) IPOINT = INT(ARRAY(3))

      IF (INT(ARRAY(3)) .GT. ITOTRR) THEN
         WRITE(JOSTND,2960) KEYWRD, ARRAY(3),ITOTRR
         IPOINT = 0
      ENDIF

 2960 FORMAT (/T12,A8,'**** INVALID DISEASE TYPE. VALUE ENTERED: ',
     &        F3.0,' VALUE MUST BE LESS THAN ',I2,
     &        /T12,8X, 'CHANGE WILL AFFECT ALL DISEASE TYPES')

C.... SET ROOT DISEASE TYPE TO BE MODIFIED.

      IRMIN = MINRR
      IRMAX = MAXRR
      IF (IPOINT .GT. 0) THEN
         IRMIN = IPOINT
         IRMAX = IPOINT
      ENDIF

C.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

      CALL SPDECD(1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)

      IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....    ONLY ONE SPECIES TO BE CHANGED.

         DO 2965 IDI=IRMIN,IRMAX
            IF (LNOTBK(2)) PCOLO(IRTSPC(ISPC),IDI) = ARRAY(2)
 2965    CONTINUE

      ELSE

C....    ALL SPECIES GET CHANGED.

         READ (IREAD,2921) (PCOLO(IRTSPC(KSP),IRMIN), KSP=1,MAXSP)
 2921    FORMAT (11F5.2)

         DO 2967 IDI=IRMIN+1,IRMAX
            DO 2966 KSP=1,MAXSP
               PCOLO(IRTSPC(KSP),IDI) = PCOLO(IRTSPC(KSP),IRMIN)
 2966       CONTINUE
 2967    CONTINUE
      ENDIF

C.... OUTPUT FOR INFCOLO KEYWORD

 2930 CONTINUE
      IF(LKECHO)WRITE(JOSTND,2931) KEYWRD
 2931 FORMAT (/A8,'   PROPORTION ROOT COLONIZED AT DEATH')
      IF(LKECHO)WRITE(JOSTND,2932)
 2932 FORMAT (/T12,5X,'SPECIES',8X,'PROPORTION COLONIZED')
      IF(LKECHO)WRITE(JOSTND,2933)
 2933 FORMAT (T12,41('-'))

      DO 2970 IDI=IRMIN,IRMAX
         IF(LKECHO)WRITE(JOSTND,2971) DISTYP(IDI)
         IF(LKECHO)WRITE(JOSTND,2972)
         DO 2934 KSP=1,MAXSP
            IF(LKECHO)WRITE(JOSTND,2935) RRJSP(IRTSPC(KSP)),
     &            PCOLO(IRTSPC(KSP),IDI)
 2934    CONTINUE
 2970 CONTINUE

 2935 FORMAT (T12,A16,13X,F5.2)
 2971 FORMAT (T12,A16)
 2972 FORMAT (T12,16('-'))

      GOTO 90

   30 CONTINUE
C
C  ==========  OPTION NUMBER 30: BORATE  =============================
C
      IF (MINRR .GT. 2 .AND. MAXRR .GT. 2) THEN
         WRITE(JOSTND,3002) 
 3002    FORMAT(/,'**** BORATE: INVALID DISEASE TYPE ACTIVE. ',
     &         'BORATE APPLIES ONLY TO ANNOSUS.',
     &         /T12,'KEYWORD WILL BE IGNORED.')
         GOTO 90
      ENDIF

      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

      PRMS(1) = BOTRT
      PRMS(2) = BODBH

      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)

      IF (LMTRIC .AND. LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN

      CALL OPNEW (KODE,IDT,MYACT(7),2,PRMS)
      IF (KODE .GT. 0) GOTO 90

      LBORAX(1) = .TRUE.

      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,3010) KEYWRD,IDT,PRMS(1),PRMS(2)*INTOCM
      ELSE
         IF(LKECHO)WRITE(JOSTND,3010) KEYWRD,IDT, (PRMS(I), I=1,2)
      ENDIF

 3010 FORMAT (/A8,'   BORATE WILL BE APPLIED IN DATE/CYCLE=',I5,
     &       '     PROP TREAT:', F4.2, '   MIN DBH:', F6.2)

C      IF (LNOTBK(1) .AND. ARRAY(1) .GE. 0.0 .AND. ARRAY(1) .LE. 1.0)
C     &   BOTRT = ARRAY(1)
C      IF (LNOTBK(2)) BODBH = ARRAY(2)

C      IF(LKECHO)WRITE(JOSTND,3000) BOTRT, BODBH
C 3000 FORMAT (/'BORAX      PROP TREAT:', F4.2, '   MIN DBH:', F6.2)

      GOTO 90

   31 CONTINUE
C
C  ==========  OPTION NUMBER 31: SPORE    =============================
C
C     FIELD 5 CONTAINS THE DISEASE TYPE.  1 = P TYPE.  2 = S TYPE.
C
      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

      IDI = INT(ARRAY(6))
      IF (IDI .NE. 2) IDI = 1

      PRMS(1) = SPINF(IDI)
      PRMS(2) = SPDBH(IDI)
      PRMS(3) = SPYTK(IDI)
      PRMS(4) = SPTRAN(IDI)
      PRMS(5) = IDI

      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
      IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)

      IF (LMTRIC .AND. LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN

      CALL OPNEW (KODE,IDT,MYACT(8),5,PRMS)
      IF (KODE .GT. 0) GOTO 90

      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,3100) KEYWRD,IDT,PRMS(1),PRMS(2)*INTOCM,
     &                       PRMS(3),PRMS(4),PRMS(5)
      ELSE
         IF(LKECHO)WRITE(JOSTND,3100) KEYWRD,IDT,(PRMS(I), I=1,5)
      ENDIF

 3100 FORMAT (/A8,'   IN DATE/CYCLE : ',I4,' VALUES WILL BE ',
     &        'CHANGED TO :',
     &        /T12, 'PROPORTION OF STUMPS TO INFECT :',F4.2,
     &        /T12, 'MINIMUM STUMP DIAMETER TO FORM NEW CENTERS :',F6.2,
     &        /T12, 'MULTIPLIER FOR TIME-TO-DEATH :',F7.2,
     &        /T12, 'MULTIPLIER FOR PROBABILITY OF INFECTION :', F6.2,
     &        /T12, 'DISEASE TYPE :',F3.0)

      GOTO 90

   32 CONTINUE
C
C  ==========  OPTION NUMBER 32: BBTYPE4 ==============================
C
C     TYPE 4 BARK BEETLES
C
C     Changed order of parameter fields: 10/94
C
      IDOBB = IDOBB + 1

      IDT = 1
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))

C.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MINIMUM ELIGIBLE TPA,
C.... MORTALITY RATE, AND MINIMUM PROPORTION ROOTS INFECTED.

      PRMS(1) = RROBTS(4)
      PRMS(2) = RROBSC(4)
      PRMS(3) = RROBOL(4)
      PRMS(4) = RROBMR(4)
      PRMS(5) = RROBRD(4)

C.... SET THE DEFAULTS FOR MORTALITY RATES AND NUMBER OF OUTBREAKS.

      PRMS(6) = 0.3
      PRMS(7) = 0.15
      PRMS(8) = 0.01
      PRMS(9) = 0.0

      IF (LNOTBK(2)) THEN

C....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

         CALL SPDECD(2,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &               ARRAY,KARD)

         IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....       VALID SPECIES CODE.

            PRMS(1) = ARRAY(2)

         ELSE

C....       INVALID SPECIES CODE.

            WRITE(JOSTND,1510) KARD(2), ISPC
            ISPC = INT(RROBTS(4))
            KARD(2)(1:2) = NSP(ISPC,1)(1:2)
            KARD(2)(3:3) = ' '
         ENDIF
      ELSE
         ISPC = INT(RROBTS(4))
         KARD(2)(1:2) = NSP(ISPC,1)(1:2)
         KARD(2)(3:3) = ' '
      ENDIF

      IF (LMTRIC) THEN
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4) / HAtoACR
      ELSE
         IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
         IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      ENDIF
      
      IF (LNOTBK(5)) PRMS(5) = ARRAY(5)
      IF (LNOTBK(6)) PRMS(9) = ARRAY(6)
      IF (LNOTBK(7)) PRMS(4) = ARRAY(7)

 3201 READ (IREAD,3202) T1, T2, T3
 3202 FORMAT (A10,A10,A10)

      IF (T1.NE.'          ') READ (T1,'(G10.0)') PRMS(6)
      IF (T2.NE.'          ') READ (T2,'(G10.0)') PRMS(7)
      IF (T3.NE.'          ') READ (T3,'(G10.0)') PRMS(8)

      CALL OPNEW (KODE,IDT,MYACT(9),9,PRMS)
      IF (KODE .GT. 0) GOTO 90

C.... OUTPUT FOR BBTYPE4 KEYWORD

      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,3213) KEYWRD, IDT, KARD(2), ISPC,
     &      PRMS(2)*INTOCM,PRMS(3)/ACRtoHA, PRMS(5), PRMS(4),
     &      (PRMS(I),I=6,8)
      ELSE
         IF(LKECHO)WRITE(JOSTND,3203) KEYWRD,IDT,KARD(2),ISPC,
     &      PRMS(2),PRMS(3), PRMS(5), PRMS(4), (PRMS(I),I=6,8)
      ENDIF

 3203 FORMAT(/ A8, '   TYPE 4 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &       '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, ');  ',
     &       'MINIMUM DBH=', F5.2, '  INCHES',
     &       /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,
     &       ' STEMS/ACRE ; MINIMUM ROOT DISEASE INFECTION= ', F5.2,
     &       /T12, 'INFECTED MORTALITY= ', F5.3,
     &       ' UNINFECTED-WITHIN-PATCH MORTALITY= ', F5.2,
     &       /T12, 'UNINFECTED-FRINGE MORTALITY=', F5.2,
     &       ' UNINFECTED-NON-PATCH MORTALITY= ', F5.2)

 3213 FORMAT(/ A8, '   TYPE 4 BARK BEETLE TO BE ATTEMPTED IN DATE',
     &       '/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, ');  ',
     &       'MINIMUM DBH=', F5.2, '  CMS',
     &       /T12, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,
     &       ' STEMS/HA ; MINIMUM ROOT DISEASE INFECTION= ', F5.2,
     &       /T12, 'INFECTED MORTALITY= ', F5.3,
     &       ' UNINFECTED-WITHIN-PATCH MORTALITY= ', F5.2,
     &       /T12, 'UNINFECTED-FRINGE MORTALITY=', F5.2,
     &       ' UNINFECTED-NON-PATCH MORTALITY= ', F5.2)

      IF (PRMS(9) .EQ. 1.0) THEN
         IF(LKECHO)WRITE(JOSTND,1560)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1570)
      ENDIF

      GO TO 90

   33 CONTINUE
C
C  ==========  OPTION NUMBER 33: DNSCALC ==============================
C
C     PROCESS THE ARGUMENTS TO IMPLEMENT THE REINEKE STAND DENSITY
C     INDEX METHOD FOR EVALUATING THE THRESHOLD FOR A BARK BEETLE
C     TYPE 1 OUTBREAK.
C     ASSIGN REINEK(4) IF THE ARGUMENT IS { -2.5 <= arg <= -0.5 }
C
      REINGO = .TRUE.

      DO 3300 I=1,3
         IF (LNOTBK(I)) REINEK(I) = ARRAY(I)
         IF (REINEK(I) .NE. 0) REINEK(I) = 1
 3300 CONTINUE

      IF (LMTRIC) ARRAY(4) = (ARRAY(4) / HAtoACR) / CMTOIN
      IF (LNOTBK(4) .AND. ARRAY(4) .LE. -0.5 .AND. ARRAY(4) .GE. -2.5)
     &    REINEK(4) = ARRAY(4)

C.... OUTPUT FOR DNSCALC KEYWORD

      IF(LKECHO)WRITE(JOSTND, 3302) KEYWRD

      IF (REINEK(1) .EQ. 0) THEN
         IF(LKECHO)WRITE(JOSTND, 3303) 'TREES/ACRE'
      ELSE
         IF(LKECHO)
     &   WRITE(JOSTND, 3303) 'REINEKE STAND DENSITY INDEX (SDI)'
      ENDIF

      IF (REINEK(2) .EQ. 0) THEN
         IF(LKECHO)WRITE(JOSTND, 3303) 'ENTIRE STAND'
      ELSE
         IF(LKECHO)
     &   WRITE(JOSTND, 3303) 'ONLY CLEAN (OUTSIDE DISEASE PATCHES)'
      ENDIF

      IF (REINEK(3) .EQ. 0) THEN
         IF(LKECHO)WRITE(JOSTND, 3303) 'ONLY LIVING TREES'
      ELSE
         IF(LKECHO)
     &   WRITE(JOSTND, 3303) 'ALL TREES (LIVING AND STANDING DEAD)'
      ENDIF

      IF (REINEK(1) .NE. 0) THEN
         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND, 3306) (REINEK(4) / ACRtoHA) / INTOCM
         ELSE
            IF(LKECHO)WRITE(JOSTND, 3306) REINEK(4)
         ENDIF
      ENDIF   

 3302 FORMAT (/A8, '  DNSCALC (REINEKE) OPTIONS:')
 3303 FORMAT (A40, ' USED TO CALCULATE OUTBREAK DENSITY THRESHOLD')
 3306 FORMAT (' SLOPE OF SDI FUNCTION = ', F5.3)

      GO TO 90

   34 CONTINUE
C
C  ==========  OPTION NUMBER 34: SMCOUT ===============================
C
C     OUTPUT FROM THE SPREAD RATE MONTE CARLO SIMULATION
C     (IN REGULAR TABLE FORMAT, WITH HEADINGS)     
C
      IMCOUT = 26
      IF (ARRAY(1) .GT. 0.0) IMCOUT = INT(ARRAY(1))

C.... OUTPUT FOR SMCOUT KEYWORD

      IF(LKECHO)WRITE(JOSTND,3401) KEYWRD, IMCOUT
 3401 FORMAT (/A8,'   SPREAD RATE MONTE CARLO OUTPUT ON LOGICAL',
     &               ' UNIT ',I4)

      GOTO 90

   35 CONTINUE
C
C  ==========  OPTION NUMBER 35: BBOUT ===============================
C
C     OUTPUT DETAILING BARK BEETLE MORTALITY BY SIZE CLASS
C     (IN REGULAR TABLE FORMAT, WITH HEADINGS)

C  12/11/14 implementatin of general report facilty eliminates the need
C  for unit number specification (field 1) and opening of the file
C  which occurs elsewhere. Report will be written to main out file and
C  will be optionally written to output DB, but I have not done the DB
C  part yet. -LD

CX      IBBOUT = 27
CX      IF (ARRAY(1) .GT. 0.0) IBBOUT = INT(ARRAY(1))

C.... OUTPUT FOR BBOUT KEYWORD

CX      IF(LKECHO)WRITE(JOSTND,3501) KEYWRD, IBBOUT
CX 3501 FORMAT (/A8,'   DETAILED BARK BEETLE MORTALITY OUTPUT ON ',
CX     &               'LOGICAL UNIT ',I4)

C     Set IBBOUT as trigger to call output routine.

      IBBOUT = 999
      IF(LKECHO)WRITE(JOSTND,3501) KEYWRD
 3501 FORMAT (/A8,'   DETAILED BARK BEETLE MORTALITY OUTPUT WILL ',
     &        'BE WRITTEN')

      GOTO 90

   36 CONTINUE
C
C  ==========  OPTION NUMBER 36: PLOTINF ===============================
C
C     Set the percent infected in each plot. If a plot is not mentioned,
C     assume there is no infection and that all trees in that plot are 
C     outside centers.

C.... Set the root disease type from field 1.

      IDI = MINRR 
      IF (LNOTBK(1)) IDI = INT(ARRAY(1))

C.... Set the LONECT flag to show that the model is being run with 
C.... multiple plots and tell the model that PLOTINF is being used.

      LONECT(IDI) = 2
      LPLINF = .TRUE.

      K = 0

C.... Start loop to read supplemental records.

 3601 READ (IREAD,3602) IDSPLT,RDPRP
 3602 FORMAT (I4,F5.2)

      IF (IDSPLT .EQ. -999 .OR. K .EQ. 50) GOTO 3610

      K = K + 1
      IANPLT(IDI,K) = IDSPLT
      PLPROP(IDI,K) = RDPRP
      GOTO 3601

 3610 CONTINUE

C.... Set M to print out sub-plot info.  If there are more then 25
C.... sub-plots then more then one record needs to be written.

      IF (K .LE. 25) M = K
      IF (K .GT. 25) M = 25 

C.... Write keyword.

      IF(LKECHO)WRITE(JOSTND,3611) KEYWRD, DISTYP(IDI),
     &      (IANPLT(IDI,J),J=1,M)

 3611 FORMAT (/A8,'   ROOT DISEASE IS ', A12 /
     &        T12, 'DISEASE SUBPLOTS READ IN'/
     &        T12, 'DISEASED PLOTS=', 25(I5))

      IF (K .LE. 25) GOTO 3655

C.... Write a new line.

      IF(LKECHO)WRITE(JOSTND,3612)
 3612 FORMAT (/)

C.... Set NEXT to print sub-plot numbers 25 to 50.

      NEXT = M + 1

      IF(LKECHO)WRITE(JOSTND,3613) (IANPLT(IDI,J),J=NEXT,K)
 3613 FORMAT (T26,25(I5))
     
 3655 CONTINUE

C.... Print out the proportion of root disease in each sub-plot.

      IF(LKECHO)WRITE(JOSTND,3621) (PLPROP(IDI,J),J=1,M)
 3621 FORMAT (T12, 'DISEASE PROPS =',25(F5.2))

      IF (K .LE. 25) GOTO 90

C.... Write a new line.

      IF(LKECHO)WRITE(JOSTND,3622)
 3622 FORMAT (/)

C.... Set NEXT to print sub-plot numbers 25 to 50.

      NEXT = M + 1

      IF(LKECHO)WRITE(JOSTND,3633) (PLPROP(IDI,J),J=NEXT,K)
 3633 FORMAT (T26,25(F5.2))

      GOTO 90

   37 CONTINUE
C
C  ==========  OPTION NUMBER 37: SDIRMULT ===========================
C
C     ROOT RADIUS MULTIPLIER BASED ON SDI. USER SPECIFIES A SLOPE
C     AND THE SDI AT WHICH THE MULTIPLIER SHOULD BE ONE. (SLOPE
C     SHOULD NORMALLY BE NEGATIVE). DEFAULT IS NO MULTIPLER
C
      SDNORM = 369.0
      SDISLP = -0.0033

      IF (LMTRIC) THEN
         IF (ARRAY(1) .GT. 0.0) SDNORM = ARRAY(1) / HAtoACR
         IF (LNOTBK(2)) SDISLP = ARRAY(2) * HAtoACR
      ELSE
         IF (ARRAY(1) .GT. 0.0) SDNORM = ARRAY(1)
         IF (LNOTBK(2)) SDISLP = ARRAY(2)
      ENDIF

C.... OUTPUT FOR SMCOUT KEYWORD

      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,3701)KEYWRD,SDNORM/ACRtoHA,
     &      SDISLP*ACRtoHA
      ELSE
         IF(LKECHO)WRITE(JOSTND,3701) KEYWRD, SDNORM, SDISLP
      ENDIF

 3701 FORMAT (/A8,'   ROOT RADIUS MULTIPLER IS ONE AT SDI= ',F6.1,
     &        ' WITH SLOPE = ',F8.4)

      GOTO 90

   38 CONTINUE
C
C  ==========  OPTION NUMBER 38: TIMEDEAD ==============================
C
C     TIME SINCE DEATH FOR DEAD STUFF IN THE INVENTORY.  USED TO REDUCE
C     THE AMOUNT OF INOCULUM PRESENT INITIALLY IN THE STAND BY DECAYING
C     THEDEAD STUFF TO THE PRESENT TIME.  USER CAN SPECIFY DIFFERENT
C     VALUES FOR EACH SIZE CLASS.  FIELD 6 (ONE VALUE FOR ALL CLASSES)
C     ELIMINATED 10/94.
C

      IF (LNOTBK(1)) DEDAGE(1) = INT(ARRAY(1))
      IF (LNOTBK(2)) DEDAGE(2) = INT(ARRAY(2))
      IF (LNOTBK(3)) DEDAGE(3) = INT(ARRAY(3))
      IF (LNOTBK(4)) DEDAGE(4) = INT(ARRAY(4))
      IF (LNOTBK(5)) DEDAGE(5) = INT(ARRAY(5))

C.... OUTPUT FOR TIMEDEAD KEYWORD

      IF (LMTRIC) THEN
         IF(LKECHO)WRITE(JOSTND,3811) KEYWRD, (DEDAGE(J),J=1,5)
      ELSE
         IF(LKECHO)WRITE(JOSTND,3801) KEYWRD, (DEDAGE(J),J=1,5)
      ENDIF

 3801 FORMAT (/ A8, '   TIME SINCE DEATH OF TREES IN INVENTORY IS:',
     &        /T12, ' 0- 12":', I4, '  12-24":', I4, '  24-48":', I4,
     &        /T12, '48-100":', I4, '   >100":', I4)
 3811 FORMAT (/ A8, '   TIME SINCE DEATH OF TREES IN INVENTORY IS:',
     &        /T12, '  0- 30CM:', I4, '  30-61CM:', I4,'  61-122CM:',I4,
     &        /T12, '122-254CM:', I4, '   >254CM:', I4)

      GOTO 90

   39 CONTINUE
C
C  ==========  OPTION NUMBER 39: RRHOSTS ==============================
C
C     THIS KEYWORD CHANGES THE HOST TREES FOR EACH OF THE ROOT DISEASE
C     TYPES SO THAT USERS CAN RUN THE MODEL THINKING ABOUT ROOT DISEASE
C     AS SOME OTHER MORTALITY AGENT
C
      ISPC = 0
      IDI = 1

      IF (LNOTBK(1)) THEN

C....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

         CALL SPDECD(1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &               ARRAY,KARD)

         IF (ISPC .GE. 1 .AND. ISPC .LE. MAXSP) THEN

C....       Change only one species

            IF (LNOTBK(2)) IDI=INT(ARRAY(2))

            IF (IDI .LT. 0 .OR. IDI .GT. ITOTRR) THEN

C....          INVALID DISEASE TYPE

               WRITE(JOSTND,3965) RRJSP(IRTSPC(ISPC)), IDI
               IDI = MAXRR
               IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISPC))
               WRITE(JOSTND,3966) IDI, DISTYP(IDI)
            ELSE
               IF (MAXRR .LT. 3) IDITYP(IRTSPC(ISPC)) = IDI
            ENDIF

         ELSEIF (ISPC .EQ. 0) THEN

C....       Change all species

            READ(IREAD,3963) (IHST(KSP), KSP=1,MAXSP)
 3963       FORMAT(11I5)

            DO 3910 KSP=1,MAXSP
               IDI = IHST(KSP)

               IF (IDI .LT. 0 .OR. IDI .GT. ITOTRR) THEN

C....             INVALID DISEASE TYPE

                  WRITE(JOSTND,3965) RRJSP(IRTSPC(KSP)), IDI
                  IDI = IDITYP(IRTSPC(KSP))
                  WRITE(JOSTND,3966) IDI, DISTYP(IDI)
               ELSE

C....             VALID DISEASE TYPE

                  IDITYP(IRTSPC(KSP))=IDI
               ENDIF
 3910       CONTINUE

         ELSE

C....       IF SPECIES OUT OF RANGE. PRINT MESSAGE AND IGNORE THE
C....       KEYWORD.

            WRITE(JOSTND,3961) KEYWRD, KARD(1), INT(ARRAY(1))
 3961       FORMAT(A8, '   ERROR!!!!    INVALID SPECIES : ', A3,
     &             ' (', I2, ');  KEYWORD IGNORED!!! ')
         ENDIF
      ENDIF

 3965 FORMAT(/'********   ERROR IN RRHOSTS, DISEASE TYPE OUT OF ',
     &       'RANGE (0-2)', /, T12, 'SPECIES: ', A16,
     &       ',        DISEASE TYPE: ',I5)
 3966 FORMAT(T12, 'DEFAULT TYPE USED:  ', I5, '    -    ', A12)

C.... Output for RRHOSTS

      IF(LKECHO)WRITE(JOSTND,3971) KEYWRD
      IF(LKECHO)WRITE(JOSTND,3972)
      IF(LKECHO)WRITE(JOSTND,3973)

      DO 3920 KSP=1,MAXSP
         IF (MAXRR .LT. 3) THEN
            IF(LKECHO)WRITE(JOSTND,3975) RRJSP(IRTSPC(KSP)),
     &                         DISTYP(IDITYP(IRTSPC(KSP)))
         ELSE
            IF(LKECHO)WRITE(JOSTND,3975) RRJSP(IRTSPC(KSP)),
     &                         DISTYP(MAXRR)
         ENDIF
 3920 CONTINUE

 3971 FORMAT(/ A8, '   HOST TREE SPECIES')
 3972 FORMAT(/T12, 5X, 'SPECIES', 14X, 'DISEASE TYPE')
 3973 FORMAT(T12, 41('-'))
 3975 FORMAT(T12, A16, 9X, A12)

      GOTO 90

   40 CONTINUE
C
C  ==========  OPTION NUMBER 40: INOCLIFE =============================
C
C     THIS KEYWORD CHANGES THE LIFESPAN AND DECAY PATTERN OF INOCULUM
C     IN THE FORM OF INFECTED ROOTS ON DEAD TREES AND STUMPS.
C     CHANGES AFFECT ALL SIZE CLASSES
C

      IPOINT = 0 

      IF (LNOTBK(7)) IPOINT = INT(ARRAY(7))

C.... CHECK TO MAKE SURE ROOT DISEASE TYPE IS ACCEPTABLE

      IF (INT(ARRAY(7)) .GT. ITOTRR) THEN
         WRITE(JOSTND,4010)
         WRITE(JOSTND,4020)
         IPOINT = 0
      ENDIF

 4010 FORMAT (/,'********   ERROR IN DISEASE TYPE, INOCLIFE KEYWORD')
 4020 FORMAT ('********   CHANGE WILL AFFECT ALL ACTIVE DISEASE ',
     &        'TYPES')

C.... SET THE ROOT DISEASE TYPE TO BE MODIFIED

      IRMIN = MINRR
      IRMAX = MAXRR

      IF (IPOINT .GT. 0) THEN
         IRMIN = IPOINT
         IRMAX = IPOINT
      ENDIF

      DO 4030 IDI=IRMIN,IRMAX

         IF (LMTRIC) THEN
            IF (LNOTBK(1)) DECFN(IDI,1,1) = ARRAY(1)
            IF (LNOTBK(2)) DECFN(IDI,2,1) = ARRAY(2) * MTOFT
            IF (LNOTBK(3)) RSITFN(IDI,1) = ARRAY(3) * MTOFT / CMTOIN
            IF (LNOTBK(4)) RSITFN(IDI,2) = ARRAY(4) * MTOFT
            IF (LNOTBK(5)) YRSITF(IDI,1,1) = ARRAY(5) / CMTOIN
            IF (LNOTBK(6)) YRSITF(IDI,2,1) = ARRAY(6) * MTOFT 
      
C....       ALSO ASSIGN THESE VARIABLES TO THE LARGER SIZE CLASS,
C....       IF THEY WERE CHANGED

            IF (LNOTBK(1)) DECFN(IDI,1,2) = ARRAY(1)
            IF (LNOTBK(2)) DECFN(IDI,2,2) = ARRAY(2) * MTOFT
            IF (LNOTBK(5)) YRSITF(IDI,1,2) = ARRAY(5) / CMTOIN
            IF (LNOTBK(6)) YRSITF(IDI,2,2) = ARRAY(6) * MTOFT
         ELSE
            IF (LNOTBK(1)) DECFN(IDI,1,1) = ARRAY(1)
            IF (LNOTBK(2)) DECFN(IDI,2,1) = ARRAY(2)
            IF (LNOTBK(3)) RSITFN(IDI,1) = ARRAY(3)
            IF (LNOTBK(4)) RSITFN(IDI,2) = ARRAY(4)
            IF (LNOTBK(5)) YRSITF(IDI,1,1) = ARRAY(5)
            IF (LNOTBK(6)) YRSITF(IDI,2,1) = ARRAY(6) 
      
C....       ALSO ASSIGN THESE VARIABLES TO THE LARGER SIZE CLASS,
C....       IF THEY WERE CHANGED

            IF (LNOTBK(1)) DECFN(IDI,1,2) = ARRAY(1)
            IF (LNOTBK(2)) DECFN(IDI,2,2) = ARRAY(2)
            IF (LNOTBK(5)) YRSITF(IDI,1,2) = ARRAY(5)
            IF (LNOTBK(6)) YRSITF(IDI,2,2) = ARRAY(6) 
         ENDIF   
 4030 CONTINUE
       
C.... OUTPUT FOR INOCLIFE KEYWORD

      DO 4040 IDI=IRMIN,IRMAX

         IF (LMTRIC) THEN
            IF(LKECHO)WRITE(JOSTND,4050) KEYWRD, DISTYP(IDI),
     &                          DECFN(IDI,1,2),
     &                          DECFN(IDI,2,2)*FTTOM,
     &                          RSITFN(IDI,1)*FTTOM/INTOCM,
     &                          RSITFN(IDI,2)*FTTOM,
     &                          YRSITF(IDI,1,2)/CMTOIN,
     &                          YRSITF(IDI,2,2)*FTTOM
         ELSE
            IF(LKECHO)WRITE(JOSTND,4050) KEYWRD, DISTYP(IDI),
     &                          DECFN(IDI,1,2),
     &                          DECFN(IDI,2,2), RSITFN(IDI,1),
     &                          RSITFN(IDI,2), YRSITF(IDI,1,2),
     &                          YRSITF(IDI,2,2)
         ENDIF
 4040 CONTINUE

 4050 FORMAT (/ A8, '   FOR ', A12, '   DECAY FUNCTION SLOPE IS ',
     &        F7.5, ' AND INTERCEPT IS ', F7.5,
     &        /T12, 'FUNCTION DESCRIBING THE ROOT RADIUS AT WHICH TO ',
     &        ' STOP DECAYING HAS SLOPE ', F7.5, ' AND INTERCEPT ',F7.5,
     &        /T12, 'FUNCTION DESCRIBING THE NUMBER OF YEARS TO NOT ',
     &        ' DECAY HAS SLOPE ', F7.4, ' AND INTERCEPT ', F7.4)

      GOTO 90

   41 CONTINUE
C
C  ==========  OPTION NUMBER 41: BBCLEAR ==============================
C
C     THIS KEYWORD ENSURES THAT NO BARK BEETLES WILL BE TURNED ON
C     IN THIS SIMULATION
C
      LBBON = .FALSE.

      IF(LKECHO)WRITE(JOSTND,4101) KEYWRD
 4101 FORMAT (/A8,'   NO BARK BEETLES WILL BE ACTIVE')

      GOTO 90

   42 CONTINUE
C
C  ==========  OPTION NUMBER 42: RRTYPE ==============================
C
C     THIS KEYWORD TELLS THE MODEL WHICH DISEASE TYPES ARE ACTIVE IN
C     THE STAND.  
C     FOR NOW, ASSUME THAT WE CAN HAVE ONLY ONE TYPE (1,2=ANNOSUS, 
C     3=ARMILLARIA, 4=PHELLINUS)
C
      LRTYPE = .TRUE.
C      
      MINRR = 0
      MAXRR = 0

      DO 4205 IDI=1,ITOTRR
         IF (LNOTBK(IDI)) THEN
            IPOINT = INT(ARRAY(IDI))
            IF (IPOINT .LT. MINRR .OR. MINRR .EQ. 0) MINRR = IPOINT
            IF (IPOINT .GT. MAXRR) MAXRR = IPOINT
         ENDIF   
 4205 CONTINUE                     

      IF (MINRR .LE. 0) MINRR = 1
      IF (MINRR .GT. ITOTRR) MINRR = ITOTRR
      IF (MAXRR .LE. 0) MAXRR = 1
      IF (MAXRR .GT. ITOTRR) MAXRR = ITOTRR
      IF (MAXRR .LT. MINRR) MAXRR = MINRR  

C.... THIS NEXT STATEMENT IS TO ENSURE THAT CAN ONLY HAVE ONE TYPE
C.... IN THE STAND

      IF (MAXRR .NE. MINRR .AND. (MINRR .NE. 1 .AND. MAXRR .NE.2)) THEN
         MAXRR = MINRR 

         WRITE(JOSTND,4209)
 4209    FORMAT (/T12, '***ERROR IN RRTYPE KEYWORD. ONLY ONE DISEASE ',
     &          'CAN BE SIMULATED IN THE STAND.')   
      ENDIF

      IF (MINRR .EQ. MAXRR) THEN
         IF(LKECHO)WRITE(JOSTND,4210) KEYWRD, DISTYP(MINRR)
 4210    FORMAT (/A8,3X,A15,' WILL BE SIMULATED.')
      ELSEIF (MAXRR .EQ. MINRR+1) THEN
         IF(LKECHO)WRITE(JOSTND,4215)
     &      KEYWRD,DISTYP(MINRR),DISTYP(MAXRR)
 4215    FORMAT (/A8,3X,A15,' AND ',A15,' WILL BE SIMULATED.')
      ENDIF
      
      GOTO 90
C
C ================ ENTRY POINT RDKEY =============================
C                                
C     SPECIAL ENTRY TO RETREIVE KEYWORDS
C
      ENTRY RDKEY(KEY,PASKEY)
      PASKEY = TABLE(KEY)

 9000 RETURN
      END
