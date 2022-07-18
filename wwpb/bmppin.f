      SUBROUTINE BMPPIN (IREADx,IRECNTx,IJOPPR)
C----------
C WWPB $Id$
C----------
C  **BMPPIN  DATE OF LAST REVISION: DECEMBER 13, 1994
C  **BMPPIN  DATE OF LAST REVISION: May 6, 1998 (RNH, May98)
C     Revised OPTION 29 -- SANITIZE to read user input stand lists
C                          similar to the way OPTION 28 -- SALVAGE
C                          works.  Also modified the BMSANI module to
C                          work like BMSALV
C  **BMPPIN  DATE OF LAST REVISION: May 27, 1998 (MJO May98)(MJO July98)
C     Added OPTION 38 -- BMFDBK beetle negative feedback keyword
C
C  **BMPPIN  DATE OF LAST REVISION Aug 26, 1999 (AJM)  Modified ATTRACT
c            section (Option 14) so that conversions are done appropriately.
c
C   Revised 3/21/00 AJM;  replaced all calls to CH8* sort subroutines to
C     C26* (ie c26srt & c26bsr). Passed arguments are now 26 characters.
C     NOTE: I also changed dimensions of NSSTDS and BMSTDS in BMCOM.f77.
C
C...Revised 3/28/00 AJM.  Modified BMHIST, WETSCORE, RPHERO, APHERO, SPRAY,
c     SPRAY, SANITIZE, SALVAGE, SLASHMGT, and NONSTOCK, so that supplemental 
C     records can be 26 characters long.
c
C   Revised: 7/00 Fixed logicals in QRROT and QSRUST.  Because they were 
C     changed (in BMINIT?) to be OFF by default, the logicals herein
C     need to be switched ON if the keyword is encountered.
C
C   Revised 4/01 AJM. Because I added CONTRL.F77 as common (so we can use
C   variable KWDFIL in BMTOTALS [et al.]), I am renaming the IREAD &
C   IRECNT arguments in this SUBROUTINE's argument list--so that they 
C   won't be used here as passed arguments (i.e. they are now included 
c   via common block.  Passed arguments are now dummy.)
C
C   Revised 9/19/01. ajm.  changed 8th argument in calls to MYOPEN from 
c     1 to 0. (With a 1, format of output files was altered...1st column 
c     was omitted, which resulted in errors in the ArcView Display tools.)
C
C   Revised 08/16/02 Lance R. David (FHTET)
C   Added option number 39, COMMENT keyword, processing.
C
C   10-NOV-2003 - Lance R. David (FHTET)
C      Added LFLAG to KEYRDR call statement argument list.

C   Revised 9/05 AJM (FHTET)
C    Adding new keyword BMOUT.  It contains 2 fields.  
C    Field 1 invokes writing of *NEW* "cycle" output file (*.bmc)
C    Field 2 invokes writing of *NEW* landscape average output file (*.bml).
C    See new BMOUT routine for details.  This change concurrent with the 
C    addition to the WWPBM of a new stand level option processor: BMIN,
C    used for processing ** stand level ** output generating keywords.
C    Keywords BMTOTALS, BMDETAIL, and BMDVOUT are now *OBSOLETE*!
C
C----------
C
C     WWPB - West Wide Pine Beetle
C            [MOUNTAIN PINE BEETLE/WESTERN PINE BEETLE/IPS]
C     OPTION PROCESSOR FOR THE MULTI-STAND MOUNTAIN PINE BEETLE MODEL.
C     THIS IS AN EXTENSION TO THE PARALLEL PROCESSING SYSTEM.
C
C     CALLED FROM: PPIN
C
C PARAMETER DEFINITIONS
C     IREAD  - logical unit number for the keyword file [I]               ! SEE 4/01 REVISION NOTE 
C     IRECNT - count for number of records read from the keyword file [O] ! DITTO
C     IJOPPR - logical unit number for standard output file.
C             (this is a dummmy variable for this routine because the
C              variable JOPPRT is in the PPE common space defined in the
C              include file PPCNTL.F77 which means JOPPRT can not be used
C              in the parameter list. The parameter is necessary because the
C              call from PPE routine PPIN needs to identify an output unit
C              on which to write a message if the WWPB model is not attached.
C              look in the WWPB stub routine EXBM.F77 entry point BMPPIN.)
C
C VARIABLE DEFINITIONS
C     DEFYRS - THE DEFAULT NUMBER OF YEARS AN OPTION WILL BE "ON" AT THE
C              GIVEN PARAMETER VALUES.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'PPCNTL.F77'

C Adding CONTRL for varialbe KWDFIL used with BMPPIN AJM 4/01

      INCLUDE 'CONTRL.F77'

      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMRRCM.F77'
      INCLUDE 'BMPCOM.F77'
C
COMMONS
C
      PARAMETER (KWCNT = 40)

C...TEST.  CURRENTLY, SUPPLEMENTAL STAND IDs ARE READ AS VARIABLE
C   "KEYWRD", WHICH DOESN'T WORK ANYMORE SINCE STAND IDs ARE NOW
C   CHARACTER*26.  SO I'M CREATING A NEW VARIABLE NAME, DIMENSIONED
C   CHARACTER*26, WHICH WILL BE THE SUPPLEMENTAL RECORDS FOR SANITIZE
C   ie THE LIST OF STAND IDs TO SANITIZE (WHICH NEED TO BE PASSED TO
C   SUBROUTINE C26BSR AS CHARACTER*26).

      CHARACTER*4   C4TMP
      CHARACTER*8   TABLE, KEYWRD, PASKEY
      CHARACTER*26  SUPLRECS
      CHARACTER*80  RECORD
      CHARACTER*10  KARD(7)
      CHARACTER*250 OUTNAM
      LOGICAL LNOTBK,LKECHO
      INTEGER MINSZ, NPARMS, ISIZ, IDT, IJOPPR, HISTSZ
      INTEGER BSPEC
      INTEGER MYLST1(MXSTND), MYLST2(MXSTND)
      REAL DEFYRS,ARRAY(7),HISTNUM
      DIMENSION PRMS(8)
      DIMENSION TABLE(KWCNT),LNOTBK(7)
      DATA DEFYRS/ 20/
      DATA ISIZE / KWCNT/
C
C *** Added BMFDBK to the end of table below. MJO May98
CChanging BMTOTALS to BMOUT.  Changing LBMVOL to LBMCYC

      DATA TABLE / 
     >     'DISPERSE','RVDENSE ','BMTOTALS','BMDETAIL','DEBUG   ',
     >     'NODEBUG ','RANNSEED','END     ','NOTOTAL ','BMDVOUT',
     >     'BMHIST  ','BMPARM  ','VARYRAIN','ATTRACT ','WETSCORE',
     >     'REPRODN ','BKPKILL ','LIGHTN  ','IPSDV   ','QWINDTH ',
     >     'QRROT   ','QSRUST  ','OTHERBB ','QMORT   ','RPHERO  ',
     >     'APHERO  ','SPRAY   ','SALVAGE ','SANITIZE','OWVALUES',
     >     'OWTYPE  ','OWIPSDV ','QFIRE   ','QDEFOL  ','SLASHMGT',
     >     'NONSTOCK','BMDAMAGE','BMFDBK  ','COMMENT ','BMOUT   '/
C
C     **********          EXECUTION BEGINS          **********
C
C     Initialization is carried out only the first time that BMPPIN is called
c     (ie, when LBMINT=True). This should avoid the problem of values that were
c     changed by a keyword being reset when the management keywords are added.

      IF (LBMINT) CALL BMINIT (JOPPRT)

      LKECHO = .TRUE.

   10 CONTINUE
      CALL KEYRDR (IREAD,JOPPRT,LBMDEB,KEYWRD,LNOTBK,
     >             ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)
C
C     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF
C
      IF (KODE .EQ. 0) GO TO 30
      IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
      CALL ERRGRO (.TRUE.,6)
      GOTO 10
   30 CONTINUE
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,LBMDEB,JOPPRT)
C
C     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.
C
      IF (KODE .EQ. 0) GOTO 90
      IF (KODE .EQ. 1) THEN
         CALL ERRGRO (.TRUE.,1)
         GOTO 10
      ENDIF
      GOTO 90
C
C     SPECIAL END-OF-FILE TARGET
C
C* 80 CONTINUE
C*    CALL ERRGRO (.FALSE.,2)
   90 CONTINUE
C
C     PROCESS OPTIONS *** Added 4800 to list below. MJO May98
C
      GO TO( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000
     >      ,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000
     >      ,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000
     >      ,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000)NUMBER
C
 1000 CONTINUE
C                        OPTION 1 -- DISPERSE
c      IF (LNOTBK(1)) GOTO 1005
c      CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
c      CALL ERRGRO (.TRUE.,4)
c      GOTO 10
C 1005 CONTINUE
      IF(LNOTBK(1))THEN
         I=IFIX(ARRAY(1))
      ELSE
         I=1
      ENDIF
      IF (.NOT.LNOTBK(2)) ARRAY(2)=50.
      CALL GPNEW (KODE,I,301,1,ARRAY(2))
      IF (KODE.GT.0) THEN
         CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
      ELSE
         IF(LKECHO)WRITE(JOPPRT,1010) KEYWRD,I,IFIX(ARRAY(2))
 1010    FORMAT (/1X,A8,'   ALL STANDS WILL BE INVOLVED IN ',
     >   'THE WWPB DISPERSAL AREA.'/T13,'SIMULATION IS SCHEDULED',
     >   ' TO START IN ',I4,' WITH A MAXIMUM DURATION OF ',I4,' YEARS.')
     
      ENDIF
      GOTO 10
 1200 CONTINUE
C                        OPTION NUMBER 2 -- RVDENSE
      LCDENS = .FALSE.
      IF(LKECHO)WRITE(JOPPRT,1210) KEYWRD
 1210 FORMAT (/1X,A8,'    STAND DENSITY WILL NOT AFFECT THE RATING ',
     >        'VALUE OF TREES')
      GOTO 10
 1300 CONTINUE
C                        OPTION NUMBER 3 -- BMTOTALS
C
C THIS IS NOW OBSOLETE.  ISSUE WARNING
C
      WRITE(JOPPRT,1310) KEYWRD
 1310 FORMAT (/1X,A8,'   WWPB KEYWORD BMTOTALS IS NO LONGER SUPPORTED.'/
     >   ,T13,'Keyword BMTOTALS has been replaced by keyword "BMOUT";',
     >     ' PLEASE REVISE YOUR KEYWORD SETS.'//,  
     >   T13,'! * !   THIS PROGRAM WILL REDIRECT YOUR BMTOTALS OUTPUT ',
     >       'REQUEST TO KEYWORD BMOUT (see below).      ! * !' )
C
C  GO TO BMOUT PROCESSING.  SET LNOTBKs TO FALSE TO INVOKE THE WRITING 
C  OF BOTH OF BMOUT's OUTPUT FILES 
C  (THESE SHOULD ALREADY BE FALSE ANYWAY; THIS ENSURES IT SO.)
C
      KEYWRD='BMOUT   '
      LNOTBK(1)=.FALSE.
      LNOTBK(2)=.FALSE.
C
      GO TO 5000     ! GO TO BMOUT 

C COMMENT OUT ALL THE REST  AJM SEPT 5005
C
CC          LBMVOL=.TRUE.
CC
CC     Set flag to write annual stand output to unit JBMSPY (28)
CC     and landscape annual data to unit JBMLPY (29)
CC     (RNH July98)
CC
C      LBMSPY= .TRUE.
C      LBMLPY= .TRUE.
CC
C      IF (LNOTBK(1)) JBMVOL=IFIX(ARRAY(1))
C      IF(LKECHO)WRITE(JOPPRT,1310) KEYWRD,JBMVOL
C 1310 FORMAT (/1X,A8,'   WWPB MAIN OUTPUT STATISTICS WILL BE ',
C     >       'WRITTEN TO DATA SET REFERENCE NUMBER ',I4)
C
CC ADDING CODE THAT WILL (1) OPEN THE OUTPUT FILES; (2) NAME THE OUTPUT FILES
CC INVOKED BY THIS KEYWORD.  Now, the output files are not named by the user;
CC instead, they are named the same name as the simulation filename, with extensions
CC *.bmt, *.bml, *.bms for the MAIN, LANDSCAPE-PER-YEAR and STAND-PER-YEAR
CC output files respectively.  Before this change, users would have to OPEN and name
CC these files separately and independently via the OPEN keyword.  NOW, if keyword
CC BMTOTALS is used, all three files will be written, opened, and named.  AJM 4/01
C
C      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmt'
CC      CALL MYOPEN (JBMVOL,OUTNAM,1,133,0,1,1,1,KODE)
C      CALL MYOPEN (JBMVOL,OUTNAM,1,133,0,1,1,0,KODE)
C      IF (KODE.GT.0) THEN
C         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
C      ENDIF
C
C      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bml'
CC      CALL MYOPEN (JBMLPY,OUTNAM,1,133,0,1,1,1,KODE)
C      CALL MYOPEN (JBMLPY,OUTNAM,1,133,0,1,1,0,KODE)
C      IF (KODE.GT.0) THEN
C         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
C      ENDIF
C
C      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bms'
CC      CALL MYOPEN (JBMSPY,OUTNAM,1,133,0,1,1,1,KODE)
C      CALL MYOPEN (JBMSPY,OUTNAM,1,133,0,1,1,0,KODE)
C      IF (KODE.GT.0) THEN
C         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
C      ENDIF
C
      GOTO 10

 1400 CONTINUE
C                        OPTION NUMBER 4 -- BMDETAIL
C
C This keyword is now obsolete! AJM  8/05
C
c stand-level detailed output is now available via BMIN option processor

c      LBMDET=.TRUE.
c      IF (LNOTBK(1)) JBMDBH=IFIX(ARRAY(1))
C
C     ISSUE OBSOLETE WARNING AJM 9/05
C
      WRITE(JOPPRT,1410) KEYWRD
 1410 FORMAT (/1X,A8,'   WWPB MODEL KEYWORD BMDETAIL IS NOW OBSOLETE!',/
     >   T12,' Detailed output may be obtained via new * STAND-LEVEL *',
     >       ' WWPB Model keywords: MAINOUT, TREEOUT, BKPOUT, &',
     >       ' VOLOUT,',/T13,'which, if used, must be attached between',
     >       ' a "BMIN" & "END" set of keywords,',/T12,
     >       ' AND must be attached to each stand for which output ',
     >       ' is desired.',/T13,'(i.e. between each stand`s pair of',
     >       '"ADDSTAND" & "PROCESS" keywords.)')

C      IF(LKECHO)WRITE(JOPPRT,1410) KEYWRD,JBMDBH
C 1410 FORMAT (/1X,A8,'   WWPB DETAILED SIZE CLASS STATISTICS ',
c     >       'WILL BE WRITTEN TO DATA SET REFERENCE NUMBER ',I4)
C
C Adding code so that if this keyword is used (if detailed output file requested)
C then the file will be already opened and named.  User now no longer needs to explicilty
C name the file, nor use OPEN keyword.  Name given will be the simulation (keyword)
C filename, suffixed with "bmd".

c      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmd'
cC      CALL MYOPEN (JBMDBH,OUTNAM,1,133,0,1,1,1,KODE)
c      CALL MYOPEN (JBMDBH,OUTNAM,1,133,0,1,1,0,KODE)
c      IF (KODE.GT.0) THEN
c         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
c      ENDIF

      GOTO 10
 1500 CONTINUE
C                        OPTION NUMBER 5 -- DEBUG
      LBMDEB=.TRUE.
      IF(LKECHO)WRITE(JOPPRT,'(/1X,A8)') KEYWRD
      GOTO 10
 1600 CONTINUE
C                        OPTION NUMBER 6 -- NODEBUG
      LBMDEB=.FALSE.
      IF(LKECHO)WRITE(JOPPRT,'(/1X,A8)') KEYWRD
      GOTO 10
 1700 CONTINUE
C                        OPTION NUMBER 7 -- RANNSEED
      CALL BMRNSD (LNOTBK(1),ARRAY(1))
      IF(LKECHO)WRITE(JOPPRT,1710) KEYWRD,ARRAY(1)
 1710 FORMAT (/1X,A8,'   RANDOM SEED IS:',F10.0)
c define new variable for identifying this specific run in a series
c of monte carlo simulations.  the randome number seed, unique to
c each simulation, is the identifier.  A new ouput file is written from
c bmoutm, this variable included.  this variable added to common block.
c      DREWTEST = ARRAY(1)
      GOTO 10
 1800 CONTINUE
C                        OPTION NUMBER 8 -- END
      IF(LKECHO)WRITE(JOPPRT,1810) KEYWRD
 1810 FORMAT (/1X,A8,'   END OF MULTI-STAND WWPB OPTIONS.')
      GOTO 900
 1900 CONTINUE
C                        OPTION NUMBER 9 -- NOTOTAL

      LBMVOL=.FALSE.
      IF(LKECHO)WRITE(JOPPRT,1910) KEYWRD
 1910 FORMAT (/1X,A8,'    WWPB MAIN OUTPUT FILE WILL NOT BE WRITTEN.')
      GOTO 10
 2000 CONTINUE
C                        OPTION NUMBER 10 -- BMDVOUT
C
C This keyword is now obsolete! AJM  8/05
C
c stand-level detailed output is now available via BMIN option processor
c
c      LBMDVO=.TRUE.
c      IF (LNOTBK(1)) JBMDV=IFIX(ARRAY(1))
C
C     ISSUE OBSOLETE WARNING AJM 9/05
C
      WRITE(JOPPRT,2010) KEYWRD
 2010 FORMAT (/1X,A8,'   WWPB MODEL KEYWORD BMDVOUT IS NOW OBSOLETE!',/
     > T12,' Driving variable output may be obtained via a new',
     >     ' *STAND-LEVEL* WWPB Model keyword: BKPOUT,'/T12,
     >     ' which, if used, must be attached between a "BMIN" & "END"',
     >     ' set of keywords,'/T13,'AND must be attached to each',
     >     ' stand for which output is desired.',/T12,'(i.e. between',
     >     ' each stand`s pair of "ADDSTAND" & "PROCESS" keywords.)')
C
c      IF(LKECHO)WRITE(JOPPRT,2010) KEYWRD, JBMDV
c 2010 FORMAT (/1X,A8,'   WWPB DRIVING VARIABLES OUTPUT FILE',
c     >       ' WILL BE WRITTEN TO DATA SET REFERENCE NUMBER ',I4)
cC
cC Adding code so that if this keyword is used (if driving variable output file requested)
cC then the file will be already opened and named.  User now no longer needs to explicilty
cC name the file, nor use OPEN keyword.  Name given will be the simulation (keyword)
cC filename, suffixed with "bmv".
c
c      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmv'
cC      CALL MYOPEN (JBMDV,OUTNAM,1,133,0,1,1,1,KODE)
c      CALL MYOPEN (JBMDV,OUTNAM,1,133,0,1,1,0,KODE)
c      IF (KODE.GT.0) THEN
c         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
c      ENDIF
c
      GOTO 10
 2100 CONTINUE
C                        OPTION NUMBER 11 -- BMHIST
c
c     Initial conditions for WWPB infestation
      HISTNUM = 0
      IUNIT= IREAD
      BSPEC= PBSPEC
      IF (LNOTBK(1)) IUNIT=IFIX(ARRAY(1))
      IF (LNOTBK(2)) BSPEC=IFIX(ARRAY(2))

      IF(LKECHO)WRITE(JOPPRT,2110) KEYWRD,IUNIT,BSPEC
 2110 FORMAT (/1X,A8,'   BEETLE-KILLED TREES WILL BE READ FROM ',
     >        'DATA REFERENCE NUMBER',I4,' FOR BEETLE SPECIES ',I3)

      BMCNT=0
 2120 CONTINUE

C
C...Changes below for 26-character supplemental records.
C
C   This line is original code...READ (IUNIT,'(A8,T11,7F10.0)',END=2130) KEYWRD,ARRAY
C
C      The line below works, but I'm changing ARRAY.
C      READ (IUNIT,'(A26,T31,2F10.2,5F1.0)',END=2130) SUPLRECS,ARRAY
C      IF (INDEX(KEYWRD,'-999').GT.0) GOTO 2130
C
C   I've defined 2 new local variables--HISTSZ and HISTNUM--to substitute
c   for ARRAY(1) and ARRAY(2) parameters.
C
      READ(IUNIT,'(A26,T31,I10,F10.0)',END=2130)SUPLRECS,HISTSZ,HISTNUM
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 2120
      IF (INDEX(SUPLRECS,'-999').GT.0) GOTO 2130
      BMCNT=BMCNT+1
      IF (BMCNT.GT. MXSTND) THEN  
C     NOTE AJM 12/05.
C     SOMETIME IN THE FUTURE, WE MIGHT WANT TO CHANGE THE WAY THIS IS DONE.
C     BECAUSE WE TEMPORARILY "CO-OPT" ARRAY BMSTDS() HERE, WE NECESSARILY
C     CONSTRAIN BMCNT TO BE NO GREATER THAN MXSTND.  BUT BMCNT SHOULD BE ABLE 
C     TO BE GREATER THAN MXSTND.  FOR EXAMPLE, USERS CURRENTLY MAY ENTER FOR 
C     EACH STAND MORE THAN ONE RECORD IN THE BMHIST SUPPLEMENTAL RECORD LIST.  
C     IDEALLY, BMHIST STAND DATA OUGHT TO BE KEPT IN ITS OWN ARRAY(S) DIMENSIONED 
C     MXSTND*10. I AM LEAVING THIS AS IS FOR NOW, BECAUSE THERE IS NO IMMEDIATE 
C     NEED TO ENABLE MORE THAN 10,000 (MXSTND) # OF BMHIST SUPPLEMETAL RECORDS. 
C     NOTE HOWEVER THAT THE CONSTRAINT THAT THE NUMBER OF BMHIST RECORDS (BMCNT)
C     BE NO GREATER THAN MXSTND IS THEORETICALLY AN UNREASONABLE CONSTRAINT.
        BMCNT=MXSTND
        WRITE(JOPPRT,'(/T13,''MAX STANDS EXCEEDED; DATA '',
C     >         ''IGNORED: '',A8,2X,7F9.2)') KEYWRD,ARRAY
     >''IGNORED: '',A26,4X,I4,F6.2)') SUPLRECS,HISTSZ,HISTNUM
        IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
        GOTO 2120
      ENDIF
C      BMSTDS(BMCNT)= KEYWRD
      BMSTDS(BMCNT)= SUPLRECS
C     J= IFIX(ARRAY(1))
      J= HISTSZ
      IF ((J .GE. 1) .AND. (J .LE. NSCL)) THEN
        BMINI(BMCNT,BSPEC,1)= REAL(J)
        BMINI(BMCNT,BSPEC,2)= AMAX1(HISTNUM,0.0)
      ELSE
        BMINI(BMCNT,BSPEC,1)= -999.0
        BMINI(BMCNT,BSPEC,2)= -999.0
      ENDIF
      GOTO 2120

 2130 CONTINUE

      IF (IUNIT.EQ.IREAD) IRECNT=IRECNT+BMCNT+1
      IF(LKECHO)WRITE(JOPPRT,'(/T13,''RECORDS PROCESSED= '',I4)') BMCNT

      GOTO 10

 2200 CONTINUE
C                        OPTION NUMBER 12 -- BMPARM
C     Initialize many of the parameters used for the beetle model, such as species,
c     and number of generations
c     Minimum diam for attractiveness and distance divisor are now initialized in
c     ATTRACT

      IF (LNOTBK(1)) PBSPEC= IFIX(ARRAY(1))
      IF (PBSPEC .LE. 1) PBSPEC= 1
      IF (PBSPEC .GT. 4) PBSPEC= 4 
      IF (LNOTBK(2)) NBGEN= IFIX(ARRAY(2))
      IF (NBGEN .LE. 1) NBGEN= 1
      IF (NBGEN .GE. 5) NBGEN= 5
      IF (PBSPEC .EQ. 3) THEN
         NIBGEN = NBGEN
      ENDIF    

      IF (LNOTBK(3)) HSPEC(PBSPEC,7)= 0
      
      DO 2205 I= 3,7
       IF (LNOTBK(I)) THEN
         JSP= IFIX(ARRAY(I))
         HSPEC(PBSPEC,JSP)= 1
       ENDIF 
 2205 CONTINUE     

c    Turn off Ips as a driving variable if IPSDV was used before BMPARM and the main
c    beetle species is Ips

      IF (IPSON .AND. PBSPEC .EQ. 3) IPSON= .FALSE.
       
      IF(LKECHO)WRITE(JOPPRT,2210) KEYWRD,PBSPEC,NBGEN
 2210 FORMAT (/1X,A8,'   BEETLE SPECIES CODE IS:',I3,
     >       '     NUMBER OF GENERATIONS IS:',I3)
      DO 2225 I= 1,MAXSP
        IF (HSPEC(PBSPEC,I) .EQ. 1) THEN
          IF(LKECHO)WRITE(JOPPRT,2220) I
 2220     FORMAT ('            HOST TREE SPECIES WILL BE:',I4)
        ENDIF
 2225 CONTINUE

      GOTO 10
      
 2300 CONTINUE
C                        OPTION NUMBER 13 -- VARYRAIN
c Landscape level, ON in year 'IDT'; stays ON until turned off by user.
      
      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
      
      NPARMS= 6
      PRMS(1)= 0.0
      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      PRMS(2)=3.0
C      PRMS(2)= 0.3  ..CHANGED THIS 3/31/00 AJM
      IF(PRMS(1) .EQ. 0.0) THEN
         IF (LNOTBK(3)) PRMS(2)= MAX(0.01, ARRAY(3))
      ELSE
         IF (LNOTBK(3)) PRMS(2)= ARRAY(3)
      ENDIF
      PRMS(3)=  0.0
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      PRMS(4)=  0.0
      IF (LNOTBK(5)) PRMS(4)= ARRAY(5)
      PRMS(5)= -1.0
      IF (LNOTBK(6)) PRMS(5)= ARRAY(6)
      PRMS(6)=  0.0
      IF (LNOTBK(7)) PRMS(6)= ARRAY(7)

      CALL GPNEW (KODE,IDT,309,NPARMS,PRMS(1))
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,2305) KEYWRD
 2305   FORMAT(/1X,A8,' Unable to set options')
        GOTO 2330
      ENDIF

      IF (PRMS(1) .EQ. -1.0) THEN
        IF(LKECHO)WRITE(JOPPRT, 2321) KEYWRD, IDT
 2321 FORMAT(/1X,A8,'   CLIMATIC ACTIVITY WILL BE TURNED OFF ',
     >  'IN DATE/CYCLE ', I4)
      ELSE
        IF(LKECHO)WRITE(JOPPRT, 2310) KEYWRD, IDT
 2310   FORMAT(/1X,A8,'   SPECIFIED CLIMATIC ACTIVITY WILL BEGIN ',
     >    'IN DATE/CYCLE ', I4,' AND REMAIN ACTIVE UNTIL TURNED OFF')

        IF(PRMS(1) .EQ. 0.0) THEN
          IF(LKECHO)WRITE(JOPPRT, 2311) PRMS(2)
        ELSE
          IF(LKECHO)WRITE(JOPPRT, 2312) PRMS(2)
        ENDIF
 2311   FORMAT(12X,'CLIMATIC ACTIVITY WILL BE RANDOM WITH SD=',F7.3)
 2312   FORMAT(12X,'THE CHANGE IN CLIMATIC ACTIVITY WILL BE ',F7.3)

        IF(LKECHO)WRITE(JOPPRT, 2313) (PRMS(I),I=3,6)
 2313   FORMAT(12X,'THE COEFFICIENTS OF INFLUENCE ARE: DBH= ',F5.2,
     &       ' ELEVATION= ',F5.2,' SDI= ',F5.2,' OTHER= ',F5.2)
      ENDIF

c Note the use of a special stand id 'MXSTND' to hold landscape level global
c parms. The special ID is picked up in the relevant routine.

      CALL GPADSD(MXSTND)
      CALL GPCLOS(309)

 2330 CONTINUE

      GOTO 10

 2400 CONTINUE
C                        OPTION NUMBER 14 -- ATTRACT
C     These are the user-controlled parameters in the between stand
c     attractiveness equation

      IBSP = PBSPEC
      IF (LNOTBK(1)) IBSP= INT(ARRAY(1))
      IF (IBSP .LT. 1) IBSP = 1
      IF (IBSP .GT. 3) IBSP = 3

C     the second field contains the minimum dbh for attractiveness
      IF (LNOTBK(2)) THEN
         MINSZ= IFIX(ARRAY(2))
         DO 2401 I= 1,NSCL
           IF (MINSZ .LE. UPSIZ(I)) THEN
              ISCMIN(IBSP)= I
              GOTO 2402
           END IF
           IF (I .EQ. NSCL) ISCMIN(IBSP)= I
 2401   CONTINUE
 2402   CONTINUE
      ENDIF

C     The third field contains the special tree multiplier

      IF (LNOTBK(3)) USERA(IBSP)= ARRAY(3)

c This makes the 'old' DDWT=.1 give identical results to the new 'DDWT=.1'
c allowing for meters (old) and miles(new) and [1/ddwt**2] (old) and [ddwt] (new)
c Print the DDWT as the meters divisor (DDWTM) instead of the miles one
C      DDWTM = USERC(IBSP)
      IF (LNOTBK(4)) THEN
        USERC(IBSP)= AMAX1(1.0E-5,ARRAY(4))
      END IF
	DDWTM= USERC(IBSP)
        USERC(IBSP) = USERC(IBSP) * USERC(IBSP) * 259.0 * 10000.0
c         USERC(IBSP) = USERC(IBSP) * USERC(IBSP)
C      ENDIF
C
c     The fifth field controls the "n" in the actractiveness equation
c     "n" is assumed to be in square meters, so it will be multiplied
c     here to be in the same units as ("c" * distance)^2
C     Note that "n" cannot be zero since we take the log of it later

C      OLDN = SELFA(IBSP)
      IF (LNOTBK(5)) THEN
        SELFA(IBSP)= AMAX1(1.0E-5,ARRAY(5))
      END IF
	OLDN = SELFA(IBSP)
C       SELFA(IBSP)= USERC(IBSP) * USERC(IBSP) * SELFA(IBSP)
c
C       SELFA(IBSP) = SELFA(IBSP) * 259.0 * 10000.0
C
c...Above CHANGED 8/24/99 (ajm).
c
C      ENDIF
C
C...Moved the last two "END IF" statements up so that the conversion
c   will be performed on the default values of C and N, and not just
C   to those values input in the ATTRACT keyword. (AJM 8/24/99)
c
c     The last field contains Rmax, the value used in determining
c     the extent of outside world and also how far beetles can travel.

      IF (LNOTBK(6)) URMAX(IBSP) = ARRAY(6)
      URMAX(IBSP) = MAX(URMAX(IBSP),0.1)

      IF (ISCMIN(IBSP) .EQ. 1) THEN
        MINSIZ = 0
      ELSE
        MINSIZ = UPSIZ(ISCMIN(IBSP)-1)
      ENDIF

      IF(LKECHO)WRITE(JOPPRT,2450) KEYWRD,IBSP,INT(MINSIZ),
     >                    USERA(IBSP),DDWTM,OLDN,URMAX(IBSP)
C     >                    USERA(IBSP),USERC(IBSP),OLDN,URMAX(IBSP)

 2450 FORMAT (/1X,A8,'   FOR BEETLE SPECIES TYPE ',I4,
     >       '  MINIMUM ATTRACTIVE DIAMETER WILL BE ',I4,' INCHES;',
     >       '  SPECIAL TREE MULTIPLIER IS ',F6.3,/T13,
     >    'DISTANCE WILL BE MEASURED IN METERS TIMES ',F8.4,
     >    ' AND "N" WILL BE ',F8.4,' SQUARE METERS.',
     >    /T13,'BEETLES CAN "SEE" OR TRAVEL ',F7.1,' MILES (RMAX).')

      GOTO 10

 2500 CONTINUE

C                        OPTION NUMBER 15 -- WETSCORE
c

      IUNIT=IREAD
      IF (LNOTBK(1)) IUNIT=IFIX(ARRAY(1))

      IF(LKECHO)WRITE(JOPPRT,2510) KEYWRD,IUNIT
 2510 FORMAT (/1X,A8,'   SITE WETNESS INDICES WILL BE ',
     >        'READ FROM DATA REFERENCE NUMBER ',I4)

      WTCNT=0
 2520 CONTINUE
C      READ (IUNIT,'(A8,T11,7F10.0)',END=2530) KEYWRD,ARRAY
C      IF (INDEX(KEYWRD,'-999').GT.0) GOTO 2530
C      WTCNT=WTCNT+1
C      IF (WTCNT.GT.MXSTND) THEN
C        WTCNT=MXSTND
C        IF(LKECHO)WRITE(JOPPRT,'(/T13,''MAX STANDS EXCEEDED; DATA '',
C     >         ''IGNORED: '',A8,2X,7F9.2)') KEYWRD,ARRAY
C        IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C        GOTO 2520
C        ENDIF
C
C      WTSTDS(WTCNT)= KEYWRD
C      WTSC(WTCNT)= ARRAY(1)
C
C...Fix for 26-character supplemental records.  1/28/00.
c...this first attempt may not work--let's see if SUPLRECS
c   and ARRAY can be space delimited...

C...SPACE DELIMITING DID NOT WORK THIS WAY...
C...SO, I'll put variable ARRAY in columns 31-40

c      READ (IUNIT,*,END=2530) SUPLRECS,ARRAY
      READ (IUNIT,'(A26,T31,F10.3)',END=2530) SUPLRECS,ARRAY
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 2520
      IF (INDEX(SUPLRECS,'-999').GT.0) GOTO 2530
      WTCNT=WTCNT+1
      IF (WTCNT.GT.MXSTND) THEN
        WTCNT=MXSTND
        WRITE(JOPPRT,'(/T13,''MAX STANDS EXCEEDED; DATA '',
     >         ''IGNORED: '',A26,2X,7F9.2)') SUPLRECS,ARRAY
        IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
        GOTO 2520
      ENDIF

      WTSTDS(WTCNT)= SUPLRECS
      WTSC(WTCNT)= ARRAY(1)
      GOTO 2520

 2530 CONTINUE

      IF (IUNIT.EQ.IREAD) IRECNT=IRECNT+WTCNT+1
      IF(LKECHO)WRITE(JOPPRT,'(/T13,''RECORDS PROCESSED= '',I4)') WTCNT


      GOTO 10

 2600 CONTINUE
C                        OPTION NUMBER 16 -- REPRODN
C
C     This keyword sets the reproduction rate for BKP, used when beetles are 
C     emerging from killed (or strip-killed) trees.
C
      IBSP = PBSPEC
      IF (LNOTBK(1)) IBSP= INT(ARRAY(1))
      IF (LNOTBK(2)) REPLAC= ARRAY(2)
      IF (LNOTBK(3)) REPMAX= ARRAY(3)
      IF (LNOTBK(4)) DBHMAX= ARRAY(4)   
      
      RSLOPE = (REPMAX - 1.0) / (DBHMAX - REPLAC)
      B = 1.0 - (REPLAC * RSLOPE)

      LOW = 0
      DO 2610 I= 1,NSCL
        IF (I .GT. 1) LOW= UPSIZ(I-1)
        DBHMID= (UPSIZ(I) + LOW) / 2.0

        IF (DBHMID .LT. DBHMAX) THEN
          INC(IBSP,I) = (RSLOPE * DBHMID) + B
        ELSE
          INC(IBSP,I) = REPMAX
        ENDIF
C
C     CHECK FOR NEGATIVE VALUES (WHICH CAN HAPPEN IN SMALL SCs) AJM 9/05
C
        IF(INC(IBSP,I).LT.0.01) INC(IBSP,I)=0.01
C
 2610 CONTINUE
        
      IF(LKECHO)WRITE(JOPPRT, 2650) KEYWRD,IBSP,REPLAC,RSLOPE,
     &                              REPMAX,DBHMAX
 2650 FORMAT(/1X,A8,'   FOR BEETLE TYPE ',I4,' REPRODUCTION WILL ',
     &    'OCCUR WITH BMULT=1 AT DBH=',F6.2,/T13,'SLOPE=',F6.3,
     &    ' AND MAXIMUM AT BMULT=',F5.2,' AND DBH=',F6.2)
      
      GOTO 10       
      
 2700 CONTINUE
 
C                        OPTION NUMBER 17 -- BKPKILL
c
c     This keyword is used to invoke climatic events which affect
c     how the beetle reproduces once inside the tree. It affects 
c     the amount of beetles emerging from the tree and the amount of 
c     beetles in the outside world.

      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      NPARMS= 4
      PRMS(1)= 1.0  
      PRMS(2)= PBSPEC  
      IF (PBSPEC .LT. 3) PRMS(3)= 0.35
      IF (PBSPEC .EQ. 3) PRMS(3)= 0.035
      PRMS(4)= 0.035
      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(2)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(4)= ARRAY(5)
      
      CALL GPNEW (KODE,IDT,317,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,2705) KEYWRD
 2705   FORMAT(/1X,A8,' Unable to set options')
        GOTO 2780
      ENDIF

c Note the use of a special stand id 'MXSTND' to hold landscape level global
c parms. The special ID is picked up in the relevant routine.

      CALL GPADSD(MXSTND)
      CALL GPCLOS(317)

      IF(LKECHO)WRITE(JOPPRT, 2750) KEYWRD,IDT,INT(PRMS(1)),
     &                         INT(PRMS(2)),PRMS(3), PRMS(4)
 2750 FORMAT(/1X,A8,'   IN YEAR/CYCLE ',I4,' FOR ',I4,' YEARS',
     &    ' AND FOR BEETLE TYPE ',I4,
     &    ' REPRODUCTION WILL BE ',F6.3,' OR (IF TYPE 4)',F6.3)

 2780 CONTINUE

      GOTO 10

 2800 CONTINUE
C                        OPTION NUMBER 18 -- LIGHTN (Lightning)

c     Landscape level, Lightning is always on unless user specifies a
c     strike densiyt=0.  Keyword allows users to change strike density
c     in different years (or to turn lightning off)

      
      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      NPARMS= 2
      PRMS(1)= 1.0
      PRMS(2)= DENS
      
      IF (LNOTBK(2)) PRMS(2)= MAX(0.0, ARRAY(2))
      
      CALL GPNEW (KODE,IDT,306,NPARMS,PRMS(1))
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,2805) KEYWRD
 2805   FORMAT(/1X,A8,' Unable to set options')    
        GOTO 2830
      ENDIF
      
c Note the use of a special stand id 'MXSTND' to hold landscape level global
c parms. The special ID is picked up in the relevant routine.
     
      CALL GPADSD(MXSTND)
      CALL GPCLOS(306)

      IF(LKECHO)WRITE(JOPPRT, 2810) KEYWRD, IDT, IFIX(PRMS(1)), PRMS(2)
 2810 FORMAT(/1X,A8,'   LIGHTNING STRIKE DENSITY WILL CHANGE IN ',
     >  'DATE/CYCLE ',I4, ' END AFTER ',I4, ' YEARS,',
     >  /T13,'AND HAVE A STRIKE DENSITY OF ',F8.5,' TREES/ACRE.')
 
 2830 CONTINUE
 
      GOTO 10
 2900 CONTINUE
c                        OPTION NUMBER 19 -- IPSDV
c
c     Turn Ips on as a driving variable (IPSON=true) and set the min and max
c     size classes for killing. Size classes can be set here even if Ips is the
c     main beetle species. Also, set the number of generations for Ips

      IF (PBSPEC .NE. 3) IPSON= .TRUE.

      IF (LNOTBK(1)) THEN
        MINSZ= IFIX(ARRAY(1))
        DO 2901 I= 1,NSCL
          IF (MINSZ .LT. UPSIZ(I)) THEN
             IPSMIN= I
             GOTO 2902
          ENDIF
          IF (I .EQ. NSCL) IPSMIN= NSCL
 2901   CONTINUE
 2902   CONTINUE
      ENDIF
      
      IF (LNOTBK(2)) THEN
        MAXSZ= IFIX(ARRAY(2))
        DO 2903 I= 1,NSCL
          IF (MAXSZ .LT. UPSIZ(I)) THEN
             IPSMAX= I
             GOTO 2904
          ENDIF
          IF (IPSMAX .EQ. 0) IPSMAX= NSCL
 2903   CONTINUE
 2904   CONTINUE
      ENDIF

      IF (LNOTBK(3)) NIBGEN= IFIX(ARRAY(3))
      IF (LNOTBK(4)) PFSLSH= ARRAY(4)
      
      IF (IPSON) THEN
        IF(LKECHO)WRITE(JOPPRT,2910) KEYWRD
 2910   FORMAT (/1X,A8,'   IPS IS A DRIVING VARIABLE',I5)
      ENDIF
      IF(LKECHO)WRITE(JOPPRT,2915)KEYWRD,IPSMIN,IPSMAX,NIBGEN,PFSLSH
 2915 FORMAT (/1X,A8,'   MINIMUM SIZE CLASS IPS ATTACKS WILL BE',I5,
     &               '   MAXIMUM SIZE CLASS IPS CAN KILL WILL BE',I5,
     &               '   NUMBER OF IPS GENERATIONS WILL BE', I5,
     &               '   PROPORTION OF SLASH TO FILL FIRST IS ', F4.2)

      GOTO 10
 3000 CONTINUE
C                        OPTION NUMBER 20 -- WINDTHROW (QWINDTH)

c     Landscape level, ON in year 'IDT'; stays ON for specified duration

      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      NPARMS= 3
      PRMS(1)= DEFYRS
      PRMS(2)= 8.0 
      PRMS(3)= WTHRSH
      PRMS(4)= CRASH
      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(2)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(4)= ARRAY(5)
      PRMS(4) = AMIN1(PRMS(4),1.0)

      CALL GPNEW (KODE,IDT,305,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3005) KEYWRD
 3005   FORMAT(/1X,A8,' Unable to set options')    
        GOTO 3030
      ENDIF
      
      IF(LKECHO)WRITE(JOPPRT,3010) KEYWRD, IDT, (PRMS(I), I=1,4)
 3010 FORMAT(/1X,A8,'   Landscape windthrow to be attempted ',
     >  'beginning in date/cycle:', I4,' and lasting', F4.0,
     >  ' years.',/T13,'Minimum height to be thrown:',F6.1,
     >  ' feet. Minimum number of eligible stems/acre:',
     >   F6.0,/T13,'Proportion to throw: ',F4.2)

c Note the use of stand ID MXSTND, which is a dummy for the use of the keyword for
c the entire landscape. Any 1:MXSTND value could be used.
     
      CALL GPADSD(MXSTND)
      CALL GPCLOS(305)
 
 3030 CONTINUE                    
      GOTO 10 
      
C                        OPTION NUMBER 21 -- QRROT
C
C     Read in the parameters associated with quick root disease model 

 3100 CONTINUE
      IF (LNOTBK(1)) BMRRSR = ARRAY(1)
      IF (BMRRSR .LT. 0.0) BMRRSR = 0.0
      IF (BMRRSR .GT. 1.0) BMRRSR = 1.0
C      IF (BMRRSR .EQ. 0.0) LRRON = .FALSE.
      IF (BMRRSR .GT. 0) LRRON = .TRUE.
      IF(LKECHO)WRITE(JOPPRT,3150) KEYWRD, BMRRSR
 3150 FORMAT (/1X,A8,'   QUICK ROOT DISEASE: ANNUAL SPREAD RATE',
     >   '(0-1) FOR TREES INFECTED AT INVENTORY WILL BE', F4.2)
      GOTO 10


C                        OPTION NUMBER 22 -- QSRUST
C
C     Read in the parameters associated with quick stem rust model

 3200 CONTINUE
      IF (LNOTBK(1)) BMSRSR = ARRAY(1)
      IF (BMSRSR .LT. 0.0) BMSRSR = 0.0
      IF (BMSRSR .GT. 1.0) BMSRSR = 1.0
C      IF (BMSRSR .EQ. 0.0) LSRON = .FALSE.
      IF (BMSRSR .GT. 0) LSRON = .TRUE.

      IF(LKECHO)WRITE(JOPPRT,3250) KEYWRD, BMSRSR
 3250 FORMAT (/1X,A8,'   QUICK STEM RUST: ANNUAL SPREAD RATE',
     >   '(0-1) FOR TREES INFECTED AT INVENTORY WILL BE', F4.2)
      GOTO 10

c                        OPTION NUMBER 23 -- OTHERBB
c
c     This is loosely based on the Type 3 bark beetles in the root disease models.
c     Parameters are all initialized here (maybe should be done in a common block?)
c     and are somewhat based on the defaults used in the Annosus model. 
c
c     On in year specified, for specified number of years, but attacks only occur if
c     conditions are right.                       

 3300 CONTINUE                                    
      IDT= 1
      IF (LNOTBK(1)) IDT= INT(ARRAY(1))

      NPARMS= 6
      PRMS(1)= DEFYRS
      PRMS(2)= 4.0
      PRMS(3)= 10.0
      PRMS(4)= 0.75
      PRMS(5)= 0.7

      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      IF (LNOTBK(3)) THEN
          TEMP= ARRAY(3)
          PRMS(2)= 0.0
      ENDIF
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(4)= ARRAY(5)
      IF (LNOTBK(6)) PRMS(5)= ARRAY(6)

      ISIZ= 0
 3310 CONTINUE
      IF ((PRMS(2) .EQ. 0.0) .AND. (ISIZ .LT. NSCL)) THEN
         ISIZ= ISIZ + 1
         IF (TEMP .LE. UPSIZ(ISIZ)) PRMS(2)= ISIZ
         GOTO 3310
      ELSE
         IF (PRMS(2) .EQ. 0.0) PRMS(2)= NSCL
         GOTO 3315
      ENDIF
 3315 CONTINUE

      CALL GPNEW (KODE,IDT,323,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3320) KEYWRD
 3320   FORMAT(/1X,A8,' Unable to set options')
        GOTO 3330
      ENDIF

      IF(LKECHO)WRITE(JOPPRT, 3340) KEYWRD,IDT,INT(PRMS(1)),
     &                              (PRMS(I),I=2,5)
 3340 FORMAT(/1X,A8,'   OTHER BARK BEETLES TO BE ATTEMPTED IN DATE',
     > '/CYCLE=',I5,' AND LASTING', I4, ' YEARS; MIN SIZE CLASS=',F4.0,
     > /T13,'MINIMUM ELIGIBLE STEMS FOR EVENT= ',F7.1, ' STEMS/ACRE; ',
     > 'ATTACK RATE= ',F5.3, /T13,'MAXIMUM RV= ',F4.2)

c Note the use of stand ID MXSTND, which is a dummy for the use of the keyword for
c the entire landscape. Any 1:MXSTND value could be used.

      CALL GPADSD(MXSTND)
      CALL GPCLOS(323)

 3330 CONTINUE
      GOTO 10

 3400 CONTINUE
C                        OPTION NUMBER 24 -- "Quick" Mortality model (QMORT)
c
c     Keyword that causes increasing mortality on host or non-host trees.

      IDT= 1
      IF (LNOTBK(1)) IDT= INT(ARRAY(1))

      NPARMS= 6
      PRMS(1)= DEFYRS
      PRMS(2)= 2.0
      PRMS(3)= 0.0
      PRMS(4)= 0.0
      PRMS(5)= 0.5
      TEMP = 0.0
      TEMP1= 100.0
      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(2)= ARRAY(3)
      IF (LNOTBK(4)) TEMP= ARRAY(4)
      IF (LNOTBK(5)) TEMP1= ARRAY(5)
      IF (LNOTBK(6)) PRMS(5)= ARRAY(6)
      PRMS(6)= PRMS(1)

      ISIZ= 0
 3410 CONTINUE
      IF (((PRMS(3) .EQ. 0.0) .OR. (PRMS(4) .EQ. 0.0))
     >                         .AND. (ISIZ .LT. NSCL)) THEN
         ISIZ= ISIZ + 1
         IF (TEMP .LE. UPSIZ(ISIZ) .AND. PRMS(3) .EQ. 0.0)
     >                PRMS(3)= ISIZ
         IF (TEMP1 .LE. UPSIZ(ISIZ) .AND. PRMS(4) .EQ. 0.0)
     >                PRMS(4)= ISIZ
         GOTO 3410
      ELSE
         IF (PRMS(3) .EQ. 0.0) PRMS(3)= NSCL
         IF (PRMS(4) .EQ. 0.0) PRMS(4)= NSCL
         GOTO 3415
      ENDIF

 3415 CONTINUE
C
      CALL GPNEW (KODE,IDT,307,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3406) KEYWRD
 3406   FORMAT(/1X,A8,'WARNING: Unable to set options')
        GOTO 3470
      ENDIF
c Note the use of stand ID MXSTND, which is a dummy for the use of the keyword for
c the entire landscape. Any 1:MXSTND value could be used.

      CALL GPADSD(MXSTND)
      CALL GPCLOS(307)

      IF(LKECHO)WRITE(JOPPRT, 3450) KEYWRD, IDT, (PRMS(I), I=1,5)
 3450 FORMAT(/1X,A8,'   Extra mortality to start in date/cycle ', I5,
     >  ' and will last ',F4.0,' years;  Species =',F3.0,
     >  ';  Min siz class=',F4.0,/T13,'Max size class= ',
     >  F4.0,' Total mortality= ',F5.2)

 3470 CONTINUE

      GOTO 10
 3500 CONTINUE
C                        OPTION NUMBER 25 -- RPHERO (Repelling pheromone)
c
c     Apply repelling pheromone in year 'IDT' to stand 'NNN'.

      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IUNIT=IREAD
      IF (LNOTBK(2)) IUNIT=IFIX(ARRAY(2))

      NPARMS = 1
      PRMS(1) = 75
      IF (LNOTBK(3)) PRMS(1)= ARRAY(3)
      PRMS(1) = AMIN1(PRMS(1),100.0)
      PRMS(1) = AMAX1(PRMS(1),0.0)

      IF (NOSTND.LE.0) THEN
        WRITE(JOPPRT,3504) KEYWRD
 3504   FORMAT(/1X,A8,' Stands not yet read; ',
     >         'Keyword MUST follow inventory')
        GOTO 3540
      ENDIF

      IF(LKECHO)WRITE(JOPPRT,3510) KEYWRD, IDT, PRMS(1), IUNIT
 3510 FORMAT(/1X,A8,'   REPELLING pheromone applied in year ', I5,
     >  '; with efficiency =',F5.2,'%. Stands read from logical ',
     >  'unit ',I4)

      CALL C26SRT (NOSTND,STDIDS,MYLST1,.TRUE.)

c     Read the input stand IDs, find their location in BMSTDS, and assign
c     that position to MYLST2.

      J = 0
*******************************************************
C 3520 CONTINUE
C      READ(IUNIT,'(A8)',END=3530) KEYWRD
C      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3530
C      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
C      IF (IP .GT. 0) THEN
C        J = J + 1
C        IF (J .LE. MXSTND) THEN
C          MYLST2(J) = IP
C          IF(LKECHO)WRITE(JOPPRT,3525) KEYWRD, IP
C 3525     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)
C
C...Fix for 26-character supplemental records.  AJM 3/28/00
*******************************************************
 3520 CONTINUE
      READ(IUNIT,*,END=3530) SUPLRECS
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 3520
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 3530
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
      CALL C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
      IF (IP .GT. 0) THEN
        J = J + 1
        IF (J .LE. MXSTND) THEN
          MYLST2(J) = IP
          IF(LKECHO)WRITE(JOPPRT,3525) SUPLRECS, IP
 3525     FORMAT(T13,'STAND ID: ',A26,'   position ',I4)
        ENDIF
      ENDIF
      GOTO 3520
 3530 CONTINUE

c     Apply the list to the global option list.

      CALL GPADD (KODE, IDT, 302, NPARMS, PRMS(1), J, MYLST2)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3505) KEYWRD
 3505   FORMAT(/1X,A8,' No room to set option.')
        GOTO 3540
      ENDIF

      CALL GPCLOS(302)
 3540 CONTINUE

      GOTO 10

 3600 CONTINUE
C                        OPTION NUMBER 26 -- APHERO (Attracting pheromone)
c
c     Apply attracting pheromone in year 'IDT' to stand 'NNN'.

      IF (NOSTND .EQ. 0) THEN
        WRITE(JOPPRT,3604) KEYWRD
 3604   FORMAT(/1X,A8,' Stands not yet read; ',
     >    'Keyword MUST follow inventory.')
        GOTO 3640
      ENDIF

      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IUNIT=IREAD
      IF (LNOTBK(2)) IUNIT=IFIX(ARRAY(2))

      NPARMS= 1
      PRMS(1)= 75
      IF (LNOTBK(3)) PRMS(1)= ARRAY(3)
      PRMS(1) = AMIN1(PRMS(1),100.0)
      PRMS(1) = AMAX1(PRMS(1),0.0)

      IF(LKECHO)WRITE(JOPPRT,3610) KEYWRD, IDT, PRMS(1), IUNIT
 3610 FORMAT(/1X,A8,'   ATTRACTING pheromone applied in year ', I5,
     >  '; with efficiency =',F5.2,'%. Stands read from logical ',
     >  'unit ',I4)

c     Sort the STDIDS vector and return MYLST1 as its sorted order.

      CALL C26SRT (NOSTND,STDIDS,MYLST1,.TRUE.)

c     Read the input stand IDs, find their location in BMSTDS, and assign
c     that position to MYLST2.

      J = 0
**********************************************************
C 3620 CONTINUE
C      READ(IUNIT,'(A8)',END=3630) KEYWRD
C      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3630
C      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
C      IF (IP .GT. 0) THEN
C        J = J + 1
C        IF (J .LE. MXSTND) THEN
C          MYLST2(J) = IP
C          IF(LKECHO)WRITE(JOPPRT,3625) KEYWRD, IP
C 3625     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)
C
C...Fix for 26-character supplemental records.  AJM 3/28/00
***********************************************************
 3620 CONTINUE
      READ(IUNIT,*,END=3630) SUPLRECS
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 3620
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 3630
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
      CALL C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
      IF (IP .GT. 0) THEN
        J = J + 1
        IF (J .LE. MXSTND) THEN
          MYLST2(J) = IP
          IF(LKECHO)WRITE(JOPPRT,3625) SUPLRECS, IP
 3625     FORMAT(T13,'STAND ID: ',A26,'   position ',I4)
        ENDIF
      ENDIF
      GOTO 3620
 3630 CONTINUE

c     Apply the list to the global option list.

      CALL GPADD (KODE, IDT, 303, NPARMS, PRMS(1), J, MYLST2)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3605) KEYWRD
 3605   FORMAT(/1X,A8,' No room to set option.')
        GOTO 3640
      ENDIF

      CALL GPCLOS(303)

 3640 CONTINUE

      GOTO 10

 3700 CONTINUE
C                        OPTION NUMBER 27 -- SPRAY (Pesticides)
c
c     Apply pesticide in year 'IDT' to stands listed by the user.

      IF (NOSTND .EQ. 0) THEN
        WRITE(JOPPRT,3704) KEYWRD
 3704   FORMAT(/1X,A8,' Stands not yet read; ',
     >    'Keyword MUST follow inventory.')
        GOTO 3640
      ENDIF

      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
      IUNIT=IREAD
      IF (LNOTBK(2)) IUNIT=IFIX(ARRAY(2))

      NPARMS= 1
      IF (LNOTBK(3)) PRMS(1)= ARRAY(3)

      IF(LKECHO)WRITE(JOPPRT, 3750) KEYWRD,IDT,PRMS(1),IUNIT
 3750 FORMAT(/1X,A8,'   Pesticide will be applied in date/cycle ', I5,
     >'  Prop of trees in stand sprayed =',F4.2,/T13,'Stands read from',
     >' logical unit ',I4)

      CALL C26SRT (NOSTND,STDIDS,MYLST1,.TRUE.)

      J = 0
 3720 CONTINUE
C
C      READ(IUNIT,'(A8)',END=3730) KEYWRD
C      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3730
C      CALL C26BSR (NOSTND,STDIDS,MYLST1,KEYWRD,IP)
C      IF (IP .GT. 0) THEN
C        J = J + 1
C        IF (J .LE. MXSTND) THEN
C          MYLST2(J) = IP
C          IF(LKECHO)WRITE(JOPPRT,3725) KEYWRD, IP
C 3725     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)
C
C...Fix for 26-character supplemental records.  AJM 3/28/00
***********************************************************
c
      READ(IUNIT,*,END=3730) KEYWRD
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 3720
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 3730
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
      CALL C26BSR (NOSTND,STDIDS,MYLST1,SUPLRECS,IP)
      IF (IP .GT. 0) THEN
        J = J + 1
        IF (J .LE. MXSTND) THEN
          MYLST2(J) = IP
          IF(LKECHO)WRITE(JOPPRT,3725) SUPLRECS, IP
 3725     FORMAT(T13,'STAND ID: ',A26,'   position ',I4)
        ENDIF
      ENDIF
      GOTO 3720
 3730 CONTINUE

      CALL GPADD (KODE, IDT, 304, NPARMS, PRMS(1), J, MYLST2)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3705) KEYWRD
 3705   FORMAT(/1X,A8,' No room to set option.')
        GOTO 3740
      ENDIF

      CALL GPCLOS(304)
 3740 CONTINUE

      GOTO 10

 3800 CONTINUE
C                        OPTION NUMBER 28 -- SALVAGE
C
C     Salvage removes dead standing wood. User must specify the stand(s) that will be salvaged.
C
C     Modified the keyword input array to correspond with the SANITIZE
C     keyword and to consider minimum volume for action on a stand
C     (RNH May98)
C
      IF (NOSTND .EQ. 0) THEN
        WRITE(JOPPRT,3804) KEYWRD
 3804   FORMAT(/1X,A8,' Stands not yet read; ',
     >    'Keyword MUST follow inventory.')
        GOTO 3840
      ENDIF
C
C     Set Defaults
C
      IDT= 1
      IUNIT=IREAD
C      NPARMS= 4
C
C     With Min. Vol. of Salvage number of Parameters is 5 (RNH May 98)
      NPARMS= 5
      TEMP= 10.0
      TEMP1= 60.0
      TEMP2= 2.0
C
C     Default Min. Vol. for Salvage set to 10.0 (cf/acre) (RNH May98)
C
      PRMS(4)= 10.0
C
C     Efficiency in now 5th parameter in field 7 (RNH May 98)
C      PRMS(4)= 0.8

      PRMS(5)= 0.8

C     Set user defined variables
C
      IF (LNOTBK(1)) IDT= INT(ARRAY(1))
      IF (LNOTBK(2)) IUNIT= INT(ARRAY(2))
      IF (LNOTBK(3)) TEMP= ARRAY(3)             !  Min. DBH
      IF (LNOTBK(4)) TEMP1= ARRAY(4)            !  Max. DBH
      IF (LNOTBK(5)) TEMP2= ARRAY(5)            !  Max. age, dead wood
C
C     Min. Vol. is set to 4th parameter and Field 6 (RNH May 98)
      IF (LNOTBK(6)) PRMS(4)= ARRAY(6)
C
C     Efficiency is 5th parameter inField 7 (RNH May 98)
      IF (LNOTBK(7)) PRMS(5)= ARRAY(7)

      PRMS(1)= 0.0
      PRMS(2)= 0.0
      PRMS(3)= 0.0

C     DETERMINE WHICH SIZE CLASS CONTAINS THE MAX AND MIN DBH
C         classes are (for SDWP): <3, <10, <20, <60
      IF (TEMP .LE. 3.0) PRMS(1)= 1
      IF (TEMP1 .LE. 3.0) PRMS(2)= 1

      ISIZ= 0
 3810 CONTINUE
      IF (((PRMS(1) .EQ. 0.0) .OR. (PRMS(2) .EQ. 0.0))
     >                         .AND. (ISIZ .LT. MXDWHZ)) THEN
         ISIZ= ISIZ + 1
         IF (TEMP .LE. WPSIZ(ISIZ) .AND. PRMS(1) .EQ. 0.0)
     >                PRMS(1)= ISIZ + 1
         IF (TEMP1 .LE. WPSIZ(ISIZ) .AND. PRMS(2) .EQ. 0.0)
     >                PRMS(2)= ISIZ + 1
         GOTO 3810
      ELSE
         IF (PRMS(1) .EQ. 0.0) PRMS(1)= MXDWHZ + 1
         IF (PRMS(2) .EQ. 0.0) PRMS(2)= MXDWHZ + 1
         GOTO 3815
      ENDIF
 3815 CONTINUE

C     DETERMINE WHICH AGE CLASS CONTAINS THE MAXIMUM AGE
C         classes are: <1, 1-2, 2-5, 5-10, >10
      IF (TEMP2 .LT. 1.0) THEN
        PRMS(3)= 1
      ELSEIF (TEMP2 .LT. 2.0) THEN
        PRMS(3)= 2
      ELSEIF (TEMP2 .LT. 5.0) THEN
        PRMS(3)= 3
      ELSEIF (TEMP2 .LT. 10.0) THEN
        PRMS(3)= 4
      ELSE
        PRMS(3)= 5

      ENDIF
C
C     Change input echo to correspond to new input data fields (RNH May 98)
C
      IF(LKECHO)WRITE(JOPPRT, 3850) KEYWRD,IDT,TEMP,TEMP1,TEMP2,
     &                              PRMS(4),PRMS(5)
 3850 FORMAT(/1X,A8,'   SALVAGE WILL OCCUR IN', I5,
     >  ' MINIMUM DBH FOR SALVAGE=', F6.2,
     >  ' MAXIMUM DBH=',F6.2,T13,'MAX AGE FOR SALVAGE= ',F8.2,
     >  /T13,'MIN VOL. FOR SALVAGE=',F8.0,' EFFICIENCY =',F4.2)

c     Sort the STDIDS vector and return MYLST1 as its sorted order.

      CALL C26SRT (NOSTND,STDIDS,MYLST1,.TRUE.)

c     Read the input stand IDs, find their location in BMSTDS, and assign
c     that position to MYLST2.

      J = 0
c
c 3820 CONTINUE
c      READ(IUNIT,'(A8)',END=3830) KEYWRD
c      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
c      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3830
c      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
c      IF (IP .GT. 0) THEN
c        J = J + 1
c        IF (J .LE. MXSTND) THEN
c          MYLST2(J) = IP
c          IF(LKECHO)WRITE(JOPPRT,3825) KEYWRD, IP
c 3825     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)
c
C...Fix for 26-character supplemental records.  AJM 3/28/00
*************************************************
 3820 CONTINUE
      READ(IUNIT,*,END=3830) SUPLRECS
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 3820
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 3830
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
      CALL C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
      IF (IP .GT. 0) THEN
        J = J + 1
        IF (J .LE. MXSTND) THEN
          MYLST2(J) = IP
          IF(LKECHO)WRITE(JOPPRT,3825) SUPLRECS, IP
 3825     FORMAT(T13,'STAND ID: ',A26,'   position ',I4)
        ENDIF
      ENDIF
      GOTO 3820
 3830 CONTINUE

c     Apply the list to the global option list.

      CALL GPADD (KODE, IDT, 318, NPARMS, PRMS(1), J, MYLST2)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3805) KEYWRD
 3805   FORMAT(/1X,A8,' No room to set option.')
        GOTO 3840
      ENDIF

      CALL GPCLOS(318)

 3840 CONTINUE

      GOTO 10
 3900 CONTINUE
C                        OPTION NUMBER 29 -- SANITIZE

c     Currently this is landscape level (like the driving variables) so the user
c     specifies a year and threshold and any stands that meet the threshold criteria
c     in that year will have a sanitation cut performed.  We may want to change this
c     to be like the pheromones and have the user specify a stand to cut (instead of
c     a threshold).
C
C     Changed SANITIZE keyword to stand level (RNH May98)
C
C     From SALVAGE
C
      IF (NOSTND .EQ. 0) THEN
        WRITE(JOPPRT,3814) KEYWRD
 3814   FORMAT(/1X,A8,' Stands not yet read; ',
     >    'Keyword MUST follow inventory.')    
        GOTO 3841
      ENDIF
C
C     End from SALVAGE
C     Changed the keyword read such that SALVAGE and SANITIZE have
C     consistent keyword fields.  Added IUNIT=IREAD (RNH (May 98)
C
C     Default variable values
C
      IDT= 1         
      IUNIT = IREAD
      NPARMS= 5
      TEMP= 6.0
      TEMP1= 999.
      PRMS(3) = 0.0
C      PRMS(3)= 0.8
      PRMS(4)= 10.0
      PRMS(5)= 0.95
C
C     From Salvage (RNH May 98)
C     User defined variable values
C
      IF (LNOTBK(1)) IDT= INT(ARRAY(1))
      IF (LNOTBK(2)) IUNIT= INT(ARRAY(2)) !  Optional input file for stand list
C
C     Moved keyword parameter read over one field (RNH May 98)
C
      IF (LNOTBK(3)) TEMP= ARRAY(3)              !  Min. DBH
      IF (LNOTBK(4)) TEMP1= ARRAY(4)             !  Max. DBH
      IF (LNOTBK(5)) PRMS(3)= ARRAY(5)           !  Max. rating value (RV)
      IF (LNOTBK(6)) PRMS(4)= ARRAY(6)           !  Min. elibible Vol.
      IF (LNOTBK(7)) PRMS(5)= ARRAY(7)           !  Efficiency of Sanitation
C
C     Number of parameters remains the same as in old Sanitize keyword
C
      PRMS(1)= 0.0
      PRMS(2)= 0.0
      IF (TEMP .LE. 3.0) PRMS(1)= 1
      IF (TEMP1 .LE. 3.0) PRMS(2)= 1
C
C     From SALVAGE
C
C     DETERMINE WHICH SIZE CLASS CONTAINS THE MAX AND MIN DBH
C         classes are (for SDWP): <3, <10, <20, <60
      IF (TEMP .LE. 3.0) PRMS(1)= 1
      IF (TEMP1 .LE. 3.0) PRMS(2)= 1

C
C     End from SALVAGE
C      
      ISIZ= 0
 3910 CONTINUE    
      IF (((PRMS(1) .EQ. 0.0) .OR. (PRMS(2) .EQ. 0.0)) 
     >                         .AND. (ISIZ .LT. NSCL)) THEN
         ISIZ= ISIZ + 1
         IF (TEMP .LE. UPSIZ(ISIZ) .AND. PRMS(1) .EQ. 0.0)
     >                PRMS(1)= ISIZ
         IF (TEMP1 .LE. UPSIZ(ISIZ) .AND. PRMS(2) .EQ. 0.0)
     >                PRMS(2)= ISIZ
         GOTO 3910
      ELSE
         IF (PRMS(1) .EQ. 0.0) PRMS(1)= NSCL
         IF (PRMS(2) .EQ. 0.0) PRMS(2)= NSCL
         GOTO 3915
      ENDIF
 3915 CONTINUE
C
C     From SALVAGE
C
C      IF(LKECHO)WRITE(JOPPRT, 3950) KEYWRD,IDT,TEMP,TEMP1,(PRMS(I),I=3,5)
C 3950 FORMAT(/1X,A8,' SANITATION WILL OCCUR AT ', I5,
C     >  ' MINIMUM DBH FOR SANITATION= ', F6.2, 
C     >  ' MAXIMUM DBH= ',F6.2,' MAX. RV TO REMOVE= ',F5.2,
C     >  /T13,'MIN VOL. FOR SANITATION=',F8.0,' EFFICIENCY =',F4.2)
C
C     Change input echo to correspond to new input data fields (RNH May 98)
C
      IF(LKECHO)WRITE(JOPPRT, 3950) KEYWRD,IDT,TEMP,TEMP1,
     &                              (PRMS(I),I=3,5)
 3950 FORMAT(/1X,A8,' SANITATION WILL OCCUR AT ', I5,
     >  ' MINIMUM DBH FOR SANITATION= ', F6.2, 
     >  ' MAXIMUM DBH= ',F6.2,' MAX. RV TO REMOVE= ',F5.2,
     >  /T13,'MIN VOL. FOR SANITATION=',F8.0,' EFFICIENCY =',F4.2)
C
C     Sort the STDIDS vector and return MYLST1 as its sorted order.
C
      CALL C26SRT (NOSTND,STDIDS,MYLST1,.TRUE.)

c     Read the input stand IDs, find their location in BMSTDS, and assign
c     that position to MYLST2.

      J = 0     
C...Fix for 26-character supplemental records.  AJM 3/28/00
*************************************************
 3821 CONTINUE
C      READ(IUNIT,'(A8)',END=3831) KEYWRD
      READ(IUNIT,*,END=3831) SUPLRECS
C      READ(IUNIT,'(A8)',END=3831) SUPLRECS
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 3821
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 3831
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3831

C      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
      CALL C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
      IF (IP .GT. 0) THEN
        J = J + 1
        IF (J .LE. MXSTND) THEN
          MYLST2(J) = IP
C          IF(LKECHO)WRITE(JOPPRT,3826) KEYWRD, IP
          IF(LKECHO)WRITE(JOPPRT,3826) SUPLRECS, IP
 3826     FORMAT(T13,'STAND ID: ',A26,'   position ',I4)
        ENDIF
      ENDIF
      GOTO 3821
 3831 CONTINUE

c     Apply the list to the global option list.
C
C     Change glaobal list code to 308 for SANI
C
 
      CALL GPADD (KODE, IDT, 308, NPARMS, PRMS(1), J, MYLST2)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,3816) KEYWRD
 3816   FORMAT(/1X,A8,' No room to set option.')    
        GOTO 3841
      ENDIF
C
C     End from SALVAGE
C     Comment out teh following (RNH MAy98)
C
C 
C      CALL GPNEW (KODE,IDT,308,NPARMS,PRMS)
C      IF (KODE.GT.0) THEN
C        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
C        IF(LKECHO)WRITE(JOPPRT,3906) KEYWRD
C 3906   FORMAT(/1X,A8,'WARNING: Unable to set options')    
C        GOTO 3960
C      ENDIF
C
C     End RNH
C
C     Comment out GPADSD (RNH, may98)
C
C      CALL GPADSD(MXSTND)
      CALL GPCLOS(308)
      
 3960 CONTINUE

C
C     Loop out for SANI (RNH, MAy98)
C     
 3841 CONTINUE
      GOTO 10
 4000 CONTINUE
C                        OPTION NUMBER 30 -- OWVALUES: Outside World
c      
C This keyword now sets all values of the outside world that can vary during a 
c simulation (such as BAhost, BAspecial, BAtotal, BKP, RV, #special). This may 
c also be used with the keyword WORLD which sets the type of outside world.
c If any fields are blank, the average initial conditions will be used.

      NPARMS = 6
      DO 4005 I= 1,NPARMS
C         Set PRMS to -1 to indicate that the user did not specify values.
         PRMS(I) = -1
 4005 CONTINUE
 
      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
                                         
c     total bkp
      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
c     basal area of host above min size      
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
c     basal area of special trees      
      IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
c     density of special trees
      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)
c     total basal area      
      IF (LNOTBK(6)) PRMS(5) = ARRAY(6)
c     rating value
      IF (LNOTBK(7)) PRMS(6) = ARRAY(7)

      CALL GPNEW (KODE,IDT,310,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,4006) KEYWRD
 4006   FORMAT(/1X,A8,'WARNING: Unable to set options')
        GOTO 4050
      ENDIF
      
      IF(LKECHO)WRITE(JOPPRT,4010) KEYWRD, IDT, (PRMS(I), I=1,6) 
 4010 FORMAT(/1X,A8,'   Outside world parameters will change in ',I5,
     >    ' BKP outside= ',F6.2,' sqft/acre.',/T13,
     >    'BA host= ',F7.2,' sqft/acre; Special trees: BA= ',F7.2,
     >    ' sqft/acre and density= ',F7.2,/T13,'trees/acre. Total BA= ',
     >      F7.2,' sqft/acre; Rating value= ',F5.2,/T13,'NOTE: ',
     >    '-1 means that landscape average or previous year value used')

      CALL GPADSD(MXSTND)
      CALL GPCLOS(310)
 4050 CONTINUE
      
      GOTO 10
 4100 CONTINUE
C                        OPTION NUMBER 31 -- OWTYPE (More outside world info)
c                                                                           
c This keyword sets whether the outside world should be used, the type of 
c outside world (floating or constant) and the % of the outside world that
c is non-stoackable.

      IF (LNOTBK(1) .AND. (IFIX(ARRAY(1)).EQ.0)) OUTOFF = .TRUE.
      IF (LNOTBK(2)) UFLOAT = IFIX(ARRAY(2))
      IF (LNOTBK(3)) STOCKO = 100.0 - ARRAY(3)
      IF (STOCKO .LT. 1.0) STOCKO = 1.0

      IF (.NOT. OUTOFF) THEN       
        IF (UFLOAT .EQ. -1) THEN
          IF(LKECHO)WRITE(JOPPRT,4110) KEYWRD, STOCKO
 4110     FORMAT(/1X,A8,'   Outside world values are always the ',
     >     'average of the landscape.  The outside is ',F4.0,
     >     '% stockable.')
        ELSE
          IF(LKECHO)WRITE(JOPPRT,4115) KEYWRD, STOCKO
 4115     FORMAT(/1X,A8,'   Outside world values are always ',
     >     'constant (users can change them).  The outside is ',F4.0,
     >     '% stockable.')
        ENDIF                                                             
        
C       Change STOCKO into a proportion because that is what is used later
        STOCKO = STOCKO / 100.0
      ELSE                                 
        IF(LKECHO)WRITE(JOPPRT,4120) KEYWRD
 4120   FORMAT(/1X,A8,'   The outside world is NOT being simulated.')
      ENDIF

      GOTO 10
 4200 CONTINUE
C                        OPTION NUMBER 32 -- OWIPSDV: More Outside World
c      
c     This keyword supplements OWVALUES. It is used only for Ips as a driving
c     variable, and not as a main beetle species.  Any blanks are assumed to
c     be landscape averages as with OWVALUES. Note that there are fewer 
c     fields since we don't want the user re-specifiying values that can't
c     differ (RV and total BA in the outside world).

      NPARMS = 4
      DO 4205 I= 1,NPARMS
C         Set PRMS to -1 to indicate that the user did not specify values.
         PRMS(I) = -1
 4205 CONTINUE
 
      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
                                         
      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
      IF (LNOTBK(4)) PRMS(3) = ARRAY(4)
      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)

      CALL GPNEW (KODE,IDT,311,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,4206) KEYWRD
 4206   FORMAT(/1X,A8,'WARNING: Unable to set options')    
        GOTO 4250
      ENDIF
      
      IF(LKECHO)WRITE(JOPPRT,4210) KEYWRD, IDT, (PRMS(I), I=1,6) 
 4210 FORMAT(/1X,A8,'   Ips outside world parameters will change in ',
     &   I5,' BKP outside= ',F6.2,' sqft/acre; BA host= ',F7.2,
     &   ' sqft/acre;',/T13,'Special trees: BA= ',F7.2,
     &   ' sqft/acre and density= ',F7.2,' trees/acre.',/T13,'NOTE: ',
     &   '-1 means that landscape average or previous year value used')
     
      IF (.NOT. IPSON) WRITE(JOPPRT,4211) KEYWRD
 4211 FORMAT(/1X,A8,'   WARNING: Ips may not be on as a driving ',
     >     'variable. Requested options will not be used.')
     
      CALL GPADSD(MXSTND)
      CALL GPCLOS(311)
 4250 CONTINUE
      
      GOTO 10
 4300 CONTINUE
C                        OPTION NUMBER 33 -- QFIRE: Fire model.
c      

      NPARMS = 2
      PRMS(1)= DEFYRS
      PRMS(2)= 3.0
      
      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
                                         
      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
      PRMS(2) = AMIN1(PRMS(2),4.0)
      PRMS(2) = AMAX1(PRMS(2),1.0)

      CALL GPNEW (KODE,IDT,313,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,4306) KEYWRD
 4306   FORMAT(/1X,A8,'WARNING: Unable to set options')    
        GOTO 4350
      ENDIF
      
      IF(LKECHO)WRITE(JOPPRT,4310) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
 4310 FORMAT(/1X,A8,'   Fire will be active in year/cycle ',I5,
     &  ' with a period of ',I4,' years, using fuel ',
     &  ' moisture type ',I4)

     
      CALL GPADSD(MXSTND)
      CALL GPCLOS(313)
 4350 CONTINUE
      
      GOTO 10
 4400 CONTINUE
C                        OPTION NUMBER 34 -- QDEFOL: Defoliator model
c

      NPARMS = 4
      PRMS(1)= DEFYRS
      PRMS(2)= 0.25
      PRMS(3)= 0.0
      PRMS(4)= 0.0 
      TEMP= 8.0
      TEMP1= 30.0
              
      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
                                         
      IF (LNOTBK(2)) PRMS(1) = ARRAY(2)
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)                      
      PRMS(2) = AMIN1(PRMS(2),1.0)
      PRMS(2) = AMAX1(PRMS(2),0.0)
      
      IF (LNOTBK(4)) TEMP= ARRAY(4)
      IF (LNOTBK(5)) TEMP1= ARRAY(5)

      ISIZ= 0
 4410 CONTINUE
      IF (((PRMS(3) .EQ. 0.0) .OR. (PRMS(4) .EQ. 0.0))
     >                         .AND. (ISIZ .LT. NSCL)) THEN
         ISIZ= ISIZ + 1
         IF (TEMP .LE. UPSIZ(ISIZ) .AND. PRMS(3) .EQ. 0.0) 
     >                PRMS(3)= ISIZ
         IF (TEMP1 .LE. UPSIZ(ISIZ) .AND. PRMS(4) .EQ. 0.0)
     >                PRMS(4)= ISIZ
         GOTO 4410
      ELSE
         IF (PRMS(3) .EQ. 0.0) PRMS(3)= NSCL
         IF (PRMS(4) .EQ. 0.0) PRMS(4)= NSCL
         GOTO 4415
      ENDIF       
      
 4415 CONTINUE


      CALL GPNEW (KODE,IDT,314,NPARMS,PRMS)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,4406) KEYWRD
 4406   FORMAT(/1X,A8,'WARNING: Unable to set options')
        GOTO 4450
      ENDIF

      IF(LKECHO)WRITE(JOPPRT,4430) KEYWRD,IDT,INT(PRMS(1)),PRMS(2),
     &                               INT(PRMS(3)),INT(PRMS(4))
 4430 FORMAT(/1X,A8,'   DEFOLIATORS WILL BE ACTIVE IN YEAR/CYCLE',I5,
     &   ' FOR ',I4,' YEARS.',F6.2,' PROPORTION OF HOST TREES BETWEEN '
     &   /T13,'SIZE CLASS',I3,' AND',I3,' ARE MORE THAN 70% AFFECTED.')
     

      CALL GPADSD(MXSTND)
      CALL GPCLOS(314)
 4450 CONTINUE

      GOTO 10

 4500 CONTINUE
C                        OPTION NUMBER 35 -- SLASHMGT
c
c     Remove NEW downed dead wood in specified stands in specified years.

      IF (NOSTND .EQ. 0) THEN
        WRITE(JOPPRT,4504) KEYWRD
 4504   FORMAT(/1X,A8,' Stands not yet read; ',
     >    'Keyword MUST follow inventory.')    
        GOTO 4540
      ENDIF

      IDT= 1
      IUNIT=IREAD
      NPARMS= 2
      PRMS(1)= 0.0
      PRMS(2)= 0.8
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
      IF (LNOTBK(2)) IUNIT=IFIX(ARRAY(2))
      IF (LNOTBK(3)) PRMS(1)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(2)= ARRAY(4)
      PRMS(1) = AMIN1(PRMS(1),1.0)
      PRMS(1) = AMAX1(PRMS(1),0.0)
      PRMS(2) = AMIN1(PRMS(2),1.0)
      PRMS(2) = AMAX1(PRMS(2),0.0)

      IF(LKECHO)WRITE(JOPPRT,4510) KEYWRD, IDT, PRMS(1),PRMS(2),IUNIT
 4510 FORMAT(/1X,A8,'    Fresh slash to be removed in year', I5,
     >  '; Prop slash < 3 in diam. to remove =',F5.3,/T13,
     >  'Prop. >= 3in diam. =',F5.3,' Stands read from logical ',
     >  'unit ',I4)

      CALL C26SRT (NOSTND,STDIDS,MYLST1,.TRUE.)

      J = 0
 4520 CONTINUE
c
C      READ(IUNIT,'(A8)',END=4530) KEYWRD
C      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 4530
C      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
C      IF (IP .GT. 0) THEN
C        J = J + 1
C        IF (J .LE. MXSTND) THEN
C          MYLST2(J) = IP
C          IF(LKECHO)WRITE(JOPPRT,4525) KEYWRD, IP
C 4525     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)
C
C...Fix for 26-character supplemental records.  AJM 3/28/00.
************************************************************
      
      READ(IUNIT,*,END=4530) SUPLRECS
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 4520
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 4530
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
      CALL C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
      IF (IP .GT. 0) THEN
        J = J + 1
        IF (J .LE. MXSTND) THEN
          MYLST2(J) = IP
          IF(LKECHO)WRITE(JOPPRT,4525) SUPLRECS, IP
 4525     FORMAT(T13,'STAND ID: ',A26,'   position ',I4)
        ENDIF
      ENDIF
      GOTO 4520
 4530 CONTINUE

c     Apply the list to the global option list.

      CALL GPADD (KODE, IDT, 315, NPARMS, PRMS(1), J, MYLST2)
      IF (KODE.GT.0) THEN
        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE(JOPPRT,4505) KEYWRD
 4505   FORMAT(/1X,A8,' No room to set option.')
        GOTO 4540
      ENDIF

      CALL GPCLOS(315)

 4540 CONTINUE

      GOTO 10
C                        OPTION NUMBER 36 -- NONSTOCK (nonstockable stands)

 4600 CONTINUE

      IUNIT=IREAD
      IF (LNOTBK(1)) IUNIT=IFIX(ARRAY(1))

      IF(LKECHO)WRITE(JOPPRT,4610) KEYWRD, IUNIT
 4610 FORMAT(/1X,A8,' Nonstockable stands read from logical unit ',I4)

      NSSTND = 0
************************************************
C 4620 CONTINUE
C      READ(IUNIT,'(A8)',END=4630) KEYWRD
C      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
C      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 4630
C      NSSTND = NSSTND + 1
C      IF (NSSTND .LE. MXSTND) THEN
C        NSSTDS(NSSTND) = KEYWRD
C        IF(LKECHO)WRITE(JOPPRT,4625) KEYWRD
C 4625   FORMAT(T13,'NONSTOCKABLE STAND ID: ',A8)
C
C...Fix for 26-character supplemental records.  AJM 3/28/00
*************************************************
 4620 CONTINUE
      READ(IUNIT,*,END=4630) SUPLRECS
      IF (SUPLRECS(1:1) .EQ. '*') GOTO 4620
      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
      IF (INDEX(SUPLRECS,'-999') .GT. 0) GOTO 4630
      NSSTND = NSSTND + 1
      IF (NSSTND .LE. MXSTND) THEN
        NSSTDS(NSSTND) = SUPLRECS
        IF(LKECHO)WRITE(JOPPRT,4625) SUPLRECS
 4625   FORMAT(T13,'NONSTOCKABLE STAND ID: ',A26)
      ENDIF
      GOTO 4620
 4630 CONTINUE

      GOTO 10

 4700 CONTINUE
C                        OPTION NUMBER 37 -- BMDAMAGE
C
C     This controls the number of years prior to the first master cycle, from which
C     any inventoried attack of MPB will be included in the initial conditions for
C     the dispersal. If the keyword is ABSENT, inventory will *not* be used. If it
C     is present with no arguments, the default value of 2 years will be applied. If
C     there is an argument in field 1, that number will be applied.


      LINVON = .TRUE.
      IF (LNOTBK(1)) DEDYRS = MAX(0, INT(ARRAY(1)))

      IF(LKECHO)WRITE(JOPPRT,4710) KEYWRD, DEDYRS
 4710 FORMAT(/1X,A8,' Inventoried beetle attacks (bole only) will',
     >       ' be included when',/T13, 'inventory is within ',
     >       I4,' years of the first master cycle year.')

      GOTO 10

 4800 CONTINUE
C                  OPTION NUMBER 38 -- BMFDBK (Beetle Negative Feedback) MJO May98
C
C     This keyword parameterizes the functions in the BMFDBK.F subroutine, which
C     monitors the landscape-level beetle population and applies a negative feedback
C     event when triggered.

      TFDBK = 50.0
C     Added the flag and 6th keyword field below (MJO July98).
      LFDBK = .TRUE.

      IF (LNOTBK(1)) TFDBK = ARRAY(1)

      IF(LKECHO)WRITE(JOPPRT,4810) KEYWRD
 4810 FORMAT (/1X,A8,'   NEGATIVE FEEDBACK INITIALIZED:')
      IF(LKECHO)WRITE(JOPPRT,4820) TFDBK
 4820 FORMAT ('               Mean stand-level BKP to trigger feedback =
     &',F7.3)
c      IF(LKECHO)WRITE(JOPPRT,4830) SFDBK
c 4830 FORMAT ('               Feedback severity = ',F5.3)
c      IF(LKECHO)WRITE(JOPPRT,4840) LSBKPA,LSBKPB
c 4840 FORMAT ('               BKP(t-1) = ',F6.3,' and BKP(t-2) = ',F6.3)
c      IF(LKECHO)WRITE(JOPPRT,4850) LAG
c 4850 FORMAT ('               Feedback Lag = ',I1,' year(s)')

      GOTO 10
**************************************************************************
C ORIGINAL LANDSCAPE-LEVEL CODE BELOW...SAVED FOR POSTERITY
C      TFDBK = 0.3
C      SFDBK = 1.0
C      LSBKPA = 0.2
C      LSBKPB = 0.1
C      LAG = 1
C     Added the flag and 6th keyword field below (MJO July98).
C      LFDBK = .TRUE.

C      IF (LNOTBK(1)) TFDBK = ARRAY(1)
C      IF (LNOTBK(2)) SFDBK = ARRAY(2)
C      IF (LNOTBK(3)) LSBKPA = ARRAY(3)
C      IF (LNOTBK(4)) LSBKPB = ARRAY(4)
C      IF (LNOTBK(5)) LAG = ARRAY(5)

C      IF(LKECHO)WRITE(JOPPRT,4810) KEYWRD
C 4810 FORMAT (/1X,A8,'   NEGATIVE FEEDBACK INITIALIZED:')
C      IF(LKECHO)WRITE(JOPPRT,4820) TFDBK
C 4820 FORMAT ('               Mean landscape BKP to trigger feedback =
C     &',F7.3)
C      IF(LKECHO)WRITE(JOPPRT,4830) SFDBK
C 4830 FORMAT ('               Feedback severity = ',F5.3)
C      IF(LKECHO)WRITE(JOPPRT,4840) LSBKPA,LSBKPB
C 4840 FORMAT ('               BKP(t-1) = ',F6.3,' and BKP(t-2) = ',F6.3)
C      IF(LKECHO)WRITE(JOPPRT,4850) LAG
C 4850 FORMAT ('               Feedback Lag = ',I1,' year(s)')

C      GOTO 10

 4900 CONTINUE
C
C  ==========  OPTION NUMBER 39: COMMENT ==============================
C
C     COMMENT KEYWORD.  THIS ENABLES THE USER TO INCLUDE 
C     A DESCRIPTIONS WITHIN THE KEYWORD BLOCK.
C
      IF(LKECHO)WRITE(JOPPRT,4901) KEYWRD
 4901 FORMAT (/1X,A8)

C.... READ IN COMMENT STATEMENTS UNTIL 'END' IS ENCOUNTERED

 4902 CONTINUE
      READ (IREAD,4903,END=10) RECORD
 4903 FORMAT (A80)
      IRECNT = IRECNT + 1
      C4TMP = RECORD(1:4)
      CALL UPCASE (C4TMP(1:1))
      CALL UPCASE (C4TMP(2:2))
      CALL UPCASE (C4TMP(3:3))
      IF(C4TMP .EQ. 'END ') THEN
         IF(LKECHO)WRITE(JOPPRT,4905) RECORD(1:4)
 4905    FORMAT (/' ',A4)
         GOTO 10
      ELSE
 4904    IF(LKECHO)WRITE(JOPPRT,4906) RECORD
 4906    FORMAT (T13,A80)
      ENDIF
      GOTO 4902
C
C******************************************************************************
C
 5000 CONTINUE
C                        OPTION NUMBER 40 -- BMOUT (REPLACES BMTOTALS)
C
C     IF FIELD ONE IS BLANK, NAME & OPEN THE CYCLE OUTPUT FILE (*.bmc)

      IF (.NOT. LNOTBK(1).OR.ARRAY(1).EQ.0) THEN
         LBMCYC = .TRUE.          
C
         OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmc'
C
         IF(LKECHO)WRITE(JOPPRT,5010) KEYWRD,OUTNAM,JBMCYC
 5010    FORMAT (/1X,A8,'   WWPB MODEL "CYCLE" OUTPUT DATA WILL BE ',
     >     'WRITTEN AT CYCLE BOUNDARIES TO FILE: ',A,/
     >  T12,' (DATA SET REFERENCE NUMBER:',I4,')')
C
         CALL MYOPEN (JBMCYC,OUTNAM,1,133,0,1,1,0,KODE)
         IF (KODE.GT.0) THEN
            WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
         ENDIF
      ENDIF 
C
C-----------
C IF FIELD 2 IS BLANK, NAME & OPEN THE LANDSCAPE PER YR OUTPUT FILE (*.bml)

      IF (.NOT. LNOTBK(2).OR.ARRAY(2).EQ.0) THEN
         LBMLPY = .TRUE.
C
         OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bml'
C
         IF(LKECHO)WRITE(JOPPRT,5020) KEYWRD,OUTNAM,JBMLPY
 5020    FORMAT (/1X,A8,'   WWPB MODEL LANDSCAPE AVERAGE OUTPUT ',
     >       'STATISTICS WILL BE WRITTEN ANNUALLY TO FILE: ',A,/
     >    T12,' (DATA SET REFERENCE NUMBER:',I4,')')
C
         CALL MYOPEN (JBMLPY,OUTNAM,1,133,0,1,1,0,KODE)
         IF (KODE.GT.0) THEN
            WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
         ENDIF
      ENDIF

      GOTO 10
C
C******************************************************************************
C
C AFTER END KEYWORD, EXIT

  900 RETURN

C.... Special entry to retrieve keywords.
c commenting out ajm 8/05.
c this is being moved to the new sytand-level option processor BMIN.
c We do not need this here anyway, hence our ability to "co-opt" it , rather than duplicate it.

C      ENTRY BMKEY (KEY,PASKEY)
C      PASKEY= TABLE(KEY)
C      RETURN

      END
