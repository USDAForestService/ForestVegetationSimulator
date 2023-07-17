      SUBROUTINE BRIN(PASKEY,ARRAY,LNOTBK,LKECHO)
      IMPLICIT NONE
C THIS IS JUST A TEST FILE UPDATE FOR GIT REPO PULL REQUEST.
C GO AHEAD AND REJECT TO THE ETHER.
C----------
C WPBR $Id$
C----------
C  Purpose:
C     This subroutine reads the White Pine Blister Rust keywords.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  23-APR-1999 Lance David (FHTET)
C     Changed code to eliminate floating-point comparison warnings from
C     FORTRAN 90 compiler (Lahey LF90).
C     Added automatic scheduling of pruning when excising is scheduled.
C  18-MAY-1999 Lance David (FHTET)
C     Added debug code. Corrected IF construct in keyword INACT
C     processing because it only allowed one canker inactivation rate
C     to be changed at a time.
C  06-SEP-00 Lance R. David (FHTET)
C     Added use of FVS subroutine UPCASE to eliminate case sensitivity in
C     COMMENT's "END" keyword processing.
C  14-DEC-00 Lance R. David (FHTET)
C     Date (field 1) on prune keyword set to cycle 1 if blank or zero.
C  09-JAN-01 Lance R. David (FHTET)
C     RUSTINDX field 2 - Stand Deviation Factor will be calculated if 
C     blank or 0.0, values > 0.0 will set static deviation factor.
C  28-FEB-01 Lance R. David (FHTET)
C     Deactivated automatic scheduling of prune activity with EXCISE
C     keyword processing.
C     Due to problems experienced with other models, literal activity
C     codes have been changed to variable IACT and number of parameters
C     to variable NP in calls to option processor routines.
C  06-MAR-01 Lance R. David (FHTET)
C     Added pathological pruning option (field 5)to prune keyword.
C     Added GROWRATE keyword for branch and bole canker growth rates.
C  14-MAR-01 Lance R. David (FHTET)
C     Added DEVFACT keyword (activity code 1010) processing.
C     Field 2 of the RUSTINDX keyword the original single deviation
C     factor value and was deactivated.
C  27-MAR-01 Lance R. David (FHTET)
C     Added entry validation for GROWRATE and DEVFACT keywords.
C  24-APR-01 Lance R. David (FHTET)
C     Added species dimension for PRPSTK and RESIST arrays in STOCK
C     keyword processing.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C  16-MAY-2001 Lance R. David (FHTET)
C     Expanded canker growth rate variables by species and stock type
C     in keyword GROWRATE.
C  23-MAY-2001 Lance R. David (FHTET)
C     File names for canker data, BR canker list, and BR tree list
C     default to same name as FVS main keyword file with extensions
C     of .can, .brc and .brt respectively.
C  06-NOV-2002 Lance R. David (FHTET)
C     Added options to RUSTINDX keyword for use of new suboutine (BRICAL) 
C     and rust index equations. Added Rust index assignment methods 3&4
C     supplemental record for the parameters of the equations.
C  10-NOV-2003 - Lance R. David (FHTET)
C     Added LFLAG to KEYRDR call statement argument list.
C  15-MAY-2006 - Lance R. David (FHTET)
C     Added BROUT keyword to control output written to standard FVS
C     output file.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'

C.... Local variable declarations.

      LOGICAL DEBUG,LOPEN,LREDF,LNOTBK(7),LALLSP,LALLST,LKECHO
      INTEGER I, I3, I4, I5, IACT, ICLS, IDT, IKEY, ISIZE, ITSP,
     &        J, KSP, KODE, LENTH, NP, NMBER
      REAL    ARRAY(7),PRMS(10),EXCESS,BOTMP,BRTMP,REDFAC
      CHARACTER*4  C4TMP
      CHARACTER*8  TABLE(20),KEYWRD,PASKEY
      CHARACTER*10 KARD(7)
      CHARACTER*80 RECORD,DNAME,TNAME,CNAME

C.... Data statements.

      DATA ISIZE/20/

      DATA TABLE/'PRUNE','PRNSPECS','EXCISE','EXSPECS','RIBES','INACT',
     &   'END','COMMENT','GROWRATE','BRTLST','BRCLST','CANFMT',
     &   'DEVFACT','RUSTINDX','BRSEED','STOCK','CANKDATA','BROUT',
     &   ' ',' '/

C.... Check for debug.

      CALL DBCHK(DEBUG,'BRIN',4,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC,PASKEY,ARRAY,TABLE
   10 FORMAT('Begin BRIN: Cycle= ',I5,/,' PASKEY=',A8,/,
     &' ARRAY=',7F7.3,/,' TABLE=',20A8)

C.... Initializations.

      BRYES=.TRUE.
      TNAME=' '
      CNAME=' '
      DNAME=' '

C.... Load the passed keyword into KEYWRD and branch to the FNDKEY
C.... rountine.

      KEYWRD=PASKEY

C.... Top of the loop to read keywords.

   90 CONTINUE

C.... Read keyword from input keyword file.

      CALL KEYRDR(IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,ARRAY,IRECNT,
     &   KODE,KARD,LFLAG,LKECHO)

C.... Process keyword errors.
C.... Return codes 0 = no error, 1 = column 1 blank, 2 = EOF

      IF(KODE.NE.0) THEN
         IF(KODE.EQ.2) CALL ERRGRO(.FALSE.,2)
         CALL ERRGRO(.TRUE.,6)
         GO TO 90
      ENDIF

C.... Find the keyword.

      CALL FNDKEY(NMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

C.... Return codes 0 = no error, 1 = keyword not found.

      IF(KODE.NE.0) THEN
         CALL ERRGRO(.TRUE.,1)
         GO TO 90
      ENDIF

C.... Process the keyword.

      GO TO (100,200,300,400,500,600,700,800,900,1000,
     &   1100,1200,1300,1400,1500,1600,1700,1800),NMBER

 100  CONTINUE

C.... ==========  Option Number 1: PRUNE ===========================

C.... This keyword triggers a pruning management activity in the
C.... year specified.
C.... Field 1 - year/cycle
C.... Field 2 - success rate of pruning
C.... Field 3 - prune infected prunable trees
C.... Field 4 - prune uninfected/clean trees
C.... Field 5 - pathological pruning flag,
C....           0 = (default) change crown base height when pruned
C....           1 = do not change crown base height when pruned

      IDT=1
      IF(LNOTBK(1) .AND. ARRAY(1) .GT. 0.0) IDT=INT(ARRAY(1))
      PRMS(1)=SRATE(1)
      IF(LNOTBK(2)) PRMS(1)=ARRAY(2)
      PRMS(2)=ARRAY(3)
      PRMS(3)=ARRAY(4)
      IF(LNOTBK(5) .AND. ARRAY(5) .NE. 0) THEN
C....    A non-zero entry in field 5 indicates pathological pruning.
         PRMS(4)=1
      ELSE
         PRMS(4)=0
      ENDIF

C.... If field 3 is 1 then pruning will be done on prunable trees.
C.... If field 4 is 1 then pruning will be done on clean trees.
C.... If neither field is 1 then an error message is printed and
C.... pruning will not be done.

      IF(INT(ARRAY(3)).EQ.0 .AND. INT(ARRAY(4)).EQ.0) THEN
         WRITE(JOSTND,101)
 101     FORMAT(/,'**** WARNING **** PRUNE ACTIVITY HAS BEEN ',
     &     'SPECIFIED BUT NEITHER PRUNABLE TREES NOR CLEAN TREES '/,
     &     '**** WARNING **** HAVE BEEN TARGETED FOR TREATMENT. ',
     &     'PRUNING WILL NOT OCCUR.')
         GO TO 90
      ENDIF

      IACT = 1001
      NP = 4
      CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
      IF(KODE.GT.0) GO TO 90

C.... Output for PRUNE keyword.

      IF(LKECHO)WRITE(JOSTND,110) KEYWRD,IDT,PRMS(1)
  110 FORMAT(/,A8,'   PRUNING SCHEDULED IN CYCLE/YEAR ',I4,
     &  ';  SUCCESS RATE = ',F6.2,';')
      IF(INT(ARRAY(3)).EQ.1 .AND. INT(ARRAY(4)).EQ.1) THEN
         WRITE (JOSTND,111)
  111    FORMAT(T12,'BOTH PRUNABLE INFECTED AND CLEAN TREES HAVE',
     &     ' BEEN TARGETED FOR PRUNING')
      ELSE IF(INT(ARRAY(3)).EQ.1) THEN
         IF(LKECHO)WRITE(JOSTND,112)
  112    FORMAT(T12,'PRUNABLE INFECTED TREES HAVE BEEN TARGETED',
     &     ' FOR PRUNING')
      ELSE IF(INT(ARRAY(4)).EQ.1) THEN
         IF(LKECHO)WRITE(JOSTND,113)
  113    FORMAT(T12,'CLEAN TREES HAVE BEEN TARGETED FOR PRUNING')
      ENDIF

      IF(INT(PRMS(4)) .EQ. 1) THEN
         IF(LKECHO)WRITE(JOSTND,114)
  114    FORMAT(T12,'PATHOLOGICAL PRUNING IS SPECIFIED; THEREFOR,',/,
     &     T12,'CROWN BASE HEIGHTS WILL NOT BE CHANGED WHEN PRUNED.')
      ELSE
         IF(LKECHO)WRITE(JOSTND,115)
  115    FORMAT(T12,
     &     'CROWN BASE HEIGHTS WILL BE INCREASED ON PRUNED TREES.')
      ENDIF
      GO TO 90

  200 CONTINUE

C.... ==========  Option Number 2: PRNSPECS  ========================

C.... This keyword is used to change the default values for defining
C.... prunable cankers that would qualify for removal when the prune
C.... keyword is specified.
C.... Date/Cycle 0 changes values, but does not schedule an activity
C.... with the option processor.
C.... Field 3 is provided in feet and converted to centimeters for
C.... processing.
C.... Fields 4 and 5 are provided in inches and converted to
C.... centimeters for processing.
C.... The parameters are stored in the event monitor in feet and inches
C.... and will be converted to centimeters when the activity occurs.

      IDT=0
      IF(LNOTBK(1)) IDT=INT(ARRAY(1))
      PRMS(1)=HTPRPR
      PRMS(2)=HTMAX(1)/30.48
      PRMS(3)=OUTDST/2.54
      PRMS(4)=OUTNLD/2.54

      IF(IDT.EQ.0) THEN
         IF(LNOTBK(2)) THEN
            HTPRPR=ARRAY(2)
            PRMS(1)=ARRAY(2)
         ENDIF
         IF(LNOTBK(3))THEN
            HTMAX(1)=ARRAY(3)*30.48
            PRMS(2)=ARRAY(3)
         ENDIF
         IF(LNOTBK(4)) THEN
            OUTDST=ARRAY(4)*2.54
            PRMS(3)=ARRAY(4)
         ENDIF
         IF(LNOTBK(5)) THEN
            OUTNLD=ARRAY(5)*2.54
            PRMS(4)=ARRAY(5)
         ENDIF
      ELSE
         IF(LNOTBK(2)) PRMS(1)=ARRAY(2)
         IF(LNOTBK(3)) PRMS(2)=ARRAY(3)
         IF(LNOTBK(4)) PRMS(3)=ARRAY(4)
         IF(LNOTBK(5)) PRMS(4)=ARRAY(5)
         IACT = 1002
         NP = 4
         CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
         IF(KODE.GT.0) GO TO 90
      ENDIF

C.... Output for PRNSPECS keyword.

      IF(LKECHO)WRITE(JOSTND,210) KEYWRD,IDT,(PRMS(I),I=1,4)
  210 FORMAT(/,A8,'   PRUNE PARAMETERS CHANGED IN DATE/CYCLE ',I4,
     &   ';  MAXIMUM PRUNABLE HEIGHT AS A PROPORTION OF TOTAL',
     &   ' TREE HEIGHT = ',F4.2,';',/T12,'MAXIMUM ABSOLUTE PRUNE',
     &   ' HEIGHT = ',F5.1,' FEET;  MINIMUM DISTANCE OUT FOR PRUNABLE',
     &   ' CANKER = ',F4.1,' INCHES;',/T12,'MINIMUM DISTANCE OUT FOR',
     &   ' NON-LETHAL CANKER = ',F4.1,' INCHES')
      GO TO 90

  300 CONTINUE

C.... ==========  Option Number 3: EXCISE  ===========================

C.... This keyword triggers an excise management activity in the year
C.... specified.

      IDT=1
      IF(LNOTBK(1)) IDT=INT(ARRAY(1))
      PRMS(1)=SRATE(2)
      IF(LNOTBK(2)) PRMS(1)=ARRAY(2)

      IACT = 1003
      NP = 1
      CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
      IF(KODE.GT.0) GO TO 90

C.... Output for EXCISE keyword.

      IF(LKECHO)WRITE(JOSTND,310) KEYWRD,IDT,PRMS(1)
  310 FORMAT(/,A8,'   EXCISING SCHEDULED IN DATE/CYCLE ',I4,
     &   ';  SUCCESS RATE = ',F4.2)

C.... Automatic PRUNE activity scheduling.
C.... When the EXCISE activity is scheduled, PRUNE on prunable trees
C.... only will also be scheduled for the same date.
C.... IDT     date remains same as EXCISE date.
C.... PRMS(1) is success rate of pruning.
C.... PRMS(2) set to 1 to target prunable trees.
C.... PRMS(3) set to 0 to not prune clean (uninfected) trees.
C
C     The automatic scheduling has been deactivated per work session
C     with Geral McDonald and John Schwandt 12-DEC-00.
C     Lance David 28-FEB-01
C
C     KEYWRD=TABLE(1)
C     PRMS(1)=SRATE(1)
C     PRMS(2)=1
C     PRMS(3)=0
C
C     IACT = 1001
C     NP = 3
C     CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
C     IF(KODE.GT.0) GO TO 90
C
C.... Output for EXCISE initiated PRUNE keyword.
C
C     WRITE(JOSTND,320) KEYWRD,IDT,PRMS(1)
C 320 FORMAT(/,A8,'   EXCISE INITIATED PRUNING SCHEDULED IN CYCLE',
C    &       '/YEAR ',I4,';  SUCCESS RATE = ',F6.2,';',/,
C    &       T12,'ONLY PRUNABLE TREES HAVE BEEN TARGETED FOR PRUNING')

      GO TO 90

  400 CONTINUE

C.... ==========  Option Number 4: EXSPECS  ==========================

C.... This keyword changes the default values which define how status
C.... code 3 (excisable) is assigned to a canker, which causes the
C.... canker to be removed when a excise activity is performed.
C.... Date/Cycle 0 changes values, but does not schedule an activity
C.... with the option processor.
C.... Field 3 is provided in feet and fields 2 and 6 are in inches; all
C.... are converted to centimeters for processing.
C.... The parameters are stored in the event monitor in feet and inches
C.... and will be converted to centimeters when the activity occurs.

      IDT=0
      IF(LNOTBK(1)) IDT=INT(ARRAY(1))
      PRMS(1)=EXDMIN/2.54
      PRMS(2)=HTMAX(2)/30.48
      PRMS(3)=GIRMAX
      PRMS(4)=GIRMRT
      PRMS(5)=HTMIN/2.54

      IF(IDT.EQ.0) THEN
         IF(LNOTBK(2)) THEN
            EXDMIN=ARRAY(2)*2.54
            PRMS(1)=ARRAY(2)
         ENDIF
         IF(LNOTBK(3)) THEN
            HTMAX(2)=ARRAY(3)*30.48
            PRMS(2)=ARRAY(3)
         ENDIF
         IF(LNOTBK(4)) THEN
            GIRMAX=ARRAY(4)
            PRMS(3)=ARRAY(4)
         ENDIF
         IF(LNOTBK(5)) THEN
            GIRMRT=ARRAY(5)
            PRMS(4)=ARRAY(5)
         ENDIF
         IF(LNOTBK(6)) THEN
            HTMIN=ARRAY(6)*2.54
            PRMS(5)=ARRAY(6)
         ENDIF
      ELSE
         IF(LNOTBK(2)) PRMS(1)=ARRAY(2)
         IF(LNOTBK(3)) PRMS(2)=ARRAY(3)
         IF(LNOTBK(4)) PRMS(3)=ARRAY(4)
         IF(LNOTBK(5)) PRMS(4)=ARRAY(5)
         IF(LNOTBK(6)) PRMS(5)=ARRAY(6)
         IACT = 1004
         NP = 5
         CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
         IF(KODE.GT.0) GO TO 90
      ENDIF

C.... Output for EXSPECS keyword.

      IF(LKECHO)WRITE(JOSTND,410) KEYWRD,IDT,(PRMS(I),I=1,5)
  410 FORMAT(/,A8,'   EXCISE PARAMETERS CHANGED IN DATE/CYCLE ',I4,
     &  ';  MINIMUM TREE DBH NECESSARY FOR EXCISING = ',
     &  F5.1,' INCHES;',/T12,'MAXIMUM ABSOLUTE EXCISE HEIGHT = ',
     &  F5.1,' FEET;  MAXIMUM PERCENT GIRDLE = ',F5.1,'%;',/T12,
     &  'PERCENT GIRDLING WHICH CAUSES MORTALITY OR TOP KILL = ',F5.1,
     &  '%;'/T12,'MINIMUM BOLE CANKER HEIGHT NECESSARY FOR EXCISING = '
     &  ,F5.1,' INCHES')
      GO TO 90

  500 CONTINUE

C.... ==========  Option Number 5: RIBES =============================

C.... This keyword keyword changes the population values for
C.... Ribes H, L and V modifies the stand rust index.  The values
C.... provided for each ribes species is old number of bushes per acre,
C.... and new number of bushes per acre.
C.... This keyword functions in two ways:
C.... 1- as an activity when date (field 1) is > 0;
C....    Fields 1-6 values are used to calculate rust indexes based on
C....    old and new ribes populations. If both old and new population
C....    values are present, the reduction factor is calculated.
C....    If only new population values are present, the rust index is
C....    calculated.
C.... 2- calculates rust index from ribes populations when date = 0;
C....    Fields 3, 5 and 7 values (new bushes/acre) will be used to
C....    to calculate rust index directly.  No activity is scheduled.

      IDT=1
      IF(LNOTBK(1)) IDT=INT(ARRAY(1))
      IF(IDT.EQ.0) THEN

C....    Calculate stand rust index.

         IF(LNOTBK(2)) RIBUS(1,1) = ARRAY(2)
         IF(LNOTBK(3)) RIBUS(2,1) = ARRAY(3)
         IF(LNOTBK(4)) RIBUS(1,2) = ARRAY(4)
         IF(LNOTBK(5)) RIBUS(2,2) = ARRAY(5)
         IF(LNOTBK(6)) RIBUS(1,3) = ARRAY(6)
         IF(LNOTBK(7)) RIBUS(2,3) = ARRAY(7)

         CALL BRIBES(REDFAC,LREDF)

C....    Output for RIBES keyword when date/cycle = 0.

         IF(LKECHO)
     &   WRITE(JOSTND,510) KEYWRD,RIDEF,((RIBUS(I,J),I=2,2),J=1,3)
  510    FORMAT(/,A8,'   STAND RUST INDEX INITIALLY SET TO ',F7.5,';',
     &      /T12,'CALCULATED FROM NEW BUSHES PER ACRE VALUES SHOWN '
     &      'BELOW:',/T12,'RIBES H.   NEW BUSHES/ACRE = ',F6.1,';',
     &      /T12,'RIBES L.   NEW BUSHES/ACRE = ',F6.1,';',
     &      /T12,'RIBES V.   NEW BUSHES/ACRE = ',F6.1)
      ELSE

C....    Schedule an activity.

         PRMS(1)=RIBUS(1,1)
         PRMS(2)=RIBUS(2,1)
         PRMS(3)=RIBUS(1,2)
         PRMS(4)=RIBUS(2,2)
         PRMS(5)=RIBUS(1,3)
         PRMS(6)=RIBUS(2,3)

         IF(LNOTBK(2)) PRMS(1)=ARRAY(2)
         IF(LNOTBK(3)) PRMS(2)=ARRAY(3)
         IF(LNOTBK(4)) PRMS(3)=ARRAY(4)
         IF(LNOTBK(5)) PRMS(4)=ARRAY(5)
         IF(LNOTBK(6)) PRMS(5)=ARRAY(6)
         IF(LNOTBK(7)) PRMS(6)=ARRAY(7)
         IACT = 1005
         NP = 6
         CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
         IF(KODE.GT.0) GO TO 90

C....    Output for RIBES keyword when date/cycle not 0.

         IF(LKECHO)WRITE(JOSTND,530) KEYWRD,IDT,(PRMS(I),I=1,6)
  530    FORMAT(/,A8,'   RUST INDEX TO BE MODIFIED IN DATE/CYCLE ',
     &      I4,' BASED ON CHANGE IN NUMBER OF RIBES BUSHES;',
     &      /T12,'RIBES H.   OLD BUSHES/ACRE = ',F6.1,
     &      ';  NEW BUSHES/ACRE = ',F6.1,';',
     &      /T12,'RIBES L.   OLD BUSHES/ACRE = ',F6.1,
     &      ';  NEW BUSHES/ACRE = ',F6.1,';',
     &      /T12,'RIBES V.   OLD BUSHES/ACRE = ',F6.1,
     &      ';  NEW BUSHES/ACRE = ',F6.1)
      ENDIF
      GO TO 90

  600 CONTINUE

C.... ==========  Option Number 6: INACT ===========================

C.... Changes the default value for yearly canker inactivation rate.
C.... for branch and bole cankers. This is an activity.
C.... Date/Cycle 0 changes values, but does not schedule an activity
C.... with the option processor.

      IF(DEBUG) THEN
         WRITE(JOSTND,*) ' at INACT: ARRAY=',ARRAY
         WRITE(JOSTND,*) '          LNOTBK=',LNOTBK
         WRITE(JOSTND,*) '       RATINV(1)=',RATINV(1)
         WRITE(JOSTND,*) '       RATINV(2)=',RATINV(2)
      ENDIF

      IDT=0
      IF(LNOTBK(1)) IDT=INT(ARRAY(1))
      PRMS(1)=RATINV(1)
      PRMS(2)=RATINV(2)

      IF(IDT.EQ.0) THEN
         IF(LNOTBK(2)) THEN
            RATINV(1)=ARRAY(2)
            PRMS(1)=ARRAY(2)
         ENDIF 
         IF(LNOTBK(3)) THEN
            RATINV(2)=ARRAY(3)
            PRMS(2)=ARRAY(3)
         ENDIF
      ELSE
         IF(LNOTBK(2)) PRMS(1)=ARRAY(2)
         IF(LNOTBK(3)) PRMS(2)=ARRAY(3)
         IACT = 1006
         NP = 2
         CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
         IF(KODE.GT.0) GO TO 90
      ENDIF

C.... Output for INACT keyword.

      IF(LKECHO)WRITE(JOSTND, 610) KEYWRD,IDT,PRMS(1),PRMS(2)
  610 FORMAT(/,A8,'   CANKER INACTIVATION RATES TO BE CHANGED IN',
     &   ' DATE/CYCLE ',I4,';',/T12,'NEW VALUE FOR BRANCH CANKERS = ',
     &   F5.3,';  NEW VALUE FOR BOLE CANKERS = ',F5.3)
      GO TO 90

  700 CONTINUE

C.... ==========  Option Number 7: END =============================

C.... End of Blister Rust keywords.

      IF(LKECHO)WRITE(JOSTND,710) KEYWRD
  710 FORMAT(/,A8,'   END OF BLISTER RUST KEYWORDS')

C.... If Blister Rust canker and/or tree list requested, open files.

      IF(BRCL) THEN
         INQUIRE(UNIT=IDCOUT,OPENED=LOPEN)
         IF(LOPEN) CLOSE(UNIT=IDCOUT)
         CALL MYOPEN(IDCOUT,CNAME,1,132,0,1,1,0,KODE)
         IF(KODE.NE.0) THEN
            PRINT*,'*BRIN* Error opening filename: ',CNAME
            GO TO 9000
         ENDIF
      ENDIF

      IF(BRTL) THEN
         INQUIRE(UNIT=IDTOUT,OPENED=LOPEN)
         IF(LOPEN) CLOSE(UNIT=IDTOUT)
         CALL MYOPEN(IDTOUT,TNAME,1,132,0,1,1,0,KODE)
         IF(KODE.NE.0) THEN
            PRINT*,'*BRIN* Error opening filename: ',TNAME
            GO TO 9000
         ENDIF
      ENDIF

C.... Return.

      GO TO 9000

  800 CONTINUE

C.... ==========  Option Number 8: COMMENT =========================

C.... This keyword enables the user to include comment lines within the
C.... keyword list.  These comments are written with the standard
C.... keyword output at the beginning of the blister rust information.

      IF(LKECHO)WRITE(JOSTND,810) KEYWRD
  810 FORMAT(/,A8)

C.... Read comment statments until 'END' is encountered.

  802 CONTINUE
      READ(IREAD,820,END=8900) RECORD
  820 FORMAT(A80)
      IRECNT=IRECNT+1
      C4TMP = RECORD(1:4)
      CALL UPCASE (C4TMP(1:1))
      CALL UPCASE (C4TMP(2:2))
      CALL UPCASE (C4TMP(3:3))
      IF(C4TMP .EQ. 'END ') THEN
         IF(LKECHO)WRITE(JOSTND,815) RECORD(1:4)
  815    FORMAT(/,A4)
         GO TO 90
      ELSE
         IF(LKECHO)WRITE(JOSTND,830) RECORD
  830    FORMAT(T12,A80)
      ENDIF
      GO TO 802

  900 CONTINUE

C.... ==========  Option Number 9: GROWRATE =========================
C....
C.... Annual growth rates (cm) for branch and bole cankers.
C.... field 1 - FVS Tree species code
C.... field 2 - Stock type code
C.... field 3 - Bole canker growth rate
C.... field 4 - Branch canker growth rate
C....
C.... I3 is the FVS tree species code
C.... I4 is the BR host tree species code
C.... I5 is the BR host stock type

      IF(LKECHO)WRITE(JOSTND,910) KEYWRD
  910 FORMAT(/,A8,'   CHANGE ANNUAL GROWTH RATES FOR CANKERS.')
c    &  ,/,
c    &  T12,'FIELDS 1 AND 2 FOR SPECIES AND STOCK TYPE NOT YET ACTIVE,')

      I3 = 0
      I4 = 0
      BOTMP = 0.0
      BRTMP = 0.0
      LALLSP = .FALSE.
      LALLST = .FALSE.

      IF(LNOTBK(1) .AND. ARRAY(1) .NE. 0.0) THEN
         I3=INT(ARRAY(1))
         I4=BRSPM(I3)
      ENDIF

C.... Vaildate species code
      IF(I3 .LT. 0 .OR. I3 .GT. MAXSP) THEN
         WRITE(JOSTND,914) KEYWRD, I3
  914    FORMAT(/,A8,'   ***** INVALID SPECIES CODE ',I3,'. *****')
         GO TO 90
      ELSE IF(I4 .EQ. 0 .AND. I3 .NE. 0) THEN
         WRITE(JOSTND,915) KEYWRD, I3, NSP(I3,1)(1:2)
  915    FORMAT(/,A8,'   ***** NON-HOST SPECIES CODE ',
     &          I3,' - ',A,'. *****')
         GO TO 90
      ENDIF

C.... Vaildate stock type.
      I5=INT(ARRAY(2))
      IF(I5.LT. 0 .OR. I5 .GT. 4) THEN
         WRITE(JOSTND,917) KEYWRD, I5
  917    FORMAT(/,A8,'   ***** INVALID STOCK TYPE ',I1,'. *****')
         GO TO 90
        I5=INT(ARRAY(2))
      ENDIF

      IF(LNOTBK(3)) THEN
        IF(ARRAY(3).GE.0.1 .AND. ARRAY(3).LE.10.0) THEN
          BOTMP=ARRAY(3)
        ELSE
          WRITE(JOSTND,918)
  918     FORMAT(T12,'*** INVALID BOLE CANKER GROWTH RATE. VALID ',
     &    'RANGE IS 0.1 TO 10.0 -- DEFAULT VALUE(S) RETAINED. ***')
        ENDIF
      ENDIF

      IF(LNOTBK(4)) THEN
        IF(ARRAY(4).GE.0.1 .AND. ARRAY(4).LE.10.0) THEN
          BRTMP=ARRAY(4)
        ELSE
          WRITE(JOSTND,919)
  919     FORMAT(T12,'*** INVALID BRANCH CANKER GROWTH RATE. VALID ',
     &    'RANGE IS 0.1 TO 10.0 -- DEFAULT VALUE(S) RETAINED. ***')
        ENDIF
      ENDIF

      IF(I4 .EQ. 0) THEN
C....   All species will be changed
        LALLSP = .TRUE.
        IF(I5 .EQ. 0) THEN
C....     All stock types (and species) will be changed
          LALLST = .TRUE.
          DO I4 = 1, NBRSP
            DO I5 = 1, 4
              IF(BOTMP .NE. 0.0) BOGRTH(I4,I5)= BOTMP
              IF(BRTMP .NE. 0.0) BRGRTH(I4,I5)= BRTMP
            END DO
          END DO
        ELSE
C....     Change 1 stock type for all species
          DO I4 = 1, NBRSP
            IF(BOTMP .NE. 0.0) BOGRTH(I4,I5)= BOTMP
            IF(BRTMP .NE. 0.0) BRGRTH(I4,I5)= BRTMP
          END DO
        ENDIF
      ELSE
C....   Changing only 1 species
        IF(I5 .EQ. 0) THEN
          LALLST = .TRUE.
C....     Change all stock types for 1 species
          DO I5 = 1, 4
            IF(BOTMP .NE. 0.0) BOGRTH(I4,I5)= BOTMP
            IF(BRTMP .NE. 0.0) BRGRTH(I4,I5)= BRTMP
          END DO
        ELSE
C....     Changing 1 species, 1 stock type
          IF(BOTMP .NE. 0.0) BOGRTH(I4,I5)= BOTMP
          IF(BRTMP .NE. 0.0) BRGRTH(I4,I5)= BRTMP
        ENDIF
      ENDIF

      IF(LALLSP .AND. LALLST) THEN
        IF(LKECHO)WRITE(JOSTND,925)
      ELSE IF(LALLSP .AND. .NOT. LALLST) THEN
        IF(LKECHO)WRITE(JOSTND,926) I5
      ELSE IF(.NOT. LALLSP .AND. LALLST) THEN
        IF(LKECHO)WRITE(JOSTND,927) BRSPC(I4)
      ELSE
        IF(LKECHO)WRITE(JOSTND,928) BRSPC(I4), I5
      ENDIF

  925 FORMAT(T12,'ALL BLISTER RUST HOST SPECIES AND STOCK TYPES',
     &  ' CANKER GROWTH RATES SET AS FOLLOWS:')
  926 FORMAT(T12,'ALL BLISTER RUST HOST SPECIES STOCK TYPE ',I1,
     &  ' CANKER GROWTH RATES SET AS FOLLOWS:')
  927 FORMAT(T12,'ALL STOCK TYPES OF BLISTER RUST HOST SPECIES ',A3,
     &  ' CANKER GROWTH RATES SET AS FOLLOWS:')
  928 FORMAT(T12,'BLISTER RUST HOST SPECIES ',A3,' STOCK TYPE ',I1,
     &  ' CANKER GROWTH RATES SET AS FOLLOWS:')

C.... If all species and/or stock types were changed, indices need reset
C.... because do loops will have their values at 1 too high.
      IF(LALLSP) I4=NBRSP
      IF(LALLST) I5=4

C.... Now write canker growth rate values that have been set.
      IF(LKECHO)WRITE(JOSTND,930) BOGRTH(I4,I5),BRGRTH(I4,I5)
  930 FORMAT(T12,'BOLE CANKER WIDEST HORIZONTAL WIDTH GROWTH ',
     &  'RATE (CM) = ',F6.3,/,
     &  T12,'BRANCH CANKER RADIAL GROWTH RATE (CM) = ',F6.3)
      GO TO 90

 1000 CONTINUE

C.... ==========  Option Number 10:  BRTLST =========================

C.... Keyword to write list of trees affected by the WPBR Model to
C.... a file.  This is an activity and the default cycle is all
C.... cycles including cycle 0.  Default unit is 26 and the default
C.... file name is BRTOUT.
C.... Field 1 - date/cycle (0 - all cycles, otherwise cycle 0 and
C....                       the cycle specified will be written)
C.... Field 2 - unit number.
C.... Field 3 - if not blank, indicates that a file name is specified
C....           on a supplemental record.  The file name may be up to
C....           30 characters.
C....           If field 3 is blank, the file name will be the same
C....           as the main FVS keyword file with ".brt" extension.

      BRTL=.TRUE.
      IDT=0
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))
      IF(LNOTBK(2)) IDTOUT=IFIX(ARRAY(2))

      IF(LNOTBK(3)) THEN
         READ(IREAD,1010,END=8900) TNAME
 1010    FORMAT(A80)
      ELSE
C....    Load tname with the first 76 characters of the FVS keyword
C....    file name and concatenate .brt extension.
         TNAME=KWDFIL(1:76)//'.brt'
      ENDIF

C.... Remove blanks from file name.
      CALL UNBLNK(TNAME,LENTH)

      IACT = 1007
      NP = 0
      CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
      IF(KODE.GT.0) GO TO 90

C.... Output for BRTLST keyword.

      IF(IDT.EQ.0) THEN
         IF(LKECHO)WRITE(JOSTND,1020) KEYWRD,IDTOUT,TNAME
 1020    FORMAT(/,A8,'   BLISTER RUST TREE LIST WILL BE WRITTEN TO',
     &      ' UNIT NUMBER ',I2,' FOR ALL CYCLES;',
     &      /T12,'TREELIST FILENAME = ',A50)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1021) KEYWRD,IDTOUT,IDT,TNAME
 1021    FORMAT(/,A8,'   BLISTER RUST TREE LIST WILL BE WRITTEN TO',
     &      ' UNIT NUMBER ',I2,' FOR DATE/CYCLE ',I4,';',
     &      /T12,'TREELIST FILENAME = ',A50)
      ENDIF
      GO TO 90

 1100 CONTINUE

C.... ==========  Option Number 11: BRCLST ==========================

C.... Keyword to write list of cankers being tracked by WPBR Model to
C.... a file.  This is an activity and the default cycle is all
C.... cycles including cycle 0.  Default unit is 26 and the default
C.... file name is BRCOUT.
C.... Field 1 - date/cycle (0 - all cycles, otherwise cycle 0 and
C....                       the cycle specified will be written)
C.... Field 2 - unit number.
C.... Field 3 - if not blank, indicates that a file name is specified
C....           on a supplemental record.  The file name may be up to
C....           80 characters.
C....           If field 3 is blank, the file name will be the same
C....           as the main FVS keyword file with ".brc" extension.

      BRCL=.TRUE.
      IDT=0
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))
      IF(LNOTBK(2)) IDCOUT=IFIX(ARRAY(2))
      IF(LNOTBK(3)) THEN
         READ(IREAD,1110,END=8900) CNAME
 1110    FORMAT(A80)
      ELSE
C....    Load cname with the first 76 characters of the FVS keyword
C....    file name and concatenate .brc extension.
         CNAME=KWDFIL(1:76)//'.brc'
      ENDIF

C.... Remove blanks from file name.
      CALL UNBLNK(CNAME,LENTH)

      IACT = 1008
      NP = 0
      CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
      IF(KODE.GT.0) GO TO 90

C.... Output for BRCLST keyword.

      IF(IDT.EQ.0) THEN
         IF(LKECHO)WRITE(JOSTND,1120) KEYWRD,IDCOUT,CNAME
 1120    FORMAT(/,A8,'   BLISTER RUST CANKER LIST WILL BE WRITTEN TO',
     &      ' UNIT NUMBER ',I2,' FOR ALL CYCLES;',
     &      /T12,'CANKER LIST FILENAME = ',A50)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1121) KEYWRD,IDCOUT,IDT,CNAME
 1121    FORMAT(/,A8,'   BLISTER RUST CANKER LIST WILL BE WRITTEN TO',
     &      ' UNIT NUMBER ',I2,' FOR DATE/CYCLE ',I4,';',
     &      /T12,'CANKER LIST FILENAME = ',A50)
      ENDIF
      GO TO 90

 1200 CONTINUE

C.... ==========  Option Number 12: CANFMT ==========================

C.... Keyword to change the default format for reading canker data.

      IF(LKECHO)WRITE(JOSTND,1210) KEYWRD
 1210 FORMAT(/,A8,'   THE FORMAT BELOW WILL BE USED FOR READING ',
     &   'CANKER DATA. THE ORDER OF THE VARIABLES IS:',/,T12,
     &   'TREE ID, STOCK TYPE, TREE AGE, DIST UP, DIST OUT, ',
     &   '% GIRDLE, TOTAL CANKER COUNT')
      READ(IREAD,1220,END=8900) ICFMT
 1220 FORMAT (A80)
      IF(LKECHO)WRITE(JOSTND,1230) ICFMT
 1230 FORMAT(T13,A80)
      GO TO 90

 1300 CONTINUE

C.... ==========  Option Number 13: DEVFACT  ========================

C.... Species and Stock Type Deviation Factor.
C....
C.... The purpose of the deviation factor is to truncate the infection
C.... level and assure a proportion of clean (or escape) trees in the
C.... stand.
C....
C.... This keyword is a activity and may be scheduled by cycle/year or
C.... condition evaluated by the FVS Event Monitor.
C.... Field 1 -- Cycle/year (entry of 0 will apply new dev. factors now)
C.... Field 2 -- Tree species code
C.... Fields 3-6 -- Deviation Factors for stock types 1 thru 4. 

      IDT=0
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))
      IF(LNOTBK(2)) KSP=IFIX(ARRAY(2))

C     Write initial output for keyword.
C
      IF(IDT.EQ.0) THEN
         IF(LKECHO)WRITE(JOSTND,1316) KEYWRD
      ELSE
         IF(LKECHO)WRITE(JOSTND,1317) KEYWRD, IDT
      ENDIF

C     Vaildate species code
      IF(KSP .LT. 0 .OR. KSP .GT. MAXSP) THEN
         WRITE(JOSTND,1305) KEYWRD, KSP
 1305    FORMAT(/,A8,'   ***** INVALID SPECIES CODE ',I3,'. *****')
         GO TO 90
      ELSE IF(BRSPM(KSP) .EQ. 0) THEN
         WRITE(JOSTND,1306) KEYWRD, KSP, NSP(KSP,1)(1:2)
 1306    FORMAT(/,A8,'   ***** NON-HOST SPECIES CODE ',
     &          I3,' - ',A,'. *****')
         GO TO 90
      ENDIF

      IF(IDT .EQ. 0) THEN
C        Apply new deviation factors now.
C
         LDFACT = .TRUE.
         IF(LNOTBK(3)) THEN
           IF(ARRAY(3).GE.0.01 .AND. ARRAY(3).LE.6.0) THEN
             DFACT(KSP,1) = ARRAY(3)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(3), DFACT(KSP,1)
           ENDIF
         ENDIF
         IF(LNOTBK(4)) THEN
           IF(ARRAY(4).GE.0.01 .AND. ARRAY(4).LE.6.0) THEN
             DFACT(KSP,2) = ARRAY(4)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(4), DFACT(KSP,2)
           ENDIF
         ENDIF
         IF(LNOTBK(5)) THEN
           IF(ARRAY(5).GE.0.01 .AND. ARRAY(5).LE.6.0) THEN
             DFACT(KSP,3) = ARRAY(5)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(5), DFACT(KSP,3)
           ENDIF
         ENDIF
         IF(LNOTBK(6)) THEN
           IF(ARRAY(6).GE.0.01 .AND. ARRAY(6).LE.6.0) THEN
             DFACT(KSP,4) = ARRAY(6)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(6), DFACT(KSP,4)
           ENDIF
         ENDIF
      ELSE
C        Load default/current values into PRMS array.
C        Update with new values provided with keyword.
C        Schedule activity.

         PRMS(1) = KSP
         PRMS(2) = DFACT(KSP,1)
         PRMS(3) = DFACT(KSP,2)
         PRMS(4) = DFACT(KSP,3)
         PRMS(5) = DFACT(KSP,4)
         IF(LNOTBK(3)) THEN
           IF(ARRAY(3).GE.0.01 .AND. ARRAY(3).LE.6.0) THEN
             PRMS(2)=ARRAY(3)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(3), PRMS(2)
           ENDIF
         ENDIF
         IF(LNOTBK(4)) THEN
           IF(ARRAY(4).GE.0.01 .AND. ARRAY(4).LE.6.0) THEN
             PRMS(3)=ARRAY(4)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(4), PRMS(4)
           ENDIF
         ENDIF
         IF(LNOTBK(5)) THEN
           IF(ARRAY(5).GE.0.01 .AND. ARRAY(5).LE.6.0) THEN
             PRMS(4)=ARRAY(5)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(5), PRMS(4)
           ENDIF
         ENDIF
         IF(LNOTBK(6)) THEN
           IF(ARRAY(6).GE.0.01 .AND. ARRAY(6).LE.6.0) THEN
             PRMS(5)=ARRAY(6)
           ELSE
             IF(LKECHO)WRITE(JOSTND,1321) ARRAY(6), PRMS(5)
           ENDIF
         ENDIF

         IACT = 1010
         NP = 5
         CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
         IF(KODE.GT.0) THEN
           WRITE(JOSTND,1313) KEYWRD
 1313      FORMAT(/,A8,'   ***** Activity Scheduling Error *****')
           GO TO 90
         ENDIF
      ENDIF

C     Write final output for keyword.
C
      IF(IDT.EQ.0) THEN
         IF(LKECHO)
     &   WRITE(JOSTND,1320) NSP(KSP,1)(1:2), (DFACT(KSP,I),I=1,4)
      ELSE
         IF(LKECHO)WRITE(JOSTND,1320) NSP(KSP,1)(1:2), (PRMS(I),I=2,5)
      ENDIF

 1316 FORMAT(/,A8,'   STAND DEVIATION FACTORS INITIALIZED FOR')
 1317 FORMAT(/,A8,'   STAND DEVIATION FACTORS WILL BE RESET AT ',
     &       'CYCLE/YEAR ',I4,' FOR')
 1320 FORMAT(T12,'TREE SPECIES = ',A,/,T12,'STOCK TYPE 1 = ',F5.3,
     &      /,T12,'STOCK TYPE 2 = ',F5.3,/,T12,'STOCK TYPE 3 = ',F5.3,
     &      /,T12,'STOCK TYPE 4 = ',F5.3)
 1321 FORMAT(T12,'*** INVALID DEVIATION FACTOR ENTERED: ',F5.3,
     &      ' DEFAULT VALUE: ',F5.3,' RETAINED. ***')
      GO TO 90

 1400 CONTINUE

C.... ==========  Option Number 14: RUSTINDX  =======================
C....
C.... Keyword to change the default value of Rust Index, Stand
C.... Deviation Factor, Rust Index Assignment Method, and Ribes
C.... species proportions of population for RI calculation.
C....
C.... Stand Deviation Factor (F2) has been relocated to new 
C.... keyword DEVFACT.
C.... LRD 14-MAR-01

      IF(LNOTBK(1)) RIDEF=ARRAY(1)
      IF(LKECHO)WRITE(JOSTND,1410) KEYWRD,RIDEF

      IF(LNOTBK(3)) THEN
         RIMETH=ARRAY(3)
         IF(RIMETH.EQ.1) THEN
            IF(LKECHO)WRITE(JOSTND,1412)
         ELSE IF(RIMETH.EQ.2) THEN
            IF(LKECHO)WRITE(JOSTND,1413)
         ELSE IF(RIMETH.EQ.3) THEN
            IF(LKECHO)WRITE(JOSTND,1414)
         ELSE IF(RIMETH.EQ.4) THEN
            IF(LKECHO)WRITE(JOSTND,1418)
         ELSE
            RIMETH=0
            IF(LKECHO)WRITE(JOSTND,1415)
         ENDIF
      ELSE
         IF(LKECHO)WRITE(JOSTND,1415)
      ENDIF

      IF(RIMETH.EQ.1 .OR. RIMETH.EQ.2) THEN

C....    Process ribes proportions of population values.

         IF(LNOTBK(4)) RIBPRP(1)=ARRAY(4)
         IF(LNOTBK(5)) RIBPRP(2)=ARRAY(5)
         IF(LNOTBK(6)) RIBPRP(3)=ARRAY(6)

 1405    CONTINUE
         IF(RIBPRP(1).LT.0.0) RIBPRP(1)=0.0
         IF(RIBPRP(2).LT.0.0) RIBPRP(2)=0.0
         IF(RIBPRP(3).LT.0.0) RIBPRP(3)=0.0

         EXCESS=1.0-RIBPRP(1)-RIBPRP(2)-RIBPRP(3)

         IF(EXCESS.GT.0.01.OR.EXCESS.LT.-0.01) THEN

C....       Adjust proportions if total is not within 1 percent.

            RIBPRP(1)=RIBPRP(1)+(EXCESS/3.0)
            RIBPRP(2)=RIBPRP(2)+(EXCESS/3.0)
            RIBPRP(3)=RIBPRP(3)+(EXCESS/3.0)
            GO TO 1405
         ENDIF
         IF(LKECHO)WRITE(JOSTND,1416) RIBPRP(1),RIBPRP(2),RIBPRP(3)

      ELSEIF (RIMETH.EQ.3) THEN
C....    For Rust Index length of exposure equation, parameters
C....    may be provided.
C....    Read supplemental record for equation parameters. This
C....    is a unformatted read, so four values must be present on 
C....    the record. A zero will cause the default value for the
C....    parameter to be retained.

C....    Default values for Gaussian equation
         MINRI = 1.238867E-3
         MAXRI = 7.222711E-3
         PKAGE = 23.34596387
         PKSHP = 4.078136888

         READ (IREAD,*) ARRAY(1), ARRAY(2), ARRAY(3), ARRAY(4)
         IF (ARRAY(1).GT.0.0) MINRI = ARRAY(1)
         IF (ARRAY(2).GT.0.0) MAXRI = ARRAY(2)
         IF (ARRAY(3).GT.0.0) PKAGE = ARRAY(3)
         IF (ARRAY(4).GT.0.0) PKSHP = ARRAY(4)
         IF(LKECHO)WRITE(JOSTND,1417) MINRI,MAXRI,PKAGE,PKSHP

      ELSEIF (RIMETH.EQ.4) THEN
C....    For Rust Index length of exposure equation, parameters
C....    may be provided.
C....    Read supplemental record for equation parameters. This
C....    is a unformatted read, so four values must be present on 
C....    the record. A zero will cause the default value for the
C....    parameter to be retained.

C....    default values for log function
         MINRI = 9.6E-5
         MAXRI = 4.7E-3
         PKAGE = 1.66
         PKSHP = 1.086

         READ (IREAD,*) ARRAY(1), ARRAY(2), ARRAY(3), ARRAY(4)
         IF (ARRAY(1).GT.0.0) MINRI = ARRAY(1)
         IF (ARRAY(2).GT.0.0) MAXRI = ARRAY(2)
         IF (ARRAY(3).GT.0.0) PKAGE = ARRAY(3)
         IF (ARRAY(4).GT.0.0) PKSHP = ARRAY(4)
         IF(LKECHO)WRITE(JOSTND,1417) MINRI,MAXRI,PKAGE,PKSHP
      ENDIF

C.... Output for RUSTINDX keyword.

 1410 FORMAT(/,A8,'   STAND RUST INDEX INITIALLY SET TO ',F7.5,';')
 1412 FORMAT(T12,'RUST INDEX WILL BE CALCULATED FROM BASAL AREA AT ',
     &   'INITIALIZATION AND USED THROUGHOUT THE SIMULATION')
 1413 FORMAT(T12,'RUST INDEX WILL BE CALCULATED FROM BASAL AREA ',
     &   'EACH CYCLE AND APPLIED TO ALL TREES.')
 1414 FORMAT(T12,'RUST INDEX BASED ON STAND AGE GAUSSIAN FUNCTION ',
     &   'EACH CYCLE AND APPLIED TO ALL TREES')
 1415 FORMAT(T12,'RUST INDEX SHOWN ABOVE WILL BE USED ',
     &   'THROUGHOUT THE SIMULATION')
 1416 FORMAT(T12,'RIBES SPECIES PROPORTIONS OF POPULATION WHICH WILL ',
     &   'BE USED FOR RUST INDEX CALCULATION ARE:',/T15,
     &   'RIBES H. = ',F4.2,';  RIBES L. = ',F4.2,';  RIBES V. = ',F4.2)
 1417 FORMAT(T12,'THE PARAMETERS USED IN EQUATION OF RI ASSIGNMENT ',
     &   'METHOD SELECTED ARE AS FOLLOWS.',
     &   /,T15,'MINIMUM RUST INDEX VALUE: ',F11.9,
     &   /,T15,'MAXIMUM RUST INDEX VALUE: ',F11.9,
     &   /,T15,'STAND AGE WHERE RI PEAKS: ',F10.5,
     &   /,T15,'CURVE SHAPE DEFINED BY:   ',F10.7)
 1418 FORMAT(T12,'RUST INDEX BASED ON STAND AGE LOG FUNCTION',
     &   'EACH CYCLE AND APPLIED TO ALL TREES.')

      GO TO 90

 1500 CONTINUE

C.... ==========  Option Number 15: BRSEED =========================

C.... Changes the default random number generator seed value.

      CALL BRNSED(LNOTBK(1),ARRAY(1))

C.... Output for BRSEED keyword.

      IF(LKECHO)WRITE(JOSTND,1510) KEYWRD,ARRAY(1)
 1510 FORMAT(/,A8,'   RANDOM SEED FOR BLISTER RUST MODEL = ',F10.0)
      GO TO 90

 1600 CONTINUE

C.... ==========  Option Number 16: STOCK  =========================

C.... Changes the default rust index adjustment factors for host pine
C.... stock, wild (natural regen) and three families of planting stock
C.... (F1, F2, and GCOP - General Combiner Open Pollinated) and
C.... their respective proportion of the host pine population.
C.... This is an activity.
C.... Date/Cycle 0 changes values, but does not schedule an activity
C.... with the option processor.
C.... fields are: 1-date, 2-tree species, 3-stock type,
C....             4-proportion of population, 5-RI adjustment

      IDT=0
      IF(LNOTBK(1)) IDT=INT(ARRAY(1))
      ITSP=IFIX(ARRAY(2))
      ICLS=IFIX(ARRAY(3))
      IF(ICLS.GE.1 .AND. ICLS.LE.4 .AND. BRSPM(ITSP).NE.0) THEN
         CONTINUE
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         CALL ERRGRO(.TRUE.,4)
         GO TO 90
      ENDIF

      PRMS(1)=ARRAY(2)
      PRMS(2)=ARRAY(3)
      PRMS(3)=PRPSTK(ITSP,ICLS)
      PRMS(4)=RESIST(ITSP,ICLS)

      IF(IDT.EQ.0) THEN
         IF(LNOTBK(4)) THEN
            PRPSTK(ITSP,ICLS)=ARRAY(4)
            PRMS(3)=ARRAY(4)
         ENDIF
         IF(LNOTBK(5)) THEN
            RESIST(ITSP,ICLS)=ARRAY(5)
            PRMS(4)=ARRAY(5)
         ENDIF
      ELSE
         IF(LNOTBK(4)) PRMS(3)=ARRAY(4)
         IF(LNOTBK(5)) PRMS(4)=ARRAY(5)
         IACT = 1009
         NP = 4
         CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
         IF(KODE.GT.0) GO TO 90
      ENDIF

C.... Output for STOCK keyword.

      IF(LKECHO)WRITE(JOSTND,1620) KEYWRD,IDT,ITSP,ICLS,PRMS(3),PRMS(4)
 1620 FORMAT(/,A8,'   MIX OF STOCK TYPES TO BE CHANGED IN ',
     & 'DATE/CYCLE ',I4,';',/T12,'TREE SPECIES=',I3,'; STOCK TYPE = ',
     & I2,';  PROPORTION OF STOCK IN POPULATION = ',F4.2,
     & ';  RI ADJUSTMENT = ',F4.2)
      GO TO 90

 1700 CONTINUE

C.... ==========  Option Number 17: CANKDATA  =======================

C.... Read canker data.  Field 1 contains the logical unit number from
C.... which to read the canker data.  Field 2 contains the units of
C.... measure for the canker data where 0 = U.S. (data is in ft./in.)
C.... and 1 = metric (data is in cm.).  Field 3 is the "initialize
C.... canker conditions" flag; if 0 then canker data will be read in
C.... normally and the canker count will just be added to the ITCAN
C.... array.  If 1 then the canker count listed in the canker data
C.... will be used to randomly generate cankers up to 10 (or the canker
C.... count - whichever is less) minus the ones that have already been
C.... read into the ILCAN array from the canker data.  Supplemental
C.... record(s) contain either the name of the canker data file (up to
C.... 80 characters including a pathname) or the canker data itself.

      IF(LNOTBK(1)) ICIN=IFIX(ARRAY(1))
      IF(LNOTBK(2)) THEN
         IF(IFIX(ARRAY(2)).EQ.1) THEN
            LMETRIC=.TRUE.
         ENDIF
      ENDIF
      IF(LNOTBK(3)) THEN
         IF(IFIX(ARRAY(3)).EQ.1) THEN
            CKINIT=.TRUE.
         ENDIF
      ENDIF

C.... Output for CANKDATA keyword.

      IF(LMETRIC)THEN
         IF(LKECHO)WRITE(JOSTND,1720) KEYWRD
 1720    FORMAT(/,A8,'   CANKER DATA PROVIDED IN CENTIMETERS',
     &      ' (DISTANCE UP AND DISTANCE OUT);')
      ELSE
         WRITE (JOSTND,1725) KEYWRD
 1725    FORMAT(/,A8,'   CANKER DATA PROVIDED IN FEET (DISTANCE UP)',
     &      ' AND INCHES (DISTANCE OUT);')
      ENDIF

      IF(CKINIT)THEN
         IF(LKECHO)WRITE(JOSTND,1730)
 1730    FORMAT(T12,'CANKER DATA WILL BE RANDOMLY GENERATED AT',
     &      ' INITIALIZATION ACCORDING TO CANKER COUNTS READ IN;')
      ENDIF

C.... If unit number = 15 then we will read canker data from the
C.... keyword file (directly following the CANKDATA keyword).
C.... Otherwise, read the name of the canker data file from the
C.... supplemental record (directly following the CANKDATA keyword).

      IF(ICIN.EQ.15) THEN
         IF(LKECHO)WRITE(JOSTND,1705)
 1705    FORMAT(T12,'CANKER DATA WILL BE READ FROM SUPPLEMENTAL',
     &      ' RECORDS FOLLOWING THIS KEYWORD')
         ICIN=IREAD
      ELSE
         READ(IREAD,1710,END=8900) DNAME
 1710    FORMAT(A80)

         IF(DNAME .EQ. ' ') THEN
C....       If no filename provided on supplemental record, load
C....       dname with the first 76 characters of the FVS keyword
C....       file name and concatenate .can extension.
            DNAME=KWDFIL(1:76)//'.can'
         ENDIF

C....    Remove blanks from file name.
         CALL UNBLNK(DNAME,LENTH)

C....    Open the canker data file.

         INQUIRE(UNIT=ICIN,OPENED=LOPEN)
         IF(LOPEN) CLOSE(UNIT=ICIN)
         CALL MYOPEN(ICIN,DNAME,1,132,0,1,1,0,KODE)
         IF(KODE.NE.0) THEN
            PRINT*,'*BRIN* Error opening filename: ',DNAME
            GO TO 9000
         ENDIF
         IF(LKECHO)WRITE(JOSTND,1715) ICIN,DNAME
 1715    FORMAT(T12,'CANKER DATA WILL BE READ FROM UNIT NUMBER ',I2,
     &      ';',/T12,'CANKER DATA FILENAME = ',A80)
      ENDIF

C.... Read the canker data; either from the external file or from
C.... supplemental records.

      CALL BRCANK
      GO TO 90

 1800 CONTINUE

C.... ==========  Option Number 18: BROUT  =======================

C.... Option 18 Controls the summary output written to the FVS output
C.... file. 
C....
C.... Field 1: controls "Stand Summary Statistics for Blister Rust Hosts"
C....          Default is on
C.... Field 2: controls "Stand DBH Class Statistics for Blister Rust Hosts"
C....          Default is off

      IF(LNOTBK(1)) THEN
         IF(IFIX(ARRAY(1)).NE.0) THEN
            LBRSUM=.TRUE.
         ELSE
            LBRSUM=.FALSE.
         ENDIF
      ENDIF
      IF(LNOTBK(2)) THEN
         IF(IFIX(ARRAY(2)).NE.0) THEN
            LBRDBH=.TRUE.
         ELSE
            LBRDBH=.FALSE.
         ENDIF
      ENDIF

C.... Output for BROUT keyword.

      IF(LKECHO)WRITE(JOSTND,1820) KEYWRD
 1820 FORMAT(/,A8,'   BLISTER RUST SUMMARY AND DBH CLASS ',
     &      'STATISTICS TABLES CONTROL.')
      IF(LKECHO .AND. LBRSUM)WRITE(JOSTND,1821)
 1821 FORMAT(T12,'STAND SUMMARY STATISTICS WILL BE PRINTED.')
      IF(LKECHO .AND. LBRDBH)WRITE(JOSTND,1822)
 1822 FORMAT(T12,'STAND DBH CLASS STATISTICS WILL BE PRINTED.')
      IF(LKECHO .AND. .NOT. LBRDBH
     &          .AND. .NOT. LBRSUM)WRITE(JOSTND,1823)
 1823 FORMAT(T12,'BOTH STAND DBH CLASS AND SUMMARY STATISTICS '
     &      'IS SUPPRESSED.')

      GO TO 90

C.... Special entry to retrieve keywords

      ENTRY BRKEY(IKEY,PASKEY)
      PASKEY=TABLE(IKEY)
      GO TO 9020

C.... Found EOF prematurely.

 8900 CONTINUE
      CALL ERRGRO(.TRUE.,2)

C.... Common return.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010)ICYC
 9010 FORMAT('End BRIN: Cycle = ',I5)

 9020 RETURN
      END
