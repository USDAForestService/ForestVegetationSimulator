      SUBROUTINE MISIN(PASKEY,ARRAY,LNOTBK,LKECHO)
***********************************************************************
C MISTOE $Id: misin.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Reads in mistletoe keywords and processes the ones that aren't
*  cycle dependent.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ARRAY:  (I) Array containing keyword entries.
*     LNOTBK: (O) Logical array depicting blank keyword fields.
*     PASKEY: (I) Character buffer containing keyword name.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on and off.
*     IDT:    Temporary variable to hold date from keyword.
*     ISIZE:  Size of a keyword entry (10 bytes).
*     ISPL:   Numeric species code.
*     KARD:   Character buffer returned from KEYRDR.
*     KEY:    Keyword number used in MISKEY entry point call list.
*     KEYWRD: Character buffer to hold local copy of PASKEY.
*     KODE:   Error code passed back from keyword reading routines.
*     NUMBR:  Keyword number passed back from FNDKEY.
*     TABLE:  Character array of possible keywords.
*
*  Common block variables and parameters:
*
*     DMRMIN: From MISCOM; minimum tree DBH for DMR/DMI statistics.
*     ICYC:   From CONTRL; cycle index number.
*     IREAD:  From CONTRL; logical unit number for keyword input.
*     IRECNT: From CONTRL; number of keyword parameter records read.
*     ITYPE:  From PLOT;   input stand habitat type code.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MAXCYC: From PRGPRM; maximum # cycles.
*     MAXSP:  From PRGPRM; maximum # species.
*     MISFLG: From MISCOM; logical flag for turning mistletoe on/off.
*     NSP:    From PLOT;   valid species-tree class codes.
*     PRTMIS: From MISCOM; log. flag for turning DM printing on/off.
*
*  Revision History :
*     02/28/95 - Lance R. David (MAG)
*                Changed MISTGMOD keyword to use supplemental to provide
*                growth modification proportions for 1 species DMRs 1-6.
*                Field 2 of keyword record specifies species being
*                modified.
*     02/26/98 - Lance R. David (FHTET)
*                Default value for MISTPRT field 1 (DMRMIN) set to 1.0
*     12/05/01 - Lance R. David (MAG)
*                Initialization of DMRMIN moved to MISIN0.f.
*                Slight modification to clean up MISPRT keyword process.
*     10/20/03 - Bob Havis (FMSC)
*                COMMENT keyword processing removed.
*                Added LFLAG to KEYRDR call statement argument list.
*     12/20/03 - Don Robinson (ESSA)
*                Modified so that all Dwarf Mistletoe summaries are written
*                to the FVS standard output file. Also added ability to
*                utilize the FVS database extension.
*                Subroutine (MISOUT) and parameter file (MISPRM) removed.
*     03/14/05 - Lance David (FHTET)
*                Removed manipulation of IDMSOUT(3) and IDMSOUT(4) at
*                MISTPRT and MISTABLE keyword processing.
*     07/13/05 - Lance David (FHTET)
*                SPDECD invalid species return code changed to -999.
*                Species format changed from I2 to I3.
*     01/25/06 - Robert Havis (FMSC)
*                Added conditional at writing of keywords to utilize new
*                FVS keywords ECHO and NOECHO
*     02/02/06 - Lance David (FHTET)
*                Removed obsolete message "*** MISTMORT keyword ignored in
*                CR variant". Modified keyword output for MISTMULT and
*                MISTMORT keywords to be more descriptive.
*     03/22/07 - Lance David (FHTET)
*                Functionality of MistOff keyword was changed so that it
*                is not required to be processed prior to tree data input.
*                Added inclusion of ARRAYS.F77, which required change of
*                local variable "ISP" to "ISPL".
*                MistOff keyword text modified to reflect change in behavior.
*     07/09/07 - Lance David (FHTET)
*                With restructuring of damage code processing after reading
*                of the input tree data, conditional re-zeroing of IMIST
*                array added for previous update is no longer needed and
*                was removed along with inclusion of common ARRAYS.F77.
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.
      INTEGER ISIZE
      PARAMETER (ISIZE=11)

      LOGICAL LNOTBK(7),DEBUG,LKECHO
      REAL ARRAY(7)
      INTEGER I,IDT,ISPL,KEY,KODE,NUMBR,IRTNCD
      CHARACTER*8 TABLE(ISIZE),KEYWRD,PASKEY
      CHARACTER*10 KARD(7)

C.... Data statements.

      DATA TABLE/
     &   'MISTMULT','MISTHMOD','END     ','MISTPREF','MISTMORT',
     &   'MISTPRT ','MISTGMOD','MISTOFF ','        ','MISTPINF',
     &   'MISTABLE'/

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISIN',5,ICYC)

      IF(DEBUG)WRITE(JOSTND,10)ICYC,ITYPE
   10 FORMAT(' Begin MISIN: Cycle, ITYPE = ',I5,I5)

C.... Load the passed keyword into KEYWRD.

      KEYWRD=PASKEY

C.... Call the keyword reader.

   50 CONTINUE
      CALL KEYRDR(IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,ARRAY,IRECNT,KODE,
     &   KARD,LFLAG,LKECHO)

C.... Process errors; 0=no error, 1=first column blank, 2=EOF.

      IF(KODE.NE.0) THEN
         IF(KODE.EQ.2) THEN
            CALL ERRGRO(.FALSE.,2)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ELSE
            CALL ERRGRO(.TRUE.,6)
         ENDIF
         GO TO 50
      ENDIF

      CALL FNDKEY(NUMBR,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

C.... Return codes; 0=no error, 1=keyword not found.

      IF(KODE.EQ.1) THEN
         CALL ERRGRO(.TRUE.,1)
         GO TO 50
      ENDIF
      GO TO 70

C.... Special EOF target.

   60 CONTINUE
      CALL ERRGRO(.FALSE.,2)
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN

C.....Process the keyword.

   70 CONTINUE
      GO TO(100,200,300,400,500,600,700,800,900,1000,1100),NUMBR

C.... Option number 1: MISTMULT.

  100 CONTINUE

C.... Check for mistletoe spread probability adjustment multipliers.
C.... ARRAY(1): effective date, ARRAY(2): species number, ARRAY(3):
C.... probability(+) multiplier, ARRAY(4): probability(-) multiplier.

      IF(.NOT.LNOTBK(3)) ARRAY(3)=1.0
      IF(.NOT.LNOTBK(4)) ARRAY(4)=1.0

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for valid species code.

      IF(ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid multipliers.

         IF(ARRAY(3).LT.0.0) THEN
            WRITE(JOSTND,8100) ARRAY(3)
            ARRAY(3)=1.0
         ENDIF
         IF(ARRAY(4).LT.0.0) THEN
            WRITE(JOSTND,8100) ARRAY(4)
            ARRAY(4)=1.0
         ENDIF

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2001,3,ARRAY(2))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,120) KEYWRD,IDT,KARD(2),ISPL,ARRAY(3),
     &                               ARRAY(4)
  120    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,'); DMR Increase Multiplier=',F6.2,
     &      '; DMR Decrease Multiplier=',F6.2)
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)
      ENDIF
      GO TO 50

C.... Option number 2: MISTHMOD.

  200 CONTINUE
C.... Mistletoe height growth modification proportion values.
C.... ARRAY(1): effective date, ARRAY(2): species number
C.... Supplemental record: fields 1-6, height growth proportions
C.... for DMRs 1-6.

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for invalid species code.

      IF(ISPL.LT.0.OR.ISPL.GT.MAXSP) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)

C.... Process height growth modification proportions from
C.... supplemental record

      ELSE
         ARRAY(1)=ARRAY(2)
         READ(IREAD,*,END=60) (ARRAY(I+1),I=1,6)

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2004,7,ARRAY(1))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,250)KEYWRD,IDT,KARD(2),ISPL
  250    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,
     &      ' (',I3,'):')
         DO 290 I=1,6
            IF(LKECHO)WRITE(JOSTND,280) I,ARRAY(I+1)
  280       FORMAT(10X,' DMR=',I1,
     &      ';  height growth modification proportion=',
     &      F10.2)
  290    CONTINUE
      ENDIF
      GO TO 50

C.... Option number 3: END.

  300 CONTINUE

C.... End mistletoe keyword processing.

      IF(LKECHO)WRITE(JOSTND,310) KEYWRD
  310 FORMAT(/A8,'   End of mistletoe keywords.')
      GO TO 9000

C.... Option number 4: MISTPREF.

  400 CONTINUE

C.... Check for mistletoe cutting preference values.
C.... ARRAY(1): effective date, ARRAY(2): species number
C.... Supplemental record: fields 1-6, preferences for DMRs 1-6.

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for invalid species code.

      IF(ISPL.LT.0.OR.ISPL.GT.MAXSP) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)

C.... Process preferences from supplemental record

      ELSE
         ARRAY(1)=ARRAY(2)
         READ(IREAD,*,END=60) (ARRAY(I+1),I=1,6)

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2002,7,ARRAY(1))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,450)KEYWRD,IDT,KARD(2),ISPL
  450    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,
     &      ' (',I3,'):')
         DO 490 I=1,6
            IF(LKECHO)WRITE(JOSTND,480) I,ARRAY(I+1)
  480       FORMAT(' DMR=',I1,';  cutting preference=',F10.2)
  490    CONTINUE
      ENDIF
      GO TO 50

C.... Option number 5: MISTMORT.

  500 CONTINUE

C.... Check for mistletoe mortality percentage multiplier.
C.... ARRAY(1): effective date, ARRAY(2): species number,
C.... ARRAY(3): mortality percentage multiplier.

      IF(.NOT.LNOTBK(3)) ARRAY(3)=1.0

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for valid species code.

      IF(ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid multipliers.

         IF(ARRAY(3).LT.0.0) THEN
            WRITE(JOSTND,8100) ARRAY(3)
            ARRAY(3)=1.0
         ENDIF

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2003,2,ARRAY(2))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,520) KEYWRD,IDT,KARD(2),ISPL,ARRAY(3)
  520    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,'); Mortality Multiplier=',F6.2)
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)
      ENDIF
      GO TO 50

C.... Option number 6: MISTPRT.

  600 CONTINUE

C.... If this keyword is used, then print 3 statistical summary
C.... tables.  This is separate from MISTABLE.
C.... Check for DBH threshold for large trees.  Trees this size
C.... and above will be counted in DMR/DMI statistics.

      PRTMIS=.TRUE.

      IF(LNOTBK(1).AND.ARRAY(1).GE.0.0) DMRMIN=ARRAY(1)
      IF(LKECHO)WRITE(JOSTND,620) KEYWRD,DMRMIN
  620 FORMAT(/A8,'   Print mistletoe summary output; ',
     &   'DMRs and DMIs calculated for trees with DBH >=',F4.1)
      GO TO 50

C.... Option number 7: MISTGMOD.

  700 CONTINUE

C.... Mistletoe diameter growth modification proportion values.
C.... ARRAY(1): effective date, ARRAY(2): species number
C.... Supplemental record: fields 1-6, growth modification proportions
C.... for DMRs 1-6.

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for invalid species code.

      IF(ISPL.LT.0.OR.ISPL.GT.MAXSP) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)

C.... Process growth modification proportions from supplemental record

      ELSE
         ARRAY(1)=ARRAY(2)
         READ(IREAD,*,END=60) (ARRAY(I+1),I=1,6)

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2005,7,ARRAY(1))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,750)KEYWRD,IDT,KARD(2),ISPL
  750    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,
     &      ' (',I3,'):')
         DO 790 I=1,6
            IF(LKECHO)WRITE(JOSTND,780) I,ARRAY(I+1)
  780       FORMAT(' DMR=',I1,
     &      ';  diameter growth modification proportion=',
     &      F10.2)
  790    CONTINUE
      ENDIF
      GO TO 50

C.... Option number 8: MISTOFF.

  800 CONTINUE

C.... Check for mistletoe processing ON/OFF option.  If this keyword is
C.... used, then ignore the dwarf mistletoe in the stand.  The default
C.... is to run with mistletoe if any exists.

      MISFLG=.FALSE.

      IF(LKECHO)WRITE(JOSTND,820) KEYWRD
  820 FORMAT(/A8,'   Mistletoe stand infection ignored.')

      GO TO 50

C.... Option number 9: ** OPEN SPOT

  900 CONTINUE

      GO TO 50

C.... Option number 10: MISTPINF.

 1000 CONTINUE

C.... Check for new (introduced) mistletoe infections option.
C.... ARRAY(1): effective date, ARRAY(2): species number, ARRAY(3):
C.... proportion of species to infect, ARRAY(4): infection level
C.... (DMR 1-6), ARRAY(5): infection method (0-2).

      IF(.NOT.LNOTBK(3)) ARRAY(3)=0.0
      IF(.NOT.LNOTBK(4)) ARRAY(4)=1.0
      IF(.NOT.LNOTBK(5)) ARRAY(5)=0.0

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for valid species code.

      IF(ISPL.LT.0.OR.ISPL.GT.MAXSP) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)

C.... Check for invalid proportion to be infected.

      ELSE IF(ARRAY(3).LT.0.OR.ARRAY(3).GT.1) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8300) ARRAY(3)
         CALL ERRGRO(.TRUE.,4)

C.... Check for invalid DMR level of infection.

      ELSE IF(ARRAY(4).LT.1.OR.ARRAY(4).GT.6) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8400) NINT(ARRAY(4))
         CALL ERRGRO(.TRUE.,4)

C.... Check for invalid method of infection.

      ELSE IF(ARRAY(5).LT.0.OR.ARRAY(5).GT.2) THEN
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8500) NINT(ARRAY(5))
         CALL ERRGRO(.TRUE.,4)
      ELSE

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2006,4,ARRAY(2))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,1020) KEYWRD,IDT,KARD(2),ISPL,ARRAY(3)
 1020    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,'); Proportion to Infect=',F10.2,';')
         IF(LKECHO)WRITE(JOSTND,1021) ARRAY(4),ARRAY(5)
 1021    FORMAT(8X,'   Infection Level=',F10.2,'; Infection Method=',
     &      F10.2)
      ENDIF
      GO TO 50

C.... Option number 11: MISTABLE.

 1100 CONTINUE

C.... Check for mistletoe detail (species/DBH) table output preference.
C.... ARRAY(1): effective date, ARRAY(2): species number.

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

C.... Get species code.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for valid species code.

      IF(ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2007,1,ARRAY(2))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,1120) KEYWRD,IDT,KARD(2),ISPL
 1120    FORMAT(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,')')
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)
      ENDIF
      GO TO 50

C.... Error messages.

 8000 FORMAT(/' *** Species specified,',I3,', outside range of',
     &       ' valid species ( 0 -',I3,' )')
 8100 FORMAT(/' *** Value specified,',F10.4,', negative; set to',
     &       ' 1.0.')
 8300 FORMAT(/' *** Proportion to be infected, ',F10.4,', outside',
     &       ' range of valid values ( 0.0 - 1.0 )')
 8400 FORMAT(/' *** DMR infection specified,',I3,', outside range',
     &       ' of valid values ( 1 - 6 )')
 8500 FORMAT(/' *** Method of infection specified,',I3,', outside',
     &       ' range of valid values ( 0 - 2 )')

C.... Common return.

 9000 CONTINUE

      IF(DEBUG)WRITE(JOSTND,9010)ICYC
 9010 FORMAT('End MISIN: Cycle = ',I5)
      GO TO 9020

C.... Special entry to retrieve keywords.

      ENTRY MISKEY(KEY,PASKEY)
      PASKEY=TABLE(KEY)

 9020 CONTINUE
      RETURN
      END
