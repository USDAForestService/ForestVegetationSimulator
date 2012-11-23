        SUBROUTINE MISIN(PASKEY,ARRAY,LNOTBK,LKECHO)
***********************************************************************
*  **MISIN/M    Date of last revision:  07/09/07
*               Spatial Model
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
*     NUMBER: Keyword number passed back from FNDKEY.
*     TABLE:  Character array of possible keywords.
*     VVER:   Character buffer for variant version.
*
*  Common block variables and parameters:
*
*     DMFILE: From MISCOM; array to hold DM table perm. file names.
*     DMDETH: From DMCOM;
*     DMETUN: From DMCOM;
*     DMFLWR: From DMCOM;
*     DMITUN: From DMCOM;
*     DMKTUN: From DMCOM;
*     DMLTNP: From DMCOM;
*     DMLTRX: From DMCOM;
*     DMOPAQ: From DMCOM;
*     DMOPQM: From DMCOM;
*     DMSTUN: From DMCOM;
*     DMRMIN: From MISCOM; minimum tree DBH for DMR/DMI statistics.
*     ICYC:   From CONTRL; cycle index number.
*     IREAD:  From CONTRL; logical unit number for keyword input.
*     IRECNT: From CONTRL; number of keyword parameter records read.
*     ITYPE:  From PLOT;   input stand habitat type code.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MAXCYC: From PRGPRM; maximum # cycles.
*     MAXSP:  From PRGPRM; maximum # species.
*     MISFLG: From MISCOM; logical flag for turning mistletoe on/off.
*     NEWMOD: From DMCOM;
*     NSP:    From PLOT;   valid species-tree class codes.
*     PRTMIS: From MISCOM; log. flag for turning DM printing on/off.
*
*  Revision History :
*     03/19/96 - Matthew K. Thompson (MAG)
*                Changed MISTGMOD keyword to use supplemental to provide
*                growth modification proportions for 1 species DMRs 1-6.
*                Field 2 of keyword record specifies species being
*                modified.  This change was made for the Interim model
*                by Lance R. David on 02/28/95.
*     02/26/98 - Lance R. David (FHTET)
*                Default value for MISTPRT field 1 (DMRMIN) set to 1.0
*     12/05/01 - Lance R. David (MAG)
*                Initialization of DMRMIN moved to MISIN0.f
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
      INCLUDE 'DMCOM.F77'
      INCLUDE 'METRIC.F77'

C.... Variable declarations.

      INTEGER ISIZE
      PARAMETER (ISIZE=30)

      LOGICAL      LNOTBK(7),DEBUG,LCHK(2),LOK,LKECHO
      INTEGER      I,IDT,ISPL,KEY,KODE,NUMBER,IKD,IPRMPT
      INTEGER      LTNP(2), ERRT(2),J,K,M,N,P
      INTEGER      DMT(5,3)
      REAL         DMLRX(2,2,4),X
      REAL         ARRAY(7)
      CHARACTER*8  TABLE(ISIZE),KEYWRD,PASKEY,LTNM
      CHARACTER*10 KARD(7)
      CHARACTER*8  RTYPE(2)
      CHARACTER*7  VVER

C.... Data statements.

      DATA TABLE/
     >   'MISTMULT','        ','END     ','MISTPREF','MISTMORT',
     >   'MISTPRT ','MISTGMOD','MISTOFF ','        ','MISTPINF',
     >   'MISTABLE','NEWSPRED','DMCRTHRD','DMSED   ','DMOPAQ  ',
     >   'DMKTUNE ','DMSTUNE ','DMITUNE ','DMETUNE ','DMLIGHT ',
     >   'DMDEATH ','DMCAP   ','DMCLUMP ','DMAUTO  ','DMTABLE ',
     >   '        ','DMOPQMLT','DMFLOWER','DMBCI   ','DMBCA   '/

C.... Initialize variable NEWMOD.

      NEWMOD = .FALSE.

C.... See 'OPLIST' for arrays with OPNEW values and all that
C.... weird stuff in arrays ITRSL1() and ITRSL2().

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISIN',5,ICYC)

      IF(DEBUG)WRITE(JOSTND,10)ICYC,ITYPE
   10 FORMAT(' Begin MISIN: Cycle, ITYPE = ',I5,I5)

C.... Check for variant version.

      CALL VARVER(VVER)

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
         ELSE
            CALL ERRGRO(.TRUE.,6)
         ENDIF
         GO TO 50
      ENDIF

      CALL FNDKEY(NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

C.... Return codes; 0=no error, 1=keyword not found.

      IF(KODE.EQ.1) THEN
         CALL ERRGRO(.TRUE.,1)
         GO TO 50
      ENDIF
      GO TO 70

C     Special EOF target.

   60 CONTINUE
      CALL ERRGRO(.FALSE.,2)

C.....Process the keyword.

   70 CONTINUE
      GO TO( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     &      1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     &      2100,2200,2300,2400,2500,2600,2700,2800,2900,3000),
     &      NUMBER

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
  120    FORMAT(/1X,A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,'); DMR Increase Multiplier=',F6.2,
     &      '; DMR Decrease Multiplier=',F6.2)
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)
      ENDIF
      GO TO 50

C.... Option number 2:

  200 CONTINUE
      GO TO 50

C.... Option number 3: END.

  300 CONTINUE

C.... End mistletoe keyword processing.

      IF(LKECHO)WRITE(JOSTND,310) KEYWRD
  310 FORMAT(/1X,A8,'   End of mistletoe keywords.')
      GO TO 9000

C.... Option number 4: MISTPREF

  400 CONTINUE

C.... Check for mistletoe cutting preference values.
C.... ARRAY(1): effective date, ARRAY(2): species number,
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
  450    FORMAT(/1X,A8,'   Date/Cycle=',I5,'; Species= ',A3,
     &      ' (',I3,'):')
         DO 490 I=1,6
            IF(LKECHO)WRITE(JOSTND,480) I,ARRAY(I+1)
  480 FORMAT(1X,' DMR=',I1,';  cutting preference=',F10.2)
  490    CONTINUE
      ENDIF
      GO TO 50

C.... Option 5: MISTMORT.

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
  520    FORMAT(/1X,A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,'); Mortality Multiplier=',F6.2)
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)
      ENDIF
      GO TO 50

C.... Option 6: MISTPRT.

  600 CONTINUE

C.... If this keyword is used, then print 3 statistical summary
C.... tables.  This is separate from MISTABLE.
C.... Check for DBH threshold for large trees.  Trees this size
C.... and above will be counted in DMR/DMI statistics.

      PRTMIS=.TRUE.

      IF(LNOTBK(1).AND.ARRAY(1).GE.0.0) DMRMIN=ARRAY(1)*CMtoIN
      IF(LKECHO)WRITE(JOSTND,620) KEYWRD,DMRMIN*INtoCM
  620 FORMAT(/1X,A8,'   Print mistletoe summary output; ',
     &   'DMRs and DMIs calculated for trees with DBH >=',F4.1)

      GO TO 50

C.... Option 7: MISTGMOD.

  700 CONTINUE

C.... Check for mistletoe cutting growth modification proportion values.
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

      IF (ISPL .LT. 0 .OR. ISPL .GT. MAXSP) THEN
         CALL KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE (JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO (.TRUE.,4)

C.... Process growth modification proportions from supplemental record

      ELSE
         ARRAY(1) = ARRAY(2)
         READ (IREAD,*,END=60) (ARRAY(I+1),I=1,6)

C....    Insert activity into schedule.

         CALL OPNEW (KODE,IDT,2005,7,ARRAY(1))
         IF (KODE .GT. 0) GOTO 50
         IF(LKECHO)WRITE (JOSTND,750) KEYWRD,IDT,KARD(2),ISPL
  750    FORMAT (/1X,A8,'   Date/Cycle=',I5,'; Species= ',A3,
     &          ' (',I3,'):')
         DO 790 I=1,6
            IF(LKECHO)WRITE (JOSTND,780) I,ARRAY(I+1)
  780       FORMAT (1X,' DMR=',I1,';  growth modification proportion=',
     &              F10.2)
  790    CONTINUE
      ENDIF
      GOTO 50

C.... Option 8: MISTOFF.

  800 CONTINUE

C.... Check for mistletoe processing ON/OFF option.  If this keyword is
C.... used, then ignore the dwarf mistletoe in the stand.  The default
C.... is to run with mistletoe if any exists.

      MISFLG=.FALSE.

      IF(LKECHO)WRITE(JOSTND,820) KEYWRD
  820 FORMAT(/1X,A8,'   Mistletoe stand infection ignored.')
      GO TO 50

C.... Option  9: ** OPEN SPOT **

  900 CONTINUE
      GO TO 50

C.... Option 10: MISTPINF

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
 1020    FORMAT(/1X,A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,'); Proportion to Infect=',F10.2,';')
         IF(LKECHO)WRITE(JOSTND,1021) ARRAY(4),ARRAY(5)
 1021    FORMAT(1X,8X,'   Infection Level=',F10.2,'; Infection Method=',
     &      F10.2)
      ENDIF
      GO TO 50

C.... Option 11: MISTABLE

 1100 CONTINUE

C.... Check for mistletoe detail (species/DBH) table output preference.
C.... ARRAY(1): effective date, ARRAY(2): species number

C.... Check for blank date.

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF(ISPL.EQ.-999) GOTO 50

C.... Check for valid species code.

      IF(ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Insert activity into schedule.

         CALL OPNEW(KODE,IDT,2007,1,ARRAY(2))
         IF(KODE.GT.0) GO TO 50
         IF(LKECHO)WRITE(JOSTND,1120) KEYWRD,IDT,KARD(2),ISPL
 1120    FORMAT(/1X,A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',
     &      I3,')')
      ELSE
         CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         WRITE(JOSTND,8000) ISPL,MAXSP
         CALL ERRGRO(.TRUE.,4)
      ENDIF
      GO TO 50

C.... Option 12: NEWSPRED (USING NEW SPREAD AND INTENSIFICATION MODEL).

 1200 CONTINUE

      NEWMOD = .TRUE.
      WRITE(JOSTND,1220) KEYWRD
 1220 FORMAT(/1X,A8,'   New Spread & Intensification Model in use')
      GO TO 50

C.... Option 13: DMCRTHRD
C
C     Change the default values for initial distribution of DM in the
C     crown thirds.

 1300 CONTINUE

      WRITE(JOSTND,1301) KEYWRD
 1301 FORMAT(/1X,A8,' Default initial crownthird distribution changed')

      DO 1302 I= 1,5
        READ(IREAD,1303) DMT(I,1), DMT(I,2), DMT(I,3)
 1302 CONTINUE
 1303 FORMAT (3I10)

C     Check that crownthird values sum to DMR.

      LCHK(1)=.TRUE.
      DO 1310 I= 1,5
        J= DMT(I,1) + DMT(I,2) + DMT(I,3)
        IF (I .NE. J) THEN
          WRITE(JOSTND,1311) I
          LCHK(1)=.FALSE.
        END IF
 1310 CONTINUE
 1311 FORMAT(/1X,'** ERROR: DMR class:',I2,', Sum of thirds .NE. DMR,',
     >         'Change not made.')

      IF (LCHK(1)) THEN
        WRITE(JOSTND,1307)
        DO 1306 I= 1,5
          WRITE(JOSTND,1309) I, DMT(I,1), DMT(I,2), DMT(I,3)
          DMDMR(I,1)= DMT(I,1)
          DMDMR(I,2)= DMT(I,2)
          DMDMR(I,3)= DMT(I,3)
 1306   CONTINUE
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        CALL ERRGRO(.TRUE.,4)
      END IF

 1307 FORMAT(T11,'  DMR    Upper    Mid   Lower')
 1309 FORMAT(T7,4I8)

      GO TO 50

C     Option 14: DMSED
C     Provide a new seed for the random number routine. If no seed
C     is provided (or a negative number), the clock is used.

 1400 CONTINUE

      IF (LNOTBK(1).AND.ARRAY(1).EQ.0.0) CALL GETSED (ARRAY(1))
      CALL DMRNSD (.TRUE., ARRAY(1))
      WRITE (JOSTND,1401) KEYWRD,ARRAY(1)
 1401 FORMAT (/1X,A8,'   RANDOM SEED IS:',F14.1)
      GO TO 50

C     Option 15: DMOPAQ
C     Change the relative opacity of foliage, by species.

 1500 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): relative opacity.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=1.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid opacity.

         IF (ARRAY(2).LT.0.0) THEN
            WRITE(JOSTND,8100) ARRAY(2)
            ARRAY(2) = 1.0
         ENDIF

        WRITE(JOSTND,1501) KEYWRD,KARD(1),ISPL,ARRAY(2)
 1501   FORMAT(/1X, A8, 'Relative opacity (LP=1, PP=1) for ',
     >    'foliage of species: ', A3, ' (', I2,') changed to: ', F4.2)
        IF (ISPL .GT. 0) THEN
          DMOPAQ(ISPL) = ARRAY(2)
	  ELSE
	    DO 1502 J = 1, MAXSP
            DMOPAQ(J) = ARRAY(2)
 1502     CONTINUE
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 16: DMKTUNE
C     Alter the critical DMR class that determines if an infected
C     individual is to be a potential Source.

 1600 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): threshold DMR.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=0.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid threshold DMR.

         IF (ARRAY(2).LT.0.0.OR.ARRAY(2).GT.6) THEN
            WRITE(JOSTND,8150) ARRAY(2)
            ARRAY(2)=0.0
         ENDIF

        WRITE(JOSTND,1601) KEYWRD,KARD(1),ISPL,NINT(ARRAY(2))
 1601   FORMAT(/1X, A8, 'Critical DMR for spreading DM from a ',
     >    'tree of species: ', A3, ' (', I2, ') changed to ', I1)
	  IF (ISPL .GT. 0) THEN
          DMKTUN(ISPL) = ARRAY(2)
	  ELSE
	    DO 1602 J = 1, MAXSP
	      DMKTUN(J) = ARRAY(2)
 1602     CONTINUE
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 17: DMSTUNE
C     Alter the spread multiplier

 1700 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): multiplier.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=0.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid multiplier.

         IF (ARRAY(2).LT.0.0) THEN
            WRITE(JOSTND,8150) ARRAY(2)
            ARRAY(2)=0.0
         ENDIF

        WRITE(JOSTND,1701) KEYWRD,KARD(1),ISPL,ARRAY(2)
 1701   FORMAT(/1X, A8, 'Spread scaling factor for DM on ',
     >    'species: ', A3, ' (', I2, ') changed to ', F6.2)
	  IF (ISPL .GT. 0) THEN
          DMSTUN(ISPL) = ARRAY(2)
	  ELSE
	    DO 1702 J = 1, MAXSP
	      DMSTUN(J) = ARRAY(2)
 1702     CONTINUE
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 18: DMITUNE
C     Alter the intensification multiplier.

 1800 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): multiplier.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=0.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid multiplier.

         IF (ARRAY(2).LT.0.0) THEN
            WRITE(JOSTND,8150) ARRAY(2)
            ARRAY(2)=0.0
         ENDIF

        WRITE(JOSTND,1801) KEYWRD,KARD(1),ISPL,ARRAY(2)
 1801   FORMAT(/1X, A8, 'Intensification scaling factor for DM ',
     >    'on species: ', A3, ' (', I2, ') changed to ', F6.2)
	  IF (ISPL .GT. 0) THEN
          DMITUN(ISPL) = ARRAY(2)
	  ELSE
	    DO 1802 J = 1, MAXSP
	      DMITUN(J) = ARRAY(2)
 1802     CONTINUE
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 19: DMETUNE
C     Alter the spread and intensification multipliers simultaneously.

 1900 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): multiplier.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=0.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid multiplier.

         IF (ARRAY(2).LT.0.0) THEN
            WRITE(JOSTND,8150) ARRAY(2)
            ARRAY(2)=0.0
         ENDIF

        WRITE(JOSTND,1901) KEYWRD,KARD(1),ISPL,ARRAY(2)
 1901   FORMAT(/1X, A8, 'Total establishment scaling factor for DM ',
     >    'on species: ', A3, ' (', I2, ') changed to ', F6.2)
	  IF (ISPL .GT. 0) THEN
          DMETUN(ISPL) = ARRAY(2)
	  ELSE
	    DO 1902 J = 1, MAXSP
	      DMETUN(J) = ARRAY(2)
 1902     CONTINUE
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 20: DMLIGHT
C
C Modify the coefficients describing the latent -> immature forward
C and backward transitions.  The default values for the forward
C transition low and high (DMLT) are 0 and 1.  The default
C back-transition values are 1 and 0.  For the forward case these
C values mean that at 0% light, none of the latent will move to
C immature; at 100% light all the latent will move to immature. The
C back-transition values are similar, but govern the immature ->
C latent case.  The user-supplied values are the values of 'y' which
C result from the 0.0 and 1.0 values of 'x', where 'y' is the
C proportion making the transition and 'x' is the 0-1 light level.

 2000 CONTINUE

C Assign default values. Paste species-specific coefficients from
C species 1 default when 'all species' is indicated by a 0 in ARRAY(1).

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C Zero the matrix to hold coefficients for the forward and bacward functions

      DO 2001 I = 1, 2
        DO 2002 J = 1, 2
          DO 2003 K = 1, 4
            DMLRX(I, J, K) = 0.
 2003     CONTINUE
 2002   CONTINUE
 2001 CONTINUE

C Check for valid species code. Pick up at most 4 points for
C forward and backward reactions.

      LTNM = KEYWRD

      IF (ISPL .GE. 0 .AND. ISPL .LE. MAXSP) THEN
        LTNP(1) = 0
        LTNP(2) = 0
        LCHK(1) = .TRUE.
        LCHK(2) = .TRUE.

        DO 2005 i = 1, 4
          CALL KEYRDR(IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,ARRAY,IRECNT,
     >       KODE,KARD,LFLAG,LKECHO)

C Errors; 0=no error, 1=first column blank (OK here), 2=EOF.

          IF(KODE .EQ. 2) THEN
            CALL ERRGRO(.FALSE., 2)
            GO TO 50
          ENDIF

C If -999 is encountered, all values have been read; skip
C If all fields are blank all values have been read; skip

          IF (INDEX(KEYWRD, '-999') .GT. 0) GOTO 2006
          IF (.NOT. LNOTBK(1) .AND.
     >        .NOT. LNOTBK(2) .AND.
     >        .NOT. LNOTBK(3) .AND.
     >        .NOT. LNOTBK(4)) GOTO 2006

C Read Light (Forward reaction) point. If entry is absent, set LCHK(1).

          IF(LCHK(1) .AND. LNOTBK(1) .AND. LNOTBK(2)) THEN
            LTNP(1) = LTNP(1) + 1
            DMLRX(1, 1, LTNP(1)) = ARRAY(1)
            DMLRX(1, 2, LTNP(1)) = ARRAY(2)
          ELSE
            LCHK(1) = .FALSE.
          ENDIF

C Read Dark (Backward reaction) point. If entry is absent, set LCHK(2).

          IF(LCHK(2) .AND. LNOTBK(3) .AND. LNOTBK(4)) THEN
            LTNP(2) = LTNP(2) + 1
            DMLRX(2, 1, LTNP(2)) = ARRAY(3)
            DMLRX(2, 2, LTNP(2)) = ARRAY(4)
          ELSE
            LCHK(2) = .FALSE.
          ENDIF

 2005   CONTINUE
 2006   CONTINUE

C The forward reaction is indexed with 1; the backward reaction with 2.
C Possible errors are: (1) not enought points; (2) points not sorted in x;
C (3) points have x-values outside 0-1 range. After determining an error type,
C print out a message and the values.

        RTYPE(1) = 'FORWARD'
        RTYPE(2) = 'BACKWARD'

        DO 2010 I = 1, 2

          ERRT(I) = 0
          LCHK(I) = .TRUE.
          IF (LTNP(I) .LE. 1) THEN
            LCHK(I) = .FALSE.
            ERRT(I) = 1
            GOTO 2013
          ENDIF

          DO 2011 J = 2, LTNP(I)
            IF (DMLRX(I, 1, J) .LE. DMLRX(I, 1, J-1)) THEN
              LCHK(I) = .FALSE.
              ERRT(I) = 2
              GOTO 2013
            ENDIF
 2011     CONTINUE

          DO 2012 J = 2, LTNP(I)
            IF ( (DMLRX(I, 1, J) .LT. 0.0) .OR.
     >           (DMLRX(I, 1, J) .GT. 1.0) .OR.
     >           (DMLRX(I, 2, J) .LT. 0.0) .OR.
     >           (DMLRX(I, 2, J) .GT. 1.0) ) THEN
              LCHK(I) = .FALSE.
              ERRT(I) = 3
              GOTO 2013
            ENDIF
 2012     CONTINUE

 2013     CONTINUE

          IF (LCHK(I)) THEN
            WRITE(JOSTND, 2020) LTNM, RTYPE(I), ISPL
 2020       FORMAT(/1X, A8,A8, ' reaction function default',
     >        ' values for species ',I2,' WILL be changed.')
          ELSEIF (.NOT.LCHK(I) .AND. LTNP(I) .GT. 0) THEN
            WRITE(JOSTND,2021) LTNM, RTYPE(I), ISPL, ERRT(I)
 2021       FORMAT(/1X, A8,'Error reading ', A8, ' reaction',
     >        ' function for species: ',I2,'. Error is of',/,' type ',
     >        I1,'; where 1 means not enough points (<2), 2 means',
     >        ' the x-values',/,' are not sorted in ascending order',
     >        ' and 3 means the x or y values are',/,' not within',
     >        ' the 0-1 range. Default values WILL NOT be changed.')
          ENDIF

C Write out values if they were changed or in error.

          IF (LTNP(I) .GT. 0) THEN
            WRITE(JOSTND,2025) RTYPE(I), LTNP(I)
 2025       FORMAT(/1X,A8, ' reaction points = ',I1,/)
            WRITE(JOSTND,2026)
 2026       FORMAT('            X         Y')
            DO 2040 J = 1, LTNP(I)
              WRITE(JOSTND,2041) J, DMLRX(I,1,J), DMLRX(I,2,J)
 2041         FORMAT(I2,':',2(3X,F7.3))
 2040       CONTINUE
          ENDIF

 2010   CONTINUE

C Copy user-specified values over defaults if everything is OK.

        DO 2030 I = 1,2

          IF (ISPL .EQ. 0) THEN
            J = 1
            K = MAXSP
          ELSE
            J = ISPL
            K = ISPL
          ENDIF

          IF(LCHK(I)) THEN
            DO 2031 M = J, K
              DO 2032 N = 1, 2
                DO 2033 P = 1, LTNP(I)
                  DMLTRX(M,I,N,P) = DMLRX(I,N,P)
 2033           CONTINUE
              DMLTNP(M,I) = LTNP(I)
 2032         CONTINUE
 2031       CONTINUE
          ENDIF

 2030   CONTINUE
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8800) ISPL,MAXSP
      ENDIF

      GO TO 50

C     Option 21: DMDEATH
C     Annual death rate of DM plants, regardless of latency or maturity.

 2100 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): death rate.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=0.08

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid death rate.

         IF (ARRAY(2).LT.0.0.OR.ARRAY(2).GT.1.0) THEN
            WRITE(JOSTND,8600) ARRAY(2)
            ARRAY(2)=0.08
         ENDIF

        WRITE(JOSTND,2101) KEYWRD,KARD(1),ISPL,ARRAY(2)
 2101   FORMAT(/1X, A8, 'Annual mortality for DM on',
     >    'species: ', A3, ' (', I2, ') changed to ', F5.3)
	  IF (ISPL .GT. 0) THEN
          DMDETH(ISPL) = ARRAY(2)
	  ELSE
	    DO 2102 J = 1, MAXSP
	      DMDETH(J) = ARRAY(2)
 2102     CONTINUE
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 22: DMCAP
C     Cap the total density of infection.

 2200 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): capping factor.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=0.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid capping factor.

         IF (ARRAY(2) .LT. 0.0) THEN
            WRITE(JOSTND,8150) ARRAY(2)
            ARRAY(2) = 0.0
         ENDIF

        WRITE(JOSTND,2201) KEYWRD,KARD(1),ISPL,ARRAY(2)
 2201   FORMAT(/1X, A8, 'Capping factor for DM on ',
     >    'species: ', A3, ' (', I2, ') changed to ', F6.2)
        IF (ISPL .GT. 0) THEN
          DMCAP(ISPL) = ARRAY(2)
        ELSE
          DO 2202 J = 1, MAXSP
            DMCAP(J) = ARRAY(2)
 2202     CONTINUE
        END IF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 23: DMCLUMP
C     Select a spatial clumping term. The user supplies a year in
C     which the clumping term is to be set, along with a non-zero
C     value for the variance/mean ratio. By default, the system
C     determines the variance/mean ratio from the simulated stems/ha
C     of the sample points.

 2300 CONTINUE

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

      IF (LNOTBK(2) .AND. (ARRAY(2) .GT. DMTINY)) THEN
        CALL OPNEW(KODE,IDT,2008,1,ARRAY(2))
        IF(KODE.GT.0) GO TO 50
        WRITE(JOSTND, 2301) KEYWRD, IDT, ARRAY(2)
 2301   FORMAT(/1X, A8, '   Date/Cycle=', I5,
     >    ': Stems/ha variance/mean ratio will be changed to', F7.3)
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        CALL ERRGRO(.TRUE.,4)
      END IF
      GO TO 50

C     Option 24: DMAUTO (DMALPHA and DMBETA)
C     Select the decay term for the terms of the double exponential.
C     If both are missing, an error occurs; if either is missing, a
C     dummy value is substituted and detected in DMOPTS

 2400 CONTINUE

      IDT=1
      IF(LNOTBK(1)) IDT=IFIX(ARRAY(1))

      IF (LNOTBK(2)) THEN
        IF (ARRAY(2) .GT. 0.) ARRAY(2)= -999.
      ELSE
        ARRAY(2)= -999.
      END IF

      IF (LNOTBK(3)) THEN
        IF (ARRAY(3) .GT. 0.) ARRAY(3)= -999.
      ELSE
        ARRAY(3)= -999.
      END IF

      CALL OPNEW(KODE,IDT,2009,2,ARRAY(2))
      IF(KODE.GT.0) GO TO 50

      IF (ARRAY(2)  .NE. -999.) THEN
        WRITE(JOSTND, 2401) KEYWRD, IDT, ARRAY(2)
 2401   FORMAT(/1X, A8, '   Date/Cycle=', I5,
     >   ': Autocorrelation alpha is: ', F7.3)
      ELSE
        WRITE(JOSTND, 2402) KEYWRD, IDT
 2402   FORMAT(/1X, A8, '   Date/Cycle=', I5, ': Autocorrelation alpha',
     >      ' value is missing or greater than zero. Current value',
     >      ' not changed.')
      END IF

      IF (ARRAY(3) .NE. -999.) THEN
        WRITE(JOSTND, 2403) KEYWRD, IDT, ARRAY(3)
 2403   FORMAT(/1X, A8, '   Date/Cycle=', I5,
     >   ': Autocorrelation beta is: ', F7.3)
      ELSE
        WRITE(JOSTND, 2404) KEYWRD, IDT
 2404   FORMAT(/1X, A8, '   Date/Cycle=', I5, ': Autocorrelation beta',
     >     ' value is missing or greater than zero. Current value',
     >     ' not changed.')
      END IF

      GO TO 50

C     Option 25: DMTABLE
C     ENABLE PRINTING OF DM-TREELIST AND LIGHT TABLES TO DATABASE

 2500 CONTINUE
      LDETAIL = .TRUE.
      WRITE(JOSTND,2501) KEYWRD
 2501 FORMAT(/1X,A8,'  DM-Treelist table printing is on')
      GO TO 50

C     Option 26: SAREA
C     The area of the stand, used for selecting the likelihood of being
C     selected as a target record.

 2600 CONTINUE

C >> DEFUNCT <<

      GO TO 50

C     Option 27: DMOPQMLT
C     Change the relative default opacity of all species. Use this to
C     calibrate the relative values of DMOPAQ().

 2700 CONTINUE

      IF (LNOTBK(1) .AND. (ARRAY(1) .GT. 0.0)) THEN
        X = ARRAY(1)
        WRITE(JOSTND, 2701) KEYWRD, X
 2701   FORMAT(/1X, A8, ' Relative P(stopping DM seed) IN 1 M**3 of ',
     &          'avg. PP/LP foliage will be multiplied by: ', F4.2)
        DMOPQM = X
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 28: DMFLOWER
C     Change the number of years required to become flowering, once
C     latency is ended and immaturity begins.

 2800 CONTINUE

C.... ARRAY(1): species code,  ARRAY(2): delay in years.
C.... Set defaults.

      IF (.NOT.LNOTBK(1)) ARRAY(1)=0.0
      IF (.NOT.LNOTBK(2)) ARRAY(2)=4.0

C.... Get species code.

      CALL SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) GOTO 50

C.... Check for valid species code.

      IF (ISPL.GE.0.AND.ISPL.LE.MAXSP) THEN

C....    Check for valid delay.

        IF (ARRAY(2).LT.1.0) THEN
          WRITE(JOSTND,8700) ARRAY(2)
          ARRAY(2)=4.0
        ENDIF

        WRITE(JOSTND,2801) KEYWRD,KARD(1),ISPL,NINT(ARRAY(2))
 2801   FORMAT(/1X, A8, ' Years to flowering for species: ',
     >          A3, ' (', I2, ') changed to ', I3, ' years.')
	  IF (ISPL .GT. 0) THEN
          DMFLWR(ISPL) = NINT(ARRAY(2))
	  ELSE
	    DO J = 1,MAXSP
	      DMFLWR(J) = NINT(ARRAY(2))
          ENDDO
	  ENDIF
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        WRITE (JOSTND,8000) ISPL,MAXSP
        CALL ERRGRO(.TRUE.,4)
      END IF

      GO TO 50

C     Option 29: DMBCI:	Biological control initiation

 2900 CONTINUE

	LOK = .TRUE.
      IF (.NOT.LNOTBK(1)) LOK = .FALSE.
	I = IFIX(ARRAY(1))
	IF (I .LT. 1 .OR. I .GT. MAXBC) LOK = .FALSE.

      CALL SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &            ARRAY,KARD)
      IF (ISPL.EQ.-1) LOK = .FALSE.

	IF (LOK) THEN
	  READ(IREAD,2901,END=60) (BC(I).Mort(K),  K=1,ACTIVE)
        IRECNT=IRECNT+1
        READ(IREAD,2901,END=60) (BC(I).Suprs(K), K=1,ACTIVE)
        IRECNT=IRECNT+1
        READ(IREAD,2901,END=60) (BC(I).Yr(K),    K=1,ACTIVE)
        IRECNT=IRECNT+1
        DO K = 1,ACTIVE ! bound the values
	    BC(I).Mort(K)  = MIN(100.0, MAX(0.0, BC(I).Mort(K)))  ! 0,   100%
	    BC(I).Suprs(K) = MIN(100.0, MAX(0.0, BC(I).Suprs(K))) ! 0,   100%
	    BC(I).Yr(K)    = MIN(999.0, MAX(0.04, BC(I).Yr(K)))   ! 0.04,999yr
          IF (BC(I).Yr(K) .GE. 0.04) THEN
	      BC(I).HfLf(K) = EXP(LOG(0.5)/BC(I).Yr(K))
	    ENDIF
	  ENDDO
	ENDIF
 2901 FORMAT(4F10.0)

 	IF (LOK) THEN
	  BC(I).Spp = ISPL
        WRITE(JOSTND,2902) KEYWRD,I,KARD(2)(1:3),
     >  BC(I).Spp,
     >  (BC(I).Mort(K),  K=1,ACTIVE),
     >  (BC(I).Suprs(K), K=1,ACTIVE),
     >  (BC(I).Yr(K),    K=1,ACTIVE)
 2902   FORMAT(/1X,A8,
     >  /T8,'Definition of Biocontrol Agent: ',I2,
     >  /T8,'Host species: ',A,' (CODE= ',I2,')',
     >  /T8,'Immediate Mortality (%)',  4F10.1,
     >  /T8,'Suppression (%)        ',  4F10.1,
     >  /T8,'Suppr''n half-life (yr) ', 4F10.1)
      ELSE
        CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        CALL ERRGRO(.TRUE.,4)
      END IF
      GO TO 50

C     Option 30: DMBCA:	Biological control application

 3000 CONTINUE
      IDT = 1
	IKD = 2004
      IF (LNOTBK(1)) IDT=IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
        IF (IPRMPT.NE.2) THEN
          CALL KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
          CALL ERRGRO (.TRUE.,25)
        ELSE
          CALL OPNEWC (KODE,JOSTND,IREAD,IDT,IKD,KEYWRD,KARD,
     >                 IPRMPT,IRECNT,ICYC)
        ENDIF
      ELSE
        IF(.NOT.LNOTBK(2)) ARRAY(2) = 1.0
        IF(.NOT.LNOTBK(3)) ARRAY(3) = 1.0
        IF(.NOT.LNOTBK(4)) ARRAY(4) = FLOAT(MXHT*MESH)
	
	  ARRAY(2) = FLOAT(IFIX(MIN(FLOAT(MAXBC), MAX(1.0, ARRAY(2)))))
	  ARRAY(3) = MIN(20.0,             MAX(0.0, ARRAY(3)))
	  ARRAY(4) = MIN(FLOAT(MXHT*MESH), MAX(0.0, ARRAY(4)))

        WRITE (JOSTND,3010) KEYWRD,IDT,IFIX(ARRAY(2)),
     >                      ARRAY(3),ARRAY(4)
 3010   FORMAT (/1X,A8,'   DATE/CYCLE=',I5,
     >  '; Application of Biocontrol Agent:', I2,
     >  /T13,'Strength: ',F7.2, '; Height (m): ', F7.2)
        CALL OPNEW (KODE,IDT,IKD,3,ARRAY(2))
        IF (KODE.GT.0) THEN
          CALL KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
          CALL ERRGRO(.TRUE.,4)
        ENDIF
	ENDIF
      GO TO 50

C====================
C.... Error messages.

 8000 FORMAT(/1X,' *** Species specified,',I3,', outside range of',
     &       ' valid species ( 0 -',I3,' )')
 8100 FORMAT(/1X,' *** Value specified,',F10.4,', negative; set to',
     &       ' 1.0.')
 8150 FORMAT(/1X,' *** Value specified,',F10.4,', negative; set to',
     &       ' 0.0.')
 8200 FORMAT(/1X,' *** DMR specified,',I3,', outside range of',
     &       ' valid values ( 0 - 6 )')
 8300 FORMAT(/1X,' *** Proportion to be infected, ',F10.4,', outside',
     &       ' range of valid values ( 0.0 - 1.0 )')
 8400 FORMAT(/1X,' *** DMR infection specified,',I3,', outside range',
     &       ' of valid values ( 1 - 6 )')
 8500 FORMAT(/1X,' *** Method of infection specified,',I3,', outside',
     &       ' range of valid values ( 0 - 2 )')
 8600 FORMAT(/1X,' *** Death rate specified,',F6.2,', outside',
     &       ' range of valid values ( 0 - 1 ).  Default 0.08 used.')
 8700 FORMAT(/1X,' *** Years delay specified,',F6.2,', less than 1.',
     &       '  Default 4.0 used.')
 8800 FORMAT(/1X,' *** Backwards coefficients for species: ', A3,
     &       ' (', I2, ') - not used.')

C.... Common return.

 9000 CONTINUE

      IF(DEBUG)WRITE(JOSTND,9010)ICYC
 9010 FORMAT(' End MISIN: Cycle = ',I5)
      GO TO 9020

C.... Special entry to retrieve keywords.

      ENTRY MISKEY(KEY,PASKEY)
      PASKEY=TABLE(KEY)

 9020 CONTINUE
      RETURN
      END
