      SUBROUTINE MISIN0
***********************************************************************
C MISTOE $Id: misin0.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant independent and sets the variant independent variables for
*  other mistletoe routines.
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on and off.
*
*  Common block variables and parameters:
*     DMMMLT: From MISCOM; DM mortality multiplier.
*     DMMTPA: From MISCOM; array containing TPA mortality from DM.
*     FSTMIS: From MISCOM; log. for printing DM summary output tables.
*     FSTTBL: From MISCOM; log. for printing DM detail output tables.
*     ICYC:   From CONTRL; cycle index number.
*     IMIST:  From MISCOM; array containing current tree DMR.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     JRAN:   From MISCOM; seed for DM random number generator.
*     MAXCYC: From PRGPRM; maximum # cycles.
*     MAXSP:  From PRGPRM; maximum # species.
*     MAXTRE: From PRGPRM; maximum # tree records.
*     MISCYC: From MISCOM; logical array used in MISDGF and MISHGF.
*     MISFLG: From MISCOM; logical flag for turning mistletoe on/off.
*     PRFMST: From MISCOM; species removal preference due to DM array.
*     PRTMIS: From MISCOM; log. flag for turning DM printing on/off.
*     USEMRT: From MISCOM; true if using this model's mortality calcs.
*     YNGMLT: From MISCOM; DM spread probability(-) multplier.
*     YPLMLT: From MISCOM; DM spread probability(+) multplier.
*
*
*  Revision History :
*     03/01/95 - Lance R. David (MAG)
*                Initialization of real variable array DMGMLT (size
*                MAXSP) to 1.0 removed due to change MISTGMOD keyword.
*                DMGMLT: From MISCOM; DM diameter growth multiplier.
*     12/05/01 - Lance R. David (FHTET)
*                Initialization of DMRMIN that use to be in MISINTxx.f
*     01/07/04 - Lance R. David (FHTET)
*                Initialization of IDMSOUT and set default printing of 
*                Dwarf Mistletoe summaries 1 and 2.
*     03/14/05 - Lance R. David (FHTET)
*                Removed calls to GETID for assignment of report ID Number.
*                Assignment now made at time of header writing in MISPRT.
*     04/21/09 - Lance R. David (FMSC)
*                Added initialization of varibles IMOUT_. (thanks to
*                Don Robinson)
*     04/01/11 - Lance R. David (FMSC)
*                Added dimension of MISCYC array.
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      INTEGER I,J

C.... No check for debug.
C.... This routine is called before any keywords are processed.
C.... So, no call to DBCHK would work.

C.... Mistletoe model initializations.

      DMRMIN=1.0
      PRTMIS=.FALSE.
      FSTMIS=.TRUE.
      FSTTBL=.TRUE.
      MISFLG=.TRUE.
      USEMRT=.TRUE.
      JRAN=123231

      DO 20, I=1,MAXCYC
         MISCYC(1,I)=.FALSE.
         MISCYC(2,I)=.FALSE.
   20 CONTINUE

      DO 30, I=1,MAXSP
         DO 25, J=1,6
            PRFMST(I,J)=0.0
   25    CONTINUE
         YNGMLT(I)=1.0
         YPLMLT(I)=1.0
         DMMMLT(I)=1.0
   30 CONTINUE

      DO 50 I=1,MAXTRE
         IMIST(I)=0
         DMMTPA(I)=0.0
   50 CONTINUE

      DO I = 1,4
        IDMSOUT(I) = 0
      ENDDO

      IMOUT1 = 0
      IMOUT2 = 0
      IMOUT3 = 0
      IMOUT4 = 0

C.... Common return.

      RETURN
      END
