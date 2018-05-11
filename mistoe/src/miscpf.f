      SUBROUTINE MISCPF(PRFCUT)
***********************************************************************
C MISTOE $Id: miscpf.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Processes the MISTPREF mistletoe cutting preference keyword.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     PRFCUT: (O) Array containing cutting preference values by species
*                and mistletoe rating, as input on MISTPREF keyword.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on or off.
*     I:      Loop counter.
*     IACTK:  Passed back from OPGET (unused).
*     IDATE:  Passed back from OPGET (unused).
*     IDMR:   Loop index for DM rating.
*     ISPC:   Current species number.
*     NP:     Passed back from OPGET (unused).
*     NTODO:  Number of actions to perform in a cycle.
*     PRMS:   Array containing MISPREF keyword values.
*
*  Common block variables and parameters:
*     ICYC:   From CONTRL; cycle index number.
*     IY:     From CONTRL; inventory year.
*     JOSTND: From CONTRL; unit number of stand output.
*     MAXSP:  From PRGPRM; maximum number of species.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).
*     PRFMST: From MISCOM; mistletoe cutting preference values.
*
* Revision History:
*     02-AUG-99; Lance R. David (FHTET-FC)
*        Added definition and data statements for MYACTS array
*        and replaced the activity code literal in the CALL OPFIND
*        statements.
*        This change was provided by Bob Havis (FMSC) to eliminate
*        LF95 FORTRAN compiler warnings.
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      REAL    PRFCUT(MAXSP,6),PRMS(7)
      INTEGER MYACTS(1)
      INTEGER I,IACTK,IDATE,IDMR,ISPC,NP,NTODO

C.... Data statements.
C
      DATA MYACTS(1) / 2002 /
C
C.... Check for debug.

      CALL DBCHK(DEBUG,'MISCPF',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,100) ICYC
  100 FORMAT(' Begin MISCPF: Cycle = ',I5)

C.... Process MISTPREF keyword (activity code 2002) if scheduled.
C
       CALL OPFIND(1,MYACTS(1),NTODO)
      IF(NTODO.GT.0) THEN
         DO 150 I=1,NTODO
            CALL OPGET(I,7,IDATE,IACTK,NP,PRMS)
            CALL OPDONE(I,IY(ICYC))

C....       Check for a particular species number.
C....       MISFIT insures that only species of this variant that
C....        are affected by mistletoe will get pref. multipliers -
C....        others will be set to 0.

            ISPC=IFIX(PRMS(1))
            IF(ISPC.NE.0) THEN
               DO 120 IDMR=1,6
                  PRFMST(ISPC,IDMR)=PRMS(IDMR+1)*MISFIT(ISPC)
  120          CONTINUE
            ELSE

C....          Otherwise defaults to all species.

               DO 140 ISPC=1,MAXSP
                  DO 130 IDMR=1,6
                     PRFMST(ISPC,IDMR)=PRMS(IDMR+1)*MISFIT(ISPC)
  130             CONTINUE
  140          CONTINUE
            ENDIF
  150    CONTINUE
      ENDIF

C.... Copy values from PRFMST to PRFCUT; PRFMST is saved in a common
C.... block so that cutting preference values remain in effect until
C.... a subsequent MISTPREF keyword changes the values and therefore
C.... subroutine CUTS can use PRFCUT for every cycle.

      IF(DEBUG) WRITE(JOSTND,9010) (I, I=1,6)
 9010 FORMAT(' Species/DMR ', 6I10)

      DO 170 ISPC=1,MAXSP
         DO 160 IDMR=1,6
            PRFCUT(ISPC,IDMR)=PRFMST(ISPC,IDMR)
  160    CONTINUE

      IF(DEBUG) WRITE(JOSTND,9020) ISPC,(PRFCUT(ISPC,I), I=1,6)
 9020 FORMAT(I5, 9X, 6F10.2)

  170 CONTINUE

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,9040) ICYC
 9040 FORMAT(' End MISCPF: Cycle = ',I5)

      RETURN
      END
