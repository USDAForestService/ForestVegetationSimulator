      SUBROUTINE MISDAM(ITREE,ICODES)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
*  **MISDAM--MS  Date of last revision:  07/15/94
*----------------------------------------------------------------------
*  Purpose:
*     Processes damage codes to determine whether the tree in
*  question has dwarf mistletoe. The codes are as follows:
*     30 Dwarf Mistletoe
*     31 Lodgepole Pine Mistletoe
*     32 Western Larch Mistletoe
*     33 Douglas Fir Mistletoe
*     34 Ponderosa Pine Mistletoe
*  Code 30 is recorded if mistletoe is found on a species other
*  than LP, WL, DF or PP.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ICODES: (I) Array of mistletoe damage codes.
*     ITREE:  (I) Current tree record number.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on or off.
*     J:      Loop counter.
*
*  Common block variables and parameters:
*     DMRATE: From DMCOM;
*     ICYC:   From CONTRL; cycle index number.
*     IMIST:  From MISCOM; tree mistletoe rating (Hawksworth 0-6).
*     JOSTND: From CONTRL; unit number of stand output.
*     MISFLG: From MISCOM; logical flag to turn DM effects on or off.
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.
 
      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'
      INCLUDE 'DMCOM.F77'
 
C.... Variable declarations.

      LOGICAL DEBUG
      INTEGER ITREE, J
      INTEGER ICODES(6)

C.... Data statements.

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISDAM',6,ICYC)

C.... Initializations.

      IMIST(ITREE)=0

C.... Check mistletoe processing option; if MISFLG is FALSE then
C.... process without the effects of dwarf mistletoe (skip out of
C.... this routine).

      IF(.NOT.MISFLG) GO TO 9000

C.... Process damage codes.

      DO 100 J=1,5,2
         IF(ICODES(J).EQ.30.OR.ICODES(J).EQ.31.OR.ICODES(J).EQ.32.
     &      OR.ICODES(J).EQ.33.OR.ICODES(J).EQ.34) THEN
            IMIST(ITREE)=ICODES(J+1)
            IF(IMIST(ITREE).GT.6) IMIST(ITREE)=6
            IF(IMIST(ITREE).LT.0) IMIST(ITREE)=0

C           Copy value of IMIST to new S+I variable

            DMRATE(ITREE) = IMIST(ITREE)

         ENDIF
  100 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010)ICYC,ITREE,IMIST(ITREE)
 9010 FORMAT(' MISDAM: Cycle = ',I5,'  IMIST(',I4,')= ',I2)

C.... Common return.

 9000 CONTINUE

      RETURN
      END
