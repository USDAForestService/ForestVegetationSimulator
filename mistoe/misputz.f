      SUBROUTINE MISPUTZ(ITREE,IDMR)
***********************************************************************
*  **MISPUTZ--MS  Date of last revision:  01/21/94
*----------------------------------------------------------------------
*  Purpose:
*     Sets dwarf mistletoe rating (stored in array IMIST) for the
*  current tree record to the value passed in IDMR, and zeros
*  elements in 3 DM arrays.
*     Entry point MISPUT omits zeroing DM arrays, and is the same
*  as the previous (before Sept. 1993) MISPUT code.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ITREE:  (I) Current tree record number.
*     IDMR:   (I) Mistletoe rating to put into IMIST for current tree.
*
*  Common block variables and parameters:
*     IMIST:  From MISCOM; array containing tree record DMR's.
*
***********************************************************************
      IMPLICIT NONE

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      INTEGER IDMR,ITREE

C.... Set zeros for initial values of DM variables.
C.... At this time, MISPUTZ and MISPUT are equivalent.

      ENTRY MISPUT(ITREE,IDMR)

C.... Check for valid dwarf mistletoe rating.

      IF(IDMR.GE.0.AND.IDMR.LE.6) THEN

C....    Put value into IMIST for current tree record number.

         IMIST(ITREE)=IDMR

      ELSE

C....    Print error message and return.

         PRINT*,' *MISPUT* Invalid dwarf mistletoe rating (',
     &          IDMR, ') input for tree ', ITREE
      ENDIF

      RETURN
      END
