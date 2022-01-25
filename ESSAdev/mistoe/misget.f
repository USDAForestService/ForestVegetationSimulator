      SUBROUTINE MISGET(ITREE,IDMR)
***********************************************************************
C MISTOE $Id$
*----------------------------------------------------------------------
*  Purpose:
*     Gets dwarf mistletoe rating (stored in array IMIST) for the
*  current tree record.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ITREE:  (I) Current tree record number.
*     IDMR:   (O) Dwarf mistletoe rating for current tree record.
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

      INTEGER IDMR,ITREE
      
C.... Get dwarf mistletoe rating for current tree.

      IDMR=IMIST(ITREE)

      RETURN
      END
