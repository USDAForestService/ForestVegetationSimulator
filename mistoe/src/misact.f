      SUBROUTINE MISACT (LACTV)
***********************************************************************
*  **MISACT--MS  Date of last revision:  01/12/94
*----------------------------------------------------------------------
*  Purpose:
*     Returns TRUE when called to signal that the "real" mistletoe
*  submodel is linked to the program.  There is an entry in EXMIST
*  that always returns FALSE.
*
***********************************************************************
      IMPLICIT NONE

      LOGICAL LACTV
      LACTV=.TRUE.

      RETURN
      END
