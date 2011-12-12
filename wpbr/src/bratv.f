      SUBROUTINE BRATV(L)
C**********************************************************************
C  **BRATV        DATE OF LAST REVISION:  05/01/1994
C----------------------------------------------------------------------
C  Purpose:
C  BRATV returns L=.TRUE. to indicate that the Blister Rust model is
C  available from the current link and is actually being called.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations

      LOGICAL L

      L=BRYES

C.... Common return

      RETURN
      END
