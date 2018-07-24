      SUBROUTINE TXNOTE (LXNOTE)
      IMPLICIT NONE
C----------
C WSBWE $Id$
C----------
C
C     This module sets the logical flag LXNOTE to true when the 
C     ??X.exe executables are being linked.  This flag causes,
C     the following warning to be written in the disease model output
C     (RNH May98). LXN_ON is set to true after the warning has been
C     written once during a model execution to prevent multiple messages
C     in one output file
C
C     TXNOTE is called by:
C     - BWIN  where LXNOTE is passed by BWCNTL.F77
C     - DFBIN where LXNOTE is passed by DFBCOM.F77
C     - MPBIN where LXNOTE is passed by MPBCOM.F77
C     - RRIN  where LXNOTE is passed by RRCOM.F77
C
      LOGICAL LXNOTE, LXN_ON

      IF (LXN_ON) THEN
         LXNOTE = .FALSE.
      ELSE
         LXNOTE = .TRUE.
         LXN_ON = .TRUE.
      ENDIF

      RETURN
      END
