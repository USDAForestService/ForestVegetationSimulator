      SUBROUTINE FXNOTE
      IMPLICIT NONE
C----------
C  **FXNOTE                 DATE OF LAST REVISION:  07/14/10
C----------
C
C     This module satisfies the external reference to the module
C     TXNOTE when the disease extensions. other than the ??X.exe 
C     executables are being linked (RNH May98)
C     TXNOTE is called by:
C     - BWIN  where LXNOTE is passed by BWCNTL.F77
C     - DFBIN where LXNOTE is passed by DFBCOM.F77
C     - MPBIN where LXNOTE is passed by MPBCOM.F77
C     - RRIN  where LXNOTE is passed by RRCOM.F77
C
      LOGICAL LXNOTE

      ENTRY TXNOTE (LXNOTE)
      LXNOTE = .FALSE.
      RETURN
      END
