      SUBROUTINE BRPR
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C  Purpose:
C  BRPR performs the following functions:
C     1) calls BRCSTA to assign canker status when a compression
C        has been performed
C     2) calls BRTSTA to calculate the status of a tree
C     3) calls BRSTAT to calculate Blister Rust statistics
C     4) calls BRSUM to load the summary information
C     5) calls BRCOUT to write detailed canker output
C     6) calls BRTOUT to write detailed tree output
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      LOGICAL BRGO, DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRPR',4,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT('Entering subroutine BRPR: cycle = ',I2)

C.... See if Blister Rust model is being used in this simulation.
C.... If not, then return.

      CALL BRATV(BRGO)
      IF(.NOT.BRGO) GO TO 90

C.... When a compression has been performed, canker statuses must be
C.... assigned.

      IF(BRCMP) THEN
         CALL BRCSTA
         BRCMP=.FALSE.
      ENDIF

C.... Call BRTSTA to calcualte status for tree based on the tree's
C.... worst canker.

      CALL BRTSTA

C.... Call BRSTAT to calculate statistics. BRSTAT is called from BRSETP
C.... during the initialization phase.

      IF(.NOT.LSTART) CALL BRSTAT

C.... Call BRSUM to load summary statistics arrays

      CALL BRSUM

C.... Call BRCOUT to print list of cankers of requested.

      IF(BRCL) CALL BRCOUT

C.... Call BRTOUT to print list of trees if requested.

      IF(BRTL) CALL BRTOUT

C.... Common return.

   90 CONTINUE
      IF(DEBUG) WRITE(JOSTND,100) ICYC
  100 FORMAT('Leaving subroutine BRPR: cycle = ',I2)
      RETURN
      END
