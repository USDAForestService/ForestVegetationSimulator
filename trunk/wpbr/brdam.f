      SUBROUTINE BRDAM(II,ICODES)
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C  Purpose:
C   This subroutine processes damage codes to determine whether
C   the tree in question is infected with Blister Rust.
C   The Region 1 code for White Pine Blister Rust is 36.
C   ************
C   *** NOTE ***
C   ************
C   At this point nothing is done with this information from the
C   treelist damage codes.
C----------------------------------------------------------------------
C
C   Parameters:
C      II     - current tree ID
C      ICODES - integer array (size 6) holding 3 pairs of damage
C               codes and severity ratings
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Changed literal value for rust damage code to variable IBRDAM.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      INTEGER ICODES(6), II, J, ISEVRT

C.... Process damage codes.

      DO 100 J=1,5,2
         IF(ICODES(J).EQ.IBRDAM) THEN

C....       Get the severity rating code.

            ISEVRT=ICODES(J+1)
            GO TO 110
         ENDIF
  100 CONTINUE

C.... Common return.

  110 CONTINUE
      RETURN
      END
