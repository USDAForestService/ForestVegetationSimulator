      FUNCTION SURFLP (DBH)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C     CALCULATES THE SURFACE AREA OF LPP.
C     FUNCTION IS FROM DON BURNELL.
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
      REAL DBH, SURFLP

      SURFLP = DBH  *  0.672
      IF ( DBH .GT. 5.0 ) SURFLP = 8.835 * DBH - 40.82
      RETURN
      END
