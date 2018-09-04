      FUNCTION EMERG(BY,T,INCRS)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C   08/22/14 Lance R. David (FMSC)
C     Function name was used as variable name.
C     changed variable INT to INCRS
C----------
      DOUBLE PRECISION C
      INTEGER T, INCRS
      REAL EMERG,BY
      
      IF(T .NE. 0) GO TO 1
      C = 1.0D00/2**INCRS
      GO TO 2
    1 C = C*(INCRS-T+1)/T
    2 EMERG = BY*C
      RETURN
      END
