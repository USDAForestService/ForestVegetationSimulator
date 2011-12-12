      FUNCTION EMERG(BY,T,INT)
      IMPLICIT NONE
C----------
C  **EMERG         DATE OF LAST REVISION:  07/08/11
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
      DOUBLE PRECISION C
      INTEGER T, INT
      REAL EMERG,BY
      
      IF(T .NE. 0) GO TO 1
      C = 1.0D00/2**INT
      GO TO 2
    1 C = C*(INT-T+1)/T
    2 EMERG = BY*C
      RETURN
      END
