      SUBROUTINE COLDBH
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     INITIALIZATION ROUTINE FOR COLE'S MPB MODEL
C
C Revision History
C   05/31/00 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COLCOM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
COMMONS
C
      INTEGER I,J,I1,I2,INDEX
C
C     INITIALIZE ARRAYS
C
      STOTAL=0.0
      DO 5 I=1,10
        DEADCL(I)=0.0
        START(I)=0.0
        TGREEN(I)=0.0
        TDEAD(I)=0.0
        DO 4 J=1,10
          DEAD(I,J)=0.0
          GREEN(I,J)=0.0
   4    CONTINUE
   5  CONTINUE
C
C     DETERMINE NUMBER OF TREES PER DBH CLASS IN THE STAND
C
C     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
C     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
C
C      I1=ISCT(7,1)
      I1=ISCT(IDXLP,1)
C
      IF (I1.EQ.0) GOTO 20
C
C     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
C     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
C
C      I2=ISCT(7,2)
      I2=ISCT(IDXLP,2)
      DO 10 I=I1,I2
        J=IND1(I)
C
C       LOAD THE STARTING NUMBER OF TREES WITH THE
C       NUMBER OF TREES PER ACRE THAT THIS TREE RECORD
C       REPRESENTS.
C
        CALL COLIND (DBH(J),INDEX)
        START(INDEX)=START(INDEX)+PROB(J)
  10  CONTINUE
  20  CONTINUE
      RETURN
      END








