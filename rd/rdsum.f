      SUBROUTINE RDSUM(N,A1,A2,IYEND)
      IMPLICIT NONE
C----------
C  **RDSUM       LAST REVISION:  09/03/14
C----------
C
C  SUBROUTINE TO SUM UP THE ARRAY PROBI(1350,41)
C
C  CALLED BY :
C     RDBBDO  [ROOT DISEASE]
C     RDEND   [ROOT DISEASE]
C     RDESTB  [ROOT DISEASE]
C     RDGROW  [ROOT DISEASE]
C     RDINF   [ROOT DISEASE]
C     RDINSD  [ROOT DISEASE]
C     RDMN2   [ROOT DISEASE]
C     RDMORT  [ROOT DISEASE]
C     RDSETP  [ROOT DISEASE]
C     RDSTR   [ROOT DISEASE]
C     RDTREG  [ROOT DISEASE]
C
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     N      - NUMBER OF TREE RECORDS TO SUM UP OVER
C     A1     - ARRAY TO SUM UP INTO
C     A2     - ARRAY TO BE SUMMED UP
C     IYEND  - NUMBER OF YEARS TO SUM UP OVER
C
C  Revision History :
C   11/23/93 - Last revision date.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'RDPARM.F77'
C
COMMONS
C

      INTEGER I, IYEND, J, N
      REAL    A1(IRRTRE), A2(IRRTRE,41,2)

      IF (N .LE. 0) RETURN

      DO 1000 I = 1,N
         A1(I) = 0.0

         DO 900 J = 1,IYEND
            A1(I) = A1(I) + A2(I,J,1) + A2(I,J,2)
  900    CONTINUE
 1000 CONTINUE

      RETURN
      END
