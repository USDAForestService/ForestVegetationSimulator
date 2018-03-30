      SUBROUTINE RDSSIZ(ISP,A,STCUT,ISL,ISPS,IRTSPC)
      IMPLICIT NONE
C----------
C RD $Id: rdssiz.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  SUBROUTINE FOR ASSIGNING SIZE CLASS TO STUMPS.
C
C  CALLED BY :
C     RDDAM   [ROOT DISEASE]
C     RDEND   [ROOT DISEASE]
C     RDIN    [ROOT DISEASE]
C     RDSADD  [ROOT DISEASE]
C     RDSTR   [ROOT DISEASE]
C
C  CALLS :
C     NONE
C
C  PARAMETERS :
C     ISP    -
C     A      -
C     STCUT  -
C     ISL    -
C     ISPS   -
C     IRTSPC -
C
C  Revision History :
C   03/05/95 - Last revision date.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
      INTEGER ISL, ISP, ISPS(ITOTSP), IRTSPC(MAXSP), J
      REAL    A, STCUT(5)
      REAL DANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = REAL(IRTSPC(1))
      DANUW = REAL(ISP)
      DANUW = REAL(ISPS(1))
C
      DO 100 J = 2,5
         IF (A .LT. STCUT(J-1) .OR. A .GT. STCUT(J)) GOTO 100
         ISL = J - 1
         RETURN
 100  CONTINUE

      ISL = 5

      RETURN
      END

