      SUBROUTINE RDSHST
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  SUBROUTINE FOR UPDATING THE STUMP HISTORY ARRAYS
C
C  CALLED BY :
C     RDMN2   [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  Revision History :
C   08/14/12 - Last revision date.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDCOM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'
C
COMMONS
C
      INTEGER I, J, K, KK

      DO 501 I = 1,IRRTRE
         DO 502 J = 1,4
            K = 4 - J + 1
            KK = K - 1
            IF (J .EQ. 4) GOTO 503
            ROOTH(K,I) = ROOTH(KK,I)
            XMTH(K,I) = XMTH(KK,I)
            GOTO 502

  503       CONTINUE
            IF (I .GT. ITRN) GOTO 504
            ROOTH(1,I) = RROOTT(I)
            XMTH(1,I) = WK22(I)
            GOTO 505

  504       CONTINUE
            ROOTH(1,I) = -1.0
            XMTH(1,I) = -1.0

  505       CONTINUE
            RROOTT(I) = 0.0
            WK22(I) = 0.0
  502    CONTINUE
  501 CONTINUE

      RETURN
      END
