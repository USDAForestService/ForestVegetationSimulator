      SUBROUTINE RDSHST
C----------
C  **RDSHST      LAST REVISION:  08/14/12
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
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'RDPARM.F77'
C
C
      INCLUDE 'RDCOM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C

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
