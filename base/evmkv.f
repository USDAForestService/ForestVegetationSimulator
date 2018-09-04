      SUBROUTINE EVMKV(CTOK)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     CREATES A NEW VARIABLE, IF IT CAN!
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
COMMONS
C
      CHARACTER*(*) CTOK

      IF (ITST5.LT.MXTST5) THEN
         ITST5=ITST5+1
         CTSTV5(ITST5)=CTOK
         LTSTV5(ITST5)=.FALSE.
         LEVUSE=.TRUE.
      ELSE
         CALL ERRGRO (.TRUE.,10)
         CALL ERRGRO (.TRUE.,12)
      ENDIF
      RETURN
      END
