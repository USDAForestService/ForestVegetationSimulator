      SUBROUTINE DBADD (SUBIN,NC,ICYC,IRC)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C
COMMONS
C
C
      INCLUDE 'DBSTK.F77'
C
C
COMMONS
C
C
      INTEGER IRC,ICYC,NC,IPLEN,I,IS,IE
      CHARACTER*(*) SUBIN
C
      IPLEN=ITOP+NC+4
      IF (IPLEN.LT.MAXTOP) THEN
         DO 40 I=1,NC
         ITOP=ITOP+1
         SUBNAM (ITOP:ITOP)=SUBIN(I:I)
   40    CONTINUE
         ITOP=ITOP+1
         SUBNAM(ITOP:ITOP)=' '
         ITOP=ITOP+3
         SUBNAM(ITOP:ITOP)=' '
         IS=ITOP-2
         IE=ITOP-1
         CALL CH2NUM (SUBNAM (IS:IE),ICYC)
         IRC=0
      ELSE
         IRC=2
      ENDIF
      RETURN
      END
