      SUBROUTINE DBSCAN (LDEBG,SUBIN,NC,ICYC)
      IMPLICIT NONE
C----------
C  **DBSCAN--BS   DATE OF LAST REVISION:  07/23/08
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
      INTEGER ICYC,NC,IB1,IS,IE,IP,IQ
      CHARACTER*(*) SUBIN
      CHARACTER*20 CHECK
      LOGICAL LDEBG
C
      LDEBG=.FALSE.
      IF ((NC.GT.0).AND.(NC.LE.MAXLEN).AND.(ITOP.GT.0)) THEN
        CHECK(1:NC)=SUBIN(1:NC)
        IB1=NC+1
        CHECK(IB1:IB1)=' '
        IB1=NC+4
        CHECK(IB1:IB1)=' '
        IS=NC+2
        IE=NC+3
        CALL CH2NUM (CHECK(IS:IE),ICYC)
        IE=IE+1
        IP=INDEX(SUBNAM,CHECK(1:IE))
        CALL CH2NUM (CHECK(IS:IE),0)
        IQ=INDEX(SUBNAM,CHECK(1:IE))
        IF ((IP.GT.0).OR.(IQ.GT.0)) LDEBG=.TRUE.
      ENDIF
      RETURN
      END
