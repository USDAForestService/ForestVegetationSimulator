      SUBROUTINE DBSPUSGET (CBUFF,IPNT,LNCBUF)
      IMPLICIT NONE
C----------
C DBS $Id$
C----------
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
C
      INTEGER I
      INTEGER LNCBUF, IPNT
      CHARACTER*1 CBUFF(*),CT

      DSNOUT = ' '
      I=1
      DO
         CALL CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
         IF (CT.EQ.CHAR(0)) EXIT
         DSNOUT(I:I)=CT
         I=I+1
      ENDDO

      DBMSOUT = ' '
      I=1
      DO
         CALL CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
         IF (CT.EQ.CHAR(0)) EXIT
         DBMSOUT(I:I)=CT
         I=I+1
      ENDDO

      DSNIN = ' '
      I=1
      DO
         CALL CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
         IF (CT.EQ.CHAR(0)) EXIT
         DSNIN(I:I)=CT
         I=I+1
      ENDDO

      DBMSIN = ' '
      I=1
      DO
         CALL CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
         IF (CT.EQ.CHAR(0)) EXIT
         DBMSIN(I:I)=CT
         I=I+1
      ENDDO

      RETURN
      END
