      SUBROUTINE UNBLNK (RECORD,IRLEN)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     THIS SUBROUTINE REMOVES ALL BLANKS FROM A CHARACTER
C     STRING, LEAVING THE NONBLANK CONTENTS OF THE STRING
C     LEFT JUSTIFIED, AND THE REST OF THE STRING BLANK.
C     IRLEN IS RETURNED AS THE LENGTH OF THE NONBLANK PORTION.
C
      INTEGER IRLEN,LENREC,I
      CHARACTER*(*) RECORD
      CHARACTER*7   VVER
      
      IRLEN=0

      ! if ON or BC then allow embedded spaces with path/file names
      ! could also strip CHR(33) quotation marks with this sub.
      CALL VARVER(VVER)
      IF(VVER(:2).EQ.'BC'.OR.VVER(:2).EQ.'ON') THEN
         RECORD = ADJUSTL(RECORD)
         IRLEN = LEN_TRIM(RECORD)
      ELSE
         LENREC=LEN(RECORD)
         DO I=1,LENREC
            IF (RECORD(I:I).NE.' ') THEN
               IRLEN=IRLEN+1
               IF (IRLEN.LT.I) RECORD(IRLEN:IRLEN)=RECORD(I:I)
            ENDIF
         ENDDO
         IF (IRLEN.LT.LENREC) RECORD(IRLEN+1:LENREC)=' '
      ENDIF
      RETURN
      END
