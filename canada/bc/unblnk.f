      SUBROUTINE UNBLNK (RECORD,IRLEN)
      IMPLICIT NONE
C----------
C CANADA-BC $Id$
C----------
C
C     THIS SUBROUTINE REMOVES ALL BLANKS FROM A CHARACTER
C     STRING, LEAVING THE NONBLANK CONTENTS OF THE STRING
C     LEFT JUSTIFIED, AND THE REST OF THE STRING BLANK.
C     IRLEN IS RETURNED AS THE LENGTH OF THE NONBLANK PORTION.
C
      INTEGER IRLEN,LENREC,I
      CHARACTER*(*) RECORD
      
      IRLEN=0

      ! if ON or BC then allow embedded spaces with path/file names
      ! could also strip CHR(33) quotation marks with this sub.
C
         RECORD = ADJUSTL(RECORD)
         IRLEN = LEN_TRIM(RECORD)
C
      RETURN
      END
