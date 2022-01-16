      SUBROUTINE UPCASE (C)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
      INTEGER IP
      CHARACTER C
      CHARACTER*26 UPPER,LOWER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      IP=INDEX(LOWER,C)
      IF (IP.GT.0) C=UPPER(IP:IP)
      RETURN
      END
