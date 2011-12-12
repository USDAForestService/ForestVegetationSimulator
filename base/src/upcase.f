      SUBROUTINE UPCASE (C)
      IMPLICIT NONE
C----------
C  **UPCASE DATE OF LAST REVISION:  07/23/08
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
