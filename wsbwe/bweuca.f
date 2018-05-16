      SUBROUTINE BWEUCA (C)
      IMPLICIT NONE
C----------
C  **BWUPCA                 DATE OF LAST REVISION:  07/14/10
C----------
      CHARACTER C
      CHARACTER*26 UPPER,LOWER
      INTEGER IP
      
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/

      IP=INDEX(LOWER,C)
      IF (IP.GT.0) C=UPPER(IP:IP)
      RETURN
      END
