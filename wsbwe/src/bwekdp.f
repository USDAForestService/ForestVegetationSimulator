      SUBROUTINE BWEKDP (IOUT,IRECNT,KEYWRD,ARRAY,KARD)
      IMPLICIT NONE
C----------
C  **BWEKDP                 DATE OF LAST REVISION:  07/14/10
C----------
C  Revision History:
C    14-JUL-2010 Lance R. David (FMSC)
C       Previous note revision date 1986
C       Added IMPLICIT NONE and declared variables as needed.
C----------
      CHARACTER*10 KARD(7)
      CHARACTER*8 KEYWRD
      INTEGER IOUT, IRECNT
      REAL ARRAY(7)
      WRITE (IOUT,70) IRECNT,KEYWRD,ARRAY,KARD
   70 FORMAT (/,' CARD NUM =',I5,'; KEYWORD FIELD = ''',A8,''''/
     >        '      PARAMETERS ARE:',7F14.7,/
     >        '      COL 11 TO 80 =''',7A10,'''')
C
      RETURN
      END
