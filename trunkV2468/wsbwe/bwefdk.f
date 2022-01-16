      SUBROUTINE BWEFDK (NUMBER,KEYWRD,TABLE,ISIZE,KODE,IOUT)
      IMPLICIT NONE
C----------
C  **BWEFDK                 DATE OF LAST REVISION:  06/17/13
C----------
C
C     FUNCTION TO FIND A KEYWORD IN A TABLE.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
C     NICHOLAS L. CROOKSTON                          SEPT 1979
C     INT-FORESTRY SCIENCES LABORATORY-MOSCOW, IDAHO
C
C     NUMBER= THE KEYWORD NUMBER (TABLE INDEX VALUE) RETURNED
C             TO CALLING SUBROUTINE.
C     KEYWRD= KEYWORD TO BE FOUND IN TABLE ARRAY
C     TABLE = CHARACTER*8 ARRAY CONTAINING THE KEYWORDS
C     ISIZE = THE DIMENSIONAL SIZE OF TABLE ARRAY
C     KODE  = RETURN CODE:
C             0= NO ERRORS FOUND.
C             1= KEYWORD NOT FOUND
C     IOUT  = OUTPUT WRITE UNIT REFERENCE NUMBER
C
C Revision History:
C   12-MAY-00 Lance David
C     Brought comments and variable descriptions up to date.
C     Changed TABLE array definition to inherit dimension from
C     calling program unit.
C   14-JUL-2010 Lance R. David (FMSC)
C     Added IMPLICIT NONE and declared variables as needed.
C----------
      INTEGER     I, IOUT, ISIZE, KODE, NUMBER
      CHARACTER*8 KEYWRD,TABLE(*)

      NUMBER=0
      DO 10 I=1,ISIZE
      IF (KEYWRD.NE.TABLE(I)) GOTO 10
      NUMBER=I
      GOTO 15
   10 CONTINUE
   15 CONTINUE
      IF (NUMBER.EQ.0) THEN
        KODE=1
        WRITE (IOUT,85) KEYWRD
   85   FORMAT (/,'''',A8,''' :KEYWORD SPECIFIED')
      ELSE
        KODE=0
      ENDIF
      RETURN
      END
