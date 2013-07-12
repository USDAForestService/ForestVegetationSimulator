      SUBROUTINE BWEKRD (INUNIT,IOUT,KEYWRD,LNOTBK,
     >                   ARRAY,IRECNT,KODE,KARD)
      IMPLICIT NONE
C----------
C  **BWEKRD                 DATE OF LAST REVISION:  06/17/13
C----------
c
c  Also revised by K.Sheehan 7/1/96 to run on PowerStation
c
C     KEYWORD CARD READER FOR THE STAND PROGNOSIS SYSTEM
C
C     ARGUMENTS:
C     INUNIT= READER REFERENCE NUMBER
C     IOUT  = PRINTER REFERENCE NUMBER
C     KEYWRD= CHARACTER*8 KEYWORD.
C     LNOTBK= DIMENSION 7, VALUES ARE TRUE IF THE CORRESPONDING
C             NUMERIC FIELD IS NOT BLANK, FALSE IF THEY ARE.
C     ARRAY = DIMENSION 7, VALUES READ FROM THE NUMERIC FIELD.
C     IRECNT= RECORD COUNTER.
C     KODE  = RETURN CODE:
C             0= NO ERRORS FOUND.
C             1= FIRST COLUMN OF CARD WAS BLANK OR INVALID CHAR DATA
C                WAS FOUND.
C             2= END-OF-FILE WAS FOUND.
C             LESS THAN ZERO:  PARMS STATEMENT IS ON THE RECORD START-
C             ING IN FIELD IABS(KODE).
C
C  Revision History:
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C----------
C  DECLARATIONS AND DATA STATEMENTS:
C----------
      CHARACTER*80 RECORD
      CHARACTER*10 KARD(8)
      CHARACTER*8  KEYWRD
      INTEGER I, INUNIT, IOS, IOUT, IP, IRECNT, ISTLNB, J, KODE, NF
      REAL         ARRAY(7)
      LOGICAL      LNOTBK(7),LCOM

C----------
C  READ KEYWORD AND LOCATE BLANK PARAMETER FIELDS.
C----------
      LCOM=.FALSE.
    5 CONTINUE
c      READ (INUNIT,10,END=30) RECORD
      READ (INUNIT,10,END=30,iostat=ios) RECORD
   10 FORMAT (A80)
      IRECNT = IRECNT + 1
      IF (RECORD(1:1).EQ.'!') GOTO 5
      IF (RECORD(1:1).EQ.'*'.OR. RECORD.EQ.' ') THEN
         IF (.NOT.LCOM) THEN
            WRITE (IOUT,'(/)')
            LCOM=.TRUE.
         ENDIF
         WRITE (IOUT,'(T13,A)') RECORD(1:ISTLNB(RECORD))
         GOTO 5
      ELSE
         LCOM=.FALSE.
      ENDIF
C
C     CHECK FOR THE PRESENCE OF A 'P' SO THAT THE PARMS STATEMENT
C     MAY BE DETECTED.
C
      NF=7
      IP=INDEX(RECORD(11:80),'P')
      IF (IP.EQ.0) IP=INDEX(RECORD(11:80),'p')
      IF (IP.GT.0) THEN
         DO 7 I=IP+10,IP+14
         CALL BWEUCA (RECORD(I:I))
    7    CONTINUE
         IF (RECORD(IP+10:IP+14).EQ.'PARMS') THEN
            IF (MOD(IP,10).EQ.0) THEN
               NF=IP/10-1
            ELSE
               NF=IP/10
            ENDIF
         ENDIF
      ENDIF
      KEYWRD=RECORD(1:8)
      J=1
      DO 25 I=1,NF
      J=J+10
c
c substituted by KAS
c
c      READ (RECORD(J:J+9),20,ERR=26) ARRAY(I),KARD(I)
c   20 FORMAT (F10.0,T1,A10)
      READ (RECORD(J:J+9),20) KARD(I)
   20 FORMAT (A10)
   25 CONTINUE
c
c added by KAS 7/1/96
c
      J=1
	do 125 I=1,NF
	J=J+10
	READ (RECORD(J:J+9),120,ERR=26) ARRAY(I)
  120 FORMAT (F10.0)
  125 CONTINUE
c
c end of kas addition 7/1/96
c
   26 CONTINUE
      IF (KEYWRD(1:1).NE. ' ') GO TO 40
C
      KODE = 1
      GO TO 60
C
   30 CONTINUE
      KODE = 2
      RETURN
C
   40 CONTINUE
      CALL BWEUKY (KEYWRD)
      KODE = 0
      DO 50 I=1,NF
      LNOTBK(I) = KARD(I).NE.' '
   50 CONTINUE
      IF (NF.LT.7) THEN
         KODE=-(NF+1)
         DO 55 I=NF+1,7
         LNOTBK(I)=.FALSE.
         ARRAY(I)=0.0
         J=I*10+1
         KARD(I)=RECORD(J:J+10)
   55    CONTINUE
      ENDIF
C
   60 CONTINUE
      RETURN
      END
