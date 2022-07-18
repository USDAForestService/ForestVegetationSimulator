      SUBROUTINE BWEKRD (INUNIT,IOUT,KEYWRD,LNOTBK,
     >                   ARRAY,IRECNT,KODE,KARD)
      IMPLICIT NONE
C----------
C WSBWE $Id$
C  **BWEKRD                 DATE OF LAST REVISION:  03/23/17
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
C    23-MAR-2017 Lance R. David (FMSC)
C       Was not properly processing alph entries in fields. Made
C       consistent with FVS base routine keyword reader (keyrdr.f).
C----------
C  DECLARATIONS AND DATA STATEMENTS:
C----------
      CHARACTER*80 RECORD
      CHARACTER*10 KARD(7)
      CHARACTER*8  KEYWRD
      INTEGER I, INUNIT, IOS, IOUT, IP, IRECNT, ISTLNB, J, K, KODE, NF
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
      K=11
      DO WHILE (K <= 73) 
         IP=INDEX(RECORD(K:73),'P')
         IF (IP.EQ.0) IP=INDEX(RECORD(K:73),'p')
         IF (IP.GT.0) THEN
C
C        BORROW THE USE OF KEYWRD TO HOLD "PARMS" IN MIXED, THEN UPPER
C        CASE...
C
            KEYWRD(1:5)=RECORD(IP+K-1:IP+K+4)
            DO I=1,5
               CALL UPCASE (KEYWRD(I:I))
            ENDDO
            IF (KEYWRD(1:5).EQ.'PARMS') THEN
               IP=K+IP-11
               NF=(IP-1)/10   
               EXIT
            ENDIF
            K=K+IP
            CYCLE
         ELSE
            EXIT   
         ENDIF
      END DO  
C
C     LOAD THE KEYWORD INTO KEYWRD...DECODE THE FIELDS.
C
      KEYWRD=RECORD(1:8)
      J=1
      DO 25 I=1,NF
      J=J+10
      KARD(I)=RECORD(J:J+9)
      ARRAY(I)=0.0
C
C     MAKE SURE ALL OF THE CHARACTERS IN KARD ARE NUMBERS...
C
      DO 15 K=1,10
      IF (INDEX(' .+-eE0123456789',KARD(I)(K:K)).EQ.0) GOTO 25
   15 CONTINUE
      READ (KARD(I),'(G10.0)',ERR=25) ARRAY(I)
   25 CONTINUE
      GOTO 40
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
         KARD(I)=RECORD(J:J+9)
   55    CONTINUE
      ENDIF
C
   60 CONTINUE
      RETURN
      END
