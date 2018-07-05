      SUBROUTINE KEYRDR (INUNIT,IOUT,LDEBUG,KEYWRD,LNOTBK,
     >                   ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     KEYWORD CARD READER FOR THE STAND PROGNOSIS SYSTEM
C
C     ARGUMENTS:
C     INUNIT= READER REFERENCE NUMBER
C     IOUT  = PRINTER REFERENCE NUMBER
C     LDEBUG= TRUE IF DEBUGGING.
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
C             3= STOP WAS FOUND.
C             LESS THAN ZERO:  PARMS STATEMENT IS ON THE RECORD START-
C             ING IN FIELD IABS(KODE).
C     KARD  = A CHARACTER IMAGE OF THE INPUT FIELDS
C     LFLAG = .TRUE. IF THE HEADING NEEDS TO BE CREATED, FALSE OTHERWISE
C----------
C  DECLARATIONS AND DATA STATEMENTS:
C----------
      INTEGER KODE,IRECNT,IOUT,INUNIT,I,ISTLNB,NF,IP,J,K
      REAL ARRAY(7)
      CHARACTER*110 CKEYFN
      COMMON /CKEYFN/ CKEYFN
      CHARACTER*80 RECORD
      CHARACTER*10 KARD
      CHARACTER*8 KEYWRD,TMP
      CHARACTER*(*)CFN
      LOGICAL LDEBUG,LNOTBK,LCOM,LFLAG,L,LKECHO
      DIMENSION KARD(7),LNOTBK(7)
C----------
C  READ KEYWORD AND LOCATE BLANK PARAMETER FIELDS.
C----------
      LCOM=.FALSE.
    5 CONTINUE
      READ (INUNIT,10,END=30) RECORD
   10 FORMAT (A80)
      IRECNT = IRECNT + 1
      IF (RECORD(1:1).EQ.'!') GOTO 5
      IF (LFLAG .AND. RECORD.EQ.' ') GOTO 5
C
C     CHECK FOR "COMMENT" KEYWORD AND PROCESS.
C
      TMP=RECORD(1:8)
      DO I=1,8
         CALL UPCASE (TMP(I:I))
      ENDDO
      IF (TMP.EQ.'STOP') THEN
         IF (.NOT.LFLAG) WRITE (IOUT,'(/'' STOP'')')
         KODE=3
         RETURN
      ENDIF
      IF (LFLAG) THEN
         CALL GROHED (IOUT)
         CALL PPEATV (L)
         IF (L) WRITE (IOUT,11)
   11    FORMAT (/T39,'PARALLEL PROCESSING EXTENSION -- VERSION 1.0')
         WRITE (IOUT,12)TRIM(CKEYFN)
   12    FORMAT (/130('-')//T49,'OPTIONS SELECTED BY INPUT'//
     >           'KEYWORD FILE NAME: ',A/   
     >            130('-')/'KEYWORD    PARAMETERS:'/
     >                         '--------   ',119('-'))
         LFLAG=.FALSE.
      ENDIF
      IF (RECORD(1:1).EQ.'*'.OR. RECORD.EQ.' ') THEN
         IF (.NOT.LCOM) THEN
            WRITE (IOUT,'(/)')
            LCOM=.TRUE.
         ENDIF
         WRITE (IOUT,'(T12,A)') RECORD(1:MAX(1,ISTLNB(RECORD)))
         GOTO 5
      ELSE
         LCOM=.FALSE.
      ENDIF
      IF (TMP.EQ.'COMMENT') THEN
         IF(LKECHO)WRITE (IOUT,'(/A)') TMP
   14    CONTINUE
         READ (INUNIT,'(A)',END=30) RECORD
         IRECNT=IRECNT+1
         TMP=RECORD (1:4)
         DO I=1,4
            CALL UPCASE (TMP(I:I))
         ENDDO
         IF (TMP(1:4).EQ.'END ') THEN
            IF(LKECHO)WRITE (IOUT,'(/A4)') TMP(1:4)
            GOTO 5
         ELSE
            IF(LKECHO)WRITE (IOUT,'(T12,A)') RECORD(1:ISTLNB(RECORD))
            GOTO 14
         ENDIF
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
      CALL UPKEY(KEYWRD)
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
      IF ( LDEBUG ) CALL KEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD)
      RETURN
C
C     KEYWORD FILE NAME FROM FILOPN FOR WRITING TO OUTPUT FILE
C
      ENTRY KEYFN(CFN)
      CKEYFN=' '
      I=LEN_TRIM(CFN)
      IF (I.GT.LEN(CKEYFN)) THEN
         CKEYFN=CFN(1:4)//'...'//CFN(I+8-LEN(CKEYFN):)
      ELSE
         CKEYFN = CFN
      ENDIF
      RETURN
      END
