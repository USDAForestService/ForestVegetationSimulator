      SUBROUTINE SPDECD (IPOS,ISP,CNSP,JOSTND,IRECNT,
     >                   KEYWRD,ARRAY,KARD)
      IMPLICIT NONE
C----------
C  **SPDECD--BASE   DATE OF LAST REVISION:  06/04/12
C----------
C
C     DECODE THE SPECIES CODE.
C     IPOS  = THE INDEX OF WHICH MEMBER OF KARD AND ARRAY THE SPECIES
C             CODE IS LOCATED.
C     ISP   = I*4 NUMERIC SPECIES CODE, -999 IF NO SPECIES FOUND.
C     CNSP  = C*4 VECTOR OF LENGTH MAXSP, SPECIES CODES, LAST CHAR
C             IS IGNORED.
C     MAXSP = LENGTH OF CNSP
C     JOSTND= STD OUTPUT.
C     IRECNT= RECORD COUNT.
C     KEYWRD= C*8 KEYWORD.
C     ARRAY = R*4 ARRAY OF PARAMETERS.
C     KARD  = C*10 ARRAY OF PARAMETERS.
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INTEGER IRECNT,JOSTND,ISP,IPOS,IFLAG,J,I
      CHARACTER*10 KARD(7)
      CHARACTER*8  KEYWRD
      CHARACTER*4  CNSP(MAXSP),TEMP
      REAL ARRAY(7)
C
      ISP=IFIX(ARRAY(IPOS))
C----------
C  CHECK FOR SPECIES GROUP NAME. IFLAG=1 MEANS A SPECIES GROUP NAME
C  WAS FOUND.
C----------
      IFLAG=0
      CALL SGDECD(ISP,KARD(IPOS),IFLAG)
      IF(IFLAG .NE. 0)THEN
        ARRAY(IPOS)=FLOAT(ISP)
        GO TO 100
      ENDIF
C
C     IF ISP IS OUT OF RANGE, THEN WE HAVE AN ERROR.
C
      IF (ISP.GE.0 .AND. ISP.LE.MAXSP) THEN
C
C        IF ISP IS ZERO, THEN THE ALPHA CODE MAY BE PRESENT.
C
         IF (ISP.EQ.0) THEN
C
C           LOAD TEMP WITH UP TO THREE NON-BLANK CHARS IN KARD.
C
            TEMP='ALL'
            J=0
            DO 10 I=1,10
            IF (KARD(IPOS)(I:I).NE.' ') THEN
               TEMP=' '
               DO 5 J=1,MIN(3,10-I+1)
               IF (KARD(IPOS)(I+J-1:I+J-1).EQ.'0') THEN
                  TEMP='ALL'
                  GOTO 50
               ENDIF
               TEMP(J:J)=KARD(IPOS)(I+J-1:I+J-1)
               CALL UPCASE(TEMP(J:J))
    5          CONTINUE
               GOTO 20
            ENDIF
   10       CONTINUE
C
C           NOTHING WAS FOUND.
C
            GOTO 50
   20       CONTINUE
C
C           TRY TO DECODE THE SPECIES CODE. 
C           'ALL' IS SPECIES CODE ZERO.
C
            IF (TEMP.NE.'ALL') THEN
C
C              DEAL WITH ONE CHARACTER SPECIES CODES.
C
               IF (TEMP(2:).EQ.' ') THEN
                  TEMP(2:)=TEMP(1:1)
                  TEMP(1:1)=' '
               ENDIF
               DO 30 I=1,MAXSP
               IF (TEMP(1:2).EQ.CNSP(I)(1:2)) THEN
                  ISP=I
                  GOTO 50
               ENDIF
   30          CONTINUE
C
C              NO VALID SPECIES CODE WAS FOUND.  SIGNAL AN ERROR.
C
               CALL KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
               IF(KEYWRD .EQ. 'SPGROUP')THEN
                 CALL ERRGRO (.TRUE.,29)
               ELSE
                 CALL ERRGRO (.TRUE.,4)
               ENDIF
               ISP=-999
               KARD(IPOS)=TEMP
               RETURN
            ENDIF
   50       CONTINUE
            KARD(IPOS)=TEMP
            ARRAY(IPOS)=FLOAT(ISP)
         ELSE
            ISP=IFIX(ARRAY(IPOS))
            KARD(IPOS)=CNSP(ISP)(1:2)
         ENDIF
C
      ELSE
         CALL KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
         IF(KEYWRD .EQ. 'SPGROUP')THEN
           CALL ERRGRO (.TRUE.,29)
         ELSE
           CALL ERRGRO (.TRUE.,4)
         ENDIF
         ISP=-999
      ENDIF
C
  100 CONTINUE
      RETURN
      END
