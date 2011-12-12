      SUBROUTINE CRDECD (IHB,CNHB,MAXHB,ARRAY2,KARD2)
      IMPLICIT NONE
C----------
C  **CRDECD--CR   DATE OF LAST REVISION:  03/13/08
C
C  CALLED FROM SUBROUTINE **HABTYP**.
C----------
C
C     DECODE THE HABITAT TYPE/PLANT ASSOCIATION CODE.
C     HT/PA CODE IS IN THE SECOND POSITION OF KARD AND ARRAY.
C     AND IS USED AS A SCALAR KARD2 AND ARRAY2 IN THIS ROUTINE.
C
C     IHB   = NUMERIC HABITAT CODE, ZERO IF NO HABITAT FOUND.
C     CNHB  = C*8 VECTOR OF LENGTH MAXHB, HABITAT CODES.
C     MAXHB = LENGTH OF CNHB.
C----------
      CHARACTER*10 KARD2
      CHARACTER*8  CNHB(MAXHB),TEMP
      REAL   ARRAY2
      INTEGER IHB,MAXHB,J,I
C
      IHB=0
C----------
C  LOAD TEMP WITH UP TO EIGHT NON-BLANK CHARS IN KARD.
C----------
            TEMP='UNKNOWN'
            J=0
            DO 10 I=1,10
            IF (KARD2(I:I).NE.' ') THEN
               TEMP=' '
               DO 5 J=1,MIN(8,10-I+1)
               TEMP(J:J)=KARD2(I+J-1:I+J-1)
               CALL UPCASE(TEMP(J:J))
    5          CONTINUE
               GO TO 20
            ENDIF
   10       CONTINUE
C----------
C  NOTHING WAS FOUND.
C----------
            GO TO 60
C----------
C  TRY TO DECODE THE HABITAT CODE.
C  'UNKNOWN' IS HABITAT CODE ZERO.
C----------
   20       CONTINUE
            IF (TEMP.NE.'UNKNOWN') THEN
               DO 30 I=1,MAXHB
               IF (TEMP(1:8).EQ.CNHB(I)(1:8)) THEN
                  IHB=I
                  GO TO 60
               ENDIF
   30          CONTINUE
C----------
C  NO VALID HABITAT CODE WAS FOUND.  SET FLAG TO LOAD DEFAULT.
C----------
               IHB=-1
               GO TO 60
            ENDIF
C
   60 CONTINUE
      RETURN
      END
