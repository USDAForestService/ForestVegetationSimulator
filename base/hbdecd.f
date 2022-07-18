      SUBROUTINE HBDECD (IHB,CNHB,MAXHB,ARRAY2,KARD2)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C  CALLED FROM SUBROUTINE **HABTYP**.
C----------
C
C     DECODE THE HABITAT TYPE/PLANT ASSOCIATION CODE.
C     HT/PA CODE IS IN THE SECOND POSITION OF KARD AND ARRAY
C     AND IS USED AS A SCALAR KARD2 AND ARRAY2 IN THIS ROUTINE.
C
C     IHB   = NUMERIC HABITAT CODE, ZERO IF NO HABITAT FOUND.
C     CNHB  = C*8 VECTOR OF LENGTH MAXHB, HABITAT CODES.
C     MAXHB = LENGTH OF CNHB.
C----------
      INTEGER IHB,MAXHB,J,I
      CHARACTER*10 KARD2
      CHARACTER*8  CNHB(MAXHB),TEMP
      REAL         ARRAY2
C
      IHB=IFIX(ARRAY2)
C----------
C  IF IHB IS OUT OF RANGE, SET FLAG TO LOAD DEFAULT CODE.
C----------
      IF (IHB.GE.0 .AND. IHB.LE.MAXHB) THEN
C----------
C  IF IHB IS ZERO, THEN THE ALPHA CODE MAY BE PRESENT.
C----------
         IF (IHB.EQ.0) THEN
C----------
C  LOAD TEMP WITH UP TO EIGHT NON-BLANK CHARS IN KARD2.
C----------
            TEMP='UNKNOWN'
            J=0
            DO 10 I=1,10
            IF (KARD2(I:I).NE.' ') THEN
               TEMP=' '
               DO 5 J=1,MIN(8,10-I+1)
               IF ((KARD2(I+J-1:I+J-1).EQ.'0').AND.(J.EQ.1)) THEN
                  TEMP='DEFAULT'
                  GO TO 50
               ENDIF
               TEMP(J:J)=KARD2(I+J-1:I+J-1)
               CALL UPCASE(TEMP(J:J))
    5          CONTINUE
               GO TO 20
            ENDIF
   10       CONTINUE
C----------
C  NOTHING WAS FOUND.
C----------
            TEMP='DEFAULT'
            GO TO 50
   20       CONTINUE
C----------
C  TRY TO DECODE THE HABITAT CODE.
C  'UNKNOWN' IS HABITAT CODE ZERO.
C----------
            IF (TEMP.NE.'UNKNOWN') THEN
               DO 30 I=1,MAXHB
               IF (TEMP(1:8).EQ.CNHB(I)(1:8)) THEN
                  IHB=I
                  GO TO 50
               ENDIF
   30          CONTINUE
C----------
C  NO VALID HABITAT CODE WAS FOUND.  SET FLAG TO LOAD DEFAULT.
C----------
               IHB=-1
               KARD2=TEMP
               GO TO 60
            ENDIF
C----------
C  VALID HABITAT CODE FOUND.
C----------
   50       CONTINUE
            KARD2=TEMP
            ARRAY2=FLOAT(IHB)
         ELSE
            IHB=IFIX(ARRAY2)
            KARD2=CNHB(IHB)(1:8)
         ENDIF
      ELSE
         IHB=-1
      ENDIF
   60 CONTINUE
      RETURN
      END
