      FUNCTION ISTLNB (STRING)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     FIND THE LOCATION OF THE LAST NON-BLANK CHAR IN A STRING.
C     RETURNS A ZERO IF THE ENTIRE STRING IS BLANK.
C
      INTEGER ISTLNB,MAX,J,K,I
      CHARACTER*(*) STRING
      IF (STRING.EQ.' ') THEN
         ISTLNB=0
      ELSE
         MAX=LEN(STRING)
         J=MAX/2
         K=MAX
   10    CONTINUE
         IF (K-J.GT.10) THEN
            IF (STRING(J:K).EQ.' ') THEN
               K=J+1
               J=J/2
            ELSE
               J=J+((K-J)/2)-1
            ENDIF
            GOTO 10
         ENDIF
         DO 20 I=K,1,-1
         IF (STRING(I:I).NE.' ') THEN
            ISTLNB=I
            RETURN
         ENDIF
   20    CONTINUE
      ENDIF
      RETURN
      END
