      SUBROUTINE RDLOAD(A,B,N)
C----------
C  **RDLOAD      LAST REVISION:  12/15/87
C----------
C
C  SUBROUTINE FOR COPYING ONE ARRAY INTO ANOTHER.
C
C  CALLED BY :
C     RDTREG  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     A      - DESTINATION ARRAY OF COPY.
C     B      - SOURCE ARRAY OF COPY.
C     N      - NUMBER OF ELEMENTS IN THE 2 ARRAYS.
C
      DIMENSION A(N),B(N)

      IF (N .GT. 0) THEN
         DO 1000 I=1,N
            A(I) = B(I)
 1000    CONTINUE
      ENDIF

      RETURN
      END
