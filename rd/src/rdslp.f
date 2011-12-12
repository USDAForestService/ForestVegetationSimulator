      FUNCTION RDSLP(X,XX,YY,N)
C----------
C  **RDSLP       LAST REVISION:  08/06/01
C----------
C
C  Given two arrays XX and YY, each of dimension N, representing
C  a series of line segments (XX strictly increasing), and an X-
C  coordinate, return its Y-coordinate on the line.
C
C  CALLED BY :
C     RDGROW  [ROOT DISEASE]
C     RDINOC  [ROOT DISEASE]
C     RDSPRD  [ROOT DISEASE]
C     ANCARY  [ROOT DISEASE]
C     RDMORT  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     X      - (I ) X-coordinate of desired point.
C     XX     - (I ) X points on series of line segments.
C     YY     - (I ) Y points on series of line segments.
C     N      - (I ) number of points in XX and YY arrays.
C
C  Revision History:
C    21-MAR-00 Lance David (FHTET)
C       Reduced RETURN statements to 1 at the end of routine.
C       Modified Debug code.
C    06-AUG-01 Lance R. David (FHTET)
C       Changed dimensions on XX and YY arrays from 5 to *.
C....................................................................
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'CONTRL.F77'
C
C.... Local variables
C
      LOGICAL DEBUG
      DIMENSION XX(*), YY(*)
C
C.... Check for DEBUG
C
      CALL DBCHK(DEBUG,'RDSLP',5,ICYC)

      IF      (X .LT. XX(1)) THEN
         RDSLP = YY(1)
      ELSE IF (X .GT. XX(N)) THEN
         RDSLP = YY(N)
      ELSE
         I = 1
   10    IF (X. GT. XX(I+1)) THEN
            I = I + 1
            GOTO 10
         ENDIF
C
C....    Now X in the range XX(I) to XX(I+1); find Y.
C
         RDSLP = YY(I) + (YY(I+1) - YY(I)) /
     &            (XX(I+1) - XX(I)) * (X-XX(I))
      ENDIF

      IF (DEBUG) THEN
         WRITE (JOSTND,910) ' IN RDSLP:  N= ', N
         WRITE (JOSTND,900) ' IN RDSLP: XX= ', (XX(I),I=1,5), X
         WRITE (JOSTND,900) ' IN RDSLP: YY= ', (YY(I),I=1,5), RDSLP
  900 FORMAT (A15, 5F10.3, ';  -> ', F10.3)
  910 FORMAT (A15, I5)
      ENDIF
      RETURN
      END
