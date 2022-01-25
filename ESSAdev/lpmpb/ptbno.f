      FUNCTION PTBNO(X)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     FIND LARGEST INTEGER POWER OF 10 LESS THAN X
C     IF X IS LESS THAN 3.5*10**I, THEN PTBNO=10**I/2
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
      REAL P10, PTBNO, X, XRND, XX 

      P10=10.
      XX=X
      IF(XX .LT. 0. ) XX=-XX
      XRND=0.1005*XX
      IF(XRND .LE. 10.) GO TO 1
C
    2 P10=10.*P10
              IF(P10.LE. XRND) GO TO 2
              GO TO 3
C
    1 P10=P10/10.
              IF(P10 .GT. XRND) GO TO 1
              P10=10.*P10
    3 PTBNO=P10
      IF(3.5*P10 .GE. X) PTBNO=.5*P10
      RETURN
      END
