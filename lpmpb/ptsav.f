      SUBROUTINE PTSAV(IPLTNO,XAXIS,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     LIBRARY ROUTINE TO SPOOL PLOT OUTPUT
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
C
      INCLUDE 'PT.F77'
C
C
COMMONS

      INTEGER ILUN, IPLTNO, J, N
      REAL SAV, XAXIS, Y1, Y10, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9

      DIMENSION SAV(10)
C             BLOCK OF 10 PLOT VARIABLES TO BE SPOOLED
C
      SAV(1)=Y1
      SAV(2)=Y2
      SAV(3)=Y3
      SAV(4)=Y4
      SAV(5)=Y5
      SAV(6)=Y6
      SAV(7)=Y7
      SAV(8)=Y8
      SAV(9)=Y9
      SAV(10)=Y10
C
C        UPDATE MAX AND MIN
      N = IPTVAR(IPLTNO)
      DO 1 J=1,N
      IF(SAV(J) .GT. PTMAX(IPLTNO,J)) PTMAX(IPLTNO,J)=SAV(J)
      IF(SAV(J) .LT. PTMIN(IPLTNO,J)) PTMIN(IPLTNO,J)=SAV(J)
    1 CONTINUE
C
      IPTSPL(IPLTNO)=IPTSPL(IPLTNO)+1
      ILUN= 8+IPLTNO
      WRITE(ILUN) XAXIS,SAV
C
      RETURN
      END
