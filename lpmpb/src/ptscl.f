      SUBROUTINE PTSCL(IPLTNO,IPTLUN)
      IMPLICIT NONE
C----------
C  **PTSCL         DATE OF LAST REVISION:  07/02/10
C----------
C
C        ROUTINE TO PRINT SCALES
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
COMMONS
C
C
      INCLUDE 'PT.F77'
C
C
COMMONS
C
      CHARACTER*1  L(10)
      CHARACTER*4  IFORM
      CHARACTER*35 IFORM4

      INTEGER I, INT, IPLTNO, IPTLUN, J, K

      REAL DIV, S(5), SI, TEMP, XJ

C
C        SET UP LINE LENGTH  AND DEFAULT FORMAT
C
      IFORM4 ='(1X,0PF10.3,4(15X,0PF10.3),1X,10A1)'
      INT=5
      DIV=INT-1
C
      I=1
    7 SI=(PTU(IPLTNO,I)-PTL(IPLTNO,I))/DIV
      DO 1 J=1,INT
      XJ=J-1
    1 S(J)=PTL(IPLTNO,I)+XJ*SI
C             SETS UP 3 OR 5 MARKERS ACROSS TOP
C
      DO 2 J=1,10
      L(J)=IPTLET(IPLTNO,I)
      I=I+1
      IF (I .GT. IPTVAR(IPLTNO)) GO TO 4
      IF(PTL(IPLTNO,I) .NE. PTL(IPLTNO,I-1)) GO TO 4
      IF(PTU(IPLTNO,I) .NE. PTU(IPLTNO,I-1)) GO TO 4
    2 CONTINUE
C             LOOP FORCES ALL SIMILAR SCALES INTO
C                     A SINGLE LINE
C
    4 TEMP=ABS(PTU(IPLTNO,I-1))
      IF (TEMP .EQ. 0.) GO TO 5
      IF (1. .LE. TEMP .AND. TEMP .LT. 100000.) GO TO 5
      IFORM4 ='(1X,1PE10.3,4(15X,1PE10.3),1X,10A1)'
    5 TEMP=ABS(PTL(IPLTNO,I-1))
      IF (TEMP .EQ. 0.) GO TO 6
      IF (1. .LE. TEMP .AND. TEMP .LT. 100000.) GO TO 6
      IFORM4 ='(1X,1PE10.3,4(15X,1PE10.3),1X,10A1)'
C
    6 WRITE(IPTLUN,IFORM4) (S(K),K=1,INT),(L(K),K=1,J)
      IFORM4 ='(1X,0PF10.3,4(15X,0PF10.3),1X,10A1)'
      IF(I .LE. IPTVAR(IPLTNO)) GO TO 7
C
      RETURN
      END
