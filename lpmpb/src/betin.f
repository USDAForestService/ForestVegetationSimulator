      DOUBLE PRECISION FUNCTION BETIN  (A,B,X)
      IMPLICIT NONE
C----------
C  **BETIN         DATE OF LAST REVISION:  07/02/10
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
CAUTION - BETIN IS AN INCOMPLETE BETA FUNCTION WRITEN IN DOUBLE
C     PRECISION.  THUS A,B AND X MUST BE DECLARED DOUBLE PRECISION.
C
C     THIS ROUTINE, WITH THE SUBPROGRAMS FORW, BACK, PQSML, AND DGAMMA
C     EVALUATES THE NORMALIZED INCOMPLETE BETA FUNCTION:
C
C     IF Y(A,R,X) = INTEGRAL FROM 0 TO X OF T**(A-1)*(1-T)**(B-1)DT
C     THEN BETIN(A,B,X) = Y(A,B,X)/Y(A,B,1).
C     RESTRICTIONS  A.GT.0, B.GT.0, & 0.LE.X.LE.1 MUST BE SATISFIED.
C
C     REFERENCE:   COMM OF THE ACM, MARCH 1964,  ALG. 222.
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
C
COMMONS
C
C
      INCLUDE 'MPBETA.F77'
C
C
COMMONS

      INTEGER IA,M
      DOUBLE PRECISION A,B,P,Q,X,Y,P1,Q1,X1,X2,BACK,FORW,PQSML

      IF(A.GT.0..AND. B.GT.0..AND.X.GE.0..AND.X.LE.1.) GO TO 5
      WRITE(6,101)
  101 FORMAT('0  ****** PARAMETER RESTRICTIONS IN BETIN ARE NOT',
     >        ' SATISFIED, BETIN HAS BEEN SET TO ZERO.')
      BETIN = 0
      RETURN
C
    5 IA = A
      P = A-IA
      IF(P.NE. 0.) GO TO 1
      P = 1.
      IA = IA-1
    1 BETIN=X
      IF(X.EQ.0..OR.X.EQ.1.) RETURN
      IF(X .LE. .5) GO TO 3
      Y = 1. - X
      Q = P
      P = B
      M = P
      P1 = P-M
      IF(P1.NE.0.) GO TO 2
      M = M-1
      P1 = 1.
    2 X1 = BACK(Y,P1,Q,PQSML(Y,P1,Q),M)
      X2 = BACK(Y,P1,Q+1,PQSML(Y,P1,Q+1.),M)
      BETIN = 1. - FORW(Y,P,Q,X1,X2,IA)
      IF (BETIN .LT. BMIN) BETIN = 0.
      RETURN
    3 Q=B
      M = Q
      Q1 = Q-M
      IF(Q1.NE.0.) GO TO 4
      Q1 = 1.
      M = M-1
    4 X1=PQSML(X,P,Q1)
      IF(M.GT.0) X2=PQSML(X,P,Q1+1.)
      BETIN=BACK(X,P,Q,FORW(X,P,Q1,X1,X2,M),IA)
      IF (BETIN .LT. BMIN) BETIN = 0.
      RETURN
      END
