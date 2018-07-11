      DOUBLE PRECISION FUNCTION BACK  (X,P,Q,Z,N)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
      INCLUDE 'MPBETA.F77'
      INTEGER N
      DOUBLE PRECISION P,Q,R,T,U,X,Z,AP,FN,Q1,XN,XNU

      BACK = Z
      IF(N.EQ.0) RETURN
      Q1 = Q-1.
      AP = Z
      FN = N
      XNU = N+N+6
    1 XN = XNU
      R = 0.
      BACK = Z
    2 XN = XN-1
      T = P + XN
      U = (T+Q1)*X
      R = U/(U+T-T*R)
      IF(XN .LE. FN) BACK = R*BACK
      IF(BACK .LT. BMIN)RETURN
      IF(XN.GT.1.) GO TO 2
      IF(DABS(BACK-AP) .LT. DABS(EPS*BACK))RETURN
      AP = BACK
      XNU = XNU+5.
      GO TO 1
      END
