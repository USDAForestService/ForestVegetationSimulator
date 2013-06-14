      DOUBLE PRECISION FUNCTION PQSML  (X,P,Q)
      IMPLICIT NONE
C----------
C  **PQSML         DATE OF LAST REVISION:  07/02/10
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
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
      INCLUDE 'MPBETA.F77'
C
C
COMMONS
C
      DOUBLE PRECISION P,Q,S,U,V,X,XK,MPBGAM
C
      U = X**P
      S = U/P
      XK = 0.
    1 XK = XK + 1.
      U = (XK-Q)*X*U/XK
      V = U/(XK+P)
      S = S+ V
      IF(DABS(V) .GT. DABS(EPS*S)) GO TO 1
C     WRITE(16,777) P,Q,S
 777  FORMAT('IN PQSML, P,Q,S', 3D16.8)
      PQSML = S*MPBGAM(P+Q)/(MPBGAM(P)*MPBGAM(Q))
C     WRITE(16,444) PQSML
 444  FORMAT('PQSML=',D16.8)
      RETURN
      END
