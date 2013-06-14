      FUNCTION TAFIT (X, Y)
      IMPLICIT NONE
C----------
C  **TAFIT         DATE OF LAST REVISION:  07/02/10
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C
C        TAFIT FITS A CURVE OF THE FORM Y = AX**2 + BX + C TO ANY
C        THREE POINTS.
C
C        X'S CORRESPOND TO TA'S, X(1) BEING THE ORIGINAL
C        ESTIMATE OF TA.
C
C        Y'S CORRESPOND TO CUMULATIVE SURFACE KILLED IN THE THREE
C        TRIAL RUNS.
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
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
COMMONS
C
      INTEGER I
      REAL  A, B, C, DISCRM, TAFIT, X(3), X0, X01, X02,
     &      XAVE, XSQR(3), Y(3), Y0

      DATA X01 /-999./, X02 /-999./
      DO 10 I = 1,3
        XSQR(I) = X(I)**2
   10 CONTINUE
C
      Y0 = SADLPP
C
      A = (Y(1) - Y(2))/((X(1) - X(3))*(X(1) - X(2))) -
     :    (Y(2) - Y(3))/((X(2) - X(3))*(X(1) - X(3)))
C
      B = (Y(2) - Y(3) - A*(XSQR(2) - XSQR(3)))/(X(2) - X(3))
C
      C = Y(1) - A*XSQR(1) - B*X(1)
C
      IF (A .EQ. 0.) GO TO 30
C
      DISCRM = B**2 - 4.*A*(C - Y0)
      IF (DISCRM .LT. 0.) GO TO 20
C
      X01 = (-B + SQRT(DISCRM))/(2.*A)
      X02 = (-B - SQRT(DISCRM))/(2.*A)
C
C     ** CHOOSE X0 AND CALCULATE TAFIT
C
      X0 = X01
      IF (ABS(TAMID-X01).GT.ABS(TAMID-X02)) X0=X02
C     -- CHOOSE SOLUTION ON SIDE OF CURVE MOST WITHIN RANGE
C     -- NOTE: PROBABLY ALWAYS CHOOSES X02 (LEFT SIDE)
      GO TO 40
C
C     **  IMAGINARY SOLUTION -- USE INFLECTION POINT OF CURVE...
C     POINT WHERE FIRST DERIVATIVE IS ZERO.
   20 CONTINUE
      X0 = -B/(A+A)
      GO TO 40
C
C     ** LINEAR FUNCTION (A = 0.)
   30 CONTINUE
      X0 = (Y0 - C)/B
C
C     ** USE AVERAGE OF ORIGINAL ESTIMATION AND FITTED VALUE
   40 CONTINUE
      XAVE = X0
      IF (DEBUIN) WRITE (JOMPB,100) A,B,C,DISCRM, XAVE,X0,X01,X02
  100 FORMAT (//,'A,B,C,DISCRM =',1P4G12.4,'  XAVE,X0,X01,X02 =',4G12.4)
      TAFIT = XAVE
      IF (TAFIT .LT. TAMIN) TAFIT = TAMIN
      IF (TAFIT .GT. TAMAX) TAFIT = TAMAX
C
      RETURN
      END
