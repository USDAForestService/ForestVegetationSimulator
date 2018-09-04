C----------
C ORGANON $Id$
C----------
C     Subroutines required for the calculation of
C     height growth using the western hemlock top height curves of
C     Flewelling.  These subroutines are required:
C       SITECV_F   computes top height from site and age
C       SITEF_C    computes model parameters
C       SITEF_SI   calculates an approximate psi for a given site
C       Note: Flewelling's curves are metric.
C             Site Index is not adjusted for stump height.
C             These subroutines contain unique commons not in the include file.
C----------------------------------------------------------------------
C
C**********************************************************************
      SUBROUTINE SITECV_F(SI,AGE,HTOP)
C
C----------------------------------------------------------------------
C     Purpose:  Implements new height-increment methods (F)
C
C     Current Date: FEB 2, 1994    J. FLEWELLING
C
C     SI    IN     R*4    Site index (m) (basis is BH AGE 50)
C     AGE   IN     R*4    Breast height age (.= 1.0)
C     HTOP  OUT    R*4    Top height (== site height) (m)
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*4 SI,AGE,HTOP
      REAL*4 OLD_SI,XK,B1,C,BETA,ALPHA,H1,YK,PPSI
      REAL*4 PSI,X,Z
C
      SAVE OLD_SI
      COMMON /SITEFPRM/XK,B1,C,BETA,ALPHA,H1,YK,PPSI
C
C                           determine if coefficients for this SI are
C                          already in siteprm. If not, get them.
      IF(SI. NE. OLD_SI) THEN
             OLD_SI = SI
             CALL SITEF_SI(SI,PSI)
             CALL SITEF_C (PSI)
      ENDIF
C                                         apply height-age equation.
      X=AGE-1.0
      IF(X .LT. XK)THEN
         HTOP = H1+PPSI*X
     1        + (1.0-B1)*PPSI*XK/(C+1.0)*(((XK-X)/XK)**(C+1.0)-1.0)
      ELSE
         Z = X-XK
         HTOP = YK+ALPHA*(1.0-EXP(-BETA*Z))
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE SITEF_C(PSI)
C
C----------------------------------------------------------------------
C     Purpose:  For a specified psi, calculates all of the height-age
C               model parameters, and stores them in /sitefprm/
C
C     Current Date: FEB 2, 1994    J. FLEWELLING
C
C     psi     input   REAL     productivity index (m/yr)
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*4 PSI
      REAL*4 XK,B1,C,BETA,ALPHA,H1,YK,PPSI
      REAL*4 FP(10)
      INTEGER*4 I
      COMMON /SITEFPRM/XK,B1,C,BETA,ALPHA,H1,YK,PPSI
C                                                   R24 Coefficients
      DATA (FP(I),I=1,5),FP(9)  /128.326, -2.54871, 5.33208, -9.00622,
     1                           1.2, 52.7948/
      PPSI=PSI
      XK = FP(1)*EXP(FP(2)*PSI)
      B1 = 0.2+0.8/(1.0+EXP(FP(3)+FP(4)*PSI))
      C  =1.0+FP(5)*PSI
      ALPHA = FP(9)*PSI
      H1 = 1.3+(B1*PSI)/2.0
      YK = H1+PSI*XK*(1.0-(1.0-B1)/(C+1.0))
      BETA = PSI/ALPHA
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE SITEF_SI(SI,PSI)
C
C----------------------------------------------------------------------
C     Purpose:  Calculates an approximate psi for a given site index
C               Ref 'Model Fitting: top height increment', Feb 2, 1994.
C
C     Current Date: FEB 2, 1994    J. FLEWELLING and A. ZUMRAWI
C
C          si      input  r*4    site index (top height at BH age 50)
C          psi     output r*4    site productivity parameter.
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*4 SI,PSI
      REAL*4 SI_PIV,B(6,2),X
      INTEGER*4 I,J
      DATA SI_PIV  /32.25953/
      DATA (B(I,1),I=1,6) /0.299720, 0.116875, 0.074866, 0.032348,
     1                     0.006984, 0.000339/
      DATA (B(I,2),I=1,6) /0.290737, 0.129665, -0.058777,
     1                    -0.000669, 0.006003, -0.001060/
      IF(SI.LE. SI_PIV)THEN
         J=1
      ELSE
         J=2
      ENDIF
      X = (SI-SI_PIV)/10.0
      PSI = 0.75 + X*( B(1,J) + X*( B(2,J)  + X*( B(3,J)
     1           + X*( B(4,J) + X*( B(5,J)  + X*B(6,J) )))))
      RETURN
      END
