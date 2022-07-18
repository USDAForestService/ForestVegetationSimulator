      SUBROUTINE DMNAC(A,B)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMNAC -- NISI  Date of last revision: January 10, 2002
C----------------------------------------------------------------------
C Purpose:
c     set the SF() based on alpha, beta
C----------------------------------------------------------------------
C
C Called by:
C
C     DMOPTS
C     DMTREG
C     PPDMTREG
C
C
C Argument list definitions:                        
C
C     REAL      A         ALPHA TERM
C     REAL      B         BETA TERM
C
C Local variable definitions:
C
C     INTEGER   i         Loop counter for various things: options,
C                          trees, plots and DM categories.
C     INTEGER   j         Loop counter for sampling rings.
C     REAL      x         Scalar used in calculation of sample ring
C                          modified density; also used to hold
C                          trees/acre density of individual records.
C     REAL      tmp       Temporary calculation for sample ring
C                          density scaling.
C     REAL      PltDns   Dnsty (trees/acre) for each sample point.
C
C Common block variables and parameters:
C
C     MXTHRX    DMCOM
C     DMALPH    DMCOM
C     DMBETA    DMCOM
C     SF        DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C     LOCAL VARIABLES.

      INTEGER   I, J      
      REAL      X, A, B, TMP(MXTHRX)

      DO J = 1, MXTHRX
        TMP(J) = EXP(DSTNCE(J) * B)
      ENDDO

      DO I = 0,6
        X = FLOAT(I) * A
        DO J = 1,MXTHRX
          SF(I, J) = EXP(X * TMP(J))
        ENDDO
      ENDDO

      RETURN
      END
