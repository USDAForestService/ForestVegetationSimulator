      SUBROUTINE Y0COMP 
      IMPLICIT NONE
C---------- 
C DFTM $Id$
C---------- 
C     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978. 
C     
COMMONS     
C     
      INCLUDE 'ICOND.F77'
C     
      INCLUDE 'GPASS.F77'
C     
      INCLUDE 'UPPER.F77'
C     
      INCLUDE 'LIMITS.F77'
C     
COMMONS     
C     
      INTEGER INUM
      
      KP = 4
      INUM = IC(2) - IC(1) + 1
      CALL REDIST (KP, INUM)

      RETURN
      END   
