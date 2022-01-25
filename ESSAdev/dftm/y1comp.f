      SUBROUTINE Y1COMP 
      IMPLICIT NONE
C---------- 
C DFTM $Id$
C---------- 
C     
C     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978. 
C     
COMMONS     
C     
      INCLUDE 'ICOND.F77'
C     
      INCLUDE 'LOWER.F77'
C     
      INCLUDE 'LIMITS.F77'
C     
COMMONS     
C     
      INTEGER I
      
      DO 10 I=1,3 
        Y1(I) = X1(I)   
   10 CONTINUE    

      X5(ICOUNT)  = Y1(1)     
      X6(ICOUNT)  = Y1(2)     
      X7(ICOUNT)  = Y1(3)     
      G19(ICOUNT) = G1(19)    

      RETURN
      END   
