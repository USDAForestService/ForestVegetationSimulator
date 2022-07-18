      SUBROUTINE DFOLE8 
      IMPLICIT NONE
C---------- 
C DFTM $Id$
C---------- 
C     
C  DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.    
C     
C******************************************************     
C     
C  *** S(0) MODULE V3.1 DFTM MODEL ***    
C     
C******************************************************     
C  REVISION HISTORY:
C    01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C----------
C     
COMMONS     
C     
      INCLUDE 'LIMITS.F77'    
C     
      INCLUDE 'DFOL.F77'
C     
      INCLUDE 'ICOND.F77'     
C     
COMMONS     
C     
      INTEGER II,I

      II = K(3) - 1     

      DO 10 I=1,ICOUNT  
        G19OUT(I,II) = G19(I) 
        DPCENT(I,II) = 100.0 * (1.0 - X6(I) / Z3(I))  
   10 CONTINUE    

      RETURN
      END   
