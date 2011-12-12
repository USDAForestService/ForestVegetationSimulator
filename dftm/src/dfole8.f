      SUBROUTINE DFOLE8 
      IMPLICIT NONE
C---------- 
C  **DFOLE8 DATE OF LAST REVISION:  06/30/10 
C---------- 
C     
C  DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.    
C     
C******************************************************     
C     
C  *** S(0) MODULE V3.1 DFTM MODEL ***    
C     
C******************************************************     
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
      INTEGER II,I,JCLASS,IZ6,ICOUNT,IC,KP,K
      REAL G19,X7,X6,X5,Z3,Z2,Z5,Z4,G19OUT,DPCENT

      II = K(3) - 1     

      DO 10 I=1,ICOUNT  
        G19OUT(I,II) = G19(I) 
        DPCENT(I,II) = 100.0 * (1.0 - X6(I) / Z3(I))  
   10 CONTINUE    

      RETURN
      END   
