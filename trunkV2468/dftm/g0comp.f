      SUBROUTINE G0COMP 
      IMPLICIT NONE
C---------- 
C  **G0COMP DATE OF LAST REVISION:  04/01/13
C---------- 
C     
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
C  REVISION HISTORY:
C    01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C----------
C
COMMONS     
C     
      INTEGER INUM,IP,J,I,KPP

      IF (K(3) .EQ. K(1)) RETURN    
      INUM = IC(2) - IC(1) + 1
      KP = K(3) - 1     
      IP = KP + 1 

      CALL REDIST(KP, INUM) 

      DO 10 J=1,INUM    
        G3(J) = AMIN1((1.0 - Z2(J) / 100.0) * Z3(J), X6(J)) 
   10 CONTINUE    

      DO 120 I=1,INUM   
        X6(I) = G3(I)   
        KPP = K(3) + 56 
        X7(I) = (1.0 - B0(KPP)) * G2(I)   
  120 CONTINUE    

      RETURN
      END   
