      SUBROUTINE Z1COMP 
      IMPLICIT NONE
C---------- 
C  **Z1COMP DATE OF LAST REVISION:  06/30/10 
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
      INCLUDE 'LOWER.F77'
C     
      INCLUDE 'LIMITS.F77'
C     
COMMONS     
C     
       INTEGER I1, I2, I3, IP, J

C
C***    READ IN THE Z VECTOR, INITIAL X VECTOR   ***  
C     

      Z1(3) = FLOAT(K(3))     
      IP = K(3)   
      Z1(1) = Z2(ICOUNT)
      Z1(2) = Z3(ICOUNT)
      X1(1) = X5(ICOUNT)
      X1(2) = X6(ICOUNT)
      X1(3) = X7(ICOUNT)
      Z1(4) = FLOAT(IZ6(ICOUNT))    
      X1(4) = 0.0 

C     
C *** SET PHASE-SPECIFIC PARAMETERS B1(29) AND B1(34--36)   
C     
      B1(29) = Z1(3)    

      B1(34) = B0(IP + 48)    
      B1(35) = B0(IP + 52)    
      B1(36) = B0(IP + 62)    

C     
C***     READ IN HOST AND PHASE SPECIFIC MORTALITY VECTORS   ***  
C     
      DO 6 J=1,6  
        I1 = J + (IP - 1) * 6 
        I2 = J + (IP + 3) * 6 
        I3 = J + (IZ6(ICOUNT) - 1) * 6    

        B1(J) = R0(J+12)
        R1(J) = B0(I1)  
        R1(J+6) = B0(I2)
        R1(J+12) = R0(I3)     
    6 CONTINUE    

      RETURN
      END   
