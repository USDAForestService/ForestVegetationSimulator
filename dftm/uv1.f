      FUNCTION UV1 (KP,X,B,ALPHA,G) 
      IMPLICIT NONE
C---------- 
C  **UV1    DATE OF LAST REVISION:  06/30/10 
C---------- 
C     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978. 
C     
      INTEGER I,KP
      REAL ALPHA,B(21),G(12),U,UV1,X(4)     

      UV1 = 0.0   
      IF (G(12) .LE. 0.000001) RETURN     
      IF (KP .EQ. 0 .OR. KP .GT. 6) RETURN
      U = X(3) * X(4)   
      I = KP + 6  

C     IF(G(12).LE.0.0.OR.B(21).LE.0.0) PRINT 998,KP,G(6),G(12),B(21)    
C 998 FORMAT(' DEBUG 998:',I5,3(2X,G10.4))

      UV1 = U * (B(19) + B(20) / G(12)) * (EXP(G(12)) - 1.0) * 0.001 /  
     &      B(21) * B(I) * (1.0 - (G(6) * EXP(G(12)))**ALPHA) /   
     &      (1.0 - G(6) * EXP(G(12)))     

      RETURN
      END   
