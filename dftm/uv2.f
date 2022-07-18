      FUNCTION UV2 (G, KP, X, B, ALPHA, ETA)    
      IMPLICIT NONE
C---------- 
C DFTM $Id$
C---------- 
C     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978. 
C     
      INTEGER I, KP
      REAL ALPHA,B(21),ETA,G(12),U,UV2,X(4)     

      UV2 = 0.0   
      IF (G(12) .LE. 0.000001) RETURN     
      IF (KP .EQ. 0 .OR. KP .GT. 6) RETURN

      U = X(3) * X(4)   
      I = KP + 12 
      IF (G(7) .EQ. 0.0 .AND. ETA .EQ. 0.0) RETURN    

      UV2 = U * (B(19) + B(20) / G(12)) * (EXP(G(12)) - 1.0) * 0.001 /  
     &      B(21) * B(I) * G(6)**ALPHA * EXP(ALPHA * G(12)) *     
     &      (1.0 - (G(6) * G(7) * EXP(G(12)))**ETA) / 
     &      (1.0 - G(6) * G(7) * EXP(G(12)))    

      RETURN
      END   
