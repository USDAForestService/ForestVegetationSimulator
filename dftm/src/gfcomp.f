      SUBROUTINE GFCOMP 
      IMPLICIT NONE
C---------- 
C  **GFCOMP DATE OF LAST REVISION:  04/01/13
C---------- 
C     
C     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978. 
C     
C  REVISION HISTORY:
C    01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C----------
C
C     
COMMONS     
C     
      INCLUDE 'LOWER.F77'
C     
      INCLUDE 'LIMITS.F77'
C     
COMMONS     
C     
      INTEGER I,N,NN
      REAL ALPHA,UV1,ETA,ETAPLS,UV2,G19TOT,G17TOT,PHI

      G1(6) = 0.0 
      IF (KP .GT. 0 .AND. KP .LE. 6) G1(6) = (1.0 - R1(KP)) *     
     &   (1.0 - R1(KP + 6)) * (1.0 - B1(KP))    

      G1(7) = 0.0 
      IF (KP .GT. 0 .AND. KP .LE. 6) G1(7) = 1.0 - R1(KP + 12)    

      G1(12) = 0.0
C     IF (X1(3) .LE. 0.0 .OR. X1(1) + X1(2) .LE. 0.0) GO TO 991   
      IF (KP .GE. 1 .AND. KP .LT. 4) G1(12) = B1(22)  
      IF (KP .EQ. 4) G1(12) = B1(23)
      IF (KP .GT. 4 .AND. KP .LE. 6) G1(12) = B1(24)  

 991  CONTINUE    
      G1(14) = AMAX1(2.0 * X1(4) / B1(26) - 1.0, 0.0) 
      IF (KP .EQ. 0 .OR. KP .GT. 6) GO TO 15    
      IF (X1(1) .LE. 0.0) GO TO 15  

      DO 10 I=1,11
        ALPHA = FLOAT(11-I)   
        G1(15) = ALPHA  
        IF (UV1(KP, X1, B1, ALPHA, G1) .LT. X1(1)) GOTO 20  
 10   CONTINUE    
      GO TO 20    

 15   G1(15) = 0.0

 20   CONTINUE    
      G1(16) = 0.0
      IF (KP .EQ. 0 .OR. KP .GT. 6) GOTO 30     
      ALPHA = AMIN1(G1(15) + 1.0, 10.0)   
      IF (X1(1) .GT. 0.0) G1(16) = UV1(KP,X1,B1,ALPHA,G1)   

 30   CONTINUE    
      IF ((X1(1) + X1(2)) .LE. 0.0) GOTO 920    
      IF (KP .EQ. 0 .OR. KP .GT. 6) GOTO 920    
      G1(17) = 10.0 - G1(15)  
      ALPHA = G1(15)    
      N = IFIX (10.0 - G1(15))
      NN = N + 1  

      DO 910 I=1,NN     
        ETA = FLOAT(I-1)
        ETAPLS = ETA + 1.     
        IF ((UV2(G1, KP, X1, B1, ALPHA, ETAPLS) - (X1(1) -  
     &      UV1(KP, X1, B1, ALPHA, G1))) .GT. X1(2)) GO TO 915    
 910  CONTINUE    
      GO TO 40    

 915  G1(17) = ETA
      GO TO 40    

 920  G1(17) = 0.0

 40   CONTINUE    
      G1(18) = 0.0
      IF (KP .EQ. 0 .OR. KP .GT. 6) GO TO 50    
      ALPHA = G1(15)    
      ETA = 10.0 - ALPHA
      G1(18) = UV2(G1, KP, X1, B1, ALPHA, ETA) -
     &        (X1(1) - UV1(KP, X1, B1, ALPHA, G1))    
      IF (G1(18) .LT. 0.0) G1(18) = 0.0   

 50   CONTINUE    
      IF (KP .EQ. 0.0) G19TOT = 0.0 
      IF (KP .GT. 0 .AND. KP .LE. 6) G19TOT = G19TOT +
     &                               (10.0 - (G1(15) + G1(17)))   
      G1(19) = G19TOT   

      G1(1) = 0.0 
      IF (KP .EQ. 8) G1(1) = (Z1(2) * Z1(1) / 100.0) *
     &   ((X1(1) + X1(2) + Z1(2)) / (2.0 * Z1(2)) - G1(19) * B1(27))    

      G1(3) = 0.0 
      IF (KP .EQ. 8) G1(3) = X1(1)  

      G1(4) = 0.0 
      IF (KP .GT. 0 .AND. KP .LE. 6) G1(4) = AMIN1(G1(16), X1(1)) 

      G1(5) = 0.0 
      IF (KP .GE. 1 .AND. KP .LE. 6) G1(5) = AMIN1(G1(18), X1(2)) 
      G1(8) = 0.0 

      IF (KP .EQ. 0 .OR. KP .GT. 6) GO TO 99    
      G1(8) = G1(6)**10.0 - 1.0     
      IF (G1(15) .NE. 10.0 .AND. G1(7) .NE. 0.0) G1(8) = (G1(6)**10.0) *
     &   (G1(7)**(10.0 - G1(15))) - 1.0   
      IF (G1(7) .EQ. 0.0 .AND. G1(15) .NE. 10.0) G1(8) = -1.0     

 99   CONTINUE    
      IF (KP .EQ. 0 .OR. KP .EQ. 9) G17TOT = 0.0
      G17TOT = G17TOT + G1(17)
      G1(9) = G17TOT    

      G1(10) = 0.0
      IF (KP .EQ. 0) G1(10) = -B1(36)     
      IF (KP .EQ. 5) G1(10) = -(1.0 + G1(8)) * B1(30) 
      IF (KP .EQ. 7) G1(10) = -B1(34)     
      IF (KP .EQ. 8) G1(10) = (B1(35) * G1(14) * (1.0 - B1(28) *  
     &                        G1(9)) - 1.0)     

      G1(11) = (G1(8) + G1(10)) * X1(3)   

      G1(13) = 0.0
      IF (KP .EQ. 0) G1(13) = B1(25)

C     IF (G1(4) .LE. 0.0 .OR. G1(18) .LE. 0.0) PRINT 997, KP,     
C    +   G1(4), G1(5), G1(18) 
C 997 FORMAT(' DEBUG 997:',I5,3(2X,G10.4))

      IF ((G1(4) + G1(18)) .GT. 0.0) PHI = (G1(4) + G1(5)) /
     &                               (G1(4) + G1(18)) 
      IF (X1(3) .LE. 0.0 .OR. X1(1) + X1(2) .LE. 0.0) G1(12) = 0.0
      IF (KP .GT. 0 .AND. KP .LE. 6) G1(13) = X1(4) * 
     &                               (EXP(10.0 * G1(12) * PHI) - 1.0)   

      F1(1) = (G1(1) - G1(4) - G1(3))     
      F1(2) = (G1(3) - G1(5)) 
      F1(3) = G1(11)    
      F1(4) = G1(13)    

      RETURN
      END   
