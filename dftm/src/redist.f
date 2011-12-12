      SUBROUTINE REDIST(KP, INUM)   
      IMPLICIT NONE
C---------- 
C  **REDIST DATE OF LAST REVISION:  06/30/10
C---------- 
C     
C     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978. 
C     
COMMONS     
C     
      INCLUDE 'ICOND.F77'

      INCLUDE 'GPASS.F77'

      INCLUDE 'UPPER.F77'
C
COMMONS     

       INTEGER INUM,KP,J,JCLASS,IZ6
       REAL SUMEGG,TSTEM,R0,B0,X0,G2,G3,G19,X7,X6,X5,Z3,Z2,Z5,Z4

C     
C ***              REDISTRIBUTION OF EGGS        ***  
C          INITIALIZE LOCAL VARIABLES     
C     
      SUMEGG = 0.0   
      TSTEM = 0.0 

C     
C     CALCULATE TOTAL EGGS OVER STAND (SUMEGG)     
C     AND TOTAL WEIGHTED SUM OF TREES(TOTSTEM)  
C     
      DO 10 J=1,INUM    
        SUMEGG = SUMEGG + X7(J) * Z4(J) * Z5(J) 
        TSTEM = TSTEM + Z4(J) * Z5(J)     
 10   CONTINUE    

      X0(KP) = SUMEGG / TSTEM     

C     
C     CALCULATE VALUES OF G(2,J) FOR S0 HERE,   
C     INSTEAD OF IN FUNCTION G02 CODE AND WRITE ON LUN 3    
C     
      DO 20 J=1,INUM    
        G2(J) = X7(J) + B0(62) * (X0(KP) - X7(J)) 
 20   CONTINUE    

      RETURN
      END   
