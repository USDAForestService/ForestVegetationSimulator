      SUBROUTINE DFTMOD (ISTART,IFIN)     
      IMPLICIT NONE
C---------- 
C  **DFTMOD DATE OF LAST REVISION:  04/01/13
C---------- 
C     
C     *** DOUGLAS-FIR TUSSOCK MOTH INSECT AND BRANCH MODEL ***    
C     DEVELOPED BY SCOTT OVERTON, JIM COLBERT AND CURTIS WHITE    
C     AT OREGON STATE UNIVERSITY, CORVALLIS, OREGON.  
C     
C     MODIFIED FOR USE IN THE PROGNOSIS MODEL BY JIM COLBERT,     
C     JAN. 1978   
C
C  REVISION HISTORY:
C    01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C----------
C
COMMONS     
C     
      INCLUDE 'TMCOM1.F77'
C     
      INCLUDE 'UPPER.F77'
C     
      INCLUDE 'LOWER.F77'
C     
      INCLUDE 'LIMITS.F77'
C     
      INCLUDE 'DFOL.F77'
C     
      INCLUDE 'ICOND.F77'
C     
      INCLUDE 'GPASS.F77'
C     
COMMONS     
C     
      INTEGER IFIN,ISTART,I
      INTEGER K1,K2,IC1,IC2,K3,IC3,IX
C
C                  K(1):  BEGINNING PHASE 
C                  K(2):  LAST PHASE
C                  K(3):  PHASE COUNTER   
C                  IC(1): FIRST TREE CLASS
C                  IC(2): LAST TREE CLASS 
C                  IC(3): TREE CLASS COUNTER    
C     
C     
C                 SET INITIAL K AND IC VAR.     
C     
      K(1) = 1    
      K(2) = 3    
      IC(1) = ISTART    
      IC(2) = IFIN
      ICOUNT = IC(2)-IC(1)+1  
C     
C     SUPPRESS REDISTRIBUTION IF LREDIS IS FALSE
C     
      B0(57) = ICOUNT   
C     
C     WRITE PARAMETER FILE TO JOTMDK
C     
      IF (.NOT. LPUNCH) GO TO 67    
      WRITE (JOTMDK,65) 
   65 FORMAT (' LIST OF DFTMOD PARAMETERS:')    
      WRITE (JOTMDK,66) B0,R0,(B1(I),I=1,30)    
   66 FORMAT (6F12.7)   

   67 CONTINUE    
C     
C                  START SIMULATION 
C     
      DO 3 I=1,100
        DPCENT(I,1) = 0.0     
        DPCENT(I,2) = 0.0     
    3 CONTINUE    

      K1 = K(1)   
      K2 = K(2)   
      IC1 = IC(1) 
      IC2 = IC(2) 

      DO 100 K3=K1,K2   
        K(3) = K3 
C     
C       PROCESS G FUNCTIONS FOR UPPER MODULE    
C     
        CALL G0COMP     

C     
C       PROCESS LOWER MODULE FOR DESIRED TREE CLASSES 
C     
        DO 50 IC3=IC1,IC2     
          IC(3) = IC3   
          ICOUNT = IC3 - IC1 + 1    

C     
C         CALL ROUTINE TO SET UP PARAMETERS     
C     
          CALL Z1COMP   

C     
C         DO LOWER LEVEL PROCESSING IN MODULE S(1)    
C     
          DO 10 I=1,10  
C     
C           SET LOWER LEVEL TIME INDEX, KP
C     
            KP = I - 1  
C     
C           PROCESS G AND F FUNCTIONS IN MODULE S(1)  
C     
            CALL GFCOMP 
C     
C           UPDATE STATE VARIABLES IN S(1)
C     
            DO 6 IX=1,4 
              X1(IX) = X1(IX) + F1(IX)    
C     
C             CHECK FOR NEGATIVE X VALUES 
C     
              IF (X1(IX) .GE. 0.0) GO TO 6

C     
C             PRINT MESSAGE AND RESET X   
C     
              IF (TMDEBU) WRITE (JODFTM,1000) IX,X1(IX),I,IC(3),K(3)    
 1000         FORMAT ('0X(',I1,')=',F14.6,' AT OCCASION ',I2,     
     &                ' FOR TREE CLASS',I3,' IN PHASE',I2/  
     &                ' RESET TO 0.0')    
              X1(IX) = 0.0    
    6       CONTINUE    
   10     CONTINUE

C     
C         FINISH UPPER LEVEL PROCESSING IN MODULE S(1)
C     
          CALL Y1COMP   
   50   CONTINUE  
        IF (K3 .EQ. 2 .OR. K3 .EQ. 3) CALL DFOLE8     
  100 CONTINUE    

C     
C     FINISH UPPER LEVEL PROCESSING IN MODULE S(0)    
C     
      CALL Y0COMP 

C*****************************************************************

      IF (.NOT. TMDEBU) RETURN
      WRITE (JODFTM,9992)     
 9992 FORMAT('0   ***PERCENT BRANCH  DEFOLIATION FROM DFTMOD***'//
     >'     JCLASS       IZ6   DPCENT1   DPCENT2'//)  

      DO 999 I=1,ICOUNT 
        WRITE (JODFTM,9991) I, IZ6(I), DPCENT(I,1), DPCENT(I,2)   
  999 CONTINUE    
 9991 FORMAT(1X,2I10,2F10.4)  

C**************************************************************** 

      RETURN
      END   
