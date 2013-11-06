      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C  **BRATIO--NC  DATE OF LAST REVISION:  08/27/13
C----------
C  FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
C  REPLACES ARRAY BKRAT IN BASE MODEL.
C
C  SPECIES LIST FOR KLAMATH MOUNTAINS VARIANT.
C  1=OC,2=SP,3=DF,4=WF,5=M,6=IC,7=BO,8=TO,9=RF,10=PP,11=OH
C----------
C
      REAL BRKRAT(4,11),H,D,BRATIO,DBT,DIB
      INTEGER IS,J,I
C
      DATA ((BRKRAT(I,J),I=1,4),J=1,11)/
     &  1.,  0.1429,  0.1137,    1.0,
     &  2.,  0.1429,  0.1137,    1.0,
     &  3.,  0.1045,  0.1661,    1.0,
     &  4.,  0.1593,  0.1089,    1.0,
     &  5., -0.01348, 0.98155,   2.0,
     &  6., -0.0549,  0.1626,    1.0,
     &  7., -0.26824, 0.95767,   2.0,
     &  8., -0.26824, 0.95354,   2.0,
     &  9.,  0.1593,  0.1089,    1.0,
     &  10., 0.4448,  0.1033,    1.0,
     &  11.,-0.26824, 0.95767,   2.0/
C
C-------
C Equation Types:
C 1 DBT= a+b*DOB
C 2 DIB= a+b*DOB
C-------
C
      IF (D .GT. 0) THEN
        IF (BRKRAT(4,IS) .EQ. 1.) THEN
          DBT = BRKRAT(2,IS) + BRKRAT(3,IS)*D
          BRATIO = (D - DBT) / D
        ELSEIF (BRKRAT(4,IS) .EQ. 2.) THEN
          DIB = BRKRAT(2,IS) + BRKRAT(3,IS)*D
          BRATIO = DIB/D
        ENDIF
      ELSE
        BRATIO = 0.99
      ENDIF

C
      IF(BRATIO .GT. 0.99) BRATIO= 0.99
      IF(BRATIO .LT. 0.80) BRATIO= 0.80
C
      RETURN
      END
