      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C  **BRATIO--TT   DATE OF LAST REVISION:  04/09/10
C----------
C
C FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
C REPLACES ARRAY BKRAT IN BASE MODEL.  
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C----------
      REAL BARK1(18),BARK2(18),H,D,BRATIO,TEMD,DIB
      INTEGER IMAP(18),IS,IEQN
C
      DATA BARK1/  0.969,   0.969,    0.867,       0.,    0.956,
     &             0.969,   0.969,    0.956,    0.937, 0.809427,
     &                0.,      0.,  0.94782,    0.950,    0.892,
     &               0.9,   0.969,    0.892/
C
      DATA BARK2/     0.,      0.,       0.,       0.,       0.,
     &                0.,      0.,       0.,       0., 1.016866,
     &                0.,      0.,   0.0836,       0.,   -0.086,
     &                0.,      0.,   -0.086/
C
      DATA IMAP/ 2, 2, 2, 1, 2, 2, 2, 2, 2, 4,
     &           1, 1, 3, 2, 3, 2, 2, 3/
C----------
      IEQN=IMAP(IS)
      TEMD=D
      IF(TEMD.LT.1.)TEMD=1.
C
      IF(IEQN .EQ. 1) THEN
        IF(BARK1(IS).EQ.0.0 .AND. BARK2(IS).EQ.0.0)THEN
          IF(TEMD.GT.19.)TEMD=19.
          BRATIO = 0.9002 - 0.3089*(1/TEMD)
        ELSE
          BRATIO=BARK1(IS)+BARK2(IS)*D
          BRATIO=1.0/BRATIO
        ENDIF
C
      ELSEIF(IEQN .EQ. 2)THEN
        BRATIO=BARK1(IS)
C
      ELSEIF(IEQN .EQ. 3)THEN
        BRATIO=BARK1(IS)+BARK2(IS)*(1.0/TEMD)
      ELSEIF(IEQN .EQ. 4)THEN
      	DIB=BARK1(IS)*D**BARK2(IS)
      	BRATIO=DIB/D
      	IF (BRATIO .GT. 0.97) BRATIO=0.97
      	GO TO 100
      ENDIF
C
      IF(BRATIO .GT. 0.99) BRATIO=0.99
      IF(BRATIO .LT. 0.80) BRATIO=0.80
C
  100 CONTINUE
      RETURN
      END
