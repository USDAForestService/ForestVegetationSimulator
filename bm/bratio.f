      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C BM $Id$
C----------
C FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
C REPLACES ARRAY BKRAT IN BLKDAT. 
C----------
C  COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C  COMMONS
C----------
C  SPECIES ORDER:
C   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
C  17=OS, 18=OH
C
C  SPECIES EXPANSION
C  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
C  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
C  LM USES UT LM
C  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
C  YC USES WC YC
C  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
C  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
C  OS USES BM PP BARK COEFFICIENT
C  OH USES SO OH (ORIGINALLY FROM WC VARIANT)
C----------
      REAL BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD,DIB
      INTEGER IS
      REAL RDANUW
C
      DATA BARK1/
     & 0.859045,  0.859045,  0.903563,  0.904973,  0.903563,
     &       0.,       0.9,       0.9,  0.904973,  0.809427,
     &    0.969,    0.9625,  0.933290,  0.837291,     0.950,
     & 0.075256,  0.809427,    0.9000/
C
      DATA BARK2/
     &       1.,        1.,  0.989388,        1.,  0.989388,
     &       0.,        1.,        1.,        1.,  1.016866,
     &       0.,   -0.1141,        1.,        1.,        0.,
     & 0.949670,  1.016866,        1./
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = H
C----------
      SELECT CASE (IS)
C----------
C  ORIGINAL BM VARIANT SPECIES
C     BARK COEFFICIENTS:
C     202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
C     242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133
C----------
      CASE(1:5,7:10,17)
        IF (D .GT. 0) THEN 
          DIB=BARK1(IS)*D**BARK2(IS)
          BRATIO=DIB/D
          IF(BRATIO .GT. 1.0 .OR. BRATIO .LE. 0.0) THEN
            BRATIO=.999
            GO TO 10
          ENDIF
        ELSE
          BRATIO = 0.999
          GO TO 10
        ENDIF
C----------
C  PACIFIC YEW (13=PY)
C  ALASKA CEDAR (14=YC)
C  OTHER HARDWOODS (18=OH)
C----------
      CASE(13,14,18)
        IF (D .GT. 0) THEN 
          DIB=BARK1(IS)*D**BARK2(IS)
          BRATIO=DIB/D
        ELSE
          BRATIO = 0.99
        ENDIF
C----------
C  WESTERN JUNIPER (6 = WJ)
C----------
      CASE(6)
        TEMD=D
        IF(TEMD.LT.1.)TEMD=1.
        IF(TEMD.GT.19.)TEMD=19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
C----------
C  WHITEBARK PINE (11 = WB)
C  QUAKING ASPEN (15 = AS)
C----------
      CASE(11,15)
        BRATIO=BARK1(IS)
C----------
C  LIMBER PINE (12 = LM)
C----------
      CASE(12)      
        TEMD=D
        IF(TEMD.LT.1.)TEMD=1.
        BRATIO=BARK1(IS)+BARK2(IS)*(1.0/TEMD)
C----------
C  BLACK COTTONWOOD (16 = CW)
C----------
      CASE(16)
        IF (D .GT. 0) THEN 
          DIB=BARK1(IS) + BARK2(IS)*D
          BRATIO=DIB/D
        ELSE
          BRATIO = 0.99
        ENDIF
      END SELECT
C
      IF(BRATIO .GT. 0.99) BRATIO=0.99
      IF(BRATIO .LT. 0.80) BRATIO=0.80
C
   10 CONTINUE
      RETURN
      END

