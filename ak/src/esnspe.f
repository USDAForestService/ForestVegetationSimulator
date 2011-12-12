      SUBROUTINE ESNSPE (PSPE,ITPP)
      IMPLICIT NONE
C----------
C  **ESNSPE--AK     DATE OF LAST REVISION:   02/14/08
C----------
C     PREDICT PROBS FOR NUMBER OF SPECIES ON STOCKED PLOTS
C
      INTEGER ITPP
      REAL PSPE(6),RITPP
C   
      RITPP=FLOAT(ITPP)
C----------
C     P(1 SPECIES ON STOCKED PLOTS)
C----------
      PSPE(1) = 0.92154 - 0.17420*RITPP + 0.03109*(RITPP**1.5)
      IF(ITPP .GE. 13) PSPE(1)=0.114
C----------
C     P(2 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.2) GO TO 78
      PSPE(2) = 0.08527 + 0.16500*RITPP - 0.03031*(RITPP**1.5)
      IF(ITPP .GE. 13) PSPE(2)=0.810
C----------
C     P(3 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.3) GO TO 78
      PSPE(3) = -0.00682 + 0.00920*RITPP - 0.00078*(RITPP**1.5)
      IF(ITPP .GE. 13) PSPE(3)=0.076
C----------
C     P(4 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.4) GO TO 78
      PSPE(4) = 0.0
C----------
C     P(5 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.5) GO TO 78
      PSPE(5) = 0.0
C----------
C     P(6 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.6) GO TO 78
      PSPE(6) = 0.0
C
   78 CONTINUE
      RETURN
      END
