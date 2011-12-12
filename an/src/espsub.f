      SUBROUTINE ESPSUB (RTOP40)
      IMPLICIT NONE
C----------
C  **ESPSUB--AN     DATE OF LAST REVISION:   02/14/08
C
C     PREDICT THE PROBABILITY OF SUBSEQUENT SPECIES.
C     PLANTING COEFFICIENTS NOT INCLUDED IN THIS SUBROUTINE.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'ESCOM2.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ESHAP.F77'
C
C
COMMONS
C----------
C     P(SUBSEQUENT WHITE SPRUCE).
C----------
      INTEGER I
      REAL RTOP40
      PSUB(1) = 0.0
C----------
C     P(SUBSEQUENT WESTERN RED CEDAR).
C----------
      PSUB(2) = 0.05 * OCURNF(IFO,2) * XESMLT(2)
C----------
C     P(SUBSEQUENT PACIFIC SILVER FIR).
C----------
      PSUB(3) = 0.0
C----------
C     P(SUBSEQUENT MTN HEMLOCK).
C----------
      PSUB(4) = 0.0
C----------
C     P(SUBSEQUENT WESTERN HEMLOCK).
C----------
      PSUB(5) = (60.187-0.022*RTOP40*RTOP40)/100.0 * OCURNF(IFO,5)
     &          * XESMLT(5)
C----------
C     P(SUBSEQUENT YELLOW CEDAR).
C----------
      PSUB(6) = 0.01 * OCURNF(IFO,6) * XESMLT(6)
C----------
C     P(SUBSEQUENT LODGEPOLE PINE).
C----------
      PSUB(7) = 0.0
C----------
C     P(SUBSEQUENT SITKA SPRUCE).
C----------
      PSUB(8) = (34.49+0.0272*RTOP40*RTOP40)/100.0 * OCURNF(IFO,8)
     &          * XESMLT(8)
C----------
C     P(SUBSEQUENT SUBALPINE FIR).
C----------
      PSUB(9) = 0.0
C----------
C     P(SUBSEQUENT RED ALDER).
C----------
      PSUB(10) = 0.0
C----------
C     P(SUBSEQUENT COTTONWOOD).
C----------
      PSUB(11)=0.0
C----------
C     P(SUBSEQUENT HARDWOODS).
C----------
      PSUB(12) = 0.0
C----------
C     P(SUBSEQUENT OTHER SPECIES).
C----------
      PSUB(13)=0.0
C
      DO 10 I=1,MAXSP
      IF(PSUB(I).LT.0.)PSUB(I)=0.
      IF(PSUB(I).GT.1.)PSUB(I)=1.
   10 CONTINUE
C
      RETURN
      END
