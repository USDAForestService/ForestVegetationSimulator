      SUBROUTINE ESPADV (RTOP40)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     SUBROUTINE TO PREDICT THE PROBS OF ADVANCE SPECIES.
C----------
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
      INTEGER I
      REAL RTOP40
C----------
C     P(ADVANCE WHITE SPRUCE).
C----------
      PADV(1) = 0.0
C----------
C     P(ADVANCE RED CEDAR).
C----------
      PADV(2) = 0.05 * OCURNF(IFO,2) * XESMLT(2)
C----------
C     P(ADVANCE SILVER FIR).
C----------
      PADV(3) = 0.0
C----------
C     P(ADVANCE MTN HEMLOCK).
C----------
      PADV(4) = 0.0
C----------
C     P(ADVANCE WESTERN HEMLOCK).
C----------
      PADV(5) = (60.187-0.022*RTOP40*RTOP40)/100.0 * OCURNF(IFO,5)
     &          * XESMLT(5)
C----------
C     P(ADVANCE ALASKA CEDAR).
C----------
      PADV(6) = 0.01 * OCURNF(IFO,6) * XESMLT(6)
C----------
C     P(ADVANCE LODGEPOLE PINE).
C----------
      PADV(7) = 0.0
C----------
C     P(ADVANCE SITKA SPRUCE).
C----------
      PADV(8) = (34.49+0.0272*RTOP40*RTOP40)/100.0 * OCURNF(IFO,8)
     &          * XESMLT(8)
C----------
C     P(ADVANCE SUBALPINE FIR).
C----------
      PADV(9) = 0.0
C----------
C     P(RED ALDER).
C----------
      PADV(10) = 0.0
C----------
C     P(COTTONWOOD).
C----------
      PADV(11)=0.0
C----------
C     P(ADVANCE HARDWOODS).
C----------
      PADV(12) = 0.0
C----------
C     P(ADVANCE OTHER SPECIES).
C----------
      PADV(13)=0.0
C
      DO 10 I=1,MAXSP
      IF(PADV(I).LT.0.)PADV(I)=0.
      IF(PADV(I).GT.1.)PADV(I)=1.
   10 CONTINUE
C
      RETURN
      END
