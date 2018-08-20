      SUBROUTINE ESPXCS (RTOP40)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C    PREDICT THE PROBABILITY OF EXCESS SPECIES
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
      INCLUDE 'ESCOM2.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'ESHAP.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C
C  SPECIES ORDER
C  1    2    3    4    5    6    7    8    9   10   11  12  13
C WS  WRC  PSF   MH   WH  AYC   LP   SS   SAF  RA   CW  OH  OS
C----------
C
      INTEGER I
C
      REAL RTOP40
C
C----------
C     P(EXCESS WHITE SPRUCE).
C----------
      PXCS(1) = 0.0
C----------
C     P(EXCESS WESTERN RED CEDAR).
C----------
      PXCS(2) = 0.05 * OCURNF(IFO,2) * XESMLT(2)
C----------
C     P(EXCESS PACIFIC SILVER FIR).
C----------
      PXCS(3) = 0.0
C----------
C     P(EXCESS MTN HEMLOCK).
C----------
      PXCS(4) = 0.0
C----------
C     P(EXCESS WESTERN HEMLOCK).
C----------
      PXCS(5) = (60.187-0.022*RTOP40*RTOP40)/100.0 * OCURNF(IFO,5)
     &          * XESMLT(5)
C----------
C     P(EXCESS ALASKA CEDAR).
C----------
      PXCS(6) = 0.01 * OCURNF(IFO,6) * XESMLT(6)
C----------
C     P(EXCESS LODGEPOLE PINE).
C----------
      PXCS(7) = 0.0
C----------
C     P(EXCESS SITKA SPRUCE).
C----------
      PXCS(8) = (34.49+0.0272*RTOP40*RTOP40)/100.0 * OCURNF(IFO,8)
     &          * XESMLT(8)
C----------
C     P(EXCESS SUBALPINE FIR).
C----------
      PXCS(9) = 0.0
C----------
C     P(EXCESS RED ALDER).
C----------
      PXCS(10) = 0.0
C----------
C     P(EXCESS COTTONWOOD).
C----------
      PXCS(11)=0.0
C----------
C     P(EXCESS HARDWOODS).
C----------
      PXCS(12) = 0.0
C----------
C     P(EXCESS OTHER SPECIES).
C----------
      PXCS(13)=0.0
C
      DO 10 I=1,MAXSP
      IF(PXCS(I).LT.0.)PXCS(I)=0.
      IF(PXCS(I).GT.1.)PXCS(I)=1.
   10 CONTINUE
C
      RETURN
      END
