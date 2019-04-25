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
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ESPARM.F77'

      INCLUDE 'ESCOM2.F77'

      INCLUDE 'ESCOMN.F77'

      INCLUDE 'ESHAP.F77'

      INCLUDE 'PLOT.F77'
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------

      INTEGER I

      REAL RTOP40

C----------
C          1     SF   Pacific silver fir
C
      PXCS(1) = 0.0
C----------
C          2     AF   subalpine fir
C
      PXCS(2) = 0.0
C----------
C          3     YC   Alaska cedar
C
      PXCS(3) = 0.01 * OCURNF(IFO,3) * XESMLT(3)
C----------
C          4     TA   tamarack
C
      PXCS(4) = 0.0
C----------
C          5     WS   white spruce
C
      PXCS(5) = 0.0
C----------
C          6     LS   Lutz’s spruce
C
      PXCS(6) = 0.0
C----------
C          7     BE   black spruce
C
      PXCS(7) = 0.0
C----------
C          8     SS   Sitka spruce
C
      PXCS(8) = (34.49+0.0272*RTOP40*RTOP40)/100.0 * OCURNF(IFO,8)
     &          * XESMLT(8)
C----------
C          9     LP   lodgepole pine
C
      PXCS(9) = 0.0
C----------
C         10     RC   western redcedar
C
      PXCS(10) = 0.05 * OCURNF(IFO,10) * XESMLT(10)
C----------
C         11     WH   western hemlock
C
      PXCS(11) = (60.187-0.022*RTOP40*RTOP40)/100.0 * OCURNF(IFO,11)
     &          * XESMLT(11)
C----------
C         12     MH   mountain hemlock
C
      PXCS(12) = 0.0
C----------
C         13     OS   other softwoods
C
      PXCS(13) = 0.0
C----------
C         14     AD   alder species

      PXCS(14) = 0.0
C----------
C         15     RA   red alder
C
      PXCS(15) = 0.0
C----------
C         16     PB   paper birch
C
      PXCS(16) = 0.0
C----------
C         17     AB   Alaska birch
C
      PXCS(17) = 0.0
C----------
C         18     BA   balsam poplar
C
      PXCS(18) = 0.0
C----------
C         19     AS   quaking aspen
C
      PXCS(19) = 0.0
C----------
C         20     CW   black cottonwood
C
      PXCS(20) = 0.0
C----------
C         21     WI   willow species
C
      PXCS(21) = 0.0
C----------
C         22     SU   Scouler’s willow
C
      PXCS(22) = 0.0
C----------
C         23     OH   other hardwoods
C
      PXCS(23) = 0.0

      DO 10 I=1,MAXSP
      IF(PXCS(I).LT.0.)PXCS(I)=0.
      IF(PXCS(I).GT.1.)PXCS(I)=1.
   10 CONTINUE
C
      RETURN
      END
