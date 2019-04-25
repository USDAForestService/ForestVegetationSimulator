      SUBROUTINE ESPADV (RTOP40)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     SUBROUTINE TO PREDICT THE PROBS OF ADVANCE SPECIES.
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
C
      INTEGER I
C
      REAL RTOP40
C
C----------
C          1     SF   Pacific silver fir
C
      PADV(1) = 0.0
C----------
C          2     AF   subalpine fir
C
      PADV(2) = 0.0
C----------
C          3     YC   Alaska cedar
C
      PADV(3) = 0.01 * OCURNF(IFO,3) * XESMLT(3)
C----------
C          4     TA   tamarack
C
      PADV(4) = 0.0
C----------
C          5     WS   white spruce
C
      PADV(5) = 0.0
C----------
C          6     LS   Lutz’s spruce
C
      PADV(6) = 0.0
C----------
C          7     BE   black spruce
C
      PADV(7) = 0.0
C----------
C          8     SS   Sitka spruce
C
      PADV(8) = (34.49+0.0272*RTOP40*RTOP40)/100.0 * OCURNF(IFO,8)
     &          * XESMLT(8)
C----------
C          9     LP   lodgepole pine
C
      PADV(9) = 0.0
C----------
C         10     RC   western redcedar
C
      PADV(10) = 0.05 * OCURNF(IFO,10) * XESMLT(10)
C----------
C         11     WH   western hemlock
C
      PADV(11) = (60.187-0.022*RTOP40*RTOP40)/100.0 * OCURNF(IFO,11)
     &          * XESMLT(11)
C----------
C         12     MH   mountain hemlock
C
      PADV(12) = 0.0
C----------
C         13     OS   other softwoods
C
      PADV(13)=0.0
C----------
C         14     AD   alder species

      PADV(14) = 0.0
C----------
C         15     RA   red alder
C
      PADV(15) = 0.0
C----------
C         16     PB   paper birch
C
      PADV(16) = 0.0
C----------
C         17     AB   Alaska birch
C
      PADV(17) = 0.0
C----------
C         18     BA   balsam poplar
C
      PADV(18)=0.0
C----------
C         19     AS   quaking aspen
C
      PADV(19)=0.0
C----------
C         20     CW   black cottonwood
C
      PADV(20)=0.0
C----------
C         21     WI   willow species
C
      PADV(21) = 0.0
C----------
C         22     SU   Scouler’s willow
C
      PADV(22) = 0.0
C----------
C         23     OH   other hardwoods
C
      PADV(23) = 0.0

      DO 10 I=1,MAXSP
      IF(PADV(I).LT.0.)PADV(I)=0.
      IF(PADV(I).GT.1.)PADV(I)=1.
   10 CONTINUE

      RETURN
      END
