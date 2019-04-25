      SUBROUTINE ESPSUB (RTOP40)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICT THE PROBABILITY OF SUBSEQUENT SPECIES.
C     PLANTING COEFFICIENTS NOT INCLUDED IN THIS SUBROUTINE.
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
      PSUB(1) = 0.0
C----------
C          2     AF   subalpine fir
C
      PSUB(2) = 0.0
C----------
C          3     YC   Alaska cedar
C
      PSUB(3) = 0.01 * OCURNF(IFO,3) * XESMLT(3)
C----------
C          4     TA   tamarack
C
      PSUB(4) = 0.0
C----------
C          5     WS   white spruce
C
      PSUB(5) = 0.0
C----------
C          6     LS   Lutz’s spruce
C
      PSUB(6) = 0.0
C----------
C          7     BE   black spruce
C
      PSUB(7) = 0.0
C----------
C          8     SS   Sitka spruce
C
      PSUB(8) = (34.49+0.0272*RTOP40*RTOP40)/100.0 * OCURNF(IFO,8)
     &          * XESMLT(8)
C----------
C          9     LP   lodgepole pine
C
      PSUB(9) = 0.0
C----------
C         10     RC   western redcedar
C
      PSUB(10) = 0.05 * OCURNF(IFO,10) * XESMLT(10)
C----------
C         11     WH   western hemlock
C
      PSUB(11) = (60.187-0.022*RTOP40*RTOP40)/100.0 * OCURNF(IFO,11)
     &          * XESMLT(11)
C----------
C         12     MH   mountain hemlock
C
      PSUB(12) = 0.0
C----------
C         13     OS   other softwoods
C
      PSUB(13) = 0.0
C----------
C         14     AD   alder species

      PSUB(14) = 0.0
C----------
C         15     RA   red alder
C
      PSUB(15) = 0.0
C----------
C         16     PB   paper birch
C
      PSUB(16) = 0.0
C----------
C         17     AB   Alaska birch
C
      PSUB(17) = 0.0
C----------
C         18     BA   balsam poplar
C
      PSUB(18) = 0.0
C----------
C         19     AS   quaking aspen
C
      PSUB(19) = 0.0
C----------
C         20     CW   black cottonwood
C
      PSUB(20) = 0.0
C----------
C         21     WI   willow species
C
      PSUB(21) = 0.0
C----------
C         22     SU   Scouler’s willow
C
      PSUB(22) = 0.0
C----------
C         23     OH   other hardwoods
C
      PSUB(23) = 0.0

      DO 10 I=1,MAXSP
      IF(PSUB(I).LT.0.)PSUB(I)=0.
      IF(PSUB(I).GT.1.)PSUB(I)=1.
   10 CONTINUE
C
      RETURN
      END
