      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-EC $Id$
C----------
C
C     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
COMMONS
C----------
      INTEGER ISP
      REAL    DBH,FMBRKT
      REAL    B1(MAXSP)
C
      DATA B1/
     >     0.035,    !1  white pine
     >     0.063,    !2  larch
     >     0.063,    !3  Douglas-fir
     >     0.047,    !4  pacific silver fir
     >     0.035,    !5  western redcedar
     >     0.046,    !6  grand fir
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Englemann spruce
     >     0.041,    !9  subalpine fir
     >     0.063,    !10 ponderosa pine
     >     0.040,    !11 western hemlock from WC
     >     0.040,    !12 mountain hemlock from WC
     >     0.025,    !13 Pacific yew from WC
     >     0.030,    !14 whitebark pine from WC
     >     0.045,    !15 noble fir from WC
     >     0.046,    !16 white fir - use grand fir
     >     0.050,    !17 subalpine larch from WC
     >     0.022,    !18 Alaska cedar from WC
     >     0.025,    !19 western juniper from WC juniper
     >     0.024,    !20 bigleaf maple from WC
     >     0.024,    !21 vine maple from WC bigleaf maple
     >     0.026,    !22 red alder from WC
     >     0.027,    !23 paper birch from WC
     >     0.045,    !24 golden chinkapin from WC
     >     0.062,    !25 Pacific dogwood from WC
     >     0.044,    !26 quaking aspen from WC
     >     0.044,    !27 black cottonwood from WC
     >     0.029,    !28 Oregon white oak from WC
     >     0.062,    !29 cherry and plum species from WC bitter cherry
     >     0.041,    !30 willow species from WC
     >     0.040,    !31 other softwoods - use mountain hemlock
     >     0.044/    !32 other hardwoods - use aspen
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
