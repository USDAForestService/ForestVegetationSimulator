      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-SO $Id$
C----------
C
C     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
COMMONS
C
      INTEGER ISP
      REAL    DBH,FMBRKT
      REAL    B1(MAXSP)
C
      DATA B1/
     >     0.035,    !1  white pine
     >     0.072,    !2  sugar pine
     >     0.063,    !3  Douglas-fir
     >     0.048,    !4  white fir/grand fir - use white fir
     >     0.040,    !5  mountain hemlock
     >     0.060,    !6  incense cedar (use NI western redcedar)
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Englemann spruce
     >     0.039,    !9  red fir/subalpine fir - use red fir
     >     0.063,    !10 ponderosa pine
     >     0.025,    !11 western juniper
     >     0.046,    !12 grand fir
     >     0.041,    !13 subalpine fir
     >     0.047,    !14 pacific silver fir     
     >     0.045,    !15 noble fir     
     >     0.03,     !16 whitebark pine     
     >     0.063,    !17 western larch     
     >     0.035,    !18 western redcedar
     >     0.04,     !19 western hemlock
     >     0.025,    !20 pacific yew
     >     0.062,    !21 white alder
     >     0.026,    !22 red alder
     >     0.024,    !23 big leaf maple
     >     0.044,    !24 quaking aspen     
     >     0.044,    !25 black cottonwood
     >     0.062,    !26 bitter cherry
     >     0.029,    !27 oregon white oak     
     >     0.041,    !28 willow     
     >     0.045,    !29 giant chinkapin
     >     0.044,    !30 curl-leaf mt. mahog.- used WC-other (AS)
     >     0.044,    !31 birch-leaf mt. mahog.- used WC-other (AS)
     >     0.063,    !32 other softwoods- used Douglas-fir
     >     0.044/    !33 other hardwoods- used WC-other (AS)                                                                 
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
