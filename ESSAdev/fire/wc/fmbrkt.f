      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-WC $Id$
C----------
C     VIRTUALLY IDENTICAL TO PN-FFE VERSION
C     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)


COMMONS

      INCLUDE 'PRGPRM.F77'

COMMONS

      INTEGER ISP
      REAL    DBH,FMBRKT
      REAL    B1(MAXSP)

      DATA B1/
     >     0.047,  ! 1 = Pacific silver fir
     >     0.048,  ! 2 = white fir
     >     0.046,  ! 3 = grand fir
     >     0.041,  ! 4 = subalpine fir
     >     0.039,  ! 5 = California red fir/Shasta red fir
     >    -0.000,  ! 6 = ---
     >     0.045,  ! 7 = noble fir
     >     0.022,  ! 8 = Alaska cedar/western larch
     >     0.081,  ! 9 = incense cedar
     >     0.036,  !10 = Engelmann spruce/Sitka spruce - use ES
     >     0.028,  !11 = lodgepole pine
     >     0.068,  !12 = Jeffrey pine
     >     0.072,  !13 = sugar pine
     >     0.035,  !14 = western white pine
     >     0.063,  !15 = ponderosa pine
     >     0.063,  !16 = Douglas-fir
     >     0.081,  !17 = coast redwood
     >     0.035,  !18 = western redcedar
     >     0.040,  !19 = western hemlock
     >     0.040,  !20 = mountain hemlock
     >     0.024,  !21 = bigleaf maple
     >     0.026,  !22 = red alder
     >     0.060,  !23 = white alder/pacific madrone
     >     0.027,  !24 = paper birch
     >     0.045,  !25 = giant chinkapin/tanoak
     >     0.044,  !26 = quaking aspen
     >     0.044,  !27 = black cottonwood
     >     0.029,  !28 = Oregon white oak/California black oak
     >     0.025,  !29 = juniper
     >     0.050,  !30 = subalpine larch
     >     0.030,  !31 = whitebark pine
     >     0.030,  !32 = knobcone pine
     >     0.025,  !33 = Pacific yew
     >     0.062,  !34 = Pacific dogwood
     >     0.038,  !35 = hawthorn (r4 definition)
     >     0.062,  !36 = bitter cherry
     >     0.041,  !37 = willow
     >    -0.000,  !38 = ---
     >     0.044/  !39 = other (AS)

      FMBRKT = DBH*B1(ISP)

      RETURN
      END
