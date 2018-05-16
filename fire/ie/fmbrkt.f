      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-IE-DATE OF LAST REVISION:  03/13/09
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
     &     0.035,    !1  white pine
     &     0.063,    !2  western larch
     &     0.063,    !3  Douglas-fir
     &     0.046,    !4  grand fir
     &     0.040,    !5  western hemlock
     &     0.035,    !6  western redcedar
     &     0.028,    !7  lodgepole pine
     &     0.036,    !8  Engelmann spruce
     &     0.041,    !9  subalpine fir
     &     0.063,    !10 ponderosa pine
     &     0.040,    !11 mountain hemlock
     &     0.030,    !12 whitebark pine
     &     0.030,    !13 limber pine
     &     0.050,    !14 subalpine larch
     &     0.030,    !15 pinyon pine, uses pinus spp.
     &     0.025,    !16 Rocky Mountain juniper
     &     0.025,    !17 pacific yew
     &     0.044,    !18 quaking aspen
     &     0.038,    !19 cottonwood, uses populus spp.
     &     0.040,    !20 mountain maple
     &     0.027,    !21 paper birch
     &     0.026,    !22 other hardwoods
     &     0.040/    !23 other softwoods
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
