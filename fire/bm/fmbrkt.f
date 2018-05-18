      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-BM-DATE OF LAST REVISION:  05/13/09
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
     >     0.063,    !2  western larch
     >     0.063,    !3  Douglas fir
     >     0.046,    !4  grand fir
     >     0.040,    !5  mountain hemlock
     >     0.025,    !6  western juniper (use so wj)
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Engelmann spruce
     >     0.041,    !9  subalpine fir
     >     0.063,    !10 ponderosa pine
     >     0.030,    !11 whitebark pine (use so wb)
     >     0.030,    !12 limber pine (use so wb)
     >     0.025,    !13 pacific yew (use so py)
     >     0.022,    !14 Alaska cedar (use wc yc)
     >     0.044,    !15 quaking aspen (use so as)
     >     0.044,    !16 black cottonwood (use so cw)
     >     0.063,    !17 other softwoods -  use BM ponderosa pine
     >     0.044/    !18 other hardwoods - used SO OH (WC other (AS))
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
