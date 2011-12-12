      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-EC-DATE OF LAST REVISION:  02/03/03
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
     >     0.063,    !2  larch
     >     0.063,    !3  Douglas-fir
     >     0.047,    !4  pacific silver fir
     >     0.035,    !5  western redcedar
     >     0.046,    !6  grand fir
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Englemann spruce
     >     0.041,    !9  subalpine fir
     >     0.063,    !10 ponderosa pine
     >     0.040/    !11 other - use mountain hemlock
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
