      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-AK $Id$
C----------

C     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)

COMMONS

      INCLUDE 'PRGPRM.F77'

COMMONS

      INTEGER ISP
      REAL    DBH,FMBRKT
      REAL    B1(MAXSP)

      DATA B1/
     >     0.025,  ! 1 = white spruce
     >     0.035,  ! 2 = western redcedar
     >     0.047,  ! 3 = pacific silver fir
     >     0.040,  ! 4 = mountain hemlock
     >     0.040,  ! 5 = western hemlock
     >     0.022,  ! 6 = alaska-cedar
     >     0.028,  ! 7 = lodgepole pine
     >     0.027,  ! 8 = sitka spruce
     >     0.041,  ! 9 = subalpine fir
     >     0.026,  !10 = red alder
     >     0.044,  !11 = black cottonwood
     >     0.044,  !12 = other hardwoods
     >     0.047/  !13 = other softwoods

      FMBRKT = DBH*B1(ISP)

      RETURN
      END
