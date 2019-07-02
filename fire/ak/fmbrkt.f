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
     >     0.047,  !  1 = pacific silver fir
     >     0.041,  !  2 = subalpine fir
     >     0.022,  !  3 = alaska-cedar
     >     0.031,  !  4   tamarack
     >     0.025,  !  5 = white spruce
     >     0.025,  !  6 = Lutz's spruce
     >     0.032,  !  7 = black spruce
     >     0.027,  !  8 = sitka spruce
     >     0.028,  !  9 = lodgepole pine
     >     0.035,  ! 10 = western redcedar
     >     0.040,  ! 11 = western hemlock
     >     0.040,  ! 12 = mountain hemlock
     >     0.047,  ! 13 = other softwoods
     >     0.019,  ! 14 = alder species
     >     0.026,  ! 15 = red alder   ***** Duncan Lutes file shows 0.019
     >     0.027,  ! 16 = paper birch ***** Duncan Lutes file shows 0.019
     >     0.019,  ! 17 = Alaska birch
     >     0.040,  ! 18 = balsam poplar
     >     0.044,  ! 19 = quaking aspen
     >     0.044,  ! 20 = black cottonwood
     >     0.041,  ! 21 = willow species
     >     0.041,  ! 22 = Scouler's willow
     >     0.044/  ! 23 = other hardwoods

      FMBRKT = DBH*B1(ISP)

      RETURN
      END
