      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-CR-DATE OF LAST REVISION:  06/18/09
C----------
C
C     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)
C
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
     >     0.041,    ! 1 subalpine fir
     >     0.041,    ! 2 corkbark fir - use subalpine fir
     >     0.063,    ! 3 Douglas-fir
     >     0.046,    ! 4 grand fir
     >     0.048,    ! 5 white fir
     >     0.040,    ! 6 mountain hemlock
     >     0.035,    ! 7 western redcedar
     >     0.063,    ! 8 western larch
     >     0.030,    ! 9 bristlecone pine - use Pinus sp
     >     0.030,    !10 limber pine
     >     0.028,    !11 lodgepole pine
     >     0.030,    !12 pinyon pine - use Pinus sp
     >     0.063,    !13 ponderosa pine
     >     0.030,    !14 whitebark pine
     >     0.035,    !15 southwestern white pine - use white pine
     >     0.025,    !16 Utah juniper
     >     0.031,    !17 blue spruce
     >     0.036,    !18 Engelmann spruce
     >     0.025,    !19 white spruce
     >     0.044,    !20 quaking aspen
     >     0.038,    !21 narrowleaf cottonwood - use Populus sp
     >     0.038,    !22 plains cottonwood - use Populus sp
     >     0.045,    !23 gambel oak - use Quercus sp
     >     0.045,    !24 Arizona white oak - use Quercus sp
     >     0.045,    !25 emory oak - use Quercus sp
     >     0.045,    !26 bur oak - use Quercus sp
     >     0.045,    !27 silverleaf oak - use Quercus sp
     >     0.044,    !28 paper birch - use quaking aspen
     >     0.025,    !29 alligator juniper - use Utah juniper
     >     0.025,    !30 Rocky Mountain juinper - use Utah juniper
     >     0.025,    !31 oneseed juniper - use Utah juniper
     >     0.025,    !32 Eastern redcedar - use Utah juniper
     >     0.030,    !33 singleleaf pinyon - use Pinus sp
     >     0.030,    !34 border pinyon - use Pinus sp
     >     0.030,    !35 Arizona pinyon - use Pinus sp
     >     0.063,    !36 Chihuahua pine - use ponderosa pine
     >     0.030,    !37 other softwoods - use Pinus sp
     >     0.038/    !38 other hardwoods - use Populus sp
C
      FMBRKT = DBH * B1(ISP)
C
      RETURN
      END
