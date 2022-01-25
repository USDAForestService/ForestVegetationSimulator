      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-TT $Id$
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
C----------
      INTEGER ISP
      REAL    DBH,FMBRKT
      REAL    B1(MAXSP)
C
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C----------
      DATA B1/
     >     0.030,    !1  whitebark pine
     >     0.030,    !2  limber pine
     >     0.063,    !3  Douglas-fir
     >     0.030,    !4  singleleaf pinyon - use Pinus sp
     >     0.036,    !5  blue spruce
     >     0.044,    !6  quaking aspen
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Engelmann spruce
     >     0.041,    !9  subalpine fir
     >     0.063,    !10 ponderosa pine - use CI (NI) PP
     >     0.025,    !11 Utah juniper - use UT western juniper
     >     0.025,    !12 Rocky Mountain juniper - use UT western juniper
     >     0.024,    !13 bigtooth maple - use SO big leaf maple
     >     0.040,    !14 Rocky Mountain maple - use IE MM (NI-mtn. hemlock)
     >     0.038,    !15 narrowleaf cottonwood - use CR Populus sp
     >     0.044,    !16 curlleaf mountain-mahogany - use SO (WC-other(AS))
     >     0.030,    !17 other softwoods - use whitebark pine
     >     0.038/    !18 other hardwoods - use CR Populus sp
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
