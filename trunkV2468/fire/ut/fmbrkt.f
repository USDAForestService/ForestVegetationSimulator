      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-UT-DATE OF LAST REVISION:  11/29/09
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
C----------
C SPECIES ORDER FOR UTAH VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
C 21=BI, 22=BE, 23=OS, 24=OH
C
C VARIANT EXPANSION:
C GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
C NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM UT
C----------
      DATA B1/
     >     0.030,    !1  whitebark pine
     >     0.030,    !2  limber pine
     >     0.063,    !3  Douglas-fir
     >     0.048,    !4  white fir
     >     0.031,    !5  blue spruce
     >     0.044,    !6  quaking aspen
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Engelmann spruce
     >     0.041,    !9  subalpine fir
     >     0.063,    !10 ponderosa pine
     >     0.030,    !11 common pinyon - use Pinus sp
     >     0.025,    !12 western juniper
     >     0.045,    !13 Gambel oak - use Quercus sp
     >     0.030,    !14 singleleaf pinyon - use Pinus sp
     >     0.025,    !15 Rocky Mtn juniper - use western juniper
     >     0.025,    !16 Utah juniper - use western juniper
     >     0.030,    !17 GB bristlecone pine - from CR, uses Pinus sp
     >     0.038,    !18 narrowleaf cottonwood - from CR, uses Populus sp
     >     0.038,    !19 Fremont cottonwood - from CR, uses Populus sp
     >     0.044,    !20 curl-leaf mt. mahog.- from SO, uses WC-other (AS)
     >     0.024,    !21 bigtooth maple - from SO, uses big leaf maple
     >     0.038,    !22 box elder - from CR, uses Populus sp
     >     0.030,    !23 other softwoods - use whitebark pine
     >     0.045/    !24 other hardwoods - use Quercus sp
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
