      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-CI $Id$
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
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
C----------
      DATA B1/
     >     0.035,    !1  white pine
     >     0.063,    !2  western larch
     >     0.063,    !3  Douglas-fir
     >     0.046,    !4  grand fir
     >     0.040,    !5  western hemlock
     >     0.035,    !6  western redcedar
     >     0.028,    !7  lodgepole pine
     >     0.036,    !8  Engelmann spruce
     >     0.041,    !9  subalpine fir
     >     0.063,    !10 ponderosa pine
     >     0.030,    !11 whitebark pine uses UT WB/LM = pinus spp.
     >     0.025,    !12 pacific yew uses IE PY
     >     0.044,    !13 quaking aspen uses UT AS
     >     0.025,    !14 western juniper uses UT WJ
     >     0.044,    !15 curl-leaf mt. mahog.- uses WC-other (AS)
     >     0.030,    !16 limber pine uses UT WB/LM = pinus spp.
     >     0.038,    !17 cottonwood, uses populus spp.
     >     0.040,    !18 other softwoods (mountain hemlock)
     >     0.038/    !19 other hardwoods, uses populus spp.
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
