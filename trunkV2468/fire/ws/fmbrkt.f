      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-WS-DATE OF LAST REVISION:  03/23/11
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
C----------
C     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
C
C     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
C     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
C     3 = WHITE FIR (WF)                    ABIES CONCOLOR
C     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
C     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
C     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
C     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
C     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
C     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
C    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
C    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
C    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
C    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
C    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
C    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
C    16 = COULTER PINE (CP)                 PINUS COULTERI
C    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
C    18 = MONTEREY PINE (MP)                PINUS RADIATA
C    19 = GRAY PINE (GP)                    PINUS SABINIANA
C         (OR CALIFORNIA FOOTHILL PINE)
C    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
C    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
C    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
C    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
C    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
C    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
C    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
C    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
C    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
C    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
C    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
C    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
C    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
C         (OR CALIFORNIA WHITE OAK)
C    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
C    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
C    35 = GIANT CHINKAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA
C    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
C    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
C    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
C    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
C    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
C    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
C    42 = OTHER SOFTWOODS (OS)
C    43 = OTHER HARDWOODS (OH)
C
C----------
C  DATA STATEMENTS
C----------
      DATA B1/
     >     0.072,    !1  sugar pine
     >     0.063,    !2  Douglas-fir
     >     0.048,    !3  white fir
     >     0.081,    !4  giant sequoia
     >     0.060,    !5  incense cedar
     >     0.068,    !6  Jeffrey pine
     >     0.039,    !7  California red fir
     >     0.063,    !8  ponderosa pine
     >     0.028,    !9  lodgepole pine CA
     >     0.030,    !10 whitebark pine use whitebark pine CA
     >     0.035,    !11 western white pine use western white pine CA
     >     0.030,    !12 singleleaf pinyon use Pinus sp. (as done in UT)
     >     0.047,    !13 Pacific silver fir use Pacific silver fir SO
     >     0.030,    !14 knobcone pine CA
     >     0.030,    !15 foxtail pine use knobcone pine CA
     >     0.063,    !16 Coulter pine use ponderosa pine (as done in CA)
     >     0.030,    !17 limber pine use knobcone pine CA
     >     0.030,    !18 Monterey pine use Monterey pine CA
     >     0.033,    !19 gray or California foothill pine use gray pine CA
     >     0.030,    !20 washoe pine use knobcone pine CA
     >     0.030,    !21 GB bristlecone pine use UT GB = CR Pinus sp
     >     0.063,    !22 bigcone Douglas-fir use Douglas-fir
     >     0.081,    !23 redwood use giant sequoia
     >     0.040,    !24 mountain hemlock use CA mountain hemlock
     >     0.025,    !25 western juniper use CA western juniper
     >     0.025,    !26 Utah juniper use CA western juniper
     >     0.025,    !27 California juniper CA western juniper
     >     0.050,    !28 California live oak use CA California live oak
     >     0.024,    !29 canyon live oak use CA canyon live oak
     >     0.033,    !30 blue oak use CA blue oak
     >     0.030,    !31 California black oak use corkbark oak
     >     0.043,    !32 California white oak/valley oak use CA valley oak
     >     0.034,    !33 interior live oak use CA interior live oak
     >     0.052,    !34 tanoak
     >     0.045,    !35 giant chinkapin use CA/SO giant chinkapin
     >     0.044,    !36 quaking aspen use CA/SO quaking aspen
     >     0.026,    !37 California-laurel use CA California-laurel
     >     0.060,    !38 Pacific madrone use CA/NC Pacific madrone
     >     0.062,    !39 Pacific dogwood use CA Pacific dogwood
     >     0.024,    !40 bigleaf maple use CA/SO bigleaf maple
     >     0.044,    !41 curl-leaf mt. mahog use SO MC = WC-other (AS)
     >     0.028,    !42 other softwoods use lodgepole pine
     >     0.030/    !43 other hardwoods use corkbark oak
C
      FMBRKT = DBH*B1(ISP)
C
      RETURN
      END
