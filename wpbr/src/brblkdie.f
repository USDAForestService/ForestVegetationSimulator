      BLOCK DATA BRBLKD
      IMPLICIT NONE
C**********************************************************************
C  **BRBLKD--IE   DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  Block data for the Blister Rust model.
C----------
C     SPECIES LIST FOR INLAND EMPIRE (NI) VARIANT.
C
C     SPECIES LIST FOR INLAND EMPIRE (IE) VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA (FR0M NI)
C     2 = WESTERN LARCH (WL)             LARIX OCCIDENTALIS (FR0M NI)
C     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII (FR0M NI)
C     4 = GRAND FIR (GF)                 ABIES GRANDIS (FR0M NI)
C     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA (FR0M NI)
C     6 = WESTERN REDCEDAR (RC)          THUJA PLICATA (FR0M NI)
C     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA (FR0M NI)
C     8 = ENGLEMAN SPRUCE (ES)           PICEA ENGELMANNII (FR0M NI)
C     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA (FR0M NI)
C    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA (FR0M NI)
C    11 = MOUNTAIN HEMLOCK (MH)          TSUGA MERTENSIANA (OT FR0M NI)
C    12 = WHITEBARK PINE (WB)            PINUS ALBICAULIS (WL FR0M NI)
C    13 = LIMBER PINE (LM)               PINUS FLEXILIS (FR0M TT)
C    14 = SUBALPINE LARCH (LL)           LARIX LYALLII (AF FR0M NI)
C    15 = PINYON PINE (PI)               PINUS EDULIS (FR0M UT)
C    16 = ROCKY MOUNTAIN JUNIPER (RM)    JUNIPERUS SCOPULORUM (FR0M UT)
C    17 = PACIFIC YEW (PY)               TAXUS BREVIFOLIA (LM FR0M TT)
C    18 = QUAKING ASPEN (AS)             POPULUS TREMULOIDES (FR0M UT)
C    19 = COTTONWOOD (CO)                POPULUS SPP. (OH FR0M CR)
C    20 = MOUNTAIN MAPLE (MM)            ACER GLABRUM (AS FROM UT)
C    21 = PAPER BIRCH (PB)               BETULA PAPYRIFERA (AS FROM UT)
C    22 = OTHER HARDWOODS (OH)           (OH FR0M CR)
C    23 = OTHER SOFTWOODS (OS)           (FR0M NI)
C----------
C
C----------------------------------------------------------------------
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-MAR-2014 Lance David (FMSC)
C     Created this file from the NI version for the IE varaint.
C     -------- Note for link to IE variant --------
C     Original host species for the model was Western white pine and
C     sugar pine when linked to NI varaint. NI variant only represented WP
C     and no other blister rust host. IE variant represents WP and limber
C     pine (LM) which is a blister rust host. For IE variant link, sugar
C     pine representation in the WPBR model is changed to LM representation.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'

C.... Data statements.

      DATA RSF/2.3,1.0,0.64/
      DATA BRPI/3.14159/
      DATA BRS0/55329D0/, BRSS/55329./

C.... Input canker data file format. Order of variables is:
C.... TreeID, StockType, TreeAge, DistUp, DistOut, %Gird, TotalCount
C.... Col 1-7      9       11-13   15-19   21-25   27-30    32-35

      DATA ICFMT/'(I7,1X,I1,1X,F3.0,1X,F5.1,1X,F5.1,1X,F4.0,1X,F4.0)'/
      DATA ICIN/55/, IDTOUT/56/, IDCOUT/57/

C.... Blister Rust Species Map.
C.... WPBR Model species/indices are: WP/1, LM/2
C.... Western White Pine set as host to Blister Rust model species 1.
C.... Limber Pine set as host to Blister Rust model species 2.
C.... 

      DATA BRSPM/1,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0/

C.... Blister Rust model alpha species codes.
      DATA BRSPC/'WP  ','LM  '/

C.... Blister Rust damage code
      DATA IBRDAM/36/

      END
