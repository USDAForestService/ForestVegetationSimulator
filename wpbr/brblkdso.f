      BLOCK DATA BRBLKD
      IMPLICIT NONE
C**********************************************************************
C  **BRBLKD--SO   DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  Block data for the Blister Rust model.
C----------
C     SPECIES LIST FOR Southern Oregon/Northeast California (SO) VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)            PINUS MONTICOLA
C     2 = SUGAR PINE (SP)                    PINUS LAMBERTIANA
C     3 = DOUGLAS-FIR (DF)                   PSEUDOTSUGA MENZIESII
C     4 = WHITE FIR (WF)                     ABIES CONCOLOR (SO - WF/GF)
C     5 = MOUNTAIN HEMLOCK (MH)              TSUGA MERTENSIANA
C     6 = INCENSE CEDAR (IC)                 LIBOCEDRUS DECURRENS
C     7 = LODGEPOLE PINE (LP)                PINUS CONTORTA
C     8 = ENGELMANN SPRUCE (ES)              PICEA ENGELMANNII
C     9 = SHASTA RED FIR (SH)                ABIES MAGNIFICA (SHASTENSIS)(FROM CA)
C    10 = PONDEROSA PINE (PP)                PINUS PONDEROSA
C    11 = WESTERN JUNIPER (WJ)               JUNIPERUS OCCIDENTALIS
C    12 = GRAND FIR (GF)                     ABIES GRANDIS (SO - WF/GF)
C    13 = SUBALPINE FIR (AF)                 ABIES LASIOCARPA
C    14 = PACIFIC SILVER FIR (SF)            ABIES AMABILIS (FROM EC)
C    15 = NOBLE FIR (NF)                     ABIES PROCERA (FROM WC)
C    16 = WHITEBARK PINE (WB)                PINUS ALBICAULIS (FROM TT)
C    17 = WESTERN LARCH (WL)                 LARIX OCCIDENTALIS (FROM EC)
C    18 = WESTERN REDCEDAR (RC)              THUJA PLICATA (FROM EC)
C    19 = WESTERN HEMLOCK (WH)               TSUGA HETEROPHYLLA (FROM WC)
C    20 = PACIFIC YEW (PY)                   TAXUS BREVIFOLIA (FROM WC)
C    21 = WHITE ALDER (WA)                   ALNUS RHOMBIFOLIA (FROM WC)
C    22 = RED ALDER (RA)                     ALNUS RUBRA (FROM WC)
C    23 = BIGLEAF MAPLE (BM)                 ACER MACROPHYLLUM (FROM WC)
C    24 = QUAKING ASPEN (AS)                 POPULUS TREMULOIDES (FROM UT)
C    25 = BLACK COTTONWOOD (CW)              POPULUS TRICHOCARPA (FROM WC)
C    26 = BITTER CHERRY (CH)                 PRUNUS EMARGINATA (FROM WC)
C    27 = OREGON WHITE OAK (WO)              QUERCUS GARRYANA (FROM CA)
C    28 = WILLOW (WI)                        SALIX sp. (FROM WC)
C    29 = GIANT CHINQUAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA (FROM WC)
C    30 = CURL-LEAF MOUNTAIN MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS (FROM WC)
C    31 = BIRCHLEAF MOUNTAIN MAHOGANY (MB)   CERCOCARPUS ALNIFOLIUS (FROM WC)
C    32 = OTHER SOFTWOODS (OS)               DOUGLAS-FIR (DF) (FROM SO)
C    33 = OTHER HARDWOODS (OH)               MISCELLANEOUS HARDWOOD (FROM WC)
C
C----------------------------------------------------------------------
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  13-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Initialize ISPBR array and IBRDAM variable.
C  30-MAR-2001 Lance R. David (FHTET)
C     Changed canker file default format and added stock type.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR array to BRSPM. Added BR alpha species code array,
C     BRSPC.
C  08-MAY-2006 Lance R. David (FHTET)
C     Changed random number seed variable names to unique variables
C     BRS0, BRSS.
C  02-JUN-2006 Lance R. David (FHTET)
C     Changed I/O units from 25, 26, 27 to units 55, 56, 57
C  12-JUN-2006 Lance R. David (FHTET)
C     Moved RIBUS initilization to brinit.
C  02-APR-2013 Lance R. David (FMSC)
C     Updated the species mapping. May have removed the wrong version
C     of this file when the 11 species SO was taken out of service.
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
C.... WPBR Model species/indices are: WP/1, SP/2
C.... Western White Pine and Sugar Pine set as host to Blister Rust

      DATA BRSPM/1,2,0,0,0,0,0,0,0,0,0,
     &           0,0,0,0,0,0,0,0,0,0,0,
     &           0,0,0,0,0,0,0,0,0,0,0/

C.... Blister Rust model alpha species codes.
      DATA BRSPC/'WP  ','SP  '/

C.... Blister Rust damage code
      DATA IBRDAM/36/

      END
