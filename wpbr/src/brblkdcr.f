      BLOCK DATA BRBLKD
C----------------------------------------------------------------------
C  **BRBLKD--CR   DATE OF LAST REVISION:  06/12/2006
C----------------------------------------------------------------------
C  Purpose:
C  Block data for the Blister Rust model.
C----------
C     SPECIES LIST FOR CENTRAL ROCKIES VARIANT.
C
C     1 = SUBALPINE FIR (AF)          ABIES LASIOCARPA var. LASIOCARPA
C     2 = CORKBARK FIR (CB)           ABIES LASIOCARPA var. ARIZONICA
C     3 = DOUGLAS-FIR (DF)            PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)              ABIES GRANDIS
C     5 = WHITE FIR (WF)              ABIES CONCOLOR
C     6 = MOUNTAIN HEMLOCK (MH)       TSUGA MERTENSIANA
C     7 = WESTERN REDCEDAR (RC)       THUJA PLICATA
C     8 = WESTERN LARCH (WL)          LARIX OCCIDENTALIS
C     9 = BRISTLECONE PINE (BC)       PINUS ARISTATA
C    10 = LIMBER PINE (LM)            PINUS FLEXILIS var. FLEXILIS
C    11 = LODGEPOLE PINE (LP)         PINUS CONTORTA
C    12 = PINYON PINE (PI)            PINUS EDULIS
C    13 = PONDEROSA PINE (PP)         PINUS PONDEROSA
C    14 = WHITEBARK PINE (WB)         PINUS ALBICAULIS
C    15 = SOUTHWESTERN WHITE PINE (WP)PINUS STROBIFORMUS
C    16 = ROCKY MTN JUNIPER (JU)      JUNIPERUS SCOPULORUM
C    17 = BLUE SPRUCE (BS)            PICEA PUNGENS
C    18 = ENGELMANN SPRUCE (ES)       PICEA ENGELMANNII
C    19 = WHITE SPRUCE (WS)           PICEA GLAUCA
C    20 = QUAKING ASPEN (AS)          POPULUS TREMULOIDES
C    21 = COTTONWOODS (CO)            POPULUS sp.
C    22 = OAKS (OA)                   QUERCUS sp.
C    23 = OTHER SOFTWOODS (OS)
C    24 = OTHER HARDWOODS (OH)
C----------------------------------------------------------------------
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  23-FEB-2006 Lance David (FHTET)
C     Created this Central Rockies version from NI for purpose of testing
C     suitability to use WPBR model to represent Comandra Blister Rust on  
C     lodgepole and ponderosa pines.
C  08-MAY-2006 Lance R. David (FHTET)
C     Changed random number seed variable names to unique variables
C     BRS0, BRSS.
C  02-JUN-2006 Lance R. David (FHTET)
C     Changed I/O units from 25, 26, 27 to units 55, 56, 57
C  12-JUN-2006 Lance R. David (FHTET)
C     Moved RIBUS initilization to brinit.
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
C.... WPBR Model species/indices are: LP/1, PP/2
C.... Lodgepole and Ponderosa Pine set as host to Blister Rust
C.... (comandrae), both species use the original coefficients 
C.... and default values of white pine blister rust with NI variant.

      DATA BRSPM/0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0/

C.... Blister Rust model alpha species codes.
      DATA BRSPC/'LP  ','PP  '/

C.... Blister Rust damage code
      DATA IBRDAM/36/

      END
