      BLOCK DATA BRBLKD
      IMPLICIT NONE
C----------------------------------------------------------------------
C  **BRBLKD--CR   DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  Block data for the Blister Rust model.
C----------
C     SPECIES LIST FOR CENTRAL ROCKIES VARIANT.
C
C    AL COMMON                  FIA SCIENTIFIC                         
C  # CD NAME                    CD  NAME                               
C -- -- ---------------------   --- -----------------------------------
C  1 AF SUBALPINE FIR           019 ABIES LASIOCARPA var. LASIOCARPA
C  2 CB CORKBARK FIR            018 ABIES LASIOCARPA var. ARIZONICA
C  3 DF DOUGLAS-FIR             202 PSEUDOTSUGA MENZIESII
C  4 GF GRAND FIR               017 ABIES GRANDIS
C  5 WF WHITE FIR               015 ABIES CONCOLOR
C  6 MH MOUNTAIN HEMLOCK        264 TSUGA MERTENSIANA
C  7 RC WESTERN REDCEDAR        242 THUJA PLICATA
C  8 WL WESTERN LARCH           073 LARIX OCCIDENTALIS
C  9 BC BRISTLECONE PINE        102 PINUS ARISTATA
C 10 LM LIMBER PINE             113 PINUS FLEXILIS var. FLEXILIS
C 11 LP LODGEPOLE PINE          108 PINUS CONTORTA
C 12 PI COMMON PINYON           106 PINUS EDULIS
C 13 PP PONDEROSA PINE          122 PINUS PONDEROSA
C 14 WB WHITEBARK PINE          101 PINUS ALBICAULIS
C 15 SW SOUTHWESTERN WHITE PINE 114 PINUS STROBIFORMUS
C 16 UJ UTAH JUNIPER            065 JUNIPERUS OSTEOSPERMA
C 17 BS BLUE SPRUCE             096 PICEA PUNGENS
C 18 ES ENGELMANN SPRUCE        093 PICEA ENGELMANNII
C 19 WS WHITE SPRUCE            094 PICEA GLAUCA
C 20 AS QUAKING ASPEN           746 POPULUS TREMULOIDES
C 21 NC NARROWLEAF COTTONWOOD   749 POPULUS ANGUSTIFOLIA
C 22 PW PLAINS COTTONWOOD       745 POPULUS DELTOIDES var. MONOLIFERA
C 23 GO GAMBEL OAK              814 QUERCUS GAMBELII
C 24 AW ARIZONA WHITE OAK       803 QUERCUS ARIZONICA
C 25 EM EMORY OAK               810 QUERCUS EMORYI
C 26 BK BUR OAK                 823 QUERCUS MACROCARPA
C 27 SO SILVERLEAF OAK          843 QUERCUS HYPOLEUCOIDES
C 28 PB PAPER BIRCH             375 BETULA PAPYRIFERA
C 29 AJ ALLIGATOR JUNIPER       063 JUNIPERUS DEPPEANA
C 30 RM ROCKY MOUNTAIN JUNIPER  066 JUNIPERUS SCOPULORUM
C 31 OJ ONESEED JUNIPER         069 JUNIPERUS MONOSPERMA
C 32 ER EASTERN REDCEDAR        068 JUNIPERUS VIRGINIANA
C 33 PM SINGLELEAF PINYON       133 PINUS MONOPHYLLA
C 34 PD BORDER PINYON           134 PINUS DISCOLOR
C 35 AZ ARIZONA PINYON PINE     143 PINUS MONOPHYLLA var. FALLAX
C 36 CI CHIHUAHUA PINE          118 PINUS LEIOPHYLLA var. CHIHUAHUANA
C 37 OS OTHER SOFTWOODS         298
C 38 OH OTHER HARDWOODS         998
C
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
C  03-JUN-2014 Lance R. David (FMSC)
C     Modified for the CR 38 species representation.
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
C....
C.... FVS Central Rockies Species list for all model types:
C....              1   2   3   4   5   6   7   8   9  10  11  12  -- FVS index
C....             AF  CB  DF  GF  WF  MH  RC  WL  BC  LM  LP  PI  -- FVS species
      DATA BRSPM / 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,
C....             13  14  15  16  17  18  19  20  21  22  23  24  -- FVS index
C....             PP  WB  SW  UJ  BS  ES  WS  AS  NC  PW  GO  AW  -- FVS species
     &             2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
C....             25  26  27  28  29  30  31  32  33  34  35  36  -- FVS index
C....             EM  BK  SO  PB  AJ  RM  OJ  ER  PM  PD  AZ  CI  -- FVS species
     &             0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
C....             37  38                                          -- FVS index
C....             OS  OH                                          -- FVS species
     &             0,  0/

C.... Blister Rust model alpha species codes.
      DATA BRSPC/'LP  ','PP  '/

C.... Blister Rust damage code
      DATA IBRDAM/36/

      END
