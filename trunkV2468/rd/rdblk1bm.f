      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C  **RDBLK1-BM     LAST REVISION:  08/27/14
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS.
C
C  Previous revision date 06/05/09
C
COMMONS
C

C.... PARAMETER INCLUDE FILES

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'METRIC.F77'

C.... COMMON INCLUDE FILES

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDCRY.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDADD.F77'


C.... The array IRTSPC is used to index the species dependent arrays
C.... HABFAC, PNINF, PKILLS, RRJSP, ISPS, DBIFAC, HTIFAC, PROOT,
C.... RSLOP, ROWDOM, ROWIBP, RRPSWT, SSSFAC, IDITYP, PCOLO.
C.... In the root disease model, the defaults for these variables 
C.... are indexed as follows :
C....
C.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
C.... Species  | WP | WL | DF | GF | WH | RC | LP | ES | AF | PP |
C....
C.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
C.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
C....
C.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
C.... Species  | CB | WB | LM | CO | WS | JU | OC | GS | BO | OTH|
C....
C.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
C.... Species  | JP | TO | PI | YC | RW | LL | KP | PY | NF | NH |
C....
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.
C
C RD     
C INDX SP   SPECIES LIST FOR BLUE MOUNTAINS VARIANT.
C         
C   1  WP   1 = WESTERN WHITE PINE   119 WP  PIMO3  PINUS MONTICOLA
C   2  WL   2 = WESTERN LARCH        073 WL  LAOC   LARIX OCCIDENTALIS
C   3  DF   3 = DOUGLAS-FIR          202 DF  PSME   PSEUDOTSUGA MENZIESII
C   4  GF   4 = GRAND FIR            017 GF  ABGR   ABIES GRANDIS
C  11  MH   5 = MOUNTAIN HEMLOCK     264 MH  TSME   TSUGA MERTENSIANA
C  26  JU   6 = WESTERN JUNIPER      064 WJ  JUOC   JUNIPERUS OCCIDENTALIS
C   7  LP   7 = LODGEPOLE PINE       108 LP  PICO   PINUS CONTORTA
C   8  ES   8 = ENGLEMANN SPRUCE     093 ES  PIEN   PICEA ENGELMANNII
C   9  AF   9 = SUBALPINE FIR        019 AF  ABLA   ABIES LASIOCARPA
C  10  PP  10 = PONDEROSA PINE       122 PP  PIPO   PINUS PONDEROSA
C  22  WB  11 = WHITEBARK PINE       101 WB  PIAL   PINUS ALBICAULIS
C  23  LM  12 = LIMBER PINE          113 LM  PIFL2  PINUS FLEXILIS
C  38  PY  13 = PACIFIC YEW          231 PY  TABR2  TAXUS BREVIFOLIA
C  34  YC  14 = ALASKA YELLOW CEDAR  042 YC  CHNO   CHAMAECYPARIS NOOTKATENSIS
C  19  AS  15 = QUAKING ASPEN        746 AS  POTR5  POPULUS TREMULOIDES
C  24  CO  16 = BLACK COTTONWOOD     747 CW  POBAT  POPULUS BALSAMIFERA
C  17  OS  17 = OTHER SOFTWOODS      298 OS  2TE
C  18  OH  18 = OTHER HARDWOODS      998 OH  2TD
C
C....
C.... The following IRTSPC is used with variant BM 18 species :

      DATA IRTSPC / 1,  2,  3,  4, 11, 26,  7,  8,  9,
     &             10, 22, 23, 38, 34, 19, 24, 17, 18/

      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
