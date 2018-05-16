      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C  **RDBLK1-WC     LAST REVISION:  08/27/14
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS.
C
C  Previous revision date 04/30/09
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

C.... The following IRTSPC is used with variant WC :
C.... WC has 39 species.
C RD     
C INDX SP   SPECIES LIST FOR WEST CASCADES VARIANT
C        
C  16  SF     1 = PACIFIC SILVER FIR (SF)      ABIES AMABILIS
C  13  WF     2 = WHITE FIR (WF)               ABIES CONCOLOR
C   4  GF     3 = GRAND FIR (GF)               ABIES GRANDIS
C   9  AF     4 = SUBALPINE FIR (AF)           ABIES LASIOCARPA
C  15  RF     5 = CALIFORNIA RED FIR (RF)/     ABIES MAGNIFICA
C                 SHASTA RED FIR
C  40  NH     6 = ---
C  39  NF     7 = NOBLE FIR (NF)               ABIES PROCERA
C  34  YC     8 = ALASKA CEDAR (YC)/           CHAMAECYPARIS NOOTKATENSIS
C                 WESTERN LARCH                LARIX OCCIDENTALIS
C  14  IC     9 = INCENSE CEDAR (IC)           LIBOCEDRUS DECURRENS
C   8  ES    10 = ENGELMANN SPRUCE (ES)/       PICEA ENGELMANNII
C                 SITKA SPRUCE                 PICEA SITCHENSIS
C   7  LP    11 = LODGEPOLE PINE (LP)          PINUS CONTORTA
C  31  JP    12 = JEFFREY PINE (JP)            PINUS JEFFREYI
C  12  SP    13 = SUGAR PINE (SP)              PINUS LAMBERTIANA
C   1  WP    14 = WESTERN WHITE PINE (WP)      PINUS MONTICOLA
C  10  PP    15 = PONDEROSA PINE (PP)          PINUS PONDEROSA
C   3  DF    16 = DOUGLAS-FIR (DF)             PSEUDOTSUGA MENZIESII
C  35  RW    17 = COAST REDWOOD (RW)           SEQUOIA SEMPERVIRENS
C   6  RC    18 = WESTERN REDCEDAR (RC)        THUJA PLICATA
C   5  WH    19 = WESTERN HEMLOCK (WH)         TSUGA HETEROPHYLLA
C  11  MH    20 = MOUNTAIN HEMLOCK (MH)        TSUGA MERTENSIANA
C  40  NH    21 = BIGLEAF MAPLE (BM)           ACER MACROPHYLLUM
C  40  NH    22 = RED ALDER (RA)               ALNUS RUBRA
C  40  NH    23 = WHITE ALDER (WA) /           ALNUS RHOMBIFOLIA
C                 PACIFIC MADRONE              ARBUTUS MENZIESII
C  40  NH    24 = PAPER BIRCH (PB)             BETULA PAPYRIFERA
C  40  NH    25 = GIANT CHINKAPIN (GC) /       CASTANOPSIS CHRYSOPHYLLA
C                 TANOAK                       LITHOCARPUS DENSIFLORUS
C  19  AS    26 = QUAKING ASPEN (AS)           POPULUS TREMULOIDES
C  40  NH    27 = BLACK COTTONWOOD (CW)        POPULUS TRICHOCARPA
C  40  NH    28 = OREGON WHITE OAK (WO) /      QUERCUS GARRYANA
C                 CALIFORNIA BLACK OAK         QUERCUS KELLOGGII
C  26  JU    29 = WESTERN JUNIPER (WJ)         JUNIPERUS OCCIDENTALIS
C  36  LL    30 = SUBALPINE LARCH (LL)         LARIX LYALLII
C  22  WB    31 = WHITEBARK PINE (WB)          PINUS ALBICAULIS
C  37  KP    32 = KNOBCONE PINE (KP)           PINUS ATTENUATA
C  38  PY    33 = PACIFIC YEW (PY)             TAXUS BREVIFOLIA
C  40  NH    34 = PACIFIC DOGWOOD (DG)         CORNUS NUTTALLII
C  40  NH    35 = HAWTHORN (HT)                CRATAEGUS sp.
C  40  NH    36 = BITTER CHERRY (CH)           PRUNUS EMARGINATA
C  40  NH    37 = WILLOW (WI)                  SALIX sp.
C  40  NH    38 = ---
C  40  NH    39 = OTHER (OT)
C

      DATA IRTSPC /    16,     13,      4,      9,     15,
     &                 40,     39,     34,     14,      8,
     &                  7,     31,     12,      1,     10,
     &                  3,     35,      6,      5,     11,
     &                 40,     40,     40,     40,     40,
     &                 19,     40,     40,     26,     36,
     &                 22,     37,     38,     40,     40,
     &                 40,     40,     40,     40/


      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
