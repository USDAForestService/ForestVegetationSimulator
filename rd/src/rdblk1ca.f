      BLOCK DATA RDBLK1
C----------
C  **RDBLK1-CA                              LAST REVISION:  06/25/01
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS.
C
C     This version of RDBLK1 is specific to the Inland CA, Southern
C     CAscades (ICASCA)(CA) FVS Variant representing 49 tree species.
C     Created by Lance R. David 25-JUN-2001
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
C.... are indexed as follows:
C....
C.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
C.... Species  | WP | WL | DF | GF | WH |  C | LP |  S | AF | PP |
C....
C.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
C.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
C....
C.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
C.... Species  | CB | WB | LM | CW | WS | J  | OC | GS | BO | OTH|
C....
C.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
C.... Species  | JP | TO | P  | YC | RW | LL | KP | PY | NF | NH |
C....
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.
C
C     Species present in the CA variant but not in the RD model
C     are assigned to a surrogate RD species.
C
C     SPECIES LIST FOR (ICASCA) CA VARIANT
C Index and (specie code)
C    RD       FVS
C -------   ------- 
C _______    1 (PC) PORT ORFORD CEDAR     CHAMAECYPARIS LAWSONIANA
C 14 (IC)    2 (IC) INCENSE CEDAR         LIBOCEDRUS DECURRENS
C  6 (C )    3 (RC) WESTERN REDCEDAR      THUJA PLICATA
C 13 (WF)    4 (WF) WHITE FIR             ABIES CONCOLOR
C 15 (RF)    5 (RF) CALIFORNIA RED FIR    ABIES MAGNIFICA (MAGNIFICA)
C _______    6 (SH) SHASTA RED FIR        ABIES MAGNIFICA (SHASTENSIS)
C  3 (DF)    7 (DF) DOUGLAS-FIR           PSEUDOTSUGA MENZIESII
C  5 (WH)    8 (WH) WESTERN HEMLOCK       TSUGA HETEROPHYLLA
C 11 (MH)    9 (MH) MOUNTAIN HEMLOCK      TSUGA MERTENSIANA
C 22 (WB)   10 (WB) WHITEBARK PINE        PINUS ALBICAULIS
C 37 (KP)   11 (KP) KNOBCONE PINE         PINUS ATTENUATA
C  7 (LP)   12 (LP) LODGEPOLE PINE        PINUS CONTORTA
C _______   13 (CP) COULTER PINE          PINUS COULTERI
C 23 (LM)   14 (LM) LIMBER PINE           PINUS FLEXILIS (FLEXILIS)
C 31 (JP)   15 (JP) JEFFREY PINE          PINUS JEFFREYI
C 12 (SP)   16 (SP) SUGAR PINE            PINUS LAMBERTIANA
C  1 (WP)   17 (WP) WESTERN WHITE PINE    PINUS MONTICOLA
C 10 (PP)   18 (PP) PONDEROSA PINE        PINUS PONDEROSA
C _______   19 (MP) MONTEREY PINE         PINUS RADIATA
C _______   20 (GP) GRAY PINE             PINUS SABINIANA
C 26 (J )   21 (JU) WESTERN JUNIPER       JUNIPERUS OCCIDENTALIS
C _______   22 (BR) BREWER SPRUCE         PICEA BREWERIANA
C 28 (GS)   23 (GS) GIANT SEQUOIA         SEQUOIADENDRON GIGANTEUM
C 38 (PY)   24 (PY) PACIFIC YEW           TAXUS BREVIFOLIA
C 17 (OS)   25 (OS) OTHER SOFTWOODS 
C _______   26 (LO) COAST LIVE OAK        QUERCUS AGRIFOLIA
C _______   27 (CY) CANYON LIVE OAK       QUERCUS CHRYSOLEPSIS
C _______   28 (BL) BLUE OAK              QUERCUS DOUGLASII
C _______   29 (EO) ENGELMANN OAK         QUERCUS ENGELMANNI
C _______   30 (WO) OREGON WHITE OAK      QUERCUS GARRYANA
C 29 (BO)   31 (BO) CALIFORNIA BLACK OAK  QUERCUS KELLOGGII
C _______   32 (VO) VALLEY WHITE OAK      QUERCUS LOBATA
C _______   33 (IO) INTERIOR LIVE OAK     QUERCUS WISLIZENII
C _______   34 (BM) BIGLEAF MAPLE         ACER MACROPHYLLUM
C _______   35 (BU) CALIFORNIA BUCKEYE    AESCULUS CALIFORNICA
C _______   36 (RA) RED ALDER             ALNUS RUBRA
C _______   37 (MA) PACIFIC MADRONE       ARBUTUS MENZIESII
C 32 (TO)   38 (GC) GOLDEN CHINKAPIN      CASTANOPSIS CHRYSOPHYLLA
C _______   39 (DG) PACIFIC DOGWOOD       CORNUS NUTTALLII
C _______   40 (OA) OREGON ASH            FRAXINUS LATIFOLIA
C _______   41 (WN) WALNUT                JUGLANS sp.
C 32 (TO)   42 (TO) TANOAK                LITHOCARPUS DENSIFLORUS
C _______   43 (SY) CALIFORNIA SYCAMORE   PLATANUS RACEMOSA
C 19 (AS)   44 (AS) QUAKING ASPEN         POPULUS TREMULOIDES
C 24 (CW)   45 (CW) BLACK COTTONWOOD      POPULUS TRICHOCARPA
C _______   46 (WI) WILLOW                SALIX sp.
C _______   47 (CN) CALIFORNIA NUTMEG     TORREYA CALIFORNICA
C _______   48 (CL) CALIFORNIA LAUREL     UMBELLULARIA CALIFORNICA
C 18 (OH)   49 (OH) OTHER HARDWOODS 
C
C.... The following IRTSPC is used with CA variant and corresponds with
C.... column 1 of the preceding table.
C         
      DATA IRTSPC /    41,     14,      6,     13,     15,
     &                 42,      3,      5,     11,     22,
     &                 37,      7,     43,     23,     31,
     &                 12,      1,     10,     44,     45,
     &                 26,     46,     28,     38,     17,
     &                 19,     19,     19,     99,     19,
     &                 19,     19,     19,     19,     19,
     &                 19,     19,     19,     19,     19,
     &                 19,     19,     19,     19,     24,
     &                 19,     19,     19,     18/


      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
