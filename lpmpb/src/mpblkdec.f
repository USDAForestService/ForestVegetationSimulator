      BLOCK DATA MPBLKD
      IMPLICIT NONE
C----------
C  **MPBLKD--EC  DATE OF LAST REVISION:  03/16/12
C----------
C
C     MOUNTAIN PINE BEETLE --
C     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
C
C REVISION HISTORY:
C   03/16/12 Lance R. David (FMSC)
C     Created this 32 species East Cascades variant version to
C     accomodate the species expansion of the variant. Surrogate species
C     assignments are based on those made in the SO, BM and IE variants.
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'MPBCOM.F77'

COMMONS
C
      DATA  JOMPB  / 7 /

      DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /,
     >      INT/ 10 /

C----------
C vv---- MPB surface area calculation surrogate specie (surfce.f)
C !!   
C !!   SPECIES LIST FOR EC VARIANT.
C !!  FVS Alpha                         
C !!   #   Code Common Name             Scientific Name
C !! ---- ----- ----------------------- ------------------------------------
C WP   1    WP  WESTERN WHITE PINE      PINUS MONTICOLA
C WL   2    WL  WESTERN LARCH           LARIX OCCIDENTALIS
C DF   3    DF  DOUGLAS-FIR             PSEUDOTSUGA MENZIESII
C DF   4    SF  PACIFIC SILVER FIR      ABIES AMABILIS
C DF   5    RC  WESTERN REDCEDAR        THUJA PLICATA
C DF   6    GF  GRAND FIR               ABIES GRANDIS
C LP   7    LP  LODGEPOLE PINE          PINUS CONTORTA
C WP   8    ES  ENGELMANN SPRUCE        PICEA ENGELMANNII
C DF   9    AF  SUBALPINE FIR           ABIES LASIOCARPA
C PP  10    PP  PONDEROSA PINE          PINUS PONDEROSA
C DF  11    WH  WESTERN HEMLOCK         TSUGA HETEROPHYLLA
C WL  12    MH  MOUNTAIN HEMLOCK        TSUGA MERTENSIANA
C WL  13    PY  PACIFIC YEW             TAXUS BREVIFOLIA
C WL  14    WB  WHITEBARK PINE          PINUS ALBICAULIS
C DF  15    NF  NOBLE FIR               ABIES PROCERA
C DF  16    WF  WHITE FIR               ABIES CONCOLOR
C WL  17    LL  SUBALPINE LARCH         LARIX LYALLII
C WL  18    YC  ALASKA CEDAR            CHAMAECYPARIS NOOTKATENSIS
C WL  19    WJ  WESTERN JUNIPER         JUNIPERUS OCCIDENTALIS
C DF  20    BM  BIGLEAF MAPLE           ACER MACROPHYLLUM
C DF  21    VN  VINE MAPLE              ACER CIRCINATUM
C WL  22    RA  RED ALDER               ALNUS RUBRA
C DF  23    PB  PAPER BIRCH             BETULA PAPYRIFERA
C DF  24    GC  GOLDEN CHINKAPIN        CASTANOPSIS CHRYSOPHYLLA
C DF  25    DG  PACIFIC DOGWOOD         CORNUS NUTTALLII
C DF  26    AS  QUAKING ASPEN           POPULUS TREMULOIDES
C DF  27    CW  BLACK COTTONWOOD        POPULUS BALSAMIFERA var. TRICHOCARPA
C DF  28    WO  OREGON WHITE OAK        QUERCUS GARRYANA
C DF  29    PL  CHERRY AND PLUM SPECIES PRUNUS sp.
C DF  30    WI  WILLOW SPECIES          SALIX sp.
C WL  31    OS  OTHER SOFTWOODS         
C DF  32    OH  OTHER HARDWOODS         
C----------


      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

C     Use appropriate surrogate species for the calculations in surfce.f
C                  WP  L DF GF WH  C LP  S AF PP WH
      DATA MPBSPM / 1, 2, 3, 3, 3, 3, 7, 1, 3,10, 3,
C                  MH PY WB NF WF LL YC WJ BM VN RA 
     &              2, 2, 2, 3, 3, 2, 2, 2, 3, 3, 2,
C                  PB GC DG AS CW WO PL WI OS OH 
     &              3, 3, 3, 3, 3, 3, 3, 3, 2, 3 /
      END
