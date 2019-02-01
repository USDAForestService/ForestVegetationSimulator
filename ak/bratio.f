      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER
C
C SPECIES LIST FOR ALASKA VARIANT.
C
C Number Code  Common Name         FIA  PLANTS Scientific Name
C   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4     TA   tamarack            071  LALA   Larix laricina
C   5     WS   white spruce        094  PIGL   Picea glauca
C   6     LS   Lutz’s spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TE
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TD
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER IS
C
      REAL BRATIO,D,DD,DIB,H,RDANUW
C
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = H
C----------
C  RED ALDER AND COTTONWOOD (FROM PN VARIANT)
C----------
      IF(IS.EQ.10 .OR. IS.EQ.11) THEN
        IF (D .GT. 0) THEN
          DIB=0.075256 + 0.94373*D
          BRATIO=DIB/D
        ELSE
          BRATIO = 0.99
        ENDIF
      ELSE
C----------
C  EQUATIONS FOR OTHER SPECIES
C  COMPUTE DOUBLE BARK THICKNESS AND STORE IN BRATIO
C----------
        DD=D
        IF(DD .LT. 1.)DD=1.
        BRATIO = 0.186 * (DD ** 0.45417)
C----------
C SUBTRACT DOUBLE BARK THICKNESS FROM DIAMETER, THEN DIVIDE BY
C DIAMETER TO GET BARK RATIO.
C----------
        BRATIO = (DD - BRATIO) / DD
      ENDIF

      RETURN
      END
