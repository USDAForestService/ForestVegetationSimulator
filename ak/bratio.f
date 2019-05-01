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
      REAL BARKB(4,23),H,D,BRATIO,DIB,DBT
      INTEGER JBARK(23),IS
      REAL RDANUW
C----------
      DATA JBARK/
     &  1,  2,  3,  4,  5,  5,  7,  8,  9, 10,
     & 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     & 21, 22, 23/
C----------
      DATA BARKB/
     & 011.,  0.186   ,  0.45417 , 1.,
     & 019.,  0.186   ,  0.45417 , 1.,
     & 042.,  0.186   ,  0.45417 , 1.,
     & 071.,  0.796645,  1.060823, 3.,
     & 094.,  0.839825,  1.039951, 3.,
     &    .,  0.839825,  1.039951, 3.,	C Lutz's spruce has no FIA#
     & 095.,  0.796645,  1.060823, 3.,
     & 098.,  0.843047,  1.030798, 3.,
     & 108.,  0.186   ,  0.45417 , 1.,
     & 242.,  0.186   ,  0.45417 , 1.,
     & 263.,  0.186   ,  0.45417 , 1.,
     & 264.,  0.743935,  1.048186, 3.,
     & 298.,  0.839825,  1.039951, 3.,
     & 350.,  0.075256,  0.94373 , 2.,
     & 351.,  0.075256,  0.94373 , 2.,
     & 375.,  0.899145,  1.011965, 3.,
     & 376.,  0.899145,  1.011965, 3.,
     & 741.,  0.792559,  1.028571, 3.,
     & 746.,  0.921204,  0.996669, 3.,
     & 747.,  0.784468,  1.035819, 3.,
     & 920.,  0.784468,  1.035819, 3.,
     & 928.,  0.784468,  1.035819, 3.,
     & 998.,  0.784468,  1.035819, 3./
C----------
C  EQUATION TYPES AND COEFS (COLUMN 4 OF BARKB)
C  1  DBT = a * DOB ** b
C     BRATIO = (D - DBT)/D
C         TAKEN FROM EQ 4.2.1 OF AK VARIANT GUIDE
C  2  BRATIO = (a + (b * DOB))/DOB
C         TAKEN FROM EQ 4.2.2 OF AK VARIANT GUIDE
C  3  DIB = a * DOB ** b
c     BRATIO = DIB/D
c         TAKEN FROM THE GENIUS OF MARK CASTLE
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = H
C
      IF (D .GT. 0) THEN
        IF(BARKB(4,JBARK(IS)) .EQ. 1.)THEN
          DBT=BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
          BRATIO=(D-DBT)/D
        ELSEIF (BARKB(4,JBARK(IS)) .EQ. 2.)THEN
          BRATIO=(BARKB(2,JBARK(IS)) + (BARKB(3,JBARK(IS))*D))/D
        ELSEIF (BARKB(4,JBARK(IS)) .EQ. 3.)THEN
          DIB=BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
          BRATIO=DIB/D
        ELSE
          BRATIO= 0.9
        ENDIF
      ELSE
        BRATIO = 0.99
      ENDIF

C
      IF(BRATIO .GT. 0.99) BRATIO= 0.99
      IF(BRATIO .LT. 0.80) BRATIO= 0.80
C
      RETURN
      END
            
