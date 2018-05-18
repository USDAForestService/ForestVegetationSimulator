      SUBROUTINE FORMCL(ISPC,IFOR,D,FC)
      IMPLICIT NONE
C----------
C EC $Id: formcl.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
C BOARD FOOT VOLUMES.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
C----------
      REAL GIFPFC(MAXSP,5),MTHDFC(MAXSP,5),OKANFC(MAXSP,5)
      REAL WENAFC(MAXSP,5),FC,D
      INTEGER IFOR,ISPC,IFCDBH
C----------
C  FOREST ORDER: (IFOR)
C  1=MOUNT HOOD(606)  2=OKANOGAN(608)  3=WENATCHEE(617)
C  4=OKANOGAN (TONASKET RD) (699) 5=GIFFORD PINCHOT(603)
C
C  SPECIES LIST FOR EAST CASCADES VARIANT.
C
C   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
C   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
C   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
C   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
C   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
C   6 = GRAND FIR               (GF)    ABIES GRANDIS
C   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
C   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
C   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
C  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
C  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
C  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
C  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
C  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
C  15 = NOBLE FIR               (NF)    ABIES PROCERA
C  16 = WHITE FIR               (WF)    ABIES CONCOLOR
C  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
C  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
C  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
C  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
C  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
C  22 = RED ALDER               (RA)    ALNUS RUBRA
C  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
C  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
C  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
C  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
C  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
C  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
C  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
C  30 = WILLOW SPECIES          (WI)    SALIX sp.
C  31 = OTHER SOFTWOODS         (OS)
C  32 = OTHER HARDWOODS         (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE EC VARIANT:
C      USE 6(GF) FOR 16(WF)
C      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
C
C
C  FROM THE WC VARIANT:
C      USE 19(WH) FOR 11(WH)
C      USE 33(PY) FOR 13(PY)
C      USE 31(WB) FOR 14(WB)
C      USE  7(NF) FOR 15(NF)
C      USE 30(LL) FOR 17(LL)
C      USE  8(YC) FOR 18(YC)
C      USE 29(WJ) FOR 19(WJ)
C      USE 21(BM) FOR 20(BM) AND 21(VN)
C      USE 22(RA) FOR 22(RA)
C      USE 24(PB) FOR 23(PB)
C      USE 25(GC) FOR 24(GC)
C      USE 34(DG) FOR 25(DG)
C      USE 26(AS) FOR 26(AS) AND 32(OH)
C      USE 27(CW) FOR 27(CW)
C      USE 28(WO) FOR 28(WO)
C      USE 36(CH) FOR 29(PL)
C      USE 37(WI) FOR 30(WI)
C----------
C  GIFFORD PINCHOT FORM CLASS VALUES
C----------
      DATA GIFPFC/
     & 84., 76., 82., 87., 70., 84., 82., 80., 80., 76.,
     & 86., 82., 60., 82., 84., 84., 75., 76., 60., 74.,
     & 74., 74., 70., 75., 74., 75., 74., 70., 74., 75.,
     & 82., 75.,
C
     & 84., 76., 82., 87., 70., 84., 82., 80., 80., 76.,
     & 86., 82., 60., 82., 84., 84., 75., 76., 60., 74.,
     & 74., 74., 70., 75., 74., 75., 74., 70., 74., 75.,
     & 82., 75.,
C
     & 84., 74., 80., 86., 68., 84., 82., 80., 80., 78.,
     & 84., 82., 60., 82., 84., 84., 75., 74., 60., 74.,
     & 74., 74., 70., 75., 74., 75., 74., 70., 74., 75.,
     & 82., 75.,
C
     & 82., 74., 79., 84., 68., 84., 82., 78., 80., 80.,
     & 84., 80., 60., 82., 82., 84., 74., 72., 60., 74.,
     & 74., 74., 70., 75., 74., 75., 74., 70., 74., 75.,
     & 80., 75.,
C
     & 82., 74., 78., 84., 68., 84., 82., 78., 80., 82.,
     & 82., 80., 60., 82., 82., 84., 74., 70., 60., 74.,
     & 74., 74., 70., 75., 74., 75., 74., 70., 74., 75.,
     & 80., 75./
C----------
C  MOUNT HOOD FORM CLASS VALUES
C----------
      DATA MTHDFC/
     & 84., 86., 76., 87., 75., 76., 76., 77., 84., 79.,
     & 78., 72., 60., 82., 78., 76., 75., 75., 60., 74.,
     & 74., 74., 70., 75., 70., 75., 74., 70., 74., 75.,
     & 72., 75.,
C
     & 76., 86., 82., 87., 82., 72., 68., 77., 84., 79.,
     & 74., 72., 60., 82., 78., 72., 75., 75., 60., 74.,
     & 74., 74., 70., 75., 70., 75., 74., 70., 74., 75.,
     & 72., 75.,
C
     & 76., 86., 82., 86., 82., 72., 68., 77., 82., 82.,
     & 74., 72., 60., 82., 78., 72., 75., 73., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 72., 75.,
C
     & 76., 86., 82., 84., 82., 72., 68., 77., 80., 83.,
     & 74., 72., 60., 82., 78., 72., 74., 70., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 72., 75.,
C
     & 76., 87., 82., 80., 82., 72., 68., 77., 75., 82.,
     & 74., 72., 60., 82., 78., 72., 74., 70., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 72., 75./
C----------
C  OKANOGAN FORM CLASS VALUES
C----------
      DATA OKANFC/
     & 78., 78., 72., 82., 75., 76., 85., 82., 84., 78.,
     & 87., 75., 56., 82., 78., 76., 75., 75., 60., 74.,
     & 74., 74., 70., 75., 70., 75., 74., 70., 74., 75.,
     & 75., 75.,
C
     & 80., 78., 72., 82., 75., 78., 85., 82., 84., 80.,
     & 87., 78., 60., 82., 78., 78., 75., 75., 60., 74.,
     & 74., 74., 70., 75., 70., 75., 74., 70., 74., 75.,
     & 78., 75.,
C
     & 80., 78., 73., 82., 72., 77., 85., 83., 85., 81.,
     & 85., 79., 60., 82., 78., 77., 75., 73., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 79., 75.,
C
     & 82., 73., 75., 84., 68., 76., 85., 86., 85., 82.,
     & 85., 79., 60., 82., 78., 76., 74., 70., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 79., 75.,
C
     & 80., 73., 75., 84., 61., 76., 85., 86., 85., 84.,
     & 84., 78., 60., 82., 78., 76., 74., 70., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 78., 75./
C----------
C  WENATCHEE FORM CLASS VALUES
C----------
      DATA WENAFC/
     & 83., 77., 75., 85., 69., 78., 82., 79., 76., 77.,
     & 87., 82., 56., 82., 78., 78., 75., 75., 60., 74.,
     & 74., 74., 70., 75., 70., 75., 74., 70., 74., 75.,
     & 82., 75.,
C
     & 84., 78., 76., 86., 70., 79., 82., 80., 77., 78.,
     & 87., 82., 60., 82., 78., 79., 75., 75., 60., 74.,
     & 74., 74., 70., 75., 70., 75., 74., 70., 74., 75.,
     & 82., 75.,
C
     & 84., 79., 75., 84., 70., 79., 82., 80., 78., 81.,
     & 85., 82., 60., 82., 78., 79., 75., 73., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 82., 75.,
C
     & 85., 80., 76., 86., 68., 79., 82., 82., 76., 81.,
     & 85., 80., 60., 82., 78., 79., 74., 70., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 80., 75.,
C
     & 84., 80., 73., 86., 70., 80., 82., 82., 77., 80.,
     & 84., 80., 60., 82., 78., 80., 74., 70., 60., 74.,
     & 74., 75., 70., 75., 70., 75., 74., 70., 75., 75.,
     & 80., 75./
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
      IF(FRMCLS(ISPC).LE.0.) THEN
        IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
        IF(IFCDBH .LT. 1) IFCDBH=1
        IF(D.GT.40.9) IFCDBH=5
C
        SELECT CASE (IFOR)
C
        CASE(1)
          FC = MTHDFC(ISPC,IFCDBH)
C
        CASE(2,4)
          FC = OKANFC(ISPC,IFCDBH)
C
        CASE(5)
          FC = GIFPFC(ISPC,IFCDBH)
C
        CASE DEFAULT
          FC = WENAFC(ISPC,IFCDBH)
C
        END SELECT
C
      ELSE
        FC=FRMCLS(ISPC)
      ENDIF
C
      RETURN
      END
