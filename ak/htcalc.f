      SUBROUTINE HTCALC(I,ISPC,XSITE,POTHTG,DG10)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C   THIS SUBROUTINE COMPUTES THE HEIGHT INCREMENT GIVEN TREE-SPECIFIC
C   INDEPENDENT VARIABLES SUCH AS DBH, DG AGE ...
C   CALLED FROM **HTGF**
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS

C Number  V  Code  Common Name         FIA  PLANTS Scientific Name
C   1        SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2        AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3        YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4        TA   tamarack            071  LALA   Larix laricina
C   5     P  WS   white spruce        094  PIGL   Picea glauca
C   6     P  LS   Lutz’s spruce            PILU   Picea lutzii
C   7     P  BE   black spruce        095  PIMA   Picea mariana
C   8        SS   Sitka spruce        098  PISI   Picea sitchensis
C   9        LP   lodgepole pine      108  PICO   Pinus contorta
C  10        RC   western redcedar    242  THPL   Thuja plicata
C  11        WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12        MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     P  OS   other softwoods     298  2TE
C  14        AD   alder species       350  ALNUS  Alnus species
C  15        RA   red alder           351  ALRU2  Alnus rubra
C  16     P  PB   paper birch         375  BEPA   Betula papyrifera
C  17     P  AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     P  BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     P  AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     P  CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     P  WI   willow species      920  SALIX  Salix species
C  22     P  SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     P  OH   other hardwoods     998  2TD

C------------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL DEBUG
C
CC      INTEGER I,ISPC,ISPEC,IWHO,I1,PI2,RDZ1
      INTEGER I,ISPC
C
CC      REAL POTHTG,XSITE,RDZ,ZRD(MAXPLT),CRAT,ELEVATN,XMAX
      REAL POTHTG,XSITE,ELEVATN
cc      REAL DLO,DHI,SDIC,SDIC2,A,B,AX,BX,CX,DX,EX,FX,PBAL
      REAL B1, B2, B3, B4, B5, B6, DG10
      REAL HGCAP(MAXSP), HGMLT(MAXSP)
      REAL PFNUMR,PFDENO,PFMOD
C----------
C  DATA STATEMENTS:
C----------

C ARRAY CONTAINING MAXIMUM HEIGHT VALUES THAT SPECIES ARE EXPECTED TO
C ACHIEVE. VALUES ARE USED TO CONSTRAIN HEIGHT GROWTH VALUES.
      DATA HGCAP/
     &      150.0, 120.0, 150.0,  75.0,  150.0, 150.0,  75.0, 250.0,
     &      100.0, 150.0, 200.0, 200.0,  150.0, 130.0, 130.0,  80.0,
     &      80.0,  100.0,  80.0, 100.0,   50.0,  50.0, 100.0/

C ARRAY SUPPLYING HG MULTIPLIERS FOR SPECIES. THESE MULTIPLIERS ARE USED
C TO HELP CERTAIN SPECIES HIT SITE INDEX. MUTIPIERS HELP SPECIES HIT THE 
C 95TH PERCENTILE SITE INDEX VALUE FOUND IN THE FITTING DATASET. THE
C MULTIPLIERS WERE CREATED BY TESTING WHAT MULTIPLICATION FACTOR WAS 
C NEEDED FOR A SPECIES TO ACHIEVE 95TH PERCENTILE SITE INDEX DURING
C A 100 YEAR PLANTING SIMULATION.
      DATA HGMLT/
     &     1.00, 1.00, 1.60, 1.00, 1.00, 1.00, 1.00, 1.00, 1.70, 1.45,
     &     1.25, 1.40, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,
     &     1.00, 1.00, 1.00/

C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HTCALC',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTCALC  CYCLE =',I5)

      POTHTG = 0.
      IF(DG10 .LE. 0.0) GO TO 900
C----------
C  CALCULATE HEIGHT GROWTH FOR ALL SPECIES
C----------
      IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
      ELEVATN=ELEV*100
      B1=NOPERMH1(ISPC)
      B2=NOPERMH2(ISPC)
      B3=NOPERMH3(ISPC)
      B4=NOPERMH4(ISPC)
      B5=NOPERMH5(ISPC)
      B6=NOPERMH6(ISPC)
      POTHTG=EXP(B1 + B2*(HT(I))**2 + B3*LOG(HT(I)) + B4*ELEVATN +
     &           B5*LOG(XSITE) + B6*LOG(DG10))*YR
C----------
C  CALCULATE PERMAFROST HEIGHT GROWTH MODIFIER IF LPERM IS TRUE
C----------
      IF(LPERM) THEN
        SELECT CASE (ISPC)
        CASE (5:7, 13, 16:23)
          B1=PERMH1(ISPC)
          B2=PERMH2(ISPC)
          B3=PERMH3(ISPC)
          B4=PERMH4(ISPC)
          B5=PERMH5(ISPC)
          B6=PERMH6(ISPC)

C  NUMERATOR OF PFMOD
          PFNUMR=EXP(B1 + B2 + B3*(HT(I))**2 + B4*LOG(HT(I)) + 
     &               B5*ELEVATN + B6*LOG(DG10))*YR

C  DENOMINATOR OF PFMOD
          PFDENO=EXP(B1 + B3*(HT(I))**2 + B4*LOG(HT(I)) + B5*ELEVATN +
     &               B6*LOG(DG10))*YR

C  CALCULATE PFMOD
          PFMOD=PFNUMR/PFDENO
        CASE DEFAULT
          PFMOD = 1.0
        END SELECT
      ELSE
        PFMOD = 1.0
      ENDIF
C----------
C CONSTRAIN POTHTG BASED ON SPECIES AND TREE HEIGHT
C MULTPLIERS ARE APPLIED TO CERTAIN SPECIES TO HELP THEM ACHIEVE
C SITE INDEX. MULTIPLIERS ARE PROVIDED IN HGMLT ARRAY.
C----------
      IF(HT(I) .GE. HGCAP(ISPC)) THEN
        POTHTG = 0.1
      ELSE
        POTHTG = POTHTG * HGMLT(ISPC) * PFMOD
      ENDIF
C
  900 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9)
     & I, ISPC, DBH(I), HT(I), ELEVATN, XSITE, DG10, PFMOD, POTHTG
    9 FORMAT(' IN HTCALC: I=',I5,' ISPC=',I5,' DBH=',F7.4,' HT=',F8.4,
     & ' ELEVATN=',F7.2,' XSITE=',F7.2, ' DG10=',F7.4, ' PFMOD=',F7.4,
     & ' POTHTG=',F8.4)
C
      RETURN
      END