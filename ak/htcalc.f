      SUBROUTINE HTCALC(I,ISPC,XSITE,POTHTG,ZRD)
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
      INTEGER I,ISPC,RDZ1
C
CC      REAL POTHTG,XSITE,RDZ,ZRD(MAXPLT),CRAT,ELEVATN,XMAX
      REAL POTHTG,XSITE,RDZ,ZRD(MAXPLT),CRAT,ELEVATN
cc      REAL DLO,DHI,SDIC,SDIC2,A,B,AX,BX,CX,DX,EX,FX,PBAL
      REAL B1, B2, B3, B4, B5, B6, B7, PBAL
      REAL HGCAP(MAXSP), HGMLT(MAXSP)
C----------
C  DATA STATEMENTS:
C----------
C ARRAY FOR CONSTRAINING HEIGHT GROWTH BY SPECIES
      DATA HGCAP/
     &      150.0, 100.0, 140.0, 50.0,  120.0, 120.0, 50.0,  250.0,
     &      100.0, 150.0, 200.0, 150.0, 120.0, 120.0, 120.0, 80.0,
     &      80.0,  100.0, 80.0,  100.0, 100.0, 100.0, 100.0/

C ARRAY SUPPLYING HG MULTIPLIERS FOR SPECIES. THESE MULTIPLIERS ARE USED
C TO HELP CERTAIN SPECIES HIT SITE INDEX.
      DATA HGMLT/
     &     1.20, 1.20, 1.80, 1.00, 1.00, 1.00, 1.00, 1.20, 1.85, 1.40,
     &     1.25, 1.65, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,
     &     1.00, 1.00, 1.00/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HTCALC',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTCALC  CYCLE =',I5)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC This piece of code relocated to htgf.f because it needs to be 
CC done just once. Find it near line 76 of htgf.f
CC
CC  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
CC  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
CC  TO DO SO. IWHO PARAMETER HAS NO AFFECT ON XMAXPT ARRAY VALUES.
CC
C      IWHO = 2
C      CALL SDICAL (IWHO, XMAX)
C
CC
CC  COMPUTE RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
CC  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
CC
C      DLO = 0.0
C      DHI = 500.0
C      ISPEC = 0
C      IWHO = 1
C      PI2=INT(PI)
Ccld      DO I1 = I, PI2
C      DO I1 = 1, PI2
C         CALL SDICLS (ISPEC,DLO,DHI,IWHO,SDIC,SDIC2,A,B,I1)
C         ZRD(I1) = SDIC2
C      END DO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     CALCULATE RD AND THEN CONSTRAIN RD IF NEEDED
      RDZ1=ITRE(I)
      IF (XMAXPT(RDZ1).EQ.0.0) THEN
        RDZ = 0.01
      ELSE
        RDZ = ZRD(RDZ1) / XMAXPT(RDZ1)
      ENDIF

      POTHTG = 0.
      SELECT CASE(ISPC)
C----------
C  CALCULATE HIEGHT GROWTH FOR BOREAL SPECIES WITH AND WITHOUT PERMAFROST
C----------
      CASE(4:7,13,16:23)
        IF(LPERM) THEN
          IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
          PBAL=PTBALT(I)
          CRAT=ICR(I)
          ELEVATN=ELEV*100
          B1=PERMH1(ISPC)
          B2=PERMH2(ISPC)
          B3=PERMH3(ISPC)
          B4=PERMH4(ISPC)
          B5=PERMH5(ISPC)
          B6=PERMH6(ISPC)
          B7=PERMH7(ISPC)
          POTHTG=EXP(B1 + B2 + B3*(HT(I))**2 + B4*LOG(HT(I)) +
     &           B5*PBAL +  B6*LOG(CRAT) + B7*ELEVATN)*YR
        ELSE
          IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
          PBAL=PTBALT(I)
          CRAT=ICR(I)
          ELEVATN=ELEV*100
          B1=NOPERMH1(ISPC)
          B2=NOPERMH2(ISPC)
          B3=NOPERMH3(ISPC)
          B4=NOPERMH4(ISPC)
          B5=NOPERMH5(ISPC)
          B6=NOPERMH6(ISPC)
          B7=NOPERMH7(ISPC)
          POTHTG=EXP(B1 + B2*(HT(I))**2 + B3*LOG(HT(I)) + B4*PBAL +
     &           B5*LOG(CRAT) + B6*ELEVATN + B7*LOG(XSITE))*YR
        ENDIF
C----------
C  CALCULATE HIEGHT GROWTH FOR COASTAL SPECIES
C----------
      CASE(1:3,8:12,14,15)
        IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
        PBAL=PTBALT(I)
        CRAT=ICR(I)
        ELEVATN=ELEV*100
        B1=NOPERMH1(ISPC)
        B2=NOPERMH2(ISPC)
        B3=NOPERMH3(ISPC)
        B4=NOPERMH4(ISPC)
        B5=NOPERMH5(ISPC)
        B6=NOPERMH6(ISPC)
        B7=NOPERMH7(ISPC)
        POTHTG=EXP(B1 + B2*(HT(I))**2 + B3*LOG(HT(I)) + B4*PBAL +
     &         B5*LOG(CRAT) + B6*ELEVATN + B7*LOG(XSITE))*YR
C----------
C  SPACE FOR OTHER SPECIES
C---------
      CASE DEFAULT
        POTHTG = 0.
C
      END SELECT
      
C CONSTRAIN POTHTG BASED ON SPECIES AND TREE HEIGHT
      IF(HT(I) .GE. HGCAP(ISPC) .AND. POTHTG .GT. 1.0) THEN
        POTHTG = 1.0
      ELSE
        POTHTG = POTHTG * HGMLT(ISPC)
      ENDIF
C
  900 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9)
     & I, ISPC, DBH(I), HT(I), PBAL, RDZ, CRAT, ELEVATN, XSITE, POTHTG
    9 FORMAT(' IN HTCALC: I=',I5,' ISPC=',I5,' DBH=',F7.4,' HT=',F8.4,
     & ' PBAL=',F10.5,' RDZ=',F7.4,' CRAT=',F7.4,
     & ' ELEVATN=',F7.2,' XSITE=',F7.2,' POTHTG=',F8.4)
C
      RETURN
      END