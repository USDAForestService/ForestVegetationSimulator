      SUBROUTINE ESXCSH (HTMAX,HTMIN,TIME,II,DRAW,HHT)
      IMPLICIT NONE
C----------
C   **ESXCSH--CI  DATE OF LAST REVISION:   06/20/11
C
C   SUBROUTINE TO ASSIGN HEIGHTS TO EXCESS TREES
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
      INTEGER II,ITIME
      REAL BB(3,MAXSP),CC(3,MAXSP),SHIFT(MAXSP)
      REAL HHT,DRAW,TIME,HTMIN,HTMAX,CLASS,XUPPR,XX
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C----------
C  DATA STATEMENTS
C----------
      DATA SHIFT/
     & 4.0, 4.0, 2.0, 2.0, 2.0, 2.0, 4.0, 2.0, 2.0, 4.0,
     &  0.,  0.,  0.,  0.,  0.,  0.,  0., 2.0,  0./
C----------
C     COEFFICIENTS FOR ARRAYS BB AND CC PREDICT TREE HEIGHT 'CLASS'
C     RATHER THAN ACTUAL HEIGHT; E.G.
C     CLASS  DF,GF,C,H,S,AF  WP,WL,LP,PP
C        1         .6            1.0
C        2         .8            1.2
C        3        1.0            1.4    ETC.
C     PLOT AGE(YRS): 3 THRU 7  8 THRU 12  13 THRU 20
C----------
      DATA BB/
     1       2.121455,  5.060402,   5.979549,
     2       6.643726, 11.422982,  19.618871,
     3       3.816083,  8.161474,  10.987699,
     4       3.089571,  5.830185,  10.105748,
     5       3.347712,  6.806825,  13.553455,
     6       3.169513,  4.506403,   8.940539,
     7       7.360424, 10.928846,  25.214411,
     8       1.466152,  5.159270,   9.272780,
     9       2.921356,  4.581383,  10.333282,
     O       2.779221,  9.033310,  14.131212,
     1             0.,        0.,         0.,
     2             0.,        0.,         0.,
     3             0.,        0.,         0.,
     4             0.,        0.,         0.,
     5             0.,        0.,         0.,
     6             0.,        0.,         0.,
     7             0.,        0.,         0.,
     8       3.347712,  6.806825,  13.553455,
     9             0.,        0.,         0./
C
      DATA CC/
     1        .745850,   .782170,    .842171,
     2        .902909,  1.166155,   1.306380,
     3        .996732,   .845413,    .948037,
     4        .800681,   .832278,    .954081,
     5        .567768,   .894628,   1.214044,
     6        .640554,   .813543,    .943493,
     7       1.148084,  1.232333,   1.117025,
     8        .722527,   .739031,   1.125510,
     9        .885137,   .871559,   1.043759,
     O        .899325,  1.074932,    .930698,
     1             0.,        0.,         0.,
     2             0.,        0.,         0.,
     3             0.,        0.,         0.,
     4             0.,        0.,         0.,
     5             0.,        0.,         0.,
     6             0.,        0.,         0.,
     7             0.,        0.,         0.,
     8        .567768,   .894628,   1.214044,
     9             0.,        0.,         0./
C
      ITIME=1
      IF(TIME.GT.7.5.AND.TIME.LT.12.5) ITIME=2
      IF(TIME.GT.12.5) ITIME=3
C
      SELECT CASE (II)
      CASE(11,12,14,15,16)
        HHT = 0.5
C
      CASE(13,17,19)
        HHT = 5.0
C
      CASE DEFAULT
        CLASS=(HTMAX/0.2) -SHIFT(II)
        XUPPR=1.0-EXP(-(((CLASS-HTMIN)/BB(ITIME,II))**CC(ITIME,II)))
        XX=XUPPR*DRAW
        HHT=((-(ALOG(1.00-XX)))**(1.0/CC(ITIME,II)))*BB(ITIME,II)+HTMIN
        HHT=0.2*(HHT+SHIFT(II))
C
      END SELECT
C
      RETURN
      END
