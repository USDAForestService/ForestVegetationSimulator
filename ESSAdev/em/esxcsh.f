      SUBROUTINE ESXCSH (HTMAX,HTMIN,TIME,II,DRAW,HHT)
      IMPLICIT NONE
C----------
C EM $Id$
C----------
C     SUBROUTINE TO ASSIGN HEIGHTS TO EXCESS TREES
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
C
      REAL BB(3,MAXSP),CC(3,MAXSP),SHIFT(MAXSP)
      REAL HHT,DRAW,TIME,HTMIN,HTMAX,CLASS,XUPPR,XX
      INTEGER II,ITIME
C----------
C  SPECIES ORDER:
C   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
C  17=PB, 18=OS, 19=OH
C
C  SPECIES EXPANSION
C  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
C  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
C  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
C  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
C  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)
C----------
C
      DATA SHIFT/ 4.0, 4.0, 2.0, 0.0, 2.0, 0.0, 4.0, 2.0, 2.0, 4.0,
     &            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0/
C
C     COEFFICIENTS FOR ARRAYS BB AND CC PREDICT TREE HEIGHT 'CLASS'
C     RATHER THAN ACTUAL HEIGHT; E.G.
C     CLASS  DF,GF,C,H,S,AF  WP,WL,LP,PP
C        1         .6            1.0
C        2         .8            1.2
C        3        1.0            1.4    ETC.
C     PLOT AGE(YRS): 3 THRU 7  8 THRU 12  13 THRU 20
C
      DATA BB/      2.121455,  5.060402,   5.979549,
     2              6.643726, 11.422982,  19.618871,
     3              3.816083,  8.161474,  10.987699,
     4                    0.,        0.,         0.,
     9              2.921356,  4.581383,  10.333282,
     6                    0.,        0.,         0.,
     7              7.360424, 10.928846,  25.214411,
     8              1.466152,  5.159270,   9.272780,
     9              2.921356,  4.581383,  10.333282,
     O              2.779221,  9.033310,  14.131212,
     &                 21*0.,
     1              3.347712,  6.806825,  13.553455,
     3                  3*0./
C
      DATA CC/       .745850,   .782170,    .842171,
     2               .902909,  1.166155,   1.306380,
     3               .996732,   .845413,    .948037,
     4                    0.,        0.,         0.,
     9               .885137,   .871559,   1.043759,
     6                    0.,        0.,         0.,
     7              1.148084,  1.232333,   1.117025,
     8               .722527,   .739031,   1.125510,
     9               .885137,   .871559,   1.043759,
     O               .899325,  1.074932,    .930698,
     &                 21*0.,
     1               .567768,   .894628,   1.214044,
     3                  3*0./
C
      ITIME=1
      IF(TIME.GT.7.5.AND.TIME.LT.12.5) ITIME=2
      IF(TIME.GT.12.5) ITIME=3
C
      SELECT CASE (II)
C
      CASE(1:3,5,7:10,18)
      CLASS=(HTMAX/0.2) -SHIFT(II)
      XUPPR=1.0-EXP(-(((CLASS-HTMIN)/BB(ITIME,II))**CC(ITIME,II)))
      XX=XUPPR*DRAW
      HHT=((-(ALOG(1.00-XX)))**(1.0/CC(ITIME,II)))*BB(ITIME,II)+HTMIN
      HHT=0.2*(HHT+SHIFT(II))
C
      CASE(4,6)
      HHT = 0.5
C
      CASE(11:17,19)
      HHT = 5.0
C
      END SELECT
C
      RETURN
      END
