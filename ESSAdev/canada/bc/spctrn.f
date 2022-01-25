      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C CANADA-BC $Id$
C----------
C     PURPOSE:
C     TAKES TREELIST FILE UPPER CASE SPECIES CODE AS INPUT AND RETURNS
C     AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN THE 1-15
C     RANGE; NON-MATCHES ARE ASSIGNED 14. THIS MESSY FORM IS SIMPLEST
C     GIVEN THE WIDE VARIETY OF INPUT SPECIES CODES.
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      CHARACTER*(*)SPCIN
      CHARACTER*3  SPCOUT
      INTEGER   ISPC1

      SELECT CASE (SPCIN(:2))
        CASE ("PW")
          ISPC1 = 1
          SPCOUT = "PW "
        CASE ("LW","L ")
          ISPC1 = 2
          SPCOUT = "LW "
        CASE ("FD","F ")
          ISPC1 = 3
          SPCOUT = "FD "
        CASE ("BG")
          ISPC1 = 4
          SPCOUT = "BG "
        CASE ("HW","H ","HM")
          ISPC1 = 5
          SPCOUT = "HW "
        CASE ("CW","C ")
          ISPC1 = 6
          SPCOUT = "CW "
        CASE ("PL")
          ISPC1 = 7
          SPCOUT = "PL "
        CASE ("SE","SX","S ","SW","SB","SS")
          ISPC1 = 8
          SPCOUT = "SE "
        CASE ("BL","B ")
          ISPC1 = 9
          SPCOUT = "BL "
        CASE ("PY")
          ISPC1 = 10
          SPCOUT = "PY "
        CASE ("EP","E ")
          ISPC1 = 11
          SPCOUT = "EP "
        CASE ("AT")
          ISPC1 = 12
          SPCOUT = "AT "
        CASE ("AC")
          ISPC1 = 13
          SPCOUT = "AC "
        CASE ("OC","Y ","YC","YP","BA","J ","JR","LA","LT","P ")
          ISPC1 = 14
          SPCOUT = "OC "
        CASE ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
          ISPC1 = 14
          SPCOUT = "OC "
        CASE ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
          ISPC1 = 14
          SPCOUT = "OC "
        CASE ("OT")
          ISPC1 = 14
          SPCOUT = "OC "
        CASE ("OH","D ","DR","U ","UP","A ","AX","R ","RA","EA")
          ISPC1 = 15
          SPCOUT = "OH "
        CASE ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
          ISPC1 = 15
          SPCOUT = "OH "
        CASE ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
          ISPC1 = 15
          SPCOUT = "OH "
        CASE ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
          ISPC1 = 15
          SPCOUT = "OH "
        CASE ("MN","MS","OD","OE","OF","OG","QE")
          ISPC1 = 15
          SPCOUT = "OH "
        CASE DEFAULT
          ISPC1 = 14
          SPCOUT = "OC "
      END SELECT
      RETURN
      END
