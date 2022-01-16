      SUBROUTINE HTGF
      IMPLICIT NONE
C----------
C CANADA-BC $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT INCREMENT FOR
C  EACH CYCLE AND LOADS IT INTO THE ARRAY HTG. HEIGHT INCREMENT IS
C  PREDICTED FROM SPECIES, HABITAT TYPE, HEIGHT, DBH, AND PREDICTED DBH
C  INCREMENT.  THIS ROUTINE IS CALLED FROM **TREGRO** DURING REGULAR
C  CYCLING.  ENTRY **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE
C  DEPENDENT CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'MULTCM.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'BCPLOT.F77'
C
C
      INCLUDE 'METRIC.F77'
C
COMMONS
C----------
C   MODEL COEFFICIENTS AND CONSTANTS:
C
C    BIAS -- THE AVERAGE RESIDUAL.
C   HTCON -- AN ARRAY CONTAINING HABITAT TYPE CONSTANTS FOR
C            HEIGHT GROWTH MODEL (SUBSCRIPTED BY SPECIES)
C   HDGCOF -- COEFFICIENT FOR DIAMETER GROWTH TERMS.
C    HGLD -- AN ARRAY, SUBSCRIPTED BY SPECIES, OF THE
C             COEFFICIENTS FOR THE DIAMETER TERM IN THE HEIGHT
C             GROWTH MODEL.
C   H2COF -- COEFFICIENT FOR HEIGHT SQUARED TERMS.
C    IND2 -- ARRAY OF POINTERS TO SMALL TREES.
C   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
C            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
C            WHICH MODELS WERE DEVELOPED.
C
C     P_MD  - NUMBER OF MODEL TYPES
C
      INTEGER    P_MD, P_SS
      PARAMETER (P_MD  = 57)
      PARAMETER (P_SS  = 30)

      INTEGER      I,ISPC,I1,I2,I3,ITFN,J,K,M,IHT
      INTEGER      IPOS(MAXSP),IPAS
      REAL         SCALE,XHT,HTI,D,HTNEW,LTSP(MAXSP,3),CON,Y
      REAL         XHT1(MAXSP),XHT2(MAXSP)
      REAL         V3HTG
      CHARACTER*15 PAT

      LOGICAL DEBUG
c     v2
      INTEGER       MAPHAB
      REAL          HGLD,HGHC,HGLDD,HGSC,HGH2,BIAS,HGLH,MISHGF
      DIMENSION     HGLD(MAXSP),HGHC(8),HGLDD(8),HGSC(MAXSP),HGH2(8),
     &              MAPHAB(30)

      DATA HGLD/
     >  -.04935,-.3899,-.4574,-.09775,-.1555,-.1219,
     >  -.2454, -.5720,-.1997,-.5657, -.1219,-.1219,
     >  -.1219,-.4574,-.1219/
      DATA BIAS/ .4809 /, HGLH/ 0.23315 /
C
C     LOCAL HEIGHT-GROWTH DATA STRUCTURE FOR EACH SITE SERIES.
C
      TYPE LTHG_STR
        INTEGER            :: SPP(MAXSP)       ! SPECIES CODE
        CHARACTER (LEN=15) :: PRETTYNAME(P_SS) ! BEC/SITE SERIES
        REAL               :: SI               ! SI - Old model
        REAL               :: B3               ! B3
        REAL               :: C                ! C
C-----------------------------------------------------------------------
        REAL               :: CN               ! CN - New model
        REAL               :: CNSI             ! CNSI
        REAL               :: DG               ! DG
        REAL               :: DG2              ! DG**2
        REAL               :: DBH1             ! DBH Additive
        REAL               :: DBH2             ! DBH Multiplicative
        REAL               :: HT2              ! HT**2
      END TYPE

      TYPE (LTHG_STR) LTHG(P_MD)
C
C     SPECIES-SPECIFIC LARGE TREE COEFFICIENTS:
C     A1, B1, B2
C
      DATA ((LTSP(I,J), J=1,3), I=1,MAXSP) /
     >  4.1060,   0.0,      0.0,      ! PW - 1
     >  2.2341,  -0.0532,  -0.00222,  ! LW - 2
     >  2.6937,  -0.2034,  -0.00387,  ! FD - 3
     >  2.0204,   0.0,      0.0,      ! BG - 4 (RARE: USE BL)
     >  1.4113,   0.4019,  -0.00760,  ! HW - 5 (use CW)
     >  1.4113,   0.4019,  -0.00760,  ! CW - 6
     >  0.0   ,   0.0,      0.0,      ! PL - 7 - new model
     >  0.0   ,   0.0,      0.0,      ! SE - 8 - new model
     >  2.0204,   0.0,      0.0,      ! BL - 9
     >  5.1763,  -0.3013,   0.0,      ! PY - 10
     >  0.0   ,   0.0,      0.0,      ! EP - 11 - new model
     >  0.0   ,   0.0,      0.0,      ! AT - 12 - new model
     >  0.0   ,   0.0,      0.0,      ! CT - 13 - new model
     >  2.6937,  -0.2034,  -0.00387,  ! OC - 14 [FD]
     >  0.0   ,   0.0,      0.0       ! OH - 15 [EP] - new
     >  /
C
C     SITE SERIES COEFFICIENTS: THESE ARE IN SPECIES ORDER.
C     NOTE THAT THERE IS A SPECIAL BEC STRING WITH 'all' AS THE
C     SUBZONE. THIS IS USED (FOR SOME SPECIES) WHEN THERE IS NOT
C     A PERFECT MATCH TO THE SITE SERIES
C
      DATA LTHG(1) /
     >  LTHG_STR(  ! ICH+IDF PW 21m 1-8
     >    (/ 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw3/04      ', 'ICHmw3/05      ',
     >      'ICHdw/01a      ', 'ICHmw1/02      ',
     >      'ICHmw1/03      ', 'ICHmw1/04      ',
     >      'ICHmw1/06      ', 'ICHxw/01       ',
     >      'ICH/all        ', 'IDF/all        ',
     >      'SBPS/all       ', 'SBS/all        ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,      ! SI
     >     0.0429,   ! B3
     >    -0.00142,  ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(2) /
     >  LTHG_STR(  ! ICH+IDF PW 24m 1-28
     >    (/ 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw2/01      ', 'ICHmw2/01-ys   ',
     >      'ICHmw2/02      ', 'ICHmw2/03      ',
     >      'ICHmw2/04      ', 'ICHmw2/05      ',
     >      'ICHmw3/01      ', 'ICHmw3/01-yc   ',
     >      'ICHmw3/06      ', 'ICHmw3/07      ',
     >      'ICHvk1/01      ', 'ICHvk1/02      ',
     >      'ICHvk1/03      ', 'ICHvk1/04      ',
     >      'ICHvk1/05      ', 'ICHwk1/01      ',
     >      'ICHwk1/03      ', 'ICHwk1/04      ',
     >      'ICHwk1/05      ', 'ICHdw/01b      ',
     >      'ICHdw/03       ', 'ICHdw/04       ',
     >      'ICHmw1/01      ', 'ICHmw1/05      ',
     >      'ICHmw2/03      ', 'ICHmw2/04      ',
     >      'ICHmw2/06      ', 'ICHwk1/07      ',
     >      '               ', '               '/),
     >    -2.5072,   ! SI
     >     0.0944,   ! B3
     >    -0.00086,  ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(3) /
     >  LTHG_STR(  ! ICH+IDF LW 15m 1-2
     >    (/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdm1/04      ', 'IDFdm2/03      ',
     >      'ICH/all        ', 'IDF/all        ',
     >      'SBPS/all       ', 'SBS/all        ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,       ! SI
     >     0.0808,    ! B3
     >    -0.00115,   ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(4) /
     >  LTHG_STR(  ! ICH+IDF LW 18m 1-3
     >    (/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFdm1/01      ', 'IDFdm2/01      ',
     >      'IDFdm2/04      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,       ! SI
     >     0.1058,    ! B3
     >    -0.00123,   ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(5) /
     >  LTHG_STR(  ! ICH+IDF LW 21m 1-12
     >    (/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/01      ', 'ICHmk1/01-ys   ',
     >      'ICHmk1/03      ', 'ICHmk1/04      ',
     >      'ICHmw3/04      ', 'IDFdm1/05      ',
     >      'IDFdm1/07      ', 'IDFmw1/04      ',
     >      'ICHdw/01a      ', 'ICHdw/02       ',
     >      'ICHxw/01       ', 'IDFdm2/05      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,       ! SI
     >     0.0802,    ! B3
     >    -0.00026,   ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(6) /
     >  LTHG_STR(  ! ICH+IDF LW 24m 1-19
     >    (/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/05      ', 'ICHmk1/06      ',
     >      'ICHmw2/01      ', 'ICHmw2/01-ys   ',
     >      'ICHmw2/02      ', 'ICHmw2/03      ',
     >      'ICHmw2/04      ', 'ICHmw2/05      ',
     >      'IDFdm1/06      ', 'IDFmw1/01      ',
     >      'IDFmw1/01-yc   ', 'IDFmw1/05      ',
     >      'IDFmw1/06      ', 'ICHdw/01b      ',
     >      'ICHdw/03       ', 'ICHdw/04       ',
     >      'ICHmw2/03      ', 'ICHmw2/04      ',
     >      'ICHmw2/06      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0802,
     >    -0.00026,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(7) /
     >  LTHG_STR(  ! ICH+IDF LW Class 66 (Not in SIBEC table)
     >    (/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw3/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,     ! SI
     >     0.0602,  ! B3
     >    -0.00026, ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(8) /
     >  LTHG_STR(  ! ICH+IDF LW Class 77 (Not in SIBEC table)
     >    (/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFmw2/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0,
     >    -0.00026,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(9) /
     >  LTHG_STR(  ! ICH+IDF FD,OC 12m 1-23
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFdk3/02      ', 'IDFdk3/03      ',
     >      'IDFdk3/04      ', 'IDFdk4/01      ',
     >      'IDFdk4/02      ', 'IDFdk4/03      ',
     >      'IDFdk4/04      ', 'IDFdk4/05      ',
     >      'IDFdk4/07      ', 'IDFxm/02       ',
     >      'IDFxm/03       ', 'IDFxm/04       ',
     >      'IDFxw/02       ', 'IDFxw/03       ',
     >      'IDFxw/04       ', 'IDFdk1/02      ',
     >      'IDFdk2/02      ', 'IDFdm1/03      ',
     >      'IDFww/02       ', 'IDFxh1/02      ',
     >      'IDFxh1/03      ', 'IDFxh2/02      ',
     >      'IDFxh2/03      ', 'ICH/all        ',
     >      'IDF/all        ', 'SBPS/all       ',
     >      'SBS/all        ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0673,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(10) /
     >  LTHG_STR(  ! ICH+IDF FD 15m 1-30 (IDFdk3/01 -> Class 88)
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHdk/02       ', 'ICHmk3/02      ',
     >      'ICHwk2/02      ', 'ICHwk2/03      ',
     >      'ICHwk4/02      ', 'ICHwk4/03      ',
     >      'IDFdk3/05      ', 'IDFdk4/09      ',
     >      'IDFdw/01       ', 'IDFxm/01       ',
     >      'IDFxm/05       ', 'IDFxm/06       ',
     >      'IDFxm/07       ', 'IDFxw/01       ',
     >      'IDFxw/05       ', 'ICHmk1/02      ',
     >      'ICHmk2/02      ', 'ICHmw3/02      ',
     >      'IDFdk1/01      ', 'IDFdk1/03      ',
     >      'IDFdk1/04      ', 'IDFdk2/01      ',
     >      'IDFdk2/03      ', 'IDFdk3/06      ',
     >      'IDFdm1/04      ', 'IDFmw1/02      ',
     >      'IDFmw2/02      ', 'IDFxh1/01      ',
     >      'IDFxh1/04      ', '               '/),
     >     0.0,
     >     0.0799,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /
      DATA LTHG(11) /
     >  LTHG_STR(  ! ICH+IDF FD 15m 31-38 (IDFxh2/04 & /06 -> Class 77)
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFxh1/05      ', 'IDFxh2/01      ',
     >      'IDFxh2/05      ', 'ICHdw/02       ',
     >      'IDFdm2/01      ', 'IDFdm2/03      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0799,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(12) /
     >  LTHG_STR(  ! ICH+IDF FD 18m 1-24
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHmk3/03      ', 'ICHwk4/04      ',
     >      'IDFdk3/06      ', 'IDFdk3/07      ',
     >      'IDFdk3/08      ', 'IDFxm/08       ',
     >      'ICHmk1/03      ', 'ICHmk1/04      ',
     >      'ICHmk2/01      ', 'ICHmk2/03      ',
     >      'ICHmk2/04      ', 'ICHmw3/03      ',
     >      'IDFdk1/05      ', 'IDFdk2/04      ',
     >      'IDFdm1/01      ', 'IDFmw1/03      ',
     >      'IDFmw1/04      ', 'IDFmw2/03      ',
     >      'IDFww/03       ', 'IDFxh1/06      ',
     >      'IDFxh1/07      ', 'ICHmw1/02      ',
     >      'IDFdm2/04      ', 'IDFdm2/07      ',
     >      'SBPSmk/01      ', 'SBSdw2/01      ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.6750,
     >     0.0647,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(13) /
     >  LTHG_STR(  ! ICH+IDF FD 21m 1-30 (ICHmw3/04 -> Class 66)
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHdk/04       ', 'ICHmk3/06      ',
     >      'ICHwk2/04      ', 'ICHwk4/05      ',
     >      'IDFxw/06       ', 'IDFxw/07       ',
     >      'ICHmk1/01      ', 'ICHmk1/01-ys   ',
     >      'ICHmk1/05      ', 'ICHmk2/05      ',
     >      'ICHmw2/02      ', 'ICHmw2/03      ',
     >      'ICHmw3/01      ', 'ICHmw3/01-yc   ',
     >      'ICHmw3/05      ', 'ICHwk1/03      ',
     >      'IDFdk1/06      ', 'IDFdk2/05      ',
     >      'IDFdk2/06      ', 'IDFdm1/05      ',
     >      'IDFdm1/07      ', 'IDFmw1/01      ',
     >      'IDFmw1/01-yc   ', 'IDFmw1/05      ',
     >      'IDFmw2/01      ', 'IDFmw2/04      ',
     >      'IDFxh1/08      ', 'IDFxh2/07      ',
     >      'IDFxh2/08      ', '               '/),
     >     0.0,
     >     0.0853,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(14) /
     >  LTHG_STR(  ! ICH+IDF FD 21m 31-39
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHdw/01a      ', 'ICHmw1/03      ',
     >      'ICHmw1/04      ', 'ICHmw2/03      ',
     >      'ICHmw2/04      ', 'ICHxw/01       ',
     >      'IDFdm2/05      ', 'SBSdw1/01      ',
     >      'SBSmh/01       ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0853,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(15) /
     >  LTHG_STR(  ! ICH+IDF FD 24m 1-30
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHdk/01       ', 'ICHdk/05       ',
     >      'ICHmk3/01      ', 'ICHmk3/04      ',
     >      'ICHwk2/01      ', 'ICHwk2/07      ',
     >      'ICHwk4/01      ', 'ICHwk4/07      ',
     >      'ICHmk1/06      ', 'ICHmw2/01      ',
     >      'ICHmw2/01-ys   ', 'ICHmw2/04      ',
     >      'ICHmw2/05      ', 'ICHmw3/06      ',
     >      'ICHmw3/07      ', 'ICHvk1/01      ',
     >      'ICHvk1/02      ', 'ICHvk1/03      ',
     >      'ICHvk1/04      ', 'ICHwk1/01      ',
     >      'ICHwk1/04      ', 'ICHwk1/05      ',
     >      'IDFdm1/06      ', 'IDFmw1/06      ',
     >      'IDFww/01       ', 'IDFww/04       ',
     >      'IDFww/05       ', 'IDFww/06       ',
     >      'ICHdw/01b      ', 'ICHdw/03       '/),
     >     0.6854,
     >     0.0750,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(16) /
     >  LTHG_STR(  ! ICH+IDF FD 24m 31-35
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHdw/04       ', 'ICHmw1/01      ',
     >      'ICHmw1/05      ', 'ICHmw1/06      ',
     >      'ICHmw2/06      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.6854,
     >     0.0750,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(17) /
     >  LTHG_STR(  ! ICH+IDF FD Class 66
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw3/04      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     1.8869,
     >     0.0519,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(18) /
     >  LTHG_STR(  ! ICH+IDF FD Class 77
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFxh2/04      ', 'IDFxh2/06      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -1.4087,
     >     0.0699,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(19) /
     >  LTHG_STR(  ! ICH+IDF FD Class 88
     >    (/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdk3/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -1.2464,
     >     0.1298,
     >    -0.00020,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(20) /
     >  LTHG_STR(  ! ICH+IDF BG (use BL 15m 1-9)
     >    (/ 4, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICH/all        ', 'IDF/all        ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,     ! SI
     >     0.0545,  ! B3
     >    -0.00071, ! C
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /
      DATA LTHG(21) /
     >  LTHG_STR(  ! ICH+IDF HW 15m 1-18
     >    (/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw2/02      ', 'ICHmw2/06      ',
     >      'ICHmw2/07      ', 'ICHmw3/08      ',
     >      'ICHvk1/05      ', 'ICHvk1/06      ',
     >      'ICHwk1/02      ', 'ICHwk1/03      ',
     >      'ICHwk1/06      ', 'ICHwk1/07      ',
     >      'ICHmw1/02      ', 'ICHmw1/03      ',
     >      'ICHmw1/04      ', 'ICHmw1/07      ',
     >      'ICHmw2/03      ', 'ICHmw2/08      ',
     >      'ICHmw3/04      ', 'ICHwk1/08      ',
     >      'ICH/all        ', 'IDF/all        ',
     >      'SBPS/all       ', 'SBS/all        ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -0.6909,
     >     0.0451,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(22) /
     >  LTHG_STR(  ! ICH+IDF HW 18m 1-28
     >    (/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw2/01      ', 'ICHmw2/01-ys   ',
     >      'ICHmw2/03      ', 'ICHmw2/04      ',
     >      'ICHmw2/05      ', 'ICHmw3/01      ',
     >      'ICHmw3/01-yc   ', 'ICHmw3/05      ',
     >      'ICHmw3/06      ', 'ICHmw3/07      ',
     >      'ICHvk1/01      ', 'ICHvk1/02      ',
     >      'ICHvk1/03      ', 'ICHvk1/04      ',
     >      'ICHwk1/01      ', 'ICHwk1/04      ',
     >      'ICHwk1/05      ', 'ICHdw/01a      ',
     >      'ICHdw/01b      ', 'ICHdw/02       ',
     >      'ICHdw/03       ', 'ICHdw/04       ',
     >      'ICHmw1/01      ', 'ICHmw1/05      ',
     >      'ICHmw1/06      ', 'ICHmw2/05      ',
     >      'ICHmw2/06      ', 'ICHxw/01       ',
     >      '               ', '               '/),
     >    -0.8108,
     >     0.0692,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(23) /
     >  LTHG_STR(  ! ICH+IDF HW Class 66 (not in SIBEC table)
     >    (/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHmw3/03      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -0.6909,  !! use 15m CW
     >     0.0451,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(24) /
     >  LTHG_STR(  ! ICH+IDF CW 12m 1-3
     >    (/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHmk1/04      ', 'ICHmk2/04      ',
     >      'ICHmw1/02      ', 'ICH/all        ',
     >      'IDF/all        ', 'SBPS/all       ',
     >      'SBS/all        ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(25) /
     >  LTHG_STR(  ! ICH+IDF CW 15m 1-30
     >    (/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/01      ', 'ICHmk1/01-ys   ',
     >      'ICHmk1/07      ', 'ICHmk2/01      ',
     >      'ICHmk2/06      ', 'ICHmw2/02      ',
     >      'ICHmw3/01      ', 'ICHmw3/01-yc   ',
     >      'ICHmw3/03      ', 'ICHmw3/04      ',
     >      'ICHmw3/05      ', 'ICHmw3/08      ',
     >      'ICHvk1/05      ', 'ICHvk1/06      ',
     >      'ICHwk1/02      ', 'ICHwk1/03      ',
     >      'ICHwk1/06      ', 'ICHwk1/07      ',
     >      'IDFdk2/07      ', 'IDFmw1/01      ',
     >      'IDFmw1/01-yc   ', 'IDFmw1/04      ',
     >      'IDFmw2/01      ', 'IDFmw2/05      ',
     >      'IDFww/01       ', 'IDFww/04       ',
     >      'IDFww/07       ', 'ICHdw/01a      ',
     >      'ICHmw1/03      ', 'ICHmw1/04      '/),
     >    -0.6909,
     >     0.0451,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(26) /
     >  LTHG_STR(  ! ICH+IDF CW 15m 31-34
     >    (/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw1/07      ', 'ICHmw2/03      ',
     >      'ICHwk1/08      ', 'ICHxw/01       ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -0.6909,
     >     0.0451,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(27) /
     >  LTHG_STR(  ! ICH+IDF CW 18m 1-30
     >    (/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/05      ', 'ICHmk1/06      ',
     >      'ICHmk2/05      ', 'ICHmw2/01      ',
     >      'ICHmw2/01-ys   ', 'ICHmw2/03      ',
     >      'ICHmw2/04      ', 'ICHmw2/05      ',
     >      'ICHmw3/06      ', 'ICHmw3/07      ',
     >      'ICHvk1/01      ', 'ICHvk1/03      ',
     >      'ICHvk1/04      ', 'ICHwk1/01      ',
     >      'ICHwk1/04      ', 'ICHwk1/05      ',
     >      'IDFmw1/05      ', 'IDFmw1/06      ',
     >      'IDFmw2/04      ', 'IDFww/05       ',
     >      'IDFww/06       ', 'ICHdw/01b      ',
     >      'ICHdw/02       ', 'ICHdw/03       ',
     >      'ICHdw/04       ', 'ICHmw1/01      ',
     >      'ICHmw1/05      ', 'ICHmw1/06      ',
     >      'ICHmw2/04      ', 'ICHmw2/06      '/),
     >    -0.8108,
     >     0.0692,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(28) /
     >  LTHG_STR(  ! ICH+IDF CW Class 66  (Not in SIBEC table)
     >    (/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/03      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0,
     >    -0.00022,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(29) /
     >  LTHG_STR(  ! ICH+IDF PL 15m 1-25
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHdk/02       ', 'ICHwk2/02      ',
     >      'ICHwk2/03      ', 'ICHwk4/02      ',
     >      'ICHwk4/03      ', 'IDFdk3/02      ',
     >      'IDFdk3/03      ', 'IDFdk3/05      ',
     >      'IDFdk4/01      ', 'IDFdk4/02      ',
     >      'IDFdk4/05      ', 'IDFdk4/06      ',
     >      'IDFdk4/07      ', 'IDFdk4/08      ',
     >      'IDFdk4/09      ', 'IDFxm/03       ',
     >      'IDFdk1/02      ', 'IDFdk1/03      ',
     >      'IDFdk1/04      ', 'IDFdk3/06      ',
     >      'IDFdm1/03      ', 'IDFdm1/04      ',
     >      'IDFmw1/03      ', 'IDFww/02       ',
     >      'IDFdm2/03      ', 'ICH/all        ',
     >      'IDF/all        ', 'SBPS/all       ',
     >      'SBS/all        ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >    -0.1115,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(30) /
     >  LTHG_STR(  ! ICH+IDF PL 18m 1-25 (IDFdk2/01 & /04 -> Class 88)
                   !                     (IDFdk3/01 & /06 -> Class 99)
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHdk/03       ', 'ICHmk3/02      ',
     >      'IDFdk3/07      ', 'IDFdk3/08      ',
     >      'IDFdk3/09      ', 'IDFdw/01       ',
     >      'ICHmk1/02      ', 'ICHmk2/02      ',
     >      'ICHmw3/02      ', 'ICHwk1/02      ',
     >      'IDFdk1/01      ', 'IDFdk2/03      ',
     >      'IDFdm1/01      ', 'IDFmw1/04      ',
     >      'IDFmw2/02      ', 'IDFmw2/05      ',
     >      'IDFww/03       ', 'ICHmw1/02      ',
     >      'IDFdm2/01      ', 'IDFdm2/04      ',
     >      'IDFdm2/07      ', 'SBPSdc/01      ',
     >      'SBPSmk/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >    -0.1115,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(31) /
     >  LTHG_STR(  ! ICH+IDF PL 21m 1-30 (ICHmk2/03 -> Class 99)
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHdk/04       ', 'ICHmk3/03      ',
     >      'ICHmk3/05      ', 'ICHmk3/06      ',
     >      'ICHmk3/07      ', 'ICHwk2/04      ',
     >      'ICHwk2/05      ', 'ICHwk2/06      ',
     >      'ICHwk2/07      ', 'ICHwk4/04      ',
     >      'ICHwk4/05      ', 'ICHwk4/06      ',
     >      'ICHwk4/07      ', 'ICHmk1/01      ',
     >      'ICHmk1/01-ys   ', 'ICHmk1/03      ',
     >      'ICHmk1/04      ', 'ICHmk1/05      ',
     >      'ICHmk1/07      ', 'ICHmk2/01      ',
     >      'ICHmk2/04      ', 'ICHmk2/06      ',
     >      'ICHmw2/02      ', 'ICHmw3/01      ',
     >      'ICHmw3/01-yc   ', 'ICHmw3/03      ',
     >      'ICHmw3/04      ', 'ICHmw3/05      ',
     >      'ICHwk1/03      ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >     0.8675,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(32) /
     >  LTHG_STR(  ! ICH+IDF PL 21m 31-55
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHwk1/04      ', 'IDFdk1/05      ',
     >      'IDFdk1/06      ', 'IDFdk2/05      ',
     >      'IDFdk2/06      ', 'IDFdk2/07      ',
     >      'IDFdm1/05      ', 'IDFdm1/07      ',
     >      'IDFmw1/01      ', 'IDFmw1/01-yc   ',
     >      'IDFmw1/05      ', 'IDFmw1/06      ',
     >      'IDFmw2/01      ', 'IDFmw2/03      ',
     >      'ICHdw/01a      ', 'ICHdw/02       ',
     >      'ICHmw1/03      ', 'ICHmw1/04      ',
     >      'ICHmw1/07      ', 'ICHmw2/03      ',
     >      'ICHxw/01       ', 'IDFdm2/05      ',
     >      'SBSdw1/01      ', 'SBSmh/01       ',
     >      'SBPSmk/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >     0.8675,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(33) /
     >  LTHG_STR(  ! ICH+IDF PL 24m 1-25
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHdk/01       ', 'ICHdk/05       ',
     >      'ICHmk3/01      ', 'ICHmk3/04      ',
     >      'ICHwk2/01      ', 'ICHwk4/01      ',
     >      'ICHmk1/06      ', 'ICHmk2/05      ',
     >      'ICHmw2/01      ', 'ICHmw2/01-ys   ',
     >      'ICHmw2/03      ', 'ICHmw2/04      ',
     >      'ICHmw2/05      ', 'ICHmw3/06      ',
     >      'ICHwk1/01      ', 'IDFdm1/06      ',
     >      'IDFmw2/04      ', 'IDFww/01       ',
     >      'ICHdw/01b      ', 'ICHdw/03       ',
     >      'ICHdw/04       ', 'ICHmw1/01      ',
     >      'ICHmw1/05      ', 'ICHmw1/06      ',
     >      'ICHmw2/06      ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >     1.1317,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(34) /
     >  LTHG_STR(  ! ICH+IDF PL Class 88
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdk2/01      ', 'IDFdk2/04      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >     0.5698,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(35) /
     >  LTHG_STR(  ! ICH+IDF PL Class 99
     >    (/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk2/03      ', 'IDFdk3/01      ',
     >      'IDFdk3/06      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     1.7767,       !cn
     >    -1.4182,       !cnsi
     >     1.4445,       !dg
     >    -0.0354,       !dg**2
     >    -0.2837,       !dbh additive
     >     0.00172,      !dbh multiplicative
     >    -0.00063       !ht**2
     >    ) /

      DATA LTHG(36) /
     >  LTHG_STR(  ! ICH+IDF SE 15m 1-15
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHwk2/04      ', 'IDFdk3/09      ',
     >      'IDFdk4/08      ', 'IDFdk4/09      ',
     >      'IDFdk4/10      ', 'IDFdw/01       ',
     >      'ICHmw2/02      ', 'ICHmw3/03      ',
     >      'IDFdk1/01      ', 'IDFdk2/01      ',
     >      'IDFdk2/04      ', 'IDFdm1/01      ',
     >      'ICHmw1/02      ', 'ICHmw2/03      ',
     >      'IDFdm2/07      ', 'ICHmw2/03      ',
     >      'ICH/all        ', 'IDF/all        ',
     >      'SBPSdc/01      ', 'SBPS/all       ',
     >      'SBS/all        ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.1475,       !cnsi (use 18m)
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(37) /
     >  LTHG_STR(  ! ICH+IDF SE 18m 1-30 (ICHmw2/03 -> 15m: AAZ)
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHwk4/04      ', 'ICHwk4/05      ',
     >      'ICHwk4/08      ', 'IDFdk3/07      ',
     >      'IDFdk3/08      ', 'IDFxm/08       ',
     >      'IDFxm/09       ', 'IDFxw/06       ',
     >      'ICHmk1/01      ', 'ICHmk1/01-ys   ',
     >      'ICHmk1/03      ', 'ICHmk1/04      ',
     >      'ICHmk1/07      ', 'ICHmk2/01      ',
     >      'ICHmk2/03      ', 'ICHmk2/04      ',
     >      'ICHmk2/06      ', 'ICHmw2/01      ',
     >      'ICHmw2/01-ys   ', 'ICHmw2/07      ',
     >      'ICHmw3/01      ', 'ICHmw3/01-yc   ',
     >      'ICHmw3/04      ', 'ICHmw3/05      ',
     >      'ICHmw3/08      ', 'ICHvk1/06      ',
     >      'ICHwk1/03      ', 'ICHwk1/07      ',
     >      'IDFdk1/05      ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.1475,       !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(38) /
     >  LTHG_STR(  ! ICH+IDF SE 18m 31-47
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFdk2/05      ', 'IDFdk2/07      ',
     >      'IDFdm1/05      ', 'IDFmw1/01      ',
     >      'IDFmw1/01-yc   ', 'IDFmw2/01      ',
     >      'IDFmw2/05      ', 'IDFxh1/08      ',
     >      'IDFxh2/07      ', 'ICHmw1/03      ',
     >      'ICHmw1/04      ', 'ICHmw1/07      ',
     >      'ICHmw2/04      ', 'ICHmw2/08      ',
     >      'ICHwk1/08      ', 'IDFdm2/04      ',
     >      'SBSdw2/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.1475,       !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(39) /
     >  LTHG_STR(  ! ICH+IDF SE 21m 1-30
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHdk/01       ', 'ICHdk/04       ',
     >      'ICHdk/05       ', 'ICHmk3/01      ',
     >      'ICHmk3/05      ', 'ICHmk3/07      ',
     >      'ICHwk2/01      ', 'ICHwk2/05      ',
     >      'ICHwk2/06      ', 'ICHwk2/08      ',
     >      'ICHwk4/01      ', 'ICHwk4/06      ',
     >      'IDFxw/07       ', 'ICHmk1/05      ',
     >      'ICHmk1/06      ', 'ICHmk2/05      ',
     >      'ICHmw2/06      ', 'ICHmw3/06      ',
     >      'ICHvk1/02      ', 'ICHvk1/03      ',
     >      'ICHvk1/05      ', 'ICHwk1/04      ',
     >      'ICHwk1/06      ', 'IDFdk1/06      ',
     >      'IDFdk2/06      ', 'IDFdm1/07      ',
     >      'IDFmw1/05      ', 'IDFmw2/04      ',
     >      'IDFxh2/08      ', 'ICHdw/01b      '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.2268,       !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(40) /
     >  LTHG_STR(  ! ICH+IDF SE 21m 31-39
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw1/01      ', 'ICHmw1/05      ',
     >      'ICHmw1/06      ', 'ICHmw2/07      ',
     >      'ICHwk1/07      ', 'IDFdm2/05      ',
     >      'SBSdw1/01      ', 'SBSmh/01       ',
     >      'SBSmk/01       ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.2268,       !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(41) /
     >  LTHG_STR(  ! ICH+IDF SE 24m 1-16
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk3/04      ', 'ICHmk3/06      ',
     >      'ICHwk2/07      ', 'ICHwk4/07      ',
     >      'ICHmw2/04      ', 'ICHmw2/05      ',
     >      'ICHmw3/07      ', 'ICHvk1/01      ',
     >      'ICHvk1/04      ', 'ICHwk1/01      ',
     >      'ICHwk1/05      ', 'IDFdm1/06      ',
     >      'IDFmw1/06      ', 'ICHdw/03       ',
     >      'ICHdw/04       ', 'ICHmw2/06      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.5203,       !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(42) /
     >  LTHG_STR(  ! ICH+IDF SE Class 66+99 (Not in SIBEC table)
                   !         IDFdk3/01 added manually: AAZ
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdk3/04      ', 'IDFdk3/01      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.0511,       !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(43) /
     >  LTHG_STR(  ! ICH+IDF SE Class 77 & 88 (Not in SIBEC table)
     >    (/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdm2/01      ', 'IDFdm1/04      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.1717,       !cn
     >     0.0,          !cnsi
     >     0.4598,       !dg
     >    -0.0159,       !dg**2
     >     0.3659,       !dbh additive
     >    -0.0060,       !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

      DATA LTHG(44) /
     >  LTHG_STR(  ! ICH+IDF BL 15m 1-9
     >    (/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/03      ', 'ICHmk2/03      ',
     >      'ICHmw2/02      ', 'ICHmw3/03      ',
     >      'ICHmw3/08      ', 'ICHvk1/06      ',
     >      'ICHwk1/02      ', 'ICHmw1/02      ',
     >      'ICHmw2/03      ', 'ICH/all        ',
     >      'IDF/all        ', 'SBPS/all       ',
     >      'SBS/all        ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0545,
     >    -0.00071,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(45) /
     >  LTHG_STR(  ! ICH+IDF BL 18m 1-30
     >    (/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/01      ', 'ICHmk1/01-ys   ',
     >      'ICHmk1/04      ', 'ICHmk1/07      ',
     >      'ICHmk2/01      ', 'ICHmk2/06      ',
     >      'ICHmw2/01      ', 'ICHmw2/01-ys   ',
     >      'ICHmw2/03      ', 'ICHmw2/06      ',
     >      'ICHmw2/07      ', 'ICHmw3/01      ',
     >      'ICHmw3/01-yc   ', 'ICHmw3/05      ',
     >      'ICHvk1/02      ', 'ICHvk1/03      ',
     >      'ICHvk1/05      ', 'ICHwk1/03      ',
     >      'ICHwk1/04      ', 'ICHwk1/06      ',
     >      'ICHwk1/07      ', 'IDFdk1/05      ',
     >      'IDFdk1/06      ', 'IDFdm1/05      ',
     >      'IDFdm1/07      ', 'ICHdw/01a      ',
     >      'ICHmw1/01      ', 'ICHmw1/03      ',
     >      'ICHmw1/04      ', 'ICHmw1/07      '/),
     >     0.0,
     >     0.0617,
     >    -0.00071,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(46) /
     >  LTHG_STR(  ! ICH+IDF BL 18m 31-35
     >    (/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmw2/04      ', 'ICHmw2/08      ',
     >      'ICHwk1/08      ', 'SBSdw2/01      ',
     >      'SBSdw1/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0617,
     >    -0.00071,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(47) /
     >  LTHG_STR(  ! ICH+IDF BL 21m 1-19
     >    (/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICHmk1/05      ', 'ICHmk1/06      ',
     >      'ICHmk2/05      ', 'ICHmw2/04      ',
     >      'ICHmw2/05      ', 'ICHmw3/06      ',
     >      'ICHmw3/07      ', 'ICHvk1/01      ',
     >      'ICHvk1/04      ', 'ICHwk1/01      ',
     >      'ICHwk1/05      ', 'IDFdm1/06      ',
     >      'ICHdw/03       ', 'ICHdw/04       ',
     >      'ICHmw1/05      ', 'ICHmw1/06      ',
     >      'ICHmw2/05      ', 'ICHmw2/06      ',
     >      'SBSmh/01       ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -0.6005,
     >     0.0857,
     >    -0.00071,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(48) /
     >  LTHG_STR(  ! ICH+IDF BL 24m 1-1
     >    (/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'ICHdw/01b      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -1.0159,
     >     0.1335,
     >    -0.00071,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(49) /
     >  LTHG_STR(  ! ICH+IDF PY 12m 1-11
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFxw/01       ', 'IDFxw/02       ',
     >      'IDFxw/03       ', 'IDFxw/04       ',
     >      'IDFdk1/02      ', 'IDFdk1/03      ',
     >      'IDFdk2/02      ', 'IDFdk2/03      ',
     >      'IDFxh1/02      ', 'IDFxh2/02      ',
     >      'IDFxh2/03      ', 'ICH/all        ',
     >      'IDF/all        ', 'SBPS/all       ',
     >      'SBS/all        ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(50) /
     >  LTHG_STR(  ! ICH+IDF PY 15m 1-15
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdk1/04      ', 'IDFdk2/04      ',
     >      'IDFdm1/03      ', 'IDFmw1/02      ',
     >      'IDFmw2/02      ', 'IDFmw2/03      ',
     >      'IDFxh1/03      ', 'IDFxh1/04      ',
     >      'IDFxh1/05      ', 'IDFxh2/01      ',
     >      'IDFxh2/04      ', 'IDFxh2/05      ',
     >      'IDFxh2/06      ', 'ICHmk1/02      ',
     >      'IDFdm2/03      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -1.6556,
     >     0.0616,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(51) /
     >  LTHG_STR(  ! ICH+IDF PY 18m 1-12
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFdm1/01      ', 'IDFdm1/04      ',
     >      'IDFmw1/03      ', 'IDFmw1/04      ',
     >      'IDFmw2/01      ', 'IDFww/03       ',
     >      'IDFxh1/01      ', 'IDFxh1/06      ',
     >      'IDFxh1/07      ', 'ICHmk1/03      ',
     >      'IDFdm2/01      ', 'IDFdm2/04      ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -1.7829,
     >     0.0748,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /
C
C     SIBEC 21 absent from AAZs table
C
      DATA LTHG(52) /
     >  LTHG_STR(  ! ICH+IDF PY 21m 1-8 -> all use 18m coefficients
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdm1/05      ', 'IDFmw1/01      ',
     >      'IDFmw1/01-yc   ', 'IDFxh1/08      ',
     >      'IDFxh2/07      ', 'ICHdw/01a      ',
     >      'ICHdw/02       ', 'ICHxw/01       ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >    -1.7829,
     >     0.0748,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(53) /
     >  LTHG_STR(  ! ICH+IDF PY 24m 1-3
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFww/01       ', 'IDFww/04       ',
     >      'ICHdw/01b      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0550,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(54) /
     >  LTHG_STR(  ! ICH+IDF PY Class 66 (Not in SIBEC table)
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),
     >    (/'IDFdk1/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0382,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(55) /
     >  LTHG_STR(  ! ICH+IDF PY Class 77 (Not in SIBEC table)
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdk1/05      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0335,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(56) /
     >  LTHG_STR(  ! ICH+IDF PY Class 88 (Not in SIBEC table)
     >    (/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'IDFdk2/01      ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0,
     >     0.0,
     >    -0.00030,
     >     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
     >    ) /

      DATA LTHG(57) /
     >  LTHG_STR ( ! ICH+IDF EP,AT,AC,OH all sites - AT model
     >    (/11,12,13,15, 0,0,0,0,0,0,0,0,0,0,0 /),
     >    (/'ICH/all        ', 'IDF/all        ',
     >      'SBPSmk/01      ', 'SBSdw2/01      ',
     >      'SBPS/all       ', 'SBS/all        ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               ',
     >      '               ', '               '/),
     >     0.0, 0.0, 0.0,
     >     0.0,          !cn
     >     0.0,          !cnsi
     >     2.8176,       !dg
     >    -0.2574,       !dg**2
     >    -0.2224,       !dbh additive
     >    -0.00098,      !dbh multiplicative
     >    -0.00085       !ht**2
     >    ) /

C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HTGF',4,ICYC)
      SCALE=FINT/YR
      ISMALL=0
C----------
C  GET THE HEIGHT GROWTH MULTIPLIERS.
C----------

      CALL MULTS (2,IY(ICYC),XHMULT)

C----------
C   BEGIN SPECIES LOOP:
C----------
      DO 40 ISPC=1,MAXSP
      I1 = ISCT(ISPC,1)
      IF (I1 .EQ. 0) GO TO 40
      I2 = ISCT(ISPC,2)
      XHT=1.0
      XHT=XHMULT(ISPC)
C-----------
C   BEGIN TREE LOOP WITHIN SPECIES LOOP
C-----------
      DO 30 I3=I1,I2
        I   = IND1(I3)
        HTI = HT(I)
        D   = DBH(I)

        HTG(I)=0.
        IF (PROB(I).LE.0.0)THEN
          IF(LTRIP)THEN
            ITFN=ITRN+2*I-1
            HTG(ITFN)=0.
            HTG(ITFN+1)=0.
          ENDIF
          GO TO 30
        ENDIF

C       HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
C       MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
C       MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        ITFN=ITRN+2*I-1
        Y = SCALE*XHT
C----------
C    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
C    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.
C----------
        Y = Y * MISHGF(I,ISPC)
        IF (LV2ATV) THEN

          CON=HTCON(ISPC)+H2COF*HTI*HTI+HGLD(ISPC)*ALOG(D)+
     &      HGLH*ALOG(HTI)
          HTG(I)=(EXP(CON+HDGCOF*ALOG(DG(I)))+BIAS) * Y
          IF (LTRIP) THEN
            HTG(ITFN)  =(EXP(CON+HDGCOF*ALOG(DG(ITFN)))  +BIAS) * Y
            HTG(ITFN+1)=(EXP(CON+HDGCOF*ALOG(DG(ITFN+1)))+BIAS) * Y
          ENDIF

        ELSE

C         NOTE: IF TRIPLING IS SCHEDULED, THE VALUE OF HT(ITFN & ITFN+1) IS
C         NOT YET DEFINED (CALL TO TRIPLE IS LATER); IN THIS CASE JUST
C         USE HTI=HT(I); D=DBH(ITFN) AND DG(ITFN) ARE BOTH DEFINED AT THIS
C         POINT.

          HTG(I) = V3HTG(ISPC,HTI,D,DG(I)) * Y
          IF (LTRIP) THEN
            HTG(ITFN)   = V3HTG(ISPC,HTI,D,DG(ITFN)) * Y
            HTG(ITFN+1) = V3HTG(ISPC,HTI,D,DG(ITFN+1)) * Y
          ENDIF

        ENDIF

C       END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        IF (DEBUG) THEN
          HTNEW=HT(I)+HTG(I)
          WRITE (JOSTND,9000) HTG(I),CON,HTCON(ISPC),H2COF,D,
     &    WK1(I),HGLH,HTNEW,HDGCOF,I,ISPC
 9000     FORMAT(' 9000 HTGF, HTG=',F8.4,' CON=',F8.4,' HTCON=',F8.4,
     &    ' H2COF=',F12.8,' D =',F8.4/' WK1=',F8.4,' HGLH=',F8.4,
     &    ' HTNEW=',F8.4,' HDGCOF=',F8.4,' I=',I4,' ISPC=',I2)
        ENDIF

C       CHECK FOR SIZE CAP COMPLIANCE.

        IF((HT(I)+HTG(I)).GT.SIZCAP(ISPC,4))THEN
          HTG(I)=SIZCAP(ISPC,4)-HT(I)
          IF(HTG(I) .LT. 0.1) HTG(I)=0.1
        ENDIF
        IF (LTRIP) THEN
          HTG(ITFN)   = HTG(I)
          HTG(ITFN+1) = HTG(I)
        ENDIF
C----------
       IF(DEBUG.AND.LTRIP) WRITE(JOSTND,9001) HTG(ITFN),HTG(ITFN+1)
9001   FORMAT( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

C       END OF TREE LOOP

   30 CONTINUE

C     END OF SPECIES LOOP

   40 CONTINUE
      RETURN

C  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT ARE
C  SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC CONTAINS
C  HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT DEPENDENT
C  COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2 CONTAINS HABITAT
C  DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED TERM, AND HGHC CONTAINS
C  SPECIES DEPENDENT INTERCEPTS.  HABITAT TYPE IS INDEXED BY ITYPE (SEE
C  /PLOT/ COMMON AREA).

C  HEIGHT DUBBING (HT1, HT2) FOR V3 HAPPENS HERE; IN V2 IT HAPPENS IN BECSET
C  (TO ALLOW COMPATIBILITY WITH PRINTING OF COEFFICIENTS IN MAIN OUTPUT)

      ENTRY HTCONS

C     V2
      DATA MAPHAB/
     & 1,1, 7*2, 3,3,4,5,6, 4*7, 4,4,1,4,4, 3*8, 4*1/
      DATA  HGHC /
     & 2.03035, 1.72222, 1.19728, 1.81759, 2.14781, 1.76998, 2.21104,
     & 1.74090/
      DATA  HGLDD /
     & 0.62144, 1.02372, 0.85493, 0.75756, 0.46238, 0.49643, 0.37042,
     & 0.34003/
      DATA  HGH2 /
     & -13.358E-05, -3.809E-05, -3.715E-05, -2.607E-05, -5.200E-05,
     & -1.605E-05, -3.631E-05, -4.460E-05/
      DATA  HGSC /
     &  -.5342,.1433,.1641,-.6458,-.6959,-.9941,-.6004,.2089,
     &  -.5478,.7316,-.9941,-.9941,-.9941,.1641,-.9941 /

C     V2 - ASSIGN HABITAT DEPENDENT COEFFICIENTS.

      IF (LV2ATV) THEN

        IHT=MAPHAB(ITYPE)
        HGHCH=HGHC(IHT)
        H2COF=HGH2(IHT)
        HDGCOF=HGLDD(IHT)
        SEISHT = 1.0 ! V2 SMALL TREE MODEL

      ELSE

C       REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
C       STRING LENGTHS WILL BE FIGURED CORRECTLY
        DO J = 1,P_MD
          DO K = 1,P_SS
            DO M = 1,15
              IF (ICHAR(LTHG(J)%PrettyName(K)(M:M)) .EQ. 0)
     >          LTHG(J)%PrettyName(K)(M:M) = ' '
            ENDDO
          ENDDO
        ENDDO

C     LOCATE ZONE AND SITE SERIES; IF NO PERFECT MATCH TO SUBZONE
C     CAN BE FOUND, TRY ".../all" AND LOOP ONE MORE TIME BEFORE
C     GIVING UP ON A MATCH.

        LTH%PrettyName = BEC%PrettyName
        DO I = 1,MAXSP
          IPOS(I) = 0
        ENDDO

        DO I = 1,MAXSP

          IPAS = 0
          PAT  = BEC%PrettyName

    7     DO J = 1,P_MD
            DO K = 1,MAXSP
              IF (LTHG(J)%SPP(K) .EQ. 0) EXIT
              IF (LTHG(J)%SPP(K) .EQ. I) THEN
                DO M = 1,P_SS
                   IF (LEN_TRIM(LTHG(J)%PrettyName(M))  .EQ. 0) EXIT
                  IF (INDEX(LTHG(J)%PrettyName(M),PAT) .GT. 0) THEN
                    IPOS(I) = J
                    GOTO 8
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO

          IF (J .GT. P_MD .AND. IPAS .EQ. 0) THEN
            IPAS = 1
            PAT = BEC%Zone(1:LEN_TRIM(BEC%Zone))//'/all'
            GOTO 7
          ENDIF

    8   ENDDO

C     COPY COEFFICIENTS INTO LTHG STRUCTURE

        DO I = 1,MAXSP

          LTH%FIT(I) = .FALSE.
          J           = IPOS(I)
          IF (J .GT. 0) THEN
            LTH%FIT(I)  = .TRUE.

            LTH%A1(I)   = LTSP(I,1)     ! old v3 model spp constants
            LTH%B1(I)   = LTSP(I,2)
            LTH%B2(I)   = LTSP(I,3)

            LTH%SI(I)   = LTHG(J)%SI    ! old v3 model site terms
            LTH%B3(I)   = LTHG(J)%B3
            LTH%C(I)    = LTHG(J)%C

            LTH%CN(I)   = LTHG(J)%CN    ! new v3 model
            LTH%CNSI(I) = LTHG(J)%CNSI
            LTH%DG(I)   = LTHG(J)%DG
            LTH%DG2(I)  = LTHG(J)%DG2
            LTH%DBH1(I) = LTHG(J)%DBH1
            LTH%DBH2(I) = LTHG(J)%DBH2
            LTH%HT2(I)  = LTHG(J)%HT2
          ENDIF
        ENDDO
      ENDIF

C  LOAD OVERALL INTERCEPT MULTIPLIER FOR EACH SPECIES.

      IF (LV2ATV) THEN

        DO ISPC = 1,MAXSP
          HTCON(ISPC) = HGHCH + HGSC(ISPC)
          IF(LHCOR2 .AND. HCOR2(ISPC).GT.0.0)
     >      HTCON(ISPC) = HTCON(ISPC) + ALOG(HCOR2(ISPC))
        ENDDO

      ELSE

        DO ISPC = 1,MAXSP
          LTH%MLT(ISPC) = 1.0
          IF (LTH%FIT(ISPC)) THEN
            IF (LHCOR2 .AND. HCOR2(ISPC).GT. 0.0)
     >        LTH%MLT(ISPC) = HCOR2(ISPC)
          ENDIF
        ENDDO

      ENDIF

C     STEP 2:
C     ENTRY POINT FOR REFIT OF HEIGHT DUBBING MODEL FOR VERSION 3
C     THIS ENTRY POINT IS FOR ICH/IDF ONLY; OTHERS ARE DONE IN BECSET
C
C     FIND AND REPLACE HEIGHT COEFFICIENTS HELD IN
C     ARRAYS HT1() AND HT2() (C_0 AND C_1 OF HT/DIAM EQN)
C
C     IF NOT YET DONE, PUT COMPILE-TIME HT1,HT2 INTO ARCHIVE VARIABLES
C     THIS USES A V2 FLAG BECAUSE HEIGHT DUBBING IS A HOLD-OVER FROM V2
C     THAT IS BASICALLY UNALTERED IN V3

      IF (LV2HTCP) THEN
        LV2HTCP = .FALSE.
        DO I = 1,MAXSP
          XHT1B(I) = HT1(I)
          XHT2B(I) = HT2(I)
        ENDDO
      ENDIF

C     COPY TEMPORARY VALUES FROM ARCHIVE

      DO I = 1,MAXSP
        XHT1(I) = XHT1B(I)
        XHT2(I) = XHT2B(I)
      ENDDO
C
C     SEARCH FOR MATCHES AND REPLACE IF FOUND
C
      DO I = 1,MAXSP

        IF (I.EQ.1) THEN   ! western white pine - PW
          IF (INDEX(BEC%Zone,'ICH') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'mw') .GT. 0) THEN
              XHT1(I) =   3.9738
              XHT2(I) = -20.8459
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   3.7771
              XHT2(I) = -15.8162
            ENDIF
          ENDIF

        ELSEIF (I.EQ.2) THEN   ! western larch - LW
          IF (INDEX(BEC%Zone,'ICH') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dw') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mk') .GT. 0) THEN
              XHT1(I) =   3.9045
              XHT2(I) = -22.0149
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0) THEN
              XHT1(I) =   3.7825
              XHT2(I) = -14.5352
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'IDF') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dm') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'xh') .GT. 0) THEN
              XHT1(I) =   3.9012
              XHT2(I) = -19.1924
            ENDIF
          ENDIF

        ELSEIF (I.EQ.3 .OR. I.EQ.14) THEN  ! Douglas-fir - FD (&OC)
          IF (INDEX(BEC%Zone,'ICH') .GT. 0 .OR.
     >        INDEX(BEC%PrettyName,'SBSdw1') .GT. 0 .OR.
     >        INDEX(BEC%PrettyName,'SBSmh') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mk') .GT. 0 .OR.
     >          INDEX(BEC%PrettyName,'SBSdw1') .GT. 0 .OR.
     >          INDEX(BEC%PrettyName,'SBSmh') .GT. 0) THEN
              XHT1(I) =   3.9432
              XHT2(I) = -25.9091
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0) THEN
              XHT1(I) =   3.9274
              XHT2(I) = -20.1322
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   4.1218
              XHT2(I) = -29.6135
            ELSEIF (INDEX(BEC%SubZone,'dw') .GT. 0) THEN
              XHT1(I) =   3.7859
              XHT2(I) = -22.0166
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'IDF') .GT. 0 .OR.
     >            INDEX(BEC%Zone,'SBPS') .GT. 0 .OR.
     >            INDEX(BEC%PrettyName,'SBSdw2') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mw') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'xm') .GT. 0 .OR.
     >          INDEX(BEC%Zone,'SBPS') .GT. 0 .OR.
     >          INDEX(BEC%PrettyName,'SBSdw2') .GT. 0) THEN
              XHT1(I) =   3.7049
              XHT2(I) = -22.9781
            ELSEIF (INDEX(BEC%SubZone,'dm') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'ww') .GT. 0) THEN
              XHT1(I) =   3.7877
              XHT2(I) = -23.1796
            ELSEIF (INDEX(BEC%SubZone,'xh') .GT. 0) THEN
              XHT1(I) =   3.6337
              XHT2(I) = -18.2998
            ENDIF
          ENDIF

        ELSEIF (I.EQ.5) THEN   ! western hemlock - HW
          IF (INDEX(BEC%Zone,'ICH') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'vc') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'vk') .GT. 0) THEN
              XHT1(I) =   3.9685
              XHT2(I) = -30.4439
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   3.9491
              XHT2(I) = -26.1417
            ELSEIF (INDEX(BEC%SubZone,'mc') .GT. 0) THEN
              XHT1(I) =   3.8170
              XHT2(I) = -21.3197
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'IDF') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'mm') .GT. 0) THEN
              XHT1(I) =   3.2884
              XHT2(I) = -12.9475
            ELSEIF (INDEX(BEC%SubZone,'wv') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'ww') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'wc') .GT. 0) THEN
              XHT1(I) =   4.0577
              XHT2(I) = -28.1854
            ENDIF
          ENDIF

        ELSEIF (I.EQ.6) THEN   ! western redcedar - CW
          IF (INDEX(BEC%Zone,'ICH') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'mw') .GT. 0 ) THEN
              XHT1(I) =   3.8565
              XHT2(I) = -25.7777
            ELSEIF (INDEX(BEC%SubZone,'mm') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'vk') .GT. 0) THEN
              XHT1(I) =   4.1251
              XHT2(I) = -41.8833
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mc') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mk') .GT. 0) THEN
              XHT1(I) =   3.9432
              XHT2(I) = -30.5647
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'IDF') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dm') .GT. 0 ) THEN
              XHT1(I) =   3.2692
              XHT2(I) = -10.1899
            ELSEIF (INDEX(BEC%SubZone,'ww') .GT. 0) THEN
              XHT1(I) =   4.0537
              XHT2(I) = -36.7598
            ENDIF
          ENDIF

        ELSEIF (I.EQ.7) THEN   ! lodgepole pine - PL
          IF (INDEX(BEC%Zone,'ICH') .GT. 0 .OR.
     >        INDEX(BEC%PrettyName,'SBSdw1') .GT. 0 .OR.
     >        INDEX(BEC%PrettyName,'SBSmh') .GT. 0) THEN
           IF (INDEX(BEC%SubZone,'mk') .GT. 0 .OR.
     >         INDEX(BEC%SubZone,'mw') .GT. 0 .OR.
     >         INDEX(BEC%SubZone,'mc') .GT. 0 .OR.
     >         INDEX(BEC%PrettyName,'SBSdw1') .GT. 0 .OR.
     >         INDEX(BEC%PrettyName,'SBSmh') .GT. 0) THEN
              XHT1(I) =   3.8470
              XHT2(I) = -18.5836
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'IDF') .GT. 0 .OR.
     >            INDEX(BEC%Zone,'SBPS') .GT. 0 .OR.
     >            INDEX(BEC%PrettyName,'SBSdw2') .GT. 0) THEN
           IF (INDEX(BEC%SubZone,'xh') .GT. 0 .OR.
     >         INDEX(BEC%SubZone,'dm') .GT. 0) THEN
              XHT1(I) =   3.5683
              XHT2(I) = -13.0736
            ELSEIF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >              INDEX(BEC%Zone,'SBPS') .GT. 0 .OR.
     >              INDEX(BEC%PrettyName,'SBSdw2') .GT. 0) THEN
              XHT1(I) =   3.1192
              XHT2(I) =  -8.2208
            ENDIF
          ENDIF

        ELSEIF (I.EQ.8) THEN   ! Engelmann spruce - SE
          IF (INDEX(BEC%Zone,'ICH') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'IDF') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'SBPS') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'SBS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%Zone,'SBPS') .GT. 0 .OR.
     >          INDEX(BEC%PrettyName,'SBSdw2') .GT. 0) THEN
              XHT1(I) =   3.6519
              XHT2(I) = -17.8872
            ELSEIF (INDEX(BEC%SubZone,'dm') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mk') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mc') .GT. 0 .OR.
     >              INDEX(BEC%Zone,'SBS') .GT. 0) THEN
              XHT1(I) =   4.0115
              XHT2(I) = -25.0730
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0) THEN
              XHT1(I) =   3.9729
              XHT2(I) = -22.6984
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   4.1277
              XHT2(I) = -29.2780
            ENDIF
          ENDIF

        ELSEIF (I.EQ.9 .OR. I.EQ.4) THEN   ! subalpine fir - BL, BG
          IF (INDEX(BEC%Zone,'ICH') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'IDF') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'SBS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'mk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'vk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >         INDEX(BEC%Zone,'SBS') .GT. 0) THEN
              XHT1(I) =   3.7656
              XHT2(I) = -20.1044
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'wk') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mc') .GT. 0) THEN
              XHT1(I) =   3.9598
              XHT2(I) = -24.0969
            ENDIF
          ENDIF

        ELSEIF (I.EQ.10) THEN   ! ponderosa pine - PY
          IF (INDEX(BEC%Zone,'ICH') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dw') .GT. 0) THEN
              XHT1(I) =   4.0178
              XHT2(I) = -27.1633
            ELSEIF (INDEX(BEC%SubZone,'mk') .GT. 0) THEN
              XHT1(I) =   3.7798
              XHT2(I) = -29.9875
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'IDF') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'xh') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'dm') .GT. 0) THEN
              XHT1(I) =   3.7948
              XHT2(I) = -26.8113
            ELSEIF (INDEX(BEC%SubZone,'dk') .GT. 0) THEN
              XHT1(I) =   3.8113
              XHT2(I) = -29.7283
            ENDIF
          ENDIF
        ENDIF
      ENDDO

C     COPY TEMPORARY TO PERMANENT ARRAYS

      DO I = 1,MAXSP
        HT1(I) = XHT1(I)
        HT2(I) = XHT2(I)
      ENDDO

C     SET FLAG FOR WHETHER METRIC DUB IS USED, OR NOT

      DO I = 1,MAXSP
        IF ((HT1(I) .NE. XHT1B(I)) .OR.
     >      (HT2(I) .NE. XHT2B(I))) THEN
          LMHTDUB(I) = .TRUE.
        ELSE
          LMHTDUB(I) = .FALSE.
        ENDIF
      ENDDO

      RETURN
      END

C     V3HTG--SEI
C     THIS PRIVATE FUNCTION LOCALIZES THE V3 CALCULATIONS FOR HEIGHT GROWTH;
C     OTHERWISE THEY WOULD BE REPEATED SEVERAL TIMES

      REAL FUNCTION V3HTG(ISPC,HT,DBH,DG)
      IMPLICIT NONE

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BCPLOT.F77'
      INCLUDE 'METRIC.F77'

      INTEGER ISPC
      REAL    HT,DBH,DG
      REAL    HTGM,HTM,D,DGM

      V3HTG = 0.0
      IF (.NOT. LTH%FIT(ISPC)) RETURN

      HTM = HT  * FTtoM
      D   = DBH * INtoCM
      DGM = DG  * INtoCM

      SELECT CASE (ISPC)
        CASE (7,8,11,12,13,15)  ! NEW v3 MODEL
          HTGM = (LTH%CN(ISPC) + LTH%CNSI(ISPC) +
     >      (LTH%DG(ISPC) * DGM + LTH%DG2(ISPC) * DGM * DGM)) *
     >      (HTM ** (LTH%DBH1(ISPC) + LTH%DBH2(ISPC) * D)) *
     >      EXP(LTH%HT2(ISPC) * HTM * HTM)
        CASE DEFAULT          ! OLD V3 MODEL
          HTGM = (LTH%A1(ISPC) + LTH%SI(ISPC)) *
     >      HTM ** (LTH%B1(ISPC) +
     >      (LTH%B2(ISPC) * D) + (LTH%B3(ISPC) * DGM)) *
     >      EXP(LTH%C(ISPC) * HTM * HTM)
        END SELECT
        V3HTG = HTGM * MtoFT * LTH%MLT(ISPC)

      RETURN
      END
