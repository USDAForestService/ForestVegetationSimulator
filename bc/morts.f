      SUBROUTINE MORTS
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
C  RECORDS AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
C  TREE RECORD.  MORTALITY RATE IS PREDICTED FROM DBH, DBH INCREMENT,
C  RELATIVE DIAMETER, AND STAND BASAL AREA. AN ADJUSTMENT IS MADE TO
C  INCREASE MORTALITY RATE AS THE STAND BASAL AREA APPROACHES THE
C  THEORETICAL MAXIMUM THAT THE HABITAT TYPE CAN SUPPORT.  THIS ROUTINE
C  IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH PREDICTION.  ENTRY
C  **MORCON** IS ACCESSED TO LOAD SITE DEPENDENT CONSTANTS.
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CALCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'ESTREE.F77'
      INCLUDE 'MULTCM.F77'
      INCLUDE 'PDEN.F77'
      INCLUDE 'WORKCM.F77'
      INCLUDE 'BCPLOT.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C----------
C  DEFINITIONS:
C
C        I -- TREE SUBSCRIPT.
C        D -- TREE DIAMETER.
C      RIP -- ESTIMATED ANNUAL MORTALITY RATE BASED ON HAMILTON'S
C             MODEL.
C       RZ -- RATE SUCH THAT BASAL AREA IN 10 YEARS IS EQUAL TO BA
C             PLUS (BAMAX-BA)/BAMAX PROPORTION OF THE PREDICTED BA
C             INCREMENT.
C     RIPP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON BA, BAMAX,
C             RIP, AND RZ.  THIS IS THE RATE THAT IS USED TO PREDICT
C             TREE MORTALITY.  IT IS AN ANNUAL RATE THAT WILL BE
C             COMPOUNDED TO OBTAIN A FINT-YEAR RATE.
C        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
C             OF TREE MORTALITY (TREES/ACRE).
C     FMRT -- 1.2: BEC: 1= ICH, 2= IDF
C             1,6: SPP: 1= PW, 2=LW, 3= FD, 4= PL, 5= SE, 6=PY
C             1,5: BA:  1= <20, 2=20-30, 3=30-40, 4=40-50, 5=50+ m**2/ha
C             1,4: DBH: 1= <10, 2=10-25, 3=25-40, 4=40+ cm DBH
C             THESE ARE MAPPED TO TMRT (SIMILAR DIMS) IN MORCON
C----------
C
C     P_MD  - NUMBER OF SETS OF SITE SERIES COEFFICIENTS
C     P_SS  - MAXIMUM NUMBER OF SITE SERIES PER COEFFICIENT SET
C
      INTEGER   P_MD,P_SS
      PARAMETER (P_MD  =  1)
      PARAMETER (P_SS  = 30)

      LOGICAL DEBUG,LINCL

      CHARACTER*15 PAT
      INTEGER MYACTS,NTODO,I,IDATE,IACTK,NP,ISPCC,IS,ISPC
      INTEGER I1,I2,I3,IP,ITODO,KPOINT,J,K,M
      INTEGER IPOS(MAXSP),IPAS,KBIG,IG,IX
      INTEGER MRTCLS,DBHCLS,BACLS,BECCLS,IDMFLG
      INTEGER IGRP,IULIM
      REAL    PRM,T,SD2SQ,P,D,G,CIOBDS,DQ10,DELTBA,BA10,TB
      REAL    TTB,RZ,BARK,XMORT,DX,BAX
      REAL    D1,D2,WKI,RIP,RIPP
      REAL    X,XCHECK,PRES,VLOS,XMORE,TEMP,CREDIT
      REAL    BAL,SPH
      REAL    BRATIO
      REAL    FMRT(MRT_IDF,MRT_PY,MRT_BA_UB,MRT_DBH_UB)
      REAL    DBHBRK(MRT_DBH_UB-1), BABRK(MRT_BA_UB-1)

      DIMENSION PRM(6),MYACTS(2)

C     V2
      REAL      PMSC(MAXSP),POT(54),REIN_C(2,MAXSP),GMULT_C(2,MAXSP)
      REAL      DSUM,WPROB,AVED,B5,RELDBH,DGT,POTENT
      INTEGER   IPDG(30,MAXSP),IPDG2(30,MAXSP),IPT,IPT2


C     LOCAL MORTALITY DATA STRUCTURE FOR EACH SITE SERIES.

      TYPE LMRT_STR
        INTEGER            :: SPP(MAXSP)       ! SPECIES CODE
        CHARACTER (LEN=15) :: PrettyName(P_SS) ! BEC/SITE SERIES
        REAL               :: COEF(8)          ! COEFFICIENTS
      END TYPE

C  DATA STATEMENTS.

      DATA MYACTS/94,97/

C     V2
      DATA PMSC/
     &  0.0,-.17603,2*0.317888,0.607725,1.57976,-.12057,.94019,
     &  2*.21180,0.0,0.0,0.0,0.317888,0.0/

      DATA ((IPDG(I,J),I=1,30),J=1,2)/
     &  7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,
     &  4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,
     &     12,12,15,12,7,10,15/
      DATA ((IPDG(I,J),I=1,30),J=3,5)/
     &  4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,
     &     15,11,5,10,15,
     &  4*12, 11,11, 3*10, 11,11,12,12, 3*14, 3*13, 9,6,8,7,9,10,12,10,
     &     4,8,12,
     &  4*10, 9,9,8,9,8,9, 3*10, 12, 5*11, 8,6,7,5,7,7,10,8,6,7,10/
      DATA ((IPDG(I,J),I=1,30),J=6,7)/
     &  6*9, 7, 3*8, 3*9, 11, 4*10, 11,7,5,6,4,7,7,9,8,3,6,9,
     &  13, 5*12, 10,12, 3*11, 12,12,15,13,13,14,13,13,9,8,9,7,10,10,
     &     12,10,4,8,12/
      DATA ((IPDG(I,J),I=1,30),J=8,9)/
     &  4*9, 8,9,7,8,7, 3*8, 9,11, 5*10, 7,5,6,5,7,7,9,8,3,6,9,
     &  4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7/
      DATA ((IPDG(I,J),I=1,30),J=10,10)/
     &  4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,
     &     6,8,15/
      DATA ((IPDG(I,J),I=1,30),J=11,15)/ !EP,AC,AT,OC(FD),OH
     &  4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,
     &     6,8,15,
     &  4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,
     &     12,12,15,12,7,10,15,
     &  4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,
     &     6,8,15,
     &  4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,
     &     15,11,5,10,15,
     &  4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,
     &     12,12,15,12,7,10,15/

      DATA ((IPDG2(I,J),I=1,30),J=1,2)/
     &  30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,
     &     23,24,24,27,26,18,16,27,
     &  50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,
     &     43,44,44,50,53,36,37,50/
      DATA ((IPDG2(I,J),I=1,30),J=3,5)/
     &  4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,
     &     39,45,41,33,34,45,
     &  4*41, 40,40,39,42,39,39,40,44, 5*45, 43,44,36,33,36,36,37,38,41,
     &     38,30,31,41,
     &  38, 3*37, 36,37,37,40,35,36,37,41,41,42,40,40,41,40,40,33,30,33,
     &     31,32,31,36,34,32,27,36/
      DATA ((IPDG2(I,J),I=1,30),J=6,7)/
     &  38, 3*37, 3*36, 38,36,35,37, 3*41, 40,40,41,40,40,33, 3*32, 34,
     &     33,38,35,26,28,38,
     &  4*41, 40,40,39,45,37,38,39,43,44,46,43,43,45,43,43,35,33,35,35,
     &     36,36,41,37,30,32,41/
      DATA ((IPDG2(I,J),I=1,30),J=8,9)/
     &  38, 6*37, 41,33,37,37,41,41,42,39,39,41,40,40,33,32,33,33,34,33,
     &     38,35,28,32,38,
     &  31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,
     &     26,26,27,26,31,26,19,23,31/
      DATA ((IPDG2(I,J),I=1,30),J=10,10)/
     &  4*45, 44,44,43,48,41,42,43,47,48,50,47,47,46,44,44,36,34,37,37,
     &     38,37,43,34,30,33,43/
      DATA ((IPDG2(I,J),I=1,30),J=11,15)/ !EP,AC,AT,OC(FD),OH
     &  50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,
     &     43,44,44,50,53,36,37,50,
     &  50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,
     &     43,44,44,50,53,36,37,50,
     &  50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,
     &     43,44,44,50,53,36,37,50,
     &  4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,
     &     39,45,41,33,34,45,
     &  50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,
     &     43,44,44,50,53,36,37,50/

      DATA POT/
     &.25,.30,.35,.40,.45,.50,.55,.60,.65,.70,.75,.80,.85,.90,.95,1.00,
     &1.05,1.10,1.15,1.20,1.25,1.30,1.35,1.40,1.45,1.50,1.55,1.60,1.65,
     &1.70,1.75,1.80,1.85,1.90,1.95,2.00,2.05,2.10,2.15,2.20,2.25,2.30,
     &2.35,2.40,2.45,2.50,2.55,2.60,2.65,2.70,2.75,2.80,2.85,2.90/

C     V3
C     THESE ARE THE COEFFICIENTS OF THE TABULAR MORTALITY MODEL USED
C     FOR FD AND PL; GROUPED BY ICH AND IDF ONLY. THE VALUES ARE
C     ANNUAL PERCENT MORALITY: 0.153%/YR = 0.00153/YR
C
      DATA DBHBRK / 10.0, 25.0, 40.0 /
      DATA BABRK  / 20.0, 30.0, 40.0, 50.0 /

C     EACH ROW IS A BA CLASS; COLUMNS ARE DBH CLASS

C     ICH, PW (1)
      DATA ((FMRT(MRT_ICH,MRT_PW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.153, 0.141, 0.088, 0.062,
     > 0.305, 0.281, 0.176, 0.124,
     > 0.305, 0.281, 0.176, 0.124,
     > 0.364, 0.266, 0.188, 0.124,
     > 0.462, 0.266, 0.188, 0.124 /

C     IDF, PW - use ICH
      DATA ((FMRT(MRT_IDF,MRT_PW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.153, 0.141, 0.088, 0.062,
     > 0.305, 0.281, 0.176, 0.124,
     > 0.305, 0.281, 0.176, 0.124,
     > 0.364, 0.266, 0.188, 0.124,
     > 0.462, 0.266, 0.188, 0.124 /

C     ICH, LW  (2)
      DATA ((FMRT(MRT_ICH,MRT_LW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.196, 0.185, 0.131, 0.058,
     > 0.393, 0.370, 0.263, 0.115,
     > 0.393, 0.350, 0.263, 0.115,
     > 0.456, 0.334, 0.263, 0.115,
     > 0.456, 0.334, 0.263, 0.115 /

C     IDF, LW - use ICH
      DATA ((FMRT(MRT_IDF,MRT_LW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.196, 0.185, 0.131, 0.058,
     > 0.393, 0.370, 0.263, 0.115,
     > 0.393, 0.350, 0.263, 0.115,
     > 0.456, 0.334, 0.263, 0.115,
     > 0.456, 0.334, 0.263, 0.115 /

C     ICH, FD (3)
      DATA ((FMRT(MRT_ICH,MRT_FD,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.281, 0.072, 0.136, 0.123,
     > 0.563, 0.145, 0.272, 0.245,
     > 0.592, 0.297, 0.272, 0.245,
     > 0.400, 0.326, 0.181, 0.326,
     > 0.159, 0.385, 0.233, 0.252 /

C     IDF, FD
      DATA ((FMRT(MRT_IDF,MRT_FD,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.265, 0.259, 0.305, 0.213,
     > 0.529, 0.518, 0.609, 0.427,
     > 0.515, 0.443, 0.332, 0.358,
     > 0.409, 0.647, 0.689, 0.163,
     > 0.409, 0.510, 0.247, 0.163 /

C     ICH, BG (BL) (4,9)
      DATA ((FMRT(MRT_ICH,MRT_BG,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.103, 0.108, 0.064, 0.020,
     > 0.205, 0.216, 0.127, 0.039,
     > 0.480, 0.392, 0.216, 0.039,
     > 0.510, 0.223, 0.187, 0.079,
     > 0.202, 0.092, 0.070, 0.070 /

C     IDF, BG (BL) - use ICH(4,9)
      DATA ((FMRT(MRT_IDF,MRT_BG,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.103, 0.108, 0.064, 0.020,
     > 0.205, 0.216, 0.127, 0.039,
     > 0.480, 0.392, 0.216, 0.039,
     > 0.510, 0.223, 0.187, 0.079,
     > 0.202, 0.092, 0.070, 0.070 /

C     ICH, HW (5)
      DATA ((FMRT(MRT_ICH,MRT_HW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.125, 0.101, 0.076, 0.045,
     > 0.251, 0.202, 0.153, 0.089,
     > 0.198, 0.330, 0.210, 0.089,
     > 0.288, 0.330, 0.210, 0.089,
     > 0.394, 0.200, 0.151, 0.103 /

C     IDF, HW - use ICH
      DATA ((FMRT(MRT_IDF,MRT_HW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.125, 0.101, 0.076, 0.045,
     > 0.251, 0.202, 0.153, 0.089,
     > 0.198, 0.330, 0.210, 0.089,
     > 0.288, 0.330, 0.210, 0.089,
     > 0.394, 0.200, 0.151, 0.103 /

C     ICH, CW (6)
      DATA ((FMRT(MRT_ICH,MRT_CW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.078, 0.068, 0.065, 0.059,
     > 0.157, 0.136, 0.130, 0.119,
     > 0.225, 0.142, 0.136, 0.130,
     > 0.295, 0.223, 0.151, 0.151,
     > 0.202, 0.092, 0.070, 0.049 /

C     IDF, CW - use ICH
      DATA ((FMRT(MRT_IDF,MRT_CW,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.078, 0.068, 0.065, 0.059,
     > 0.157, 0.136, 0.130, 0.119,
     > 0.225, 0.142, 0.136, 0.130,
     > 0.295, 0.223, 0.151, 0.151,
     > 0.202, 0.092, 0.070, 0.049 /

C     ICH, PL (7)
      DATA ((FMRT(MRT_ICH,MRT_PL,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.218, 0.317, 0.196, 0.116,
     > 0.435, 0.633, 0.391, 0.232,
     > 0.457, 0.607, 0.276, 0.232,
     > 0.391, 0.468, 0.405, 0.232,
     > 0.256, 0.388, 0.128, 0.232 /

C     IDF, PL
      DATA ((FMRT(MRT_IDF,MRT_PL,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.282, 0.226, 0.130, 0.130,
     > 0.563, 0.452, 0.260, 0.260,
     > 0.493, 0.445, 0.195, 0.195,
     > 0.413, 0.592, 0.245, 0.245,
     > 0.413, 0.287, 0.433, 0.245 /

C     ICH, SE (combined ICH+IDF) (8)
      DATA ((FMRT(MRT_ICH,MRT_SE,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.126, 0.101, 0.122, 0.058,
     > 0.253, 0.202, 0.244, 0.116,
     > 0.253, 0.202, 0.244, 0.116,
     > 0.253, 0.242, 0.242, 0.116,
     > 0.219, 0.182, 0.152, 0.116 /

C     IDF, SE (combined ICH+IDF)
      DATA ((FMRT(MRT_IDF,MRT_SE,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.126, 0.101, 0.122, 0.058,
     > 0.253, 0.202, 0.244, 0.116,
     > 0.253, 0.202, 0.244, 0.116,
     > 0.253, 0.242, 0.242, 0.116,
     > 0.219, 0.182, 0.152, 0.116 /

C     ICH, PY (same as IDF) (10)
      DATA ((FMRT(MRT_ICH,MRT_PY,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.168, 0.116, 0.089, 0.063,
     > 0.335, 0.231, 0.179, 0.127,
     > 0.320, 0.222, 0.092, 0.046,
     > 0.662, 0.434, 0.206, 0.220,
     > 0.662, 0.434, 0.206, 0.220 /

C     IDF, PY
      DATA ((FMRT(MRT_IDF,MRT_PY,I,J),J=1,MRT_DBH_UB),I=1,MRT_BA_UB)/
     > 0.168, 0.116, 0.089, 0.063,
     > 0.335, 0.231, 0.179, 0.127,
     > 0.320, 0.222, 0.092, 0.046,
     > 0.662, 0.434, 0.206, 0.220,
     > 0.662, 0.434, 0.206, 0.220 /

C     THESE ARE THE COEFFICIENTS OF A LOGISTIC SURVIVAL RELATIONSHIP
C     FITTED BY TEMESGEN HAILEMARIAN, (UBC FORESTRY); DECEMBER 8,
C     2001.

      TYPE (LMRT_STR) LMRT(P_MD)

C     SITE SERIES COEFFICIENTS: NOTE THAT THERE IS A SPECIAL
C     BEC STRING WITH 'all' AS THE SUBZONE. THIS IS USED
C     WHEN THERE IS NOT A PERFECT MATCH TO THE SITE SERIES.

      DATA LMRT(1) /
     >  LMRT_STR (
     >    (/11,12,13,15, 0,0,0,0,0,0,0,0,0,0,0/),     ! EP,AT,CT,OH
     >    (/'ICH/all        ', 'IDF/all        ',
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
     >      '               ', '               ',
     >      '               ', '               '/),
     >    (/-2.774444,  0.684626, -0.180211,  0.004550, -0.634960,
     >       0.063153, -0.276905,  0.000100 /))
     >  /

C  CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'MORTS',5,ICYC)

C  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
C  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
C  MULTIPLIERS, SEE SUBROUTINE MULTS).

      CALL OPFIND (1,MYACTS(1),NTODO)
      IF (NTODO.EQ.0) GOTO 12
      DO 9 I=1,NTODO
        CALL OPGET(I,4,IDATE,IACTK,NP,PRM)
        IF (IACTK.LT.0) GOTO 9
        CALL OPDONE(I,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        IF (ISPCC.EQ.0) GOTO 2
        XMMULT(ISPCC) = PRM(2)
        XMDIA1(ISPCC) = PRM(3)
        XMDIA2(ISPCC) = PRM(4)
        GOTO 9
    2   CONTINUE
        DO 5 ISPCC=1,MAXSP
          XMMULT(ISPCC) = PRM(2)
          XMDIA1(ISPCC) = PRM(3)
          XMDIA2(ISPCC) = PRM(4)
    5   CONTINUE
    9 CONTINUE
   12 CONTINUE

C IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
C PROPORTION IN CRATET, ADJUST THEM HERE.

      IF(PMSDIL .GT. 1.0)PMSDIL = PMSDIL/100.
      IF(PMSDIU .GT. 1.0)PMSDIU = PMSDIU/100.

      IF (ITRN.LE.0) GOTO 100

C SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
C BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
C WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
C FOR MODELING CLIMATE CHANGE.

      CALL SDICAL(SDIMAX)
      IF(DEBUG)WRITE(JOSTND,*)' IN MORTS CYCLE= ',ICYC,'  BAMAX= ',
     &BAMAX,'  SDIMAX= ',SDIMAX

C     ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.

      T     = 0.0
      SD2SQ = 0.0
      DO I=1,ITRN
        P      = PROB(I)
        IS     = ISP(I)
        D      = DBH(I)
        BARK   = BRATIO(IS,D,HT(I))
        G      = DG(I)/BARK
        CIOBDS = (2.0*D*G+G*G)
        SD2SQ  = SD2SQ + P *(D*D+CIOBDS)
        T      = T + P
      ENDDO
      DQ10   = SQRT(SD2SQ/T)
      DELTBA =.005454154 * DQ10 * DQ10 * T - BA

C     PROJECT BASAL AREA FORWARD 10 YEARS ASSUMING THAT BA/BAMAX
C     PROPORTION OF THE PREDICTED BASAL AREA INCREMENT WILL BE LOST TO
C     MORTALITY.  AT BA=BAMAX, MORTALITY WILL ABSORB ALL INCREMENT.

      BA10 = BA + ((BAMAX - BA) / BAMAX) * DELTBA
      TB   = BA10 / (.005454154 * DQ10 * DQ10)

C     NOW CONVERT TO AN ESTIMATE ON ANNUAL TPA MORTALITY RATE.

      TTB = (T - TB) / T
      IF (TTB .GT. 0.9999) TTB = 0.9999
      RZ = 1.0 - (1.0 - (TTB))**0.1

C     CALCULATE AVERAGE DBH

      DSUM=0.0
      WPROB=0.0
      DO I = 1,ITRN
        WPROB=WPROB+PROB(I)
        DSUM=DSUM+DBH(I)*PROB(I)
      ENDDO
      AVED=DSUM/WPROB

C     METRIC BA & SPH

      BAX = BA * FT2pACRtoM2pHA
      SPH = T  * HAtoACR

C     GET STAND BA-CLASS FOR SPECIAL FD AND PL MODELS
C       1 =   0 - 20
C       2 = >20 - 30
C       3 = >30 - 40
C       4 = >40 - 50
C       5 = >50+

      BACLS = MRT_BA_UB
      DO I = 1, (MRT_BA_UB - 1)
        IF (BAX .LE. BABRK(I)) THEN
          BACLS = I
          EXIT
        ENDIF
      ENDDO

C  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
C  ONE AT A TIME WITHIN A SPECIES.

      DO 50 ISPC=1,MAXSP

        I1 = ISCT(ISPC,1)
        IF (I1 .LE. 0) GO TO 50
        I2 = ISCT(ISPC,2)

        IF (LV2ATV) THEN
          GMULT(1) = GMULT_C(1,ISPC)
          GMULT(2) = GMULT_C(2,ISPC)
          REIN(1)  = REIN_C(1,ISPC)
          REIN(2)  = REIN_C(2,ISPC)
          B5       = PMSC(ISPC)
        ELSE
C         FIND AND SET INDEX FOR TABULAR MORTALITY MODELS
          MRTCLS = 0
          IF (ISPC .EQ. 1) THEN
            MRTCLS = MRT_PW
          ELSEIF (ISPC .EQ. 2) THEN
            MRTCLS = MRT_LW
          ELSEIF (ISPC .EQ. 3 .OR. ISPC .EQ. 14) THEN
            MRTCLS = MRT_FD
          ELSEIF (ISPC .EQ. 4 .OR. ISPC .EQ. 9) THEN
            MRTCLS = MRT_BG
          ELSEIF (ISPC .EQ. 5) THEN
            MRTCLS = MRT_HW
          ELSEIF (ISPC .EQ. 6) THEN
            MRTCLS = MRT_CW
          ELSEIF (ISPC .EQ. 7) THEN
            MRTCLS = MRT_PL
          ELSEIF (ISPC .EQ. 8) THEN
            MRTCLS = MRT_SE
          ELSEIF (ISPC .EQ. 10) THEN
            MRTCLS = MRT_PY
          ENDIF
        ENDIF

        XMORT = XMMULT(ISPC)
        D1    = XMDIA1(ISPC)
        D2    = XMDIA2(ISPC)

C       START TREE LOOP WITHIN SPECIES.

        DO 40 I3=I1,I2


C         INITIALIZE FOR NEXT TREE.

          I      = IND1(I3)
          P      = PROB(I)
          WK2(I) = 0.0
          IF (P.LE.0.0) GO TO 40

          D    = DBH(I)
          BARK = BRATIO(ISPC,D,HT(I))
          WKI  = 0.0

          IF (LV2ATV) THEN

            RELDBH=D/AVED
            IF(D.LE.0.5)D=0.5
            DGT=WK1(I)/OLDFNT
            IF(D.LE.1.0 .AND. DGT.LT.0.05) DGT=0.05
            IF(D.LE.5.0 .AND. D.GT.1.0 .AND. DGT.LT.0.05)
     &        DGT=0.05*(5.0-D)/4.0
            G=WK1(I)/(BARK*OLDFNT)
            IF(WK1(I)/OLDFNT .LT. DGT)G=DGT/BARK

C           COMPUTE MORTALITY RATE

            IF((ICYC.EQ.1 .OR. WK1(I).EQ.0.0) .AND. DG(I).GT.0.5)
     &        G=DG(I)/(BARK*10.0)
            IP=1
            IF(D.LE.5.0)IP=2
            G=G*GMULT(IP)
            RIP=2.76253+0.222310*SQRT(D)-0.0460508*SQRT(BA)+
     &        11.2007*G-0.554421/D+B5+0.246301*RELDBH+6.07129*G/D
            IF(RIP.GT.70.0) RIP = 70.0
            IF(RIP.LT.-70.0) RIP=-70.0
            RIP=(1.0/(1.0+EXP(RIP)))
            POTENT=REIN(IP)
            RIP=RIP*POTENT

          ELSE

            ! DBH is at least 2.5cm; important for HW mortality
            DX = MAX(2.5, (D*INtoCM))
            RELDBH=DX/MAX(2.5,AVED*INtoCM)

            IF (MRTCLS .GT. 0) THEN

C             GET DBH-CLASS FOR TABULAR MODELS
C               1 =   0 - 10
C               2 = >10 - 25
C               3 = >25 - 40
C               4 = >40

              DBHCLS = MRT_DBH_UB
              DO J = 1,(MRT_DBH_UB - 1)
                IF (DX .LE. DBHBRK(J)) THEN
                  DBHCLS = J
                  EXIT
                ENDIF
              ENDDO
              RIP = TMRT(MRTCLS,BACLS,DBHCLS)

            ELSE

C             COMPUTE MORTALITY RATE; SPECIES WITH NO AVAILABLE ESTIMATE
C             HAVE COMPLETE MORTALITY. NOTE THAT THE MORT%() EXPRESSION
C             INITIALLY COMPUTES SURVIVAL. AFTER EXPONENTIATION IT IS
C             CONVERTED TO MORTALITY: (1.0 - SURVIVAL)

              BAL = (1.0 - (PCT(I)/100.)) * BAX
!             IF (MORT%FIT(ISPC) .AND. LLTDGOK(ISPC)) THEN
              IF (MORT%FIT(ISPC)) THEN
                RIP = MORT%CON(ISPC)
     >            + (MORT%INVDBH(ISPC)  / DX)
     >            + (MORT%DBH(ISPC)     * DX)
     >            + (MORT%DBHSQ(ISPC)   * DX*DX)
     >            + (MORT%RDBH(ISPC)    * RELDBH)
     >            + (MORT%BAL(ISPC)     * BAL)
     >            + (MORT%SQRTBA(ISPC)  * SQRT(BAX))
     >            + (MORT%SPH(ISPC)     * SPH)
              ELSE
                RIP = 99.0
              ENDIF
              RIP = MAX(-70.0, MIN(70.0, RIP))
              RIP = 1.0 - (1.0 / (1.0 + EXP(RIP)))
            ENDIF

          ENDIF

C         MAKE ADJUSTMENT FOR APPROACH TO MAXIMUM BASAL AREA.

          RIPP = BA * RZ
          IF (BA .GT. BAMAX) GOTO 27
          RIPP = RIPP + (BAMAX - BA) * RIP
   27     RIPP = RIPP / BAMAX
          IF (RIPP .LT. RIP) RIPP = RIP
          IF (RIPP .GT. 1.0) RIPP = 1.0

C  APPLY MORTALITY MULTIPLIERS WITHIN USER-PROVIDED DBH LIMITS
C  AND STORE TREES PER ACRE DYING IN THE ARRAY WK2.

          X = 1.0
          IF (D .GE. D1 .AND. D .LT. D2) X = XMORT

C  FROM HERE TO STATEMENT 29 IS ESTABLISHMENT MODEL CODE.
C  "BEST" TREES NOT SUBJECT TO MORTALITY FOR 20 YRS
C  AFTER DISTURBANCE DATE.

          IF (IESTAT(I) .LE. 0) GOTO 29
          IF (IY(ICYC) .GE. IESTAT(I)) IESTAT(I) = 0
          XCHECK = FLOAT(IESTAT(I) - IY(ICYC)) / FINT
          IF (XCHECK .GT. 1.0) XCHECK = 1.0
          IF (XCHECK .LT. 0.0) XCHECK = 0.0
           X = X * (1.0 - XCHECK)
   29     CONTINUE
          WKI = P * (1.0 - (1.0 - RIPP)**FINT) * X

          G = (DG(I)/BARK) * (FINT/10.0)
          IDMFLG=IFIX(SIZCAP(ISPC,3))
          IF((D+G).GE.SIZCAP(ISPC,1) .AND. IDMFLG.NE.1) THEN
            WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
            IF(DEBUG)WRITE(JOSTND,*)' SIZE CAP RESTRICTION IMPOSED, ',
     &      'I,ISPC,D,P,SIZCAP 1-3,WKI = ',
     &      I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
          ENDIF

          IF (WKI .GT. P) WKI = P
C----------
C IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
C SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.
C----------
          IF(SDIMAX .LT. 5)THEN
            WKI=P
          ENDIF
          WK2(I) = WKI

C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          IF (.NOT.DEBUG) GO TO 40
          PRES = P - WKI
          VLOS = WKI * CFV(I) / FINT
          WRITE(JOSTND,9000) I,ISPC,D,G,BA,P,WKI,PRES,VLOS
 9000     FORMAT(' IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &      ',  DBH INCREMENT=',F7.4,',  BA=',F7.2,
     &      ',  INIT PROB=',F9.3/
     &      ',  TREES DYING=',F9.3,' RES PROB=',F9.3,
     &      ',  VOL LOST=',F9.3)
   40   CONTINUE
   50 CONTINUE

  100 CONTINUE

C----------
C  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES
C---------
      CALL CLMORTS

C  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
C  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT

      CALL OPFIND (1,MYACTS(2),NTODO)
      IF (NTODO.GT.0) THEN
        IF(DEBUG)WRITE(JOSTND,*)' FIXMORT PROCESSING, ITODO= ',ITODO
        DO 300 ITODO=1,NTODO
        CALL OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
        IF (IACTK.LT.0) GOTO 300
        CALL OPDONE(ITODO,IY(ICYC))
        ISPCC=IFIX(PRM(1))
        IF(NP .LE. 4)THEN
          IF(PRM(2).GT. 1.0)PRM(2)=1.0
        ENDIF
        IF(PRM(3).LT. 0.0)PRM(3)=0.0
        IF(PRM(4).LE. 0.0)PRM(4)=999.*INtoCM
        IP=1
        IF (NP.GT.4) THEN
          IF (PRM(5).EQ.1.0) THEN
            IP=2
          ELSEIF (PRM(5).EQ.2.0) THEN
            IP=3
          ELSEIF (PRM(5).EQ.3.) THEN
            IP=4
          ENDIF
        ENDIF

C  SET FLAG FOR POINT MORTALITY, OR KILLING FROM ABOVE
C    PRM(6)    POINT      SIZE   KBIG     KILL DIRECTION
C      0         NO        NO     0      *DEFAULT CONDITION*
C      1         YES       NO     0
C     10         NO        YES    1       BOTTOM UP
C     11         YES       YES    1       BOTTOM UP
C     20         NO        YES    2       TOP DOWN
C     21         YES       YES    2       TOP DOWN

        KPOINT=0
        KBIG=0
        IF(PRM(6).GT.0.)THEN
          IF(PRM(6) .EQ. 1)THEN
            KPOINT=1
          ELSEIF(PRM(6) .EQ. 10)THEN
            KBIG=1
          ELSEIF(PRM(6) .EQ. 11)THEN
            KPOINT=1
            KBIG=1
          ELSEIF(PRM(6) .EQ. 20)THEN
            KBIG=2
          ELSEIF(PRM(6) .EQ. 21)THEN
            KPOINT=1
            KBIG=2
          ENDIF
        ENDIF

        IF (ITRN.GT.0) THEN

C IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
C EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
C   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
C   ARE IN EFFECT.
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
C   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.

            IF(KBIG.GE.1 .OR. (KPOINT.EQ.1 .AND. IPTINV.GT.1)) THEN
            XMORE=0.
              DO 199 I=1,ITRN
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 90 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 91
                ENDIF
   90           CONTINUE
              ENDIF
   91         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                 GOTO (191,192,193,194),IP
  191           CONTINUE
                XMORE=XMORE+PROB(I)*PRM(2)
                WK2(I)=0.
                GOTO 199
  192           CONTINUE
                XMORE=XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                GOTO 199
  193           CONTINUE
                TEMP=AMAX1(WK2(I),(PROB(I)*PRM(2)))
                IF(TEMP .GT. WK2(I)) THEN
                  XMORE=XMORE+TEMP-WK2(I)
                ENDIF
                GOTO 199
  194           CONTINUE
                XMORE=XMORE+WK2(I)*PRM(2)
                WK2(I)=0.
                GOTO 199
              ENDIF
  199       CONTINUE
            IF(DEBUG)WRITE(JOSTND,*)' KPOINT,KBIG,ITRN,XMORE= ',
     &               KPOINT,KBIG,ITRN,XMORE
            CREDIT=0.
            DO 201 I=1,ITRN
            IWORK1(I)=IND1(I)
            IF(KBIG .EQ. 1)THEN
              WORK3(I)=(-1.0)*
     &                (DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
            ELSE
              WORK3(I)=DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
            ENDIF
  201       CONTINUE
            CALL RDPSRT(ITRN,WORK3,IWORK1,.FALSE.)
            IF(DEBUG)WRITE(JOSTND,*)' DBH= ',(DBH(IG),IG=1,ITRN)
            IF(DEBUG)WRITE(JOSTND,*)' IWORK1= ',(IWORK1(IG),IG=1,ITRN)
            IF(DEBUG)WRITE(JOSTND,*)' WK2= ',(WK2(IG),IG=1,ITRN)

            IF(KBIG.GE.1 .AND. KPOINT.EQ.0)THEN

C  CONCENTRATION BY SIZE ONLY

              DO 310 I=1,ITRN
              IX=IWORK1(I)
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 92 IG=2,IULIM
                IF(ISP(IX) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 93
                ENDIF
   92           CONTINUE
                ENDIF
   93           CONTINUE
                IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(IX)-WK2(IX)
                 IF((TEMP .LE. XMORE).OR.
     >               (ABS(TEMP-XMORE).LT.0.0001))THEN
                  CREDIT=CREDIT+PROB(IX)-WK2(IX)
                  WK2(IX)=PROB(IX)
                ELSE
                  WK2(IX)=WK2(IX)+XMORE-CREDIT
                  CREDIT=XMORE
                  GO TO 295
                ENDIF
              ENDIF
  310         CONTINUE
              GO TO 295

              ELSEIF(KPOINT.EQ.1 .AND. KBIG.EQ.0)THEN

C  CONCENTRATION ON POINTS ONLY


            DO 205 J=1,IPTINV
              DO 204 I=1,ITRN
                IF (ITRE(I) .NE. J) GO TO 204
                LINCL = .FALSE.
                IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                  LINCL = .TRUE.
                ELSEIF(ISPCC.LT.0)THEN
                  IGRP = -ISPCC
                  IULIM = ISPGRP(IGRP,1)+1
                  DO 94 IG=2,IULIM
                  IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                    LINCL = .TRUE.
                    GO TO 95
                  ENDIF
   94             CONTINUE
                ENDIF
   95         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                  TEMP=CREDIT+PROB(I)-WK2(I)
                  IF((TEMP .LE. XMORE).OR.
     >               (ABS(TEMP-XMORE).LT.0.0001))THEN
                    CREDIT=CREDIT+PROB(I)-WK2(I)
                    WK2(I)=PROB(I)
                  ELSE
                    WK2(I)=WK2(I)+XMORE-CREDIT
                    CREDIT=XMORE
                    GO TO 295
                  ENDIF
                ENDIF
  204         CONTINUE
  205       CONTINUE
            GO TO 295

C  CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
C  WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
C  POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.
              ELSE
              DO 312 J=1,IPTINV
              DO 311 I=1,ITRN
              IX=IWORK1(I)
              IF(ITRE(IX) .NE. J)GO TO 311
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 96 IG=2,IULIM
                IF(ISP(IX) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 97
                ENDIF
   96           CONTINUE
              ENDIF
   97         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(IX)-WK2(IX)
                IF((TEMP .LE. XMORE).OR.
     >             (ABS(TEMP-XMORE).LT.0.0001))THEN
                  CREDIT=CREDIT+PROB(IX)-WK2(IX)
                  WK2(IX)=PROB(IX)
                ELSE
                  WK2(IX)=WK2(IX)+XMORE-CREDIT
                  CREDIT=XMORE
                  GO TO 295
                ENDIF
              ENDIF
  311         CONTINUE
  312         CONTINUE
              GO TO 295
              ENDIF

          ENDIF

C  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
C  IS NOT IN EFFECT.

          DO 290 I=1,ITRN
            LINCL = .FALSE.
            IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
              LINCL = .TRUE.
            ELSEIF(ISPCC.LT.0)THEN
              IGRP = -ISPCC
              IULIM = ISPGRP(IGRP,1)+1
              DO 98 IG=2,IULIM
              IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                LINCL = .TRUE.
                GO TO 99
              ENDIF
   98         CONTINUE
            ENDIF
   99       CONTINUE
            IF (LINCL .AND.
     >         (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
               GOTO (610,620,630,640),IP
  610          CONTINUE
               WK2(I)=PROB(I)*PRM(2)
               GOTO 290
  620          CONTINUE
               WK2(I)=WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
               GOTO 290
  630          CONTINUE
               WK2(I)=AMAX1(WK2(I),(PROB(I)*PRM(2)))
               GOTO 290
  640          CONTINUE
               WK2(I)=AMIN1(PROB(I),WK2(I)*PRM(2))
               GOTO 290
            ENDIF
  290     CONTINUE
  295   CONTINUE
        IF(DEBUG)WRITE(JOSTND,*)' ITODO,WK2= ',
     &    ITODO,(WK2(IG),IG=1,ITRN)
        ENDIF
  300   CONTINUE
      ENDIF

      RETURN

      ENTRY MORCON

C  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT REQUIRE ONE-
C  TIME RESOLUTION.

      IF (LV2ATV) THEN

C       ITYPE IS A HABITAT INDEX THAT IS COMPUTED IN *HABTYP*
        IPT=IPDG(ITYPE,IFOR)
        POTEN=POT(IPT)
        DO ISPC=1,MAXSP
          IF (LLTDGOK(ISPC)) THEN
            GMULT_C(1,ISPC)=.90/(POTEN*V2SEICOR(ISPC))
            REIN_C(1,ISPC)=
     >        (1-(POTEN*V2SEICOR(ISPC)/20.+1.)**(-1.605))/.06821
          ELSE
            GMULT_C(1,ISPC)=.90/POTEN
            REIN_C(1,ISPC)=(1-(POTEN/20.+1.)**(-1.605))/.06821
          ENDIF
        ENDDO

        IPT2=IPDG2(ITYPE,IFOR)
        POTEN=POT(IPT2)
        DO ISPC=1,MAXSP
          IF (LLTDGOK(ISPC)) THEN
            GMULT_C(2,ISPC)=2.50/(POTEN*V2SEICOR(ISPC))
            REIN_C(2,ISPC)=
     >        (1-(POTEN*V2SEICOR(ISPC)+1.)**(-1.605))/.86610
          ELSE
            GMULT_C(2,ISPC)=2.50/POTEN
            REIN_C(2,ISPC)=(1-(POTEN+1.)**(-1.605))/.86610
          ENDIF
        ENDDO

      ELSE

C       REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
C       STRING LENGTHS WILL BE FIGURED CORRECTLY
        DO J = 1,P_MD
          DO K = 1,P_SS
            DO M = 1,15
              IF (ICHAR(LMRT(J)%PrettyName(K)(M:M)) .EQ. 0)
     >          LMRT(J)%PrettyName(K)(M:M) = ' '
            ENDDO
          ENDDO
        ENDDO

C       LOCATE ZONE AND SITE SERIES; IF NO PERFECT MATCH TO SUBZONE
C       CAN BE FOUND, TRY ".../all" AND LOOP ONE MORE TIME BEFORE
C       GIVING UP ON A MATCH.
        MORT%PrettyName = BEC%PrettyName
        DO I = 1,MAXSP
          IPOS(I) = 0
        ENDDO
        DO I = 1,MAXSP
          IPAS = 0
          PAT  = BEC%PrettyName
    7     DO J = 1, P_MD
            DO K = 1,MAXSP
              IF (LMRT(J)%SPP(K) .EQ. 0) EXIT
              IF (LMRT(J)%SPP(K) .EQ. I) THEN
                DO M = 1, P_SS
                  IF (LEN_TRIM(LMRT(J)%PrettyName(M)) .EQ. 0) EXIT
                  IF (INDEX(LMRT(J)%PrettyName(M),PAT) .GT. 0) THEN
                    IPOS(I) = J
                    GOTO 8
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO

          IF (J .GT. P_MD .AND. IPAS .EQ. 0) THEN
            IPAS = 1
            PAT = BEC%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
            GOTO 7
          ENDIF
    8   ENDDO

C       COPY COEFFICIENTS INTO MORT STRUCTURE
        DO I = 1,MAXSP
          MORT%FIT(I) = .FALSE.
          J           = IPOS(I)
          IF (J .GT. 0) THEN
            MORT%FIT(I)    = .TRUE.
            MORT%CON(I)    = LMRT(J)%COEF(1)
            MORT%INVDBH(I) = LMRT(J)%COEF(2)
            MORT%DBH(I)    = LMRT(J)%COEF(3)
            MORT%DBHSQ(I)  = LMRT(J)%COEF(4)
            MORT%RDBH(I)   = LMRT(J)%COEF(5)
            MORT%BAL(I)    = LMRT(J)%COEF(6)
            MORT%SQRTBA(I) = LMRT(J)%COEF(7)
            MORT%SPH(I)    = LMRT(J)%COEF(8)
          ENDIF
        ENDDO

C       COPY COEFFICIENTS FOR TABULAR MORTALITY MODELS IN ICH- AND IDF-ZONES
C       CONVERSION: FMRT(...)/100 CONVERTS ANNUAL % MORTALITY (FROM TEMESGEN'S
C       TABLE) TO ANNUAL PROPORTION MORTALITY.

        BECCLS = 1
        IF (INDEX(BEC%Zone,'ICH') .GT. 0) BECCLS = 1
        IF (INDEX(BEC%Zone,'IDF') .GT. 0) BECCLS = 2
C           MAP SBPS TO IDF
        IF (INDEX(BEC%Zone,'SBPS') .GT. 0) BECCLS = 2
C           MAP SBS TO ICH, but ONE subzone will be mapped differently
        IF (INDEX(BEC%Zone,'SBS') .GT. 0) BECCLS = 1
        IF (INDEX(BEC%PrettyName,'SBSdw2') .GT. 0) BECCLS = 2

        DO I = 1,MRT_PY
          DO J = 1,MRT_BA_UB
            DO K = 1,MRT_DBH_UB
!!              TMRT(I,J,K) =
!!     >          EXP(LOG(1.0 - (FMRT(BECCLS,I,J,K) * 0.1)) / 10.0)
              TMRT(I,J,K) = FMRT(BECCLS,I,J,K)/100.0
            ENDDO
          ENDDO
        ENDDO

      ENDIF

      RETURN
      END
