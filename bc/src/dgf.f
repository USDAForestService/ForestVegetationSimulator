      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE

C  **DGF--SEI    DATE OF LAST REVISION:  06/06/05

C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN DIAMETER)
C  [NOT SQUARED DIAM, AS IN NI, ETC] FOR EACH TREE RECORD, AND LOADS
C  IT INTO THE ARRAY WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
C  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
C  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
C  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
C

COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CALCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'METRIC.F77'
      INCLUDE 'BCPLOT.F77'
C
COMMONS
C
C  DIMENSIONS FOR INTERNAL VARIABLES.
C
C     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
C             ARGUEMENT).
C     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
C             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
C             SPECIES).
C     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
C             EACH SPECIES).
C   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO SQUARED TERM IN THE DDS MODEL (ONE
C             COEFFICIENT FOR EACH SPECIES).
C    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
C             LARGER TREES TERM IN THE DDS MODEL
C             (ONE COEFFICIENT FOR EACH SPECIES).
C   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
C             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
C             COEFFICIENT PER SPECIES).
C    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
C             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).
C---------
C
C     P_ZL  - LENGTH OF ZONE AND SITE SERIES STRINGS
C     P_ZN  - NUMBER OF ZONE MODELS
C     P_ST  - NUMBER OF SITE SERIES STRINGS PER ZONE OR SIBEC GROUP
C     P_SS  - NUMBER OF SIBEC GROUPS
C
      INTEGER   P_ZL,P_ZN
      INTEGER   P_ST,P_SS
      PARAMETER (P_ZL = 15)
      PARAMETER (P_ZN = 28)
      PARAMETER (P_ST = 30)
      PARAMETER (P_SS = 25)
C
C     STRUCTURE FOR THE FITTED MODELS. COEFFICIENTS FOR EACH FITTED MODEL 
C     ARE FOUND HERE. ONLY ONE OF DBAL1 OR DBAL2 TERMS CAN HAVE A NON-ZERO
C     VALUE. THE BIOLOGICAL SCOPE OF THE MD_STR OBJECT IS SOMETIMES THE BEC
C     ZONE, SOMETIMES ALL BEC ZONES COMBINED, AND SOMETIMES THE SITE SERIES.
C
      TYPE MD_STR
        INTEGER              :: SPP(MAXSP) ! species list
        CHARACTER (LEN=P_ZL) :: ZONE(P_ST) ! BEC list
        INTEGER              :: OBSERV     ! observations
        REAL                 :: CON        ! constant
        REAL                 :: CASP       ! cos-aspect
        REAL                 :: SASP       ! sin-aspect
        REAL                 :: EL         ! elevation
        REAL                 :: EL2        ! elevation^2
        REAL                 :: CCFA       ! ccf
        REAL                 :: LD         ! diam
        REAL                 :: DSQ        ! diam^2
        REAL                 :: DBAL1      ! bal/dbh
        REAL                 :: DBAL2      ! bal/log(dbh+1)
        REAL                 :: CR         ! crown
        REAL                 :: BAL        ! bal
        REAL                 :: SIGMAR     ! residual error
      END TYPE
C
C     THE SURROGATE SPECIES (SP2) IS USED WHEN ONE SPECIES IS A 2ND SPECIES
C     AS A SURROGATE. THIS OCCURS WHEN THERE IS NO AVAILABLE ESTIMATE FOR
C     THE 1ST SPP
C
      TYPE SS_STR
        INTEGER              :: SPP(MAXSP)       ! species list
        CHARACTER (LEN=P_ZL) :: PrettyName(P_ST) ! BEC/SS list
        REAL                 :: CON              ! ss coefficient
      END TYPE

      REAL DIAM(MAXTRE)

      CHARACTER*15 PAT1, PAT2
      INTEGER I,J,K,M,I1,I2,I3,IP,JP,ISPC,IPAS,iSeries
      INTEGER IPOS(MAXSP),JPOS(MAXSP)
      REAL    D,CR,BAL,BALD1,BALD2,DDS,CONSPP,XPPDDS,PELEV,D2
      REAL    BRATIO

      LOGICAL DEBUG
	LOGICAL LSPPOK(MAXSP)
      REAL BECADJ(3),SEICN2(MAXSP),SEISLP(MAXSP),SEISAS(MAXSP),
     &     SEICAS(MAXSP),SEIELV(MAXSP),SEIEL2(MAXSP),SEICCF(MAXSP),
     &     DGCON1(MAXSP),DGCON2(MAXSP)
c     v2
      REAL BA100,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,DGDBLS,RELDN2,ALD
      REAL DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP),
     &    DGCRSQ(MAXSP),DGDBAL(MAXSP),DGHAB(6,MAXSP),DGFOR(6,MAXSP),
     &    DGDS(4,MAXSP),DGEL(MAXSP),DGEL2(MAXSP),DGSASP(MAXSP),
     &    DGCASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP),DGCCFA(5,MAXSP),
     &    OBSERV(6,MAXSP)
      INTEGER MAPHAB(30,MAXSP),MAPDSQ(11,MAXSP),MAPLOC(11,MAXSP),
     &    MAPCCF(30,MAXSP)
      
      DATA DGLD/
     &   0.56445, 0.54140, 0.56888, 0.68810, 0.68712, 0.58705,
     &   0.89503, 0.73045, 0.86240, 0.66101,
     >   0.89778, 0.89778, 0.89778, 0.56888, 0.89778/
      DATA DGCR/
     &   1.08338, 1.03478, 2.06850, 1.93969, 1.64133, 1.29360,
     &   1.85558, 1.54643, 0.52044, 1.31618,
     >   1.28403, 1.28403, 1.28403, 2.06850, 1.28403/
      DATA DGCRSQ/
     &   0.0,     0.07509,-0.62361, -0.78258,-0.27244,     0.0,
     &  -0.36393,-0.26635, 0.86236,  0.0,
     >   0.0,     0.0,     0.0,     -0.62361, 0.0/
      DATA DGBAL/
     &   0.42112, 0.43637, 0.50202, 0.45142,     0.0, 0.74596,
     &  -0.03665, 0.25639, 0.0,     0.0,
     >   0.0,     0.0,     0.0,     0.50202,     0.0/
      DATA DGDBAL/
     &  -2.08272,-2.03256,-2.11590,-1.76812,-0.80918,-2.28375,
     &  -0.43329,-1.18218,-0.51270,-1.25881,
     >  -0.66110,-0.66110,-0.66110,-2.11590,-0.66110/
      DATA  OBSERV/
     &  1635.0,   76.0,   94.0,    0.0,    0.0,    0.0,
     &   968.0,  900.0,  797.0, 1124.0,   47.0,  809.0,
     &  1079.0, 1682.0, 4014.0,   63.0, 4684.0,    0.0,
     &   816.0,   67.0,  122.0, 3678.0,    0.0,    0.0,
     &  1774.0,    0.0,    0.0,    0.0,    0.0,    0.0,
     &  1176.0,   67.0, 1198.0,    0.0,    0.0,    0.0,
     &    93.0,  940.0,   62.0, 2548.0,  719.0, 2590.0,
     &   907.0,  827.0,   46.0,   35.0,  146.0, 3092.0,
     &   988.0,  177.0,   40.0,  331.0,  332.0, 2380.0,
     &   429.0,   59.0,  605.0,   17.0,  879.0,    0.0,
     &    55.0,  880.0,    0.0,    0.0,    0.0,    0.0,
     &    55.0,  880.0,    0.0,    0.0,    0.0,    0.0,
     &    55.0,  880.0,    0.0,    0.0,    0.0,    0.0,
     &  1079.0, 1682.0, 4014.0,   63.0, 4684.0,    0.0,
     &    55.0,  880.0,    0.0,    0.0,    0.0,    0.0/

C  DGHAB IS AN ARRAY THAT CONTAINS HABITAT CLASS INTERCEPTS FOR
C  EACH SPECIES.  MAPHAB IS INDEXED BY ITYPE TO MAP HABITAT
C  TYPES ONTO HABITAT CLASSES.

      DATA MAPHAB/
     &  12*2, 7*1, 2,2,1,1, 7*2,
     &  9*5, 1,1,2,1, 3*2, 3,3,2,5,2, 3*1, 5,5,4, 3*5,
     &  7*3, 1, 3*3, 8*1, 3,2, 6*3, 2,3,3,
     &  12*2, 1, 8*2, 1,2,2,1, 5*2,
     &  30*1,
     &  14*2, 1,1, 14*2,
     &  4*5, 1,2,1, 4*5, 2,2, 6*3, 4,4,3,4, 3*5, 4,4,5,5,
     &  11*4, 1,1,4,2,2,4,2,1, 8*4, 3,4,4,
     &  12*6, 1,2,3,3,4,3,1, 7*6, 1,5,5,6,
     &  1,1,2,3,3,2,2, 4*3, 3*2, 4*3, 2, 11*3,
     &  21*2, 1, 8*2,
     &  21*2, 1, 8*2,
     &  21*2, 1, 8*2,
     &  7*3, 1, 3*3, 8*1, 3,2, 6*3, 2,3,3,
     &  21*2, 1, 8*2/

      DATA DGHAB/
     & 1.15584, 1.05635,     0.0,     0.0,     0.0,     0.0,
     & 0.38335, 0.51291, 0.45377, 0.71322, 0.26835,     0.0,
     & 0.47780, 0.15228, 0.29764,     0.0,     0.0,     0.0,
     & 0.66755, 0.60454,     0.0,     0.0,     0.0,     0.0,
     & 0.45264,     0.0,     0.0,     0.0,     0.0,     0.0,
     & 1.61452, 1.31772,     0.0,     0.0,     0.0,     0.0,
     & 0.77399, 0.67828, 0.64451, 0.37945, 0.54337,     0.0,
     &-0.58842,-0.21235,-0.71629,-0.53954,     0.0,     0.0,
     &-0.96389,-0.72415,-0.57308,-0.82218,-1.24093,-1.10746,
     & 1.16233, 0.73408, 0.51417,     0.0,     0.0,     0.0,
     &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,
     &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,
     &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,
     & 0.47780, 0.15228, 0.29764,     0.0,     0.0,     0.0,     
     &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0/

C  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
C  HABITAT CLASS.  MAPCCF IS INDEXED BY ITYPE TO MAP HABITAT TYPES
C  ONTO HABITAT CLASSES.

      DATA MAPCCF/
     &  12*3, 1, 3*3, 1, 4*3, 2,2, 7*3,
     &  11*3, 1, 6*3, 2,3,1,3,3,1, 6*3,
     &  6*4, 1,2, 3*4, 2,1, 3*4, 3, 3*4, 1,3,2, 4*4, 2,4,4,
     &  12*3, 1, 9*3, 2,2, 6*3,
     &  30*1,
     &  13*3, 1,2,2,3,3,2,3,3,1, 8*3,
     &  4,4,1,4,3,4,2,1,4,1, 3*4, 6*2, 4,1,4,1, 3*4, 1, 3*4,
     &  11*4, 3,3, 3*1, 3,3,1,4,2,4,1, 3*4, 1, 3*4,
     &  12*3, 7*1, 3,1,1,2,2,1,3,1,1,3,3,
     &  2,2,3,1,4,3,3,1,2,4,4,1, 4*4, 1, 13*4,
     &  30*1,
     &  30*1,
     &  30*1,
     &  6*4, 1,2, 3*4, 2,1, 3*4, 3, 3*4, 1,3,2, 4*4, 2,4,4,
     &  30*1/
     
      DATA DGCCFA/
     & -0.02430,-0.24886,-0.01079,     0.0,     0.0,
     & -0.10144,-0.14793,-0.05438,     0.0,     0.0,
     & -0.09046,-0.11884,-0.05529,-0.02180,     0.0,
     & -0.09624,-0.19544,-0.05119,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     & -0.05054,-0.15356,-0.09396,     0.0,     0.0,
     & -0.05576,-0.14919,-0.40640,-0.11400,     0.0,
     & -0.01547,-0.38386,-0.05371,-0.15159,     0.0,
     & -0.01598,-0.04477,-0.07392,     0.0,     0.0,
     & -0.10416,-0.88809,-0.25938,-0.14726,     0.0,
     & -0.10744,     0.0,     0.0,     0.0,     0.0,
     & -0.10744,     0.0,     0.0,     0.0,     0.0,
     & -0.10744,     0.0,     0.0,     0.0,     0.0,
     & -0.09046,-0.11884,-0.05529,-0.02180,     0.0,
     & -0.10744,     0.0,     0.0,     0.0,     0.0/

C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

      DATA MAPLOC/
     & 2,1,2,2,2,2,2,2,2,2,1,
     & 1,1,1,2,3,3,2,5,5,4,1,
     & 5,4,1,2,3,3,2,3,5,1,4,
     & 6,5,1,2,2,3,2,4,6,2,5,
     & 3,2,3,1,3,3,3,3,3,3,2,
     & 4,1,1,1,2,2,3,4,2,1,1,
     & 4,2,1,1,2,4,3,3,4,2,2,
     & 3,1,1,1,3,2,3,3,3,1,1,
     & 4,1,1,2,2,3,3,4,4,2,1,
     & 1,2,2,2,1,4,3,1,4,3,2,
     & 3,2,1,1,3,3,3,3,1,3,2,
     & 3,2,1,1,3,3,3,3,1,3,2,
     & 3,2,1,1,3,3,3,3,1,3,2,
     & 5,4,1,2,3,3,2,3,5,1,4,
     & 3,2,1,1,3,3,3,3,1,3,2/
 
      DATA DGFOR/
     & 0.16920,     0.0,     0.0,     0.0,     0.0,     0.0,
     & 0.20004, 0.07656, 0.08188, 0.30379,     0.0,     0.0,
     & 0.50357, 0.34920, 0.21961, 0.61812,     0.0,     0.0,
     & 0.43443, 0.28344,-0.14829, 0.20205, 0.57763,     0.0,
     & 0.10667, 0.44357,     0.0,     0.0,     0.0,     0.0,
     & 0.50070, 0.17647, 0.31745,     0.0,     0.0,     0.0,
     & 0.43735, 0.21113, 0.14808,     0.0,     0.0,     0.0,
     & 0.26262,-0.15871,     0.0,     0.0,     0.0,     0.0,
     & 0.42062, 0.14072,-0.13001,     0.0,     0.0,     0.0,
     & 0.24588, 0.56958, 0.42787,     0.0,     0.0,     0.0,
     & 0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,
     & 0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,
     & 0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,
     & 0.50357, 0.34920, 0.21961, 0.61812,     0.0,     0.0,
     & 0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0/

C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
C  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
C  COEFFICIENT.

      DATA MAPDSQ/
     & 1,2,2,2,2,1,2,1,1,1,2,
     & 1,2,1,1,1,1,1,1,1,1,2,
     & 1,4,2,2,2,3,1,4,1,1,4,
     & 1,1,1,1,1,2,2,3,1,2,1,
     & 1,3,1,2,3,1,1,1,1,1,3,
     & 1,1,2,1,1,1,1,2,1,2,1,
     & 1,1,2,2,1,1,2,3,1,4,1,
     & 1,1,2,1,1,1,3,2,1,1,1,
     & 1,2,2,1,2,1,1,2,1,1,2,
     & 1,2,2,2,2,3,3,2,1,1,2,
     & 1,2,1,1,1,1,1,1,2,1,2,
     & 1,2,1,1,1,1,1,1,2,1,2,
     & 1,2,1,1,1,1,1,1,2,1,2,
     & 1,4,2,2,2,3,1,4,1,1,4,
     & 1,2,1,1,1,1,1,1,2,1,2/

      DATA DGDS/
     & -0.000439,-0.000004,      0.0,      0.0,
     & -0.000310,-0.000566,      0.0,      0.0,
     & -0.000252,-0.000373,-0.000502,-0.000572,
     & -0.000274,-0.000089,-0.000643,      0.0,
     & -0.000225,-0.000216,-0.000429,      0.0,
     &       0.0,      0.0,      0.0,      0.0,
     & -0.001260,-0.002168,-0.001889,-0.000867,
     & -0.000132,-0.000294,-0.000427,      0.0,
     & -0.000283,-0.000780,      0.0,      0.0,
     & -0.000406,-0.000437,-0.000140,      0.0,
     & -0.000484,-0.000306,      0.0,      0.0,
     & -0.000484,-0.000306,      0.0,      0.0,
     & -0.000484,-0.000306,      0.0,      0.0,
     & -0.000252,-0.000373,-0.000502,-0.000572,
     & -0.000484,-0.000306,      0.0,      0.0/

C  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
C  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
C  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
C  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
C  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
C  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
C  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
C  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
C  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
C  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

      DATA DGCASP/
     &   0.09817,-0.21337,-0.04562,-0.01215, 0.08277,-0.06625,
     &   0.00325,-0.13091,-0.12473,-0.09976,
     &   0.17935, 0.17935, 0.17935,-0.04562,0.17935/
      DATA DGSASP/
     &   0.03876, 0.03430, 0.06287,-0.04595, 0.10987, 0.05534,
     &   0.12993,-0.06038,-0.06862, 0.01192,
     &   0.13363, 0.13363, 0.13363, 0.06287, 0.13363/      
      DATA DGSLOP/
     &  -0.17888, 0.33523, 0.78176, 1.17025, 0.04966, 0.11931,
     &   0.46546, 0.65622, 0.30070,-0.06637,
     &   0.07628, 0.07628, 0.07628, 0.78176, 0.07628/
      DATA DGSLSQ/
     &   0.0,    -0.70216,-1.12380,-1.52006, 0.0,     0.0,
     &  -0.58014,-0.90143,-0.62224,-0.43720,
     &   0.0,     0.0,     0.0,    -1.12380, 0.0/
      DATA DGEL/
     &   0.03517, 0.03730, 0.02591, 0.00917, 0.02863,-0.00175,
     &  -0.00480, 0.06259, 0.06313, 0.03229,
     &   0.08518, 0.08518, 0.08518, 0.02591, 0.08518/
      DATA DGEL2/
     &  -0.000467,-0.000433,-0.000377,-0.000117,-0.000422,-0.000067,
     &  -0.000058,-0.000709,-0.000676,-0.000422,
     &  -0.000943,-0.000943,-0.000943,-0.000377,-0.000943/

      TYPE (BEC_DESCRIPTION) BEC2
      TYPE (MD_STR) ZNKONST(P_ZN)
      TYPE (SS_STR) SSKONST(P_SS)

      DATA (ZNKONST(I), I=1,24) / 

     >  MD_STR (             ! PW - ICH+IDF
     >    (/1, (0, I=1,14) /),  ! spp
     >    (/ 'ICH/all', 'IDF/all', ('', I=1,28) /),
     >    200,               ! observations
     >    4.9231877,         ! constant
     >    0.928152,          ! cos-asp
     >    0.0,               ! sin-asp
     >   -0.929742,          ! elev
     >    0.0482885,         ! elev^2
     >    0.0,               ! NO CCF
     >    0.0297018,         ! diam
     >   -0.00058105,        ! diam^2
     >   -0.658208,          ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    0.0,               ! NO CROWN
     >    0.0,               ! NO BAL
     >    0.89852            ! sigmar
     >  ), 

     >  MD_STR (             ! LW - ICH+IDF
     >    (/ 2, (0, I=1,14) /),  ! spp
     >    (/ 'ICH/all', 'IDF/all', ('', I=1,28) /),
     >    739,               ! observations
     >   -0.1021,            ! constant
     >    0.0,               ! cos-asp
     >    0.6201,            ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.0365,            ! diam
     >   -0.00084,           ! diam^2
     >   -0.8330,            ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.3530,            ! crown
     >    0.0,               ! bal
     >    0.55480            ! sigmar
     >  ),

     >  MD_STR (             ! FD,OC - ICH 18+21m 1-30
     >    (/ 3,14, (0, I=1,13) /),  ! spp
     >    (/        'ICHdk/02',        'ICHdk/04',       'ICHmk3/02',
     >             'ICHmk3/03',       'ICHmk3/06',       'ICHwk2/02',
     >             'ICHwk2/03',       'ICHwk2/04',       'ICHwk4/02',
     >             'ICHwk4/03',       'ICHwk4/04',       'ICHwk4/05',
     >             'ICHmk1/01',    'ICHmk1/01-ys',       'ICHmk1/02',
     >             'ICHmk1/03',       'ICHmk1/04',       'ICHmk1/05',
     >             'ICHmk2/01',       'ICHmk2/02',       'ICHmk2/03',
     >             'ICHmk2/04',       'ICHmk2/05',       'ICHmw2/02',
     >             'ICHmw2/03',       'ICHmw3/01',    'ICHmw3/01-yc',
     >             'ICHmw3/02',       'ICHmw3/03',       'ICHmw3/04'/),
     >    1683,              ! observations
     >   -0.9496,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.0674,            ! diam
     >   -0.00095,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.5869,            ! crown
     >   -0.0206,            ! bal
     >    0.77801            ! sigmar
     >  ),
     >
     >  MD_STR (             ! FD,OC - ICH 18+21m 31-40
     >    (/ 3,14, (0, I=1,13) /),  ! spp  !!! syntax tweak
     >    (/       'ICHmw3/05',       'ICHwk1/03',       'ICHdw/01a',
     >              'ICHdw/02',       'ICHmw1/02',       'ICHmw1/03',
     >             'ICHmw1/04',       'ICHmw2/03',       'ICHmw2/04',
     >              'ICHxw/01',         'ICH/all', 
     >              ('', I=1,19)/), !!! syntax tweak
     >    1683,              ! observations
     >   -0.9496,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.0674,            ! diam
     >   -0.00095,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.5869,            ! crown
     >   -0.0206,            ! bal
     >    0.77801            ! sigmar
     >  ),

     >  MD_STR (             ! FD,OC - ICH 24m 1-29
     >    (/ 3,14, (0, I=1,13)  /),  ! spp
     >    (/        'ICHdk/01',        'ICHdk/05',       'ICHmk3/01',
     >             'ICHmk3/04',       'ICHwk2/01',       'ICHwk2/07',
     >             'ICHwk4/01',       'ICHwk4/07',       'ICHmk1/06',
     >             'ICHmw2/01',    'ICHmw2/01-ys',       'ICHmw2/04',
     >             'ICHmw2/05',       'ICHmw3/06',       'ICHmw3/07',
     >             'ICHvk1/01',       'ICHvk1/02',       'ICHvk1/03',
     >             'ICHvk1/04',       'ICHwk1/01',       'ICHwk1/04',
     >             'ICHwk1/05',       'ICHdw/01b',        'ICHdw/03',
     >              'ICHdw/04',       'ICHmw1/01',       'ICHmw1/05',
     >             'ICHmw1/06',       'ICHmw2/06',     ('',I=1,1) /),
     >    1367,              ! observations
     >   -1.4219,            ! constant
     >    0.0   ,            ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0   ,            ! ccf
     >    0.0802,            ! diam
     >   -0.00104,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    2.1392,            ! crown
     >   -0.0216,            ! bal
     >    0.69721            ! sigmar
     >  ),

     >  MD_STR (             ! FD,OC - IDF 9m+12m SI 1-23 (+4)
     >    (/ 3, 14, (0, I=1,13)  /), ! spp
     >    (/       'IDFdk3/02',       'IDFdk3/03',       'IDFdk3/04',
     >             'IDFdk4/01',       'IDFdk4/02',       'IDFdk4/03',
     >             'IDFdk4/04',       'IDFdk4/05',       'IDFdk4/07',
     >              'IDFxm/02',        'IDFxm/03',        'IDFxm/04',
     >              'IDFxw/02',        'IDFxw/03',        'IDFxw/04',
     >             'IDFdk1/02',       'IDFdk2/02',       'IDFdm1/03',
     >              'IDFww/02',       'IDFxh1/02',       'IDFxh1/03',
     >             'IDFxh2/02',       'IDFxh2/03',
     >             'IDFdk2/01',       'IDFdk2/03',    ! +2 from SI 15m
     >             'IDFxh2/01',       'IDFxh2/04',    ! +2 from SI 15m
     >               'IDF/all',     ('', I=1,2) /),
     >    1217,              ! observations
     >    0.5596,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.198,             ! ccf
     >    0.0,               ! diam
     >   -0.00034,           ! diam^2
     >    0.0,               ! NO bal/dbh
     >   -0.1045,            ! bal/log(dbh+1)
     >    1.3653,            ! crown
     >    0.0,               ! bal
     >    0.7339             ! sigmar
     >  ),

! OLD FORM !
c     >  MD_STR (             ! FD,OC - IDF 12m SI 1-23 (+4)
c     >    (/ 3, 14, 12*0 /), ! spp
c     >    (/       'IDFdk3/02',       'IDFdk3/03',       'IDFdk3/04',
c     >             'IDFdk4/01',       'IDFdk4/02',       'IDFdk4/03',
c     >             'IDFdk4/04',       'IDFdk4/05',       'IDFdk4/07',
c     >              'IDFxm/02',        'IDFxm/03',        'IDFxm/04',
c     >              'IDFxw/02',        'IDFxw/03',        'IDFxw/04',
c     >             'IDFdk1/02',       'IDFdk2/02',       'IDFdm1/03',
c     >              'IDFww/02',       'IDFxh1/02',       'IDFxh1/03',
c     >             'IDFxh2/02',       'IDFxh2/03',
c     >             'IDFdk2/01',       'IDFdk2/03',    ! +2 from SI 15m
c     >             'IDFxh2/01',       'IDFxh2/04',    ! +2 from SI 15m
c     >               'IDF/all'/),
c     >    889,               ! observations
c     >    0.0383,            ! constant
c     >    0.0,               ! cos-asp
c     >    0.0,               ! sin-asp
c     >    0.0,               ! elev
c     >    0.0,               ! elev^2
c     >   -0.1348,            ! ccf
c     >    0.0254,            ! diam
c     >   -0.00056,           ! diam^2
c     >    0.0,               ! NO bal/dbh
c     >    0.0,               ! bal/log(dbh+1)
c     >    1.0452,            ! crown
c     >   -0.0329,            ! bal
c     >    0.63577            ! sigmar
c     >  ),

     >  MD_STR (             ! FD,OC - IDF 15m SI 1-28 (-4)
     >    (/ 3, 14, (0, I=1,13)  /), ! spp
     >    (/       'IDFdk3/01',       'IDFdk3/05',       'IDFdk4/09',
     >              'IDFdw/01',        'IDFxm/01',        'IDFxm/05',
     >              'IDFxm/06',        'IDFxm/07',        'IDFxw/01',
     >              'IDFxw/05',       'IDFdk1/01',       'IDFdk1/03',
     >             'IDFdk1/04',       ! -2 moved to 12 (AAZ)
     >             'IDFdk3/06',       'IDFdm1/04',       'IDFmw1/02',
     >             'IDFmw2/02',       'IDFxh1/01',       'IDFxh1/04',
     >             'IDFxh1/05',       ! -2 moved to 12 (AAZ)
     >             'IDFxh2/05',       'IDFxh2/06',       'IDFdm2/01',
     >             'IDFdm2/03',    ('', I=1,6) /),
     >    1432,              ! observations
     >    0.2361,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.2418,            ! ccf
     >    0.0168,            ! diam
     >   -0.00033,           ! diam^2
     >    0.0,               ! NO bal/dbh
     >    0.0,               ! bal/log(dbh+1)
     >    1.5811,            ! crown
     >   -0.0191,            ! bal
     >    0.88944            ! sigmar
     >  ),

     >  MD_STR (             ! FD,OC - IDF 18+21+24m SI 1-30
     >    (/ 3, 14, (0, I=1,13)  /), ! spp
     >    (/       'IDFdk3/06',       'IDFdk3/07',       'IDFdk3/08',
     >              'IDFxm/08',        'IDFxw/06',        'IDFxw/07',
     >             'IDFdk1/05',       'IDFdk1/06',       'IDFdk2/04',
     >             'IDFdk2/05',       'IDFdk2/06',       'IDFdm1/01',
     >             'IDFdm1/05',       'IDFdm1/06',       'IDFdm1/07',
     >             'IDFmw1/01',    'IDFmw1/01-yc',       'IDFmw1/03',
     >             'IDFmw1/04',       'IDFmw1/05',       'IDFmw1/06',
     >             'IDFmw2/01',       'IDFmw2/03',       'IDFmw2/04',
     >              'IDFww/01',        'IDFww/03',        'IDFww/04',
     >              'IDFww/05',        'IDFww/06',       'IDFxh1/06'/),
     >    2767,              ! observations
     >   -0.9981,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.0817,            ! ccf
     >    0.0435,            ! diam
     >   -0.00053,           ! diam^2
     >    0.0,               ! NO bal/dbh
     >    0.0,               ! bal/log(dbh+1)
     >    2.2233,            ! crown
     >   -0.0206,            ! bal
     >    0.74424            ! sigmar
     >  ),

     >  MD_STR (             ! FD,OC - IDF 18+21m+24m SI 31-37
     >    (/ 3, 14, (0, I=1,13) /), ! spp
     >    (/       'IDFxh1/07',       'IDFxh1/08',       'IDFxh2/07',
     >             'IDFxh2/08',       'IDFdm2/04',       'IDFdm2/05',
     >             'IDFdm2/07',   ('', I=1,23) /),
     >    2767,              ! observations
     >   -0.9981,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.0817,            ! ccf
     >    0.0435,            ! diam
     >   -0.00053,           ! diam^2
     >    0.0,               ! NO bal/dbh
     >    0.0,               ! bal/log(dbh+1)
     >    2.2233,            ! crown
     >   -0.0206,            ! bal
     >    0.74424            ! sigmar
     >  ),

     >  MD_STR (             ! HW - ICH+IDF
     >    (/ 5, (0, I=1,14)  /),     ! spp
     >    (/ 'ICH/all', 'IDF/all', ('', I=1,28)  /),
     >    959,               ! observations
     >   -0.3410,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.1307,            ! ccf
     >    0.0605,            ! diam
     >   -0.00087,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.5816,            ! crown
     >   -0.0119,            ! bal
     >    0.88775            ! sigmar
     >  ),

     >  MD_STR (             ! CW - ICH
     >    (/ 6, (0, I=1,14) /),     ! spp
     >    (/ 'ICH/all', ('', I=1,29) /),
     >    2756,              ! observations
     >   -0.1650,            ! constant
     >    0.2763,            ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.1820,            ! ccf
     >    0.0372,            ! diam
     >   -0.00037,           ! diam^2
     >   -0.0884,            ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.4487,            ! crown
     >    0.0,               ! bal
     >    0.86261            ! sigmar
     >  ), 

     >  MD_STR (             ! CW - IDF
     >    (/ 6, (0, I=1,14)  /),     ! spp
     >    (/ 'IDF/all', ('', I=1,29)  /),
     >    357,               ! observations
     >    0.656272,          ! constant
     >   -1.027327,          ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.089231,          ! diam
     >   -0.0016866,         ! diam^2
     >    0.0,               ! NO bal/dbh
     >   -0.0432885,         ! bal/log(dbh+1)
     >    0.0,               ! crown
     >    0.0,               ! NO BAL
     >    0.88265            ! sigmar
     >    ),

     >  MD_STR (             ! PL - ICH
     >    (/ 7, (0, I=1,14)  /),     ! spp
     >    (/ 'ICH/all', ('', I=1,29) /),
     >    1313,              ! observations
     >    0.0,               ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.0209,            ! diam
     >   -0.00067,           ! diam^2
     >   -0.5552,            ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.8153,            ! crown
     >    0.0,               ! bal
     >    0.72326            ! sigmar
     >  ),

c     >  MD_STR (             ! PL - IDF
c     >    (/ 7, 14*0 /),     ! spp
c     >    (/ 'IDF/all' /),
c     >    1933,              ! observations
c     >    0.0,               ! constant
c     >    0.0,               ! cos-asp
c     >    0.0,               ! sin-asp
c     >    0.0,               ! elev
c     >    0.0,               ! elev^2
c     >   -0.3642,            ! ccf
c     >    0.0,               ! diam
c     >   -0.00032,           ! diam^2
c     >    0.0,               ! NO bal/dbh
c     >   -0.0610,            ! bal/log(dbh+1)
c     >    1.6986,            ! crown
c     >    0.0,               ! NO BAL
c     >    0.62040            ! sigmar
c     >  ),

     >  MD_STR (             ! PL - IDF
     >    (/ 7, (0, I=1,14)  /),     ! spp
     >    (/ 'IDF/all', ('', I=1,29)  /),
     >    2009,              ! observations
     >   -0.2238,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.3124,            ! ccf
     >    0.00993,           ! diam
     >   -0.00038,           ! diam^2
     >    0.0,               ! NO bal/dbh
     >   -0.0433,            ! bal/log(dbh+1)
     >    1.5667,            ! crown
     >    0.0,               ! NO BAL
     >    0.6249             ! sigmar
     >  ),

     >  MD_STR (             ! SE - ICH
     >    (/ 8, (0, I=1,14)  /),     ! spp
     >    (/ 'ICH/all', ('', I=1,29)  /),
     >    424,               ! observations
     >   -0.5766,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.0825,            ! ccf
     >    0.0196,            ! diam
     >   -0.00028,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.9456,            ! crown
     >   -0.00866,           ! bal
     >    0.87288            ! sigmar
     >  ),

     >  MD_STR (             ! SE - IDF
     >    (/ 8, (0, I=1,14)  /),     ! spp
     >    (/ 'IDF/all', ('', I=1,29)  /),
     >    102,               ! observations
     >   -0.8694668,         ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.0214183,         ! diam
     >   -0.00090472,        ! diam^2
     >    0.0,               ! NO bal/dbh
     >   -0.1589753,         ! bal/log(dbh+1)
     >    3.3882046,         ! crown
     >    0.0,               ! NO BAL
     >    0.84611            ! sigmar
     >  ),

     >  MD_STR (             ! BL,BG - ICH+IDF
     >    (/ 9, 4, (0, I=1,13)  /),  ! spp
     >    (/ 'ICH/all', 'IDF/all', ('', I=1,28)  /),
     >    350,               ! observations
     >    0.6265,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -1.0897,            ! ccf
     >    0.0515,            ! diam
     >   -0.00084,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.5198,            ! crown
     >    0.0,               ! bal
     >    0.92423            ! sigmar
     >  ),

     >  MD_STR (             ! PY - IDF+ICH
     >    (/ 10, (0, I=1,14)  /),    ! spp
     >    (/ 'ICH/all', 'IDF/all', ('', I=1,28)  /),
     >    450,               ! observations
     >   -0.50298,           ! constant
     >   -0.613773,          ! cos-asp
     >    0.596886,          ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! NO CCF
     >    0.0862013,         ! diam
     >   -0.00115276,        ! diam^2
     >   -0.609809,          ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    0.0,               ! NO CROWN
     >    0.0,               ! NO BAL
     >    0.86319            ! sigmar
     >  ),

     >  MD_STR (             ! EP,OH - ICH
     >    (/11,15, (0, I=1,13) /),  ! spp
     >    (/ 'ICH/all', ('', I=1,29)  /),
     >    835,               ! observations
     >   -3.6197,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.7542,            ! elev
     >   -0.0425,            ! elev^2
     >   -0.2492,            ! ccf
     >    0.0740,            ! diam
     >   -0.00116,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.1239,            ! crown
     >   -0.0198,            ! bal
     >    0.68942            ! sigmar
     >  ), 

     >  MD_STR (             ! EP,OH - IDF
     >    (/11,15, (0, I=1,13)  /),  ! spp
     >    (/ 'IDF/all', ('', I=1,29)  /),
     >    418,               ! observations
     >    0.0,               ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    1.1315,            ! diam
     >   -0.0012,            ! diam^2
     >    0.3599,            ! additive bal/dbh
     >   -0.2306,            ! multiplicative bal/dbh
     >    0.0,               ! crown
     >    0.0,               ! NO BAL
     >    0.78715            ! sigmar
     >  ),
C
C       ASPEN MODEL HAS DIFFERENT STRUCTURE...
C
     >  MD_STR (             ! AT,CT - IDF+ICH
     >    (/ 12,13, (0, I=1,13)  /), ! spp
     >    (/ 'ICH/all', 'IDF/all', ('', I=1,28)  /),
     >    727,               ! observations
     >    0.0,               ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! NO CCF
     >    0.1372,            ! diam
     >   -0.0015,            ! diam^2
     >    1.0768,            ! additive bal/dbh
     >   -0.1983,            ! multiplicative bal/dbh
     >    0.0,               ! NO CROWN
     >    0.0,               ! NO BAL
     >    0.78243            ! sigmar
     >  ),

C     
C       SBS and SBPS zones...
C

     >  MD_STR (             ! Pl SBS
     >    (/7, (0, I=1,14) /),  ! spp
     >    (/ 'SBSdw1/01', 'SBSdw2/01', 'SBSmc1/01',
     >       'SBSmc2/01',  'SBSmh/01',  'SBSmm/01',
     >        'SBSmw/01', 'SBSwk1/01', ('', I=1,22)  /),
     >    404,               ! observations
     >   -1.2421,            ! constant
     >    0.0,               ! cos-asp
     >    0.00955,           ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >    0.0,               ! ccf
     >    0.0630,            ! diam                     KB?
     >   -0.0012,            ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.6043,            ! crown
     >   -0.0202,            ! bal
     >    0.419              ! sigmar
     >  ), 

     >  MD_STR (             ! Pl SBPSdc 
     >    (/7, (0, I=1,14) /),  ! spp
     >    (/ 'SBPSdc/01', 'SBPSxc/01', ('', I=1,28)  /),
     >    38,                ! observations
     >   -4.1693,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.00832,           ! ccf
     >    2.4409,            ! diam
     >   -0.00565,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.3229,            ! crown
     >    0.0,               ! bal
     >    0.893              ! sigmar
     >  ), 

     >  MD_STR (             ! Pl SBPSmk
     >    (/7, (0, I=1,14) /),  ! spp
     >    (/ 'SBPSmc/01', 'SBPSmk/01', ('', I=1,28)  /), 
     >    28,                ! observations
     >   -0.9005,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.00414,           ! ccf
     >    0.7488,            ! diam
     >   -0.00075,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    1.3599,            ! crown
     >    0.0,               ! bal
     >    0.314              ! sigmar
     >  ) /

       DATA (ZNKONST(I), I=25,P_ZN) /

     >  MD_STR (             ! AT - SBPSmk, SBPSdc
     >    (/12, 15, (0, I=1,13)  /),  ! spp                KB?
     >    (/ 'SBPSmc/01',   'SBPSmk/01',    'SBPSdc/01',
     >       'SBPSxc/01',   'SBSdw1/01',    'SBSdw2/01',
     >       'SBSmc1/01',   'SBSmc2/01',     'SBSmh/01',
     >        'SBSmm/01',   'SBSmw/01',     'SBSwk1/01',
     >         'SBS/all',   'SBPS/all', ('', I=1,16) /), ! KB?
     >    27,                ! observations
     >    1.1487,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.00310,           ! ccf
     >   -0.0668,            ! diam
     >    0.0,               ! diam^2
     >    0.0,               ! additive bal/dbh
     >    0.0,               ! multiplicative bal/dbh
     >    0.0,               ! crown
     >    0.0,               ! bal
     >    0.311              ! sigmar
     >  ),

     >  MD_STR (             ! Bl SBS
     >    (/9, (0, I=1,14) /),  ! spp
     >    (/ 'SBPSmc/01',  'SBPSmk/01',  'SBPSdc/01',
     >       'SBPSxc/01',  'SBSdw1/01',  'SBSdw2/01',
     >       'SBSmc1/01',  'SBSmc2/01',   'SBSmh/01',
     >        'SBSmm/01',  'SBSmw/01',   'SBSwk1/01',
     >    ('', I=1,18)/),
     >    0,                 ! observations            KB?
     >    0.0,               ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.0152,            ! ccf
     >    0.0737,            ! diam                    KB?
     >   -0.00182,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    3.1925,            ! crown
     >    0.0,               ! bal
     >    0                  ! sigmar                  KB?
     >  ), 

     >  MD_STR (             ! Sx SBSdw1, SBSdw2, SBSmh
     >    (/8, (0, I=1,14) /),  ! spp
     >    (/ 'SBPSmc/01',   'SBPSmk/01',    'SBPSdc/01',
     >       'SBPSxc/01',   'SBSdw1/01',    'SBSdw2/01',
     >       'SBSmc1/01',   'SBSmc2/01',     'SBSmh/01',
     >        'SBSmm/01',    'SBSmw/01',    'SBSwk1/01',
     >    ('', I=1,18)/),
     >    501,               ! observations
     >    0.9159,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.00397,           ! ccf
     >    0.00620,           ! diam
     >   -0.00009,           ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    0.5586,            ! crown
     >    0.0,               ! bal
     >    0.786              ! sigmar
     >  ), 

     >  MD_STR (             ! Fd SBSdw1, SBSdw2, SBSmh
     >    (/3, 14, (0, I=1,13) /),  ! spp		          KB?
     >    (/ 'SBPSmc/01',   'SBPSmk/01',    'SBPSdc/01',
     >       'SBPSxc/01',   'SBSdw1/01',    'SBSdw2/01',
     >       'SBSmc1/01',   'SBSmc2/01',     'SBSmh/01',
     >        'SBSmm/01',    'SBSmw/01',    'SBSwk1/01',
     >         'SBS/all',    'SBPS/all', ('', I=1,16)/), !KB?
     >    568,               ! observations
     >    1.0901,            ! constant
     >    0.0,               ! cos-asp
     >    0.0,               ! sin-asp
     >    0.0,               ! elev
     >    0.0,               ! elev^2
     >   -0.00403,           ! ccf
     >    0.0144,            ! diam
     >   -0.0001,            ! diam^2
     >    0.0,               ! bal/dbh
     >    0.0,               ! NO bal/log(dbh+1)
     >    0.05062,           ! crown
     >   -0.00858,           ! bal
     >    0.855              ! sigmar
     >  ) /

C     SITE SERIES COEFFICIENTS

      DATA (SSKONST(I), I=1,P_SS) / 

     >  SS_STR (  ! ICH+IDF PW (zonal model: no site index)
     >    (/ 1, (0, I=1,14)  /),
     >    (/         'ICH/all',         'IDF/all', ('', I=1,28) /),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH+IDF LW (zonal model; rare in IDF: no site index)
     >    (/ 2, (0, I=1,14)  /),
     >    (/         'ICH/all',         'IDF/all', ('', I=1,28) /),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH+IDF FD,OC (model coeffs incorporate SIBEC)
     >    (/ 3,14, (0, I=1,13) /),
     >    (/         'ICH/all',         'IDF/all', ('', I=1,28) /),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH HW 15m 1-18 (should be absent in IDF)
     >    (/ 5, (0, I=1,14)  /),
     >    (/       'ICHmw2/02',       'ICHmw2/06',       'ICHmw2/07',
     >             'ICHmw3/08',       'ICHvk1/05',       'ICHvk1/06',
     >             'ICHwk1/02',       'ICHwk1/03',       'ICHwk1/06',
     >             'ICHwk1/07',       'ICHmw1/02',       'ICHmw1/03',
     >             'ICHmw1/04',       'ICHmw1/07',       'ICHmw2/03',
     >             'ICHmw2/08',       'ICHmw3/04',       'ICHwk1/08',
     >               'ICH/all',          'IDF/all',  ('', I=1,10) /),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH HW 18m 1-28
     >    (/ 5, (0, I=1,14)  /),
     >    (/       'ICHmw2/01',    'ICHmw2/01-ys',       'ICHmw2/03',
     >             'ICHmw2/04',       'ICHmw2/05',       'ICHmw3/01',
     >          'ICHmw3/01-yc',       'ICHmw3/05',       'ICHmw3/06',
     >             'ICHmw3/07',       'ICHvk1/01',       'ICHvk1/02',
     >             'ICHvk1/03',       'ICHvk1/04',       'ICHwk1/01',
     >             'ICHwk1/04',       'ICHwk1/05',       'ICHdw/01a',
     >             'ICHdw/01b',        'ICHdw/02',        'ICHdw/03',
     >              'ICHdw/04',       'ICHmw1/01',       'ICHmw1/05',
     >             'ICHmw1/06',       'ICHmw2/05',       'ICHmw2/06',
     >              'ICHxw/01',     ('', I=1,2)/),
     >   -0.0800
     >    ),

     >  SS_STR (  ! ICH CW 15m 1-28
     >    (/ 6, (0, I=1,14)  /),
     >    (/       'ICHmk1/01',    'ICHmk1/01-ys',       'ICHmk1/04',
     >             'ICHmk1/07',       'ICHmk2/01',       'ICHmk2/04',
     >             'ICHmk2/06',       'ICHmw2/02',       'ICHmw3/01',
     >          'ICHmw3/01-yc',       'ICHmw3/03',       'ICHmw3/04',
     >             'ICHmw3/05',       'ICHmw3/08',       'ICHvk1/05',
     >             'ICHvk1/06',       'ICHwk1/02',       'ICHwk1/03',
     >             'ICHwk1/06',       'ICHwk1/07',       'ICHdw/01a',
     >             'ICHmw1/02',       'ICHmw1/03',       'ICHmw1/04',
     >             'ICHmw1/07',       'ICHmw2/03',       'ICHwk1/08',
     >              'ICHxw/01',         'ICH/all',     ('', I=1,1)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH CW 18m 1-25
     >    (/ 6, (0, I=1,14) /),
     >    (/       'ICHmk1/05',       'ICHmk1/06',       'ICHmk2/05',
     >             'ICHmw2/01',    'ICHmw2/01-ys',       'ICHmw2/03',
     >             'ICHmw2/04',       'ICHmw2/05',       'ICHmw3/06',
     >             'ICHmw3/07',       'ICHvk1/01',       'ICHvk1/03',
     >             'ICHvk1/04',       'ICHwk1/01',       'ICHwk1/04',
     >             'ICHwk1/05',       'ICHdw/01b',        'ICHdw/02',
     >              'ICHdw/03',        'ICHdw/04',       'ICHmw1/01',
     >             'ICHmw1/05',       'ICHmw1/06',       'ICHmw2/04',
     >             'ICHmw2/06',     ('', I=1,5)/),
     >    0.0466
     >    ),

     >  SS_STR (  ! IDF CW (zonal model: no site index)
     >    (/ 6, (0, I=1,14)  /),
     >    (/       'IDF/all', ('', I=1,29)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH PL 21m 1-30
     >    (/ 7, (0, I=1,14)  /),
     >    (/        'ICHdk/02',        'ICHdk/03',        'ICHdk/04',
     >             'ICHmk3/02',       'ICHmk3/03',       'ICHmk3/05',
     >             'ICHmk3/06',       'ICHmk3/07',       'ICHwk2/02',
     >             'ICHwk2/03',       'ICHwk2/04',       'ICHwk2/05',
     >             'ICHwk2/06',       'ICHwk2/07',       'ICHwk4/02',
     >             'ICHwk4/03',       'ICHwk4/04',       'ICHwk4/05',
     >             'ICHwk4/06',       'ICHwk4/07',       'ICHmk1/01',
     >          'ICHmk1/01-ys',       'ICHmk1/02',       'ICHmk1/03',
     >             'ICHmk1/04',       'ICHmk1/05',       'ICHmk1/07',
     >             'ICHmk2/01',       'ICHmk2/02',       'ICHmk2/03'/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH PL 21m 31-50
     >    (/ 7, (0, I=1,14)  /),
     >    (/       'ICHmk2/04',       'ICHmk2/06',       'ICHmw2/02',
     >             'ICHmw3/01',    'ICHmw3/01-yc',       'ICHmw3/02',
     >             'ICHmw3/03',       'ICHmw3/04',       'ICHmw3/05',
     >             'ICHwk1/02',       'ICHwk1/03',       'ICHwk1/04',
     >             'ICHdw/01a',        'ICHdw/02',       'ICHmw1/02',
     >             'ICHmw1/03',       'ICHmw1/04',       'ICHmw1/07',
     >             'ICHmw2/03',        'ICHxw/01',         'ICH/all',
     >           ('', I=1,9)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH PL 24m 1-22
     >    (/ 7, (0, I=1,14)  /),
     >    (/        'ICHdk/01',        'ICHdk/05',       'ICHmk3/01',
     >             'ICHmk3/04',       'ICHwk2/01',       'ICHwk4/01',
     >             'ICHmk1/06',       'ICHmk2/05',       'ICHmw2/01',
     >          'ICHmw2/01-ys',       'ICHmw2/03',       'ICHmw2/04',
     >             'ICHmw2/05',       'ICHmw3/06',       'ICHwk1/01',
     >             'ICHdw/01b',        'ICHdw/03',        'ICHdw/04',
     >             'ICHmw1/01',       'ICHmw1/05',       'ICHmw1/06',
     >             'ICHmw2/06',     ('', I=1,8)/),
     >    0.2625
     >    ),

     >  SS_STR (  ! IDF PL 15m 1-20
     >    (/ 7, (0, I=1,14)  /),
     >    (/       'IDFdk3/02',       'IDFdk3/03',       'IDFdk3/05',
     >             'IDFdk4/01',       'IDFdk4/02',       'IDFdk4/05',
     >             'IDFdk4/06',       'IDFdk4/07',       'IDFdk4/08',
     >             'IDFdk4/09',        'IDFxm/03',       'IDFdk1/02',
     >             'IDFdk1/03',       'IDFdk1/04',       'IDFdk3/06',
     >             'IDFdm1/03',       'IDFdm1/04',       'IDFmw1/03',
     >              'IDFww/02',       'IDFdm2/03',         'IDF/all',
     >           ('', I=1,9)/),
     >    0.0
     >    ),

     >  SS_STR (  ! IDF PL 18m 1-18
     >    (/ 7, (0, I=1,14) /),
     >    (/       'IDFdk3/01',       'IDFdk3/06',       'IDFdk3/07',
     >             'IDFdk3/08',       'IDFdk3/09',        'IDFdw/01',
     >             'IDFdk1/01',       'IDFdk2/01',       'IDFdk2/03',
     >             'IDFdk2/04',       'IDFdm1/01',       'IDFmw1/04',
     >             'IDFmw2/02',       'IDFmw2/05',        'IDFww/03',
     >             'IDFdm2/01',       'IDFdm2/04',       'IDFdm2/07',
     >          ('', I=1,12)/),
     >    0.0
     >    ),

     >  SS_STR (  ! IDF PL 21m 1-17
     >    (/ 7, (0, I=1,14)  /),
     >    (/       'IDFdk1/05',       'IDFdk1/06',       'IDFdk2/05',
     >             'IDFdk2/06',       'IDFdk2/07',       'IDFdm1/05',
     >             'IDFdm1/06',       'IDFdm1/07',       'IDFmw1/01',
     >          'IDFmw1/01-yc',       'IDFmw1/05',       'IDFmw1/06',
     >             'IDFmw2/01',       'IDFmw2/03',       'IDFmw2/04',
     >              'IDFww/01',       'IDFdm2/05',   ('', I=1,13) /),
     >    0.3479
     >    ),

     >  SS_STR (  ! ICH SE 18m 1-30
     >    (/ 8, (0, I=1,14)  /),
     >    (/       'ICHwk2/04',       'ICHwk4/04',       'ICHwk4/05',
     >             'ICHwk4/08',       'ICHmk1/01',    'ICHmk1/01-ys',
     >             'ICHmk1/03',       'ICHmk1/04',       'ICHmk1/07',
     >             'ICHmk2/01',       'ICHmk2/03',       'ICHmk2/04',
     >             'ICHmk2/06',       'ICHmw2/01',    'ICHmw2/01-ys',
     >             'ICHmw2/02',       'ICHmw2/03',       'ICHmw2/07',
     >             'ICHmw3/01',    'ICHmw3/01-yc',       'ICHmw3/03',
     >             'ICHmw3/04',       'ICHmw3/05',       'ICHmw3/08',
     >             'ICHvk1/06',       'ICHwk1/03',       'ICHwk1/07',
     >             'ICHmw1/02',       'ICHmw1/03',       'ICHmw1/04'/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH SE 18m 31-35
     >    (/ 8, (0, I=1,14)  /),
     >    (/       'ICHmw1/07',       'ICHmw2/03',       'ICHmw2/04',
     >             'ICHmw2/08',       'ICHwk1/08',         'ICH/all',
     >          ('', I=1,24)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH SE 21m 1-30
     >    (/ 8, (0, I=1,14)  /),
     >    (/        'ICHdk/01',        'ICHdk/04',        'ICHdk/05',
     >             'ICHmk3/01',       'ICHmk3/04',       'ICHmk3/05',
     >             'ICHmk3/06',       'ICHmk3/07',       'ICHwk2/01',
     >             'ICHwk2/05',       'ICHwk2/06',       'ICHwk2/07',
     >             'ICHwk2/08',       'ICHwk4/01',       'ICHwk4/06',
     >             'ICHwk4/07',       'ICHmk1/05',       'ICHmk1/06',
     >             'ICHmk2/05',       'ICHmw2/04',       'ICHmw2/05',
     >             'ICHmw2/06',       'ICHmw3/06',       'ICHmw3/07',
     >             'ICHvk1/01',       'ICHvk1/02',       'ICHvk1/03',
     >             'ICHvk1/04',       'ICHvk1/05',       'ICHwk1/01'/),
     >    0.1750
     >    ),

     >  SS_STR (  ! ICH SE 21m 31-41
     >    (/ 8, (0, I=1,14)  /),
     >    (/       'ICHwk1/04',       'ICHwk1/05',       'ICHwk1/06',
     >             'ICHdw/01b',        'ICHdw/03',        'ICHdw/04',
     >             'ICHmw1/01',       'ICHmw1/05',       'ICHmw1/06',
     >             'ICHmw2/07',       'ICHwk1/07',    ('', I=1,19)/),
     >    0.1750
     >    ),

     >  SS_STR (  ! IDF SE (zonal model: no site index)
     >    (/ 8, (0, I=1,14) /),
     >    (/       'IDF/all', ('', I=1,29)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH BL,BG 15m 1-9
     >    (/ 9, 4, (0, I=1,13)  /),
     >    (/       'ICHmk1/03',       'ICHmk2/03',       'ICHmw2/02',
     >             'ICHmw3/03',       'ICHmw3/08',       'ICHvk1/06',
     >             'ICHwk1/02',       'ICHmw1/02',       'ICHmw2/03',
     >               'ICH/all',    ('', I=1,20)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH BL,BG 18m 1-29
     >    (/ 9, 4, (0, I=1,13) /),
     >    (/       'ICHmk1/01',    'ICHmk1/01-ys',       'ICHmk1/04',
     >             'ICHmk1/07',       'ICHmk2/01',       'ICHmk2/06',
     >             'ICHmw2/01',    'ICHmw2/01-ys',       'ICHmw2/03',
     >             'ICHmw2/06',       'ICHmw2/07',       'ICHmw3/01',
     >          'ICHmw3/01-yc',       'ICHmw3/05',       'ICHvk1/02',
     >             'ICHvk1/03',       'ICHvk1/05',       'ICHwk1/03',
     >             'ICHwk1/04',       'ICHwk1/06',       'ICHwk1/07',
     >             'ICHdw/01a',       'ICHmw1/01',       'ICHmw1/03',
     >             'ICHmw1/04',       'ICHmw1/07',       'ICHmw2/04',
     >             'ICHmw2/08',       'ICHwk1/08',     ('', I=1,1)/),
     >    0.0807
     >    ),

     >  SS_STR (  ! ICH BL,BG 21m 1-18
     >    (/ 9, 4, (0, I=1,13) /),
     >    (/       'ICHmk1/05',       'ICHmk1/06',       'ICHmk2/05',
     >             'ICHmw2/04',       'ICHmw2/05',       'ICHmw3/06',
     >             'ICHmw3/07',       'ICHvk1/01',       'ICHvk1/04',
     >             'ICHwk1/01',       'ICHwk1/05',       'ICHdw/01b',
     >              'ICHdw/03',        'ICHdw/04',       'ICHmw1/05',
     >             'ICHmw1/06',       'ICHmw2/05',       'ICHmw2/06',
     >          ('', I=1,12)/),
     >    0.2422
     >    ),

     >  SS_STR (  ! IDF BL,BG 1-5 (zonal model: use ICH SI 15)
     >    (/ 9, 4, (0, I=1,13) /),
     >    (/       'IDF/all', ('', I=1,29)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH+IDF PY 1-30 (zonal model: no site index)
     >    (/10, (0, I=1,14) /),
     >    (/       'IDF/all',         'ICH/all', ('', I=1,28)/),
     >    0.0
     >    ),

     >  SS_STR (  ! ICH+IDF EP,OH,AT,AC (zonal model for all hardwoords)
     >    (/11, 12, 13, 15, (0, I=1,11)  /),
     >    (/       'IDF/all',         'ICH/all', ('', I=1,28) /),
     >    0.0
     >    )

     >      /
      CALL DBCHK (DEBUG,'DGF',3,ICYC)

C     V2: SCALE BASAL AREA; FIND SEICCF TERMS
      IF (LV2ATV) THEN      
        BA100 = BA/100.
        CALL SEILTDG (BECADJ,SEICN2,
     &    SEISLP,SEISAS,SEICAS,SEIELV,SEIEL2,SEICCF,LSPPOK)
      ENDIF

C     BEGIN SPECIES LOOP. ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

      DO 20 ISPC = 1,MAXSP

        I1=ISCT(ISPC,1)
        IF(I1.EQ.0) GO TO 20
        I2=ISCT(ISPC,2)
        
        IF (LV2ATV) THEN
        
          IF (.NOT.LSPPOK(ISPC)) SEICCF(ISPC) = 0.0 
          CONSPP = DGCON(ISPC) + COR(ISPC) +
     &        0.01 * (DGCCF(ISPC) + SEICCF(ISPC)) * RELDEN
     
          DGLDS  = DGLD(ISPC)
          DGBALS = DGBAL(ISPC)
          DGCRS  = DGCR(ISPC)
          DGCRS2 = DGCRSQ(ISPC)
          DGDSQS = DGDSQ(ISPC)
          DGDBLS = DGDBAL(ISPC)
          
        ELSE

C         HW MODEL IN ICH MUST HAVE AT CCF>=100
C         BL MODEL IN ICH MUST HAVE AT CCF>=125
C         EP/OH MODEL IN IDF MUST HAVE AT CCF>=100
C         PL MODEL IN IDF MUST HAVE AT CCF>=100

          RELDN2 = RELDEN
          SELECT CASE (ISPC)
            CASE (5)
              IF (INDEX(BEC%Zone,'ICH') .GT. 0)
     >          RELDN2 = MAX(100.0, RELDEN)
            CASE (7)
              IF (INDEX(BEC%Zone,'IDF') .GT. 0)
     >          RELDN2 = MAX(100.0, RELDEN)
            CASE (9)
              IF (INDEX(BEC%Zone,'ICH') .GT. 0)
     >          RELDN2 = MAX(125.0, RELDEN)
            CASE (11,15)
              IF ((INDEX(BEC%Zone,'ICH') .GT. 0) .OR.
     >            (INDEX(BEC%Zone,'IDF') .GT. 0))
     >          RELDN2 = MAX(100.0, RELDEN)
          END SELECT
          CONSPP = DGCON(ISPC) + COR(ISPC) +
     >      DGCCF1(ISPC) * RELDN2 * 0.01

        ENDIF

C       BEGIN TREE LOOP WITHIN SPECIES ISPC.
        DO 10 I3 = I1,I2
          I = IND1(I3)
          IF (DIAM(I).LE.0.0) GOTO 10        
          
          CR  = ICR(I) * 0.01
          IF (LV2ATV) THEN
            D   = DIAM(I)   
            ALD = ALOG(D)
            BAL = (1.0 - (PCT(I)/100.)) * BA100            
          ELSE
            D = DIAM(I) * INtoCM
            BAL = (1.0 - (PCT(I)/100.)) * BA * FT2pACRtoM2pHA
            BALD1 = BAL/D
            BALD2 = BAL/LOG(D+1.0)

C           HW MODEL IN ICH MUST HAVE CROWN<=0.80
C           EP/OH MODEL MUST HAVE CROWN<=0.80
            SELECT CASE (ISPC)
              CASE (5)
                IF (INDEX(BEC%Zone,'ICH') .GT. 0)
     >            CR = MIN(0.8, CR)
              CASE (11,15)
                IF ((INDEX(BEC%Zone,'ICH') .GT. 0.) .OR.
     >              (INDEX(BEC%Zone,'IDF') .GT. 0))
     >            CR = MIN(0.8, CR)
            END SELECT
          ENDIF
            
          IF (LV2ATV) THEN

            DDS = CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)
     &           +DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))

          ELSE
          
            SELECT CASE (ISPC)

              ! new birch IDF model form in IDF; old form in ICH
	        CASE (11,15)
	          IF (INDEX(BEC%Zone,'IDF') .GT. 0) THEN
	            D2    = MAX(D,1.0)  ! CONSTRAIN TO 1 CM
                  BALD1 = BAL/D2
                  DDS   = DGLD1(ISPC) * (D2 **
     >              (DGDBAL1(ISPC) + DGDBAL2(ISPC) * BALD1)) * 
     >              EXP(DGDSQ1(ISPC) * D2 * D2)
	          ELSEIF (INDEX(BEC%Zone,'ICH') .GT. 0) THEN
                  DDS = CONSPP
     >              + DGLD1(ISPC)   * D
     >              + DGDSQ1(ISPC)  * D*D
     >              + DGBAL1(ISPC)  * BAL
     >              + DGDBAL1(ISPC) * BALD1
     >              + DGDBAL2(ISPC) * BALD2
     >              + DGCR1(ISPC)   * CR
                  DDS = EXP(MAX(-9.21,DDS))
	          ENDIF

	        ! new aspen model form
	        CASE (12,13)
                D2    = MAX(D,1.0)  ! CONSTRAIN TO 1 CM
                BALD1 = BAL/D2
                DDS   = DGLD1(ISPC) * (D2 **
     >            (DGDBAL1(ISPC) + DGDBAL2(ISPC) * BALD1)) * 
     >            EXP(DGDSQ1(ISPC) * D2 * D2)

	        CASE DEFAULT
                DDS = CONSPP
     >            + DGLD1(ISPC)    * D
     >            + DGDSQ1(ISPC)   * D*D
     >            + DGBAL1(ISPC)   * BAL
     >            + DGDBAL1(ISPC) * BALD1
     >            + DGDBAL2(ISPC) * BALD2
     >            + DGCR1(ISPC)    * CR
                DDS = EXP(MAX(-9.21,DDS))

	      END SELECT

C           CONVERT DG [CM] PREDICTION TO LN(DDS) [IN]
            DDS = (DDS*DDS + 2.0 * DDS * D * BRATIO(ISPC,D,0.0)) *
     >             CMtoIN * CMtoIN
	        
	    ENDIF
     
          DDS = MAX(0.001,DDS)  ! temporary: during SBS fit 
          DDS = LOG(DDS)

C         CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
C         FOR THE DENSITY OF NEIGHBORING STANDS.
          XPPDDS = 0.
          IF (LV2ATV) THEN
            CALL PPDGF (XPPDDS,BAL,RELDEN,D,DGCCF(ISPC)*.01,
     >        DGBALS,DGDBLS)
          ELSE
            CALL PPDGF (XPPDDS,BAL,RELDEN,D,DGCCF1(ISPC)*.01,
     >        DGBAL1(ISPC),DGDBAL1(ISPC))
          ENDIF
          DDS = DDS + XPPDDS
          DDS = MAX(-9.21,DDS)
          WK2(I) = DDS

C         END OF TREE LOOP. PRINT DEBUG INFO IF DESIRED.
          IF(.NOT.DEBUG) GO TO 10
          WRITE(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
 9001     FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &          ',  BAL=',F7.2,',  CR=',F7.4/
     &         '          CCF=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
   10   CONTINUE

C  END OF SPECIES LOOP.

   20 CONTINUE
      RETURN

      ENTRY DGCONS

C     ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C     MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C     ITYPE IS THE HABITAT TYPE INDEX AS COMPUTED IN **HABTYP**.
C     ASPECT IS STAND ASPECT IN RADIANS.  OBSERV CONTAINS THE NUMBER OF
C     OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
C     MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

C     ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

C     REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
C     STRING LENGTHS WILL BE FIGURED CORRECTLY AND STRING COMPARISONS
C     WILL WORK PROPERLY

      IF (LV2ATV) THEN

C       FETCH SEI VARIANT ADJUSTMENT TERMS
        CALL SEILTDG (BECADJ,SEICN2,
     &    SEISLP,SEISAS,SEICAS,SEIELV,SEIEL2,SEICCF,LSPPOK)

        DO I = 1,MAXSP
          ISPHAB = MAPHAB(ITYPE,I)
          ISPFOR = MAPLOC(IFOR,I)
          ISPDSQ = MAPDSQ(IFOR,I)
          ISPCCF = MAPCCF(ITYPE,I)

C         USE V2 SEI-PARAMETERIZATION IF IT EXISTS
          IF (LSPPOK(I)) THEN
            DGCON1(I)= DGHAB(ISPHAB,I) + DGFOR(ISPFOR,I)
C           ADD NI-AVERAGE SLOPE/ASPECT/ELEVATION USED DURING FITTING
     &        +  DGEL(I)    * BECADJ(3)
     &        +  DGEL2(I)   * BECADJ(3) * BECADJ(3)
     &        + (DGSASP(I)  * SIN(BECADJ(2))
     &        +  DGCASP(I)  * COS(BECADJ(2))
     &        +  DGSLOP(I)) * BECADJ(1)
     &        +  DGSLSQ(I)  * BECADJ(1) * BECADJ(1)

C           USE SEI-DERIVED TERMS
            DGCON2(I) = SEICN2(I)	
     &        +  SEIELV(I)  * ELEV
     &        +  SEIEL2(I)  * ELEV * ELEV
     &        + (SEISAS(I)  * SIN(ASPECT)
     &        +  SEICAS(I)  * COS(ASPECT)
     &        +  SEISLP(I)) * SLOPE

	      DGCON(I) = DGCON1(I) + DGCON2(I)
            V2SEICOR(I)= EXP(DGCON2(I))

          ELSE  ! OTHERWISE USE THE ORIGINAL NI MODEL
            DGCON(I)= DGHAB(ISPHAB,I)
     &      +  DGFOR(ISPFOR,I)
     &      +  DGEL(I)    * ELEV
     &      +  DGEL2(I)   * ELEV * ELEV
     &      + (DGSASP(I)  * SIN(ASPECT)
     &      +  DGCASP(I)  * COS(ASPECT)
     &      +  DGSLOP(I)) * SLOPE
     &      +  DGSLSQ(I)  * SLOPE * SLOPE
	      V2SEICOR(I) = 1.0
          ENDIF

          DGDSQ(I)=DGDS(ISPDSQ,I)
          DGCCF(I)=DGCCFA(ISPCCF,I)
          ATTEN(I)=OBSERV(ISPHAB,I)
          SMCON(I)=0.      ! error here...

        ENDDO
      
      ELSE
      
        DO J = 1,P_ZN
          DO K = 1,P_ST
            DO M = 1,P_ZL
              IF (ICHAR(ZNKONST(J)%Zone(K)(M:M)) .EQ. 0)
     >          ZNKONST(J)%Zone(K)(M:M) = ' '
            ENDDO
          ENDDO
        ENDDO

        DO J = 1,P_SS
          DO K = 1,P_ST
            DO M = 1,P_ZL
              IF (ICHAR(SSKONST(J)%PrettyName(K)(M:M)) .EQ. 0)
     >          SSKONST(J)%PrettyName(K)(M:M) = ' '
            ENDDO
          ENDDO
        ENDDO

C       MAKE TEMPORARY ZONE/SS MAPPING FOR SS IN THE KAMLOOPS ICHmw2.
C       THESE SS DO NOT NATURALLY CORRESPOND TO THOSE OF THE NELSON REGION,
C       AND ARE "INCREMENTED" BY ONE SS. 

        BEC2 = BEC
        IF (INDEX(BEC2%Region,'KAM') .GT. 0 .AND.
     >      INDEX(BEC2%FullName,'ICHmw2') .GT. 0 .AND.
     >      INDEX(BEC2%Series,'-') .EQ. 0) THEN
     
          READ (BEC2%Series,'(I4)') iSeries          
          SELECT CASE (iSeries)
            CASE (2)
              BEC2%PrettyName = "ICHmw2/03"
            CASE (3)
              BEC2%PrettyName = "ICHmw2/04"
            CASE (4)
              BEC2%PrettyName = "ICHmw2/05"
            CASE (5)
              BEC2%PrettyName = "ICHmw2/06"
            CASE (6)
              BEC2%PrettyName = "ICHmw2/07"
            CASE (7)
              BEC2%PrettyName = "ICHmw2/08"
          END SELECT
        ENDIF
        PAT2 = BEC2%PrettyName        

C       LOCATE ZONE USING "/all" PATTERN MATCH

        DO I = 1,MAXSP
          IPOS(I) = 0
          JPOS(I) = 0
        ENDDO

C       FIND INDEX TO BECZONE COEFFICIENTS -> IPOS()
        DO I = 1,MAXSP
          IPAS = 0
          PAT1 = PAT2
    7     DO J = 1,P_ZN
            DO K = 1,MAXSP
              IF (ZNKONST(J)%SPP(K) .EQ. 0) EXIT
              IF (ZNKONST(J)%SPP(K) .EQ. I) THEN
                DO M = 1, P_ST
                  IF (LEN_TRIM(ZNKONST(J)%Zone(M)) .EQ. 0) EXIT
                  IF (INDEX(PAT1,ZNKONST(J)%Zone(M)) .GT. 0) THEN
                    IPOS(I) = J
                    GOTO 8
                  ENDIF
                ENDDO
                EXIT
              ENDIF
            ENDDO
          ENDDO
          IF (J .GT. P_ZN .AND. IPAS .EQ. 0) THEN
            IPAS = 1
            PAT1 = BEC2%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
            GOTO 7
          ENDIF
    8     IF (IPOS(I) .EQ. 0) GOTO 5

C         FIND INDEX TO SITE SERIES COEFFICIENTS -> JPOS()
          IPAS = 0
          PAT1 = PAT2
    9     DO J = 1,P_SS
            DO K = 1,MAXSP
              IF (SSKONST(J)%SPP(K) .EQ. 0) EXIT
              IF (SSKONST(J)%SPP(K) .EQ. I) THEN
                DO M = 1, P_ST
                  IF (LEN_TRIM(SSKONST(J)%PrettyName(M)) .EQ. 0) EXIT
                  IF (INDEX(PAT1,SSKONST(J)%PrettyName(M)) .GT. 0) THEN
                    JPOS(I) = J
                    GOTO 5
                  ENDIF
                ENDDO
                EXIT
              ENDIF
            ENDDO
          ENDDO
          IF (J .GT. P_SS .AND. IPAS .EQ. 0) THEN
            IPAS = 1
            PAT1 = BEC2%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
            GOTO 9
          ENDIF
    5   ENDDO

C       BUILD EQUATION EXPRESSIONS
        DO I = 1,MAXSP
cccc          LLTDGOK(I) = .FALSE.
          DGCON(I)  = 0.0
          IP = IPOS(I)
          JP = JPOS(I)

          IF (IP .LT. 1 .OR. JP .LT. 1) GOTO 11

C         CONSTRAIN WHITE PINE TO RANGE: 500-1200m; ELEV COEFFS
C         PARAMETERIZED WITH ELEV(m)/100; OR 5.0-12.0
          PELEV = ELEV * FTtoM
          IF (I .EQ. 1) THEN
            PELEV = MIN(MAX(PELEV,5.0),12.0)
          ENDIF

          DGCON(I) = SSKONST(JP)%CON
     >      + ZNKONST(IP)%CON
     >      + ZNKONST(IP)%EL   * PELEV
     >      + ZNKONST(IP)%EL2  * (PELEV**2)
     >      + ZNKONST(IP)%SASP * SIN(ASPECT) * SLOPE
     >      + ZNKONST(IP)%CASP * COS(ASPECT) * SLOPE

C         POPULATE ARRAYS FOR OTHER CONSTANTS
          DGCCF1(I)  = ZNKONST(IP)%CCFA   ! CCF
          DGLD1(I)   = ZNKONST(IP)%LD     ! DBH
          DGBAL1(I)  = ZNKONST(IP)%BAL    ! BAL
          DGCR1(I)   = ZNKONST(IP)%CR     ! CROWN
          DGDSQ1(I)  = ZNKONST(IP)%DSQ    ! DBH^2
          DGDBAL1(I) = ZNKONST(IP)%DBAL1  ! BAL/DBH
          DGDBAL2(I) = ZNKONST(IP)%DBAL2  ! BAL/LOG(DBH+1)
          ATTEN(I)   = ZNKONST(IP)%OBSERV ! OBSERV **coeffs.f77**
          SMCON(I)   = 0.
C	    SIGMAR(I)  = ZNKONST(IP)%SIGMAR ! SIGMAR - RES.ERR
C	    SIGMAR(I)  = SIGMAR(I) * 0.5
cccc          LLTDGOK(I) = .TRUE.
   11   ENDDO
      ENDIF

C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.

      DO I = 1,MAXSP
        IF (LDCOR2.AND.COR2(I).GT.0.0) DGCON(I)=DGCON(I)
     &    + ALOG(COR2(I))
      ENDDO
       
      RETURN
      END
