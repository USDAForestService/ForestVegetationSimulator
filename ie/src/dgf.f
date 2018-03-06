      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C IE $Id: dgf.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
C  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
C  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
C  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
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
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
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
C----------
C  SPECIES EXPANSION: 
C  WB USE COEFFICIENTS FOR L
C  LM AND PY USE COEFFICIENTS FROM TT FOR LM
C  LL USE COEFFICIENTS FOR AF
C  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
C  CO, OH USE COEFFIECIENTS FOR OH FROM CR
C  OS USE COEFFEICIENTS FOR MH
C  PI, JU USE COEFFICIENTS FROM UT
C
C  ADDED IBSERV, DEFINED FOR TT AND UT VARIANTS
C  MAPSITE -- SITE INDEX ASSIGNED BY SPECIES IS DEPENDENT UPON HABITAT TYPE
C----------
      LOGICAL DEBUG
      REAL DIAM(MAXTRE),DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP)
      REAL DGCRSQ(MAXSP),DGDBAL(MAXSP),DGHAB(6,MAXSP),DGFOR(6,MAXSP)
      REAL DGDS(4,MAXSP),DGEL(MAXSP),DGEL2(MAXSP),DGSASP(MAXSP)
      REAL DGCASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP),DGCCFA(5,MAXSP)
      INTEGER MAPHAB(30,MAXSP),MAPDSQ(11,MAXSP),MAPLOC(11,MAXSP),
     &    MAPCCF(30,MAXSP)
      REAL OBSERV(6,MAXSP),YOUNG,OLD,BA100,CONSPP,DGLDS,DGBALS,DGCRS
      REAL DGCRS2,DGDSQS,DGDBLS,SI,D,ALD,BARK,BRATIO,DPP,BATEM,DF
      REAL DIAGR,DDS,CR,ASPDG,BAL,XPPDDS,XSITE,TMPASP
      INTEGER I,I1,I2,I3,ISIC,ICLS,ISPC,LSI
C
      DATA DGLD/
     &   0.56445, 0.54140, 0.56888, 0.68810, 0.68712, 0.58705,
     &   0.89503, 0.73045, 0.86240, 0.66101, 0.89778,
     &   0.54140, 0.213947,0.86240,     0.0,     0.0, 0.213947,
     &       0.0,      0.0,    0.0,     0.0,     0.0, 0.89778/
      DATA DGCR/
     &   1.08338, 1.03478, 2.06850, 1.93969, 1.64133, 1.29360,
     &   1.85558, 1.54643, 0.52044, 1.31618, 1.28403,
     &   1.03478, 1.523464,0.52044,     0.0,     0.0, 1.523464,
     &       0.0,     0.0,     0.0,     0.0,     0.0, 1.28403/
      DATA DGCRSQ/
     &       0.0, 0.07509,-0.62361,-0.78258,-0.27244,     0.0,
     &  -0.36393,-0.26635, 0.86236,     0.0,     0.0,
     &   0.07509,     0.0, 0.86236,     0.0,     0.0,     0.0,
     &       0.0,     0.0,     0.0,     0.0,     0.0,     0.0/
      DATA DGBAL/
     &   0.42112, 0.43637, 0.50202, 0.45142,     0.0, 0.74596,
     &  -0.03665, 0.25639,     0.0,     0.0,     0.0,
     &   0.43637,-0.358634,    0.0,     0.0,     0.0,-.358634,
     &       0.0,      0.0,    0.0,     0.0,     0.0,     0.0/
      DATA DGDBAL/
     &  -2.08272,-2.03256,-2.11590,-1.76812,-0.80918,-2.28375,
     &  -0.43329,-1.18218,-0.51270,-1.25881,-0.66110,
     &  -2.03256,     0.0,-0.51270,     0.0,     0.0,     0.0,
     &       0.0,     0.0,     0.0,     0.0,     0.0,-0.66110/
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
     &   968.0,  900.0,  797.0, 1124.0,   47.0,  809.0,
     &     27.,    70.,   123.,   101.,    33.,     0.,
     &   988.0,  177.0,   40.0,  331.0,  332.0, 2380.0,
     &   1000.,  1000.,  1000.,  1000.,  1000.,  1000.,
     &   1000.,  1000.,  1000.,  1000.,  1000.,  1000.,
     &     27.,    70.,   123.,   101.,    33.,     0.,
     &    184.,   429.,   356.,   162.,    74.,     0.,
     &   1000.,  1000.,  1000.,  1000.,  1000.,  1000.,
     &    184.,   429.,   356.,   162.,    74.,     0.,
     &    184.,   429.,   356.,   162.,    74.,     0.,
     &   1000.,  1000.,  1000.,  1000.,  1000.,  1000.,
     &    55.0,  880.0,    0.0,    0.0,    0.0,    0.0/
C----------
C  DGHAB IS AN ARRAY THAT CONTAINS HABITAT CLASS INTERCEPTS FOR
C  EACH SPECIES.  MAPHAB IS INDEXED BY ITYPE TO MAP HABITAT
C  TYPES ONTO HABITAT CLASSES.
C----------
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
     &  9*5, 1,1,2,1, 3*2, 3,3,2,5,2, 3*1, 5,5,4, 3*5,
     &  30*1,
     &  12*6, 1,2,3,3,4,3,1, 7*6, 1,5,5,6,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
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
     & 0.38335, 0.51291, 0.45377, 0.71322, 0.26835,     0.0,
     & 6*0.0,
     &-0.96389,-0.72415,-0.57308,-0.82218,-1.24093,-1.10746,
     & 6*0.0,
     & 6*0.0,
     & 6*0.0,
     & 6*0.0,
     & 6*0.0,
     & 6*0.0,
     & 6*0.0,
     & 6*0.0,
     &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0/
C----------
C  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
C  HABITAT CLASS.  MAPCCF IS INDEXED BY ITYPE TO MAP HABITAT TYPES
C  ONTO HABITAT CLASSES.
C----------
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
     &  11*3, 1, 6*3, 2,3,1,3,3,1, 6*3,
     &  30*1,
     &  12*3, 7*1, 3,1,1,2,2,1,3,1,1,3,3,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
     &  30*1,
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
     & -0.10144,-0.14793,-0.05438,     0.0,     0.0,
     & -0.199592,    0.0,     0.0,     0.0,     0.0,
     & -0.01598,-0.04477,-0.07392,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     & -0.199592,    0.0,     0.0,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     &      0.0,     0.0,     0.0,     0.0,     0.0,
     & -0.10744,     0.0,     0.0,     0.0,     0.0/
C----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
C----------
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
     & 1,1,1,2,3,3,2,5,5,4,1,
     & 11*1,
     & 4,1,1,2,2,3,3,4,4,2,1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
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
     & 0.20004, 0.07656, 0.08188, 0.30379,     0.0,     0.0,
     & 1.568742,    0.0,     0.0,     0.0,     0.0,     0.0,
     & 0.42062, 0.14072,-0.13001,     0.0,     0.0,     0.0,
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
     & 1.568742,    0.0,     0.0,     0.0,     0.0,     0.0,
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
     &     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
     & 0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0/
C----------
C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
C  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
C  COEFFICIENT.
C----------
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
     & 1,2,1,1,1,1,1,1,1,1,2,
     & 11*1,
     & 1,2,2,1,2,1,1,2,1,1,2,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
     & 11*1,
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
     & -0.000310,-0.000566,      0.0,      0.0,
     & 4*-0.0006538,
     & -0.000283,-0.000780,      0.0,      0.0,
     & 4*0.0,
     & 4*0.0,
     & 4*-0.0006538,
     & 4*0.0,
     & 4*0.0,
     & 4*0.0,
     & 4*0.0,
     & 4*0.0,
     & -0.000484,-0.000306,      0.0,      0.0/     
C----------
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
C----------
      DATA DGCASP/
     &   0.09817,-0.21337,-0.04562,-0.01215, 0.08277,-0.06625,
     &   0.00325,-0.13091,-0.12473,-0.09976, 0.17935,
     &  -0.21337,-0.609774,-.12473,     0.0,     0.0,-0.609774,
     &        0.0,     0.0,      0.0,     0.0,   0.0, 0.17935/
      DATA DGSASP/
     &   0.03876, 0.03430, 0.06287,-0.04595, 0.10987, 0.05534,
     &   0.12993,-0.06038,-0.06862, 0.01192, 0.13363,
     &   0.03430,-0.01752,-0.06862,     0.0,     0.0,-0.017520,
     &        0.0,    0.0,      0.0,     0.0,    0.0, 0.13363/
      DATA DGSLOP/
     &  -0.17888, 0.33523, 0.78176, 1.17025, 0.04966, 0.11931,
     &   0.46546, 0.65622, 0.30070,-0.06637, 0.07628,
     &   0.33523,-2.05706, 0.30070,     0.0,     0.0,-2.05706,
     &       0.0,      0.0,     0.0,     0.0,    0.0, 0.07628/
      DATA DGSLSQ/
     &       0.0,-0.70216,-1.12380,-1.52006,     0.0,     0.0,
     &  -0.58014,-0.90143,-0.62224,-0.43720,     0.0,
     &  -0.70216, 2.113263,-0.62224,    0.0,     0.0, 2.113263,
     &       0.0,      0.0,     0.0,    0.0,     0.0,      0.0/
      DATA DGEL/
     &   0.03517, 0.03730, 0.02591, 0.00917, 0.02863,-0.00175,
     &  -0.00480, 0.06259, 0.06313, 0.03229, 0.08518,
     &   0.03730,     0.0, 0.06313,     0.0,     0.0,     0.0,
     &       0.0,     0.0,     0.0,     0.0,     0.0, 0.08518/
      DATA DGEL2/
     &  -0.000467,-0.000433,-0.000377,-0.000117,-0.000422,-0.000067,
     &  -0.000058,-0.000709,-0.000676,-0.000422,-0.000943,
     &  -0.000433,      0.0,-0.000676,      0.0,       0.0,     0.0,
     &        0.0,      0.0,      0.0,      0.0,       0.0,-0.000943/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
C----------
C  DEBUG OUTPUT: MODEL COEFFICIENTS.
C----------
      IF(DEBUG)
     & WRITE(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGCCF,DGBAL
 9000 FORMAT(/,' DGCON',/12(1X,F9.5),/11(1X,F9.5),/,' DGDSQ',/12(1X,F9.5
     & ),/11(1X,F9.5),/,' DGLD',/12(1X,F9.5),/11(1X,F9.5),/,' DGCR',
     & /12(1X,F9.5),/11(1X,F9.5),/,' DGCRSQ',/12(1X,F9.5),/11(1X,F9.5),
     & /,' DGCCF',/12(1X,F9.5),/11(1X,F9.5),/,' DGBAL',/12(1X,F9.5),
     & /11(1X,F9.5))
C----------
C CALL BADIST AND COMPUTE THE AGE RANGE
C----------
      AGERNG= 1000.
      YOUNG= 1000.
      OLD= 0.0
      CALL BADIST(DEBUG)
C
      DO I= 1,ITRN
        IF(ABIRTH(I).LE.1.)GO TO 5
        IF(HT(I) .LE. 4.5) GO TO 5
        IF(ABIRTH(I) .LT. YOUNG) YOUNG = ABIRTH(I)
        IF(ABIRTH(I) .GT. OLD) OLD = ABIRTH(I)
    5   CONTINUE
        AGERNG = OLD - YOUNG
        AGERNG = ABS(AGERNG)
      ENDDO
C
      CALL SDICAL(SDIMAX)
C----------
C  SCALE BASAL AREA.
C----------
      BA100 = BA/100.
C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      CONSPP= DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN
      DGLDS= DGLD(ISPC)
      DGBALS = DGBAL(ISPC)
      DGCRS= DGCR(ISPC)
      DGCRS2=DGCRSQ(ISPC)
      DGDSQS=DGDSQ(ISPC)
      DGDBLS=DGDBAL(ISPC)
C
C  SPECIES EXPANSION-SITE INDEX ASSIGNED IS DEPENDENT UPON HABITAT TYPE
C
      SI = SITEAR(ISPC)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 10 I3=I1,I2
      I=IND1(I3)
      D=DIAM(I)
      IF (D.LE.0.0) GOTO 10
      ALD=ALOG(D)
      BARK=BRATIO(ISPC,D,HT(I))
C
C  PI/JU LOGIC FROM UT
C
      IF(ISPC.EQ.15 .OR. ISPC.EQ.16)THEN
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BA
        IF(BATEM .LT.  1.0) BATEM = 1.0
        DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM
     &         + 0.00177 * SI
        IF((DF-DPP) .GT. 1.0) DF = DPP + 1.0
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
        ENDIF
C
C  ASPEN LOGIC FROM UTAH
C
      ELSEIF(ISPC.EQ.18 .OR. ISPC.EQ.20 .OR. ISPC.EQ.21)THEN
        CR = ICR(I)
        CALL DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
        DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
C
C  CO LOGIC FROM CR
C
      ELSEIF(ISPC.EQ.19.OR. ISPC.EQ.22)THEN
C--DBHMAX MOVED FROM SITSET
        ICLS = IFIX(D + 1.0)
        IF(ICLS .GT. 41) ICLS = 41
        CR=ICR(I)*0.01
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        DF = 0.24506 + 1.01291 * DPP - 0.00084659 * BA
     &         + 0.00631*SI
        IF(DF .GT. 36.) DF = 36.
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
        DDS = DDS+COR(ISPC)+DGCON(ISPC)
C
C ORIGINAL NI SECTION
C
      ELSE
        CR=ICR(I)*0.01
        BAL = (1.0-(PCT(I)/100.))*BA100
        DDS=CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)
     &          +DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))
      ENDIF
C---------
C     CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
C     FOR THE DENSITY OF NEIGHBORING STANDS.
C
      XPPDDS=0.
      CALL PPDGF (XPPDDS,BAL,RELDEN,D,DGCCF(ISPC)*.01,DGBALS,DGDBLS)
C
      DDS=DDS+XPPDDS
C---------
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(.NOT.DEBUG) GO TO 10
      WRITE(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
 9001 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &      ',  BAL=',F7.2,',  CR=',F7.4/
     &      '          CCF=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
   10 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
      RETURN
C
C
C
      ENTRY DGCONS
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C  ITYPE IS THE HABITAT TYPE INDEX AS COMPUTED IN **HABTYP**.
C  ASPECT IS STAND ASPECT IN RADIANS.  OBSERV CONTAINS THE NUMBER OF
C  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
C  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO 30 ISPC=1,MAXSP
C------------
C  PUT SITE INTO ONE OF 5 CLASSES
C----------
      XSITE = SITEAR(ISPC)
      LSI=INT(XSITE/10.)
      IF(LSI.LT.2)ISIC=1
      IF(LSI.GE.5)ISIC=5
      IF(LSI.GE.3 .AND. LSI.LT.4)ISIC=3
      IF(LSI.GE.4 .AND. LSI.LT.5)ISIC=4
      IF(LSI.GE.2 .AND. LSI.LT.3)ISIC=2
C
      ISPHAB=MAPHAB(ITYPE,ISPC)
      ISPFOR=MAPLOC(IFOR,ISPC)
      ISPDSQ=MAPDSQ(IFOR,ISPC)
      ISPCCF=MAPCCF(ITYPE,ISPC)
      TMPASP = ASPECT
      IF(ISPC.EQ.13.OR.ISPC.GE.15.AND.ISPC.LE.18.OR.ISPC.EQ.20
     &   .OR.ISPC.EQ.21)TMPASP=TMPASP-.7854
      DGCON(ISPC)= DGHAB(ISPHAB,ISPC)
     &                 + DGFOR(ISPFOR,ISPC)
     &                 + DGEL(ISPC) * ELEV
     &                 + DGEL2(ISPC) * ELEV * ELEV
     &                 +(DGSASP(ISPC) * SIN(TMPASP)
     &                 + DGCASP(ISPC) * COS(TMPASP)
     &                 + DGSLOP(ISPC)) * SLOPE
     &                 + DGSLSQ(ISPC) * SLOPE * SLOPE
      DGDSQ(ISPC)=DGDS(ISPDSQ,ISPC)
      DGCCF(ISPC)=DGCCFA(ISPCCF,ISPC)
      IF(ISPC .LE. 12 .OR. ISPC .EQ. 14 .OR. ISPC .EQ. 23)THEN
        ATTEN(ISPC)=OBSERV(ISPHAB,ISPC)
      ELSE
        ATTEN(ISPC) = OBSERV(ISIC,ISPC)
        IF(ISPC.EQ.13.OR.ISPC.EQ.17)THEN
           DGCON(ISPC)=DGCON(ISPC)+.001766*XSITE
        ELSEIF(ISPC.EQ.18.OR.ISPC.EQ.20.OR.ISPC.EQ.21)THEN
           DGCON(ISPC)=DGCON(ISPC)+.006460*XSITE
        ENDIF
      ENDIF
      SMCON(ISPC)=0.
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
      IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC)
     &  + ALOG(COR2(ISPC))
   30 CONTINUE
      RETURN
      END
