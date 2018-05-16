      SUBROUTINE HTDBH (IFOR,ISPC,D,H,MODE)
      IMPLICIT NONE
C----------
C  **HTDBH--SO  DATE OF LAST REVISION:  08/19/15
C----------
C  THIS SUBROUTINE CONTAINS THE DEFAULT HEIGHT-DIAMETER RELATIONSHIPS
C  FROM THE INVENTORY DATA.  IT IS CALLED FROM CRATET TO DUB MISSING
C  HEIGHTS, AND FROM REGENT TO ESTIMATE DIAMETERS (PROVIDED IN BOTH
C  CASES THAT LHTDRG IS SET TO .TRUE.).
C
C  DEFINITION OF VARIABLES:
C         D = DIAMETER AT BREAST HEIGHT
C         H = TOTAL TREE HEIGHT (STUMP TO TIP)
C      IFOR = FOREST CODE
C             1 IS DESCHUTES
C             2 IS FREMONT
C             3 IS WINEMA
C            10 IS WARM SPRINGS RESERVATION
C      MODE = MODE OF OPERATING THIS SUBROUTINE
C             0 IF DIAMETER IS PROVIDED AND HEIGHT IS DESIRED
C             1 IF HEIGHT IS PROVIDED AND DIAMETER IS DESIRED
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
      REAL WINEMA(MAXSP,3),FREMNT(MAXSP,3),DESCHT(MAXSP,3)
      REAL H,D,P2,P3,P4,HAT3
      INTEGER MODE,ISPC,IFOR,J,I
C----------
C  SPECIES ORDER:
C  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
C 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
C 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
C 31=MB, 32=OS, 33=OH
C
C  FREMONT COEFFS FOR OT USED FOR DESCHUTES.
C----------
      DATA ((DESCHT(I,J),I=1,MAXSP),J=1,2) /
     &  582.9947,  128.8697,  253.2541,  235.3340,  197.4948,
     & 4902.9732,  777.9043,  290.2790,  606.3002, 2700.1370,
     &       0.0,  235.3340,  291.4070,  171.2219,  247.7348,
     &       0.0,  255.4638,  616.3503,  317.8257,   77.2207,
     &  133.7965,  484.4591,   76.5170,       0.0,  178.6441,
     &   73.3348,   40.3812,  149.5861,10707.3906, 1709.7229,
     & 1709.7229,  253.2541, 1709.7229,
C
     &    5.4612,    6.8868,    4.7331,    5.7931,    6.7218,
     &    7.5484,    5.2036,    6.5834,    6.2936,    7.1184,
     &       0.0,    5.7931,    5.7543,    9.9497,    6.1830,
     &       0.0,    5.5577,    5.7620,    6.8287,    3.5181,
     &    6.4050,    4.5713,    2.2107,       0.0,    4.5852,
     &    2.6548,    3.7653,    2.4231,    8.4670,    5.8887,
     &    5.8887,    4.7331,    5.8887/
C
      DATA ((DESCHT(I,J),I=1,MAXSP),J=3,3) /
     &   -0.3435,   -0.8701,   -0.4843,   -0.5972,   -0.6528,
     &   -0.1783,   -0.2843,   -0.5753,   -0.3860,   -0.2312,
     &       0.0,   -0.5972,   -0.5013,   -0.9727,   -0.6335,
     &       0.0,   -0.6054,   -0.3633,   -0.6034,   -0.5894,
     &   -0.8329,   -0.3643,   -0.6365,       0.0,   -0.6746,
     &   -1.2460,   -1.1224,   -0.1800,   -0.1863,   -0.2286,
     &   -0.2286,    -0.4843,   -0.2286/
C
C  DESCHUTES COEFFS FOR DF AND ES USED FOR FREMONT
C
      DATA ((FREMNT(I,J),I=1,MAXSP),J=1,2) /
     &  391.7346,  948.8488,  253.2541,  705.1903,  103.7798,
     &76621.9189,  113.7962,  290.2790, 606.3002, 1154.2447,
     &       0.0,   705.1903, 1056.5434,   171.2219,   247.7348,
     &       0.0,   255.4638,   616.3503,   317.8257,   77.2207,
     &   133.7965,   484.4591,   76.5170,       0.0,   178.6441,
     &   73.3348,   40.3812,   149.5861,  10707.3906, 1709.7229,
     & 1709.7229,   253.2541, 1709.7229,
C
     &    5.1102,    6.1388,    4.7331,    6.0971,   10.6932,
     &   10.2682,    4.7726,    6.5834,    6.2936,    6.6836,
     &       0.0,    6.0971,    6.6974,    9.9497,    6.1830,
     &       0.0,    5.5577,   5.7620,    6.8287,    3.5181,
     &    6.4050,    4.5713,    2.2107,       0.0,    4.5852,
     &    2.6548,    3.7653,    2.4231,    8.4670,   5.8887,
     &    5.8887,    4.7331,    5.8887/
C
      DATA ((FREMNT(I,J),I=1,MAXSP),J=3,3) /
     &   -0.3602,   -0.2877,   -0.4843,   -0.3273,   -1.0711,
     &   -0.1178,   -0.7601,   -0.5753,   -0.3860,   -0.2876,
     &       0.0,   -0.3273,   -0.2950,   -0.9727,   -0.6335,
     &       0.0,   -0.6054,   -0.3633,   -0.6034,   -0.5894,
     &   -0.8329,   -0.3643,   -0.6365,       0.0,   -0.6746,
     &   -1.2460,   -1.1224,   -0.1800,   -0.1863,   -0.2286,
     &   -0.2286,   -0.4843,   -0.2286/
C
      DATA ((WINEMA(I,J),I=1,MAXSP),J=1,2) /
     &  133.7789,  222.7080,  231.7163,  471.6016,  130.7104,
     & 4518.2601,  128.7972,  168.9700,  606.3002,  812.2630,
     &       0.0,  471.6016,  299.3002,  171.2219,  247.7348,
     &       0.0,  255.4638,  616.3503,  317.8257,  77.2207,
     &  133.7965,  484.4591,  76.5170,       0.0,  178.6441,
     &  73.3348,  40.3812,  149.5861, 10707.3906, 1709.7229,
     & 1709.7229,   231.7163, 1709.7229,
C
     &    6.9968,    6.1735,    6.7143,    5.7106,    7.7823,
     &    8.0469,    4.9833,   13.6848,    6.2936,    6.4422,
     &       0.0,    5.7106,    6.3401,    9.9497,    6.1830,
     &       0.0,    5.5577,    5.7620,    6.8287,    3.5181,
     &    6.4050,    4.5713,    2.2107,       0.0,    4.5852,
     &    2.6548,    3.7653,    2.4231,    8.4670,    5.8887,
     &    5.8887,    6.7143,    5.8887/
C
      DATA ((WINEMA(I,J),I=1,MAXSP),J=3,3) /
     &   -0.9072,   -0.6122,   -0.6647,   -0.4035,   -0.8830,
     &   -0.2090,   -0.7463,   -1.0635,   -0.3860,   -0.3348,
     &       0.0,   -0.4035,   -0.5275,   -0.9727,   -0.6335,
     &       0.0,   -0.6054,   -0.3633,   -0.6034,   -0.5894,
     &   -0.8329,   -0.3643,   -0.6365,       0.0,   -0.6746,
     &   -1.2460,   -1.1224,   -0.1800,   -0.1863,   -0.2286,
     &   -0.2286,   -0.6647,   -0.2286/
C----------
C  SET EQUATION PARAMETERS ACCORDING TO FOREST AND SPECIES.
C  WINEMA EQNS ARE USED FOR REGION 5 FORESTS.
C  DESCHUTES COEFFICIENTS ARE USED FOR WARM SPRINGS RESERVATION
C----------
      SELECT CASE (IFOR)
        CASE (1,10)
          P2 = DESCHT(ISPC,1)
          P3 = DESCHT(ISPC,2)
          P4 = DESCHT(ISPC,3)
        CASE (2)
          P2 = FREMNT(ISPC,1)
          P3 = FREMNT(ISPC,2)
          P4 = FREMNT(ISPC,3)
        CASE DEFAULT
          P2 = WINEMA(ISPC,1)
          P3 = WINEMA(ISPC,2)
          P4 = WINEMA(ISPC,3)
      END SELECT
      IF(MODE .EQ. 0) H=0.
      IF(MODE .EQ. 1) D=0.
C----------
C  PROCESS ACCORDING TO MODE
C----------
      IF(MODE .EQ. 0) THEN
        IF(D .GE. 3.) THEN
          H = 4.5 + P2 * EXP(-1.*P3*D**P4)
        ELSE
          H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
        ENDIF
      ELSE
        HAT3 = 4.5 + P2 * EXP(-1.*P3*3.0**P4)
        IF(H .GE. HAT3) THEN
          D = EXP( ALOG((ALOG(H-4.5)-ALOG(P2))/(-1.*P3)) * 1./P4)
        ELSE
          D = (((H-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))-4.51))+0.3
        ENDIF
      ENDIF
C
      RETURN
      END
