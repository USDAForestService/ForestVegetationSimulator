      SUBROUTINE HTDBH (IFOR,ISPC,D,H,MODE)
      IMPLICIT NONE
C----------
C BM $Id$
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
C             1 IS MALHEUR (604
C             2 IS OCHOCO (607)
C             3 IS UMATILLA (614)
C             4 IS WALLOWA-WHITMAN (616)
C      MODE = MODE OF OPERATING THIS SUBROUTINE
C             0 IF DIAMETER IS PROVIDED AND HEIGHT IS DESIRED
C             1 IF HEIGHT IS PROVIDED AND DIAMETER IS DESIRED
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER IFOR,ISPC,MODE
C
      REAL D,H,HAT3,P2,P3,P4
C
      REAL MALHUR(MAXSP,3),OCHOCO(MAXSP,3)
      REAL UMATIL(MAXSP,3),WALWIT(MAXSP,3)
C
C----------
C  DATA STATEMENTS:
C
C  SPECIES ORDER:
C   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
C  17=OS, 18=OH
C
C  SPECIES EXPANSION:
C  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
C  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
C  LM USES UT LM
C  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
C  YC USES WC YC
C  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
C  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
C  OS USES BM PP BARK COEFFICIENT
C  OH USES SO OH (ORIGINALLY FROM WC VARIANT)
C----------
C
C  MALHEUR -- use Wallowa-Whitman for mountain hemlock
C----------
      DATA MALHUR /
     &  140.8498,  188.1500,  476.1213,  846.4856,  150.5836,
     &       0.0, 1901.4963,  211.5595,  437.3897, 1818.1733,
     &       0.0,       0.0,   77.2207,   97.7769,       0.0,
     &  178.6441, 1818.1733, 1709.7229,
C
     &    4.9436,    5.6420,    5.0963,    6.1757,    5.5158,
     &       0.0,    5.9791,    7.3130,    5.6600,    6.8482,
     &       0.0,       0.0,    3.5181,    8.8202,       0.0,
     &    4.5852,    6.8482,    5.8887,
C
     &   -0.6048,   -0.7348,   -0.3461,   -0.3210,   -0.6435,
     &       0.0,   -0.2300,   -0.7176,   -0.3975,   -0.2535,
     &       0.0,       0.0,   -0.5894,   -1.0534,       0.0,
     &   -0.6746,   -0.2535,   -0.2286/
C----------
C  OCHOCO -- use Wallowa-Whitman for mountain hemlock
C            use Siskiyou for western white pine
C----------
      DATA OCHOCO /
     &  140.8498,  255.4638,  318.7441,  686.4831,  150.5836,
     &       0.0,  228.0877,  738.6208,  128.7188, 1526.6312,
     &       0.0,       0.0,   77.2207,   97.7769,       0.0,
     &  178.6441, 1526.6312, 1709.7229,
C
     &    4.9436,    5.5577,    5.6666,    6.5393,    5.5158,
     &       0.0,    4.2939,    5.5866,    6.9094,    6.9207,
     &       0.0,       0.0,    3.5181,    8.8202,       0.0,
     &    4.5852,    6.9207,    5.8887,
C
     &   -0.6048,   -0.6054,   -0.4666,   -0.3740,   -0.6435,
     &       0.0,   -0.4277,   -0.3193,   -0.9039,   -0.2774,
     &       0.0,       0.0,   -0.5894,   -1.0534,       0.0,
     &   -0.6746,   -0.2774,   -0.2286/
C----------
C  UMATILLA -- use Wallowa-Whitman for mountain hemlock
C----------
      DATA UMATIL /
     &  140.8498,  186.6635,  219.4816,  297.7143,  150.5836,
     &       0.0,   89.0137,  221.5298,  164.6321,  313.4270,
     &       0.0,       0.0,   77.2207,   97.7769,       0.0,
     &  178.6441,  313.4270, 1709.7229,
C
     &    4.9436,    5.3006,    5.3103,    5.9520,    5.5158,
     &       0.0,    7.7404,    6.1879,    6.9476,    6.4808,
     &       0.0,       0.0,    3.5181,    8.8202,       0.0,
     &    4.5852,    6.4808,    5.8887,
C
     &   -0.6048,   -0.7604,   -0.5643,   -0.5290,   -0.6435,
     &       0.0,   -1.3530,   -0.6629,   -0.7650,   -0.5194,
     &       0.0,       0.0,   -0.5894,   -1.0534,       0.0,
     &   -0.6746,   -0.5194,   -0.2286/
C----------
C  WALLOWA-WHITMAN
C----------
      DATA WALWIT /
     &  140.8498,  326.9389,  260.1577,  360.9231,  150.5836,
     &       0.0,  117.1495,  219.4529,  128.7188,  649.6683,
     &       0.0,       0.0,   77.2207,   97.7769,       0.0,
     &  178.6441,  649.6683, 1709.7229,
C
     &    4.9436,    4.6684,    5.2245,    5.7382,    5.5158,
     &       0.0,    4.8451,    6.1539,    6.9094,    6.1279,
     &       0.0,       0.0,    3.5181,    8.8202,       0.0,
     &    4.5852,    6.1279,    5.8887,
C
     &   -0.6048,   -0.4657,   -0.5013,   -0.4544,   -0.6435,
     &       0.0,   -0.8613,   -0.6558,   -0.9039,   -0.3511,
     &       0.0,       0.0,   -0.5894,   -1.0534,       0.0,
     &   -0.6746,   -0.3511,   -0.2286/
C----------
C  SET EQUATION PARAMETERS ACCORDING TO FOREST AND SPECIES.
C----------
      IF(IFOR .EQ. 1) THEN
        P2 = MALHUR(ISPC,1)
        P3 = MALHUR(ISPC,2)
        P4 = MALHUR(ISPC,3)
      ELSE IF(IFOR .EQ.2) THEN
        P2 = OCHOCO(ISPC,1)
        P3 = OCHOCO(ISPC,2)
        P4 = OCHOCO(ISPC,3)
      ELSE IF(IFOR .EQ.3) THEN
        P2 = UMATIL(ISPC,1)
        P3 = UMATIL(ISPC,2)
        P4 = UMATIL(ISPC,3)
      ELSE
        P2 = WALWIT(ISPC,1)
        P3 = WALWIT(ISPC,2)
        P4 = WALWIT(ISPC,3)
      ENDIF
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
