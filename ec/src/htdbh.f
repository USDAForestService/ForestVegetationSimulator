      SUBROUTINE HTDBH (IFOR,ISPC,D,H,MODE)
      IMPLICIT NONE
C----------
C  **HTDBH--EC  DATE OF LAST REVISION:  03/24/08
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
C             1 IS GIFFORD PINCHOT (603)
C             2 IS MOUNT HOOD (606)
C             3 IS OKANOGAN (608)
C             4 IS WENATCHEE (617)
C             5 IS OKANOGAN TONASKET RD (699)
C      MODE = MODE OF OPERATING THIS SUBROUTINE
C             0 IF DIAMETER IS PROVIDED AND HEIGHT IS DESIRED
C             1 IF HEIGHT IS PROVIDED AND DIAMETER IS DESIRED
C----------
C
      REAL MTHOOD(11,3)
      REAL GIFFPC(11,3),OKANOG(11,3),WENATC(11,3),OKTNRD(11,3)
      REAL H,D,P2,P3,P4,HAT3
      INTEGER MODE,ISPC,IFOR
C
C SPECIES ORDER IN EC VARIANT:
C    1 = PIMO3    WESTERN WHITE PINE        PINUS MONTICOLA
C    2 = LAOC     WESTERN LARCH             LARIX OCCIDENTALIS
C    3 = PSME     DOUGLAS-FIR               PSEUDOTSUGA MENZIESII
C    4 = ABAM     PACIFIC SILVER FIR        ABIES AMABILIS
C    5 = THPL     WESTERN RED CEDAR         THUJA PLICATA
C    6 = ABGR     GRAND FIR                 ABIES GRANDIS
C    7 = PICO     LODGEPOLE PINE            PINUS CONTORTA
C    8 = PIEN     ENGELMANN SPRUCE          PICEA ENGELMANNII
C    9 = ABLA     SUBALPILE FIR             ABIES LASIOCARPA
C   10 = PIPO     PONDEROSA PINE            PINUS PONDEROSA
C   11 = TSME     MOUNTAIN HEMLOCK/OTHER    TSUGA MERTENSIANA
C----------
C
C  GIFFORD PINCHOT --- EAST SIDE
C
      DATA GIFFPC /
     & 1143.6254,  255.4638,  519.1872,  171.2219,  616.3503,
     &  727.8110,  102.6146,  211.7962,  113.5390,  324.4467,
     &  631.7598,
C
     &    6.1913,    5.5577,    5.3181,    9.9497,    5.7620,
     &    5.4648,   10.1435,    6.7015,    9.0045,    8.0484,
     &    5.8492,
C
     &   -0.3096,   -0.6054,   -0.3943,   -0.9727,   -0.3633,
     &   -0.3435,   -1.2877,   -0.6739,   -0.9907,   -0.5892,
     &   -0.3384 /
C
C  MOUNT HOOD --- EAST SIDE
C
      DATA MTHOOD /
     &  433.7807,  220.0   ,  234.2080,  441.9959,  487.5415,
     &  376.0978,  121.1392, 2118.6711,   66.6950,  324.4467,
     &  224.6205,
C
     &    6.3318,    5.0   ,    6.3013,    6.5382,    5.4444,
     &    5.1639,   12.6623,    6.6094,   13.2615,    8.0484,
     &    7.2549,
C
     &   -0.4988,   -0.6054,   -0.6413,   -0.4787,   -0.3801,
     &   -0.4319,   -1.2981,   -0.2547,   -1.3774,   -0.5892,
     &   -0.6890 /
C
C  OKANOGAN  (USE GIFFORD PINCHOT COEFFICIENTS)
C
      DATA OKANOG /
     & 1143.6254,  255.4638,  519.1872,  171.2219,  616.3503,
     &  727.8110,  102.6146,  211.7962,  113.5390,  324.4467,
     &  631.7598,
C
     &    6.1913,    5.5577,    5.3181,    9.9497,    5.7620,
     &    5.4648,   10.1435,    6.7015,    9.0045,    8.0484,
     &    5.8492,
C
     &   -0.3096,   -0.6054,   -0.3943,   -0.9727,   -0.3633,
     &   -0.3435,   -1.2877,   -0.6739,   -0.9907,   -0.5892,
     &   -0.3384 /
C
C  WENATCHEE  (USE GIFFORD PINCHOT COEFFICIENTS)
C
      DATA WENATC /
     & 1143.6254,  255.4638,  519.1872,  171.2219,  616.3503,
     &  727.8110,  102.6146,  211.7962,  113.5390,  324.4467,
     &  631.7598,
C
     &    6.1913,    5.5577,    5.3181,    9.9497,    5.7620,
     &    5.4648,   10.1435,    6.7015,    9.0045,    8.0484,
     &    5.8492,
C
     &   -0.3096,   -0.6054,   -0.3943,   -0.9727,   -0.3633,
     &   -0.3435,   -1.2877,   -0.6739,   -0.9907,   -0.5892,
     &   -0.3384 /
C
C  OKANOGAN -- TONASKET RD  (USE GIFFORD PINCHOT COEFFICIENTS)
C
      DATA OKTNRD /
     & 1143.6254,  255.4638,  519.1872,  171.2219,  616.3503,
     &  727.8110,  102.6146,  211.7962,  113.5390,  324.4467,
     &  631.7598,
C
     &    6.1913,    5.5577,    5.3181,    9.9497,    5.7620,
     &    5.4648,   10.1435,    6.7015,    9.0045,    8.0484,
     &    5.8492,
C
     &   -0.3096,   -0.6054,   -0.3943,   -0.9727,   -0.3633,
     &   -0.3435,   -1.2877,   -0.6739,   -0.9907,   -0.5892,
     &   -0.3384 /
C----------
C  SET EQUATION PARAMETERS ACCORDING TO FOREST AND SPECIES.
C  WINEMA EQNS ARE USED FOR REGION 5 FORESTS.
C----------
      IF(IFOR .EQ. 1) THEN
        P2 = GIFFPC(ISPC,1)
        P3 = GIFFPC(ISPC,2)
        P4 = GIFFPC(ISPC,3)
      ELSE IF(IFOR .EQ.2) THEN
        P2 = MTHOOD(ISPC,1)
        P3 = MTHOOD(ISPC,2)
        P4 = MTHOOD(ISPC,3)
      ELSE IF(IFOR .EQ.3) THEN
        P2 = OKANOG(ISPC,1)
        P3 = OKANOG(ISPC,2)
        P4 = OKANOG(ISPC,3)
      ELSE IF(IFOR .EQ.4) THEN
        P2 = WENATC(ISPC,1)
        P3 = WENATC(ISPC,2)
        P4 = WENATC(ISPC,3)
      ELSE
        P2 = OKTNRD(ISPC,1)
        P3 = OKTNRD(ISPC,2)
        P4 = OKTNRD(ISPC,3)
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
