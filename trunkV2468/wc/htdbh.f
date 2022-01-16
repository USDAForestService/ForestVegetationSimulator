      SUBROUTINE HTDBH (JFOR,ISPC,D,H,MODE)
      IMPLICIT NONE
C----------
C  **HTDBH--WC  DATE OF LAST REVISION:  08/21/15
C----------
C  THIS SUBROUTINE CONTAINS THE DEFAULT HEIGHT-DIAMETER RELATIONSHIPS
C  FROM THE INVENTORY DATA.  IT IS CALLED FROM CRATET TO DUB MISSING
C  HEIGHTS, AND FROM REGENT TO ESTIMATE DIAMETERS (PROVIDED IN BOTH
C  CASES THAT LHTDRG IS SET TO .TRUE.).
C
C  DEFINITION OF VARIABLES:
C         D = DIAMETER AT BREAST HEIGHT
C         H = TOTAL TREE HEIGHT (STUMP TO TIP)
C      JFOR = FOREST CODE
C             1 IS GIFFORD PINCHOT (603)
C             2 IS MT BAKER / SNOQUALMIE (605)
C             3 IS MT HOOD (606)
C             4 IS ROGUE RIVER (610)
C             5 IS UMPQUA (615)
C             6 IS WILLAMETTE (618)
C             7 IS BLM SALEM --- USE MT HOOD
C             8 IS BLM EUGENE --- USE WILLAMETTE
C             9 IS BLM ROSEBURG --- USE UMPQUA
C            10 IS BLM MEDFORD --- USE ROGUE RIVER
C      MODE = MODE OF OPERATING THIS SUBROUTINE
C             0 IF DIAMETER IS PROVIDED AND HEIGHT IS DESIRED
C             1 IF HEIGHT IS PROVIDED AND DIAMETER IS DESIRED
C----------
C
      REAL MTBSNQ(39,3),MTHOOD(39,3)
      REAL GIFFPC(39,3),ROGRIV(39,3),UMPQUA(39,3),WILLAM(39,3)
      INTEGER MODE,ISPC,JFOR,I
      REAL H,D,P2,P3,P4,HAT3
C
C SPECIES ORDER IN WC VARIANT:
C    1 = ABAM     PACIFIC SILVER FIR        ABIES AMABILIS
C    2 = ABCO     WHITE FIR                 ABIES CONCOLOR
C    3 = ABGR     GRAND FIR                 ABIES GRANDIS
C    4 = ABLA     SUBALPINE FIR             ABIES LASIOCARPA
C    5 = ABMA     CALJFORNIA RED FIR        ABIES MAGNIFICA
C    6 =
C    7 = ABPR     NOBLE FIR                 ABIES PROCERA
C    8 = CANO9    ALASKA CEDAR              CALLITROPSIS NOOTKATENSIS
C        LAOC     WESTERN LARCH             LARIX OCCIDENTALIS
C    9 = LIDE     INCENSE CEDAR             LIBOCEDRUS DECURRENS
C   10 = PIEN     ENGELMANN SPRUCE          PICEA ENGELMANNII
C        PISI     SITKA SPRUCE              PICEA SITCHENSIS
C   11 = PICO     LODGEPOLE PINE            PINUS CONTORTA
C   12 = PIJE     JEFFREY PINE              PINUS JEFFREYI
C   13 = PILA     SUGAR PINE                PINUS LAMBERTIANA
C   14 = PIMO3    WESTERN WHITE PINE        PINUS MONTICOLA
C   15 = PIPO     PONDEROSA PINE            PINUS PONDEROSA
C   16 = PSME     DOUGLAS-FIR               PSEUDOTSUGA MENZIESII
C   17 = SESE3    COAST REDWOOD             SEQUOIA SEMPERVIRENS
C   18 = THPL     WESTERN REDCEDAR          WESTERN REDCEDAR
C   19 = TSHE     WESTERN HEMLOCK           TSUGA HETEROPHYLLA
C   20 = TSME     MOUNTAIN HEMLOCK          TSUGA MERTENSIANA
C   21 = ACMA3    BIGLEAF MAPLE             ACER MACROPHYLLUM
C   22 = ALRU2    RED ALDER                 ALNUS RUBRA
C   23 = ALRH2    WHITE ALDER               ALNUS RHOMBIFOLIA
C        ARME     PACIFIC MADRONE           ARBUTUS MENZIESII
C   24 = BEPA     PAPER BIRCH               BETULA PAPYRIFERA
C   25 = CHCHC4   GIANT CHINQUAPIN          CHRYSOLEPIS CHRYSOPHYLLA
C        LIDE3    TANOAK                    LITHOCARPUS DENSIFLORUS
C   26 = POTR5    QUAKING ASPEN             POPULUS TREMULOIDES
C   27 = POBAT    BLACK COTTONWOOD          POPULUS TRICHOCARPA
C   28 = QUGA4    OREGON WHITE OAK          QUERCUS GARRYANA
C        QUKE     CALJFORNIA BLACK OAK      QUERCUS KELLOGGII
C   29 = JUOC     JUNIPER                   JUNIPERUS OCCIDENTALIS
C   30 = LALY     SUBALPINE LARCH           LARIX LYALLII
C   31 = PIAL     WHITEBARK PINE            PINUS ALBICAULIS
C   32 = PIAT     KNOBCONE PINE             PINUS ATTENUATA
C   33 = TABR2    PACIFIC YEW               TAXUS BREVIFOLIA
C   34 = CONU4    PACIFIC DOGWOOD           CORNUS NUTTALLII
C   35 = CR__     HAWTHORN                  CRATAEGUS spp.
C   36 = PREM     BITTER CHERRY             PRUNUS EMARGINATA
C   37 = SA__     WILLOW                    SALIX spp.
C   38 =          nothing
C   39 =          OTHER
C----------
C
C  GIFFORD PINCHOT
C----------
      DATA (GIFFPC(I,1),I=1,39)/
     &  407.9956,  475.1698,  686.4831,  216.3998,  375.3820,
     &  375.3820,  561.9589,  505.2707, 4691.6337,27357.5212,
     &  133.6603, 1031.5203,  702.1856, 3261.8308, 1548.4147,
     &  452.3985,  409.8811,  531.0073,  465.0811,  368.3722,
     &  179.0706,  182.3045,  133.7965, 1709.7229,10707.3906,
     & 1709.7229,  178.6441,   55.0   ,  503.6619,  503.6619,
     &   89.5535,34749.4736, 1221.9183,  444.5618,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (GIFFPC(I,2),I=1,39)/
     &    6.7834,    6.2472,    6.5393,    6.1700,    6.0880,
     &    6.0880,    6.5507,    6.4743,    7.4671,    8.7211,
     &    4.8456,    7.6616,    5.7025,    7.3717,    6.5503,
     &    5.9690,    6.8908,    5.9643,    6.4772,    6.8267,
     &    3.6238,    3.6676,    6.4050,    5.8887,    8.4670,
     &    5.8887,    4.5852,    5.5   ,    4.9544,    4.9544,
     &    4.2281,    9.1287,    5.8166,    3.9205,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (GIFFPC(I,3),I=1,39)/
     &   -0.5225,   -0.4812,   -0.3740,   -0.6017,   -0.4720,
     &   -0.4720,   -0.4460,   -0.4324,   -0.1989,   -0.1407,
     &   -0.6971,   -0.3599,   -0.3798,   -0.2517,   -0.2700,
     &   -0.4910,   -0.5611,   -0.4083,   -0.4941,   -0.5070,
     &   -0.5730,   -0.4735,   -0.8329,   -0.2286,   -0.1863,
     &   -0.2286,   -0.6746,   -0.95  ,   -0.2085,   -0.2085,
     &   -0.6438,   -0.1417,   -0.2096,   -0.2397,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  MOUNT BAKER / SNOQUALMIE
C----------
      DATA (MTBSNQ(I,1),I=1,39)/
     &  476.6344,  475.1698,  727.8110,  495.7841,  375.3820,
     &  375.3820, 2067.8586,  181.4540, 4691.6337,  211.7962,
     &  121.1392, 1031.5203,  702.1856,  433.7807, 1181.7244,
     &  536.7368,  409.8811,  422.9704,  319.3737,  547.9487,
     &  293.1105, 1089.5045,  133.7965, 1709.7229,10707.3906,
     & 1709.7229,  290.3332,   59.4214,  503.6619,  503.6619,
     &   89.5535,34749.4736,  175.8647,  444.5618,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (MTBSNQ(I,2),I=1,39)/
     &    6.4839,    6.2472,    5.4648,    6.5302,    6.0880,
     &    6.0880,    6.8494,    6.5786,    7.4671,    6.7015,
     &   12.6623,    7.6616,    5.7025,    6.3318,    6.6981,
     &    5.5803,    6.8908,    5.7344,    6.3962,    7.1372,
     &    3.7338,    5.1997,    6.4050,    5.8887,    8.4670,
     &    5.8887,    5.2801,    5.3178,    4.9544,    4.9544,
     &    4.2281,    9.1287,    5.0890,    3.9205,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (MTBSNQ(I,3),I=1,39)/
     &   -0.4685,   -0.4812,   -0.3435,   -0.4110,   -0.4720,
     &   -0.4720,   -0.2590,   -0.6567,   -0.1989,   -0.6739,
     &   -1.2981,   -0.3599,   -0.3798,   -0.4988,   -0.3151,
     &   -0.4101,   -0.5611,   -0.4267,   -0.5698,   -0.4220,
     &   -0.3458,   -0.2566,   -0.8329,   -0.2286,   -0.1863,
     &   -0.2286,   -0.5846,   -1.0367,   -0.2085,   -0.2085,
     &   -0.6438,   -0.1417,   -0.4624,   -0.2397,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  MOUNT HOOD
C----------
      DATA (MTHOOD(I,1),I=1,39)/
     &  223.3492,  475.1698,  432.2186,  290.5142,  375.3820,
     &  375.3820,  247.7348,  255.4638, 4691.6337,  206.3211,
     &  139.7159, 1031.5203,  702.1856, 1333.8176, 1181.7244,
     &  949.1046,  409.8811, 1560.6848,  317.8257, 2478.0988,
     &   76.5170,  484.4591,  133.7965, 1709.7229,10707.3906,
     & 1709.7229,  178.6441,   59.4214,  503.6619,  503.6619,
     &   73.9147,34749.4736,   77.2207,  403.3221,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (MTHOOD(I,2),I=1,39)/
     &    6.3964,    6.2472,    6.2941,    6.4143,    6.0880,
     &    6.0880,    6.1830,    5.5577,    7.4671,    9.1227,
     &    4.0091,    7.6616,    5.7025,    6.6219,    6.6981,
     &    5.8482,    6.8908,    6.2328,    6.8287,    7.0762,
     &    2.2107,    4.5713,    6.4050,    5.8887,    8.4670,
     &    5.8887,    4.5852,    5.3178,    4.9544,    4.9544,
     &    3.9630,    9.1287,    3.5181,    4.3271,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (MTHOOD(I,3),I=1,39)/
     &   -0.6566,   -0.4812,   -0.5028,   -0.4724,   -0.4720,
     &   -0.4720,   -0.6335,   -0.6054,   -0.1989,   -0.8281,
     &   -0.7080,   -0.3599,   -0.3798,   -0.3120,   -0.3151,
     &   -0.3251,   -0.5611,   -0.2541,   -0.6034,   -0.2456,
     &   -0.6365,   -0.3643,   -0.8329,   -0.2286,   -0.1863,
     &   -0.2286,   -0.6746,   -1.0367,   -0.2085,   -0.2085,
     &   -0.8277,   -0.1417,   -0.5894,   -0.2422,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  ROGUE RIVER
C----------
      DATA (ROGRIV(I,1),I=1,39)/
     &  380.2505,  253.9246,  432.2186, 5185.9879,  375.3820,
     &  375.3820,  483.3751,   97.7769, 2245.5741,  155.0   ,
     &  115.8919, 1000.0   , 1631.3764, 1143.6254, 1548.4147,
     &  540.9410,  409.8811,  617.7622,  263.1274,  233.6987,
     &  143.9994,   88.1838,  123.2107, 1709.7229,   83.7456,
     & 1709.7229,  178.6441,   59.4214,  503.6619,  503.6619,
     &   89.5535, 4421.4577,  127.1698,  403.3221,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (ROGRIV(I,2),I=1,39)/
     &    7.3058,    6.6139,    6.2941,    8.7581,    6.0880,
     &    6.0880,    7.2443,    8.8202,    7.1989,    9.1227,
     &    4.9999,    6.5503,    6.4790,    6.1913,    6.5503,
     &    5.6796,    6.8908,    5.5213,    6.9356,    6.9059,
     &    3.5124,    2.8404,    4.1250,    5.8887,    8.3316,
     &    5.8887,    4.5852,    5.3178,    4.9544,    4.9544,
     &    4.2281,    7.0567,    4.8977,    4.3271,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (ROGRIV(I,3),I=1,39)/
     &   -0.5762,   -0.5913,   -0.5028,   -0.2265,   -0.4720,
     &   -0.4720,   -0.5111,   -1.0534,   -0.2400,   -0.8281,
     &   -0.9006,   -0.2700,   -0.2569,   -0.3096,   -0.2700,
     &   -0.4037,   -0.5611,   -0.3508,   -0.6619,   -0.6166,
     &   -0.5511,   -0.7343,   -0.5546,   -0.2286,   -1.0480,
     &   -0.2286,   -0.6746,   -1.0367,   -0.2085,   -0.2085,
     &   -0.6438,   -0.1940,   -0.4668,   -0.2422,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  UMPQUA
C----------
      DATA (UMPQUA(I,1),I=1,39)/
     &  380.2505,  475.1698,  432.2186,  133.8689,  375.3820,
     &  375.3820,  483.3751,   97.7769, 1899.3208,  206.3211,
     &  127.5714, 1031.5203,  544.3721,  433.7807, 1181.7244,
     &  316.1283,  409.8811,  617.7622,  608.6098,  393.9809,
     &  106.0296,   88.1838,  105.1293, 1709.7229, 1076.4271,
     & 1709.7229,  178.6441,   55.0   ,  503.6619,  503.6619,
     &   89.5535, 4421.4577,  139.0727,  202.9745,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (UMPQUA(I,2),I=1,39)/
     &    7.3058,    6.2472,    6.2941,    6.7798,    6.0880,
     &    6.0880,    7.2443,    8.8202,    6.9418,    9.1227,
     &    6.3455,    7.6616,    6.8804,    6.3318,    6.6981,
     &    5.9659,    6.8908,    5.5213,    6.0875,    6.3930,
     &    3.8821,    2.8404,    5.1341,    5.8887,    6.1465,
     &    5.8887,    4.5852,    5.5   ,    4.9544,    4.9544,
     &    4.2281,    7.0567,    5.2062,    3.2936,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (UMPQUA(I,3),I=1,39)/
     &   -0.5762,   -0.4812,   -0.5028,   -0.7375,   -0.4720,
     &   -0.4720,   -0.5111,   -1.0534,   -0.2553,   -0.8281,
     &   -0.8641,   -0.3599,   -0.4638,   -0.4988,   -0.3151,
     &   -0.5748,   -0.5611,   -0.3508,   -0.4163,   -0.4751,
     &   -0.7829,   -0.7343,   -0.7893,   -0.2286,   -0.2822,
     &   -0.2286,   -0.6746,   -0.95  ,   -0.2085,   -0.2085,
     &   -0.6438,   -0.1940,   -0.5409,   -0.3233,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  WILLAMETTE
C----------
      DATA (WILLAM(I,1),I=1,39)/
     &  237.9189,  475.1698,  432.2186,  133.8689,  375.3820,
     &  375.3820,  483.3751,   97.7769, 4691.6337,  206.3211,
     &  105.4453, 1031.5203,  702.1856,  514.1575, 1181.7244,
     &  439.1195,  409.8811, 1012.1267,  395.4976,  192.9609,
     &  160.2171,10099.7209,  133.7965, 1709.7229,10707.3906,
     & 1709.7229,  178.6441,   55.0   ,  503.6619,  503.6619,
     &   73.9147,34749.4736,  139.0727,  444.5618,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (WILLAM(I,2),I=1,39)/
     &    7.7948,    6.2472,    6.2941,    6.7798,    6.0880,
     &    6.0880,    7.2443,    8.8202,    7.4671,    9.1227,
     &    7.9694,    7.6616,    5.7025,    6.3004,    6.6981,
     &    5.8176,    6.8908,    6.0957,    6.4222,    7.3876,
     &    3.3044,    7.6375,    6.4050,    5.8887,    8.4670,
     &    5.8887,    4.5852,    5.5   ,    4.9544,    4.9544,
     &    3.9630,    9.1287,    5.2062,    3.9205,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (WILLAM(I,3),I=1,39)/
     &   -0.7261,   -0.4812,   -0.5028,   -0.7375,   -0.4720,
     &   -0.4720,   -0.5111,   -1.0534,   -0.1989,   -0.8281,
     &   -1.0916,   -0.3599,   -0.3798,   -0.4651,   -0.3151,
     &   -0.4854,   -0.5611,   -0.3083,   -0.5320,   -0.7231,
     &   -0.5299,   -0.1621,   -0.8329,   -0.2286,   -0.1863,
     &   -0.2286,   -0.6746,   -0.95  ,   -0.2085,   -0.2085,
     &   -0.8277,   -0.1417,   -0.5409,   -0.2397,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  SET EQUATION PARAMETERS ACCORDING TO FOREST AND SPECIES.
C----------
      IF(JFOR .EQ. 1)THEN
        P2 = GIFFPC(ISPC,1)
        P3 = GIFFPC(ISPC,2)
        P4 = GIFFPC(ISPC,3)
      ELSEIF (JFOR .EQ. 2) THEN
        P2 = MTBSNQ(ISPC,1)
        P3 = MTBSNQ(ISPC,2)
        P4 = MTBSNQ(ISPC,3)
      ELSEIF (JFOR.EQ.3 .OR. JFOR.EQ.7) THEN
        P2 = MTHOOD(ISPC,1)
        P3 = MTHOOD(ISPC,2)
        P4 = MTHOOD(ISPC,3)
      ELSEIF (JFOR.EQ.4 .OR. JFOR.EQ.10) THEN
        P2 = ROGRIV(ISPC,1)
        P3 = ROGRIV(ISPC,2)
        P4 = ROGRIV(ISPC,3)
      ELSEIF (JFOR.EQ.5 .OR. JFOR.EQ.9) THEN
        P2 = UMPQUA(ISPC,1)
        P3 = UMPQUA(ISPC,2)
        P4 = UMPQUA(ISPC,3)
      ELSE
        P2 = WILLAM(ISPC,1)
        P3 = WILLAM(ISPC,2)
        P4 = WILLAM(ISPC,3)
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
