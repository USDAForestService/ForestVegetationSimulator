      SUBROUTINE HTDBH (IFOR,ISPC,D,H,MODE)
      IMPLICIT NONE
C----------
C  **HTDBH--PN  DATE OF LAST REVISION:  05/09/12
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
C             1 IS OLYMPIC (609)
C             2 IS SIUSLAW (612)
C             3 IS QUINAULT INDIAN RESERVATION (800)
C             4 IS BLM SALAM ADMIN UNIT (708)
C             5 IS BLM EUGENE ADMIN UNIT (709)
C             6 IS BLM COOS BAY ADMIN UNIT (712)
C      MODE = MODE OF OPERATING THIS SUBROUTINE
C             0 IF DIAMETER IS PROVIDED AND HEIGHT IS DESIRED
C             1 IF HEIGHT IS PROVIDED AND DIAMETER IS DESIRED
C----------
C
      REAL OLYMPC(39,3),SIUSLW(39,3)
      REAL MTHOOD(39,3),WILLAM(39,3)
      REAL H,D,P2,P3,P4,HAT3,HAT5
      INTEGER MODE,ISPC,IFOR,I
C
C SPECIES ORDER IN PN VARIANT:
C    1 = ABAM     PACIFIC SILVER FIR        ABIES AMABILIS
C    2 = ABCO     WHITE FIR                 ABIES CONCOLOR
C    3 = ABGR     GRAND FIR                 ABIES GRANDIS
C    4 = ABLA     SUBALPINE FIR             ABIES LASIOCARPA
C    5 = ABMA     CALIFORNIA RED FIR        ABIES MAGNIFICA
C    6 = PISI     SITKA SPRUCE              PICEA SITCHENSIS
C    7 = ABPR     NOBLE FIR                 ABIES PROCERA
C    8 = CANO9    ALASKA CEDAR              CALLITROPSIS NOOTKATENSIS
C    9 = LIDE     INCENSE CEDAR             LIBOCEDRUS DECURRENS
C   10 = PIEN     ENGELMANN SPRUCE          PICEA ENGELMANNII
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
C   24 = BEPA     PAPER BIRCH               BETULA PAPYRIFERA
C   25 = CHCHC4   GIANT CHINQUAPIN          CHRYSOLEPIS CHRYSOPHYLLA
C   26 = POTR5    QUAKING ASPEN             POPULUS TREMULOIDES
C   27 = POBAT    BLACK COTTONWOOD          POPULUS TRICHOCARPA
C   28 = QUGA4    OREGON WHITE OAK          QUERCUS GARRYANA
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
C  OLYMPIC (ALSO USED FOR QUINAULT)
C
      DATA (OLYMPC(I,1),I=1,39)/
     &  697.6316,  604.8450,  356.1148,   89.0298,  202.8860,
     & 3844.3880,  483.3751, 1220.0963, 4691.6337,  206.3211,
     &  100.0   , 1031.5203,  702.1856,  433.7807, 1181.7244,
     & 1091.8526,  409.8811,  665.0944,  609.4235,  170.2653,
     &  600.0957,  139.4551,  139.4551, 1709.7229,10707.3906,
     & 1709.7229,  178.6441,   89.4301,  503.6619,  503.6619,
     &   89.5535,34749.4736,  127.1698,  403.3221,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (OLYMPC(I,2),I=1,39)/
     &    6.6807,    5.9835,    6.4100,    6.9507,    8.7469,
     &    7.0680,    7.2443,    7.2995,    7.4671,    9.1227,
     &    6.0   ,    7.6616,    5.7025,    6.3318,    6.6981,
     &    5.2936,    6.8908,    5.5002,    5.5919,   10.0684,
     &    3.8297,    4.6989,    4.6989,    5.8887,    8.4670,
     &    5.8887,    4.5852,    6.6321,    4.9544,    4.9544,
     &    4.2281,    9.1287,    4.8977,    4.3271,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (OLYMPC(I,3),I=1,39)/
     &   -0.4161,   -0.3789,   -0.5572,   -0.9871,   -0.8317,
     &   -0.2122,   -0.5111,   -0.3211,   -0.1989,   -0.8281,
     &   -0.86  ,   -0.3599,   -0.3798,   -0.4988,   -0.3151,
     &   -0.2648,   -0.5611,   -0.3246,   -0.3841,   -0.8791,
     &   -0.2380,   -0.7682,   -0.7682,   -0.2286,   -0.1863,
     &   -0.2286,   -0.6746,   -0.8876,   -0.2085,   -0.2085,
     &   -0.6438,   -0.1417,   -0.4668,   -0.2422,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C
C  SIUSLAW (ALSO USED FOR BLM COOS BAY UNIT
C
      DATA (SIUSLW(I,1),I=1,39)/
     &  697.6316,  604.8450,  432.2186,  133.8689,  202.8860,
     &  708.7788,  483.3751, 1220.0963, 4691.6337,  206.3211,
     &  100.0   , 1031.5203,  702.1856,  514.1575, 1181.7244,
     &  407.1595,  409.8811,  227.1400, 1196.6191,  170.2653,
     &   92.2964,  254.8634,  254.8634, 1709.7229,10707.3906,
     & 1709.7229,  178.6441,   89.4301,  503.6619,  503.6619,
     &   89.5535,34749.4736,  139.0727,  403.3221,   55.0   ,
     &   73.3348,  149.5861, 1709.7229, 1709.7229/
C
      DATA (SIUSLW(I,2),I=1,39)/
     &    6.6807,    5.9835,    6.2941,    6.7798,    8.7469,
     &    5.7677,    7.2443,    7.2995,    7.4671,    9.1227,
     &    6.0   ,    7.6616,    5.7025,    6.3004,    6.6981,
     &    7.2885,    6.8908,    6.1092,    5.7904,   10.0684,
     &    4.1890,    3.8495,    3.8495,    5.8887,    8.4670,
     &    5.8887,    4.5852,    6.6321,    4.9544,    4.9544,
     &    4.2281,    9.1287,    5.2062,    4.3271,    5.5   ,
     &    2.6548,    2.4231,    5.8887,    5.8887/
C
      DATA (SIUSLW(I,3),I=1,39)/
     &   -0.4161,   -0.3789,   -0.5028,   -0.7375,   -0.8317,
     &   -0.3629,   -0.5111,   -0.3211,   -0.1989,   -0.8281,
     &   -0.86  ,   -0.3599,   -0.3798,   -0.4651,   -0.3151,
     &   -0.5908,   -0.5611,   -0.6009,   -0.2906,   -0.8791,
     &   -0.9830,   -0.4149,   -0.4149,   -0.2286,   -0.1863,
     &   -0.2286,   -0.6746,   -0.8876,   -0.2085,   -0.2085,
     &   -0.6438,   -0.1417,   -0.5409,   -0.2422,   -0.95  ,
     &   -1.2460,   -0.1800,   -0.2286,   -0.2286/
C----------
C  MOUNT HOOD --- USE FOR BLM SALEM UNIT
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
C  WILLAMETTE --- USE FOR BLM EUGENE ADMIN UNIT
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
      IF(IFOR.EQ.1 .OR. IFOR.EQ.3) THEN
        P2 = OLYMPC(ISPC,1)
        P3 = OLYMPC(ISPC,2)
        P4 = OLYMPC(ISPC,3)
      ELSEIF(IFOR.EQ.4) THEN
        P2 = MTHOOD(ISPC,1)
        P3 = MTHOOD(ISPC,2)
        P4 = MTHOOD(ISPC,3)
      ELSEIF(IFOR.EQ.5) THEN
        P2 = WILLAM(ISPC,1)
        P3 = WILLAM(ISPC,2)
        P4 = WILLAM(ISPC,3)
      ELSE
        P2 = SIUSLW(ISPC,1)
        P3 = SIUSLW(ISPC,2)
        P4 = SIUSLW(ISPC,3)
      ENDIF
      IF(MODE .EQ. 0) H=0.
      IF(MODE .EQ. 1) D=0.
C----------
C  PROCESS ACCORDING TO MODE
C  DUB TRUNCATED FOR SITKA SPRUCE ON OLY NF FOR DBH>100
C----------
      IF((IFOR.EQ.2 .OR. IFOR.EQ.4) .and. ISPC.EQ.16) GO TO 100
      IF(MODE .EQ. 0) THEN
        IF(D .GE. 3.) THEN
          H = 4.5 + P2 * EXP(-1.*P3*D**P4)
          IF(D.GE.100.and.ISPC.EQ.6.and.(IFOR.EQ.1 .OR. IFOR.EQ.3)) 
     &    H = 0.25*D + 248
        ELSE
          H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
        ENDIF
      ELSE
        HAT3 = 4.5 + P2 * EXP(-1.*P3*3.0**P4)
        IF(H .GE. HAT3) THEN
          D = EXP( ALOG((ALOG(H-4.5)-ALOG(P2))/(-1.*P3)) * 1./P4)
          IF(H.GE.273.and.ISPC.EQ.6.and.(IFOR.EQ.1 .OR. IFOR.EQ.3)) 
     &    D=(H-248)/.25
        ELSE
          D = (((H-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))-4.51))+0.3
        ENDIF
      ENDIF
      GO TO 200
C----------
C  DOUGLAS-FIR ON THE SIUSLAW SPLINES AT 5.0 INCHES.
C----------
  100 CONTINUE
      IF(MODE .EQ. 0) THEN
        IF(D .GE. 5.) THEN
          H = 4.5 + P2 * EXP(-1.*P3*D**P4)
        ELSE
          H = ((4.5+P2*EXP(-1.*P3*(5.**P4))-4.51)*(D-0.3)/4.7)+4.51
        ENDIF
      ELSE
        HAT5 = 4.5 + P2 * EXP(-1.*P3*5.0**P4)
        IF(H .GE. HAT5) THEN
          D = EXP( ALOG((ALOG(H-4.5)-ALOG(P2))/(-1.*P3)) * 1./P4)
        ELSE
          D = (((H-4.51)*4.7)/(4.5+P2*EXP(-1.*P3*(5.**P4))-4.51))+0.3
        ENDIF
      ENDIF
C
  200 CONTINUE
      RETURN
      END
