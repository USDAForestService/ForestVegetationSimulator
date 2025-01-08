      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C THIS SUBROUTINE LOADS THE SITELG ARRAY WITH A SITE INDEX FOR EACH
C SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.
C
C CALLED BY INITRE
C----------
COMMONS
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
      INCLUDE 'METRIC.F77'
C
C
COMMONS
C
C
      LOGICAL DEBUG
      REAL SICOEF1(MAXSP,MAXSP),SICOEF2(MAXSP,MAXSP),
     &          BAMAX1(MAXSP)
      INTEGER METHB8,METHC8
      REAL A1(17), A2(17), A3(17), A4(17), A5(17)
      REAL THT1(17), THT2(17), THT3(17), THT4(17)
      INTEGER OSP(MAXSP)

      DOUBLE PRECISION KD
      INTEGER J,I,JJ,K,KSP,IEQAG
      CHARACTER FORST*2,DIST*2,PROD*2,VAR*2,VOLEQ*11
      INTEGER IFIASP,ERRFLAG,ISPC,IREGN,KFORST
      REAL AGEBH,HTM,SIM,SLP,DIFF,SIMIN,AGEDIF
      LOGICAL LONT
C
      DATA BAMAX1/
     & 150.,   150.,   240.,   240.,   240.,   190.,   240.,
     & 240.,   180.,   200.,   200.,   240.,   150.,   150.,
     & 150.,   150.,   130.,   150.,   150.,   200.,   150.,
     & 150.,   150.,   150.,   150.,   150.,   150.,   150.,
     & 150.,   160.,   160.,   160.,   160.,   160.,   160.,
     & 130.,   160.,   160.,   160.,   130.,   130.,   150.,
     & 150.,   150.,   150.,   160.,   150.,   150.,   150.,
     & 150.,   150.,   150.,   150.,   150.,   150.,   150.,
     & 170.,   150.,   140.,   150.,   150.,   150.,   150.,
     & 150.,   150.,   150.,   150.,   150.,
     & 150.,   240.,   190.,   180. /
C
      DATA ((SICOEF1(I,J),I=1,MAXSP),J=1,MAXSP)/
     &  2*.000,2*-.083,35*.000,3*-4.661,-11.774,29*.000,
     &  2*.000,2*-.083,35*.000,3*-4.661,-11.774,29*.000,
     &  2*.081,2*.0,3.926,2*14.453,11*.0,2*13.601,19*.0,3*-20.194,26*.0,
     &      0.081,3.926,14.453,.0,
     &  2*.081,2*.0,3.926,2*14.453,11*.0,2*13.601,19*.0,3*-20.194,26*.0,
     &      0.081,3.926,14.453,.0,
     &  2*.0,2*-4.094,.0,2*10.9,11*.0,2*5.264,13*.0,3*4.678,3*.000,
     &      3*-9.004,28*.00,10.9,.0,
     &  2*.0,2*-18.249,-13.974,5*.0,-19.486,28*.0,3*-7.893,27*.0,
     &   -13.974,2*.0,
     &  2*.0,2*-18.249,-13.974,5*.0,-19.486,28*.0,3*-7.893,27*.0,
     &      -13.974,2*.0,
     &  8*.0,-7.212,.0,-15.196,28*.0,3*-1.366,12.096,28*.0,-7.212,
     &  7*.000,6.175,.000,2.846,62*.000,
     &  8*.000,-3.700,.000,-12.708,60*.000,-3.700,
     &  5*.000,2*10.904,8.205,.000,6.434,60*.000,10.904,.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  18*.0,2*8.379,3*-.953,-15.228,.253,3*7.441,4.812,4*.0,
     &     3*-37.214,3*.000,3*-4.346,30*.000,
     &  18*.0,2*8.379,3*-.953,-15.228,.253,3*7.441,4.812,4*.0,
     &     3*-37.214,3*.000,3*-4.346,30*.000,
     &  72*.000,
     &  72*.000,
     &  2*.0,2*-17.873,-5.606,9*.0,2*-9.376,4*.0,3*6.729,-2.119,
     &      -2.628,3*3.4,11.937,4*.0,3*-11.597,3*.0,3*-18.897,-16.538,
     &      26*.0,-5.606,2*.0,
     &  2*.0,2*-17.873,-5.606,9*.0,2*-9.376,4*.0,3*6.729,-2.119,
     &      -2.628,3*3.4,11.937,4*.0,3*-11.597,3*.0,3*-18.897,-16.538,
     &      26*.0,-5.606,2*.0,
     &  14*.0,2*.946,2*.0,2*-7.973,3*.0,-6.136,-5.273,3*-1.384,
     &      3.405,4*.0,3*-9.013,3*.0,3*-.214,-5.084,29*.000,
     &  14*.0,2*.946,2*.0,2*-7.973,3*.0,-6.136,-5.273,3*-1.384,
     &      3.405,4*.0,3*-9.013,3*.0,3*-.214,-5.084,29*.000,
     &  14*.0,2*.946,2*.0,2*-7.973,3*.0,-6.136,-5.273,3*-1.384,
     &      3.405,4*.0,3*-9.013,3*.0,3*-.214,-5.084,29*.000,
     &  14*.0,2*11.436,2*.0,2*2.022,3*5.216,.0,-4.758,3*2.045,1.885,
     &      4*.0,3*-6.197,3*.0,3*-9.885,-10.258,29*.000,
     &  14*.0,2*-.256,2*.0,2*2.652,3*4.856,4.768,.0,3*4.931,9.642,
     &      4*.0,3*-15.079,3*.0,3*3.165,-1.913,29*.000,
     &  14*.0,2*-7.746,2*.0,2*-3.563,3*1.262,-2.119,-4.970,3*.0,
     &     1.006,4*.0,3*-11.199,3*.0,8.187,2*-2.435,-11.199,29*.0,
     &  14*.0,2*-7.746,2*.0,2*-3.563,3*1.262,-2.119,-4.970,3*.0,
     &     1.006,4*.0,3*-11.199,3*.0,8.187,2*-2.435,-11.199,29*.0,
     &  14*.0,2*-7.746,2*.0,2*-3.563,3*1.262,-2.119,-4.970,3*.0,
     &     1.006,4*.0,3*-11.199,3*.0,8.187,2*-2.435,-11.199,29*.0,
     &  14*.0,2*-5.428,2*.0,2*-15.895,3*-3.717,-2.162,-11.645,
     &      3*-1.143,5*.0,3*-34.762,3*.0,3*-23.105,-16.344,29*.000,
     &  33*.000,3*-1.235,36*.0,
     &  33*.000,3*-1.235,36*.0,
     &  33*.000,3*-1.235,36*.0,
     &  33*.000,3*-1.235,36*.0,
     &  4*.0,-5.576,9*.0,2*23.568,2*.0,2*10.41,3*7.934,6.247,12.35,
     &      3*10.199,22.442,4*1.321,6*.0,-8.189,2*-16.187,-16.401,
     &      26*.00,-5.576,2*.0,
     &  4*.0,-5.576,9*.0,2*23.568,2*.0,2*10.41,3*7.934,6.247,12.350,
     &      3*10.199,22.442,4*1.321,6*.0,-8.189,2*-16.187,-16.401,
     &      26*.00,-5.576,2*.0,
     &  4*.0,-5.576,9*.0,2*23.568,2*.0,2*10.41,3*7.934,6.247,12.350,
     &      3*10.199,22.442,4*1.321,6*.0,-8.189,2*-16.187,-16.401,
     &      26*.00,-5.576,2*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  2*4.385,2*16.194,8.276,2*7.893,1.605,6*.0,2*4.818,2*.0,
     &      2*16.179,3*.239,9.945,-3.615,3*-11.564,19.238,4*.0,3*7.992,
     &      4*.0,2*2.223,-19.532,25*.000,4.385,8.276,7.893,.0,
     &  2*4.385,2*16.194,8.276,2*7.893,1.605,6*.0,2*4.818,2*.0,
     &      2*16.179,3*.239,9.945,-3.615,3*2.868,19.238,4*.0,3*13.856,
     &      3*.0,-2.271,2*.0,8.500,25*.000,4.385,8.276,7.893,.0,
     &  2*4.385,2*16.194,8.276,2*7.893,1.605,6*.0,2*4.818,2*.0,
     &      2*16.179,3*.239,9.945,-3.615,3*2.868,19.238,4*.0,3*13.856,
     &      3*.0,-2.271,2*.0,8.500,25*.000,4.385,8.276,7.893,.0,
     &  2*9.002,5*.0,-17.109,10*.0,2*13.612,3*4.769,9.696,1.911,
     &      3*10.199,13.342,4*.0,3*13.069,3*.0,13.926,2*-8.947,26*.0,
     &      9.002,3*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  72*.0,
     &  2*.000,2*-.083,35*.000,3*-4.661,-11.774,29*.000,
     &  2*.0,2*-4.094,.0,2*10.9,11*.0,2*5.264,13*.0,3*4.678,3*.000,
     &      3*-9.004,28*.00,10.9,.0,
     &  2*.0,2*-18.249,-13.974,5*.0,-19.486,28*.0,3*-7.893,27*.0,
     &   -13.974,2*.0,
     &  7*.000,6.175,.000,2.846,62*.000/
      DATA ((SICOEF2(I,J),I=1,MAXSP),J=1,MAXSP)/
     &  2*.0,2*1.028,35*.0,3*1.063,1.308,29*.0,
     &  2*.0,2*1.028,35*.0,3*1.063,1.308,29*.0,
     &  2*.973,2*.0,.959,2*.792,11*.0,2*.761,19*.0,3*1.247,26*.00,
     &    .973,.959,.792,.0,
     &  2*.973,2*.0,.959,2*.792,11*.0,2*.761,19*.0,3*1.247,26*.00,
     &    .973,.959,.792,.0,
     &  2*0,2*1.043,.0,2*.78,11*.0,2*.939,13*.0,3*.839,3*.0,3*1.088,
     &     28*.000,.78,.0,
     &  2*.0,2*1.263,1.282,5*.0,1.787,28*.0,1.000,29*.000,1.282,2*.0,
     &  2*.0,2*1.263,1.282,5*.0,1.787,28*.0,1.000,29*.000,1.282,2*.0,
     &  8*.0,1.168,.0,1.852,28*.0,3*.851,.707,28*.00,1.168,
     &  7*.0,.856,.0,.769,62*.000,
     &  8*.0,1.300,.0,1.975,60*.0,1.300,
     &  5*.0,2*.560,.540,.000,.506,62*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  18*.0,2*.894,3*1.007,1.332,.99,3*.961,.887,4*.0,3*1.579,
     &      3*.0,3*.902,30*.000,
     &  18*.0,2*.894,3*1.007,1.332,.99,3*.961,.887,4*.0,3*1.579,
     &      3*.0,3*.902,30*.000,
     &  72*.0,
     &  72*.0,
     &  2*.0,2*1.314,1.065,9*.0,2*1.119,4*.0,3*.844,1.048,.991,
     &     3*.954,.751,4*.0,3*1.114,3*.0,3*1.168,1.215,26*.0,1.065,2*.0,
     &  2*.0,2*1.314,1.065,9*.0,2*1.119,4*.0,3*.844,1.048,.991,
     &     3*.954,.751,4*.0,3*1.114,3*.0,3*1.168,1.215,26*.0,1.065,2*.0,
     &  14*.0,2*.993,2*.0,2*1.185,3*.0,1.176,1.086,3*1.096,.916,
     &      4*.0,3*1.36,3*.0,3*.896,1.066,29*.000,
     &  14*.0,2*.993,2*.0,2*1.185,3*.0,1.176,1.086,3*1.096,.916,
     &      4*.0,3*1.36,3*.0,3*.896,1.066,29*.000,
     &  14*.0,2*.993,2*.0,2*1.185,3*.0,1.176,1.086,3*1.096,.916,
     &      4*.0,3*1.36,3*.0,3*.896,1.066,29*.000,
     &  14*.0,2*.751,2*.0,2*.954,3*.850,.0,.998,3*.965,.872,4*.0,3*.992,
     &      3*.0,3*.994,1.058,29*.000,
     &  14*.0,2*1.01,2*.0,2*1.009,3*.921,1.002,.0,3*.992,.828,4*.0,
     &      3*1.221,3*.0,3*.876,1.001,29*.000,
     &  14*.0,2*1.041,2*.0,2*1.048,3*.912,1.036,1.008,3*.0,.88,4*.0,
     &      3*1.098,3*.0,.708,2*.849,1.098,29*.000,
     &  14*.0,2*1.041,2*.0,2*1.048,3*.912,1.036,1.008,3*.0,.88,4*.0,
     &      3*1.098,3*.0,.708,2*.849,1.098,29*.000,
     &  14*.0,2*1.041,2*.0,2*1.048,3*.912,1.036,1.008,3*.0,.88,4*.0,
     &      3*1.098,3*.0,.708,2*.849,1.098,29*.000,
     &  14*.0,2*1.128,2*.0,2*1.332,3*1.092,1.147,1.208,3*1.136,5*.0,
     &      3*1.549,3*.0,3*1.201,1.225,29*.000,
     &  33*.000,3*.935,36*.000,
     &  33*.000,3*.935,36*.000,
     &  33*.000,3*.935,36*.000,
     &  33*.000,3*.935,36*.000,
     &  4*.0,1.192,9*.0,2*.633,2*.0,2*.898,3*.88,1.008,.819,3*.911,.646,
     &     4*1.070,.0,2*1.0,3*.0,1.025,2*1.168,1.255,26*.000,1.192,2*.0,
     &  4*.0,1.192,9*.0,2*.633,2*.0,2*.898,3*.88,1.008,.819,3*.911,.646,
     &     4*1.070,1.000,5*.0,1.025,2*1.168,1.255,26*.000,1.192,2*.0,
     &  4*.0,1.192,9*.0,2*.633,2*.0,2*.898,3*.88,1.008,.819,3*.911,.646,
     &     4*1.070,1.000,5*.0,1.025,2*1.168,1.255,26*.000,1.192,2*.0,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  2*.941,2*.802,.919,2*1.0,1.175,6*.0,2*1.109,2*.0,2*.856,3*1.116,
     &      1.006,1.142,3*1.412,.833,4*.0,3*.976,4*.0,2*.979,1.403,
     &      25*.0,.941,.919,1.175,.0,
     &  2*.941,2*.802,.919,2*1.0,1.175,6*.0,2*1.109,2*.0,2*.856,
     &      3*1.116,1.006,1.142,3*1.178,.833,4*.0,3*.856,3*.0,1.021,
     &      2*.0,.950,25*.0,.941,.919,1.175,.0,
     &  2*.941,2*.802,.919,2*1.0,1.175,6*.0,2*1.109,2*.0,2*.856,
     &      3*1.116,1.006,1.142,3*1.178,.833,4*.0,3*.856,3*.0,1.021,
     &      2*.0,.950,25*.0,.941,.919,1.175,.0,
     &  2*.756,5*.0,1.414,10*.0,2*.823,3*.938,.945,.999,3*.911,.816,
     &      4*.0,3*.797,3*.0,.713,2*1.053,26*.000,.756,3*.0,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  72*.000,
     &  2*.0,2*1.028,35*.0,3*1.063,1.308,29*.0,
     &  2*0,2*1.043,.0,2*.78,11*.0,2*.939,13*.0,3*.839,3*.0,3*1.088,
     &     28*.000,.78,.0,
     &  2*.0,2*1.263,1.282,5*.0,1.787,28*.0,1.000,29*.000,1.282,2*.0,
     &  7*.0,.856,.0,.769,62*.000/
C-----------
C  DATA STATEMENTS FOR ONTARIO SITE INDEX EQUATIONS
C   1=Balsam fir (SP8), 2=Black spruce (SP9,72), 3=Tamarak (SP10)
C    4=Black ash (SP15), 5=Black cherry (SP20), 6=Elm (SP21)
C    7=Yellow birch (SP24), 8=Basswood (SP25), 9=Hard maple (SP26)
C    10=White ash (SP29), 11=Red oak (SP34), 12=White birch (sp43)
C    13=White spruce (SP6,71),
C    14=Jack Pine(SP1,69), 15=Red Pine (Sp2&3),
C    16=White pine (SP5,70), 17=Aspen (SP41)
C-----------
      DATA A1/
     &  0.0061, 9.0023, 0.6464, 0.2388, 0.1073, 0.1898,
     &  0.1817, 0.1921, 0.1984, 0.1728, 0.1692, 0.5119,
     & 40.6506,23.7086,22.5992,24.4573,28.0109/
      DATA A2/
     &  1.3539, 1.4753, 1.000,  1.1583, 1.3455, 1.2186,
     &  1.243,  1.201, 1.2089,  1.256,  1.2648, 1.0229,
     &  5.6605,20.5596,18.7782,18.1841,24.5365/
      DATA A3/
     &  -0.00019, 0.7996,-0.0225, -0.0102, -0.007, -0.011,
     &  -0.011,  -0.01,  -0.011, -0.011, -0.011, -0.0167,
     &   1.2544, 17.3764,15.1961,12.4653,12.4653/
      DATA A4/
     &  -1.0286, 0.3976, -1.1129, -1.8455, -3.3034, -2.6865,
     &  -3.0184, -2.3009, -2.4917, -3.3605, -3.4334, -1.0284,
     &  -0.1567,14.2208, 11.5513,  6.7618, 16.9224/
      DATA A5/
     &  -0.0723, 19.26275, 0.0, -0.1883, -0.3899, -0.2717,
     &  -0.318, -0.2331, -0.2542, -0.3452, -0.3557, -0.0049,
     &   5*0.0/
      DATA THT1/13*0.0,22.56,23.0,24.42,28.53/
      DATA THT2/13*0.0,19.46,19.78,19.24,25.02/
      DATA THT3/13*0.0,16.36,16.56,14.06,21.12/
      DATA THT4/13*0.0,13.32,13.04,8.3,17.3/


C  SPECIES MAPPINGS FOR ONTARIO
C     CODES AS ABOVE.
      DATA OSP/
     & 14,15,15,15,16,13,13, 1, 2, 3,
     &  2, 9, 2, 2, 4, 4,17, 9, 9, 5,
     &  6, 6, 6, 7, 8, 9, 9, 9,10,11,
     & 11,11,11,11,11,11, 9, 9, 9,17,
     & 17,17,12, 9, 9, 9, 9, 9, 9, 9,
     &  9, 9, 9, 9, 9, 9, 9, 5, 9, 9,
     &  5, 5, 5,17,17,17, 9, 4,14,16,
     & 13, 2/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'SITSET',6,ICYC)
c = pre LS-NEW code that may be relevant

c        IF (KODFOR .EQ. 910 .OR. KODFOR .EQ. 907 .OR.
c     &      KODFOR .EQ. 915 .OR. KODFOR .EQ. 916) THEN
c      XMIN(1)=4.0
c      XMIN(2)=4.0
c      XMIN(8)=1.0
c      XMIN(11)=1.0
c      XMIN(13)=1.5
c      XMIN(14)=1.5
c      DO 20 I=15,68
c        XMIN(I)=2.0
c   20 CONTINUE
c         XMIN(69)=4.0
c     endif
C----------
C  MINIMUM DBH VALUES, BY FOREST
C----------
      DO I=1,MAXSP
      SELECT CASE(KODFOR)
      CASE(903)
        IF(I.LE.14.OR.I.GE.68)THEN
          BFMIND(I)=7.0
          DBHMIN(I)=4.0
        ELSE
          DBHMIN(I)=5.
          IF(I.LT.40.OR.I.GT.42)BFMIND(I)=9.
        ENDIF
      CASE(904,910,913,915,916)
        IF(I.GT.14.OR.I.GE.68)DBHMIN(I)=5.
      CASE(907)
        IF(I.GT.14)DBHMIN(I)=5.
        IF(I.GE.40.AND.I.LE.42)BFMIND(I)=9.
      END SELECT
      ENDDO
C----------
C     DETERMINE IF WE USE THE NEW ONTARIO EQUATIONS.
C----------
      LONT = .FALSE.
      IF ((KODFOR .EQ. 915 .OR. KODFOR.EQ. 916) .AND.
     &     OSP(ISISP) .GT. 0)
     &   LONT = .TRUE.
C----------
C  WRITE ERROR MESSAGE IF NO SITE SPECIES AND/OR SITE CODE HAS
C  BEEN ENTERED.
C     NOTE THAT IF USING ONTARIO EQUATIONS, VALUES FOR SITEAR<0
C     ARE OKAY, BECAUSE THEY ARE ACTUALLY A HEIGHT.
C----------
      IF(ISISP .LE. 0 .OR. SITEAR(ISISP) .EQ. 0.0 .OR.
     &  (SITEAR(ISISP) .LT. 0.0 .AND. .NOT. LONT)) THEN
        WRITE(JOSTND,1)
    1   FORMAT(/,3(' ***************'/),' WARNING: SITE SPECIES ',
     &    'AND/OR SITE CODE IS MISSING.  A DEFAULT IS BEING ASSIGNED.',
     &    ' THIS PROJECTION SHOULD NOT BE TAKEN SERIOUSLY.',/,
     &    3(' ***************'/),/)
      ENDIF
C----------
C  SET DEFAULT SITE SPECIES OF RED PINE NATURAL AND INDEX OF 60
C  IF MISSING.
C----------
      IF(ISISP .LE. 0) ISISP=3
      IF(SITEAR(ISISP) .EQ. 0.0 .OR.
     &  (SITEAR(ISISP) .LT. 0.0 .AND. .NOT. LONT)) SITEAR(ISISP)=60.

      IF (LONT .AND. SITEAR(ISISP) .LT. 0.0) THEN
C----------
C  START OF NEW ONTARIO SITE INDEX EQUATIONS
C        WE ONLY NEED TO USE THESE IF USERS ENTERED A TOP HEIGHT INSTEAD
C        OF A SITE INDEX.
C----------
C       AGEBH = Age2bh (use 6 for softwoods, 4 for hardwoods)
C       ieqag = USE AGE OF THE EQUATION, USUALLY 50, OR 56
C       HTM = USE TOP HEIGHT ENTERED IN THE KEYWORD.
C
         KSP = OSP(ISISP)
       AGEBH = 6
       IF (KSP .GE. 3 .AND. KSP .LE. 12) AGEBH = 4
       IEQAG = 56
       IF (KSP .GT. 1 .AND. KSP .LE. 11 .AND. KSP .NE. 7) IEQAG = 50
       AGEDIF = REAL(IEQAG - AGEBH)
c           remember that if we get here, then SITEAR is negative!
       HTM = -SITEAR(ISISP) * FTtoM

       IF (KSP.EQ.2) THEN
C            BLACK SPRUCE
C            SIBH50 = a1+0.4396*(topht-1.3)+a2*LN(topht-1.3)-a3*LN(age-age2bh)-a4*(LN(age-age2bh))^2+a5*(topht-1.3)/(age-age2bh)
            IF (HTM .LE. 1.3 .OR. AGEDIF .LE. 0.0) THEN
             SIM = 0.0
          ELSE
               SIM = A1(KSP) + 0.4396*(HTM-1.3) + A2(KSP)*LOG(HTM-1.3) -
     &             A3(KSP)*LOG(AGEDIF) - A4(KSP)*(LOG(AGEDIF))**2.0 +
     &              A5(KSP)*(HTM-1.3)/(AGEDIF)
          ENDIF

         ELSE IF (KSP .EQ. 3) THEN
C           TAMARAK
C           SI = (4.5+A1*(topht*3.28-4.5)*(1-exp(A3*(age-y2bh)))^ (A4))/3.28
           SIM = (4.5 + A1(KSP) * (HTM*3.28-4.5) *
     &           (1.0-exp(A3(KSP)*AGEDIF))**A4(KSP))/3.28

         ELSE IF (KSP .EQ. 1) THEN
C           BALSAM FIR
C          SI = (4.5 + A1*(topht*3.28)^A2*(1-exp(A3*(age-y2bh)))^ (A4*(topht*3.28)^(A5)))/3.28
           SIM = (4.5 + A1(KSP)*(HTM*3.28)**A2(KSP) *
     &           (1.0-exp(A3(KSP)*(AGEDIF)))**
     &           (A4(KSP)*(HTM*3.28)**(A5(KSP))))/3.28

         ELSE IF (KSP .EQ. 13) THEN
C          WHITE SPRUCE
C           SI = 1.3 + (1/(A1*(topht-1.3)^A2*(1-K^((age-age2bh)/Bhindage))^(A3 *(topht-1.3)^(A4))))
C           K =1-(1/(A1*(topht-1.3)^(A2+1)))^(1/(A3*(topht-1.3)^(A4)))
C           NOTE: won't work well at heights above 28 m
          KD = 1.- (1./(A1(KSP)*(HTM-1.3)**(A2(KSP)+1)))**
     &            (1.0/(A3(KSP)*(HTM-1.3)**(A4(KSP))))
          SIM = REAL(1.3 + (1.0/(A1(KSP)*(HTM-1.3)**A2(KSP)*
     &         (1.0-KD**(AGEDIF/50.))**(A3(KSP)*(HTM-1.3)**(A4(KSP))))))

         ELSE IF (KSP .LT. 14) THEN
C          MOST OTHER DEFINED SPECIES
C          SI = (4.5+a1*(topht*3.28-4.5)^a2*(1-exp(a3*(age-y2bh)))^(a4*(topht*3.28 - 4.5)^(a5)))/3.28
           SIM = (4.5 + A1(KSP)*(HTM*3.28-4.5)**A2(KSP) *
     &          (1.-exp(A3(KSP)*AGEDIF))**(A4(KSP) *
     &          (HTM*3.28 - 4.5)**(A5(KSP))))/3.28

         ELSE IF (KSP .GE. 14) THEN
C          SPECIES FOR WHICH WE DON'T HAVE A CLOSED FORM.
C           SI HAS BEEN CALCULATED FOR 4 SITE TYPES (I.E., TOP HEIGHTS).
C          IF THE USER ENTERS A DIFFERENT ONE, THEN WE WILL JUST INTERPOLATE
C          OR EXTRAPOLATE TO GET THE SI. NOTE THAT THE HEIGHTS ARE IN DECREASING
C          ORDER.

            IF ((HTM .GE. THT4(KSP) .AND. HTM .LT. THT3(KSP)) .OR.
     &          HTM .LT. THT4(KSP)) THEN
             SLP = (A3(KSP) - A4(KSP)) / (THT3(KSP) - THT4(KSP))
             DIFF = HTM - THT4(KSP)
             SIMIN = A4(KSP)

            ELSEIF (HTM .GE. THT3(KSP) .AND. HTM .LT. THT2(KSP)) THEN
             SLP = (A2(KSP) - A3(KSP)) / (THT2(KSP) - THT3(KSP))
             DIFF = HTM - THT3(KSP)
             SIMIN = A3(KSP)

            ELSEIF ((HTM .GE. THT2(KSP) .AND. HTM .LT. THT1(KSP)) .OR.
     &               HTM .GE. THT1(KSP)) THEN
             SLP = (A1(KSP) - A2(KSP)) / (THT1(KSP) - THT2(KSP))
             DIFF = HTM - THT2(KSP)
             SIMIN = A2(KSP)
          ENDIF
            SIM = DIFF * SLP + SIMIN

         ENDIF
         IF (SIM .GT. 0.0) SITEAR(ISISP) = SIM * MtoFT
C----------
C  END OF ONTARIO SECTION
C----------
      ENDIF
C----------
C  COMPUTE SPECIES SITE FROM STAND SITE FOR ALL SPECIES WHICH HAVE
C  COEFFICIENTS IN THE PARAMETER FILE.  WHEN COEFFICIENTS ARE NOT
C  AVAILABLE FOR A SPECIES, THAT SPECIES' SITE WILL BE ZERO.
C----------
      DO 5 I=1,MAXSP
       IF(SITEAR(I) .LE. .0001) SITEAR(I) = SICOEF1(ISISP,I)
     &         + SICOEF2(ISISP,I)*SITEAR(ISISP)
    5 CONTINUE
C----------
C  FOR SPECIES WHICH DID NOT HAVE SITES FIGURED ABOVE, ATTEMPT A
C  CONVERSION THRU ASPEN.  IF COEFFICIENTS ARE NOT AVAILABLE, SITES
C  WILL BE LEFT AS ZERO.
C----------
      IF(SITEAR(41) .GT. .0001) THEN
        DO 10 I=1,MAXSP
         IF(SITEAR(I) .LE. .0001) SITEAR(I) = SICOEF1(41,I)
     &      + SICOEF2(41,I)*SITEAR(41)
   10   CONTINUE
      ENDIF
C----------
C  NOW SET ALL UNSET SPECIES SITES TO THE STAND SITE INDEX
C----------
      DO 15 I=1,MAXSP
        IF(SITEAR(I) .LT. .0001) SITEAR(I) = SITEAR(ISISP)
        IF(SDIDEF(I) .LE. 0.) THEN
          IF(BAMAX .GT. 0.)THEN
            SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
          ELSE
            SDIDEF(I)=BAMAX1(I)/(0.5454154*(PMSDIU/100.))
          ENDIF
        ENDIF
   15 CONTINUE
C----------
C  LOAD VOLUME DEFAULT MERCH. SPECS.
C----------
      DO ISPC=1,MAXSP
      IF(DBHMIN(ISPC).LE.0.)THEN               !SET **DBHMIN** DEFAULT
        IF(ISPC.LE.14.OR.ISPC.GT.68)THEN       !SOFTWOODS
          DBHMIN(ISPC)=5.
        ELSE                                   !HARDWOODS
          SELECT CASE(IFOR)
          CASE(2)
            DBHMIN(ISPC)=5.
            IF(ISPC.GE.40.AND.ISPC.LE.42)DBHMIN(ISPC)=6.
          CASE(6)
            DBHMIN(ISPC)=6.
          CASE DEFAULT
            DBHMIN(ISPC)=5.
          END SELECT
        ENDIF
      ENDIF
      IF(TOPD(ISPC).LE.0.)TOPD(ISPC)=4.        !SET **TOPD** DEFAULT
      IF(BFMIND(ISPC).LE.0.)THEN               !SET **BFMIND** DEFAULT
        IF(ISPC.LE.14.OR.ISPC.GT.68)THEN       !SOFTWOODS
          BFMIND(ISPC)=9.
        ELSE                                   !HARDWOODS
          SELECT CASE(IFOR)
          CASE(2)
            BFMIND(ISPC)=9.
            IF(ISPC.GE.40.AND.ISPC.LE.42)BFMIND(ISPC)=11.
          CASE(5)
            BFMIND(ISPC)=11.
            IF(ISPC.GE.40.AND.ISPC.LE.42)BFMIND(ISPC)=9.
          CASE DEFAULT
            BFMIND(ISPC)=11.
          END SELECT
        ENDIF
      ENDIF
      IF(BFTOPD(ISPC).LE.0.)THEN               !SET **BFTOPD** DEFAULT
        IF(ISPC.LE.14.OR.ISPC.GT.68)THEN       !SOFTWOODS
          BFTOPD(ISPC)=7.6
        ELSE                                   !HARDWOODS
          SELECT CASE(IFOR)
          CASE(2)
            BFTOPD(ISPC)=7.6
            IF(ISPC.GE.40.AND.ISPC.LE.42)BFTOPD(ISPC)=9.6
          CASE(5)
            BFTOPD(ISPC)=7.6
            IF(ISPC.GE.40.AND.ISPC.LE.42)BFTOPD(ISPC)=7.6
          CASE DEFAULT
            BFTOPD(ISPC)=9.6
          END SELECT
        ENDIF
      ENDIF
      ENDDO
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES IF USING CLARK
C  OR GEVORKIANTZ. METHB8 AND METHC8 INDEX THE NUMBER OF SPECIS
C  USING TWIGS VOLUME EQS. (F7 =8 IN VOLUME AND BFVOLUME KEYWORDS
C  IF ALL TWIGS EQUATIONS, THEN DON'T PRINT THE NVEL TABLE
C----------
      KFORST = KODFOR-900
      IREGN=9
      WRITE(FORST,'(I2)')KFORST
      IF(KFORST.LT.10)FORST(1:1)='0'
      DIST='  '
      PROD='  '
      VAR='LS'
C
      METHB8=0
      METHC8=0
      DO ISPC=1,MAXSP
      READ(FIAJSP(ISPC),'(I4)')IFIASP
      IF(((METHC(ISPC).EQ.6).OR.(METHC(ISPC).EQ.9).OR.
     &    (METHC(ISPC).EQ.5)).AND.(VEQNNC(ISPC).EQ.'          '))THEN
        IF(METHC(ISPC).EQ.5)THEN
          VOLEQ(1:7)='900DVEE'
        ELSE
          VOLEQ(1:7)='900CLKE'
        ENDIF
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC)=VOLEQ
C      WRITE(16,*)' PROD,IFIASP,ISPC,VEQNNC(ISPC)= ',PROD,IFIASP,ISPC,
C     &VEQNNC(ISPC)
      ELSEIF(METHC(ISPC).EQ.8)THEN
          METHC8=METHC8+1
      ENDIF
      IF(((METHB(ISPC).EQ.6).OR.(METHB(ISPC).EQ.9).OR.
     &    (METHB(ISPC).EQ.5)).AND.(VEQNNB(ISPC).EQ.'           '))THEN
        IF(METHB(ISPC).EQ.5)THEN
          VOLEQ(1:7)='900DVEE'
        ELSE
          VOLEQ(1:7)='900CLKE'
        ENDIF
        PROD='01'
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC)=VOLEQ
      ELSEIF(METHC(ISPC).EQ.8)THEN
          METHB8=METHB8+1
      ENDIF
      ENDDO

      DO 92 I=1,15
      J=(I-1)*10 + 1
      JJ=J+9
      IF(JJ.GT.MAXSP)JJ=MAXSP
      WRITE(JOSTND,90)(NSP(K,1)(1:2),K=J,JJ)
   90 FORMAT(/' SPECIES ',5X,10(A2,6X))
      WRITE(JOSTND,91)(SDIDEF(K)/ACRtoHA,K=J,JJ )
   91 FORMAT(' SDI MAX ',   10F8.0)
      IF(JJ .EQ. MAXSP)GO TO 93
   92 CONTINUE
   93 CONTINUE
C----------
C  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE
C---------
      IF(LFIA) THEN
        CALL FIAHEAD(JOSTND)
        WRITE(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I=1,MAXSP)
 211    FORMAT ((T13,8(A3,'=',A6,:,'; '),A,'=',A6))
      ENDIF
C----------
C  WRITE VOLUME EQUATION NUMBER TABLE
C----------
      IF((METHC8.NE.MAXSP).AND.(METHB8.NE.MAXSP))
     &CALL VOLEQHEAD(JOSTND)
      IF((METHC8.NE.MAXSP).AND.(METHB8.NE.MAXSP))
     &WRITE(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J=1,MAXSP)
 230  FORMAT(4(3X,A2,4X,A10,1X,A10))
      RETURN
      END
