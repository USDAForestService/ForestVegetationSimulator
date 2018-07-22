C----------
C VOLUME $Id$
C----------
!== last modified  4-12-2004

      SUBROUTINE R1TAP(VOLEQ,FORST,DBHOB,HTTOT,TOPD,HTM,MFLAG,CVOL,D2)
C**************************************************************
C
      CHARACTER*2 FORST
      CHARACTER*10 VOLEQ
      INTEGER NNF(20),INF(20),SP,NF,NATFOR,I,MFLAG
      REAL CFF, CVOL,DBHOB,HTTOT,H,VMER
      REAL I1,I2,J1,J2,HTM,W,TOPD,D2,TOTCUB

      REAL*8 B1,B2,DOVERH,QUAN1,QUAN2
      REAL*8 M1,M2,A,B,C,DISC,HOVRH

      REAL*8 LPB3(20),LPB4(20),LPA1(20),LPA2(20)
      REAL*8 LPF1(20),LPF2(20),LPF3(20),LPF4(20)

      REAL*8 FA0(6),FA1(6),FA2(6),FB0(6),FB1(6),FB2(6),FC0(6)
      REAL*8 FC1(6),FC2(6)

C-- THE DATA NAMED B3, B4, A1, A2, F1, F2, F3, AND F4 IN THE STATEMENTS
C-- BELOW ARE STEM PROFILE EQUATION COEFFICIENTS ESTIMATED BY JOHN
C-- BYRNE, MOSCOW FSL, C-- INT 1989.  THE FIRST TEN ELEMENTS OF EACH
C-- VECTOR ARE FOR INSIDE BARK ESTIMATES OF STEM DIAMETER OR VOLUME.
C-- THE SECOND TEN ELEMENTS ARE FOR OUTSIDE BARK ESTIMATES. POSITION
C-- IN THE FIRST OR SECOND HALVES OF THE VECTOR IS SELECTED BY THE VALUE
C-- OF VARIABLE IOSB (0 OR 1) IN THE CALLING SEQUENCE. POSITION WITHIN
C-- SELECTED HALF OF THE COEFFICIENT VECTOR IS DETERMINED BY NATIONAL
C-- FOREST CODE AND THE FOREST APPLICABILITY MAPPING SHOWN BELOW:
C
C--   NATIONAL FOREST                      COEFFICIENTS USED
C--
C--   02  BEAVERHEAD          =>             BEAVERHEAD
C--   03  BITTERROOT          =>             LOLO
C--   04  IDAHO PANHANDLE     =>             IDAHO PANHANDLE
C--   05  CLEARWATER          =>             IDAHO PANHANDLE
C--   06  COEUR D'ALENE       =>             IDAHO PANHANDLE
C--   07  COLVILLE            =>             IDAHO PANHANDLE
C--   08  CUSTER              =>             GALLATIN
C--   09  DEERLODGE           =>             BEAVERHEAD
C--   10  FLATHEAD            =>             FLATHEAD
C--   11  GALLATIN            =>             GALLATIN
C--   12  HELENA              =>             GALLATIN
C--   13  KANIKSU             =>             IDAHO PANHANDLE
C--   14  KOOTENAI            =>             KOOTENAI
C--   15  LEWIS & CLARK       =>             GALLATIN
C--   16  LOLO                =>             LOLO
C--   17  NEZPERCE            =>             NEZPERCE
C--   18  ST. JOE             =>             IDAHO PANHANDLE
C--   19  TARGHEE             =>             TARGHEE
C--   20  CHALLIS             =>             CHALLIS
C--   99  ALL FORESTS         =>             COMBINED STEM PROFILE
C**************************************************************
C**************************************************************

C--  NNF CONTAINS NATIONAL FOREST CODES TO BE USED WITH THE
C--  SELECTION OF COEFFICIENTS ACCORDING TO NATIONAL FOREST.
C
      DATA NNF/2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99/
C
C--  INF CONTAINS THE MAPPING THAT DIRECTS COMPUTATION FOR EACH
C--  NATIONAL FOREST TO USE COEFFICIENTS FROM THE MOST SIMILAR FOREST.
C
      DATA INF/1,6,2,2,2,2,4,1,3,4,4,2,5,4,6,7,2,8,9,10/


      DATA LPB3  /87.581032, 107.321047, 151.829574,  94.950201, 
     >           126.84759,  114.481775, 136.118702,  65.952319,
     >            35.653746, 122.918998,  127.865601, 135.555219, 
     >           178.802131, 114.225493, 140.257108, 116.065254, 
     >           175.175261,  81.427303,  43.326343, 156.186178/

      DATA LPB4 /-1.796996, -1.521593, -1.728852, -2.173816, -1.409475,
     >         -1.395119, -1.63385,  -2.143552, -1.752816, -1.596440,
     >         -2.090729, -1.767546, -1.8451,   -2.86977,  -1.730936,
     >         -1.567023, -1.734578, -2.655859, -1.720238, -1.956792/

      DATA LPA1 /0.070748, 0.060751, 0.057501, 0.069205, 0.061861, 
     >         0.061968, 0.062308, 0.067756, 0.096783, 0.060819,
     >         0.064035, 0.060062, 0.060811, 0.067656, 0.064042, 
     >         0.069093, 0.060420, 0.066160, 0.094332, 0.059633/

      DATA LPA2 /0.739554, 0.760857, 0.761893, 0.784304, 0.755763,
     >         0.760022, 0.766816, 0.748691, 0.770176, 0.753617,
     >         0.769436, 0.789044, 0.796044, 0.814915, 0.784127, 
     >         0.777538, 0.794990, 0.756088, 0.777866, 0.787699/

      DATA LPF1 /-2.585776, -2.983474, -2.556167, -3.499448, -2.291195,
     >         -2.348082, -2.522793, -3.663878, -1.648043, -2.751331, 
     >         -3.496645, -3.629263, -2.808183, -4.824117, -2.791224,
     >         -2.738865, -2.93618,  -4.515702, -1.691230, -3.496751/

      DATA LPF2 /-55.801456, -21.001845,  -70.582552, -40.100715,
     >         -53.651279, -55.398448,  -68.942489, -25.991574,
     >        -140.676896, -36.714040,  -38.175315, -15.137555,
     >         -83.436832, -37.490316,  -65.046511, -53.563612,
     >         -61.670934, -22.754795, -140.639469, -36.62309/

      DATA LPF3 /0.76375,  1.068502, 0.711984,  1.241977, 0.644969, 
     >         0.669119, 0.650101, 1.494773, -0.086302, 0.941067,
     >         1.273269, 1.39021,  0.789947,  1.900861, 0.853215,
     >         0.844254, 0.815822, 1.934224, -0.203745, 1.292467/

      DATA LPF4 /51.814019, 28.238012, 67.492926,  41.73976,  51.495002,
     >         52.05634,  69.332948, 27.351039, 122.920199, 36.462619,
     >         39.263351, 25.294065, 76.21113,   42.487414, 60.032024,
     >         51.833866, 66.266464, 26.736753, 128.263876, 38.212058/

C  COEFFICIENTS FOR THE OTHER SPECIES (CHOJNACKY AND CUTLER)

C     SPECIES  LPINE   WPINE   WLARCH  D-FIR   G-FIR   PPINE
      DATA FA0 /0.2057, 0.1592, 0.0964, 0.1655, 0.1255, 0.1720/
      DATA FA1 /1.862, 1.661, 1.756, 1.703, 1.662, 1.860/
      DATA FA2 /1.120, 1.256, 1.283, 1.217, 1.328, 1.089/
      DATA FB0 /0.688, 0.620, 1.133, 0.709, 0.592, 1.047/
      DATA FB1 /3.580, 3.358, 3.561, 3.475, 3.595, 3.450/
      DATA FB2 /3.405, 3.137, 3.418, 3.229, 3.329, 3.290/
      DATA FC0 /0.871, 1.098, 0.941, 1.096, 1.008, 1.050/
      DATA FC1 /2.226, 2.556, 2.316, 2.365, 2.452, 2.305/
      DATA FC2 /2.188, 2.527, 2.294, 2.377, 2.443, 2.309/


C-- ZERO ALL VOLUMES TO BE CALCULATED SO THAT IF CALCULATION IS
C-- UNWANTED AND SKIPPED MISLEADING VOLUMES WILL NOT BE RETURNED.
C
      IF(FORST(2:2) .LT. '0') THEN
         FORST(2:2) = FORST(1:1)
         FORST(1:1) = '0'
         IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF

      read(FORST,'(i2)') NATFOR

C--   DETERMINE IF NATFOR IS A LEGAL FOREST.  ELSE SET NF TO 99

      IF(NATFOR.LE.1 .OR. NATFOR.GT.20) NATFOR = 99

      DO 101, I=1,20
         IF(NATFOR.EQ.NNF(I)) NF = INF(I)
 101  CONTINUE

      IF(VOLEQ(8:10) .EQ. '108')THEN
         SP = 1
      ELSEIF(VOLEQ(8:10) .EQ. '119')THEN
         SP = 2
      ELSEIF(VOLEQ(8:10).EQ.'070' .OR. VOLEQ(8:10).EQ.'073')THEN
         SP = 3
      ELSEIF(VOLEQ(8:10) .EQ. '202')THEN
         SP = 4
      ELSEIF(VOLEQ(8:10) .EQ. '017')THEN
         SP = 5
      ELSEIF(VOLEQ(8:10) .EQ. '122')THEN
         SP = 6
      ENDIF

      TOTCUB = 0.0

C-- SELECT EITHER INSIDE BARK (IOSB=0) OR OUTSIDE BARK (IOSB=1)
C-- COEFFICIENTS.  INSIDE BARK HARDCODED (6/95)

C      IOSB = 0

C      IF(IOSB.GT.1) IOSB=1
C      NF = NF + 10*IOSB

C LODGEPOLE PINE

      IF(VOLEQ(8:10).EQ.'108') THEN

C-- CALCULATE FORM-RELATED COEFFICIENTS.

          DOVERH = (DBHOB/12.0)/HTTOT
          B1 = LPF1(NF) + LPF2(NF) * DOVERH
          B2 = LPF3(NF) + LPF4(NF) * DOVERH

C-- CALCULATE CYLINDER FORM FACTOR FROM STEM PROFILE INTEGRAL.

          CFF = REAL(B2/3.0 + B1/2.0 - B1 - B2 + (LPB3(NF)/3.0) * 
     >          LPA1(NF)**3 + (LPB4(NF)/3.0) * LPA2(NF)**3)

          CVOL = CFF
      
C-- CALCULATE TOTAL VOLUME.

C-- IF NO MERCHANTABLE HT OR DIAMETERS NEEDED, RETURN TO CALLING PROG.

          IF(MFLAG.EQ.1) THEN

C-- IF MERCHANTABLE HEIGHT IS FURNISHED DON'T CALCULATE IT. 

             IF(HTM.GT.0.0) THEN
                W = HTM
             ELSE

C-- CALCULATE HEIGHT TO MERCHANTABLE TOP DIAMETER.
C-- MERCHANTABLE TOP DIAMETER IS "TOPD" IN THE CALLING SEQUENCE.
C-- MK (K=1,2) IS ESTIMATED DIAMETER AT HEIGHT AK*HT.

C-- EXTREME VALUES OF DOVERH DUE TO TREES VERY SHORT FOR
C-- THEIR DBHOB CAN YIELD VALUES OF B1 AND B2 WHICH CAUSE QUAN1 OR QUAN2
C-- BELOW TO GO NEGATIVE AND PREVENT SQUARE ROOT EXTRACTION. IF THIS
C-- OCCURS QUAN1 OR QUAN2 ARE MADE SMALL POSITIVE. THIS WILL SELDOM
C-- HAPPEN, AND ONLY FOR VERY UNUSUAL TREES.

                QUAN1 = B1*(LPA1(NF) - 1.0) + B2*(LPA1(NF)**2 - 1.0) +
     >                  LPB4(NF) * (LPA2(NF) - LPA1(NF))**2

                IF(QUAN1.LE.0.0) QUAN1 = 10.0**(-10)

                M1 = DBHOB*SQRT(QUAN1)
                QUAN2 = B1*(LPA2(NF) - 1.0) + B2*(LPA2(NF)**2 - 1.0)

                IF(QUAN2.LE.0.0) QUAN2 = 10.0**(-10)

                M2 = DBHOB*SQRT(QUAN2)
                J1 = 0.0
                IF(TOPD.GE.M1) J1 = 1.0

                J2 = 0.0
                IF(TOPD.GE.M2) J2 = 1.0

                A = B2 + LPB3(NF) * J1 + LPB4(NF) * J2
                B = B1 - 2.0*LPA1(NF)*LPB3(NF)*J1 - 2.0 * LPA2(NF) * 
     >              LPB4(NF)*J2
                C = -(B1 + B2) + LPB3(NF)*LPA1(NF)**2*J1 + LPB4(NF) * 
     >              LPA2(NF)**2 * J2 - (TOPD/DBHOB)**2
                DISC = B*B - 4.0*A*C


C-- W IS FRACTION OF TOTAL HEIGHT AT WHICH MERCH. TOP DIA. IS LOCATED.

                W = REAL((HTTOT/(2.0*A)) * (-B - (DISC**0.5)))
                HTM = W

C            ENDIF FOR NO MERCHANTABLE HEIGHT
             ENDIF

C  CHECK TO SEE IF DIAMETERS NEED CALCULATING

          ELSEIF (MFLAG .EQ. 2) THEN

              H = HTM

              HOVRH = H/HTTOT
              I1 = 0.0
              IF(HOVRH.LE.LPA1(NF)) I1 = 1.0
              I2 = 0.0
              IF(HOVRH.LE.LPA2(NF))  I2 = 1.0

              D2 = REAL(DBHOB*(B1*(HOVRH-1)+B2*(HOVRH**2-1) + LPB3(NF)*
     >             I1*(LPA1(NF) - HOVRH)**2 + LPB4(NF) * I2*(LPA2(NF) - 
     >             HOVRH)**2)**0.5)

C          ENDIF FOR MFLAG CHECKS

          ENDIF

C  ALL OTHER SPECIES
      ELSE

         TOTCUB = 0.0
         VMER = 0.0
 
         TOTCUB =REAL(0.01 * FA0(SP) * DBHOB**FA1(SP) * HTTOT**FA2(SP))
C          WRITE(*,*)' TOTCUB = ',TOTCUB
         IF (MFLAG .EQ. 1) THEN
C          WRITE(*,*)' TOPD = ',TOPD
            IF(TOPD.GT.0.0) THEN
               IF(TOPD.LT.DBHOB) THEN
                  VMER = REAL( 
     >            (1.0-(FB0(SP)*TOPD**FB1(SP)/DBHOB**FB2(SP)))*TOTCUB)
               ELSE
                  VMER = 0.0
               ENDIF
            ELSE
               IF(HTM.GT.HTTOT)  HTM = HTTOT
               IF(HTM.GT.1.0) THEN
                  VMER = REAL((1.0 - (FC0(SP)*(HTTOT-HTM)**FC1(SP)/
     >                    HTTOT**FC2(SP)))*TOTCUB)
               ELSE
                  VMER = 0.0
               ENDIF
            ENDIF
            CVOL = VMER
         ELSE
            CVOL = TOTCUB
         ENDIF
      ENDIF
      RETURN
      END
