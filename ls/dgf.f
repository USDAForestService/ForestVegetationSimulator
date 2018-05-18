      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C
C LS $Id: dgf.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  DIAMETER GROWTH IS PREDICTED FROM DBH, SITE INDEX,
C  CROWN RATIO, AND BASAL AREA PERCENTILES AND QMD FOR TREES
C  GREATER THAN OR EQUAL TO 5 INCHED DBH. THE
C  SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE ARGUMENT TO
C  DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO PROCESS DIFFERENT
C  CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED BY **DGDRIV** DURING
C  CALIBRATION AND WHILE CYCLING FOR GROWTH PREDICTION.  ENTRY
C  **DGCONS** IS CALLED BY **RCON** TO LOAD SITE DEPENDENT COEFFICIENTS
C  THAT NEED ONLY BE RESOLVED ONCE.
C
C  DIAMETER GROWTH EQUATIONS WERE DEVELOPED THROUGH COST SHARE AGREEMENT NO.
C  10-CS-11132425-258 WITH MICHIGAN TECHNOLOGICAL UNIVERSITY, OCTOBER 2012.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C-----------
C  DEFINITIONS OF LOCAL VARIABLES.
C
C    BAGE5 -- STAND BASAL AREA IN TREES 5.0" DBH AND LARGER.
C      BAL -- STAND BASAL AREA IN TREES WITH DIAMETER >= THE DIAMETER
C             OF THE SUBJECT TREE.
C     BALC -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
C             LARGER TREES TERM IN THE DDS MODEL.
C     BARK -- BARK RATIO
C   BRATIO -- BARK RATIO FUNCTION CONTAINED IN BRATIO.F
C   CONSPP -- CONSTANT TERM IN THE DIAMETER GROWTH EQUATION.
C       CR -- PERCENT CROWN RATIO FOR A TREE
C    CRSQC -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
C             RATIO SQUARED TERM IN THE DDS MODEL.
C    CRWNC -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
C             RATIO TERM IN THE DDS MODEL.
C        D -- DIAMETER AT BREAST HEIGHT FOR A TREE
C     DBHC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER TERM IN
C             THE DDS MODEL
C    DBH2C -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER SQUARED
C             TERM IN THE DDS MODEL
C      DDS -- PREDICTED INSIDE-BARK BASAL AREA INCREMENT FOR A TREE
C    DEBUG -- LOGICAL VARIABLE USED TO CONTROL DEBUG OUTPUT
C   DIAGRI -- PREDICTED INSIDE-BARK DIAMETER GROWTH FOR A TREE
C   DIAGRO -- PREDICTED OUTSIDE-BARK DIAMETER GROWTH FOR A TREE
C     DIAM -- ARRAY LOADED WITH TREE DIAMETERS
C D1 - D11 -- EQUATION COEFFICIENTS FOR A DEBUG WRITE STATEMENT
C     I,IK -- ARRAY INDICIES
C   INTERC -- DDS REGRESSION EQUATION INTERCEPT
C     ISPC -- FVS SPECIES SEQUENCE NUMBER
C I1,I2,I3 -- ARRAY INDICIES
C   OBSERV -- SAMPLE SIZE USED TO FIT THE EQUATION
C        P -- TREES-PER-ACRE FOR A TREE RECORD
C   QMDGE5 -- QUADRATIC MEAN DIAMETER FOR TREES 5.0" DBH AND LARGER
C    RDBHC -- ARRAY CONTAINING COEFFICIENTS FOR DIAM/QMDGE5
C             TERM IN THE DDS MODEL.
C  RDBHSQC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAM*DIAM/QMDGE5
C             TERM IN THE DDS MODEL.
C   RELDBH -- RELATIVE TREE DIAMETER (D/QMDGE5)
C RELDBHSQ -- RELATIVE TREE DIAMTER SQUARED ((D**2)/QMDGE5)
C     SBAC -- ARRAY CONTAINING THE COEFFICIENTS FOR STAND BASAL AREA
C             IN TREES 5.0" DBH AND LARGER TERM IN THE DDS MODEL.
C   SDQGE5 -- SUM OF TREE DIAMETERS SQUARED FOR THE STAND
C    SITEC -- ARRAY CONTAINING COEFFICIENTS FOR SITE INDEX IN THE DDS MODEL.
C        T -- TOTAL STAND TREES-PER-ACRE
C    VDBHC -- ARRAY CONTAINING COEFFICIENTS FOR THE INVERSE OF DIAMETER
C             TERM IN THE DDS MODEL.
C----------
C  SPECIES AND COEFFICIENT ORDER
C----------
C  NOTE -- commercial hardwood (44=CH) uses black walnut (46=WN) 
C          as surrogate for all coefficients.
C
C      1=JP   2=SC    3=RN    4=RP    5=WP
C      6=WS   7=NS    8=BF    9=BS   10=TA
C     11=WC  12=EH   13=OS   14=RC   15=BA
C     16=GA  17=EC   18=SV   19=RM   20=BC
C     21=AE  22=RL   23=RE   24=YB   25=BW
C     26=SM  27=BM   28=AB   29=WA   30=WO
C     31=SW  32=BR   33=CK   34=RO   35=BO
C     36=NP  37=BH   38=PH   39=SH   40=BT
C     41=QA  42=BP   43=PB   44=CH   45=BN
C     46=WN  47=HH   48=BK   49=NC   50=BE
C     51=ST  52=MM   53=AH   54=AC   55=HK
C     56=DW  57=HT   58=AP   59=BG   60=SY
C     61=PR  62=CC   63=PL   64=WI   65=BL
C     66=DM  67=SS   68=MA
C-----------
C
      LOGICAL DEBUG
C
      INTEGER I,IK,ISPC,I1,I2,I3
C
      REAL BAGE5,BAL,BARK,BRATIO,CONSPP,CR,D,DDS,DIAGRO,DIAGRI
      REAL D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11
      REAL P,QMDGE5,RELDBH,RELDBHSQ,SDQGE5,T
C
      REAL BALC(MAXSP),CRSQC(MAXSP),CRWNC(MAXSP),DBHC(MAXSP),
     &     DBH2C(MAXSP),DIAM(MAXTRE),INTERC(MAXSP),
     &     OBSERV(MAXSP),RDBHC(MAXSP),
     &     RDBHSQC(MAXSP),SBAC(MAXSP),SITEC(MAXSP),
     &     VDBHC(MAXSP)
C----------
C  DATA STATEMENTS:
C
C  COEFFICIENTS FOR DDS MODEL
C----------
      DATA INTERC/
     &    1.395,     1.395,  0.21748,   0.21748,   4.1821,
     &   3.9303,    3.9303,    3.458,    3.4206,   -0.461,
     & -0.74827,    3.2309,  0.21748,    3.0982,   2.5313,
     &   2.5313,    1.8151,   2.2302,   -0.3258,  -0.3258,
     &   3.2435,    3.2435,   3.2435,      2.54,   3.6912,
     &   3.2946,    3.2946,   2.7284,   2.47033,   1.9019,
     &   1.9019,    1.9019,   1.9019,    0.4452,  0.49086,
     &  0.49086,  0.058969, 0.058969,  0.058969,   3.4483,
     &   3.3904,    3.3904,   2.6884,    2.9591,   2.9591,
     &   2.9591,    5.3879,    3.022,    3.1876,   3.1876,
     &   3.1876,    3.1876,   3.0308,    3.0308,   1.2352,
     &   3.0308,    3.0308,   3.0308,  -0.28233,   3.8669,
     &   3.0308,    3.0308,   3.0308,    3.8669,   3.8669,
     &   3.8669,    3.3841,   3.0308/
C
      DATA VDBHC/
     & -3.9398,  -3.9398,      0.0,      0.0,  -13.911,
     & -9.3995,  -9.3995,  -10.103,  -9.4927,      0.0,
     &     0.0,  -11.816,      0.0,  -8.5844,  -10.146,
     & -10.146,      0.0,  -6.9962,      0.0,      0.0,
     & -10.737,  -10.737,  -10.737,   -7.304,  -11.035,
     & -12.691,  -12.691,  -12.059,  -9.9428,  -5.6950,
     & -5.6950,  -5.6950,  -5.6950,      0.0,      0.0,
     &     0.0,      0.0,      0.0,      0.0,  -8.1878,
     & -7.1815,  -7.1815,  -7.3181,  -9.5199,  -9.5199,
     & -9.5199,  -13.259,  -13.627,  -10.287,  -10.287,
     & -10.287,  -10.287,  -8.1059,  -8.1059,  -5.2144,
     & -8.1059,  -8.1059,  -8.1059,      0.0,  -8.8704,
     & -8.1059,  -8.1059,  -8.1059,  -8.8704,  -8.8704,
     & -8.8704,  -10.282,  -8.1059/
C
      DATA DBHC/
     &      0.0,      0.0,  0.30253,  0.30253,      0.0,
     &      0.0,      0.0,      0.0,      0.0,  0.42230,
     &  0.33025,      0.0,  0.30253,      0.0,      0.0,
     &      0.0, 0.091526,  0.11324,  0.33549,  0.33549,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0, 0.096247,
     & 0.096247, 0.096247, 0.096247,  0.22472,  0.23430,
     &  0.23430,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,  0.18300,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0/
C
      DATA DBH2C/
     &  0.01213,  0.01213, -0.00885, -0.00885,       0.0,
     &      0.0,      0.0,      0.0,      0.0,       0.0,
     &      0.0,      0.0, -0.00885,      0.0,       0.0,
     &      0.0,      0.0, -0.00186, -0.00703,  -0.00703,
     &      0.0,      0.0,      0.0,      0.0,       0.0,
     &      0.0,      0.0,      0.0,      0.0,       0.0,
     &      0.0,      0.0,      0.0, -0.00397,  -0.00371,
     & -0.00371,  0.01986,  0.01986,  0.01986,       0.0,
     &      0.0,      0.0,      0.0,      0.0,       0.0,
     &      0.0,      0.0,      0.0,      0.0,       0.0,
     &      0.0,      0.0,      0.0,      0.0,  -0.00314,
     &      0.0,      0.0,      0.0,  0.01581,       0.0,
     &      0.0,      0.0,      0.0,      0.0,       0.0,
     &      0.0,      0.0,      0.0/
C
      DATA RDBHC/
     &  1.37270,  1.37270,      0.0,      0.0, -0.33396,
     & -0.58596, -0.58596, -0.63924, -1.43830,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,  1.02090,      0.0, -0.53201, -0.53201,
     &      0.0,      0.0,      0.0,      0.0, -0.34599,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,  4.09180,  4.09180,  4.09180,      0.0,
     & -0.16910, -0.16910,      0.0,      0.0,      0.0,
     &      0.0, -2.44892,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,  3.37463,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0/
C
      DATA RDBHSQC/
     & -0.17735, -0.17735,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0, -0.11080,
     & -0.08958,      0.0,      0.0,      0.0,      0.0,
     &      0.0, -0.03235,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0, -0.01611,
     & -0.01611, -0.01611, -0.01611,      0.0,      0.0,
     &      0.0, -0.33415, -0.33415, -0.33415,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0, -0.26631,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0/
C
      DATA SBAC/
     &      0.0,      0.0, -0.00114,  -0.00114,      0.0,
     &      0.0,      0.0, -0.00192, 0.0049528,      0.0,
     & -0.00118, -0.00166, -0.00114,  -0.00386, -0.00095,
     & -0.00095,      0.0,      0.0,  -0.00203, -0.00203,
     &      0.0,      0.0,      0.0,       0.0,      0.0,
     & -0.00210, -0.00210,      0.0,  -0.00228,      0.0,
     &      0.0,      0.0,      0.0,       0.0, -0.00186,
     & -0.00186,      0.0,      0.0,       0.0,      0.0,
     &      0.0,      0.0,      0.0,       0.0,      0.0,
     &      0.0,      0.0, -0.00288,       0.0,      0.0,
     &      0.0,      0.0, -0.00179,  -0.00179, -0.00439,
     & -0.00179, -0.00179, -0.00179,       0.0,      0.0,
     & -0.00179, -0.00179, -0.00179,       0.0,      0.0,
     &      0.0,      0.0, -0.00179/
C
      DATA BALC/
     & -0.00428, -0.00428, -0.00306,   -0.00306, -0.00412,
     & -0.00375, -0.00375,      0.0, -0.0067966,      0.0,
     &      0.0,      0.0, -0.00306,        0.0,      0.0,
     &      0.0, -0.00428, -0.00171,        0.0,      0.0,
     &      0.0,      0.0,      0.0,   -0.00223, -0.00259,
     &      0.0,      0.0,      0.0,        0.0, -0.00367,
     & -0.00367, -0.00367, -0.00367,   -0.00148,      0.0,
     &      0.0,  0.00184,  0.00184,    0.00184, -0.00134,
     & -0.00311, -0.00311,      0.0,   -0.00169, -0.00169,
     & -0.00169, -0.00353,      0.0,        0.0,      0.0,
     &      0.0,      0.0,      0.0,        0.0,  0.00331,
     &      0.0,      0.0,      0.0,        0.0, -0.00535,
     &      0.0,      0.0,      0.0,   -0.00535, -0.00535,
     & -0.00535, -0.00170,      0.0/
C
      DATA CRWNC/
     & 0.02997, 0.02997, 0.01947,  0.01947, 0.03382,
     & 0.01635, 0.01635, 0.02677, 0.018393,     0.0,
     & 0.02758, 0.01535, 0.01947,  0.01628, 0.01677,
     & 0.01677,     0.0, 0.03114,  0.03314, 0.03314,
     & 0.05258, 0.05258, 0.05258,  0.01369, 0.01932,
     & 0.01787, 0.01787, 0.01843,  0.04226, 0.03021,
     & 0.03021, 0.03021, 0.03021,      0.0, 0.00770,
     & 0.00770,     0.0,     0.0,      0.0, 0.02367,
     & 0.01543, 0.01543,     0.0,  0.03012, 0.03012,
     & 0.03012,     0.0, 0.04569,  0.04919, 0.04919,
     & 0.04919, 0.04919, 0.00489,  0.00489, 0.02814,
     & 0.00489, 0.00489, 0.00489,  0.01650, 0.02034,
     & 0.00489, 0.00489, 0.00489,  0.02034, 0.02034,
     & 0.02034, 0.03201, 0.00489/
C
      DATA CRSQC/
     & -0.00015, -0.00015,      0.0,      0.0, -0.00012,
     &      0.0,      0.0, -0.00009,      0.0,  0.00012,
     & -0.00015,      0.0,      0.0,      0.0,      0.0,
     &      0.0,  0.00031, -0.00014, -0.00016, -0.00016,
     & -0.00038, -0.00038, -0.00038,      0.0,      0.0,
     &      0.0,      0.0,      0.0, -0.00029, -0.00022,
     & -0.00022, -0.00022, -0.00022,  0.00009,      0.0,
     &      0.0,  0.00008,  0.00008,  0.00008,      0.0,
     &      0.0,      0.0,  0.00020,      0.0,      0.0,
     &      0.0,      0.0, -0.00037, -0.00045, -0.00045,
     & -0.00045, -0.00045,      0.0,      0.0, -0.00011,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0,      0.0,      0.0,      0.0,      0.0,
     &      0.0, -0.00026,      0.0/
C
      DATA SITEC/
     & 0.00385,  0.00385,  0.00581,   0.00581,       0.0,
     &     0.0,      0.0,  0.00317, 0.0074198,       0.0,
     & 0.00537,  0.00676,  0.00581,       0.0,   0.01285,
     & 0.01285,      0.0,      0.0,   0.00520,   0.00520,
     &     0.0,      0.0,      0.0,   0.00647,       0.0,
     & 0.00496,  0.00496,  0.00856,   0.01152,       0.0,
     &     0.0,      0.0,      0.0,   0.01305,   0.00724,
     & 0.00724,      0.0,      0.0,       0.0,       0.0,
     & 0.00619,  0.00619,  0.00350,   0.00356,   0.00356,
     & 0.00356,      0.0,  0.00766,       0.0,       0.0,
     &     0.0,      0.0,      0.0,       0.0,   0.00447,
     &     0.0,      0.0,      0.0,       0.0,       0.0,
     &     0.0,      0.0,      0.0,       0.0,       0.0,
     &     0.0,      0.0,      0.0/
C
      DATA OBSERV/
     & 1337.0, 1337.0, 3409.0, 3409.0, 1343.0,
     &  986.0,  986.0, 2498.0, 2365.0, 1524.0,
     & 5618.0,  756.0, 3409.0, 4111.0, 3285.0,
     & 3285.0,  416.0, 3300.0, 5133.0, 5133.0,
     &  604.0,  604.0,  604.0,  563.0, 1934.0,
     & 4470.0, 4470.0,  150.0,  303.0, 1848.0,
     & 1848.0, 1848.0, 1848.0, 1605.0,  596.0,
     &  596.0,  333.0,  333.0,  333.0,  899.0,
     & 5660.0, 5660.0, 2649.0, 2298.0, 2298.0,
     & 2298.0,  209.0,  982.0,  309.0,  309.0,
     &  309.0,  309.0,  589.0,  589.0, 5969.0,
     &  589.0,  589.0,  589.0,  677.0, 1422.0,
     &  589.0,  589.0,  589.0, 1422.0, 1422.0,
     & 1422.0, 1028.0,  589.0/
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,5)ICYC
    5 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
C-----------
C  INITIALIZE VARIABLES FOR DBH >= 5 IN
C-----------
      SDQGE5=0.
      T=0.
      BAGE5=0.
      QMDGE5=0.
C----------
C  COMPUTE BASAL AREA IN TREES >= 5.0" DBH, 
C  AND QMD FOR TREES >= 5.0" DBH
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1 .LE. 0)GO TO 20
      I2=ISCT(ISPC,2)
      DO 10 I3=I1,I2
      I=IND1(I3)
      P=PROB(I)
      D=DIAM(I)
      IF(D.LT.5.)GO TO 10
      SDQGE5=SDQGE5+P*(D)**2.
      T=T+P
      BAGE5=BAGE5+D*D*P*0.005454154
  10  CONTINUE
  20  CONTINUE
      IF(SDQGE5.GT.0.)QMDGE5=SQRT(SDQGE5/T)
      IF(BAGE5 .LE. 0.) BAGE5 = 10.
C----------
C  BEGIN SPECIES LOOP.
C----------
      DO 200 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 200
      I2=ISCT(ISPC,2)
      CONSPP= DGCON(ISPC) + COR(ISPC)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 100 I3=I1,I2
      I=IND1(I3)
      D=DIAM(I)
      CR=ICR(I)
      IF(CR .LE. 0.) CR= 10.
      BAL = (1.0 - (PCT(I)/100.)) * BA
C
      IF (D.LE.0.0) GO TO 100
C----------
C  BOUND QMD FOR SOME SPECIES
C----------
      SELECT CASE (ISPC)
        CASE(1,2,10,37:39,59)                        ! SC,JP,TA,BH,PH,SH,BG
          IF(QMDGE5 .GT. 13.) QMDGE5=13.
        CASE(11)                                     ! WC
          IF(QMDGE5 .GT. 15.) QMDGE5=15.
        CASE(17,30:33)                               ! EC,WO,SW,BR,CK
          IF(QMDGE5 .GT. 25.) QMDGE5=25.
        CASE DEFAULT
      END SELECT
C----------
C  BOUND CROWN RATIO FOR SOME SPECIES
C----------
      SELECT CASE (ISPC)
        CASE(17)                                     !EC
          IF(CR .GT. 60.) CR=60.
        CASE(60,65)                                  !SY,BL
          IF(CR .GT. 85.) CR=85.
        CASE DEFAULT
      END SELECT
C----------
C  RELATIVE DBH
C----------
      RELDBH = 0.0
      RELDBHSQ = 0.0
      IF(QMDGE5.GT.0.0) RELDBH=D/QMDGE5
      IF(QMDGE5.GT.0.0) RELDBHSQ=D*D/QMDGE5
C
      IF(DEBUG) WRITE(JOSTND,*) 'IN DGF-I= ',I,' D=',D,
     &' RELDBH= ',RELDBH,' SI= ',SITEAR(ISPC),
     &'BAGE5=',BAGE5,'CR=',CR,' QMDGE5= ',QMDGE5,
     &' BAL= ',BAL,' CONSPP= ',CONSPP
C----------
C  CALCULATION OF DDS FOR LAKE STATES VARIANT
C----------
      DDS= CONSPP
     &   + INTERC(ISPC)
     &   + VDBHC(ISPC) * 1./D
     &   + DBHC(ISPC) * D
     &   + DBH2C(ISPC) * D * D
     &   + RDBHC(ISPC) * RELDBH
     &   + RDBHSQC(ISPC) * RELDBHSQ
     &   + CRWNC(ISPC) * CR
     &   + CRSQC(ISPC) * CR*CR
     &   + SBAC(ISPC) * BAGE5
     &   + BALC(ISPC) * BAL
     &   + SITEC(ISPC) * SITEAR(ISPC)
C----------
C  IF DEBUGGING, OUTPUT TERMS IN THE EQUATION
C----------
      IF(DEBUG) THEN
        D1= INTERC(ISPC)
        D2= VDBHC(ISPC) * 1./D
        D3= DBHC(ISPC) * D
        D4= DBH2C(ISPC) * D * D
        D5= RDBHC(ISPC) * RELDBH
        D6= RDBHSQC(ISPC) * RELDBHSQ
        D7= CRWNC(ISPC) * CR
        D8= CRSQC(ISPC) * CR*CR
        D9= SBAC(ISPC) * BAGE5
        D10=BALC(ISPC) * BAL
        D11=SITEC(ISPC) * SITEAR(ISPC)
C
        WRITE(JOSTND,9090)D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,DDS
 9090   FORMAT(' IN DGF ,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,DDSO=',
     &  /,15F9.3)
      ENDIF
C----------
C  CALCULATION OF DDS FOR LAKE STATES VARIANT WAS BUILT
C  DDS OUTSIDE BARK, CONVERT TO INSIDE BARK
C----------
      IF(DDS.LT.-9.21) DDS=-9.21
      DIAGRO= SQRT(DBH(I)*DBH(I)+EXP(DDS))-DBH(I)
      BARK=BRATIO(ISPC,DBH(I),HT(I))
      DIAGRI= DIAGRO*BARK
      DDS=ALOG(((DBH(I)*BARK+DIAGRI)**2.0)-(DBH(I)*BARK)**2.0)
      IF(DEBUG) WRITE(JOSTND,*) 'IN DGF-I= ',I,' D=',D,
     &' DIAGRO= ',DIAGRO,' DIAGRI= ',DIAGRI,' BARK= ',BARK,
     &' DDS= ',DDS
C
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG)THEN
        WRITE(JOSTND,9095) I,ISPC,DDS
 9095   FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DDS=',F7.4)
        WRITE(JOSTND,*)' IN DGF, DG= ',(DG(IK), IK= 1,50)
      ENDIF
C
  100 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
  200 CONTINUE
      IF(DEBUG) WRITE(JOSTND,9100)ICYC
 9100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
C
      RETURN
C
C
      ENTRY DGCONS
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C  OBSERV CONTAINS THE NUMBER OF
C  OBSERVATIONS BY SPECIES FOR THE UNDERLYING MODEL 
C  (THIS DATA IS USED BY **DGDRIV** FOR CALIBRATION).
C
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO ISPC=1,MAXSP
        DGCON(ISPC)=0.
        ATTEN(ISPC)=OBSERV(ISPC)
        SMCON(ISPC)=0.
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
        IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC)
     &  + ALOG(COR2(ISPC))
      ENDDO
C
      RETURN
      END
