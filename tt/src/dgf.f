      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C  **DGF--TT    DATE OF LAST REVISION:   05/16/11
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY WK2.
C  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
C  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
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
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
COMMONS
C----------
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
C    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
C             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).
C----------
      LOGICAL DEBUG
      INTEGER IBSERV(5,MAXSP),ISMAP(MAXSP),ICLS,MAPHAB
      INTEGER IDGSIM(5,MAXSP),MAPLOC(4,MAXSP),MAPDSQ(4,MAXSP)
      INTEGER ISI,ISC,ISPC,I1,I2,KSI,I3,I,LSI,ISIC,ISIC2,ICHBCL(363)
      REAL DGFOR(5,MAXSP),DGSIC(5,MAXSP),DGCRSQ(MAXSP),DGHAB(7)
      REAL DIAM(MAXTRE),DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP)
      REAL DGDS(4,MAXSP),IGCCFM(5,MAXSP),DGCCFC(5,MAXSP)
      REAL DGSASP(MAXSP),DGCASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP)
      REAL BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,SI,D,CR
      REAL ASPDG,BAL,DDS,XPPDDS,XSITE,YOUNG,OLD,SUMD2,SUMTRE
      REAL DGEL(MAXSP),DGEL2(MAXSP),DGDBAL(MAXSP),DGBA(MAXSP),DGDBLS,
     &     DGPCCF(MAXSP),OBSERV(MAXSP),DGQMD,STDSDI,RELSDI,ALD,DGLCCF,
     &     DGPTCC,BARK,BRATIO,DPP,BATEM,DF,DIAGR,BAUTBA,ASPTEM,TEMEL,
     &     TEMBA,PBAL
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C----------
      DATA DGLD/
     &  0.213947,  0.213947,  0.533965,       0.0,  0.378802,
     & -0.368391,  0.563751,  0.378802,  0.648535,  0.822203,
     &       0.0,       0.0,  1.024186,       0.0,       0.0,
     &  0.889596,  0.213947,       0.0/
C
      DATA DGCR/
     &  1.523464,  1.523464,  1.931900,       0.0,  1.098353,
     &  4.034753,  2.164346,  1.098353,  0.137638,  1.768935,
     &       0.0,       0.0,  0.459387,       0.0,       0.0,  
     &  1.732535,  1.523464,       0.0/
C
      DATA DGCRSQ/
     &       0.0,       0.0, -0.894368,       0.0,       0.0,
     & -5.617552, -0.625799,       0.0, 1.0665429, -0.176164,
     &       0.0,       0.0,       0.0,       0.0,       0.0, 
     &       0.0,       0.0,       0.0/
C
      DATA DGBAL/
     & -0.358634, -0.358634, -0.574858,       0.0, -0.490005,
     & -0.704392, -0.469671, -0.490005, -0.312129,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0, 
     &       0.0, -0.358634,       0.0/
C
      DATA IGCCFM/
     &5*1,
     &5*1,
     &1,2,2,2,2,
     &5*1,
     &1,1,2,3,3,
     &5*1,
     &5*1,
     &1,1,2,3,3,
     &1,1,1,2,2,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1/
C----------
C  DGCCFC CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
C  HABITAT CLASS.
C----------
      DATA DGCCFC/
     &-0.199592,    4*0.0,
     &-0.199592,    4*0.0,
     &-0.641932,-0.141370,    3*0.0,
     &     5*0.,
     &-0.045495,-0.204852,-0.311383,    2*0.0,
     & 0.472247,    4*0.0,
     &-0.206752,    4*0.0,
     &-0.045495,-0.204852,-0.311383,    2*0.0,
     &-0.186614,-0.023236,    3*0.0,
     &     5*0.,
     &     5*0.,
     &     5*0.,
     &     5*0.,
     &     5*0.,
     &     5*0.,
     &     5*0.,
     &-0.199592,    4*0.0,
     &     5*0./
C----------
C  ASPECT IS STAND ASPECT.  IBSERV CONTAINS THE NUMBER OF
C  IBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
C  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).
C----------
C  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
C  MODEL BY SPECIES AND SITE CLASS.
C----------
      DATA  IBSERV/
     &   27,  70, 123, 101,  33,
     &   27,  70, 123, 101,  33,
     &    2, 125, 387,1008,1075,
     & 5*1000,
     &    3, 111, 242, 281, 334,
     &  184, 429, 356, 162,  74,
     &    0, 112,1023,1208, 295,
     &    3, 111, 242, 281, 334,
     &    1, 140, 798, 819, 530,
     &  5*0,
     & 5*1000,
     & 5*1000,
     &  5*0,
     &  184, 429, 356, 162,  74,
     & 5*1000,
     &  5*0,
     &   27,  70, 123, 101,  33,
     & 5*1000/
C
      DATA OBSERV/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,    8420.0,
     &       0.0,       0.0,      78.0,       0.0,       0.0,     
     &     220.0,       0.0,       0.0/
C----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
C----------
      DATA MAPLOC/
     &1,1,2,3,
     &1,1,2,3,
     &1,1,2,3,
     &4*1,
     &1,1,2,3,
     &1,1,2,1,
     &1,1,2,2,
     &1,1,2,3,
     &1,1,2,3,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &1,1,2,3,
     &4*1/
C
      DATA DGFOR/
     & 1.911884, 1.568742, 2.001195,      0.0,     0.0,
     & 1.911884, 1.568742, 2.001195,      0.0,     0.0,
     & 1.084994, 0.796640, 1.042871,      0.0,     0.0,
     &    5*0.0,
     & 1.543251, 0.943003, 0.792165,      0.0,     0.0,
     & 1.543622, 1.643733,      0.0,      0.0,     0.0,
     & 0.494205, 0.502908,      0.0,      0.0,     0.0,
     & 1.543251, 0.943003, 0.792165,      0.0,     0.0,
     & 0.921658, 0.807282, 0.914279,      0.0,     0.0,
     & 1.879603,     4*0.,
     &    5*0.0,
     &    5*0.0,
     &-7.753469,      0.0,      0.0,      0.0,     0.0,
     &    5*0.0,
     &    5*0.0,
     &-0.107648,      0.0,      0.0,      0.0,     0.0,
     & 1.911884, 1.568742, 2.001195,      0.0,     0.0,
     &    5*0.0/
C----------
C    MAPDSQ IS A VECTOR WHICH MAPS FOREST ONTO A DBH**2
C  COEFFICIENT.
C----------
      DATA MAPDSQ/
     &1,1,1,1,
     &1,1,1,1,
     &1,1,1,1,
     &4*1,
     &1,1,1,1,
     &1,1,1,1,
     &1,1,2,3,
     &1,1,1,1,
     &1,1,1,2,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &4*1,
     &1,1,1,1,
     &4*1/
C-------------
C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
C   SPECIES.
C  ORIGINAL EQUATION FOR SPECIES 1,2,16 HAD + TERM, CHANGED TO - TERM
C  TO AVOID RUNAWAY DIAMETER GROWTH  GD 01/11/00
C-------------
      DATA DGDS/
     & -0.0006538,      3*0.0,
     & -0.0006538,      3*0.0,
     & -0.0001997,      3*0.0,
     &      4*0.0,
     & -0.0001056,      3*0.0,
     &  0.0058612,      3*0.0,
     &        0.0, -0.0009803, -0.0016416,        0.0,
     & -0.0001056,      3*0.0,
     & -0.0002152, -0.0002567, 2*0.0,
     & -0.0004163,      3*0.0,
     &      4*0.0,
     &      4*0.0,
     & -0.0001737,        0.0,        0.0,        0.0,
     &      4*0.0,
     &      4*0.0,
     &      4*0.0,
     & -0.0006538,      3*0.0,
     &      4*0.0/
C----------
C  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
C  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
C  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
C  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
C  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
C  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
C  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.
C----------
      DATA DGCASP /
     & -0.609774, -0.609774, -0.268610,       0.0, -0.698103,
     & -0.042546, -0.075306, -0.698103,  -0.17839,  0.127311,
     &       0.0,       0.0,       0.0,       0.0,       0.0,  
     &  0.085958, -0.609774,       0.0/
C
      DATA DGSASP/
     & -0.017520, -0.017520,  0.076614,       0.0,  0.102053,
     &  0.332422, -0.036871,  0.102053,  0.052805,  0.076531,
     &       0.0,       0.0,       0.0,       0.0,       0.0,  
     &  -0.86398, -0.017520,       0.0/
C
       DATA DGSLOP/
     & -2.057060, -2.057060, -0.711260,       0.0,  1.335928,
     &  -2.43008, -0.129291,  1.335928,  0.784185,  0.024336,
     &       0.0,       0.0,       0.0,       0.0,       0.0,  
     &       0.0, -2.057060,       0.0/
C
       DATA DGSLSQ/
     &   2.11326,  2.113263,       0.0,       0.0, -1.481349,
     &       0.0,       0.0, -1.481349, -1.504007, -0.781480,
     &       0.0,       0.0,       0.0,       0.0,       0.0,  
     &       0.0,  2.113263,       0.0/
C
      DATA DGEL/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.012111,       0.0,       0.0, 
     & -0.075986,       0.0,       0.0/
C
      DATA DGEL2/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,  
     &  0.001193,       0.0,       0.0/
C
      DATA DGDBAL/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0, -0.006065,
     &       0.0,       0.0, -0.010222,       0.0,       0.0,
     & -0.001265,       0.0,       0.0/
C
      DATA DGBA/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     & -0.000981,       0.0,       0.0/
C
      DATA DGPCCF/
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.000757,       0.0,       0.0, 
     &       0.0,       0.0,       0.0/
C----------
C SITE BASE SPECIES REFERENCE
C 1=WB,LM,LP,OT / 2=DF / 3=ES,AF / 4=-- / 5=AS
C----------
      DATA IDGSIM/
     &1,1,1,1,1,
     &1,1,1,1,1,
     &1,1,1,1,1,
     &5*1,
     &1,1,1,2,2,
     &1,1,1,1,1,
     &1,1,1,2,2,
     &1,1,1,2,2,
     &1,1,1,2,2,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &5*1,
     &1,1,1,1,1,
     &5*1/
C
      DATA DGSIC /
     & 0.001766,    4*0.0,
     & 0.001766,    4*0.0,
     & 0.011597,    4*0.0,
     &    5*0.0,
     & 0.011389, 0.019985, 3*0.0,
     & 0.006460,    4*0.0,
     & 0.009756, 0.014334, 3*0.0,
     & 0.011389, 0.019985, 3*0.0,
     & 0.003955, 0.006310, 3*0.0,
     &    5*0.0,
     &    5*0.0,
     &    5*0.0,
     & 1.965888,    4*0.0,
     & 0.006460,    4*0.0,
     &    5*0.0,
     & 0.227307,    4*0.0,
     & 0.001766,    4*0.0,
     &    5*0.0/
C
      DATA ISMAP/ 1, 1, 2, 1, 3, 5, 1, 3, 3, 1,
     &            1, 1, 1, 1, 1, 1, 1, 1/
C----------
C  PONDEROSA PINE FROM CI VARIANT USES HABITAT TYPE. TT MAPPING IS MY
C  BEST GUESS AT MATCHING THE CI MAPPING.  IF THE NUMBER OF RECOGNIZED
C  HABITAT TYPES IN TT CHANGES, THEN THIS MAPPING WILL NEED TO BE ADJUSTED.
C  ged 02/17/10
C----------
      DATA ICHBCL/ 4*1,1,4*1,1,4*1,2*2,5*1,4,18*0,7*1,4*4,              !51
     & 222*0,6*1,2,2*3,2*4,4,3,4*0,3*4,12*0,2*0,0,0,0,0,0,5,3*0,        !52-316
     & 16*0,2*0,0,3*0,9*0,0,3*0,0,0,0,2*0,3*0,3*0,0/                    !317-363 
C
      DATA DGHAB/
     & 0.0, 0.006074, 0.181590, -0.196098, -0.055780, 0.133907,
     & 0.045857/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
C----------
C  DEBUG OUTPUT: MODEL COEFFICIENTS.
C----------
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
      IF(DEBUG)WRITE(JOSTND,*) ' DGCON= ', DGCON
C----------
C  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
C  NEEDS; COMPUTE THOSE HERE.
C----------
      AGERNG= 1000.
      YOUNG= 1000.
      OLD= 0.0
C----------
C CALL ARANGE AND BADIST
C----------
      CALL BADIST(DEBUG)
C----------
C FIND AGE DISTRIBUTION
C----------
      DO I= 1,ITRN
        IF(ABIRTH(I).LE.1.)GO TO 5
        IF(HT(I) .LE. 4.5) GO TO 5
        IF(ABIRTH(I) .LT. YOUNG) YOUNG = ABIRTH(I)
        IF(ABIRTH(I) .GT. OLD) OLD = ABIRTH(I)
    5   CONTINUE
C----------
C COMPUTE AGE RANGE.
C----------
        AGERNG = OLD - YOUNG
        AGERNG = ABS(AGERNG)
      ENDDO
C----------
C COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.
C----------
      SUMD2=0.
      SUMTRE=0.
      DGQMD=0.
      IF(ITRN .EQ. 0) GO TO 1001
      DO 1000 I=1,ITRN
      SUMD2=SUMD2+DBH(I)*DBH(I)*PROB(I)
      SUMTRE=SUMTRE+PROB(I)
 1000 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' SUMD2,SUMTRE= ',SUMD2,SUMTRE
      DGQMD = SQRT(SUMD2/SUMTRE)
 1001 CONTINUE
      IF(ICYC.GT.0 .AND. BA.GT.0.0) THEN
        IF(DGQMD.GT.0.)THEN
          STDSDI=SUMTRE*(DGQMD/10.0)**1.605
        ELSE
          STDSDI=0.
        ENDIF
        RELSDI=0.
        CALL SDICAL(SDIMAX)
        IF(SDIMAX.GT.0.)RELSDI=STDSDI/SDIMAX
        IF(DEBUG)WRITE(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',
     &  STDSDI,SDIMAX,RELSDI
        IF(RELSDI .GT. 0.85) RELSDI = 0.85
        DSTAG = 1.0
        IF(RELSDI .GT. 0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
        IF(DEBUG)WRITE(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
        IF(DEBUG)WRITE(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
      ENDIF
C----------
C  END OF SPECIAL NEEDS SECTION
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
      IF(ISPC .EQ. 10)THEN
        TEMBA=BA
        IF(TEMBA .LT. 1)TEMBA=1.
        CONSPP = CONSPP - 0.257322*ALOG(BA)
      ENDIF
C
      IF(DEBUG)WRITE(JOSTND,9001)ISPC,DGCON(ISPC),COR(ISPC),
     &DGCCF(ISPC),RELDEN,CONSPP
 9001 FORMAT(' IN DGF 9001 ISPC,DGCON,COR,DGCCF,RELDEN,CONSPP=',
     &I5,5F10.5)
      DGLDS= DGLD(ISPC)
      DGBALS = DGBAL(ISPC)
      DGCRS= DGCR(ISPC)
      DGCRS2=DGCRSQ(ISPC)
      DGDSQS=DGDSQ(ISPC)
      DGPTCC=DGPCCF(ISPC)
      DGDBLS=DGDBAL(ISPC)
      SI=SITEAR(ISPC)
C
      IF(DEBUG)WRITE(JOSTND,9002)ISPC,CONSPP,DGLDS,DGBALS,
     & DGCRS,DGCRS2,DGDSQS,DGPTCC,DGCCF(ISPC),DGPTCC
 9002 FORMAT(' DGF ISPC',I2,1X,6(E12.4,1X),3E10.4)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 10 I3=I1,I2
      I=IND1(I3)
      D=DIAM(I)
      BARK=BRATIO(ISPC,D,HT(I))
      IF (D.LE.0.0) GOTO 10
C
      SELECT CASE (ISPC)
C
      CASE(1:3,5,7:9,17)
        CR = ICR(I) * 0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA100
        DDS=CONSPP + DGLDS*ALOG(D) + DGBALS*BAL + CR*DGCRS+CR*CR*DGCRS2
     &      +DGDSQS*D*D
C
      CASE(10)
        CR = ICR(I) * 0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA
        PBAL= (1.0 - (PCT(I)/100.)) * PTBAA(ITRE(I))
        DDS=CONSPP +DGLDS*ALOG(D) + CR*(DGCRS+CR*DGCRS2)
     &    + DGDSQS*D*D  + DGDBLS*PBAL/(ALOG(D+1.0))
C
      CASE(6,14)
        CR = ICR(I)
        CALL DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
        DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
C
C NOTE: EQNS IN **REGENT** ARE USED FOR ALL SIZED TREES FOR PM, UJ,
C       RM, BI, AND MC. THIS CODE IS LEFT HERE JUST AS A PLACEHOLDER
C       IN CASE WE WANT TO CHANGE THIS IN THE FUTURE. CODE HERE
C       REFLECTS THE SOURCE VARIANT WHERE THE EQNS CAME FROM.
C
      CASE(4,11,12)
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
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
C
C NOTE: EQNS IN **REGENT** ARE USED FOR ALL SIZED TREES FOR PM, UJ,
C       RM, BI, AND MC. THIS CODE IS LEFT HERE JUST AS A PLACEHOLDER
C       IN CASE WE WANT TO CHANGE THIS IN THE FUTURE. CODE HERE
C       REFLECTS THE SOURCE VARIANT WHERE THE EQNS CAME FROM.
C
      CASE(13,16)
        ALD=ALOG(D)
        CR=ICR(I)*0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA
        IF(DEBUG)WRITE(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)
        DGLCCF=0.0
        DDS=CONSPP +DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)
     &             +DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))
     &             +DGPTCC*PCCF(ITRE(I))
     &             +DGBA(ISPC)*BA
C
C  USE SPRUCE-FIR MODEL TYPE LOWER BA LIMIT FOR SPECIES USING SURROGATE
C  EQUATIONS FROM THE CR VARIANT
C
      CASE(15,18)
        ICLS = IFIX(D + 1.0)
        IF(ICLS .GT. 41) ICLS = 41
        BAUTBA = BAU(ICLS) / BA
        SI = SITEAR(ISPC)
        DPP = D
        IF(DPP .LT. 1.0) DPP = 1.0
        BATEM = BA
        IF(BATEM .LT. 5.0) BATEM = 5.0
        DF = (1.55986 + 1.01825 * DPP - 0.29342 * ALOG(BATEM)
     &       + 0.00672*SI - 0.00073*BAUTBA)*1.05
        IF(DF .LT. DPP) DF = DPP
        DIAGR = (DF - DPP) * BARK
        IF(ISTAGF(ISPC).NE.0) DIAGR=DIAGR*DSTAG
        IF(DIAGR .LE. 0.) THEN
          DDS=-9.21
        ELSE
          DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
     &          + COR(ISPC) + DGCON(ISPC)     
          IF(DDS .LT. -9.21) DDS=-9.21
        ENDIF
C
      END SELECT
C---------
C     CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
C     FOR THE DENSITY OF NEIGHBORING STANDS.
C
      XPPDDS=0.
      CALL PPDGF (XPPDDS)
C
      DDS = DDS + XPPDDS
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(.NOT.DEBUG) GO TO 10
      WRITE(JOSTND,9003) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
 9003 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &      ',  BAL=',F7.2,',  CR=',F7.4/
     &      '       RELDEN=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
   10 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
C
      RETURN
C
C
      ENTRY DGCONS
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
C------------
C  PUT SITE INTO ONE OF 5 CLASSES
C----------
C**   XSITE=SITEAR(ISISP)-ISI*100
      XSITE = SITEAR(ISISP)
      LSI=XSITE/10.
      IF(LSI.LT.2)ISIC=1
      IF(LSI.GE.5)ISIC=5
      IF(LSI.GE.3 .AND. LSI.LT.4)ISIC=3
      IF(LSI.GE.4 .AND. LSI.LT.5)ISIC=4
      IF(LSI.GE.2 .AND. LSI.LT.3)ISIC=2
C----------
C ISISP IS THE SPECIES CODE THE SITE WAS TAKEN WITH
C ISIC IS THE SITE CLASS 1=0-29,2=30-39,...,5=60+ FOR THE SITE SPECIES
C----------
      ISIC2 = SITEAR(ISISP)
      IF(ISIC2.GT.59)ISIC2=60
      IF(ISIC2.LT.20)ISIC2=20
      ISC=ISIC2/10
C----------
C PUT THE SITE IN 10'S INTO A CLASS 1-5
C----------
      ISC=ISC-1
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO 30 ISPC=1,MAXSP
      KSI=IGCCFM(ISC,ISPC)
      DGCCF(ISPC) = DGCCFC(KSI,ISPC)
      ISPFOR=MAPLOC(IFOR,ISPC)
      ISPDSQ=MAPDSQ(IFOR,ISPC)
C----------
C  LOAD DGCON, DGDSQ,  AND ATTEN.   SHIFT ASPECT 45 DEGREES
C  FOR THE DIAMETER INCREMENT MODEL INTERCEPT CALCULATIONS.
C
C  NOTE: 0.7854 IS 45 DEGREES IN RADIANS. THIS CORRECTION IS
C        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
C        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.
C----------
      ISI=ISMAP(ISISP)
      TEMEL=ELEV
      XSITE = SITEAR(ISISP)
C
      SELECT CASE(ISPC)
C
      CASE(16)
        IF (TEMEL.GT.30.)TEMEL=30.
        XSITE=ALOG(XSITE)
        ASPTEM=ASPECT
C
      CASE(13)
        XSITE=ALOG(XSITE)
        ASPTEM=ASPECT
C
      CASE(10)
        ASPTEM=ASPECT
C
      CASE DEFAULT
        ASPTEM=ASPECT-0.7854
C
      END SELECT
C
      DGCON(ISPC)= DGSIC(IDGSIM(ISI,ISPC),ISPC) *XSITE
     &                 + DGFOR(ISPFOR,ISPC)
     &                 +(DGSASP(ISPC) * SIN(ASPTEM)
     &                 + DGCASP(ISPC) * COS(ASPTEM)
     &                 + DGSLOP(ISPC)) * SLOPE
     &                 + DGSLSQ(ISPC) * SLOPE * SLOPE
     &                 + DGEL(ISPC) * TEMEL
     &                 + DGEL2(ISPC) * TEMEL * TEMEL
      DGDSQ(ISPC)=DGDS(ISPDSQ,ISPC)
      SMCON(ISPC)=0.
C
      SELECT CASE (ISPC)
C
      CASE(10)
        IF(ITYPE .GT. 0)THEN
          MAPHAB = ICHBCL(ITYPE) + 1
        ELSE
          MAPHAB = 2
        ENDIF
        DGCON(ISPC) = DGCON(ISPC) + DGHAB(MAPHAB)
        ATTEN(ISPC)=OBSERV(ISPC)
C
      CASE(13,16)
        ATTEN(ISPC)=OBSERV(ISPC)
C
      CASE DEFAULT
        ATTEN(ISPC)=IBSERV(ISIC,ISPC)
C
      END SELECT
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
      IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC)
     &  + ALOG(COR2(ISPC))
   30 CONTINUE
C
      RETURN
      END
