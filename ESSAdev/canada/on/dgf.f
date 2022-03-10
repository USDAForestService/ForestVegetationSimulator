       SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  IF SPECIES HAVE BEEN CALIBRATED FOR ONTARIO (FORKOD 915)
C  THEN DIAMETER GROWTH IS PREDICTED FROM NEW EQUATIONS USING
C  DBH, SITE INDEX, STAND AVERAGE DBH, AND BASAL AREA. NOTE THAT ALL
C  MODEL EQUATION PARAMETERS ARE METRIC, AND MUST BE CONVERTE
C  BEFORE AND AFTER THE CALCULATIONS. THE
C  SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE ARGUMENT TO
C  DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO PROCESS DIFFERENT
C  CALIBRATION OPTIONS.
C  THIS ROUTINE IS CALLED BY **DGDRIV** DURING
C  CALIBRATION AND WHILE CYCLING FOR GROWTH PREDICTION.  ENTRY
C  **DGCONS** IS CALLED BY **RCON** TO LOAD SITE DEPENDENT COEFFICIENTS
C  THAT NEED ONLY BE RESOLVED ONCE.
C
C  02/23/06 - ONTARIO DIAMETER GROWTH EQUATIONS ARE FROM MARGARET PENNER
C
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CALCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'PDEN.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C----------
C  VARIABLES DEFINED:
C  DELD  -- TREE'S DIAMETER GROWTH FOR ONE YEAR
C  COEFFICIENTS DEFINED:
C  OSPMAP   -- SPECIES MAPPING FOR ONTARIO EQUATIONS
C  DM, SIM, B, Q -- DBH, SI, BA, RMSQD IN METRIC
C  DGLN     -- LN(DBH_GROWTH) (METRIC)

C     NUMBER OF LARGE TREE DG EQUATIONS

      INTEGER   MAXEQ
      PARAMETER (MAXEQ=35)

      LOGICAL DEBUG, LQUAL(MAXSP)
      LOGICAL LHYBRID, LMBLEAD
      REAL DIAM(MAXTRE),TEMD(MAXTRE),BAMSP(MAXSP,2)
      REAL B0(MAXEQ), B1(MAXEQ), B2(MAXEQ)
      REAL BBAL(MAXEQ), BHT(MAXEQ), BSI(MAXEQ), BBA(MAXEQ)
      REAL BDBHQ(MAXEQ), BAGS(MAXEQ), B95(MAXEQ)
      REAL BARK,D,SI,DELD,DGLN,DIAGR,DDS
      REAL DBHM,SIM,BAM,QMDM,BALM,HTM
      REAL AGSBAM,X,A1,A2,BAM2,BAM3,BB
      REAL PHWBAM
      REAL R, A, B, C, X1, X2, Y
      INTEGER I,J,I1,I2,I3,ISPC,KSP,ILOOP,AGS,ITCR,ITCR2,ICUT,JI
      INTEGER OSPMAP(MAXSP),OBSERV(MAXEQ)

      REAL    BRATIO

C     MAPPING OF FVS SPECIES INTO ONTARIO GROWTH SPECIES.

      DATA OSPMAP/
     >  5,  4,  3,  4,  1,  7,  8, 12,  9, 14,  !10
     > 13, 11,  9,  9, 26, 28, 35, 17, 17, 33,  !20
     > 17, 17, 17, 19, 29, 15, 15, 24, 28, 24,  !30
     > 17, 24, 15, 22, 26, 26, 33, 33, 17, 35,  !40
     > 35, 35, 21, 15, 31, 31, 32, 31, 21, 17,  !50
     > 17, 17, 32, 17, 17, 17, 17, 17, 17, 17,  !60
     > 17, 17, 17, 17, 17, 17, 17, 17,  6,  2,  !70
     >  8, 10/

C     THESE 8 FVS SPECIES HAVE A QUALITY CODE. THE SPECIES
C     RECORDED BY OSPMAP IS THE FIRST OF THE 2 PAIRS; IF THE
C     QUALITY CODE REQUIRES IT (IMC = 1; AGS EQN) THE EQUATION
C     NUMBER IS +1, OTHER LOGIC IS USED TO FETCH THE NECESSARY
C     COEFFICIENTS
C
C     SPECIES            FVS SPP  DG EQN
C     =================  =======  ======
C     BLACK ASH          15       26/27
C     SOFT MAPLE         19       17/18
C     BLACK CHERRY       20       33/34
C     YELLOW BIRCH       24       19/20
C     BASSWOOD           25       29/30
C     SUGAR MAPLE        26       15/16
C     AMERICAN BEECH     28       24/25
C     RED OAK            34       22/23
C     =================  =======  ======

      DATA LQUAL /
     >  14*.FALSE.,
     >     .TRUE.,   ! FVS SPP 15 -> 26/27
     >   3*.FALSE.,
     >     .TRUE.,   ! FVS SPP 19 -> 17/18
     >     .TRUE.,   ! FVS SPP 20 -> 33/34
     >   3*.FALSE.,
     >     .TRUE.,   ! FVS SPP 24 -> 19/20
     >     .TRUE.,   ! FVS SPP 25 -> 29/30
     >     .TRUE.,   ! FVS SPP 26 -> 15/16
     >   1*.FALSE.,
     >     .TRUE.,   ! FVS SPP 28 -> 24/25
     >   5*.FALSE.,
     >     .TRUE.,   ! FVS SPP 34 -> 22/23
     >  38*.FALSE. /

      DATA B0 /
     >  3.691,   1.8979,  5.451,   1.1499,  4.184,
     >  2.1184,  1.3936,  1.5596,  2.8199,  1.0315,  !10
     >  1.4697,  1.4792,  2.3705,  3.3822,  2.256,
     >  2.0375,  1.5782,  1.626,   0.8673,  1.5838,  !20
     >  2.6442,  1.4321,  2.2574,  1.0566,  1.2195,
     >  0.,      0.,      2.9716,  3.2426,  3.388,   !30
     > 18.6647,  1.6447,  2.5493,  1.1281,  1.5392 /

      DATA B1 /
     >  1.0989,  0.2965,  1.5444,  0.,      1.0302,
     >  0.6541,  0.0648,  0.1237,  0.,      0.,      !10
     >  0.1872,  0.0138,  0.2704,  0.6565,  0.6342,
     >  0.4857,  0.0787,  0.0892,  0.196,   0.3696,  !20
     >  0.3003,  0.,      0.3295,  0.286,   0.2808,
     >  0.5888,  0.6024,  0.8478,  1.1018,  1.1824,  !30
     >  7.7658,  0.0835,  0.6732,  0.,      0.038 /

      DATA B2 /
     >  0.0396,  0.0045,  0.0766,  0.0282,  0.074,
     >  0.1046,  0.0158,  0.0525,  0.0147,  0.0803,
     >  0.00433, 0.000774,0.,      0.00127, 0.026,
     >  0.0215,  0.,      0.,      0.0189,  0.0322,
     >  0.,      0.00166, 0.,      0.0168,  0.0148,
     >  0.0692,  0.0644,  0.0234,  0.0323,  0.0372,
     >  0.297,   0.00941, 0.0186,  0.00176, 0. /

      DATA BBAL /
     >  0.0477,  0.0305,  0.0205,  0.0266,  0.0426,
     >  0.0552,  0.0356,  0.0661,  0.0217,  0.0468,  !10
     >  0.0225,  0.000676,0.,      0.0111,  0.0155,
     >  0.0185,  0.00224, 0.00211, 0.0302,  0.0325,  !20
     >  0.0209,  0.0486,  0.015,   0.00708, 0.00837,
     >  0.0295,  0.07,    0.0131,  0.00381, 0.00655, !30
     >  0.0545,  0.0327,  0.0045,  0.0151,  0.0267 /

      DATA BHT /
     >  0.,      0.,      0.,      0.,      0.,
     >  0.,      0.,      0.,      0.,      0.,      !10
     >  0.,      0.,      0.,      0.,      0.,
     >  0.,      0.,      0.,      0.0132,  0.,      !20
     >  0.,      0.,      0.,      0.,      0.,
     >  0.,      0.,      0.,      0.,      0.,      !30
     >  0.,      0.,      0.,      0.,      0. /

      DATA BSI /
     > -0.0117,  0.,     -0.0351, -0.0836, -0.0624,
     > -0.0379, -0.018,  -0.0596, -0.093,  -0.0665,  !10
     >  0.,      0.00166, 0.,      0.,      0.,
     >  0.,      0.,      0.,      0.,      0.,      !20
     >  0.0101, -0.025,   0.,      0.,      0.,
     >  0.,      0.,      0.,      0.,      0.,      !30
     >  0.,      0.,      0.,      0.,     -0.0211 /

      DATA BBA /
     >  0.,      0.0041,  0.,      0.0257,  0.,
     >  0.,      0.,      0.,      0.,      0.,      !10
     >  0.0226,  0.,      0.00875, 0.0244,  0.00705,
     >  0.00292, 0.,      0.,      0.,      0.,      !20
     >  0.,      0.,      0.,      0.0181,  0.0145,
     >  0.1151,  0.1273,  0.,      0.0215,  0.0205,  !30
     >  0.,      0.,      0.022,   0.0111,  0.00405 /

      DATA BDBHQ /
     > -0.015,  -0.0089, -0.0343,  0.,       0.,
     >  0.,      0.,      0.,      0.,       0.,    !10
     > -0.0201,  0.,      0.,      0.,       0.,
     >  0.,      0.,      0.,      0.000716,-0.012, !20
     >  0.,      0.,      0.,      0.,       0.,
     >  0.,      0.,      0.,      0.,       0.,    !30
     >  0.,      0.,      0.,      0.,       0. /

      DATA BAGS /
     >  0.,      0.,      0.,      0.,      0.,
     >  0.,      0.,      0.,      0.,      0.,     !10
     >  0.,      0.,      0.,      0.,      0.,
     > -0.2502,  0.,     -0.0622,  0.,     -0.1575, !20
     >  0.,      0.,      0.0612,  0.,     -0.199,
     >  0.,     -1.6033,  0.,      0.,     -0.158,  !30
     >  0.,      0.,      0.,     -0.5982,  0. /

      DATA B95 /
     >  0.56,    0.51667, 0.52,    0.46228, 0.24,
     >  0.48,    0.39,    0.557,   0.22,    0.517,  !10
     >  0.56,    0.422,   0.32,    0.2,     0.55,
     >  0.55,    0.52,    0.52,    0.514,   0.514,  !20
     >  0.241,   0.46,    0.46,    0.575,   0.575,
     >  0.48,    0.48,    0.614,   0.57,    0.57,   !30
     >  0.3,     0.28,    0.46,    0.46,    0.38 /

C  THE NUMBER OF OBSERVATIONS BY EQUATION COEFFICIENT.
C  THERE ARE SOME CASES WHERE THERE IS A TREE-QUALITY TERM
C  FOR A SPECIES; AND BECAUSE OF THEY THE FITS WERE DONE,
C  THERE ARE 2 EQUATIONS. IN THESE CASES THE FIRST (NO
C  QUALITY) SAMPLE SIZE IS USED.

      DATA  OBSERV/
     >  13692, 5314, 4954,92648,31572,
     >   4879, 1883, 4591,56172, 9139, !10
     >    830, 6281,  601,  613,26739,
     >  26739,  741,  741,  508, 1453, !20
     >   4394,  595,  740, 3971, 3971,
     >    269,  269,  284,  934,  934, !30
     >    115,  259,  274,  274,11183/

C     SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

C     STORE DIAMETERS FOR ITERATIVE PROCESSING

      DO 4 I=1,MAXTRE
        TEMD(I)=DIAM(I)
    4 CONTINUE

C     METRIC STAND-LEVEL PARAMETERS
      BAM   = BA    * FT2pACRtoM2pHA
      QMDM  = RMSQD * INtoCM
      AGSBAM = 0.

C     NUMBER OF LOOPS TO ITERATE THE ANNUAL DG GROWTH MODEL
C     FOR THE TIMESTEP SIZE
c     J = 10  ! ASSUME A 10 YEAR CYCLE AND ADJUST LATER

C     CALCULATE BAMSP (M**2/HA) FOR EACH SPECIES. THOSE 
C     WITH IMC=1 ARE PUT IN AN "AGS" SUBGROUP (JI=1); OTHERWISE
C     INTO A "UGS" SUBGROUP (JI=2). THOSE ASSIGNMENTS ARE ONLY 
C     MEANINGFUL FOR A SUBSET OF COMMERCIAL SPECIES
      DO ISPC=1,MAXSP
        BAMSP(ISPC,1)= 0.
        BAMSP(ISPC,2)= 0.
        I1=ISCT(ISPC,1)
        IF (I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            I=IND1(I3)
            DBHM = DIAM(I) * INtoCM
            IF (IMC(I) .EQ. 1) THEN
              JI = 1
            ELSE
              JI = 2
            ENDIF
            BAMSP(ISPC,JI) = BAMSP(ISPC,JI) +
     >        ((3.141593*(DBHM*0.5)**2)*PROB(I))
          ENDDO
          BAMSP(ISPC,1) = (BAMSP(ISPC,1)/10000.)/ACRtoHA
          BAMSP(ISPC,2) = (BAMSP(ISPC,2)/10000.)/ACRtoHA
        ENDIF
      ENDDO  

C     BA IN AGS HARDWOODS USED BY PENNER (MAPLES, BEECH, IRONWOOD, BASSWOOD)
C     THIS MIGHT ACTUALLY NEED TO BE ALL AGS SPECIES, NOT JUST PENNER 2020

      AGSBAM = BAMSP(18,1) + BAMSP(19,1) + BAMSP(25,1) + BAMSP(26,1) +
     >         BAMSP(27,1) + BAMSP(28,1) + BAMSP(47,1)

C     SET FLAG IF BEECH (28) OR MAPLE (18,19,26,27) ARE LEADING BY BA
      LMBLEAD = .FALSE.
      X =  0.
      I = -1
      DO ISPC=1,MAXSP
        Y = BAMSP(ISPC,1) + BAMSP(ISPC,2)
        IF (Y .GT. X) THEN
          X = Y
          I = ISPC
        ENDIF
      ENDDO
      IF ((I.EQ.18) .OR. (I.EQ.19) .OR. (I.EQ.26)
     > .OR. (I.EQ.27) .OR. (I.EQ.28)) THEN
        LMBLEAD = .TRUE.
      ENDIF
      
C     BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

      DO 20 ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.EQ.0) GO TO 20
        I2=ISCT(ISPC,2)
        SI=SITEAR(ISPC)
        SIM=SI*FTtoM

C     DETERMINE IF WE USE THE NEW ONTARIO EQUATIONS.
C     NOTE, WHEN WE GET THE LAKE STATES PARAMETERS, WE WILL MODIFY
C     THESE LINES TO DETERMINE WHICH SET OF EQUATIONS GETS USED.

        IF (OSPMAP(ISPC) .EQ. 0) GOTO 20

C       BEGIN TREE LOOP WITHIN SPECIES ISPC.

        DO 10 I3=I1,I2
          I=IND1(I3)
          
          WK2(I)=0.0
          IF(DIAM(I) .LE. 0.0) GO TO 10

C     IF THIS IS AN ISPC SPECIES FOR WHICH A QUALITY EQUATION
C     EXISTS AND THE INDIVIDUAL IS A QUALITY TREE (IMC=1), SELECT
C     COEFFICIENTS OFFSET BY +1; SIGN OF AGS COEFFS IS <0 BECAUSE
C     THE EQUATION IS CODED WITH SUBTRACTION, SO <0 GIVES A POSITIVE
C     IMPROVEMENT TO ANNUAL GROWTH RTE

          IF (LQUAL(ISPC).AND.IMC(I).EQ.1) THEN
            KSP = OSPMAP(ISPC)+1
            AGS = 1
          ELSE
            KSP = OSPMAP(ISPC)
            AGS = 0
          ENDIF

C     ITERATE 10 TIMES AT 1000, SINCE DG EQUATION IS AN ANNUAL BASIS
C     NOTE THAT THERE IS NO UPDATING OF BAM, BALM, QMDM, OR HTM
C     SO THE ITERATION IS PEDANTIC... JUST TO BE THE SAME ACROSS
C     ALL FVS VARIANTS.

          BALM = (1.0 - (PCT(I)/100.)) * BAM
          HTM  = HT(I) * FTtoM
          DBHM = DIAM(I) * INtoCM

C     THE ENTIRE DG EQUATION IS BROKEN INTO 2 PIECES; FIRST
C     PIECE IS CONSTANT OVER THE ITERATIONS; 2ND PIECE
C     INVOLVES DBH AND IS UPDATED

          X1 = - (B0(KSP))
C    >         + (B1(KSP)    * LOG(DBHM))
C    >         - (B2(KSP)    * DBHM)
     >         - (BBAL(KSP)  * BALM)
     >         - (BHT(KSP)   * HTM)
     >         - (BSI(KSP)   * SIM)
     >         - (BBA(KSP)   * BAM)
     >         - (BDBHQ(KSP) * QMDM)
     >         - (BAGS(KSP)  * REAL(AGS))

C         LN CM/YR DIAMETER GROWTH STANDARD 10 YEAR PROJECTION
          DO ILOOP = 1,10
           DGLN = X1
     >             + (B1(KSP) * LOG(DBHM))
     >             - (B2(KSP) * DBHM)

C           JUST TO BE SAFE, CONSTRAIN PREDICTED DG TO +/- 5 BEFORE
C           EXPONENTIATION; THEN CONSTRAIN THE ANNUAL GROWTH TO
C           0.001 - 95TH% FROM B95()
            DELD = MIN(MAX(DGLN,-5.0),5.0)
            DELD = EXP(DELD)
            DELD = MIN(MAX(DELD,0.0001),B95(KSP))
            DBHM = DBHM + DELD
          ENDDO

          D = DBHM * CMtoIN
          BARK=BRATIO(ISPC,D,HT(I))
          DIAGR=(D-DIAM(I))*BARK
          TEMD(I) = DIAGR

C     FINAL ASSIGNMENT OF DDS AND WK2() IS NOW DONE AT THE END, TO ALLOW
C     FOR MODIFICATION OF DIAMETER GROWTH PREDCITION (DIAGR) FOR THE HYBRID
C     HARDWOOD GROWTH MODEL.

!          IF(DEBUG)
!     &      WRITE(JOSTND,*)' I,DIAM,DIAGR,BARK= ',
!     &      I,DIAM(I),DIAGR,BARK
!
!          IF(LDCOR2 .AND. COR2(ISPC) .GT. 0.0)DIAGR= DIAGR*COR2(ISPC)
!          IF(DIAGR.LE. .0001) DIAGR=.0001
!          TEMD(I) = DIAGR
!
!          IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',
!     &      I,ISPC,COR2(ISPC),DIAGR,LDCOR2
!
!          DDS=DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
!          IF(DEBUG)WRITE(JOSTND,*)' I,DIAGR,DIAM(I),BARK,DDS= ',
!     &      I,DIAGR,DIAM(I),BARK,DDS
!
!          IF (DDS .GT. 0.0) WK2(I)=ALOG(DDS)+COR(ISPC)

   10   CONTINUE
   20 CONTINUE   ! END OF SPECIES LOOP.
 
C     MODIFY THE DIAMETER GROWTH PREDICTION USING PENNER-ADJUSTMENT IF THERE IS
C     (A) LIVE BA, (B) AT LEAST 50% OF BA IN SHADE-TOLERANT SPECIES AND
C     (C) MAPLE OR BEECH ARE THE LEADING SPECIES
      LHYBRID = .FALSE.
      IF ((BAM .GT. 0.) .AND. (AGSBAM/BAM .GT. 0.50) .AND. LMBLEAD) THEN
        LHYBRID = .TRUE.
      ENDIF
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     Temporarily disable the hybrid model for BlueSource 09-Mar-2022

      LHYBRID = .FALSE.

CCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (LHYBRID) THEN

C       STAND-LEVEL ADJUSTMENT OF GROWTH FOR HARDWOODS BASED ON PENNER.
C       (2020) "HARDWOOD SELECTION GROWTH RATES".
C       BEECH, SUGAR MAPLE, RED MAPLE, SILVER MAPLE, IRONWOOD, BLACK MAPLE
C       THE CALCULATIONS USE ENTIRE STAND BA, DQ, ETC., AND
C       NOT JUST THE HARDWOOD TREES THAT ARE THE TARGET FOR THE ADJUSTMENT

C       TIMBER CAPABILITY RATING (TCR) FROM PENNER (2020) TABLE 5.
C       DO-LOOP BELOW USES TVOL ACCRETION IN THE PREVIOUS TIMESTEP
C       (BEST WE CAN DO) IN IMPERIAL UNITS NATIVE TO IOSUM().

C       TCR   FT3/AC/YR   M3/HA/YR
C         0          NA         NA
C         1        >110       >7.7
C         2      91-110    6.3-7.7
C         3      71-90     4.9-6.3
C         4      51-70     3.5-4.9
C         5      31-50     2.1-3.5
C         6      11-30     0.7-2.1
C         7        <11        <0.7

C       THE 15TH ENTRY IN IOSUM() IS TOTAL VOLUME ACCRETION FT3/AC/YR 
C       IN A CYCLE. THIS SHOULD FIND ACCRETION FOR THE PREVIOUS CYCLE
C       DEFAULT IN THE FIRST CYCLE IS ITCR2=5

        ITCR = 0
        IF (ICYC .GT. 1) THEN
          ITCR= IOSUM(15,ICYC-1)
        ENDIF

        ITCR2 = 5
        IF (ITCR .EQ. 0) THEN
          ITCR2 = 5
        ELSEIF (ITCR .GT. 110) THEN
          ITCR2 = 1
        ELSEIF (ITCR .GT. 91 .AND. ITCR .LE. 110) THEN
          ITCR2 = 2
        ELSEIF (ITCR .GT. 71 .AND. ITCR .LE.  91) THEN
          ITCR2 = 3
        ELSEIF (ITCR .GT. 51 .AND. ITCR .LE.  71) THEN
          ITCR2 = 4
        ELSEIF (ITCR .GT. 31 .AND. ITCR .LE.  51) THEN
          ITCR2 = 5
        ELSEIF (ITCR .GT. 11 .AND. ITCR .LE.  31) THEN
          ITCR2 = 6
        ELSE
          ITCR2 = 7
        ENDIF

C       THE 7TH ENTRY IN IOSUM() IS TOTAL TPA REMOVED IN THE CYCLE
C       FIRST CHECK REMOVALS IN THE CURRENT CYCLE, THEN CHECK IF
C       ANYTHING HAS BEEN REMOVED IN THE LAST 5 YEARS, SET ICUT=1
        ICUT = 0
        IF (ICYC .EQ. 1) THEN
          IF (ONTREM(7) .GT. 0.0) THEN
            ICUT = 1
          ENDIF
        ELSE
          DO I= ICYC-1, 1, -1
            IF (IY(ICYC)-IY(I) .LE. 5) THEN
              IF (IOSUM(7,I) .GT. 0) THEN
                ICUT = 1
                EXIT
              ENDIF
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDIF

        X =     0.07292
     &       - (0.00178 * BAM)
     &       + (0.01464 * AGSBAM/BAM)
     &       + (0.00217 * REAL(ITCR2))
     &       - (0.00093 * QMDM)
     &       + (0.00404 * -1.0) ! SET Q = -1.0
     &       - (0.00974 * REAL(ICUT))

        X = X / BAM
        
C       BAM3 IS THE PREDICTED BA CHANGE FOR THE CYCLE BASED ON PENNER STAND
C       MODEL, CONVERTED TO FT2/AC USED INTERNALLY. MULTIPLY BY 10 FOR 10-YEAR
C       STANDARD PROJECTION ADJUSTED LATER FOR ACTUAL CYCLE
        BAM3 = X * BAM * 10. * M2pHAtoFT2pACR
      
C       BAM2 IS THE PREDICTED BA CHANGE FOR THE CYCLE BASED ON THE ORIGINAL
C       MODEL, in FT2/AC
        BAM2 = 0.0
        DO 201 ISPC=1,MAXSP
          I1=ISCT(ISPC,1)
          IF(I1.EQ.0) GO TO 201
          I2=ISCT(ISPC,2)
          DO 202 I3=I1,I2
            I=IND1(I3)
            A2 = ((DIAM(I)+TEMD(I))/24.0)**2
            A1 = (DIAM(I)/24.0)**2
            BAM2 = BAM2 + PROB(I) * 3.141596 * (A2-A1)
  202     ENDDO 
  201   ENDDO

C       ADJUSTMENT TO DIAMETER CHANGE BASED ON ADJUSTMENT OF THE BASAL
C       AREA GROWTH "RING"
C
C       BB * PI * (R**2 + 2*R*X2 + X2**2 - R**2) = PI * (R**2 + 2*R*X1 + X1**2 - R**2)
C       0 = X1**2 + 2*R*X1 + (-BB * (2*R*X2 + X2**2)) 
C
C       WHERE:
C       BB = BAM3/BAM2 (I.E. PENNER/FVS) RATIO OF THE BASAL AREA CHANGE FROM THE TWO MODELS
C       R  = CURRENT RADIUS BASED ON DIAMETER DIAM(I)/2
C       X2 = RADIUS CHANGE PREDICTED BY DIAMETER CHANGE BASED ON TEMDG(I)/2 IN ORIGINAL MODEL
C       X1 = RADIUS CHANGE FOLLOWING ADJUSTMENT CALCULATION (POSITIVE ROOT OF QUADRATIC)

        A = 1.0
        BB = BAM3/BAM2
        DO 203 ISPC=1,MAXSP
          I1=ISCT(ISPC,1)
          IF(I1.EQ.0) GO TO 203
          I2=ISCT(ISPC,2)
          DO 204 I3=I1,I2
            I=IND1(I3)
            B = DIAM(I)
            R = B * 0.5
            X2 = TEMD(I) * 0.5
            C = -BB * ((2.0 * R * X2) + (X2 * X2))
            X1 = (-B + SQRT(B * B - (4.0 * A * C))) * 0.5
            TEMD(I) = X1 * 2.0
  204     ENDDO   
  203   ENDDO

        IF (DEBUG) THEN
          WRITE(JOSTND,*)' BAM,AGSBAM,ITCR2,QMDM,ICUT=',
     &      BAM,AGSBAM,ITCR2,QMDM,ICUT
          WRITE(JOSTND,*)' BB,BAM3,BAM2,A,B,C=',
     &      BB,BAM3,BAM2,A,B,C
        ENDIF
        
      ENDIF ! END OF LHYBRID IF/ENDIF
      
      DO 303 ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.EQ.0) GO TO 303
        I2=ISCT(ISPC,2)
        DO 304 I3=I1,I2
          I=IND1(I3)
          DIAGR = TEMD(I)
          IF(DEBUG)
     &      WRITE(JOSTND,*)' I,DIAM,DIAGR,BARK= ',
     &      I,DIAM(I),DIAGR,BARK

          IF(LDCOR2 .AND. COR2(ISPC) .GT. 0.0)DIAGR= DIAGR*COR2(ISPC)
          IF(DIAGR.LE. .0001) DIAGR=.0001

          IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',
     &      I,ISPC,COR2(ISPC),DIAGR,LDCOR2

          DDS=DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
          IF(DEBUG)WRITE(JOSTND,*)' I,DIAGR,DIAM(I),BARK,DDS= ',
     &      I,DIAGR,DIAM(I),BARK,DDS

          IF (DDS .GT. 0.0) WK2(I)=ALOG(DDS)+COR(ISPC)
  304   ENDDO
  303 ENDDO


      IF(DEBUG)WRITE(JOSTND,100)ICYC
  100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
      RETURN

C     ENTRY DGCONS

      ENTRY DGCONS
      DO 120 ISPC=1,MAXSP
        IF (OSPMAP(ISPC) .GT. 0) THEN
          J = OBSERV(OSPMAP(ISPC))
        ELSE
          J = 1
        ENDIF
        DGCON(ISPC)=0.
        ATTEN(ISPC)=REAL(J)
        SMCON(ISPC)=0.
  120 CONTINUE
      RETURN
      END
