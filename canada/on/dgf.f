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
C  VALUES ARE IN METRIC, AND MUST BE CONVERTED BEFORE AND AFTER
C  THE CALCULATIONS. THE
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
      REAL DIAM(MAXTRE),TEMD(MAXTRE)
      REAL B0(MAXEQ), B1(MAXEQ), B2(MAXEQ)
      REAL BBAL(MAXEQ), BHT(MAXEQ), BSI(MAXEQ), BBA(MAXEQ)
      REAL BDBHQ(MAXEQ), BAGS(MAXEQ), B95(MAXEQ)
      REAL BARK,D,SI,DELD,DGLN,DIAGR,DDS
      REAL X1,XPPDDS
      REAL DBHM,SIM,BAM,QMDM,BALM,HTM,AGS
      INTEGER I,J,I1,I2,I3,ISPC,KSP,ILOOP
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

C     BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

      DO 20 ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.EQ.0) GO TO 20
        I2=ISCT(ISPC,2)
        SI=SITEAR(ISPC)

C     DETERMINE IF WE USE THE NEW ONTARIO EQUATIONS.
C     NOTE, WHEN WE GET THE LAKE STATES PARAMETERS, WE WILL MODIFY
C     THESE LINES TO DETERMINE WHICH SET OF EQUATIONS GETS USED.

        IF (OSPMAP(ISPC) .EQ. 0) GOTO 20

        SIM  = SI    * FTtoM
        BAM  = BA    * FT2pACRtoM2pHA
        QMDM = RMSQD * INtoCM

C       BEGIN TREE LOOP WITHIN SPECIES ISPC.

        DO 10 I3=I1,I2
          I=IND1(I3)

C     IF THIS IS AN ISPC SPECIES FOR WHICH A QUALITY EQUATION
C     EXISTS AND THE INDIVIDUAL IS A QUALITY TREE (IMC=1), SELECT
C     COEFFICIENTS OFFSET BY +1; SIGN OF AGS COEFFS IS <0 BECAUSE
C     THE EQUATION IS CODED WITH SUBTRACTION, SO <0 GIVES A POSITIVE
C     IMPROVEMENT TO ANNUAL GROWTH RTE

          IF (LQUAL(ISPC).AND.IMC(I).EQ.1) THEN
            KSP = OSPMAP(ISPC)+1
            AGS = 1.
          ELSE
            KSP = OSPMAP(ISPC)
            AGS = 0.
          ENDIF

C     ITERATE 10 TIMES, SINCE DG EQUATION IS AN ANNUAL BASIS
C     NOTE THAT THERE IS NO UPDATING OF BAM, BALM, QMDM, OR HTM
C     SO THE ITERATION IS PEDANTIC... JUST TO BE THE SAME ACROSS
C     ALL FVS VARIANTS.

          BALM = (1.0 - (PCT(I)/100.)) * BAM
          HTM  = HT(I) * FTtoM

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
     >         - (BAGS(KSP)  * AGS)

          DO 1000 ILOOP=1,10
            D=TEMD(I)
            WK2(I)=0.0
            IF(D .LE. 0.0) GO TO 10

C     CM/YR DIAMETER GROWTH

            DBHM = D * INtoCM
            DGLN = X1
     >             + (B1(KSP) * LOG(DBHM))
     >             - (B2(KSP) * DBHM)

C     JUST TO BE SAFE, CONSTRAIN PREDICTED DG TO +/- 5 BEFORE
C     EXPONENTIATION; THEN CONSTRAIN THE ANNUAL GROWTH TO
C     0.001 - 95TH% FROM B95()

            DELD = MIN(MAX(DGLN,-5.0),5.0)
            DELD = EXP(DELD)
            DELD = MIN(MAX(DELD,0.0001),B95(KSP))
            TEMD(I) = TEMD(I) + DELD * CMtoIN

 1000     CONTINUE

          BARK=BRATIO(ISPC,TEMD(I),HT(I))
          DIAGR=(TEMD(I)-DIAM(I))*BARK

          IF(DEBUG)
     &      WRITE(JOSTND,*)' I,TEMD,DIAM,BARK,DIAGR= ',
     &      I,TEMD(I),DIAM(I),BARK,DIAGR

          IF(LDCOR2 .AND. COR2(ISPC) .GT. 0.0)DIAGR= DIAGR*COR2(ISPC)
          IF(DIAGR.LE. .0001) DIAGR=.0001

          IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',
     &      I,ISPC,COR2(ISPC),DIAGR,LDCOR2

          DDS=DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
          IF(DEBUG)WRITE(JOSTND,*)' I,DIAGR,DIAM(I),BARK,DDS= ',
     &      I,DIAGR,DIAM(I),BARK,DDS

C     CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
C     FOR THE DENSITY OF NEIGHBORING STANDS.

          X1=0.
          XPPDDS=0.
          CALL PPDGF (XPPDDS,X1,X1,X1,X1,X1,X1)

          IF (DDS .GT. 0.0) WK2(I)=ALOG(DDS)+COR(ISPC)+XPPDDS

   10   CONTINUE
   20 CONTINUE   ! END OF SPECIES LOOP.

      IF(DEBUG)WRITE(JOSTND,100)ICYC
  100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
      RETURN

C     ENTRY DGCONS

      ENTRY DGCONS
      DO 120 ISPC=1,MAXSP
        IF (OSPMAP(ISPC) .GT. 0) THEN
              J = OBSERV(OSPMAP(ISPC))
        ELSE
          J = 1.0
        ENDIF
        DGCON(ISPC)=0.
        ATTEN(ISPC)=FLOAT(J)
        SMCON(ISPC)=0.
  120 CONTINUE
      RETURN
      END
