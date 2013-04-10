      SUBROUTINE CROWN
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND COMPUTE CROWN
C  RATIO CHANGES FOR TREES THAT ARE GREATER THAN 3 INCHES DBH.  CROWN
C  RATIO IS PREDICTED FROM HABITAT TYPE, BASAL AREA, CROWN COMPETITION
C  FACTOR, DBH, TREE HEIGHT, AND PERCENTILE IN THE BASAL AREA
C  DISTRIBUTION.  WHEN THE EQUATION IS USED TO PREDICT CROWN RATIO
C  CHANGE, VALUES OF THE PREDICTOR VARIABLES FROM THE START OF THE CYCLE
C  ARE USED TO PREDICT OLD CROWN RATIO, VALUES FROM THE END OF THE CYCLE
C  ARE USED TO PREDICT NEW CROWN RATIO, AND THE CHANGE IS COMPUTED BY
C  SUBTRACTION.  THE CHANGE IS APPLIED TO ACTUAL CROWN RATIO.  THIS
C   ROUTINE IS CALLED FROM **CRATET** TO DUB MISSING VALUES, AND BY
C  **TREGRO** TO COMPUTE CHANGE DURING REGULAR CYCLING.  ENTRY
C  **CRCONS** IS CALLED BY **RCON** TO LOAD MODEL CONSTANTS THAT ARE
C  SITE DEPENDENT AND NEED ONLY BE RESOLVED ONCE.  A CALL TO **DUBSCR**
C  IS ISSUED TO DUB CROWN RATIO WHEN DBH IS LESS THAN 3 INCHES.
C  PROCESSING OF CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY
C  **REGENT**.
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
      INCLUDE 'BCPLOT.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C----------
C  DECLARATIONS AND DIMENSIONS FOR INTERNAL VARIABLES:
C
C  DCRCON -- CONSTANT TERM FOR THE CROWN RATIO MODEL BASED ON
C            PAST TREE STAND ATTRIBUTES
C  XCRCON -- CONSTANT TERM FOR THE CROWN RATIO MODEL BASED ON
C            CURRENT TREE STAND ATTRIBUTES
C    PARM -- ARRAY OF COEFFICIENTS FOR TERMS INVOLVING TREE
C            AND STAND ATTRIBUTES
C----------

      INTEGER   P_ZN
	PARAMETER (P_ZN = 2)

      TYPE CR_STR
	  CHARACTER (LEN=4)  :: ZONE          ! BEC zone  
        INTEGER            :: SEQ(MAXSP)    ! species list
        INTEGER            :: OBSERV(MAXSP) ! observations
        REAL               :: CON(MAXSP)    ! INTERCEPT
        REAL               :: HTDBH(MAXSP)  ! HT/DBH
        REAL               :: HT(MAXSP)     ! HT (M)
        REAL               :: DBH2(MAXSP)   ! DBH**2 (CM**2)
        REAL               :: BAL(MAXSP)    ! BAL (M**2/HA)
        REAL               :: LNCCF(MAXSP)  ! LN(CCF)
        REAL               :: EL(MAXSP)     ! ELEVATION (HECTAMETRES)
        REAL               :: EL2(MAXSP)    ! ELEVATION**2 (HM**2)
        REAL               :: SLP(MAXSP)    ! SLOPE (0-1)
        REAL               :: SLP2(MAXSP)   ! SLOPE**2
        REAL               :: SASP(MAXSP)   ! SIN-ASPECT: SLP*SIN(ASP)
        REAL               :: CASP(MAXSP)   ! COS-ASPECT: SLP*COS(ASP)
        REAL               :: SEE(MAXSP)    ! STD ERR EST
      END TYPE

      EXTERNAL RANN
      LOGICAL  DEBUG
      
      REAL     CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP),PRM(5),CHG,PDIFPY
      INTEGER  MYACTS(1),ICFLG(MAXSP),NTODO,IDATE,IACTK,NP,IDT
	INTEGER  ISPCC,IGRP,IULIM,IG,IGSP
	INTEGER  IZN,ISPC,I1,I2,I3,I,ICRI,J
      INTEGER  OLDZN
    	REAL     RDM1,OBA,D,H,BARK,BRATIO,CRSDX
	REAL     X1,X2,XCRCON,DCRCON,P,EXPPCR,EXPDCR
	REAL     HN,HD,CL,CR,BAL,XRAN
      TYPE     (CR_STR) CRKONST(P_ZN)

C     V2 (NI) VARIABLES
      
      REAL     PARM(MAXSP,14),CRHAB(14,MAXSP)
      REAL     B7,B8,B9,B10,B11,B12,B13,B14
      INTEGER  MAPHAB(30,MAXSP)
      REAL     PCR,DCR,XCR,BACHLO

C     THESE USE NI-OTHER (11) FOR 12,13,15, AND NI-FD (3) FOR 14
      DATA PARM/
     & 0.0,-0.00204,0.0,-0.00183,3*0.0,-0.00203,-0.00190,-0.002165,
     & -0.00264,
     >    2*-0.00264,0.0,-0.00264,
     & 4*0.0,-0.000001902,6*0.0,
     >    4*0.0,
     & -0.34566,4*0.0,0.17479,5*0.0,
     >    4*0.0,
     & 5*0.0,-0.00183,5*0.0,
     >    4*0.0,
     & 10*0.0,0.000005116,                      
     >    2*0.000005116,0.0,0.000005116,
     & 2*0.0,-0.15334,3*0.0,-0.18555,4*0.0,
     >    2*0.0,-0.15334,0.0,
     & 0.03882,3*0.0,0.03027,-0.0056,5*0.0,
     >    4*0.0,
     & -0.0007,3*0.0,-0.00055,6*0.0,
     >    4*0.0,
     & 0.0,0.30066,0.33840,0.24293,2*0.0,0.53172,0.29699,0.23372,
     & 0.26558,0.0,
     >    2*0.0,0.33840,0.0,
     & 6*0.0,-0.02989,4*0.0,
     >    4*0.0,
     & 6*0.0,0.00011,4*0.0,
     >    4*0.0,
     & -0.21217,-0.59302,-0.59685,-0.25601,-0.25776,2*0.0,-0.38334,
     & -0.28433,-0.31555,-0.25138,
     >    2*-0.25138,-0.59685,-0.25138,
     & 0.00301,5*0.0,0.0042,0.0,0.001903,2*0.0,
     >    4*0.0,
     & 0.0,0.19558,0.16488,0.07260,0.06887,0.1105,0.0,0.09918,0.0,
     & 0.16072,0.05140,
     >    2*0.05140,0.16488,1*0.05140/
     
      DATA ((MAPHAB(I,J),I=1,30), J=1,5)/
     & 12*2,3,3*4,3*5,6,6,1,6,1,7,1,4*6,
     & 7*2,3,2,4,4,5,6,3*7,8,8,4,1,10,9,10,1,11,4*1,2,
     & 3*2,3*4,6,7,4,8,8,5,9,3*10,2*11,8,1,2*12,13,1,3,1,3,3*1,
     & 9*2,1,1,2,3,3*4,5,5,6,1,3*7,1,8,1,7,3*1,
     & 30*1/
      DATA ((MAPHAB(I,J),I=1,30), J=6,10)/
     & 16*1,3*2,11*1,
     & 6*2,4,5,5,2,2,6,7,3*8,9,9,10,2*11,12,11,1,13,1,14,3,3,11,
     & 7*2,3,2,1,1,2,4,3*5,6,6,7,8,8,9,8,10,11,1,1,2*12,8,
     & 13*2,3,4*4,5,6,6,7,6,1,8,1,9,2*10,6,
     & 2,2,4,3*1,5,6,3*1,8,7,3*9,3*3,11*1/
      DATA ((MAPHAB(I,J),I=1,30), J=11,15)/
     & 12*1,4*2,3,3,4,3*1,5,1,6,5*1,  !EP - NI Other
     & 12*1,4*2,3,3,4,3*1,5,1,6,5*1,  !AT 
     & 12*1,4*2,3,3,4,3*1,5,1,6,5*1,  !AC 
     & 7*2,3,2,4,4,5,6,3*7,8,8,4,1,10,9,10,1,11,4*1,2, !OC=FD
     & 12*1,4*2,3,3,4,3*1,5,1,6,5*1/  !OH

       DATA CRHAB/
     & 0.8884, 0.7309, 0.9347, 0.9888, 0.9945, 1.1126, 1.0263, 7*0.0,
     & 0.06533, 0.03441, 0.2307, 0.1661, -0.1253, -0.05018, 0.11005,
     & 0.08113, 0.1782, 0.03919, 0.2107, 3*0.0,
     & 0.8643, 0.7271, 0.9840, 0.8127, 0.8874, 0.7055, 0.7708, 0.7849,
     & 0.8038, 0.8742, 0.8232, 0.8415, 0.9759, 0.0,
     &-0.2304,-0.5421,-0.4343,-0.3759,-0.4129,-0.4879,-0.2674,
     &-0.1941,6*0.0,
     & -0.2413,13*0.0,           !5 
     & -1.6053, -1.7128, 12*0.0, !6
     & -0.3785, -0.4142, -0.3985, -0.2987, -0.3810, -0.4087, -0.3577,
     & -0.2994, -0.2486,-0.2863,-0.1968,-0.4931,-0.2676,-0.5625,
     & 0.05351, -0.05031, 0.1075, -0.1872, 0.01729, 0.03667, 0.01885,
     & 0.09102, 0.1371, 0.08368, 0.1230, -0.02365, 2*0.0,
     & 0.09453, -0.07740, 0.07113, 0.2039, 0.06176, 0.1513, 0.09086,
     & 0.1580, 0.09229, 0.01551, 4*0.0,
     & -0.9436, -0.8654, -0.8849, -0.9067, -0.8783, -1.0103, -1.0268,
     & -1.0050, -1.0301, 5*0.0,
     & 0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0, !EP=NIOth
     & 0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0, !AT
     & 0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0, !AC
     & 0.8643, 0.7271, 0.9840, 0.8127, 0.8874, 0.7055, 0.7708, 0.7849,
     & 0.8038, 0.8742, 0.8232, 0.8415, 0.9759, 0.0, ! OC=FD
     & 0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0/ !OH

C  DATA CRSD/12.69/ (V2 - NI)
C  THIS IS A POPULATION ESTIMATE AND TOO LARGE FOR WITHIN STAND VARIATION.
C  CUT IT IN HALF UNTIL SOMETHING BETTER COMES ALONG.   DIXON  4/19/99
      DATA CRSDX/6.35/

	DATA MYACTS/81/
C
C       ZONE COEFFICIENTS FOR ICH
C
      DATA CRKONST(1) /
     >  CR_STR (
     >    'ICH ',
     >    (/13,12, 9, 6,11, 3, 5, 2, 7, 1, 10, 8,14,15, 0 /),! spp
     >    (/ 146, 854,1277,6402,2233,7625,4185,3843,3932,818, ! obs
     >        65,2258,7625,2233, 0 /),
     >    (/-5.79412, -1.90243, -7.12668, -9.07770, -3.25686, ! intrcpt
     >      -5.61114, -7.49226, -5.29520, -4.64241, -6.50196,
     >      -6.45206, -2.85285, -5.61114, -3.25686, 0.0 /),
     >    (/ 0.51146,  0.0,      0.93059,  1.66517,  0.56474, ! ht/dbh
     >       0.70786,  1.13328,  1.15971,  1.05587,  1.22750,
     >       0.0,      2.75133,  0.70786,  0.56474,  0.0 /),
     >    (/ 0.0,      0.02921,  0.01364, -0.00873,  0.01872, ! ht 
     >       0.04186, -0.01291,  0.00550,  0.03164,  0.01112,
     >       0.0,      0.0,      0.04186,  0.01872,  0.0 /),
     >    (/ 0.0,     -0.00043,  0.00031,  0.00017,  0.0,     ! dbh**2
     >      -0.00017,  0.00030,  0.0,     -0.00021,  0.0,
     >      -0.00043,  0.0,     -0.00017,  0.0,      0.0 /),
     >    (/ 0.0,      0.01200,  0.01943,  0.00904,  0.0,     ! bal
     >       0.01275,  0.00939,  0.0,      0.0,      0.0,
     >       0.0,      0.0,      0.01275,  0.0,      0.0 /),
     >    (/ 1.05450,  0.41115,  0.76411,  1.35115,  0.68275, ! ln(ccf)
     >       0.80422,  1.09699,  0.88346,  0.66560,  0.95538,
     >       0.0,      0.0,      0.80422,  0.68275,  0.0 /),
     >    (/ 0.0,      0.0,      0.18717, -0.04259, -0.21167, ! elev
     >      -0.00873,  0.0,      0.0,      0.12483,  0.0,
     >       2.06051,  0.0,     -0.00873, -0.21167,  0.0 /),
     >    (/ 0.0,      0.0,     -0.00911,  0.0,      0.01092, ! elev**2
     >       0.0,     -0.00482,  0.0,     -0.00819,  0.0,
     >      -0.13668,  0.0,      0.0,      0.01092,  0.0 /),
     >    (/ 0.0,      0.0,      0.0,      0.0,      0.0,     ! slope
     >       0.0,      1.37891,  0.0,     -0.49553,  0.0,
     >       0.0,      0.0,      0.0,      0.0,      0.0 /),
     >    (/ 0.0,      0.0,      0.0,      0.0,      0.0,     ! slope**2
     >       0.0,     -1.65551, -0.52292,  0.0,      0.0,
     >       0.0,      0.0,      0.0,      0.0,      0.0 /),
     >    (/ 0.0,      0.0,      0.0,      0.19629,  0.0,     ! sin-asp
     >      -0.09833,  0.23237,  0.0,      0.14207,  0.0,
     >       0.0,      0.23259, -0.09833,  0.0,      0.0 /),
     >    (/-1.22540,  0.0,      0.42349,  0.35682,  0.11238, ! cos-asp
     >       0.28971,  0.40582, -0.26042,  0.11301,  0.0,
     >       0.0,      0.0,      0.28971,  0.11238,  0.0 /),
     >    (/ 0.14,     0.12,     0.18,     0.17,     0.13,    ! SEE
     >       0.06,     0.17,     0.11,     0.11,     0.16,
     >       0.09,     0.19,     0.06,     0.13,     0.0 /)
     >  ) /
C
C       ZONE COEFFICIENTS FOR IDF
C
      DATA CRKONST(2) /
     >  CR_STR (
     >    'IDF ',
     >    (/13, 9, 6,11, 3, 2, 7,10, 8, 12, 1,14,15, 0, 0 /),  ! spp
     >    (/381,  32, 328, 567,8665,1266,3367, 518, 492,      ! obs
     >      146, 818,8665, 567, 0, 0 /),
     >    (/-1.11300, -0.18008, -5.14375, -4.27986, -6.13692, ! intrcpt
     >      -2.91609, -4.55575, -1.81808, -0.88873, -5.79412,
     >      -6.50196, -6.13692, -4.27986,  0.0, 0.0 /),
     >    (/-0.60066,  0.0,      0.96161,  0.45446,  1.76502, ! ht/dbh
     >       0.63874,  0.72156,  1.50771,  1.08889,  0.51146,
     >       1.22750,  1.76502,  0.45446,  0.0, 0.0 /),
     >    (/ 0.09484, -0.00938, -0.02965,  0.04155,  0.02994, ! ht 
     >       0.02136,  0.06559,  0.01341,  0.04229,  0.0,
     >       0.01112,  0.02994,  0.04155,  0.0, 0.0 /),
     >    (/-0.00168,  0.0,      0.00082, -0.00068, -0.00010, ! dbh**2
     >      -0.00039, -0.00065,  0.0,      0.0,      0.0,
     >       0.0,     -0.00010, -0.00068,  0.0, 0.0 /),
     >    (/ 0.02445,  0.0,      0.02964, -0.01616, -0.00590, ! bal
     >       0.0,      0.00617,  0.0,      0.02837,  0.0,
     >       0.0,     -0.00590, -0.01616,  0.0, 0.0 /),
     >    (/ 0.21742,  0.0,      0.0,      0.93227,  0.83551, ! ln(ccf)
     >       0.68627,  0.49905,  0.49928,  0.0,      1.05450,
     >       0.95538,  0.83551,  0.93227,  0.0, 0.0 /),
     >    (/ 0.0,      0.03312,  0.64904, -0.12703,  0.0,     ! elev
     >      -0.16511,  0.26356, -0.33412, -0.30236,  0.0,
     >       0.0,      0.0,     -0.12703,  0.0, 0.0 /),
     >    (/ 0.0,     -0.00420, -0.04465,  0.00557,  0.0,     ! elev**2
     >       0.00636, -0.01682,  0.01516,  0.01057,  0.0,
     >       0.0,      0.0,      0.00557,  0.0, 0.0 /),
     >    (/ 0.0,      0.0,      0.83931, -1.86690,  0.0,     ! slope
     >       0.0,     -0.44586, -1.77894, -2.37636,  0.0,
     >       0.0,      0.0,     -1.86690,  0.0, 0.0 /),
     >    (/ 0.0,      0.0,      0.0,      2.34112,  0.11122, ! slope**2
     >       0.0,      0.0,      2.98975,  1.63206,  0.0,
     >       0.0,      0.11122,  2.34112,  0.0, 0.0 /),
     >    (/ 0.0,      0.0,     -0.63232,  0.0,     -0.09148, ! sin-asp
     >       0.0,      0.0,      0.0,      0.0,      0.0,
     >       0.0,     -0.09148,  0.0,      0.0, 0.0 /),
     >    (/ 0.0,      0.0,      0.76681,  0.50400, -0.10358, ! cos-asp
     >       0.69217,  0.20520,  0.0,      1.06805, -1.22540,
     >       0.0,     -0.10358,  0.50400,  0.0, 0.0 /),
     >    (/ 0.12,     0.19,     0.13,     0.15,     0.16,    ! SEE
     >       0.12,     0.15,     0.14,     0.16,     0.14,
     >       0.16,     0.16,     0.15,     0.0, 0.0 /)
     >  ) /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'CROWN',5,ICYC)
C----------
C  WRITE DEBUG INFO (STAND DENSITY TERMS) IF DESIRED
C----------
      IF(DEBUG) WRITE(JOSTND,9000) BA, RELDEN, RELDM1
 9000 FORMAT(' IN CROWN, BA=',F7.3,'  RELDEN=',F7.3,'  RELDM1=',F7.3)
C----------
C  DUB CROWNS ON DEAD TREES IF NO LIVE TREES IN INVENTORY
C----------
      IF((ITRN.LE.0).AND.(IREC2.LT.MAXTP1))GO TO 74
C---------
C  IF THERE ARE NO TREE RECORDS, THEN RETURN.
C---------
      IF(ITRN.EQ.0)THEN
        RETURN
      ELSEIF(TPROB.LE.0.0)THEN
        DO I=1,ITRN
        ICR(I)=ABS(ICR(I))
        ENDDO
        RETURN
      ENDIF
C-----------
C  PROCESS CRNMULT KEYWORD.
C-----------
      CALL OPFIND(1,MYACTS,NTODO)
      IF(NTODO .EQ. 0)GO TO 25
      DO 24 I=1,NTODO
      CALL OPGET(I,5,IDATE,IACTK,NP,PRM)
      IDT=IDATE
      CALL OPDONE(I,IDT)
      ISPCC=IFIX(PRM(1))
      IF(DEBUG)WRITE(JOSTND,*)' PRM1= ',PRM
C----------
C  ISPCC<0 CHANGE FOR ALL SPECIES IN THE SPECIES GROUP
C  ISPCC=0 CHANGE FOR ALL SPEICES
C  ISPCC>0 CHANGE THE INDICATED SPECIES
C----------
      IF(ISPCC .LT. 0)THEN
        IGRP = -ISPCC
        IULIM = ISPGRP(IGRP,1)+1
        DO 15 IG=2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        IF(PRM(2) .GE. 0.0)CRNMLT(IGSP)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(IGSP)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(IGSP)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(IGSP)=1
   15   CONTINUE
      ELSEIF(ISPCC .EQ. 0)THEN
        DO 22 ISPCC=1,MAXSP
        IF(PRM(2) .GE. 0.0)CRNMLT(ISPCC)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(ISPCC)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(ISPCC)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(ISPCC)=1
   22   CONTINUE
      ELSE
        IF(PRM(2) .GE. 0.0)CRNMLT(ISPCC)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(ISPCC)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(ISPCC)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(ISPCC)=1
      ENDIF
   24 CONTINUE
   25 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' PRM2= ',PRM
      IF(DEBUG)WRITE(JOSTND,9024)ICYC,CRNMLT
 9024 FORMAT(/' IN CROWN 9024 ICYC,CRNMLT= ',
     & I5/((1X,11F6.2)/))
C----------
C  IF CCF LAST CYCLE WAS LESS THAN 100.0, THERE IS ASSUMED TO BE
C  NO DENSITY IMPACT ON CROWN RATIOS.
C----------
      RDM1=RELDM1
      OBA=OLDBA
      IF(RELDM1.GE.100.0) GO TO 20
      OBA=BA
      RDM1=RELDEN
   20 CONTINUE
      IF(LSTART) GO TO 21
      X1=ALOG(OBA)
      X2=ALOG(RDM1)
   21 CONTINUE
C----------
C  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES
C----------
      DO 70 ISPC=1,MAXSP
        I1 = ISCT(ISPC,1)
        IF(I1 .EQ. 0) GO TO 70
        I2 = ISCT(ISPC,2)
        IF (LV2ATV) THEN
          XCRCON = CRCON(ISPC) + PARM(ISPC,1)*BA + PARM(ISPC,2)*BA*BA +
     &      PARM(ISPC,3)*ALOG(BA) + PARM(ISPC,4)*RELDEN + PARM(ISPC,5)
     &      *RELDEN*RELDEN + PARM(ISPC,6)*ALOG(RELDEN)
        ELSE        
	    XCRCON = CRCON(ISPC) + CRLNCCF(ISPC) * LOG(MAX(0.01,RELDEN))
	  ENDIF
C----------
C  IF DUBBING MISSING CROWN RATIOS, BYPASS ASSIGNMENT OF DCRCON.
C----------
        DCRCON=0.0
        IF(LSTART) GO TO 30
        IF (LV2ATV) THEN
          DCRCON = CRCON(ISPC) + PARM(ISPC,1)*OBA + 
     &      PARM(ISPC,2)*OBA*OBA +          
     &      PARM(ISPC,3)*X1 + PARM(ISPC,4)*RDM1 + PARM(ISPC,5)*RDM1
     &      *RDM1 + PARM(ISPC,6)*X2
        ELSE        
          DCRCON = CRCON(ISPC) + CRLNCCF(ISPC) * LOG(MAX(0.01,RDM1))
	  ENDIF
C----------
C  WRITE DEBUG INFO (CONSTANT TERMS) IF DESIRED
C----------
   30   CONTINUE
        IF(DEBUG) WRITE(JOSTND,9001) ISPC, XCRCON, DCRCON
 9001   FORMAT(' SPECIES: ',I2,' - XCRCON = ',F10.3,', DCRCON = ',F10.3)
C----------
C  VARIABLES B1, B2, ..., B14 ARE SET UP TO ELIMINATE THE
C  2-DIMENSIONAL ARRAY ACCESSES WHEN PROCESSING RECORDS OF THE SAME
C  SPECIES.
C----------
        IF (LV2ATV) THEN
          B7 = PARM(ISPC,7)
          B8 = PARM(ISPC,8)
          B9 = PARM(ISPC,9)
          B10 = PARM(ISPC,10)
          B11 = PARM(ISPC,11)
          B12 = PARM(ISPC,12)
          B13 = PARM(ISPC,13)
          B14 = PARM(ISPC,14)
        ENDIF
C----------
C  ENTER LOOP FOR TREE DEPENDENT VARIABLES
C----------
        DO 60 I3=I1,I2
          I = IND1(I3)
C----------
C  IF THIS IS THE INITIAL ENTRY TO 'CROWN' AND THE TREE IN QUESTION
C  HAS A CROWN RATIO ASCRIBED TO IT, THE WHOLE PROCESS IS BYPASSED.
C----------
          IF(LSTART .AND. ICR(I).GT.0) GO TO 60
C----------
C  IF ICR(I) IS NEGATIVE, CROWN RATIO CHANGE WAS COMPUTED IN A
C  PEST DYNAMICS EXTENSION.  SWITCH THE SIGN ON ICR(I) AND BYPASS
C  CHANGE CALCULATIONS.
C----------
          IF (LSTART) GO TO 40
          IF (ICR(I).GT.0) GO TO 40
          ICR(I)=-ICR(I)
          IF (DEBUG) WRITE (JOSTND,35) I,ICR(I)
   35     FORMAT (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
          GO TO 60
   40     CONTINUE
          D=DBH(I)
          H=HT(I)
          BARK=BRATIO(ISPC,D,H)
C----------
C  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 
C  3IN (V2) OR 2CM (V3)
C----------
          IF (LV2ATV) THEN
            IF(D.LT.3.0) GO TO 58   
          ELSE
            IF((D*INtoCM) .LT. 2.0) GO TO 58
          ENDIF
C----------
C  BYPASS CROWN CHANGE CALCULATION IF CROWN RATIO WAS ASSIGNED
C  THIS CYCLE IN REGENT.
C----------
          IF (.NOT.LSTART .AND. ((DBH(I)-DG(I))/BARK).LT.3.0) GO TO 60
          P=PCT(I)
          BAL = (1.0 - (P/100.)) * BA
C----------
C  CALCULATE THE PREDICTED CURRENT CROWN RATIO
C  COMMENTED OUT: ADD RANDOM ERROR ON FIRST USE ONLY
C----------
          XRAN = 0.0
C
C          IF (ICYC .EQ. 1) XRAN = CRSD(ISPC)
C
          IF (LV2ATV) THEN
            PCR = XCRCON + B7*D + B8*D*D + B9*ALOG(D) + B10*H + 
     &        B11*H*H +B12*ALOG(H) + B13*P + B14*ALOG(P)
            EXPPCR= EXP(PCR)
          ELSE
            CALL CRNMD (ISPC,XCRCON,D,H,BAL,EXPPCR,XRAN)
	    ENDIF
!C----------
C  SET DIFFERENTIAL TO 0.0 AND BRANCH TO 50 IF DUBBING
C----------
          EXPDCR=0.0
          IF(LSTART) GO TO 50
C----------
C  BACKDATE TREE ATTRIBUTES SO THAT CROWN RATIO AT THE BEGINNING
C  OF THE CYCLE CAN BE PREDICTED.
C----------
          D=(D-(DG(I)/BARK))
          IF(D.LE.0.0) D=DBH(I)
          H=(H-HTG(I))
          IF(H.LE.0.0) H=HT(I)
          IF((OLDPCT(I).GT.PCT(I).AND.ONTREM(7).GT.0.0).OR.
     &      OLDPCT(I).LE.0.0) OLDPCT(I)=PCT(I)
          P=OLDPCT(I)
          BAL = (1.0 - (P/100.)) * OLDBA
C----------
C  CALCULATE THE PREDICTED CROWN RATIO AT THE START OF THE CYCLE.
C----------
          IF (LV2ATV) THEN
            DCR = DCRCON + B7*D + B8*D*D + B9*ALOG(D) + B10*H +
     &        B11*H*H + B12*ALOG(H) + B13*P + B14*ALOG(P)
            EXPDCR=EXP(DCR)
          ELSE
	      CALL CRNMD (ISPC,XCRCON,D,H,BAL,EXPDCR,XRAN)
	    ENDIF
C----------
C  WRITE DEBUG INFO IF DESIRED
C----------
   50     CONTINUE
          IF(DEBUG) WRITE(JOSTND,9002) I,ICR(I),EXPPCR,EXPDCR,D,H,
     &      PCT(I),OLDPCT(I)
 9002     FORMAT(' ICR(',I4,')=',I3,'  EXPPCR=',F10.3,' EXPDCR=',F10.3,
     &      ' D=',F7.3,' H=',F7.3,' P=',F7.3,' OLDP=',F7.3)
C----------
C  COMPUTE THE PREDICTED CROWN RATIO AND 
C  BOUND CROWN CHANGE TO 1% PER YEAR
C----------
          CHG=EXPPCR-EXPDCR
          ICRI  = ICR(I) + (EXPPCR-EXPDCR)*100.0+0.5
          IF ((DGSD.LT.1.0).OR.(.NOT.LSTART)) GO TO 51
          XCR=ICRI
          ICRI=BACHLO(XCR,CRSDX,RANN)
   51     CONTINUE

          IF(.NOT.LSTART.OR.(ICR(I).GT.0))THEN
            PDIFPY=CHG/ICR(I)/FINT*100.
            IF(PDIFPY.GT.0.01)CHG=ICR(I)*(0.01)*FINT/100.
            IF(PDIFPY.LT.-0.01)CHG=ICR(I)*(-0.01)*FINT/100.
          ENDIF
C----------
C  APPLY CRNMULT KEYWORD ADJUSTMENTS
C----------
          IF(DBH(I).GE.DLOW(ISPC) .AND. DBH(I).LT.DHI(ISPC))THEN
            ICRI=ICR(I)+CRNMLT(ISPC)*CHG*100.0+0.5
          ELSE
            ICRI=ICR(I)+CHG*100.0+0.5
          ENDIF
C----------
C  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY
C----------
          IF (.NOT.LSTART .OR. ITRUNC(I).EQ.0) GO TO 59
          HN=NORMHT(I)/100.0
          HD=HN-ITRUNC(I)/100.0
          CL=(FLOAT(ICRI)/100.)*HN-HD
          ICRI=IFIX((CL*100./HN)+.5)
          GO TO 59
C----------
C  CROWNS FOR TREES WITH DBH <3IN (V2) OR <2CM (V3) ARE DUBBED HERE.  NO CHANGE
C  IS CALCULATED UNTIL THE TREE ATTAINS THE THRESHOLD DBH
C----------
   58     CONTINUE
          IF(.NOT.LSTART) GO TO 60
	    XCRCON = CRCON(ISPC) + CRLNCCF(ISPC) * LOG(MAX(0.01,RELDEN))
          BAL = (1.0 - (PCT(I)/100.)) * BA
          CALL DUBSCR(LV2ATV,ISPC,XCRCON,D,H,BA,BAL,CR)
          ICRI=CR*100.0+0.5
          IF(DEBUG)WRITE(JOSTND,*)' AFTER DUBSCR I,ISPC,D,H,BA,CR,
     &      ICRI= ',I,ISPC,D,H,BA,CR,ICRI
C---------
C  ADDED FOR CRNMLT ADJUSTMENT OF DUBBED VALUES
C---------
          IF(DBH(I).GE.DLOW(ISPC) .AND. DBH(I).LT.DHI(ISPC))
     &      ICRI = ICRI * CRNMLT(ISPC)
C----------
C  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
C  THE ICR VECTOR.
C----------
   59     CONTINUE
          IF(ICRI .GT. 95) ICRI=95
          IF ((ICRI.LT.5).AND.(CRNMLT(ISPC).EQ.1)) ICRI=5
          ICR(I)= ICRI
   60   CONTINUE
        IF(LSTART .AND. ICFLG(ISPC).EQ.1)THEN
          CRNMLT(ISPC)=1.0
          ICFLG(ISPC)=0
        ENDIF
   70 CONTINUE
   74 CONTINUE
C----------
C  DUB MISSING CROWNS ON CYCLE 0 DEAD TREES.
C----------
      IF(IREC2 .GT. MAXTRE) GO TO 75
      DO 79 I=IREC2,MAXTRE
        IF(ICR(I) .GT. 0) GO TO 79
        ISPC=ISP(I)
        D=DBH(I)
        H=HT(I)
        P=PCT(I)
        BAL = (1.0 - (P/100.)) * OLDBA
        XCRCON = CRCON(ISPC) + CRLNCCF(ISPC) * LOG(MAX(0.01,RELDM1))
        CALL DUBSCR (LV2ATV,ISPC,XCRCON,D,H,BA,BAL,CR)
        ICRI=CR*100.0 + 0.5
        IF(ITRUNC(I).EQ.0) GO TO 78
        HN=NORMHT(I)/100.0
        HD=HN-ITRUNC(I)/100.0
        CL=(FLOAT(ICRI)/100.)*HN-HD
        ICRI=IFIX((CL*100./HN)+.5)
   78   CONTINUE
        IF(ICRI.GT.95)   ICRI=95
        IF(ICRI .LT. 10) ICRI=10
        ICR(I)= ICRI
   79 CONTINUE
C
   75 CONTINUE
      RETURN

      ENTRY CRCONS

C  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS THAT ARE
C  SITE DEPENDENT AND REQUIRE ONE TIME RESOLUTION. 
C
C  V2: ITYPE INDEXES HABITAT TYPE (CARRIED IN /PLOT/ COMMON AREA), CRHAB
C  CONTAINS HABITAT INTERCEPTS BY HABITAT TYPE BY SPECIES, AND MAPCR MAPS
C  HABITAT TYPE ONTO HABITAT CLASS FOR EACH SPECIES.
C
C  V3: LOCATE ZONE; DEFAULTS TO ICH (IZN=1) IF NO MATCH

      DATA CRNMLT /MAXSP*1.0/
      DATA ICFLG  /MAXSP*0/
      DATA DLOW   /MAXSP*0.0/
      DATA DHI    /MAXSP*99.0/

      IF (LV2ATV) THEN
        DO I = 1,MAXSP
          ICRHAB  = MAPHAB(ITYPE,I)
          CRCON(I)= CRHAB(ICRHAB,I)
        ENDDO
      ELSE
        IZN = 0
C           MAP THE SBPS ZONE AND ONE SBS SUBZONE ONTO IDF
C               AND THE REST OF THE SBS ONTO ICH
        IF (INDEX(BEC%PrettyName,'SBSdw2') .GT. 0 .OR. 
     >      INDEX(BEC%Zone,'SBPS') .GT. 0) THEN
            IZN = MRT_IDF
        ELSEIF (INDEX(BEC%Zone,'SBS') .GT. 0) THEN
            IZN = MRT_ICH
        ELSE
            DO I = 1,P_ZN
              IF (INDEX(BEC%Zone,CRKONST(I)%Zone) .GT. 0) THEN
                IZN = I
                GO TO 5
              ENDIF
	      ENDDO
    5       IF (IZN .LT. 1 .OR. IZN .GT. P_ZN) THEN
              IZN = 1
              CALL ERRGRO (.TRUE.,24)
	      ENDIF
        ENDIF

C       ZERO ALL COEFFS THEN ASSIGN SITE EFFECTS TO CRCON AND ALL
C       OTHER COEFFS.

        DO I = 1,MAXSP
	    CRCON(I)   = 0.0
	    CRHTDBH(I) = 0.0
	    CRHT(I)    = 0.0
	    CRDBH2(I)  = 0.0
	    CRBAL(I)   = 0.0
	    CRLNCCF(I) = 0.0
	    CRSD(I)    = 0.0
        ENDDO

        DO I = 1,MAXSP
	    J = CRKONST(IZN)%SEQ(I)
	    IF (J .LT. 1 .OR. J .GT. MAXSP) GO TO 7

C         FOR SOME SPECIES IN THE SBS, WE NEED TO CHANGE THE ZONE
C           FOR THE PARAMETERS TO BE USED.
          OLDZN = IZN
          IF (INDEX(BEC%PrettyName,'SBSdw1') .GT. 0 .AND.
     >         (J .EQ. 9 .OR. J .EQ. 11 .OR. J .EQ. 12)) THEN
              IZN = MRT_IDF 
          ENDIF

          CRCON(J) = CRKONST(IZN)%CON(I)
     >      + (CRKONST(IZN)%EL(I)   * (ELEV*100.*FTtoM))
     >      + (CRKONST(IZN)%EL2(I)  * (ELEV*100.*FTtoM)**2)
     >      + (CRKONST(IZN)%SLP(I)  * SLOPE)
     >      + (CRKONST(IZN)%SLP2(I) * SLOPE**2)
     >      + (CRKONST(IZN)%SASP(I) * SIN(ASPECT) * SLOPE)
     >      + (CRKONST(IZN)%CASP(I) * COS(ASPECT) * SLOPE)
C
C         POPULATE ARRAYS FOR OTHER CONSTANTS
C
          CRHTDBH(J) = CRKONST(IZN)%HTDBH(I) ! ht/dbh
          CRHT(J)    = CRKONST(IZN)%HT(I)    ! ht
          CRDBH2(J)  = CRKONST(IZN)%DBH2(I)  ! dbh**2
          CRBAL(J)   = CRKONST(IZN)%BAL(I)   ! bal
          CRLNCCF(J) = CRKONST(IZN)%LNCCF(I) ! ln(ccf)
          CRSD(J)    = SQRT(CRKONST(IZN)%SEE(I)) ! SD

          IZN = OLDZN
        ENDDO
    7   CONTINUE
      ENDIF

      RETURN
      END

C     PRIVATE SUBROUTINE TO CALCULATE CROWN PROPORTION

      SUBROUTINE CRNMD(IISP, YCON, YD, YH, YBAL, YCR, YSD)
	IMPLICIT NONE

      INCLUDE 'PRGPRM.F77'
	INCLUDE 'VARCOM.F77'
      INCLUDE 'BCPLOT.F77'
      INCLUDE 'METRIC.F77'

      EXTERNAL RANN

      INTEGER IISP 
      REAL    YCON, YH, YD, YBAL, YCR, YSD
      REAL    YDM, YHM, YBALM, YD2, YH2
	REAL    BACHLO

	YDM   = YD   * INtoCM
	YHM   = YH   * FTtoM
	YBALM = YBAL * FT2pACRtoM2pHA

C     ADJUST HT, DBH TO APPROXIMATE A 2 CM TREE WHEN DBH<2

      IF (YDM .LT. 2.0) THEN
	  YD2 = 2.0
        IF (LMHTDUB(IISP)) THEN
          YH2=(EXP(AA(IISP)+BB(IISP)/(YD2+1.0))+1.3) * MtoFT
        ELSE
          YH2=EXP(AA(IISP)+BB(IISP)/((YD2*CMtoIN)+1.0))+4.5
        ENDIF
        IF(YH2 .LT. 4.5) YH2 = 4.5
	  YH2 = YH2 * FTtoM
	ELSE
	  YD2 = YDM
	  YH2 = YHM
	ENDIF

      YCR = YCON +
     >  CRHTDBH(IISP) * (YH2/YD2) +
     >  CRHT(IISP) * YH2 +
     >  CRDBH2(IISP) * (YD2*YD2) +
     >  CRBAL(IISP) * YBALM 

      IF (YSD .GT. 0.0) THEN
	  YCR = YCR + BACHLO(0.0,YSD,RANN)
	ENDIF

C     BOUND YCR TO 1E-4 AND 1E+4 AFTER EXP()

	YCR = MIN(9.21, MAX(-9.21, YCR))
	YCR = 1./(1.0 + EXP(YCR))

	RETURN
      END
