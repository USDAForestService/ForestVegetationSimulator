      SUBROUTINE CROWN
      IMPLICIT NONE
C----------
C  **CROWN--SN   DATE OF LAST REVISION:  03/31/11
C----------
C  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND
C  COMPUTE CROWN RATIO CHANGES FOR TREES THAT ARE GREATER THAN
C  3 INCHES DBH.  THE EQUATION USED PREDICTS CROWN RATIO FROM
C  HABITAT TYPE, BASAL AREA, CROWN COMPETITION FACTOR, DBH, TREE
C  HEIGHT, AND PERCENTILE IN THE BASAL AREA DISTRIBUTION.  WHEN
C  THE EQUATION IS USED TO PREDICT CROWN RATIO CHANGE, VALUES
C  OF THE PREDICTOR VARIABLES FROM THE START OF THE CYCLE ARE USED
C  TO PREDICT OLD CROWN RATIO, VALUES FROM THE END OF THE CYCLE
C  ARE USED TO PREDICT NEW CROWN RATIO, AND THE CHANGE IS
C  COMPUTED BY SUBTRACTION.  THE CHANGE IS APPLIED TO ACTUAL
C  CROWN RATIO.  THIS ROUTINE IS CALLED FROM **CRATET** TO DUB
C  MISSING VALUES, AND BY **TREGRO** TO COMPUTE CHANGE DURING
C  REGULAR CYCLING.  ENTRY **CRCONS** IS CALLED BY **RCON** TO
C  LOAD MODEL CONSTANTS THAT ARE SITE DEPENDENT AND NEED ONLY
C  BE RESOLVED ONCE.  A CALL TO **DUBSCR** IS ISSUED TO DUB
C  CROWN RATIO WHEN DBH IS LESS THAN 3 INCHES.  PROCESSING OF
C  CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY **REGENT**.
C----------
C----------
C  MEAN CROWN RATIO MODEL FUNCTIONS ARE SPECIES DEPENDENT
C----------
C     MCREQN  -- PARAMETERS USED TO CALCULATE MEAN CROWN RATIO (MCR)
C                MCREQN(6,MAXSP)
C                MCREQN(1,MAXSP) = 1.0  HRCL HOERL'S EQUATION
C                MCREQN(1,MAXSP) = 2.0  PWR POWER EQUATION
C                MCREQN(1,MAXSP) = 3.0  LIN LINEAR EQUATION
C                MCREQN(1,MAXSP) = 4.0  LOG LOGARITHMIC EQUATION
C                MCREQN(1,MAXSP) = 5.0  INVERSE OR HYPERBOLIC EQUATION
C
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
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
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
COMMONS
C
      LOGICAL DEBUG
      REAL CRNEW(MAXTRE),WEIBUL(5,MAXSP)
      INTEGER ISORT(MAXTRE)
      REAL CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP)
      INTEGER MYACTS(1)
      REAL PRM(5)
      INTEGER ICFLG(MAXSP)
      REAL MCREQN(6,MAXSP)
      INTEGER JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP
      INTEGER ISPC,I1,I2,I3,ICRI,J,IMCREQ,J1
      REAL RELSDI,AMCR,BMCR,CMCR,ACRNEW,A,B,C,SCALE,X,RNUMB
      REAL CRHAT,PCTHAT,DIFF
      REAL CR,CL,HD,HN,CRMAX,CRLN,PDIFPY,CHG,DEN,D
      DATA MYACTS/81/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'CROWN',5,ICYC)
C
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)
C----------
C INITIALIZE CROWN VARIABLES TO BEGINNING OF CYCLE VALUES.
C----------
      IF(LSTART)THEN
        DO 10 JJ=1,MAXTRE
        CRNEW(JJ)=0.0
        ISORT(JJ)=0
   10   CONTINUE
      ENDIF
C----------
C  DUB CROWNS ON DEAD TREES IF NO LIVE TREES IN INVENTORY
C----------
      IF((ITRN.LE.0).AND.(IREC2.LT.MAXTP1))GO TO 74
C----------
C IF THERE ARE NO TREE RECORDS, THEN RETURN
C----------
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
C----------
C  ISPCC<0 CHANGE FOR ALL SPECIES IN THE SPECIES GROUP
C  ISPCC=0 CHANGE FOR ALL SPEICES
C  ISPCC>0 CHANGE THE INDICATED SPECIES
C----------
      IF(ISPCC .LT. 0)THEN
        IGRP = -ISPCC
        IULIM = ISPGRP(IGRP,1)+1
        DO 21 IG=2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        IF(PRM(2) .GE. 0.0)CRNMLT(IGSP)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(IGSP)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(IGSP)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(IGSP)=1
   21   CONTINUE
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
      IF(DEBUG)WRITE(JOSTND,9024)ICYC,CRNMLT
 9024 FORMAT(/' IN CROWN 9024 ICYC,CRNMLT= ',
     & I5/((1X,11F6.2)/))
C----------
C LOAD ISORT ARRAY WITH DIAMETER DISTRIBUTION RANKS.  IF
C ISORT(K) = 10 THEN TREE NUMBER K IS THE 10TH TREE FROM
C THE BOTTOM IN THE DIAMETER RANKING  (1=SMALL, ITRN=LARGE)
C----------
      DO 11 JJ=1,ITRN
      J1 = ITRN - JJ + 1
      ISORT(IND(JJ)) = J1
   11 CONTINUE
      IF(DEBUG)THEN
        WRITE(JOSTND,7900)ITRN,(IND(JJ),JJ=1,ITRN)
 7900   FORMAT(' IN CROWN 7900 ITRN,IND =',I6,/,86(1H ,32I4,/))
        WRITE(JOSTND,7901)ITRN,(ISORT(JJ),JJ=1,ITRN)
 7901   FORMAT(' IN CROWN 7900 ITRN,ISORT =',I6,/,86(1H ,32I4,/))
      ENDIF
C----------
C  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES
C----------
      DO 70 ISPC=1,MAXSP
      I1 = ISCT(ISPC,1)
      IF(I1 .EQ. 0) GO TO 70
      I2 = ISCT(ISPC,2)
C----------
C ESTIMATE MEAN CROWN RATIO FROM SDI, AND ESTIMATE WEIBULL PARAMETERS
C----------
      IF(SDIDEF(ISPC) .GT. 0.)THEN
        RELSDI = SDIAC / SDIDEF(ISPC)*10.0
      ELSE
        RELSDI = 6.0
      ENDIF
      IF(RELSDI .GT. 12.0)RELSDI = 12.0
      IF(RELSDI .LT. 1.0)RELSDI= 1.0
C----------
C  SELECT THE APPROPRIATE FUNCTION BY SPECIES TO CALCULATE THE
C  MEAN CROWM RATIO (ACRNEW) FROM RSDI
C---------
      IMCREQ= INT(MCREQN(1,ISPC))
      GO TO (100, 200, 300, 400, 500) IMCREQ
C
  100 CONTINUE
C----------
C  HOERL'S EQUATION
C----------
      AMCR= MCREQN(2,ISPC)
      BMCR= MCREQN(4,ISPC)
      CMCR= MCREQN(3,ISPC)
C
      ACRNEW= EXP(AMCR+BMCR*ALOG(RELSDI)+CMCR*RELSDI)
C
      GO TO 900
C
  200 CONTINUE
C----------
C  POWER EQUATION
C----------
      AMCR= MCREQN(2,ISPC)
      BMCR= MCREQN(4,ISPC)
C
      ACRNEW= EXP(AMCR+BMCR*ALOG(RELSDI))
C
      GO TO 900
C
  300 CONTINUE
C----------
C  LINEAR EQUATION
C----------
      AMCR= MCREQN(2,ISPC)
      CMCR= MCREQN(3,ISPC)
C
      ACRNEW= AMCR + CMCR*RELSDI
C
      GO TO 900
C
  400 CONTINUE
C----------
C  LOGARITHMIC EQUATION
C----------
      AMCR= MCREQN(2,ISPC)
      BMCR= MCREQN(5,ISPC)
C
      ACRNEW= AMCR + BMCR*LOG10(RELSDI)
C
      GO TO 900
C
  500 CONTINUE
C----------
C  INVERSE OR HYPERBOLIC EQUATION
C----------
      AMCR= MCREQN(2,ISPC)
      BMCR= MCREQN(6,ISPC)
C
      ACRNEW= RELSDI/(AMCR*RELSDI + BMCR)
C
  900 CONTINUE
C----------
C  EVALUATE WEIBULL COEFFICIENTS
C----------
      A = WEIBUL(1,ISPC)
      B = WEIBUL(2,ISPC) + WEIBUL(3,ISPC)*ACRNEW
      C = WEIBUL(4,ISPC)
      IF(B .LT. 3.0) B=3.0
      IF(C .LT. 2.0) C=2.0
      IF(DEBUG) WRITE(JOSTND,9001) ISPC,SDIAC,ORMSQD,RELSDI,
     & ACRNEW,A,B,C,SDIDEF(ISPC),IMCREQ
 9001 FORMAT(' IN CROWN 9001 ISPC,SDIAC,ORMSQD,RELSDI,ACRNEW,A,B,',
     &'C,SDIDEF,IMCREQ = '/1X,I5,F8.2,F8.4,F8.2,F8.2,4F10.4,I10)
      DO 60 I3=I1,I2
      I = IND1(I3)
C----------
C  IF THIS IS THE INITIAL ENTRY TO 'CROWN' AND THE TREE IN QUESTION
C  HAS A CROWN RATIO ASCRIBED TO IT, THE WHOLE PROCESS IS BYPASSED.
C----------
      IF(LSTART .AND. ICR(I).GT.0)GOTO 60
C----------
C  IF ICR(I) IS NEGATIVE, CROWN RATIO CHANGE WAS COMPUTED IN A
C  PEST DYNAMICS EXTENSION.  SWITCH THE SIGN ON ICR(I) AND BYPASS
C  CHANGE CALCULATIONS.
C----------
      IF (LSTART) GO TO 40
      IF (ICR(I).GE.0) GO TO 40
      ICR(I)=-ICR(I)
      IF (DEBUG) WRITE (JOSTND,35) I,ICR(I)
   35 FORMAT (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
      GOTO 60
   40 CONTINUE
      D=DBH(I)
C----------
C  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 1 IN.
C  THIS WAS TAKEN OUT FOR THE SN VARIANT
C----------
C      IF(D.LT.1.0 .AND. LSTART) GO TO 58
C----------
C  CALCULATE THE PREDICTED CURRENT CROWN RATIO
C----------
      SCALE = (1.0 - .00167 * (RELDEN-100.0))
      IF(SCALE .GT. 1.0) SCALE = 1.0
      IF(SCALE .LT. 0.30) SCALE = 0.30
      IF(DBH(I) .GT. 0.0) THEN
        X = (FLOAT(ISORT(I)) / FLOAT(ITRN)) * SCALE
      ELSE
        CALL RANN(RNUMB)
        X = RNUMB * SCALE
      ENDIF
C
      IF(DEBUG) WRITE(JOSTND,*)' IN CROWN ACRNEW, A,B,C', ACRNEW, A,B,C
C
      IF(X .LT. .05) X=.05
      IF(X .GT. .95) X=.95
      CRNEW(I) = A + B*((-1.0*ALOG(1-X))**(1.0/C))
C----------
C  WRITE DEBUG INFO IF DESIRED
C----------
   50 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9002) I,X,CRNEW(I),ICR(I)
 9002 FORMAT(' IN CROWN 9002 WRITE I,X,CRNEW,ICR = ',I5,2F10.5,I5)
C----------
C  COMPUTE THE CHANGE IN CROWN RATIO
C  CALC THE DIFFERENCE BETWEEN THE MODEL AND THE OLD(OBS)
C  LIMIT CHANGE TO 1% PER YEAR
C----------
      IF(LSTART .OR. ICR(I).EQ.0) GO TO 9052
      CHG=CRNEW(I) - ICR(I)
      PDIFPY=CHG/ICR(I)/FINT
      IF(PDIFPY.GT.0.01)CHG=ICR(I)*(0.01)*FINT
      IF(PDIFPY.LT.-0.01)CHG=ICR(I)*(-0.01)*FINT
      IF(DEBUG)WRITE(JOSTND,9020)I,CRNEW(I),ICR(I),PDIFPY,CHG
 9020 FORMAT(/'  IN CROWN 9020 I,CRNEW,ICR,PDIFPY,CHG =',
     &I5,F10.3,I5,3F10.3)
      IF(DBH(I).GE.DLOW(ISPC) .AND. DBH(I).LE.DHI(ISPC))THEN
        CRNEW(I) = ICR(I) + CHG * CRNMLT(ISPC)
      ELSE
        CRNEW(I) = ICR(I) + CHG
      ENDIF
 9052 ICRI = CRNEW(I)+0.5
      IF(LSTART .OR. ICR(I).EQ.0)THEN
        IF(DBH(I).GE.DLOW(ISPC) .AND. DBH(I).LE.DHI(ISPC))THEN
          ICRI = ICRI * CRNMLT(ISPC)
        ENDIF
      ENDIF
C----------
C CALC CROWN LENGTH NOW
C----------
      IF(LSTART .OR. ICR(I).EQ.0)GO TO 55
      CRLN=HT(I)*ICR(I)/100.
C----------
C CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN
C----------
      CRMAX=(CRLN+HTG(I))/(HT(I)+HTG(I))*100.0
      IF(DEBUG)WRITE(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I),
     & CHG
 9004 FORMAT(' CRMAX=',F10.2,' CRLN=',F10.2,
     &       ' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)
C----------
C IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE
C----------
      IF(ICRI.GT.CRMAX) ICRI=CRMAX+0.5
      IF(ICRI.LT.10 .AND. CRNMLT(ISPC).EQ.1.0)ICRI=CRMAX+0.5
C----------
C  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY
C----------
   55 IF (.NOT.LSTART .OR. ITRUNC(I).EQ.0) GO TO 59
      HN=NORMHT(I)/100.0
      HD=HN-ITRUNC(I)/100.0
      CL=(FLOAT(ICRI)/100.)*HN-HD
      ICRI=IFIX((CL*100./HN)+.5)
      IF(DEBUG)WRITE(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD,
     & ICRI,CL
 9030 FORMAT(' IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ',
     & 3I5,2F10.3,I5,F10.3)
C----------
C  CROWNS FOR TREES WITH DBH LT 3.0 IN ARE DUBBED HERE.  NO CHANGE
C  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 3 INCHES.
C
C  THIS WAS TAKEN OUT FOR THE SN VARIANT
C----------
   59 CONTINUE
      IF(ICRI.GT.95) ICRI=95
      IF (ICRI .LT. 10 .AND. CRNMLT(ISPC).EQ.1) ICRI=10
      IF(ICRI.LT.1)ICRI=1
      ICR(I)= ICRI
   60 CONTINUE
      IF(LSTART .AND. ICFLG(ISPC).EQ.1)THEN
        CRNMLT(ISPC)=1.0
        ICFLG(ISPC)=0
      ENDIF
   70 CONTINUE
   74 CONTINUE
C----------
C  DUB MISSING CROWNS ON CYCLE0.00DEAD TREES.
C----------
      IF(IREC2 .GT. MAXTRE) GO TO 80
      DO 79 I=IREC2,MAXTRE
      IF(ICR(I) .GT. 0) GO TO 79
      ISPC=ISP(I)
      D=DBH(I)
      CALL DUBSCR (D,CR)
      ICRI=CR*100.0 + 0.5
      IF(ITRUNC(I).EQ.0) GO TO 78
      HN=NORMHT(I)/100.0
      HD=HN-ITRUNC(I)/100.0
      CL=(FLOAT(ICRI)/100.)*HN-HD
      ICRI=IFIX((CL*100./HN)+.5)
   78 CONTINUE
      IF(ICRI.GT.95) ICRI=95
      IF (ICRI .LT. 10) ICRI=10
      ICR(I)= ICRI
   79 CONTINUE
C
   80 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9010)ITRN,(ICR(JJ),JJ=1,ITRN)
 9010 FORMAT(' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/,
     & 43(1H ,32I4,/))
      IF(DEBUG)WRITE(JOSTND,90)ICYC
   90 FORMAT(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
      RETURN
      ENTRY CRCONS
C----------
C  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS
C----------
C     MCREQN  -- PARAMETERS USED TO CALCULATE MEAN CROWN RATIO (MCR)
C                MCREQN(MAXSP,1) = 1.0  HRCL HOERL'S EQUATION
C                MCREQN(MAXSP,1) = 2.0  PWR POWER EQUATION
C                MCREQN(MAXSP,1) = 3.0  LIN LINEAR EQUATION
C                MCREQN(MAXSP,1) = 4.0  LOG LOGARITHMIC EQUATION
C                MCREQN(MAXSP,1) = 5.0  INVERSE OR HYPERBOLIC EQUATION
C                MCREQN(MAXSP,2) = a COEFFICIENT
C                MCREQN(MAXSP,3) = c COEFFICIENT
C                MCREQN(MAXSP,4) = b COEFFICIENT
C                MCREQN(MAXSP,5) = b COEFFICIENT IN LOG FUNCTION
C                MCREQN(MAXSP,6) = b COEFICIENT IN INVERSE FUNCTION
C
      DATA ((MCREQN(I,J), I= 1,6),J= 1,15) /
     & 3.0,   63.51    , -0.09    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   67.64    , -2.25    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   63.51    , -0.09    ,  0.00    ,    0.00     ,   0.00,
     & 4.0,   54.0462  ,  0.00    ,  0.00    ,  -18.2118   ,   0.00,
     & 4.0,   47.7297  ,  0.00    ,  0.00    ,  -16.352    ,   0.00,
     & 4.0,   42.8255  ,  0.00    ,  0.00    ,  -15.0135   ,   0.00,
     & 2.0,   4.17     ,  0.00    , -0.23    ,    0.00     ,   0.00,
     & 4.0,   42.84    ,  0.00    ,  0.00    ,   -5.62     ,   0.00,
     & 4.0,   45.8231  ,  0.00    ,  0.00    ,  -13.8999   ,   0.00,
     & 1.0,   4.3546   ,  0.0163  , -0.5034  ,    0.00     ,   0.00,
     & 1.0,   3.8904   ,  0.0478  , -0.3565  ,    0.00     ,   0.00,
     & 3.0,   51.8     , -0.8     ,  0.00    ,    0.00     ,   0.00,
     & 1.0,   3.8284   ,  0.0172  , -0.2234  ,    0.00     ,   0.00,
     & 1.0,   4.1136   ,  0.007   , -0.331   ,    0.00     ,   0.00,
     & 4.0,   48.2413  ,  0.00    ,  0.00    ,  -10.1014   ,   0.00/
      DATA ((MCREQN(I,J), I= 1,6),J= 16,30) /
     & 4.0,   36.0855  ,  0.00    ,  0.00    ,   -5.4737   ,   0.00,
     & 3.0,   63.51    , -0.09    ,  0.00    ,    0.00     ,   0.00,
     & 4.0,   53.1867  ,  0.00    ,  0.00    ,   -9.4122   ,   0.00,
     & 4.0,   61.9643  ,  0.00    ,  0.00    ,   -22.3363  ,   0.00,
     & 4.0,   46.1653  ,  0.00    ,  0.00    ,   -6.088    ,   0.00,
     & 3.0,   42.98    ,  0.55    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   48.2     , -0.01    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   42.13    , -0.1     ,  0.00    ,    0.00     ,   0.00,
     & 1.0,   3.7275   ,  0.0282  , -0.1124  ,    0.00     ,   0.00,
     & 1.0,   3.8785   ,  0.0171  , -0.1749  ,    0.00     ,   0.00,
     & 1.0,   3.9904   ,  0.0171  , -0.1496  ,    0.00     ,   0.00,
     & 1.0,   3.9939   ,  0.0238  , -0.2117  ,    0.00     ,   0.00,
     & 4.0,   48.03    ,  0.00    ,  0.00    ,  -13.21     ,   0.00,
     & 4.0,   50.8266  ,  0.00    ,  0.00    ,  -14.5261   ,   0.00,
     & 4.0,   44.5839  ,  0.00    ,  0.00    ,  -14.0874   ,   0.00/
      DATA ((MCREQN(I,J), I= 1,6),J= 31,45) /
     & 4.0,   51.8467  ,  0.00    ,  0.00    ,  -14.1876   ,   0.00,
     & 1.0,   3.8415   ,  0.0297  , -0.2879  ,    0.00     ,   0.00,
     & 4.0,   59.09    ,  0.00    ,  0.00    ,  -4.99      ,   0.00,
     & 3.0,   38.26    , -0.77    ,  0.00    ,    0.00     ,   0.00,
     & 1.0,   3.7881   , -0.0055  , -0.0634  ,    0.00     ,   0.00,
     & 3.0,   35.49    ,  0.00    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   35.49    ,  0.00    ,  0.00    ,    0.00     ,   0.00,
     & 2.0,   3.82     ,  0.00    , -0.1     ,    0.00     ,   0.00,
     & 3.0,   37.83    , -0.15    ,  0.00    ,    0.00     ,   0.00,
     & 1.0,   4.4653   ,  0.107   , -0.834   ,    0.00     ,   0.00,
     & 3.0,   52.05    , -0.11    ,  0.00    ,    0.00     ,   0.00,
     & 2.0,   3.91     ,  0.00    , -0.12    ,    0.00     ,   0.00,
     & 2.0,   3.91     ,  0.00    , -0.12    ,    0.00     ,   0.00,
     & 1.0,   3.8153   ,  0.0055  , -0.0964  ,    0.00     ,   0.00,
     & 2.0,   3.87     ,  0.00    , -0.07    ,    0.00     ,   0.00/
      DATA ((MCREQN(I,J), I= 1,6),J= 46,60) /
     & 3.0,   44.71    ,  0.4     ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   42.15    , -0.11    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   44.71    ,  0.4     ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   36.5     , -0.23    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   44.71    ,  0.4     ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   55.48    , -2.38    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   42.32    , -1.08    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   36.02    , -0.3     ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   41.01    , -0.21    ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   41.379   , -0.8012  ,  0.00    ,    0.00     ,   0.00,
     & 4.0,   52.7207  ,  0.00    ,  0.00    ,  -11.484    ,   0.00,
     & 3.0,   38.71    , -0.1     ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   38.03    , -0.09    ,  0.00    ,    0.00     ,   0.00,
     & 1.0,   3.9839   , -0.0248  , -0.0462  ,    0.00     ,   0.00,
     & 4.0,   48.03    ,  0.00    ,  0.00    ,  -13.21     ,   0.00/
      DATA ((MCREQN(I,J), I= 1,6),J= 61,75) /
     & 4.0,   48.03    ,  0.00    ,  0.00    ,  -13.21     ,   0.00,
     & 3.0,   45.06    , -0.96    ,  0.00    ,    0.00     ,   0.00,
     & 2.0,   4.05     ,  0.00    , -0.12    ,    0.00     ,   0.00,
     & 4.0,   51.7     ,  0.00    ,  0.00    ,   -9.65     ,   0.00,
     & 2.0,   3.92     ,  0.00    , -0.09    ,    0.00     ,   0.00,
     & 1.0,   3.9112   ,  0.0147  , -0.1697  ,    0.00     ,   0.00,
     & 2.0,   3.95     ,  0.00    , -0.02    ,    0.00     ,   0.00,
     & 4.0,   54.36    ,  0.00    ,  0.00    ,  -11.3181   ,   0.00,
     & 4.0,   57.82    ,  0.00    ,  0.00    ,  -18.45     ,   0.00,
     & 4.0,   56.42    ,  0.00    ,  0.00    ,  -14.13     ,   0.00,
     & 1.0,   3.9344   ,  0.0043  , -0.0845  ,    0.00     ,   0.00,
     & 1.0,   4.1233   , -0.0142  , -0.1279  ,    0.00     ,   0.00,
     & 1.0,   3.9116   ,  0.0509  , -0.2657  ,    0.00     ,   0.00,
     & 4.0,   54.53    ,  0.00    ,  0.00    ,  -14.7      ,   0.00,
     & 2.0,   3.9      ,  0.00    , -0.07    ,    0.00     ,   0.00/
      DATA ((MCREQN(I,J), I= 1,6),J= 76,90) /
     & 3.0,   46.72    , -0.85    ,  0.00    ,    0.00     ,   0.00,
     & 4.0,   44.34    ,  0.00    ,  0.00    ,   -5.23     ,   0.00,
     & 2.0,   4.17     ,  0.00    , -0.18    ,    0.00     ,   0.00,
     & 3.0,   49.27    , -0.72    ,  0.00    ,    0.00     ,   0.00,
     & 4.0,   49.022   ,  0.00    ,  0.00    ,  -22.5732   ,   0.00,
     & 3.0,   44.5295  , -1.0053  ,  0.00    ,    0.00     ,   0.00,
     & 3.0,   38.85    , -0.99    ,  0.00    ,    0.00     ,   0.00,
     & 5.0,   0.0283   ,  0.00    ,  0.00    ,    0.00     , -0.012,
     & 2.0,   3.68     ,  0.00    , -0.02    ,    0.00     ,   0.00,
     & 4.0,   43.64    ,  0.00    ,  0.00    ,  -10.03     ,   0.00,
     & 1.0,   3.7366   ,  0.0151  , -0.0896  ,    0.00     ,   0.00,
     & 1.0,   3.8487   ,  0.0276  , -0.2005  ,    0.00     ,   0.00,
     & 3.0,   67.64    , -2.25    ,  0.00    ,    0.00     ,   0.00,
     & 1.0,   3.78     , -0.02    , -0.02    ,    0.00     ,   0.00,
     & 2.0,   3.93     ,  0.00    , -0.15    ,    0.00     ,   0.00/
C----------
C  WIEBULL FUNCTION COEFFICIENTS
C  WEIBUL  -  ARRAY CONTAINING COEFFIECIENTS
C  WEIBUL(1,J) -- WEIBULL A COEFFICIENT
C  WEIBUL(2,J) -- B0 IN REGRESSION FOR WEIBUL B COEFF. B= B0+B1*MCR
C  WEIBUL(3,J) -- B1 IN REGRESSION FOR WEIBUL B COEFF. B= B0+B1*MCR
C  WEIBUL(4,J) -- WEIBULL C COEFFICIENT
C----------
C
      DATA ((WEIBUL(I,J),I= 1,4),J= 1,15)/
     &  4.0659, -6.8708    ,   1.0510, 4.1741,
     &  2.4435, -32.4837   ,   1.6503, 2.6518,
     &  4.0659, -6.8708    ,   1.0510, 4.1741,
     &  4.3780, -5.0254    ,   0.9620, 2.4758,
     &  4.6721, -3.9456    ,   1.0509, 3.0228,
     &  3.8940, -4.7342    ,   0.9786, 2.9082,
     &  5.0000, -10.1125   ,   1.0734, 3.3218,
     &  3.9771, 14.3941    ,   0.5189, 3.7531,
     &  3.9190, 1.2933     ,   0.7986, 2.9202,
     &  3.9190, 1.2933     ,   0.7986, 2.9202,
     &  4.3300, -34.2606   ,   1.7823, 3.0554,
     &  4.6496, -11.4277   ,   1.1343, 2.9405,
     &  4.9701, -14.6680   ,   1.3196, 2.8517,
     &  5.0000, -10.2832   ,   1.1019, 2.4693,
     &  5.0000, -9.8322    ,   1.1062, 2.8512/
      DATA ((WEIBUL(I,J),I= 1,4),J= 16,30)/
     &  4.9986, -9.6939    ,   1.0740, 2.3667,
     &  4.0659, -6.8708    ,   1.0510, 4.1741,
     &  5.0000, -18.6340   ,   1.2622, 3.6407,
     &  5.0000, -18.6340   ,   1.2622, 3.6407,
     &  4.7322, -24.2740   ,   1.4587, 2.9951,
     &  5.0000, -18.6340   ,   1.2622, 3.6407,
     &  4.6903, -19.5613   ,   1.2928, 3.3715,
     &  5.0000, -18.6340   ,   1.2622, 3.6407,
     &  4.1939, 1.2500     ,   0.8795, 3.1500,
     &  4.1939, 1.2500     ,   0.8795, 3.1500,
     &  4.5640, 0.9693     ,   0.9093, 3.0540,
     &  5.0000, -29.1096   ,   1.5626, 3.5310,
     &  4.8371, -14.3180   ,   1.2060, 3.7345,
     &  4.5671, -49.1736   ,   2.1311, 2.9883,
     &  5.0000, 15.0407    ,   0.6546, 3.0344/
      DATA ((WEIBUL(I,J),I= 1,4),J= 31,45)/
     &  4.7093, -9.6999    ,   1.1020, 2.7391,
     &  4.7093, -9.6999    ,   1.1020, 2.7391,
     &  4.6965, -14.3809   ,   1.2016, 3.5571,
     &  4.0098, -12.7054   ,   1.2224, 2.7400,
     &  4.8776, -11.6617   ,   1.1668, 3.8475,
     &  4.0098, -12.7054   ,   1.2224, 2.7400,
     &  4.5987, -16.9647   ,   1.3925, 3.3601,
     &  4.9245, -13.3135   ,   1.2765, 2.8455,
     &  4.1992, -16.8789   ,   1.2949, 2.7697,
     &  4.7093, -9.6999    ,   1.1020, 2.7391,
     &  4.6965, -14.3809   ,   1.2016, 3.5571,
     &  4.2967, -17.7977   ,   1.3186, 3.0386,
     &  4.2967, -17.7977   ,   1.3186, 3.0386,
     &  4.6350, -39.7348   ,   1.9132, 3.0574,
     &  4.9948, -11.1090   ,   1.1089, 3.8822/
      DATA ((WEIBUL(I,J),I= 1,4),J= 46,60)/
     &  5.0000, 9.2520     ,   0.7899, 3.2166,
     &  4.9829, -5.2479    ,   0.9552, 3.8219,
     &  5.0000, 9.2520     ,   0.7899, 3.2166,
     &  4.2299, -32.4970   ,   1.7316, 2.7902,
     &  5.0000, 9.2520     ,   0.7899, 3.2166,
     &  4.2932, -7.1512    ,   1.0504, 2.7738,
     &  4.8677, -22.5591   ,   1.4240, 2.8686,
     &  5.0000, -15.1643   ,   1.2524, 3.1645,
     &  4.6134, -42.6970   ,   1.9983, 3.0081,
     &  4.8257, -7.1092    ,   1.0128, 2.7232,
     &  5.0000, 15.0407    ,   0.6546, 3.0344,
     &  4.8677, -22.5591   ,   1.4240, 2.8686,
     &  3.5122, 22.2798    ,   0.3081, 2.7868,
     &  4.5640, -30.7592   ,   1.6192, 3.2836,
     &  4.8371, -14.3180   ,   1.2060, 3.7345/
      DATA ((WEIBUL(I,J),I= 1,4),J= 61,75)/
     &  4.8371, -14.3180   ,   1.2060, 3.7345,
     &  4.2932, -7.1512    ,   1.0504, 2.7738,
     &  5.0000, -16.0927   ,   1.2319, 3.5016,
     &  5.0000, -4.6551    ,   0.9593, 3.8340,
     &  5.0000, -26.7842   ,   1.6030, 3.5160,
     &  5.0000, -4.2993    ,   1.0761, 3.5922,
     &  4.1406, 13.6950    ,   0.6895, 3.0427,
     &  4.6329, -1.2977    ,   0.9438, 3.2263,
     &  5.0000, 11.2401    ,   0.7081, 3.5258,
     &  4.1406, 13.6950    ,   0.6895, 3.0427,
     &  4.4764, -18.7445   ,   1.3539, 3.8384,
     &  5.0000, -7.5332    ,   1.0257, 3.1662,
     &  5.0000, -50.1177   ,   2.1127, 3.5148,
     &  5.0000, -9.7922    ,   1.0728, 3.6340,
     &  5.0000, -12.4107   ,   1.1363, 3.6430/
      DATA ((WEIBUL(I,J),I= 1,4),J= 76,90)/
     &  5.0000, 5.0414     ,   0.8032, 3.6764,
     &  4.7585, -83.4596   ,   3.0817, 3.4788,
     &  5.0000, -6.5883    ,   1.0266, 3.5587,
     &  5.0000, 11.2401    ,   0.7081, 3.5258,
     &  3.5643, -10.5101   ,   1.2176, 2.2033,
     &  4.8547, -17.1135   ,   1.3108, 3.2431,
     &  4.9082, -11.2413   ,   1.1519, 2.4971,
     &  4.2656, -26.6773   ,   1.5580, 4.4024,
     &  5.0000, 1.1421     ,   0.9141, 3.0621,
     &  4.9367, 7.6678     ,   0.9105, 3.0303,
     &  5.0000, 1.1421     ,   0.9141, 3.0621,
     &  4.7375, -21.8810   ,   1.5340, 3.3558,
     &  2.4435, -32.4837   ,   1.6503, 2.6518,
     &  4.1374, 17.2956    ,   0.4987, 2.2670,
     &  4.9041, -2.5097    ,   0.9225, 2.7628/
C
      DATA CRNMLT/MAXSP*1.0/
      DATA ICFLG/MAXSP*0/
      DATA DLOW/MAXSP*0.0/
      DATA DHI/MAXSP*99.0/
      RETURN
      END
