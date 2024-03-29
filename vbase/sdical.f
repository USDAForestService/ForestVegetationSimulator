      SUBROUTINE SDICAL (IWHO,XMAX)
      IMPLICIT NONE
C----------
C VBASE $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE MAXIMUM SDI IN EFFECT FOR A STAND.
C  MAXIMUM SDI IS A WEIGHTED AVERAGE OF THE SDI MAXIMUMS BY SPECIES
C  CONTAINED IN THE SDIDEF() ARRAY.  THE WEIGHT IS THE PROPORTION
C  OF TOTAL STAND BASAL AREA REPRESENTED BY A SPECIES.
C
C  IT ALSO ADJUSTS THE BAMAX VALUE TO MATCH THE MAXIMUM SDI VALUE IF
C  A USER DEFINED BA MAXIMUM IS NOT IN EFFECT.
C
C  THIS ROUTINE IS CALLED FROM:
C    **DGF** IN THE CR, EM, IE, TT, UT, AND WS VARIANTS.
C    **DISPLY**
C    **GRINCR** TO SET VALUES FOR THE EVENT MONITOR VARIABLES "BSDIMAX" 
C               AND "ASDIMAX".
C    **MORTS** IN VARIANTS WHICH USE SDI BASED MORTALITY ALGORITHMS.
C    **SDICHK**
C    **CLAUESTB** (CLIMATE EXTENSION)
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
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C----------
C  DEFINITIONS:
C    BAXSP -- BASAL AREA BY SPECIES
C   BAXPSP -- BASAL AREA BY SPECIES BY POINT (SPECIES, POINT)
C      DBH -- TREE DIAMETER.
C     IWHO -- 1 IF RECENT DEAD TREES NEED TO BE USED IN THE CALCULATION;
C             SUBROUTINE **DGF**, CR, TT & UT VARIANTS, DURING CALIBRATION
C             TO GET THE STAGNATION MULTIPLIER SET CORRECTLY.
C             2 IF RECENT DEAD TREES SHOULD NOT BE USED IN THE CALCULATION
C    LINCL -- USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
C             THE CALCULATION OR NOT.
C     PROB -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C   TREEBA -- BA REPRESENTED BY A TREE RECORD
C    TOTBA -- TOTAL STAND BA
C     XMAX -- MAXIMUM SDI WEIGHTED BY BA PROPORTION FOR THE STAND
C   XMAXPT -- MAXIMUM SDI WEIGHTED BY BA PROPORTION FOR EACH POINT
C    MAPNE -- INDICATES GROUP THAT A SPECIES BELONGS TO FOR THE
C             SILVAH-TYPE DENSITY CALCULATION
C----------
      INTEGER ISPC,JSPEC,I,IWHO,IGRP,IULIM,IG,JPNUM,MAPNE(108),IEQN,IT
      INTEGER II,ISLFTM(108)
      INTEGER NPNT
      REAL BAXSP(MAXSP),XMAX,TOTBA,TREEBA,DLO,DHI,SDIC,SPROB,TREERD
      REAL BAXPSP(MAXSP,MAXPLT),PNTBA
      REAL A,B,CCCL,CRA,CWDI,SDSQ,TPACRE,SDIC2
      REAL CRD,D2,TEM,DPROB
      REAL BA1,BA2,BA3,BA4,BA5,BATOT,RATIO1,RATIO2,RATIO3,RATIO5
      REAL CMPNT1,CMPNT2,CMPNT3,CMPNT4,CMPNT5,CMPNT6,
     &     TPAFAC,DIAMFAC,CLSD2,CLSTPA
      LOGICAL DEBUG,LINCL
C
      DATA MAPNE /
     &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     &2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
     &2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3,
     &3, 3, 3, 3, 3, 3, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     &2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
     &2, 2, 2, 2, 2, 2, 2, 2/
C
      DATA ISLFTM /
C     1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0
C
     &0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
     &0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 1,
     &0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2,
     &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
     &0, 0, 0, 0, 0, 0, 0, 0/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'SDICAL',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,10)ICYC, ITRN, IREC1, IREC2
   10 FORMAT(' ENTERING SUBROUTINE SDICAL CYCLE =',I4,
     &' ITRN=',I4,' IREC1=',I4,' IREC2=',I4)
C----------
C DETERMINE BA BY SPECIES, AND TOTAL STAND BA
C IF THIS PASS THROUGH **SDICAL** IS DURING CALIBRATION (LSTART = .TRUE.)
C INCLUDE RECENT MORTALITY TREES (IMC(I)=7) IN THE CALCULATION.
C----------
      XMAX = 0.0
      TOTBA = 0.0
      DO 20 I=1,MAXSP
      BAXSP(I)=0.0
   20 CONTINUE

C     SET NUMBER OF INVENTORY POINTS IN DATA AND INITIALIZE ARRAY
C     TO STORE BA BY SPECIES AND POINT

      NPNT = INT(PI)
      DO I=1,MAXSP
        DO II=1,(NPNT+10)   ! initialize for up to 10 unregistered plots
          BAXPSP(I,II) = 0.0
        END DO
      END DO

      IF(ITRN .LE. 0) GO TO 55
      DO 30 II=1,ITRN
        I=IND1(II)
        ISPC = ISP(I)
        IF(I.GE.IREC2) GO TO 30
        TREEBA = 0.0054542*DBH(I)*DBH(I)*PROB(I)
        BAXSP(ISPC)= BAXSP(ISPC) + TREEBA
        TOTBA = TOTBA + TREEBA
        BAXPSP(ISPC,ITRE(I)) = BAXPSP(ISPC,ITRE(I)) + TREEBA
        IF(DEBUG)WRITE(JOSTND,*)' SDICAL II,I,ISPC,DBH,PROB,TREEBA,',
     &  'BAXSP,TOTBA= ',II,I,ISPC,DBH(I),PROB(I),TREEBA,BAXSP(ISPC),
     &  TOTBA 
   30 CONTINUE
C----------
C  CR, TT, AND UT VARIANTS: DGF DURING CALIBRATION, INCLUDE RECENT
C  MORTALITY TREES
C----------
      IF(DEBUG)WRITE(JOSTND,*)' IWHO,LSTART,IREC2= ',IWHO,LSTART,IREC2
      IF((IWHO .EQ. 1) .AND. LSTART) THEN
        DO II = IREC2,MAXTRE
        ISPC = ISP(II)
        DPROB = PROB(II)/(FINT/FINTM)
        IF(DEBUG)WRITE(JOSTND,*)' II,ISPC,IMC,DBH,DPROB= ',
     &  II,ISPC,IMC(II),DBH(II),DPROB
        IF(IMC(II) .EQ. 7) THEN
          TREEBA = 0.0054542*DBH(II)*DBH(II)*DPROB
          BAXSP(ISPC)= BAXSP(ISPC) + TREEBA
          TOTBA = TOTBA + TREEBA
          IF(DEBUG)WRITE(JOSTND,*)' SDICAL II,ISPC,DBH,DPROB,TREEBA,',
     &    'BAXSP,TOTBA= ',II,ISPC,DBH(II),DPROB,TREEBA,BAXSP(ISPC),
     &    TOTBA 
          ENDIF
        ENDDO
      ENDIF
C----------
C  DEBUG OUTPUT
C----------
      IF(DEBUG)THEN
        WRITE(JOSTND,40)BAXSP
   40   FORMAT(' BAXSP(1-MAXSP) =',10(11F10.4,/' ',16X))
        WRITE(JOSTND,50)TOTBA
   50   FORMAT(' TOTAL BA = ',F10.4)
      ENDIF
C----------
C COMPUTE MAXIMUM SDI WEIGHTED BY SPECIES BA PROPORTION FOR THE STAND
C LACK OF STAND BA WILL RESULT IN MAXIMUM SDI (XMAX) GETTING SET TO 1.0
C WHICH WILL THEN BE USED TO SET THE POINT MAX SDI (XMAXPT) ARRAY.
C----------
   55 IF(TOTBA .LE. 0.)THEN
        XMAX=1.0
      ELSE
       DO 60 I=1,MAXSP
        XMAX = XMAX + SDIDEF(I) * BAXSP(I)
   60   CONTINUE
        XMAX = XMAX / TOTBA
      ENDIF

C----------
C COMPUTE MAXIMUM SDI WEIGHTED BY SPECIES BA PROPORTION FOR EACH POINT
C----------
C
C     COMPUTE THESE FOR UP TO 10 ADDITIONAL UNREGISTERED PLOTS, THAT IS
C     WHERE NUMBER OF PLOTS SPECIFIED ON THE SAMPLE DESIGN IS LESS THAN
C     THE NUMBER OF PLOT ID'S FOUND IN THE TREE RECORDS.
C
      DO 65 II=1,(NPNT+10)
C      DO 65 II=1,NPNT
        XMAXPT(II) = 0.0
        PNTBA     = 0.0
        DO 64 I=1,MAXSP
          XMAXPT(II) = XMAXPT(II) + SDIDEF(I) * BAXPSP(I,II)
          PNTBA = PNTBA + BAXPSP(I,II)
   64   CONTINUE
        IF(PNTBA .EQ. 0.0) THEN
          XMAXPT(II) = XMAX
        ELSE
          XMAXPT(II) = XMAXPT(II) / PNTBA
        ENDIF
        IF(DEBUG)WRITE(JOSTND,*)' SDICAL - NPNT, II, PNTBA, XMAXPT =',
     &    NPNT,II,PNTBA,XMAXPT(II)
   65 CONTINUE

C----------
C ADJUST MAXIMUM BA IF A USER-DEFINED VALUE IS NOT IN EFFECT.
C----------
      IF(.NOT.LBAMAX) THEN
        BAMAX = XMAX * 0.5454154 * PMSDIU
      ELSE
        TEM=PMSDIU
        IF(TEM .GT. 1.0) TEM=TEM/100.
        XMAX = BAMAX/(0.5454154*TEM)
      ENDIF

      IF(DEBUG)WRITE(JOSTND,*)' CALLING CLMAXDEN,',
     > ' CURRENT SDI MAXIMUM = ',XMAX
C
C     FURTHER MODIFY XMAX FOR CLIMATE-CHANGE
C
      CALL CLMAXDEN (SDIDEF,XMAX)
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING SDICAL, SDI MAXIMUM = ',XMAX
C
      RETURN
C
C
C
      ENTRY SDICLS (JSPEC,DLO,DHI,IWHO,SDIC,SDIC2,A,B,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE SDI FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, STAGES
C  SUMMATION METHOD (1968 RES. NOTE INT-77)
C  ZEIDE METHOD (ZEIDE, B., CAN. J. FOR. RES. VOL. 13, 1983;
C                SHAW, J.D., WAJF 15(1) 2000)
C
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C   SDIC = STAND DENSITY INDEX FOR THE DEFINED CLASS (STAGE)
C   SDIC2 = STAND DENSITY INDEX FOR THE DEFINED CLASS (ZEIDE METHOD)
C    A,B = SDI SUMMATION PARAMETR ESTIMATES
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'SDICLS',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,100)ICYC, ITRN, IREC1, IREC2
  100 FORMAT(' ENTERING SDICLS (ENTRY PT IN SDICAL) CYCLE =',I4,
     &' ITRN=',I4,' IREC1=',I4,' IREC2=',I4)
C----------
C  FIRST, DETERMINE STAGES METHOD PARAMETERS BASED ON ALL TREES IN THE STAND.
C----------
      SDSQ = 0.
      SPROB = 0.
      SDIC = 0.
      SDIC2 = 0.
      A = 0.
      B = 0.
C----------
C  LOOP THROUGH THE TREE LIST.
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------
      IF(ITRN .LE. 0) GO TO 150
      DO 110 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 110
      IF(DEBUG)WRITE(JOSTND,*)
     & ' IN SDICLS I,IWHO,DBHSTAGE,PROB(I),WK4(I)=',
     & I,IWHO,DBHSTAGE,PROB(I),WK4(I)
C----------
C  BRANCH IF DIAMETER IS LT MIN DBH
C----------
      IF(DBH(I) .LT. DBHSTAGE) GO TO 110 
      IF(IWHO .EQ. 1) THEN
        TPACRE=PROB(I)
      ELSE
        TPACRE=WK4(I)
      ENDIF
      SDSQ = SDSQ + (DBH(I)**2.0)*TPACRE
      SPROB=SPROB+TPACRE
  110 CONTINUE
      IF(SPROB .EQ. 0.)THEN
        SDIC = 0.
      ELSE
        A = (10.0**(-1.605))*(1.-1.605/2.)*((SDSQ/SPROB)**(1.605/2.))
        B = (10.0**(-1.605))*(1.605/2.)*((SDSQ/SPROB)**(1.605/2. - 1.))
        SDIC = SPROB*A + B*SDSQ
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)
     & ' IN SDICLS ITRN,SDSQ,SPROB,SDIC,A,B,JPNUM
     & = ',ITRN,SDSQ,SPROB,SDIC,A,B,JPNUM
C----------
C  NEXT, DETERMINE SDI BY SPECIES AND SIZE CLASS IN THE DEFINED CLASS
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------
      SDIC = 0.
      SDIC2=0.
C
      IF(ITRN .LE. 0) GO TO 150
      DO 140 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 140
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 140
      IF(IWHO .EQ. 1) THEN
        TPACRE=PROB(I)
      ELSE
        TPACRE=WK4(I)
      ENDIF
      IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
C
      LINCL = .FALSE.
      IF(JSPEC.EQ.0)THEN
        LINCL=.TRUE.
      ELSEIF((JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 120 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 130
        ENDIF
  120   CONTINUE
      ENDIF
  130 CONTINUE
C
      IF(LINCL)THEN
        IF(DBH(I).GE.DLO .AND. DBH(I).LT.DHI)THEN
          IF(DBH(I).GE.DBHZEIDE)SDIC2=SDIC2+TPACRE*(DBH(I)/10.)**1.605
          IF(DBH(I).GE.DBHSTAGE)SDIC = SDIC + (A+B*(DBH(I)**2.0))*TPACRE
          IF(DEBUG)WRITE(JOSTND,*)' CALC SDIC- I,SDIC,SDIC2,JSPEC,',
     &    'ISP,DBH,TPACRE,A,B,= ',I,SDIC,SDIC2,JSPEC,ISP(I),
     &     DBH(I),TPACRE,A,B
        ENDIF
      ENDIF
  140 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING SDICLS SDIC,SDIC2,JPNUM= ',
     &                          SDIC,SDIC2,JPNUM
C
  150 CONTINUE
      RETURN
C
C
C
      ENTRY CCCLS (JSPEC,DLO,DHI,IWHO,CRA,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE CANOPY COVER FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
C  BY CROOKSTON & STAGE GTR-RMRS-24, 1999.
C  
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C    CRA = SUM OF CROWN AREAS FOR THE DEFINED CLASS
C   CCCL = CANOPY COVER FOR THE DEFINED CLASS
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'CCCLS',5,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING CCCLS (ENTRY PT IN SDICAL)'
C----------
C  DETERMINE CROWN COVER BY SPECIES AND SIZE CLASS
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------
      CCCL = 0.
      CRA = 0.
C
      IF(ITRN .LE. 0) GO TO 250
      DO 240 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 240
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 240
C
      LINCL = .FALSE.
      IF((JSPEC.EQ.0 .OR. JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 220 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 230
        ENDIF
  220   CONTINUE
      ENDIF
  230 CONTINUE
C
      IF(LINCL)THEN
        IF(DBH(I).GE.DLO .AND. DBH(I).LT.DHI)THEN
          IF(IWHO .EQ. 1) THEN
            TPACRE=PROB(I)
          ELSE
            TPACRE=WK4(I)
          ENDIF
          IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
          CWDI = CRWDTH(I)
          CRA = CRA + CWDI*CWDI*TPACRE
          IF(DEBUG)WRITE(JOSTND,*)' I,JSPEC,ISP,DBH,HT,ICR,TPACRE,',
     &    'CWDI,CRA= ',I,JSPEC,ISP(I),DBH(I),HT(I),ICR(I),TPACRE,CWDI,
     &    CRA
        ENDIF
      ENDIF
  240 CONTINUE
      CCCL = 100.0*CRA*0.785398/43560.
C     CCCL = 100.0*(1.0-EXP(-CCCOEF*CCCL))
C     NEED LIMIT ON EXP VALUE TO AVOID UNDERFLOW   LD 4/2/2019
      TEM = MIN(86.0,(CCCOEF*CCCL))
      CCCL = 100.0*(1.0-EXP(-TEM))
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING CCCLS ITRN,CCCL,CRA,JPNUM,
     &CCCOEF= ',ITRN,CCCL,CRA,JPNUM,CCCOEF
C
  250 CONTINUE
C
      RETURN
C
C
C
C
      ENTRY RDCLS (JSPEC,DLO,DHI,IWHO,CLSD2,CLSTPA,CRD,JPNUM,
     &             TPAFAC,DIAMFAC)
C
C----------
C  THIS ENTRY COMPUTES THE RELATIVE DENSITY FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
C  BY LU, H.B., F. MARTIN, AND R. JOHNSON. 2017.
C  (Keyser, Chad E.; Keyser, Tara L. eds. 2017. Proceeding of the 2017
C  Forest vegetation simulator (FVS) e-Conference. e-Gen. Tech. Rep. SRS-224.
C  Asheville, NC: U.S. Department of Agriculture Forest Service,
C  Southern Research Station. 200 p).
C----------
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C  TPAFAC = CRD SUMMATION PARAMETR ESTIMATE FACTORS FOR TPA
C DIAMFAC   AND DIAMETER USED IN EVLDX.F SPMCDBH OPTION FOR COMPUTE OF
C           CURTIS RELATIVE DENSITY.
C   CLSD2 = SUM OF DIAMETERS SQUARED FOR DEFINED CLASS
C  CLSTPA = NUMBER OF TREES IN DEFINED CLASS
C    CRD = RELATIVE DENSITY FOR THE DEFINED CLASS
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
C INITIALIZE RETURN VARIABLES
C----------
      TPAFAC  = 0.0
      DIAMFAC = 0.0
      CLSD2   = 0.0
      CLSTPA  = 0.0
      CRD     = 0.0

      CALL DBCHK (DEBUG,'RDCLS',5,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING RDCLS (ENTRY PT IN SDICAL)',
     &  ' JSPEC,DLO,DHI,IWHO,CLSD2,CLSTPA,CRD,JPNUM,TPAFAC,DIAMFAC =',
     &  JSPEC,DLO,DHI,IWHO,CLSD2,CLSTPA,CRD,JPNUM,TPAFAC,DIAMFAC
C----------
C FIRST, DETERMINE PARAMETERS BY EULERS THEOREM BASED ON ALL TREES
C IN THE STAND.
C----------

      IF(ITRN .LE. 0) GO TO 355
C----------
C     STATIC COMPONENTS OF EQUATIONS SET HERE.
C----------
      CMPNT1 = 0.25*3.14159
      CMPNT2 = 24.0**2.0
      CMPNT4 = 1.5/2.0
      CMPNT5 = CMPNT1/CMPNT2
      CMPNT6 = 0.75*3.14159
C----------
C  LOOP THROUGH ALL TREES IN THE STAND.
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------
      DO 320 II = 1,ITRN
        I=IND1(II)
        IF(I.GE.IREC2) GO TO 320
        IF(DBH(I).LT.DBHSTAGE) GO TO 320    ! BRANCH IF D IS LT MIN DBH
        IF(IWHO .EQ. 1) THEN
          TPACRE = PROB(I)
        ELSE
          TPACRE = WK4(I)
        ENDIF
        CLSD2 = CLSD2 + (DBH(I)**2.0)*TPACRE
        CLSTPA = CLSTPA + TPACRE
  320 CONTINUE
      IF(CLSTPA .GT. 0.) THEN
C
C       *** PI was implemented as FUNCTION API() IN ORIGINAL CODE
C       A = (0.25*API()/24.0**2.0)*((SDSQ/SPROB)**(1.5/2.0))
C       A = (0.25*3.14159/24.0**2.0)*((CLSD2/CLSTPA)**(1.5/2.0))
C            --CMPNT1---  --CMPNT2-    ---CMPNT3---    -CMPNT4-
C            --------CMPNT5--------

        CMPNT3 = CLSD2/CLSTPA
        TPAFAC = CMPNT5 * (CMPNT3**CMPNT4)
C
C       *** PI was implemented as FUNCTION API() IN ORIGINAL CODE
C       B = (0.75*API()/24.0**2.0)*((SDSQ/SPROB)**(1.5/2.0 - 1.0))
C       B = (0.75*3.14159/24.0**2.0)*((CLSD2/CLSTPA)**(1.5/2.0 - 1.0))
C            --CMPNT6---  --CMPNT2-    ---CMPNT3---    -CMPNT4-
C
        DIAMFAC = (CMPNT6/CMPNT2) * (CMPNT3**(CMPNT4 - 1.0))

        CRD = CLSTPA*TPAFAC + CLSD2*DIAMFAC
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' IN RDCLS ALL TREES CRD= ',CRD

C----------
C NEXT, DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS IN
C THE DEFINED CLASS
C----------
      CRD = 0.

      DO 350 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 350
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM) GO TO 350
      IF(IWHO .EQ. 1) THEN
        TPACRE=PROB(I)
      ELSE
        TPACRE=WK4(I)
      ENDIF
      IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))

      LINCL = .FALSE.
      IF(JSPEC.EQ.0) THEN
        LINCL=.TRUE.
      ELSEIF((JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I))) THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0) THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 330 IG=2,IULIM
        IF((ISP(I).EQ.ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I))) THEN
          LINCL = .TRUE.
          GO TO 335
        ENDIF
  330   CONTINUE
      ENDIF
  335 CONTINUE

      IF(LINCL) THEN
        IF(DBH(I).GE.DLO .AND. DBH(I).LT.DHI) THEN
          IF(DBH(I).GE.DBHSTAGE) THEN
            CRD = CRD + (TPAFAC+DIAMFAC*(DBH(I)**2.0))*TPACRE
          ENDIF
          IF(DEBUG)WRITE(JOSTND,*)' CALC CRD CLASS- I,CRD,JSPEC,',
     &      'ISP,DBH,TPACRE= ',I,CRD,JSPEC,ISP(I),DBH(I),TPACRE
        ENDIF
      ENDIF
  350 CONTINUE
  355 CONTINUE
      IF(DEBUG) WRITE(JOSTND,*)
     &' LEAVING RDCLS CRD,JPNUM,CLSD2,CLSTPA,TPAFAC,DIAMFAC= ',
     & CRD,JPNUM,CLSD2,CLSTPA,TPAFAC,DIAMFAC
C
      RETURN
C
C
C
C
      ENTRY RDCLS2 (JSPEC,DLO,DHI,IWHO,CRD,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE RELATIVE DENSITY FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
C  BY MARQUIS, D.A., AND R.L. ERNST. 1992. USERS GUIDE TO SILVAH.
C  USDA FOREST SERVICE, NORTHEAST FOREST EXP STA, GEN TECH REP NE-96.
C  
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C    CRD = RELATIVE DENSITY FOR THE DEFINED CLASS
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'RDCLS2',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING RDCLS2 (ENTRY PT IN SDICAL)'
C----------
C DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS
C----------
      CRD = 0.
C----------
C  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------  
      IF(VARACD.NE.'NE')GO TO 460
C
      IF(ITRN .LE. 0) GO TO 460
      DO 450 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 450
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 450
C
      LINCL = .FALSE.
      IF((JSPEC.EQ.0 .OR. JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 420 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 440
        ENDIF
  420   CONTINUE
      ENDIF
  440 CONTINUE
C
      IF(LINCL)THEN
        IF((DBH(I).GE.DLO).AND.(DBH(I).LT.DHI).AND.(DBH(I).GE.1.))THEN
          IF(IWHO .EQ. 1) THEN
            TPACRE=PROB(I)
          ELSE
            TPACRE=WK4(I)
          ENDIF
          IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
          IEQN=MAPNE(ISP(I))
          D2 = DBH(I)*DBH(I)
          
          IF(IEQN .EQ. 1)THEN
            CRD= CRD + MAX(0.,TPACRE*(0.0033033+0.020426*
     &                     DBH(I)+0.0006776*D2))
          ELSEIF(IEQN .EQ. 2)THEN
            CRD= CRD + MAX(0.,TPACRE*(-0.027142+0.024257*
     &                     DBH(I)+0.0015225*D2))
          ELSEIF(IEQN .EQ. 3)THEN
            CRD= CRD + MAX(0.,TPACRE*(-0.0027935+0.0058959*
     &                     DBH(I)+0.0047289*D2))
          ENDIF
          IF(DEBUG)WRITE(JOSTND,*)' I,FIAJSP,JSPEC,ISP,DBH,HT,TPACRE,',
     &    'D2,CRD,JPNUM,IEQN= ',
     &    I,FIAJSP(ISP(I)),JSPEC,ISP(I),DBH(I),HT(I),TPACRE,D2,
     &    CRD,JPNUM,IEQN
        ENDIF
      ENDIF
  450 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING RDCLS2 ICYC,ITRN,',
     &'CRD,JPNUM,IEQN= ',ICYC,ITRN,CRD,JPNUM,IEQN
C
  460 CONTINUE
      RETURN
C
C
C
C
      ENTRY RDSLTR (JSPEC,IT,TREERD)
C
C----------
C  THIS ENTRY COMPUTES THE RELATIVE DENSITY CONTRIBUTION OF A GIVEN
C  TREE, USING THE METHOD DESCRIBED
C  BY MARQUIS, D.A., AND R.L. ERNST. 1992. USERS GUIDE TO SILVAH.
C  USDA FOREST SERVICE, NORTHEAST FOREST EXP STA, GEN TECH REP NE-96.
C  
C  JSPEC = TREE SPECIES
C     IT = TREE INDEX
C TREERD = RELATIVE DENSITY CONTRIBUTION OF THIS TREE
C----------
      CALL DBCHK (DEBUG,'RDSLTR',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING RDSLTR (ENTRY PT IN SDICAL)'
C----------
C  DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------
      TREERD = 0.
      IF(IT.GE.IREC2) GO TO 560
C----------
C  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
C----------  
      IF(VARACD.NE.'NE')GO TO 560
      IF(DBH(IT).LT.1.)GOTO 560
      IEQN=MAPNE(JSPEC)
      D2 = DBH(IT)*DBH(IT)
      IF(IEQN .EQ. 1)THEN
        TREERD = MAX(0.,(0.0033033+0.020426*DBH(IT)+0.0006776*D2))
      ELSEIF(IEQN .EQ. 2)THEN
        TREERD = MAX(0.,(-0.027142+0.024257*DBH(IT)+0.0015225*D2))
      ELSEIF(IEQN .EQ. 3)THEN
        TREERD = MAX(0.,(-0.0027935+0.0058959*DBH(IT)+0.0047289*D2))
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING RDSLTR  - IEQN,JSPEC,IT,DBH,',
     &' TREERD= ',IEQN,JSPEC,IT,DBH(IT),TREERD
  560 CONTINUE
      RETURN
C
C
C
C
      ENTRY SILFTY
C
C----------
C  THIS ENTRY COMPUTES THE FOREST TYPE ACCORDING TO THE SILVAH    
C  DEFINITIONS. THE SILVAH FOREST TYPE IS USED TO DETERMINE WHETHER
C  A THINRDSL TYPE THINNING IS VALID; A THINRDSL IS ONLY VALID IN
C  ONE OF THE DEFINED TYPES.
C
C  ISILFT = SILVAH FOREST TYPE (CARRIED IN THE VARCOM COMMON BLOCK)
C     0 = NO, THIS STAND DOES NOT MEET ONE OF THE DEFINED TYPES                   
C     1 = NORTHERN HARDWOOD: STANDS HAVING 65% OF THE BASAL AREA IN   
C         SUGAR MAPLE, RED MAPLE, AMERICAN BEECH, YELLOW BIRCH, SWEET
C         BIRCH, EASTERN HEMLOCK, AMERICAN BASSWOOD, CUCUMBERTREE, BLACK
C         CHERRY, WHITE ASH, OR YELLOW-POPLAR
C     2 = NORTHERN HARDWOOD-HEMLOCK: STANDS MEETING THE REQUIREMENTS FOR
C         THE NORTHERN HARDWOOD TYPE THAT ALSO HAVE AT LEAST 50% OF THE
C         BASAL AREA IN EASTERN HEMLOCK
C     3 = ALLEGHENY HARDWOOD: STANDS MEETING THE REQUIREMENTS FOR THE
C         NORTHERN HARDWOOD TYPE THAT HAVE AT LEAST 25% OF THE BASAL AREA
C         IN BLACK CHERRY, WHITE ASH, AND YELLOW-POPLAR, AND LESS THAN 
C         50% OF THE BASAL AREA IN EASTERN HEMLOCK
C     4 = OAK-HICKORY: STANDS THAT HAVE AT LEAST PART OF THEIR BASAL AREA
C         IN ANY OAK OR ANY HICKORY SPECIES
C     5 = TRANSITION: STANDS THAT HAVE AT LEAST 65% OF THE BASAL AREA IN
C         SPECIES OF EITHER THE NORTHERN HARDWOOD OR OAK-HICKORY TYPES,
C         BUT DO NOT QUALIFY FOR ANY OF THE OTHER TYPES ALONE
C
C----------
      CALL DBCHK (DEBUG,'SILFTY',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING SILFTY (ENTRY PT IN SDICAL)'
C
      ISILFT = 0
      BA1=0.
      BA2=0.
      BA3=0.
      BA4=0.
      BA5=0.
      BATOT=0.
      RATIO1 = 0.
      RATIO2 = 0.
      RATIO3 = 0.
      RATIO5 = 0.
C----------
C  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
C  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.
C----------  
      IF(VARACD.NE.'NE')GO TO 610
C
      DO 600 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 600
      ISPC=ISP(I)
      BATOT=BATOT+DBH(I)*DBH(I)*0.0054542*PROB(I)
      IF(ISLFTM(ISPC) .EQ. 1)THEN
        BA1=BA1+DBH(I)*DBH(I)*0.0054542*PROB(I)
        IF(ISPC.EQ.16)BA2=BA2+DBH(I)*DBH(I)*0.0054542*PROB(I)
        IF(ISPC.EQ.54 .OR. ISPC.EQ.42 .OR. ISPC.EQ.46)
     &     BA3=BA3+DBH(I)*DBH(I)*0.0054542*PROB(I)
      ELSEIF(ISLFTM(ISPC) .EQ. 2)THEN
        BA4=BA4+DBH(I)*DBH(I)*0.0054542*PROB(I)
      ENDIF
  600 CONTINUE
      BA5=BA1+BA4
      IF(DEBUG)WRITE(JOSTND,*)' IN SILFTY BATOT,BA1,BA2,BA3,BA4,BA5= ',
     &  BATOT,BA1,BA2,BA3,BA4,BA5
      IF(BATOT .GT. 0.)THEN
        RATIO1 = BA1/BATOT
        RATIO2 = BA2/BATOT
        RATIO3 = BA3/BATOT
        RATIO5 = BA5/BATOT
      ENDIF
      IF(RATIO1 .GE. 0.65)ISILFT=1
      IF(RATIO1 .GE. 0.65 .AND. RATIO2.GE.0.50)ISILFT=2
      IF(RATIO1 .GE. 0.65 .AND. RATIO3 .GE. 0.25 .AND. RATIO2.LT.0.50)
     &  ISILFT=3
      IF(BA4 .GT. 0.)ISILFT=4
      IF(ISILFT.EQ.0 .AND. RATIO5.GE.0.65)ISILFT=5
  610 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING SILFTY ISILFT= ',ISILFT
      RETURN
C
C
      END
