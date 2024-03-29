      SUBROUTINE FMPOFL (IYR, FMD, LNMOUT)
      IMPLICIT NONE
C----------
C FIRE-VBASE $Id$
C----------
C  SINGLE-STAND VERSION
C  CALLED FROM: FMBURN
C  CALLS (FM):  FMEFF
C               FMMOIS
C               FMFINT
C               FMCFIR
C               FMCONS
C               FMCFMD
C
C  PURPOSE:  CALCULATES THE POTENTIAL FLAME LENGTH AND FIRE EFFECTS
C            FOR TWO SETS OF MOISTURE AND WIND CONDITIONS, AND PRINTS
C            AN OUTPUT FILE WITH THE RESULTS.
C---------
C  CALL LIST DEFINITIONS:
C     IYR:  CURRENT YEAR OF SIMULATION
C     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
C  LNMOUT:  FLAG TO SUPPRESS ALL OUTPUT REPORTING.
C
C  LOCAL VARIABLE DEFINITIONS:
C     SFMOD:  SEVERE FUEL MODELS USED.  TRACKED BECAUSE IN SN, DIFFERENT
C             FUEL MODELS MAY BE SELECTED FOR SEVERE AND MODERATE CASES.
C     SFWT:   SEVERE FUEL MODEL WEIGHT.  USED WITH SFMOD.
C     SNFMODS:NUMBER OF FUEL MODELS USED IN THE SEVERE CASE.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
COMMONS
C
C  VARIABLE DECLARATIONS.

      INTEGER FMOIS, FMD, IDPM, M
      INTEGER SWIND(4), SW, J, IRTNCD
      LOGICAL DEBUG,LNMOUT
      REAL CRTCBH, CRRATE, POMORT, HPA
      REAL FLAME, POKILL(4), POVOLK(2), PSMOKE(2)
      REAL OINIT1(3), OACT1(3), CRB(4), PTORCH(2)
      CHARACTER*8 CFTYPE(3)
      CHARACTER*32 SNFMS,SNFMM
C
      DATA SWIND /20, 10, 5, 0/
C
      INTEGER IYR,IK,JPOTFL,I,IDPL,DBSKODE, K
      REAL    WMULT,ALGSLP,BYRAM,CRTFLM,FINTEN,FLB,FLT
      REAL    SFWT(MXFMOD)
      INTEGER SFMOD(MXFMOD),SNFMODS

C     ROUTINE BEGINS
C     CHECK FOR DEBUG

      CALL DBCHK (DEBUG,'FMPOFL',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,FMD
    1 FORMAT(' ENTERING FMPOFL CYCLE = ',I2,' IYR=',I5,' FMD=',I4)
C
C     MODIFY WIND SPEED BASED ON CROWN CANOPY CLOSURE (IF CHANGE THIS, ALSO
C     CHANGE THE CALCULATIONS IN FMBURN):

      WMULT=ALGSLP(PERCOV,CANCLS,CORFAC,4)

C     CALCULATE THE STAND LOWER CROWN BOTTOM HEIGHT (ACTCBH)
C     AND THE CROWN BULK DENSITY (CBD) SO THAT WE CAN CALCULATE
C     THE FLAME CRITIAL FLAME LENGTH AND THE CRITIAL DENSITY LATER

      CRRATE = 0
      CRTCBH = 0

C     INITIALIZE SOME VARIABLES USED IN SN-FFE TO ZERO

      DO K = 1,MXFMOD
        SFMOD(K) = 0
        SFWT(K) = 0
      ENDDO
      SNFMODS = 0

C     START LOOP OVER FIRE TYPES TO GET THE POTENTIAL FLAME LENGTHS
C     AND OTHER INFO.
C     NOTE THAT WE ARE ONLY LOOKING AT TWO TYPES:
C     1=(WILDFIRE/SEVERE), 3=(PRESCRIBED/MODERATE/2)

      DO 100 FMOIS = 1,3,2
C
        CALL FMMOIS(FMOIS, MOIS)

C       CHANGE THE DEFAULT WIND, MOISTURE, TEMPERATURE AND SEASON CONDITIONS,
C       IF NECESSARY. NOTE THAT FMOIS=3 IS THE SAME AS ARRAY ELEMENT 2
C       FOR THE WIND AND TEMPERATURE VALUES

        IDPL=1
        IF (FMOIS.EQ.3) IDPL=2
        SWIND(FMOIS) = INT(PREWND(IDPL))

        IF (PRESVL(IDPL,1) .EQ. 1) THEN
          MOIS(1,1) = PRESVL(IDPL,2)
          MOIS(1,2) = PRESVL(IDPL,3)
          MOIS(1,3) = PRESVL(IDPL,4)
          MOIS(1,4) = PRESVL(IDPL,5)
          MOIS(1,5) = PRESVL(IDPL,6)
          MOIS(2,1) = PRESVL(IDPL,7)
          MOIS(2,2) = PRESVL(IDPL,8)
        ENDIF

        FWIND = SWIND(FMOIS) * WMULT

        BURNSEAS = POTSEAS(IDPL)
C----------
C  CALL FIRE MODEL CALCULATIONS AGAIN FOR UT, CR, LS, CS AND SN VARIANTS
C  TT ADDED 06/03/10 SINCE THE EXPANDED VERSION NOW MODELS JUNIPER
C----------
        IF (IFLOGIC .EQ. 0) THEN
          SELECT CASE (VARACD)
C
            CASE ('CR','CS','LS','SN','TT','UT')
              CALL FMCFMD (IYR, FMD)
C
            CASE DEFAULT
C
          END SELECT
        ENDIF
C
        IF (DEBUG) WRITE(JOSTND,11) FMOIS
   11   FORMAT(' IN FMPOFL CALLING FMFINT, FMOIS=',I2)

C       COMPUTE BYRAM'S FIRELINE INTENSITY, GET THE FLAME LENGTH

        CALL FMFINT(IYR, BYRAM, FLAME, FMOIS, HPA, 1)
        CALL fvsGetRtnCode(IRTNCD)
        IF (IRTNCD.NE.0) RETURN

C       SAVE THE FLAME LENGTH...IT IMAY BE MODIFIED BELOW TO ACCOUNT
C       FOR CROWN FIRE BEHAVIOR.

        PFLAM(FMOIS) = FLAME
        PFLAM(FMOIS+1) = FLAME

C       CALCULATE SCORCH HEIGHT (IN FEET)
C       BUT FIRST CONVERT BYRAM TO BTU/FT/SECOND (RATHER THAN MIN)

        BYRAM = BYRAM / 60.0
        SCH = (63.0 / (140.0 - POTEMP(IDPL))) * (BYRAM ** (7.0 / 6.0)
     &         / (BYRAM + FWIND ** 3.0) ** 0.5)

C       CALL ROUTINE FOR CALCULATING CROWN FIRE INFORMATION IN WESTERN VARIANTS

        IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
          CRBURN = 0
          DO K = 1,3
            OINIT1(K) = -1
            OACT1(K) = -1
          ENDDO
        ELSE
          SW = SWIND(FMOIS)
          CALL FMCFIR(IYR,FMOIS,WMULT,SW,CFTYPE(FMOIS),OINIT1,OACT1,HPA)
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN

          CRB(FMOIS) = CRBURN
        ENDIF

C       MODIFY FLAME LENGTH AND SCORCH HEIGHT TO ACCOUNT FOR
C       CROWN FIRE BEHAVIOR.

        IF (CRBURN .GT. 0.0) THEN
          FINTEN = (HPA + TCLOAD * 7744.8 * CRBURN) * RFINAL / 60.0
          FLB=0.45*FINTEN**.46
          FLT=0.2*FINTEN**.667
          PFLAM(FMOIS)=FLB+CRBURN*(FLT-FLB)
          SCH = (63.0 / (140.0 - POTEMP(IDPL)))
     &          * (FINTEN ** (7.0 / 6.0)
     &          / (FINTEN + FWIND ** 3.0) ** 0.5)
        ENDIF

C       CALCULATE THE POTENTIAL MORTALITY
        IF (DEBUG) WRITE(JOSTND,*) 'SCH = ', SCH
        IK=1
        IF (FMOIS.EQ.3) IK=2
C
        CALL FMEFF(IYR, FMD, PFLAM(FMOIS), 1, POMORT, POVOLK(IK),1,
     &             POTPAB(IK))
        POKILL(FMOIS) = POMORT
C
C       SET SOME COMMON VARIABLES THAT ARE NEEDED FOR THE POTFMORT
C       POTFTYPE, POTSRATE, AND POTREINT EVENT MONITOR FUNCTIONS

        POTKIL(FMOIS) = POKILL(FMOIS)
        POTVOL(IK) = POVOLK(IK)
        IF (CFTYPE(FMOIS).eq.'SURFACE') THEN
          POTTYP(IK) = 1
        ELSEIF (CFTYPE(FMOIS).eq.'COND_CRN') THEN
          POTTYP(IK) = 4
        ELSEIF (CFTYPE(FMOIS).eq.'PASSIVE') THEN
          POTTYP(IK) = 2
        ELSEIF (CFTYPE(FMOIS).eq.'ACTIVE') THEN
          POTTYP(IK) = 3
        ENDIF
        POTFSR(IK) = SFRATE(FMOIS)
        POTFSR(IK+2) = RFINAL
        POTRINT(IK) = SXIR(FMOIS)

C       STORE THE POTENTIAL SMOKE PRODUCTION (PM 2.5)

        PSMOKE(IK)=0.
        CALL FMCONS(FMOIS,0,0.,IYR,1,PSMOKE(IK),POTPAB(IK))

C       SAVE THE FUEL MODELS USED IN THE SEVERE CASE, SINCE DIFFERENT ONES
C       MAY BE USED FOR THE MODERATE CASE.  (USED IN SN-FFE ONLY)

        IF (FMOIS .EQ. 1) THEN
          DO K = 1,NFMODS
            SFMOD(K) = FMOD(K)
            SFWT(K) = FWT(K)
          ENDDO
          SNFMODS = NFMODS
        ENDIF
  100 CONTINUE
      CALL FMPOFL_FMPTRH(IYR,ITRN,FMPROB,PFLAM(2),PFLAM(4),
     >                   PTORCH(1),PTORCH(2))

C     SET EVENT MONITOR VARIABLES (FROM **EVTSTV**)
C     422 CROWNIDX  CROWNING INDEX FROM POTENTIAL FIRE REPT (FM)
C     426 CRBASEHT  CROWN BASE HT FROM POTENTIAL FIRE REPT (FM)
C     427 TORCHIDX  TORCHING INDEX FROM POTENTIAL FIRE REPT (FM)
C     428 CRBULKDN  CROWN BULK DENSITY FROM POTENTIAL FIRE REPT (FM)

      CALL EVSET4(22, OACT1(1))
      CALL EVSET4(26, FLOAT(ACTCBH))
      CALL EVSET4(27, OINIT1(1))
      CALL EVSET4(28, CBD)
C
      IF (IYR .EQ. 0 .AND. IYR .EQ. IPFLME) GOTO 10 ! weird line - dr
      IF (IYR .LT. IPFLMB .OR. IYR .GT. IPFLME) RETURN

   10 CONTINUE

C     COMPUTE CRTFLM (A CRITICAL FLAME LENGTH) AND CRTCBH (A
C     CRITICAL CROWN BASE HEIGHT). THESE VARIABLE ARE NOT CURRENTLY
C     BEING USED OR OUTPUT.

      CRTFLM = 0.
      CRRATE = 0.
      CRTCBH = 0.
      IF (ACTCBH .GT. 0) THEN
        CRTFLM = 0.45 * ((0.0030976 * ACTCBH * 1316.5)**1.5)**0.46
        IF (CRTFLM .GT. ACTCBH) CRTFLM = ACTCBH
      ENDIF
      IF (CBD .GT. 0.0)
     &    CRRATE = 0.61446 / CBD
      IF (PFLAM(1) .GT. 0)
     &    CRTCBH = (1.0/(0.0030976*1316.5)) *
     &    ((((1.0/0.45) * PFLAM(1))**(1.0/0.46))**(1.0/1.5))

C     RETURN IF ALL OUTPUT IS BEING SUPPRESSED

      IF (.NOT.LNMOUT) RETURN

C     DBS CALL FOR DUMPING DATA TO THE DATABASE
      IF(ICYC.EQ.1)THEN
        DO IDPM = 1,2
          M = IDPM
          IF (IDPM .EQ. 2) M=3
          CALL FMMOIS(M, MOIS)
          IF (PRESVL(IDPM,1) .EQ. 1) THEN
            MOIS(1,1) = PRESVL(IDPM,2)
            MOIS(1,2) = PRESVL(IDPM,3)
            MOIS(1,3) = PRESVL(IDPM,4)
            MOIS(1,4) = PRESVL(IDPM,5)
            MOIS(1,5) = PRESVL(IDPM,6)
            MOIS(2,1) = PRESVL(IDPM,7)
            MOIS(2,2) = PRESVL(IDPM,8)
          ENDIF
        CALL DBSFMPFC(NPLT,PREWND(IDPM),INT(POTEMP(IDPM)),
     &          (100.0*MOIS(1,1)),(100.0*MOIS(1,2)),(100.0*MOIS(1,3)),
     &          (100.0*MOIS(1,4)),(100.0*MOIS(1,5)),(100.0*MOIS(2,1)),
     &          (100.0*MOIS(2,2)),IDPM)
       ENDDO
      ENDIF 
      DBSKODE = 1
      CALL DBSFMPF(IYR, NPLT, PFLAM(2), PFLAM(4), PFLAM(1), PFLAM(3),
     &     CFTYPE(1), CFTYPE(3),PTORCH(1),PTORCH(2),OINIT1(1),OACT1(1),
     &     ACTCBH, CBD, INT(POKILL(1)*100.),INT(POKILL(3)*100.),
     &     INT(POVOLK(1)),INT(POVOLK(2)),PSMOKE(1)*P2T,PSMOKE(2)*P2T,
     &     SFMOD,SFWT,FMOD,FWT,DBSKODE)
      IF(DBSKODE.EQ.0) RETURN

C     PRINT THE OUTPUT FILE.  NOTE THAT PFLAM IS IN FEET and CBD IS IN KG/M3

      CALL GETLUN (JPOTFL)

      IF (IPFPAS .EQ. 0) THEN
        WRITE (JPOTFL,130) IDPFLM, IDPFLM
        WRITE (JPOTFL,143) IDPFLM
        WRITE (JPOTFL,131) IDPFLM
        WRITE (JPOTFL,132) IDPFLM
        WRITE (JPOTFL,44) IDPFLM,NPLT,MGMID
        WRITE (JPOTFL,143) IDPFLM

C       WRITE NEW HEADER INFORMATION GIVING THE SEVERE AND MODERATE CONDITIONS

        WRITE (JPOTFL,144) IDPFLM
        WRITE (JPOTFL,145) IDPFLM
        DO IDPL = 1,2
          J = IDPL
          IF (IDPL .EQ. 2) J=3
          CALL FMMOIS(J, MOIS)
          IF (PRESVL(IDPL,1) .EQ. 1) THEN
            MOIS(1,1) = PRESVL(IDPL,2)
            MOIS(1,2) = PRESVL(IDPL,3)
            MOIS(1,3) = PRESVL(IDPL,4)
            MOIS(1,4) = PRESVL(IDPL,5)
            MOIS(1,5) = PRESVL(IDPL,6)
            MOIS(2,1) = PRESVL(IDPL,7)
            MOIS(2,2) = PRESVL(IDPL,8)
          ENDIF

          IF (IDPL .EQ. 1) THEN
            WRITE (JPOTFL,146) IDPFLM,PREWND(IDPL),INT(POTEMP(IDPL)),
     &                         ((100.0*MOIS(1,I)),I=1,5),
     &                         (100.0*MOIS(2,1)),(100.0*MOIS(2,2))
          ELSE
            WRITE (JPOTFL,147) IDPFLM,PREWND(IDPL),INT(POTEMP(IDPL)),
     &                         ((100.0*MOIS(1,I)),I=1,5),
     &                         (100.0*MOIS(2,1)),(100.0*MOIS(2,2))
          ENDIF
        ENDDO
        WRITE (JPOTFL,143) IDPFLM

C       FORMAT THE HEADERS SLIGHTLY DIFFERENTLY FOR SN-FFE

        IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
          WRITE (JPOTFL,233) IDPFLM
          WRITE (JPOTFL,235) IDPFLM
          WRITE (JPOTFL,237) IDPFLM
          WRITE (JPOTFL,240) IDPFLM
        ELSE
          WRITE (JPOTFL,133) IDPFLM
          WRITE (JPOTFL,135) IDPFLM
          WRITE (JPOTFL,137) IDPFLM
          WRITE (JPOTFL,140) IDPFLM
        ENDIF
        WRITE (JPOTFL,143) IDPFLM
  130   FORMAT (2(/1X,I5))
  131   FORMAT (1X,I5,1X,32X,'******  FIRE MODEL VERSION 1.0 ******')
  132   FORMAT (1X,I5,1X,40X,'POTENTIAL FIRE REPORT '
     &                       '(BASED ON STOCKABLE AREA)')
   44   FORMAT (1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
  133   FORMAT (1X,I5,7X,'FLAME LENGTH (FT)',2X,
     &         'FIRE PROB OF   TORCH  CROWN ',
     &         'CNPY CANPY    POTENTIAL MORTALITY  POTEN. SMOKE')
  135   FORMAT (1X,I5,7X,' SURFACE    TOTAL  TYPE TORCHING',
     &         2('  INDEX'),' BASE BULK   ',22('-'),1X,12('-'),
     &         11X,'FUEL MODELS')
  137   FORMAT (1X,I5,7X,10('-'),1X,7('-'),1X,4('-'),1X,9('-'),
     &         ' SEVERE SEVERE HT  DENSTY ',
     &         'SEV. MOD. SEV.  MOD.   SEV.  MOD.   ',31('-'))
  140   FORMAT (1X,I5,1X,'YEAR  SEV   MOD  SEV MOD S  M SEV   MOD',
     &         1X,'MI/HR',2X,'MI/HR  FT  ',
     &         'KG/M3   %BA  %BA (TOT CU VOL)  (T/A <2.5) ',
     &         4(' MOD %WT'))
  143   FORMAT (1X,I5,1X,132('-'))
  233   FORMAT (1X,I5,7X,'FLAME LENGTH',1X,
     &         'CNPY CANPY    POTENTIAL MORTALITY  POTEN. SMOKE')
  235   FORMAT (1X,I5,7X,12('-'),' BASE BULK   ',
     &          1X,35('-'),7X,'FUEL MODELS (SEV.)',17X,
     &          'FUEL MODELS (MOD.)')
  237   FORMAT (1X,I5,7X,'SEVERE MODER HT  DENSTY ',2X,
     &          'SEV. MOD. SEV.  ','MOD.   SEV.  MOD.   ',
     &          31('-'),3X,31('-'))
  240   FORMAT (1X,I5,1X,'YEAR   FT     FT   FT   KG/M3',
     &         '   %BA  %BA (MER CU VOL)  (T/A <2.5) ',1X,
     &         4(' MOD %WT'),2X,4(' MOD %WT'))

C       FORMAT STATEMENTS FOR THE NEW HEADERS:

  144   FORMAT (1X,I5,1X,'FIRE        WIND   TEMP ------------- FUEL ',
     &         'MOISTURE CONDITIONS (PERCENT) ---------------')
  145   FORMAT (1X,I5,1X,'CONDITION   (MPH)   (F) 0-0.25"  0.25-1"  ',
     &         ' 1-3"     3"+   DUFF    LIVE WOODY   LIVE HERB')

  146   FORMAT(1X,I5,1X,'SEVERE  ',3X,F5.1,3X,I3,2X,F5.0,5X,4(F5.0,3X),
     &         2(4X,F5.0,3X))
  147   FORMAT(1X,I5,1X,'MODERATE',3X,F5.1,3X,I3,2X,F5.0,5X,4(F5.0,3X),
     &         2(4X,F5.0,3X))
C
        IPFPAS = IPFPAS + 1
C
      ENDIF

C     WRITE OUT DATA TO FILE.  THE SN VARIANT IS PRINTED DIFFERENTLY,
C     BECAUSE CROWN FIRE IS NOT MODELLED AND DIFFERENT FUEL MODELS CAN BE
C     CHOSEN FOR THE SEVERE AND MODERATE SCENARIOS

      IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN ! write the pot. table differently
         WRITE (SNFMS,'(8I4)')
     &         (SFMOD(I),INT((SFWT(I)*100.)+0.5),I=1,SNFMODS)
         WRITE (SNFMM,'(8I4)')
     &         (FMOD(I),INT((FWT(I)*100.)+0.5),I=1,NFMODS)
         WRITE (JPOTFL,51) IDPFLM, IYR, PFLAM(2), PFLAM(4),
     &     ACTCBH, CBD,INT(POKILL(1)*100.),INT(POKILL(3)*100.),
     &     INT(POVOLK(1)),INT(POVOLK(2)),PSMOKE(1)*P2T,PSMOKE(2)*P2T,
     &     SNFMS,TRIM(SNFMM)
   51   FORMAT (1X,I5,1X,I4,2(1X,F5.1),2X,I3,1X,F6.3,1X,2(2X,I3),1X,I5,
     &          1X,I6,2X,F4.2,1X,F5.2,2X,A,2X,A)
      ELSE ! write like usual
        WRITE (JPOTFL,49) IDPFLM, IYR, PFLAM(2), PFLAM(4),
     &      INT(PFLAM(1)+.5), INT(PFLAM(3)+.5), CFTYPE(1)(1:1),
     &      CFTYPE(3)(1:1),PTORCH(1), PTORCH(2), OINIT1(1), OACT1(1),
     &      ACTCBH, CBD, INT(POKILL(1)*100.),INT(POKILL(3)*100.),
     &      INT(POVOLK(1)),INT(POVOLK(2)),PSMOKE(1)*P2T,PSMOKE(2)*P2T,
     &      (FMOD(I),INT((FWT(I)*100.)+0.5),I=1,NFMODS)
   49   FORMAT (1X,I5,1X,I4,2(1X,F5.1),2(1X,I3),
     &       1X,A1,2X,A1,2(1X,F4.2),2(1X,F6.1),1X,I3,1X,F6.3,
     &       2(2X,I3),1X,I5,1X,I6,1X,F4.2,1X,F5.2,2X,8(1X,I3))
      ENDIF
C
      IF (DEBUG) WRITE(JOSTND,50) IYR,FMKOD,LARGE,SMALL,PERCOV,PFLAM(1),
     &           (FMOD(I),INT((FWT(I)*100.)+0.5),
     &           I=1,NFMODS)
   50 FORMAT(' FMPOFL SPEC: ',2I5,2F10.3,F6.1,F5.1,8(1X,I3))
C     
      RETURN
      END


      SUBROUTINE FMPOFL_FMPTRH(IYR,MXI,PRB,FLM1,FLM2,PTR1,PTR2)
      IMPLICIT NONE
C----------
C  **FMPTRH FIRE-BASE-DATE OF LAST REVISION:  05/18/2004
C----------
C     SERVICE ROUTINE TO FMPOFL TO COMPUTE PTORCH FOR EACH OF TWO
C     FLAME LENGTHS.  NOT DESIGNED TO BE CALLED FROM OTHER PARTS
C     OF THE PROGRAM.
C
C     NL CROOKSTON -- RMRS MOSCOW -- MAY 2004
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
C
C
COMMONS
C
C     SET THE MAXIMUM NUMBER OF REPLICATIONS.

      INTEGER MXREPS
      PARAMETER (MXREPS=30)

C     SET THE VERTIAL PLOT SIZE

      REAL PSIZE
      PARAMETER (PSIZE=.025)

      DOUBLE PRECISION SAVES0
      EXTERNAL RANN
      INTEGER MXI,I,IREP,JC,YES(MXI),IYR,NYES,
     >        INDX(MAX(MXI,MXREPS)),II

      INTEGER ITOP,J,JJ
      REAL MINCB(MXREPS),MXHT

      REAL PRB(MXI),CBH(MXI),RAN,MXNT1,MXNT2,
     >     FLM1,FLM2,AVHT,SSUM,P,PTR1,PTR2,CRIT,AVCBHT

      DOUBLE PRECISION Z,PT1,PT2,Q,PDF

      LOGICAL CHKFLG

      PTR1=0.
      PTR2=0.
      AVCBHT=-1.
      IF (MXI.LE.0) RETURN
      IF (FLM1.LE.0.0 .AND. FLM2.LE.0) RETURN

      CBH=HT(1:MXI)*(1.-(ICR(1:MXI)*.01))
      CALL RDPSRT(MXI,CBH,INDX,.TRUE.)

C     SAVE THE RANDOM NUMBER STATUS

      CALL RANNGET(SAVES0)

C     DROP SOME RANDOM NUMBERS SO THAT CALLS FROM DIFFERENT YEARS HAVE
C     DIFFERENT SEQUENCES.

      DO I=1,MOD(IYR,10)
         CALL RANN(RAN)
      ENDDO

C     COMPUTE THE TOP HEIGHT OF THE STAND...AND SET THE CRITICAL HEIGHT
C     THAT THE FLM1S MUST REACH.

      AVHT=0.
      SSUM=0.
      DO I=1,MXI
         P=PRB(I)
         IF(SSUM+P.GT.40.0) P=40.0-SSUM
         SSUM=SSUM+P
         AVHT=AVHT+HT(I)*P
         IF(SSUM.GE.40.0) EXIT
      ENDDO
      IF (SSUM.GT.0.) AVHT=AVHT/SSUM
      CRIT=MAX(5.,MIN(.5*AVHT,50.))

C     COMPUTE THE PROBABILITY OF TORCHING FOR EACH REP.

      DO IREP=1,MXREPS

C        FIRST, DECIDE WHICH TREES ARE ON THE VIRTUAL PLOT...

         ITOP=0
         NYES=0
         DO II=1,MXI
            I=INDX(II)
            CALL RANN(RAN)
            CHKFLG = .FALSE.
            IF (PRB(I).GT.1000.) THEN
               CHKFLG = .TRUE.
            ELSE IF (RAN .GT. EXP(-PRB(I)*PSIZE)) THEN
               CHKFLG = .TRUE.
            ENDIF

            IF (CHKFLG) THEN
               NYES=NYES+1
               YES(NYES)=I

C              SET ITOP TO 1 IF THERE IS LEAST 1 TREE THAT IS
C              OVER THE CRITICAL HEIGHT.

               IF (HT(I).GE.CRIT) ITOP=1
            ENDIF
         ENDDO

C        FIND THE LOWEST CROWN THAT MUST BE IGNITED FOR THE PLOT
C        TO TORCH.

         JC=-1
         MINCB(IREP)=-1.
         MXHT=-1.
         IF (ITOP.EQ.1) THEN
            DO II=NYES,1,-1
               I=YES(II)
               IF (HT(I).GE.CRIT) THEN ! LADDER REACHED WITH THIS TREE
                  MINCB(IREP)=CBH(I)
                  MXHT=HT(I)
                  EXIT
               ELSE  ! SEE IF TREE J CAN CAUSE LADDER TO BURN TO CRIT
                  IF (II.GT.1) THEN
                     JC=I
                     MXHT=HT(JC)
                     DO JJ=II-1,1,-1
                        J=YES(JJ)
                        IF (MXHT*1.25 .GT. CBH(J)) THEN
                           IF (MXHT.LT.HT(J)) MXHT=HT(J)
                           IF (MXHT.GE.CRIT) THEN
                              MINCB(IREP)=CBH(I)
                              EXIT
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDIF
                  IF (MINCB(IREP).GT.-1.) EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO

C     FIND THE AVERAGE LOWEST POINT...NOT THAT CURRENTLY WE DON'T
C     USE IF FOR ANYTHING.

      II=0
      DO I=1,MXREPS
         IF (MINCB(I).NE.-1.) THEN
            II=II+1
            INDX(II)=I
         ENDIF
      ENDDO
      IF (II.EQ.1) THEN
         AVCBHT=MINCB(INDX(1))
         MINCB(INDX(1))=LOG(MINCB(INDX(1)))    ! COMPUTE THE LOG
      ELSE IF (II.GT.1) THEN
! (sort or not, as you wish)         CALL RDPSRT(II,MINCB,INDX,.FALSE.)
         AVCBHT=0.
         DO I=1,II
            AVCBHT=AVCBHT+MINCB(INDX(I))
            MINCB(INDX(I))=LOG(MINCB(INDX(I)))    ! COMPUTE THE LOG
         ENDDO
         AVCBHT=AVCBHT/FLOAT(II)
      ENDIF

C     FIND THE PROBABILITY OF TORCHING FOR BOTH IGNITION HEIGHTS.

      P=1./FLOAT(MXREPS)

      PTR1=0.
      IF (FLM1.GT. 0.0001) THEN
         MXNT1=LOG(((FLM1/.0775)**1.45)/30.5)
         DO I=1,II
            J=INDX(I)
            Z=(MINCB(J)-MXNT1)/.25 ! STD DEV IS SET TO .25 (LOG SCALE)
            CALL FMPOFL_NPROB(Z,Q,PT1,PDF)
            IF (PT1.LT.1D-7) PT1=0D0
            PTR1=REAL(PTR1+(PT1*P))
         ENDDO
      ENDIF

      PTR2=0.
      IF (FLM2.GT. 0.0001) THEN
         MXNT2=LOG(((FLM2/.0775)**1.45)/30.5)
         DO I=1,II
            J=INDX(I)
            Z=(MINCB(J)-MXNT2)/.25
            CALL FMPOFL_NPROB(Z,Q,PT2,PDF)
            IF (PT2.LT.1D-7) PT2=0D0
            PTR2=REAL(PTR2+(PT2*P))
         ENDDO
      ENDIF

C      PRINT *,'IYR=',IYR,' MXREPS=',MXREPS,' II=',II,' FLM=',FLM1,FLM2,
C     >        ' PTR=',PTR1,PTR2,' AVCBHT=',AVCBHT

C     RESTORE THE RANDOM NUMBER STATUS

      CALL RANNPUT(SAVES0)

      RETURN
      END

        SUBROUTINE FMPOFL_NPROB(Z,P,Q,PDF)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C       P, Q = PROBABILITIES TO THE LEFT AND RIGHT OF Z
C       FOR THE STANDARD NORMAL DISTRIBUTION.
C       PDF  = THE PROBABILITY DENSITY FUNCTION
C
C       REFERENCE: ADAMS,A.G. AREAS UNDER THE NORMAL CURVE,
C       ALGORITHM 39, COMPUTER J., VOL. 12, 197-8, 1969.
C
C       LATEST REVISION - 23 JANUARY 1981
C
C********************************************************************
C
        DATA A0,A1,A2,A3,A4,A5,A6,A7/0.5D0, 0.398942280444D0,
     1  0.399903438504D0, 5.75885480458D0, 29.8213557808D0,
     2  2.62433121679D0, 48.6959930692D0, 5.92885724438D0/,
     3  B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11/0.398942280385D0,
     4  3.8052D-8, 1.00000615302D0, 3.98064794D-4, 1.98615381364D0,
     5  0.151679116635D0, 5.29330324926D0, 4.8385912808D0,
     6  15.1508972451D0, 0.742380924027D0, 30.789933034D0,
     7  3.99019417011D0/
C
        ZABS = ABS(Z)
        IF(ZABS.GT.12.7D0) GO TO 20
        Y = A0*Z*Z
        PDF = EXP(-Y)*B0
        IF(ZABS.GT.1.28D0) GO TO 10
C
C       Z BETWEEN -1.28 AND +1.28
C
        Q = A0-ZABS*(A1-A2*Y/(Y+A3-A4/(Y+A5+A6/(Y+A7))))
        IF(Z.LT.0.D0) GO TO 30
        P = 1.D0-Q
        RETURN
C
C       ZABS BETWEEN 1.28 AND 12.7
C
   10   Q = PDF/(ZABS-B1+B2/(ZABS+B3+B4/(ZABS-B5+B6/(ZABS+B7-B8/
     1  (ZABS+B9+B10/(ZABS+B11))))))
        IF(Z.LT.0.D0) GO TO 30
        P = 1.D0-Q
        RETURN
C
C       Z FAR OUT IN TAIL
C
   20   Q = 0.D0
        PDF = 0.D0
        IF(Z.LT.0.D0) GO TO 30
        P = 1.D0
        RETURN
C
C       NEGATIVE Z, INTERCHANGE P AND Q
C
   30   P = Q
        Q = 1.D0-P
        RETURN
        END


