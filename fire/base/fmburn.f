      SUBROUTINE FMBURN (IYR, FMD, LNMOUT)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id: fmburn.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
C
C     CALLED FROM: FMMAIN
C     CALLS    FMCFMD
C              FMCFIR
C              FMCONS
C              FMEFF
C              FMFINT
C              FMFOUT
C              FMMOIS
C              FMPOFL
C              FMCFMD3
C
C  PURPOSE:
C     THIS SUBROUTINE IS THE DRIVING PART OF THE FIRE INTENSITY AND EFFECTS
C     SUBMODEL. IT FIRST DETERMINES THE ENVIRONMENTAL VARIABLES, THEN
C     SELECTS THE APPROPRIATE FUEL MODEL. NEXT, ALL THE OTHER SUBROUTINES
C     ARE CALLED TO DETERMINE PROB OF MORT.
C     THIS IS BASED ON INFORMATION FROM JIM BROWN AND ELIZABETH REINHARDT.
C
C  CALL LIST DEFINITIONS:
C     IYR:  CURRENT YEAR
C     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
C     LNMOUT: TRUE IF NORMAL OUTPUT PROCESS, FALSE TO SUPPRESS ALL OUTPUTS
C
C  LOCAL VARIABLE DEFINITIONS:
C     FMOIS:  FUEL MOISTURE CODES
C     SCH:     SCORCH HEIGHT (IN FEET)
C     STLAST: LAST STAND THAT COULD BE CALLED
C     HPA:    HEAT PER UNIT AREA
C     IFTYPE = 1 IF USER USED FLAMEADJ KEYWORD
C     USRFL = TRUE IS USER ENTERED FLAME LENGTH ON FLAMEADJ KEYWORD
C     MKODE = MORTALITY CODE (0=TURN OFF FFE MORTALITY, 1=FFE ESTIMATES MORTALITY)
C     PSBURN = PERCENTAGE OF THE STAND THAT IS BURNED
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
COMMONS
C----------
C     VARIABLE DECLARATIONS.
C----------
      CHARACTER VVER*7
      INTEGER  FMOIS, FMD, IFMD, I, J, INL, INDD, IRTNCD
      REAL     FLMULT
      REAL     OLDFL
      REAL     FPRMS(4),CPRMS(6),MPRMS(7),DPRMS(1),PRMS(13)
      REAL     TMP2(3), TMP3(3)
      REAL     SUMPS, XSUR(2,3),XFML(2,3), XDEP, XEXT
      CHARACTER*8 CFTMP
      INTEGER  MYACTS(5)
      LOGICAL  DEBUG,LOK,LNMOUT,USRFL,LCFTMP
      DATA     MYACTS/2505,2506,2507,2529,2539/
      INTEGER  IYR,ICOND,ITODO,IFT,NPRM,IACTK,JYR,IFTYPE,JTODO,
     &         IFC,KTODO,JDO,IFIRE, MKODE
      REAL     SWIND,FLAME,WMULT,ALGSLP,BYRAM,CRRATE,CRTCBH,
     &         PSMOKE,PMRT,PVOLKL,HPA,FINTEN,FLB,FLT,TMPINT,TMPFLAME,
     &         MAXINT,MININT,UCRBURN,PSBURN
C----------
C  CHECK FOR DEBUG.
C----------
      CALL DBCHK (DEBUG,'FMBURN',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC, IYR
    7 FORMAT(' FMBURN CYCLE=',I2,' IYR=',I5)
C----------
C  IF NO FIRES ARE SCHEDULED OR NO TREES ARE IN THE STAND, BUT
C  IF FUEL BURNING HAS BEEN DONE THEN CALL THE ROUTINE WHICH
C  PRINTS THE FIRE REPORTS.
C----------
      ICOND = 0
      CALL OPFIND(1,MYACTS(2),ITODO)
      IF (ITODO.GT.0) THEN
        DO 422 IFT = 1,ITODO
          CALL OPGET(IFT,3,JYR,IACTK,NPRM,CPRMS)
C          IF (JYR .NE. IYR) GO TO 422
          ICOND = 1
  422   CONTINUE
      ENDIF
C
      IF ((ICOND .EQ. 0 .OR. COVTYP .LE. 0) .AND. (LFLBRN)) THEN
        CALL FMFOUT(IYR,0.0,0, 0,'FUELBURN')
      ENDIF
C----------
C  CHECK TO SEE IF THERE IS A DROUGHT STATE; THIS
C  AFFECTS THE FUEL MODEL IN SOME VARIANTS (UT/CR/LS)
C----------
      CALL OPFIND(1,MYACTS(4),ITODO)
      IF (ITODO.EQ.0) GOTO 406
      DO JDO = 1,ITODO
        CALL OPGET(JDO,1,JYR,IACTK,NPRM,DPRMS)
C        IF (JYR .EQ. IYR) THEN
          CALL OPDONE(JDO,IYR)
          IDRYB = JYR
          IDRYE = INT(REAL(JYR) + DPRMS(1) - 1.)
          GOTO 406
C        ENDIF
      ENDDO
  406 CONTINUE
C----------
C  CHECK TO SEE IF THE USER HAS ALTERED THE DEFINITION
C  OF THE FUEL MODEL PARAMETERS THIS YEAR.
C  THIS IS CALLED HERE RATHER THAN IN FMGFMV BECAUSE
C  WE ONLY WANT TO HAVE TO CHECK THIS ONCE IN A YEAR
C  (AND FMGFMV CAN BE CALLED MANY TIMES).
C----------
      CALL OPFIND(1,MYACTS(5),ITODO)
      IF (ITODO.EQ.0) GOTO 399
      DO JDO = 1,ITODO
        CALL OPGET(JDO,13,JYR,IACTK,NPRM,PRMS)
C        IF (JYR .EQ. IYR) THEN
          LOK   = .TRUE.
          SUMPS = 0.0
          INL   = 0
          INDD   = 0
          DO I = 1, 2       ! ZERO TEMP COPIES
            DO J = 1, 3
              XSUR(I,J) = 0.0
              XFML(I,J) = 0.0
            ENDDO
          ENDDO
C
          IFMD = INT(PRMS(1))
          IF (IFMD .LT. 1 .OR. IFMD .GT. MXDFMD) LOK = .FALSE.
C
          IF (LOK) THEN     ! TEST EVERYTHING BEFORE COPYING
C
            DO I = 2,4
              IF (PRMS(I) .LT. 0.0) THEN
                XSUR(1,I-1) = SURFVL(IFMD,1,I-1)
              ELSE
                XSUR(1,I-1) = PRMS(I)
              ENDIF
            ENDDO
            IF (PRMS(5) .LT. 0.0) THEN
              XSUR(2,1) = SURFVL(IFMD,2,1)
            ELSE
              XSUR(2,1) = PRMS(5)
            ENDIF
            IF (PRMS(12) .LT. 0.0) THEN
              XSUR(2,2) = SURFVL(IFMD,2,2)
            ELSE
              XSUR(2,2) = PRMS(12)
            ENDIF
C
            DO I = 1,2
              DO J = 1,3
                SUMPS = SUMPS + XSUR(I,J)
              ENDDO
            ENDDO
            IF (SUMPS .LE. 0.0001) LOK = .FALSE.
C
          ENDIF
C
          IF (LOK) THEN
            DO I = 6,8
              IF (PRMS(I) .LT. 0.0) THEN
                XFML(1,I-5) = FMLOAD(IFMD,1,I-5)
              ELSE
                XFML(1,I-5) = PRMS(I)
              ENDIF
            ENDDO
            IF (PRMS(9) .LT. 0.0) THEN
              XFML(2,1) = FMLOAD(IFMD,2,1)
            ELSE
              XFML(2,1) = PRMS(9)
            ENDIF
            IF (PRMS(13) .LT. 0.0) THEN
              XFML(2,2) = FMLOAD(IFMD,2,2)
            ELSE
              XFML(2,2) = PRMS(13)
            ENDIF
C
            DO I = 1,3
              IF (XFML(1,I) .GT. 0.0) INDD = INDD + 1
              IF (XFML(2,I) .GT. 0.0) INL = INL + 1
            ENDDO

            IF (INDD .LE. 0 .AND. INL .LE. 0) LOK = .FALSE.
C
          ENDIF
C
          IF (LOK) THEN
            IF (PRMS(10) .LT. 0.0) THEN
              XDEP = FMDEP(IFMD)
            ELSE
              XDEP = PRMS(10)
            ENDIF
            IF (XDEP .LT. 0.0) LOK = .FALSE.
          ENDIF
C
          IF (LOK) THEN
            IF (PRMS(11) .LT. 0.0) THEN
              XEXT = MOISEX(IFMD)
            ELSE
              XEXT = PRMS(11)
            ENDIF
            IF (XEXT .LT. 0.0 .OR. XEXT .GT. 1.0) LOK = .FALSE.
          ENDIF
C----------
C  ALL PARAMETERS ARE OK AFTER READING FROM PRMS
C  OR COPYING FROM EXISTING VALUES: ASSIGN
C----------
          IF (LOK) THEN
            DO I = 1,2
              DO J = 1,3
                SURFVL(IFMD,I,J) = INT(XSUR(I,J))
                FMLOAD(IFMD,I,J) = XFML(I,J)
              ENDDO
            ENDDO
            FMDEP(IFMD)  = XDEP
            MOISEX(IFMD) = XEXT
            CALL OPDONE(JDO,IYR)
          ELSE
            CALL OPDEL1(JDO)
          ENDIF
C
C        ENDIF
      ENDDO
  399 CONTINUE

C----------
C  CALL NEW ROUTINE TO LOAD INFORMATION FOR CALCULATING THE 
C  FUEL MODEL VARIABLES
C----------
      CALL FMCFMD3(IYR, FMD)   
C----------
C  IF USING MODELLED FUEL LOADS TO PREDICT FIRE BEHAVIOR, LOAD CUSTOM
C  FUEL MODEL 89 WITH THE RIGHT PARAMETERS.
C----------
csb: moved to fmcfmd3

C----------
C  COMPUTE CANOPY BASE HEIGHT, CROWN BULK DENSITY AND TOTAL
C  CANOPY LOAD; RESULTS IN **FMFCOM** ACTCBH,CBD,TCLOAD
C----------
csb: moved to fmcfmd3
C      CALL FMPOCR(IYR)

csb: moved to fmcfmd3
c     &    VVER(1:2) .EQ. 'SN')) CALL FMCFMD (IYR, FMD)
c        CALL FMCFMD2 (IYR, FMD)        

C----------
C  CALCULATE AND PRINT THE POTENTIAL FLAME LENGTH REPORT
C----------
csb: move to end of routine
C      CALL FMPOFL (IYR, FMD, LNMOUT)
C----------
C  CHECK WHETHER A FIRE IS SCHEDULED FOR THIS YEAR (KEYWORD SIMFIRE)
C  IF NOT, OR IF THE COVER TYPE IS 0 (NO TREES IN THE STAND), THEN
C  RETURN.
C----------
      ICOND = 0
      CALL OPFIND(1,MYACTS(2),ITODO)
      IF (ITODO.GT.0) THEN
        DO 400 IFT = 1,ITODO
          CALL OPGET(IFT,6,JYR,IACTK,NPRM,CPRMS)
C          IF (JYR .NE. IYR) GO TO 400
          CALL OPDONE (IFT,IYR)
C----------
C  GET THE SIMFIRE PARAMETER VALUES.
C----------
          SWIND = CPRMS(1)
          FMOIS = INT(CPRMS(2))
          ATEMP = INT(CPRMS(3))
          MKODE = NINT(CPRMS(4))
          PSBURN = CPRMS(5)
          BURNSEAS = NINT(CPRMS(6))
          ICOND = 1
  400   CONTINUE
      ENDIF
C----------
C  SET THE DEFAULT VALUES FOT THE SIMFIRE VARIABLES
C  IF THE KEYWORD WAS NOT USED.
C----------
      IF (ICOND .EQ. 0) THEN
        SWIND = 20
        ATEMP = 70
        FMOIS = 1
        MKODE = 1
        PSBURN = 100.
        BURNSEAS = 1
      ENDIF
C----------
C  IF NO FIRES ARE SCHEDULED OR NO TREES ARE IN THE STAND, AND
C  IF FUEL BURNING HAS NOT BEEN DONE THEN CALL THE ROUTINE WHICH
C  PRINTS THE FIRE REPORTS JUST TO CHECK IF THE FIRST YEAR OF
C  A POTENTIAL FIRE PRINTING WAS SCHEDULED. AFTERWARDS, WHETHER
C  FUEL BURNING WAS DONE OR NOT, RETURN.
C----------
      IF (DEBUG) WRITE (JOSTND,10) ICOND,COVTYP,LFLBRN
   10 FORMAT (' FMBURN, ICOND=',I3,' COVTYP=',I4,
     &        ' LFLBRN=',L2)
C
      IF (ICOND .EQ. 0 .OR. COVTYP .LE. 0) THEN
        IF (.NOT. LFLBRN) THEN
C----------
C  THIS CALL DOES NOT CREATE AN OUTPUT...IT IS USED TO
C  INSURE THAT OPTIONS ARE PROCESSED.
C----------
          CALL FMFOUT(IYR,0.0,0,-1,'  NONE  ')
        ENDIF
        RETURN
      ENDIF
C----------
C  NOW CHECK FOR THE FLAMEADJ KEYWORD.
C----------
      IFTYPE = 0
      CALL OPFIND(1,MYACTS(3),JTODO)
      IF (JTODO.GT.0) THEN
        DO 405 IFC = 1,JTODO
          CALL OPGET(IFC,4,JYR,IACTK,NPRM,FPRMS)
C          IF (JYR .NE. IYR) GO TO 405
          CALL OPDONE (IFC,IYR)
C
          FLAME  = FPRMS(2)
          FLMULT = FPRMS(1)
          CRBURN = FPRMS(3)
          IF (NPRM.GE.4) THEN
            SCH  = FPRMS(4)
          ELSE
            SCH  = -1.
          ENDIF
          IF (CRBURN.GT.-1.) THEN
            CRBURN=CRBURN*.01
          ENDIF
          CFTMP='USER_DEF'
          IFTYPE = 1
          USRFL=.FALSE.
          IF (FLAME .GT. 0.0) USRFL=.TRUE.
  405   CONTINUE
      ENDIF
C
      IF (IFTYPE.EQ.0) THEN
        FLAME  = -1.0
        FLMULT =  1.0
        CRBURN = -1.0
        SCH    = -1.0
        USRFL=.FALSE.
        CFTMP='*NOT_SET'
      ENDIF
C----------
C  NOW CHECK FOR THE MOISTURE PARAMETERS.
C----------
      CALL OPFIND(1,MYACTS(1),KTODO)
      IF (KTODO.GT.0) THEN
        DO 415 JDO = 1,KTODO
          CALL OPGET(JDO,7,JYR,IACTK,NPRM,MPRMS)
C          IF (JYR .NE. IYR) GO TO 415
          CALL OPDONE (JDO,IY(ICYC))
          FMOIS = 0
          MOIS(1,1) = MPRMS(1)*.01
          MOIS(1,2) = MPRMS(2)*.01
          MOIS(1,3) = MPRMS(3)*.01
          MOIS(1,4) = MPRMS(4)*.01
          MOIS(1,5) = MPRMS(5)*.01
          MOIS(2,1) = MPRMS(6)*.01
          MOIS(2,2) = MPRMS(7)*.01
  415   CONTINUE
      ENDIF
C----------
C  BEGIN ROUTINE:
C  SELECT FUEL MOISTURE VALUES IF USER DID NOT ENTER THEM.
C----------
      CALL FMMOIS(FMOIS, MOIS)
C----------
C  MODIFY WIND SPEED BASED ON CROWN CANOPY CLOSURE:
C----------
      WMULT=ALGSLP(PERCOV,CANCLS,CORFAC,4)
      FWIND = SWIND * WMULT
C----------
C  IN CR/UT/LS/SN/CS, FIND FUEL MODEL AGAIN, NOW THAT WIND SPEED AND FUEL
C  MOISTURES ARE KNOWN.
C  TT ADDED TO THIS SINCE THE EXPANDED VARIANT NOW MODELS JUNIPER. 06/03/10
C----------
      IF (IFLOGIC .EQ. 0) THEN
        CALL VARVER(VVER)
        IF (VVER(1:2) .EQ. 'UT' .OR.
     &      VVER(1:2) .EQ. 'TT' .OR.
     &      VVER(1:2) .EQ. 'SM' .OR.
     &      VVER(1:2) .EQ. 'SP' .OR.
     &      VVER(1:2) .EQ. 'BP' .OR.
     &      VVER(1:2) .EQ. 'SF' .OR.
     &      VVER(1:2) .EQ. 'LP' .OR.
     &      VVER(1:2) .EQ. 'LS' .OR.
     &      VVER(1:2) .EQ. 'CS' .OR.
     &      VVER(1:2) .EQ. 'SN') CALL FMCFMD (IYR, FMD)
      ENDIF
C
      IF (DEBUG) WRITE (JOSTND,60) SWIND,FWIND,WMULT,PERCOV
   60 FORMAT (' FMBURN, SWIND=',F7.3,' FWIND=',F7.3,' WMULT=',F7.3,
     >        ' PERCOV=',F7.3)
C----------
C     IF THE USER SPECIFIED SCH, FLAME, AND CRBURN, THEN SIMPLY BRANCH
C     TO THE FIRE EFFECTS CALCULATIONS.
C----------
      IF (SCH.GT.-1. .AND. FLAME.GT.-1. .AND. CRBURN.GT.-1.) THEN
         CFTMP='USER_DEF'
         IF (SCH .GT. PBSCOR) BURNYR = IYR
         IF (CRBURN .GT. 0) THEN
           IF (CRBURN .LT. 100.0) THEN
             FIRTYPE = 2 ! PASSIVE
           ELSE
             FIRTYPE = 1 ! active
           ENDIF
         ELSE
           FIRTYPE = 3 ! surface
         ENDIF
         GOTO 490
      ENDIF
C----------
C  IF THE FLAME LENGTH WAS NOT ENTERED BY THE USER:
C----------
      OLDFL = 0.0
      BYRAM = 0.0
C
      IF (FLAME .LE. 0.0) THEN
C----------
C  COMPUTE BYRAM'S FIRELINE INTENSITY, GET THE FLAME LENGTH
C----------
        CALL FMFINT(IYR, BYRAM, FLAME, 1, HPA, 1)
        CALL fvsGetRtnCode(IRTNCD)
        IF (IRTNCD.NE.0) RETURN
        OLDFL = FLAME
C----------
C  A FLAME LENGTH MULTIPLIER WAS ADDED, SO FLAME IS MODIFIED
C----------
        IF (FLMULT .NE. 1.0) FLAME = OLDFL * FLMULT
      ELSE
C----------
C  WE NEED TO CALL FMFINT ANYWAYS TO GET SOME INFORMATION FOR CALCULATING
C  THE TYPE OF FIRE AND THE AMOUNT OF CROWN BURNED.
C----------
        CALL FMFINT(IYR, BYRAM, OLDFL, 1, HPA, 1)
        CALL fvsGetRtnCode(IRTNCD)
        IF (IRTNCD.NE.0) RETURN
        OLDFL = 0.0
      ENDIF
C----------
C  FLAME LENGTH HAS BEEN MODIFIED FROM THAT CALCULATED BY FMFINT
C  OR WAS ENTERED BY THE USER SO
C  WE NEED TO (RE)DETERMINE BYRAM SO CAN CALCULATE SCH
C----------
      IF (FLAME .NE. OLDFL) THEN
C----------
C  (RECALCULATION BASED ON LINE IN FMFINT WHICH SAYS:
C  FLAME = 0.45 * (BYRAM/60.0)**(0.46)
C----------
        BYRAM = 60.0 * ((FLAME / 0.45) ** (1.0 / 0.46))
      ENDIF
C----------
C  CALCULATE SCORCH HEIGHT (IN FEET)
C  BUT FIRST CONVERT BYRAM TO BTU/FT/SECOND (RATHER THAN MIN)
C----------
      BYRAM = BYRAM / 60.0
      SCH = (63.0 / (140.0 - ATEMP)) * (BYRAM ** (7.0 / 6.0)
     &            / (BYRAM + FWIND ** 3.0) ** 0.5)

      IF (FLAG(1) .EQ. 1) GOTO 500
C----------
C  FIRE OCCURRED
C----------
      IF (SCH .GT. PBSCOR) BURNYR = IYR
C----------
C  DETERMINE THE TYPE OF FIRE AND THE AMOUNT OF CROWN BURNING
C  (IF NOT USER ASSIGNED).
C  CALCULATE THE STAND LOWER CROWN BOTTOM HEIGHT (ACTCBH)
C  AND THE CROWN BULK DENSITY (CBD) SO THAT WE CAN CALCULATE
C  THE FLAME CRITIAL FLAME LENGTH AND THE CRITIAL DENSITY LATER
C----------
      CRRATE = 0
      CRTCBH = 0
C
      IF (DEBUG) WRITE (JOSTND,12) IFTYPE,CRBURN
   12 FORMAT (' FMBURN MAY CALL FMCFIR,IFTYPE=',I2,' CRBURN=',F5.1)
C----------
C  DON'T NEED TO DO CROWN FIRE CALCS IN THE SN/CS FFE.  JUST NEED TO SET
C  CRBURN AND FIRTYPE TO CORRESPOND TO A SURFACE FIRE
C----------
      IF ((VVER(1:2) .EQ. 'SN') .OR. (VVER(1:2) .EQ. 'CS')) THEN
        FIRTYPE = 3
        CRBURN = 0
        CFTMP = 'SURFACE'
      ELSE
        IF ((CRBURN.EQ.-1.) .OR. (.NOT. USRFL)) THEN
C----------
C  NEED TO GO INTO FMCFIR EVEN IF USER SETS % CROWNING
C  IF THEY DIDN'T ENTER FLAME LENGTH BECAUSE VALUES CALCULATED
C  WITHIN THIS ROUTINE ARE NEEDED FOR INTENSITY/FL CALCULATION.
C  MUST SAVE THE USERS % CROWNING  AND FIRE TYPE VALUES THOUGH.
C----------
          UCRBURN = CRBURN
          LCFTMP = .FALSE.
          IF (CFTMP .EQ. 'USER_DEF') LCFTMP = .TRUE.
          CALL FMCFIR(IYR,1,WMULT,INT(SWIND),CFTMP,TMP2,TMP3,HPA)
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN

          IF (UCRBURN .GE. 0) CRBURN = UCRBURN
          IF (LCFTMP) CFTMP = 'USER_DEF'
C
        ELSE
C----------
C  IF FMCFIR ISN'T CALLED, WE NEED TO SET FIRTYPE, SO THAT
C  THE FIRE IS VISUALIZED CORRECTLY IN SVS
C--------
          IF (CRBURN .GT. 0) THEN
            IF (CRBURN .LT. 100.0) THEN
              FIRTYPE = 2 ! PASSIVE
            ELSE
              FIRTYPE = 1 ! active
            ENDIF
          ELSE
            FIRTYPE = 3 ! surface
          ENDIF
C
        ENDIF
      ENDIF ! vver eq sn / cs
C----------
C  MODIFY FLAME LENGTH AND SCORCH HEIGHT TO ACCOUNT FOR
C  CROWN FIRE BEHAVIOR.
C----------
      IF (CRBURN.GT.0.) THEN
        IF (.NOT. USRFL) THEN ! users didn't alter FL
          FINTEN = (HPA+TCLOAD*7744.8*CRBURN)*RFINAL/60.
          FLB=0.45*FINTEN**.46
          FLT=0.2*FINTEN**.667
          FLAME=FLB+CRBURN*(FLT-FLB)
          IF (FLMULT .NE. 1.0) FLAME = FLAME * FLMULT
        ENDIF

        IF (USRFL .OR. FLMULT.NE.1.0) THEN
C----------
C  IF USER SETS FLAME LENGTH OR FLAME LENGTH MULTIPLIER,
C  INTENSITY AND SCORCH HEIGHT NEED TO BE BASED ON THAT.
C----------
          TMPINT = CRBURN*((FLAME/.2)**(1/.667))
     &        + (1-CRBURN)*((FLAME/.45)**(1/.46))
          MAXINT = MAX((FLAME/.21)**(1/.667), (FLAME/.45)**(1/.46))
          MININT = MIN((FLAME/.21)**(1/.667), (FLAME/.45)**(1/.46))
C
          DO 200 I=1,200
            FLB=0.45*TMPINT**.46
            FLT=0.2*TMPINT**.667
            TMPFLAME=FLB+CRBURN*(FLT-FLB)
C
            IF (TMPFLAME .GE. FLAME*0.99 .AND.
     &          TMPFLAME .LE. FLAME*1.01) THEN
              FINTEN = TMPINT
              GOTO 205
            ELSEIF (TMPFLAME .LT. FLAME) THEN
              MININT = TMPINT
              TMPINT = .5*TMPINT + .5*MAXINT
            ELSE
              MAXINT = TMPINT
              TMPINT = .5*TMPINT + .5*MININT
            ENDIF
            IF (I .EQ. 200) FINTEN = TMPINT
C
  200     CONTINUE
  205     CONTINUE
C
        ENDIF
        SCH = (63.0 / (140.0 - ATEMP)) * (FINTEN ** (7.0 / 6.0)
     &        / (FINTEN + FWIND ** 3.0) ** 0.5)
      ENDIF
  490 CONTINUE
C----------
C  CALL FMCBA IF A FIRE OCCURED AND USING THE SN VARIANT SO THAT LIVE FUELS
C  (HERB AND SHRUB) VALUES CAN BE UPDATED SINCE THEY ARE BASED ON TIME
C  SINCE LAST FIRE.
C----------
      IF (BURNYR .EQ. IYR .AND. VVER(1:2) .EQ. 'SN') THEN
        CALL FMCBA (IYR,0)
      ENDIF
C----------
C  CALCULATE SOIL HEATING USING FOFEM
C----------
      CALL FMSOILHEAT(IYR, LNMOUT)
C----------
C  CALCULATE PROBABILITY OF TREE MORTALITY, AND REDUCE THE FM TREELIST
C----------
      PMRT = 0.0
      CALL FMEFF(IYR, FMD, FLAME, 0, PMRT, PVOLKL, MKODE,PSBURN)
C----------
C  CALCULATE FUEL CONSUMPTION AND SMOKE PRODUCED
C----------
      PSMOKE = 0.0
      CALL FMCONS(FMOIS, 0, 0.0, IYR, 0, PSMOKE,PSBURN)

  500 CONTINUE          ! TARGET IF NO FIRE BURNED.
C----------
C  PRINT FIRE REPORTS
C  NOTE THAT IF FUEL BURNING WAS DONE AS WELL, THE FLAG
C  IS CHANGED FROM A 1 (STAND FIRE ONLY) TO A 2 (STAND AND
C  TREATMENT FIRE).
C----------
      IFIRE = 1
      IF (LFLBRN) IFIRE = 2
C
      IF (DEBUG) WRITE (JOSTND,20) FLAME,FMD,IFIRE,CFTMP
   20 FORMAT (' FMBURN CALLING FMFOUT, FLAME=',F7.2,
     >        ' FMD=',I7,' IFIRE=',I2,' CFTMP=',A)
C
      IF (LNMOUT) CALL FMFOUT(IYR, FLAME, FMD, IFIRE, CFTMP)
C
      RETURN

      END
