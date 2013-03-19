      SUBROUTINE FMCONS(FMOIS,BTYPE,PLAREA,IYR,ICALL,PSMOKE,PSBURN)
      IMPLICIT NONE
C
C  $Id$
C
C----------
C  **FMCONS   FIRE-BC
C----------
*     CALLED FROM: FMBURN
*                  FMPOFL
*  PURPOSE:
*     THIS ROUTINE CALCULATES THE AMOUNT OF DYNAMIC FUELS CONSUMED,
*     THE AMOUNT OF SMOKE PRODUCED, AND THE MINERAL SOIL EXPOSURE.
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FMOIS: MOISTURE MODEL IN USE (IF ANY)
*     BTYPE: 0 FOR NORMAL FIRE, 1 FOR PILES
*     PLAREA: PROPORTION OF TOTAL AREA COVERED IN PILES, USED ONLY
*             WHEN BTYPE IS 1.  R&C 07/09/96
*     ICALL:  0=ACTUALLY CONSUME THE FUELS, 1=POTENTIAL CONSUMPTION
*     PSMOKE: POTENTIAL SMOKE EMISSIONS <2.5
*     PSBURN:  PERCENTAGE OF STAND THAT IS BURNED 
*
*  LOCAL VARIABLE DEFINITIONS:
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
C      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'ESHAP.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C.... VARIABLE DECLARATIONS.

      INTEGER FMOIS, I, J, K, L, P, H, D
      INTEGER BTYPE
      REAL    PDIA(6), EMMFAC(3,MXFLCL,2,2), EMFACL(4,2)
      REAL    BURNZ(2,MXFLCL), PRMS(1)
      REAL    PRBURN(2,MXFLCL), TSMOKE, PLVBRN(2)
      LOGICAL DEBUG
      INTEGER ICALL,IYR,IL,KODE,IM,IP,IPM
      REAL    PSMOKE,PLAREA,CONS,DIARED,PRDUF,PSBURN
      REAL    ACTWFC, ACTFFC, FFC, WFC, BUI, TA2KGM2
      REAL    DMC, PLGBURN

C     PDIA IS MIDPOINT (APPROX) OF 6 LARGEST SIZE CLASSES (3-6, 6-12,
C     12-20, 20-35, 35-50 INCHES)
C
      DATA PDIA / 4.0, 8.0, 15.0, 15.0, 15.0, 15.0 /

      DATA EMMFAC /3*7.9,3*7.9,3*11.9,
     &             22.5,18.3,16.2,22.5,18.3,16.2,22.5,18.3,16.2,
     &             22.5,18.3,16.2,22.5,18.3,16.2,22.5,18.3,16.2,     
     &             3*7.9,23.9,25.8,25.8,
     &             33*17.0,
     &             3*9.3,3*9.3,3*14.0,
     &             26.6,21.6,19.1,26.6,21.6,19.1,26.6,21.6,19.1,
     &             26.6,21.6,19.1,26.6,21.6,19.1,26.6,21.6,19.1,
     &             3*9.3,28.2,30.4,30.4,
     &             33*20.0/

      DATA EMFACL /4*21.3,4*25.1/
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCONS',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,3) ICYC,ICALL
    3 FORMAT(' ENTERING ROUTINE FMCONS CYCLE = ',I2,' ICALL=',I2)
C
C     INITIALIZE THE EVENT MONITOR VARIABLES (FROM **EVTSTV**)
C     420 FIRE      0 IF STAND HAS NO FIRE, 1 IF FIRE OCCURS (FM)
C     423 FIREYEAR  CALENDAR YEAR OF LAST FIRE; -1 INITIALLY (FM)
C
      IF (ICALL .EQ. 0) THEN
        CALL EVSET4(20,1.0)
        CALL EVSET4(23,FLOAT(IYR))
      ENDIF

C     MULTIPLIER FOR CHANGING TONS/ACRE TO KG/M2
C      TA2KGM2 = (TItoTM / ACRtoHA) * (1000. / 10000.)
      TA2KGM2 = (0.90718 / 0.4046945) * (1000. / 10000.)

C      BD = 80.0
C      DC = 300
      BUI = 90

C     TRANSFORM DUFF % MOISTURE CONTENT INTO DUFF MOISTURE CODE
C     MC=exp((244.7 - DMC)/43.4) + 20
C     DMC = 244.7 - 43.4*log(MC-20)
      IF (MOIS(1,5) .GT. .20) THEN
        DMC = 244.7 - 43.4*LOG(100*MOIS(1,5)-20)
      ELSE
        DMC = 244.7 - 43.4*LOG(.5)
      ENDIF
      IF (DMC .LE. 0) DMC = 0

      IF (DMC .LE. 0.4*CFIM_DC) THEN
        BUI = 0.8*DMC*CFIM_DC / (DMC + 0.4*CFIM_DC)
      ELSE
        BUI = DMC - ((1 - (0.8*CFIM_DC) / (DMC + 0.4*CFIM_DC)) * 
     &              (0.92 + (0.0114*DMC)**1.7))
      ENDIF 
C
C     Map CWD into the BURNZ array. BURNZ holds the aggregated values before
C     a burn, and is used to calculate consumed amounts, and for disaggregating
C     the consumed amounts back into the CWD array.
C
C     Note the 'fudge' here.  If this routine is called from a 'natural' or
C     perscribed fire (BTYPE=0), only UNPILED fuel will burn.  If this routine
C     is called from the fuel treatment routine (BTYPE=1), only PILED fuels
C     will burn, so BURNZ(1,x) is set to 0
C
      DO I = 1, 2
        DO J = 1, MXFLCL
          BURNZ(I,J) = 0.0
          PRBURN(I,J) = 0.0
          IF ((BTYPE .EQ. 0 .AND. I .EQ. 1) .OR.
     &      (BTYPE .EQ. 1 .AND. I .EQ. 2)) THEN
            DO K = 1, 2
              DO L = 1, 4
                BURNZ(I,J) = BURNZ(I,J) + CWD(I,J,K,L)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO

C     LET'S SEPARATE OUT THE CALCULATIONS:

C     FIRST, BURN UNPILED FUELS, IF A 'NATURAL' OR PRESCRIBED FIRE:

      IF (BTYPE .EQ. 0) THEN

C       CALCULATE CONSUMPTION OF 1-3" CLASS.

        IF ((IYR-HARVYR) .GT. 5) THEN     ! NO ACTIVITY FUELS CASE

C         NATURAL UNPILED FUELS

          PRBURN(1,3) = 0.65

        ELSE
C         ACTIVITY, UNPILED FUELS  (BASE IT ON MOISTURE OF 0.25-1" CLASS)

          IF (MOIS(1,2) .LT. 0.137) THEN
            PRBURN(1,3) = 1.0
          ELSEIF (MOIS(1,2) .GT. 0.34) THEN
C           NO BURNING OCCURS
          ELSE
            CONS = (167.0 - 4.89 * MOIS(1,2) * 100.0) / 100.0
            IF (CONS .LT. 0.0) CONS = 0.0
            IF (CONS .GT. 1.0) CONS = 1.0
            PRBURN(1,3) = CONS
          ENDIF
        ENDIF


C       NOW CALCULATE THE CONSUMPTION OF <3" CLASSES:

        IF (CFIM_ON) THEN
C           IF USING CFIM MODEL, THEN CALCULATE AMOUNT THAT WOULD BURN
            WFC = 1.5 * (1 - EXP(-0.0201 * BUI))

            ACTWFC = BURNZ(1,1) + BURNZ(1,2) + BURNZ(2,1) + BURNZ(2,2)
C           NEED TO CHANGE TO KG/M3
            ACTWFC = ACTWFC * TA2KGM2

            IF (ACTWFC .GT. 0) THEN
                IF (WFC .LE. ACTWFC) THEN
                    PRBURN(1,1) = WFC / ACTWFC
                    PRBURN(1,2) = WFC / ACTWFC
                ELSE
                    PRBURN(1,1) = 1.0
                    PRBURN(1,2) = 1.0
                ENDIF
            ELSE
                PRBURN(1,1) = 0.0
                PRBURN(1,2) = 0.0
            ENDIF
            CFIM_INPUT(25) = PRBURN(1,1) * ACTWFC

        ELSE
            IF (BURNZ(1,3) .GT. 0.0) THEN
              IF (PRBURN(1,3) .GT. 0.9) THEN
C               BURN ALL OF THE UNPILED FUELS
                PRBURN(1,1) = 1.0
                PRBURN(1,2) = 1.0
              ELSE
C               BURN 90% OF THE UNPILED FUELS
                PRBURN(1,1) = 0.9
                PRBURN(1,2) = 0.9
              ENDIF
            ELSE
              PRBURN(1,1) = 1.0
              PRBURN(1,2) = 1.0
            ENDIF
        ENDIF

C        NEXT DO THE FUELS > 3 INCHES

        PLGBURN = 0.0
        DO IL= 4,9

          IF ((IYR-HARVYR) .GT. 5) THEN
C           NATURAL, UNPILED
            DIARED = 3.38 - 0.027 * MOIS(1,4) * 100.0
            IF (MOIS(1,4) .GT. 1.25 .OR. DIARED .LT. 0.) DIARED = 0.0
            CONS = 1.0 - ((PDIA(IL-3) - DIARED) / PDIA(IL-3))**2

          ELSE
C           ACTIVITY FUELS, UNPILED
            DIARED = 4.35 - 0.096 * MOIS(1,4) * 100.0
            IF (MOIS(1,4) .GT. 0.45 .OR. DIARED .LT. 0.) DIARED = 0.0
            CONS = 1.0 - ((PDIA(IL-3) - DIARED) / PDIA(IL-3))**2
          ENDIF
          PRBURN(1,IL) = CONS

          PLGBURN = PLGBURN + CONS*BURNZ(1,IL)*TA2KGM2
        ENDDO


C       NOW FOR DUFF:
C           note that PRDUFF is a percentage
C           If the moisture value is 2 or larger, PRDUF will be negative.

        PRDUF = 83.7 - 0.426 * MOIS(1,5) * 100.0
        IF (PRDUF .LT. 0.0) PRDUF = 0.0
        PRBURN(1,11) = PRDUF / 100.0
        IF (PRBURN(1,11) .GT. 1.0) PRBURN(1,11) = 1.0

        EXPOSR = 0.0
        EXPOSR = -8.98 + 0.899 * PRDUF
        IF (PRDUF .LT. 10.0) EXPOSR = 0.0

C       AND LITTER (100% OF LITTER IS BURNED):

        PRBURN(1,10) = 1.0

C       IF USING CFIM MODEL, THEN CALCULATE NEW PRBURN VALUES
C       FOR LITTER AND DUFF
        IF (CFIM_ON) THEN
            FFC = -.216 * .009*CFIM_BD + 0.014*BUI
            ACTFFC = BURNZ(1,11)+BURNZ(1,10)+ BURNZ(2,11)+BURNZ(2,10)
            ACTFFC = ACTFFC * TA2KGM2

            IF (ACTFFC .GT. 0) THEN
                IF (ACTFFC .GE. FFC) THEN
                    PRBURN(1,11) = FFC / ACTFFC
                    PRBURN(1,10) = PRBURN(1,11)
                ELSE
                    PRBURN(1,11) = 1.0
                    PRBURN(1,10) = 1.0
                ENDIF
            ELSE
                PRBURN(1,11) = 0.0
                PRBURN(1,10) = 0.0
            ENDIF
            CFIM_INPUT(25) = CFIM_INPUT(25) + PRBURN(1,11) * ACTFFC

        ENDIF

C       temporarily print intermediate output 
        IF (CFIM_ON) THEN
            POTCONS(2,1) = PRBURN(1,1) * ACTWFC * 10
            POTCONS(2,2) = PLGBURN  * 10
            POTCONS(2,3) = PRBURN(1,11) * ACTFFC  * 10
            WRITE(JOSTND,333) IYR, POTCONS(2,1), PLGBURN, POTCONS(2,3)
  333       FORMAT(' YEAR POTCONS: SMALL, LARGE, DUFF+LITTER (T/HA)',
     >         I4,2X,3(F6.3,2X))
            POTCONS(2,1) = PRBURN(1,1)
            POTCONS(2,3) = PRBURN(1,11)
            POTCONS(2,2) = 0.0
            PLGBURN = 0.0
            DO IL= 4,9
              POTCONS(2,2) = POTCONS(2,2) + PRBURN(1,IL) * BURNZ(1,IL)
              PLGBURN = PLGBURN + BURNZ(1,IL)
            ENDDO
            IF (PLGBURN .GT. 0.0) THEN
                POTCONS(2,2) = POTCONS(2,2) / PLGBURN
            ELSE
                POTCONS(2,2) = 0.0
            ENDIF
        ENDIF       

C       NOW BURN LIVE FUELS

        PLVBRN(1) = 1.0
        PLVBRN(2) = 0.6

        DO I = 1,MXFLCL
          PRBURN(1,I) = PRBURN(1,I)*PSBURN/100
          IF (I .LE. 2) PLVBRN(I) = PLVBRN(I)*PSBURN/100
        ENDDO
        EXPOSR = EXPOSR*PSBURN/100
        
****************
      ELSE   ! BTYPE is 1.
****************

C       BURN PILED FUELS

        PRBURN(2,1) = 1.0
        PRBURN(2,2) = 1.0

        DO IL= 3,9
          PRBURN(2,IL) = 0.9
        ENDDO

        PRBURN(2,11) = PLAREA

C       AND LITTER (100% OF LITTER UNDER THE PILES IS BURNED)

        PRBURN(2,10) = PLAREA
        
C       NO LIVE FUELS BURN 
        PLVBRN(1) = 0
        PLVBRN(2) = 0

C       Set the exposure amount to be equal to the area covered with piles.
C       R&C 7/9/96.

        EXPOSR = PLAREA * 100.

      ENDIF  ! **** THE BIG ENDIF ****


      IF (ICALL .NE. 1) THEN

C     INITIALIZE THE EVENT MONITOR VARIABLES (FROM **EVTSTV**)
C     421 MINSOIL   PERCENTAGE OF MINERAL SOIL EXPOSURE (FM)

        CALL EVSET4(21,EXPOSR)

C       Schedule a "Tally" for the Estab model...a fire is a disturbance
C       so the year of the fire is the year of the disturbance.  Also
C       Set up a "BurnPrep" (activity code 491) with the EXPOSR as the
C       percent burnprep.

        IF (LAUTAL) THEN
          PRMS(1)=EXPOSR
          CALL OPADD (IYR,491,0,1,PRMS,KODE)
          PRMS(1)=FLOAT(IYR)
          CALL OPADD (IYR,427,0,1,PRMS,KODE)
          CALL OPINCR (IY,ICYC,NCYC)
        ENDIF

C       Adjust the CWD array to reflect the proportional burning loss as
C       recorded in PRBURN. If the BURNZ pool is zero, then the parent
C       WD pool is also zero, and should be skipped. This also avoids
C       trapping zero-division.  Also, calculate the BURNED values for
C       use in fuel consumption reports later.

C       First make a compressed copy of the Fuel Load to TCWD.

        TCWD(1) = 0.
        TCWD(2) = 0.
        TCWD(3) = 0.
        TCWD(4) = 0.
        TCWD(5) = 0.
        TCWD(6) = 0.

        DO P=1,2
          DO H=1,2
            DO D=1,4
              TCWD(1)=TCWD(1)+CWD(P,10,H,D)
              TCWD(2)=TCWD(2)+CWD(P,11,H,D)
              TCWD(3)=TCWD(3)+CWD(P,1,H,D)+CWD(P,2,H,D)+CWD(P,3,H,D)
              TCWD(4)=TCWD(4)+CWD(P,4,H,D)
              TCWD(5)=TCWD(5)+CWD(P,5,H,D)
              TCWD(6)=TCWD(6)+CWD(P,6,H,D)+CWD(P,7,H,D)+CWD(P,8,H,D)
     &                       +CWD(P,9,H,D) 
            ENDDO
          ENDDO
        ENDDO

        I = 1
        IF (BTYPE .EQ. 1) I = 2
        DO J = 1, MXFLCL
          IF (BURNZ(I,J) .GT. 0.0) THEN
            BURNED(I,J) = BURNZ(I,J) * PRBURN(I,J)
            BURNED(3,J) = BURNED(3,J) + BURNED(I,J)
            DO K = 1, 2
              DO L = 1, 4
C
C               NOW MODIFY IT TO REFLECT PRBURN
C
                CWD(I,J,K,L) = CWD(I,J,K,L) * (1.0 - PRBURN(I,J))
              ENDDO
            ENDDO
          ENDIF
        ENDDO

        BURNLV(1) = PLVBRN(1) * FLIVE(1)
        BURNLV(2) = PLVBRN(2) * FLIVE(2)

      ENDIF

C     NOW CAN DO SMOKE PRODUCTION

C     IN THIS CASE, WE NEED TO CHOOSE AN AVERAGE MOISTURE LEVEL.
C     LETS CHOOSE IT BASED ON THE LARGE WOOD MOISTURE (SINCE THOSE
C     EMMISSION VALUES CHANGE MORE)

      IF (MOIS(1,4) .LE. .20) THEN
        IM = 3
      ELSEIF (MOIS(1,4) .LE. .375) THEN
        IM = 2
      ELSE
        IM = 1
      ENDIF

c     IPM: smoke size; IP: unpiled/piled; IL: fuel class; IM: moisture type

      IP = 1
      IF (BTYPE .EQ. 1) IP = 2

      DO IPM=1,2
        TSMOKE = 0.0
        DO IL=1,MXFLCL
          TSMOKE = TSMOKE + PRBURN(IP,IL) * BURNZ(IP,IL)
     &             * EMMFAC(IM,IL,IP,IPM)
     
        IF (DEBUG) WRITE(JOSTND,9) IM,IL,IP,IPM,EMMFAC(IM,IL,IP,IPM)
    9   FORMAT('  FMCONS IM=',I2,' IL=',I2,' IP=',I2,' IPM=',I2,
     &                   ' EMMFAC=',F5.1)

        ENDDO

C       Only calculate smoke from live stuff if it is a 'proper' fire.
C       We aren't burning live stuff from a fuel treatment fire.

        IF (BTYPE .EQ. 0) THEN
          DO IL=1,2
            TSMOKE = TSMOKE + PLVBRN(IL) * FLIVE(IL) * EMFACL(IL,IPM)
          ENDDO
          IF (ICALL .EQ. 0) THEN
            TSMOKE = TSMOKE + BURNCR * EMFACL(4,IPM)
          ELSE
            TSMOKE = TSMOKE + PBRNCR * EMFACL(4,IPM)
          ENDIF
        ENDIF

        IF (ICALL .EQ. 0) THEN
           SMOKE(IPM) = SMOKE(IPM) + TSMOKE
        ELSEIF (ICALL .EQ. 1 .AND. IPM .EQ. 1) THEN
           PSMOKE = TSMOKE
        ENDIF

      ENDDO

      IF (DEBUG) WRITE(JOSTND,30) ICYC,PSMOKE
   30 FORMAT(' FMCONS CYCLE = ',I2,' PSMOKE=',F10.4)

      RETURN
      END
