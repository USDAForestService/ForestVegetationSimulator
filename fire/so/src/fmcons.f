      SUBROUTINE FMCONS(FMOIS,BTYPE,PLAREA,IYR,ICALL,PSMOKE,PSBURN)
      IMPLICIT NONE
C----------
C  **FMCONS   FIRE--SO--DATE OF LAST REVISION:  12/09/10
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
      REAL    HFCORR, FMCORR, XDIA(4), XDIA2(4), YDIA(4), DIARED, DFRED
      LOGICAL DEBUG

      INTEGER ICALL,IYR,IL,KODE,IM,IP,IPM
      REAL    PSMOKE,PLAREA,PRDUF,ALGSLP,XDIA4,XCONS,PSBURN
C
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
      DATA XDIA  / 0., 75., 85., 100. / ! FOR INTERPOLATING ACTIVITY FUELS
      DATA XDIA2 / 0.,  1.,  2., 999. / ! FOR INTERPOLATING ACTIVITY FUELS
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
C
C     Map CWD into the BURNZ array. BURNZ holds the aggregated values before
C     a burn, and is used to calculate consumed amounts, and for disaggregating
C     the consumed amounts back into the CWD array.
C
C     Note the 'fudge' here.  If this routine is called from a 'natural' or
C     prescribed fire (BTYPE=0), only UNPILED fuel will burn.  If this routine
C     is called from the fuel treatment routine (BTYPE=1), only PILED fuels
C     will burn, so BURNZ(1,x) is set to 0
C
      DO I = 1, 2
        DO J = 1, MXFLCL
          BURNZ(I,J) = 0.0
          PRBURN(I,J) = 0.0
          IF ((BTYPE .EQ. 0 .AND. I .EQ. 1) .OR.
     &        (BTYPE .EQ. 1 .AND. I .EQ. 2)) THEN
            DO K = 1, 2
              DO L = 1, 4
                BURNZ(I,J) = BURNZ(I,J) + CWD(I,J,K,L)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C     LET'S SEPARATE OUT THE CALCULATIONS:
C
C     FIRST, BURN UNPILED FUELS, IF A 'NATURAL' OR PRESCRIBED FIRE:
C
      IF (BTYPE .EQ. 0) THEN

        IF ((IYR-HARVYR) .GT. 5) THEN ! NO ACTIVITY FUELS CASE

          IF (BURNZ(1,3) .GT. 0.) THEN
             PRBURN(1,1) = 0.9
             PRBURN(1,2) = 0.9
          ELSE
             PRBURN(1,1) = 1.0
             PRBURN(1,2) = 1.0
          ENDIF

          DO IL= 4,9          ! LARGER NATURAL UNPILED
            DIARED = 3.38 - 0.027 * MOIS(1,4) * 100.
            IF (MOIS(1,4) .GT. 1.25) DIARED = 0.
            PRBURN(1,IL) = 1.0 - ((PDIA(IL-3) - DIARED)
     >           / PDIA(IL-3))**2
          ENDDO

          PRBURN(1,10) = 1.0   ! LITTER

          PRDUF = 83.7 - 0.426 * MOIS(1,5) * 100. ! DUFF
          IF (PRDUF .LT. 0.0) PRDUF = 0.0
          PRBURN(1,11) = PRDUF / 100.
          IF (PRBURN(1,11) .GT. 1.) PRBURN(1,11) = 1.


        ELSE                   ! ACTIVITY FUELS CASE

          PRBURN(1,1) = 1.    ! ALL 1, 10 HR FUELS ARE CONSUMED
          PRBURN(1,2) = 1.

          HFCORR = 0.5 * BURNZ(1,3) * (1. + (100.* FMSLOP - 20.)/60. +
     >         (0.25 * FWIND))
          FMCORR = 0.03 * LOG(MAX(HFCORR,1.E-9)) / LOG(2.)
          PRBURN(1,3) = 0.9 - ((100.*MOIS(1,2)) - FMCORR - 12.) ! 100 HR
     >           * 0.0535
          PRBURN(1,3) = MAX(0.,MIN(PRBURN(1,3),1.)) ! PRBURN 0<=X<=1

          DO IL= 4,9          ! 1,000, 10,000 HR FUELS

            IF (MOIS(1,4) .GT. 0.6) THEN
              DIARED = -0.005  * (MOIS(1,4)*100.) + 0.731
            ELSEIF (MOIS(1,4) .GE. 0.44 .AND. MOIS(1,4) .LE. 0.6)THEN
              DIARED = -0.0178 * (MOIS(1,4)*100.) + 1.489
            ELSE             ! INTERPOLATE
              YDIA(1) = -0.096 * (MOIS(1,4)*100.) + 4.6495
              YDIA(2) = YDIA(1)
              YDIA(3) = -0.125 * (MOIS(1,4)*100.) + 6.27
              YDIA(4) = YDIA(3)
              DIARED  = ALGSLP(PRBURN(1,3),XDIA,YDIA,4)
c                  uses 0-1 scaled prburn: OK??
            ENDIF

            IF (MOIS(1,2) .LE. 0.15) THEN ! MODIFY DIARED IN SOME
              IF (MOIS(1,4) .LE. 0.4) THEN ! CASES
                DIARED = DIARED * (1. - 0.22)
              ELSEIF (MOIS(1,4) .GT. 0.4 .AND. MOIS(1,4).LE.0.5) THEN
                DIARED = DIARED * (1. - 0.11)
              ENDIF
            ENDIF

            SELECT CASE (IL)
              CASE (4)          !  1,000 HR FUEL
              PRBURN(1,IL) = 1. - (( 5.2 - DIARED)/5.2)**2
              XDIA4 = MAX(0.,DIARED) ! SAVE DIARED FOR DUFF
              CASE (5,6,7,8,9)        ! 10,000 HR FUEL
              PRBURN(1,IL) = 1. - ((13.7 - DIARED)/5.2)**2
            END SELECT

            PRBURN(1,IL) = MAX(0.,MIN(PRBURN(1,IL),1.)) ! PRBURN 0<=X<=1

          ENDDO            ! END OF LARGER FUEL CONSUMPTION (>=1000 HRS; IL=4-9)

          PRBURN(1,10) = 1.0       ! ALL LITTER IS CONSUMED (SAME AS NI)

          IF (MOIS(1,5) .GE. 2.) THEN ! DUFF
            XCONS = 0.
            DO IL=4,9
              XCONS = XCONS + (PRBURN(1,IL) * BURNZ(1,IL))
            ENDDO
            DFRED = 0.537 + (0.57 * XCONS)
          ELSEIF (MOIS(1,5) .GE. 1.25 .AND. MOIS(1,5) .LT. 2.) THEN
            DFRED = 0.323 + 1.034 + SQRT(XDIA4)
          ELSEIF (MOIS(1,5) .GE.  0.5 .AND. MOIS(1,5) .LT. 1.25) THEN
            DFRED = 1.323 + 1.034 + SQRT(XDIA4)
          ELSE
            DFRED = 2.323 + 1.034 + SQRT(XDIA4)
          ENDIF

          YDIA(1) = DFRED * 0.5   ! INTERPOLATE DUFF REDUCTION
          YDIA(2) = YDIA(1)
          YDIA(3) = DFRED * 0.75
          YDIA(4) = YDIA(3)
          DFRED = ALGSLP(DEPTH, XDIA2, YDIA, 4)

          PRBURN(1,11) = (DFRED * 12.1) / BURNZ(1,11)
          PRBURN(1,11) = MAX(0.,MIN(PRBURN(1,11),1.)) ! PRBURN 0<=X<=1
          PRDUF = PRBURN(1,11) * 100.

        ENDIF                     ! NON-ACTIVITY FUELS / ACTIVITY FUELS

        EXPOSR = 0.0              ! SAME AS NI
        EXPOSR = -8.98 + 0.899 * PRDUF

        IF (PRDUF .LT. 10.0) EXPOSR = 0.0

        PLVBRN(1) = 1.0           ! SAME AS NI
        PLVBRN(2) = 0.6           ! SAME AS NI

        DO I = 1,MXFLCL
          PRBURN(1,I) = PRBURN(1,I)*PSBURN/100
          IF (I .LE. 2) PLVBRN(I) = PLVBRN(I)*PSBURN/100
        ENDDO
        EXPOSR = EXPOSR*PSBURN/100

****************
      ELSE                      ! BTYPE is 1.
****************
C
C       BURN PILED FUELS
C
        PRBURN(2,1) = 1.0
        PRBURN(2,2) = 1.0

        DO IL= 3,9
          PRBURN(2,IL) = 0.9
        ENDDO

        PRBURN(2,11) = PLAREA
C
C       AND LITTER (100% OF LITTER UNDER THE PILES IS BURNED)
C
        PRBURN(2,10) = PLAREA
        
C       NO LIVE FUELS BURN 
        PLVBRN(1) = 0
        PLVBRN(2) = 0        
C
C       Set the exposure amount to be equal to the area covered with piles.
C       R&C 7/9/96.
C
        EXPOSR = PLAREA * 100.

      ENDIF  ! **** THE BIG ENDIF ****

      IF (ICALL .NE. 1) THEN
C
C     INITIALIZE THE EVENT MONITOR VARIABLES (FROM **EVTSTV**)
C     421 MINSOIL   PERCENTAGE OF MINERAL SOIL EXPOSURE (FM)
C
        CALL EVSET4(21,EXPOSR)
C
C       Schedule a "Tally" for the Estab model...a fire is a disturbance
C       so the year of the fire is the year of the disturbance.  Also
C       Set up a "BurnPrep" (activity code 491) with the EXPOSR as the
C       percent burnprep.
C
        IF (LAUTAL) THEN
          PRMS(1)=EXPOSR
          CALL OPADD (IYR,491,0,1,PRMS,KODE)
          PRMS(1)=FLOAT(IYR)
          CALL OPADD (IYR,427,0,1,PRMS,KODE)
          CALL OPINCR (IY,ICYC,NCYC)
        ENDIF
C
C       Adjust the CWD array to reflect the proportional burning loss as
C       recorded in PRBURN. If the BURNZ pool is zero, then the parent
C       WD pool is also zero, and should be skipped. This also avoids
C       trapping zero-division.  Also, calculate the BURNED values for
C       use in fuel consumption reports later.
C
C       First make a compressed copy of the Fuel Load to TCWD.
C
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
C
C     NOW CAN DO SMOKE PRODUCTION
C
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
C
C     IPM: smoke size; IP: unpiled/piled; IL: fuel class; IM: moisture type
C
      IP = 1
      IF (BTYPE .EQ. 1) IP = 2

      DO IPM=1,2
        TSMOKE = 0.0
        DO IL=1,MXFLCL
          TSMOKE = TSMOKE + PRBURN(IP,IL) * BURNZ(IP,IL)
     &         * EMMFAC(IM,IL,IP,IPM)

        IF (DEBUG) WRITE(JOSTND,9) IM,IL,IP,IPM,EMMFAC(IM,IL,IP,IPM)
    9   FORMAT('  FMCONS IM=',I2,' IL=',I2,' IP=',I2,' IPM=',I2,
     &                   ' EMMFAC=',F5.1)

        ENDDO
C
C       Only calculate smoke from live stuff if it is a 'proper' fire.
C       We aren't burning live stuff from a fuel treatment fire.
C
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

      RETURN
      END
