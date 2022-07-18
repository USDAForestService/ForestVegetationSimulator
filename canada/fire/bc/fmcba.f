      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C CANADA-FIRE-BC $Id$
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN
C
C  PURPOSE:
C     FIND THE DOMINANT SPECIES (BY BASAL AREA). SET THE INITIAL LIVE
C     AND DEAD FUEL VALUES AS WELL. THE DEAD FUELS ARE ONLY INITIALIZED
C     IN THE FIRST YEAR, BUT COVTYP AND THE LIVE FUELS MUST BE DONE
C     EACH YEAR. CALCULATE PERCOV, USED HERE AND IN FIRE BEHAVIOR CALCS.
C----------
C  LOCAL VARIABLE DEFINITIONS:
C     BAMOST:  THE HIGHEST BASAL AREA IN A SINGLE SPECIES
C     CAREA:   THE AREA COVERED BY THE CROWN AT ITS WIDEST POINT (SQFT)
C     COVINI:  THE SERAL COVER TYPE TO BE USED FOR INITIATING FUELS IN
C              BARE STANDS
C     CRL:     CROWN LENGTH
C     CWIDTH:  THE MAXIMUM WIDTH OF THE CROWNS (FT)
C     FUINIE:  THE INITIAL FUEL LOADINGS FOR ESTABLISHED STANDS (FROM JBROWN)
C     FUINII:  THE INITIAL FUEL LOADINGS FOR INITIATING STANDS (FROM JBROWN)
C     FULIVE:  THE HERB/SHRUB/REGEN FOR ESTABLISHED STANDS (FROM JBROWN)
C     FULIVI:  THE HERB/SHRUB/REGEN FOR INITIATING STANDS (FROM JBROWN)
C     ISWTCH:  =1 if called by SVSTART
C              =0 if called by any other subroutine (FMMAIN, FMPPHV)
C     TOTBA:   THE TOTAL BASAL AREA IN THE STAND (USED IN THE FUELS CALCS)
C     TOTCRA:  THE SUM OF THE AREA OF THE CROWNS, PER ACRE (SQFT)
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
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
COMMONS
C----------
C  LOCAL VARIABLES DECLARATIONS
C----------
      REAL TBA(MAXSP)
      REAL BAMOST, BA1, TOTCRA, CWIDTH
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12),FOTOVALS(9)
      LOGICAL DEBUG
      INTEGER NDRY
      INTEGER COVINI(30)
      PARAMETER (NDRY=16)
      INTEGER MAPDRY(2,NDRY),IHB
      INTEGER MYACT(3)
      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      REAL    BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD
C----------
C     CLASSIFICATION OF HABITATS TO BE DRY GRASSY (IDRYB=1),
C     DRY SHRUBBY (IDRYB=2), OR OTHER (IDRYB=0)
C     THE FOLLOWING TYPE CAN BE ADDED ONCE WE KNOW THEIR CODES:
C     ?,1  !  PIPO/ARUV
C     ?,2  !  PIPO/AMAL
C     ?,2  !  PIPO/PYMA
C     ?,2  !  PSME/SYOC
C----------
C     NOTE: BC VARIANT DOESN'T USE HABITAT TYPES. INSTEAD
C     IT USES BECS. WHEN WE CALIBRATE THIS MODEL, THIS CODE
C     WILL NEED TO CHANGE. (SARAH MARCH 04)
C
      DATA MAPDRY/
     &     130,1,     ! PIPO/AGSP
     &     140,1,     ! PIPO/FEID
     &     210,1,     ! PSME/AGSP
     &     220,1,     ! PSME/FEID
     &     230,1,     ! PSME/FESC
     &     161,2,     ! PIPO/PUTR
     &     170,2,     ! PIPO/SYAL
     &     171,2,     ! PIPO/SYAL-SYAL
     &     172,2,     ! PIPO/SYAL-BERE
     &     180,2,     ! PIPO/PRVI
     &     181,2,     ! PIPO/PRVI-PRVI
     &     182,2,     ! PIPO/PRVI-SHCA
     &     310,2,     ! PSME/SYAL
     &     311,2,     ! PSME/SYAL-AGSP
     &     312,2,     ! PSME/SYAL-CARU
     &     313,2/     ! PSME/SYAL-SYAL
C
C                  herbs, shrubs
      DATA FULIVE /0.15,  0.1,
     &             0.2,   0.2,
     &             0.2,   0.2,
     &             0.15,  0.1,
     &             0.2,   0.2,
     &             0.2,   0.2,
     &             0.2,   0.1,
     &             0.15,  0.2,
     &             0.15,  0.2,
     &             0.2,   0.25,
     &             0.25,  0.25, ! birch 21 !QA - ottmar and others 2000b
     &             0.25,  0.25, ! aspen 18
     &             0.25,  0.25, ! cotton 19 !QA - ottmar and others 2000b
     &             0.2,   0.2,  ! FD 3
     &             0.25,  0.25/ ! birch 21

      DATA FULIVI /0.30, 2.0,
     &             0.4,  2.0,
     &             0.4,  2.0,
     &             0.30, 2.0,
     &             0.4,  2.0,
     &             0.4,  2.0,
     &             0.4,  1.0,
     &             0.30, 2.0,
     &             0.30, 2.0,
     &             0.25, 0.10,
     &             0.18, 1.32,
     &             0.18, 1.32,
     &             0.18, 1.32,
     &             0.4,  2.0,
     &             0.18, 1.32/
C                  =====================================================
C                  Size Categories: inches
C                  A = 0.25,    B = <1,     C = 1-3,     D = 3-6
C                  E = 6-12,    F = 12-20,  G = 20-35,   H = 35-50
C                  I = >50,     J = Litter, K = Duff
C                  =====================================================
C                    A    B    C    D    E    F    G   H   I    J    K
C                  =====================================================
      DATA FUINIE /1.0, 1.0, 1.6,10.0,10.0,10.0, 0.0,0.0,0.0, 0.8,30.0,
     &             0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0,
     &             0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0,
     &             0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0, 0.6,25.0,
     &             2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0, 1.0,35.0,
     &             2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0, 1.0,35.0,
     &             0.9, 0.9, 1.2, 7.0, 8.0, 0.0, 0.0,0.0,0.0, 0.6,15.0,
     &             1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0,
     &             1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0,
     &             0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0, 1.4, 5.0,
     &             0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8,
     &             0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8,
     &             0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8,
     &             0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0,
     &             0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8/

C                  =====================================================
C                    A    B    C     D    E    F    G   H   I    J    K
C                  =====================================================
      DATA FUINII /0.6, 0.6, 0.8, 6.0, 6.0, 6.0, 0.0,0.0,0.0, 0.4,12.0,
     &             0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0,
     &             0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0,
     &             0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0, 0.3,12.0,
     &             1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0, 0.5,12.0,
     &             1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0, 0.5,12.0,
     &             0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0,0.0,0.0, 0.3, 7.0,
     &             0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0,
     &             0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0,
     &             0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0, 0.5, 0.8,
     &             0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6,
     &             0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6,
     &             0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6,
     &             0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0,
     &             0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6/

      DATA COVINI /10,10, 3, 3, 5, 4, 3, 3, 3, 8,
     &              8, 4, 4, 6, 6, 6, 5, 9, 9, 9,
     &              9, 9, 9, 9, 9, 9, 9, 9, 9, 9/
C
      DATA MYACT /2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
C----------
C     BEGIN ROUTINE.
C----------
C     ZERO OUT THE CUMMULATIVE VARIABLES
C----------
      COVTYP = 0
      PERCOV = 0.0
      BIGDBH = 0.0
      TOTBA  = 0.0
C----------
C     LOOP THROUGH THE TREE LIST
C----------
      IF (ITRN .GT. 0) THEN
C----------
C       ZERO OUT THE CUMMULATIVE VARIABLES
C----------
        BAMOST = 0.0
        TOTCRA = 0.0
C
        DO KSP=1,MAXSP
          TBA(KSP) = 0.0
        ENDDO
C
        DO I=1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN
C
            KSP = ISP(I)
C
            BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
            TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)
C
            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
C----------
C           CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
C           ENCOMPASSED BY ALL TREES
C----------
             CWIDTH=CRWDTH(I)
             CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF
C----------
C         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.
C----------
          CURKIL(I) = 0.0
        ENDDO
C----------
C        DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
C        -> THAT WILL BE THE COVER TYPE
C----------
        DO KSP=1,MAXSP
          IF (TBA(KSP) .GT. BAMOST) THEN
            BAMOST = TBA(KSP)
            COVTYP = KSP
          ENDIF
          TOTBA = TOTBA + TBA(KSP)
        ENDDO
C----------
C       USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
C       OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
C       CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:
C
C       PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))
C----------
        PERCOV = 1.0 - EXP(-TOTCRA/43560.)
        PERCOV = PERCOV * 100.0
C
      ENDIF
C----------
C     IF THERE ARE NO TREES (COVTYP=0) THEN USE THE HABITAT TYPE
C     (INDEX=ITYPE) TO DETERMINE A SURROGATE
C----------
      IF (COVTYP .EQ. 0) COVTYP = COVINI(ITYPE)
C----------
C     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C----------
      XCOV(1)=10.
      XCOV(2)=60.
      DO I=1,2
        YLOAD(1)=FULIVI(I,COVTYP)
        YLOAD(2)=FULIVE(I,COVTYP)
        FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO
C
      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >       2F6.3)
C----------
C     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION
C----------
      IF (IYR .EQ. IY(1)) THEN

Csng      IF (IYR .EQ. IY(1)) THEN
C----------
C        ASSUME THE FUELS ARE UNPILED AND HARD. [**THIS WILL CHANGE LATER]
C
C        LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C        STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C----------
         XCOV(1) = 10.0
         XCOV(2) = 60.0
         DO ISZ = 1,MXFLCL
           YLOAD(1)=FUINII(ISZ,COVTYP)
           YLOAD(2)=FUINIE(ISZ,COVTYP)
           STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
           STFUEL(ISZ,1) = 0
         ENDDO
C----------
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT
C----------
C       **                                         **
C       ** CURRENTLY UNSUPPORTED IN THE BC VARIANT **
C       ** BUT PERHAPS COULD BE.                   **
C       **                                         **

        CALL OPFIND(1,MYACT(2),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,2,JYR,IACTK,NPRM,PRMS)
          IF ((PRMS(1) .GE. 0) .AND. (PRMS(2) .GE. 0)) THEN
            CALL FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL,
     >                      FOTOVALS) 
            DO I = 1, MXFLCL
              IF (FOTOVAL(I) .GE. 0) STFUEL(I,2) = FOTOVAL(I)
              IF (I .LE. 9) STFUEL(I,1) = FOTOVALS(I)
            ENDDO
C----------
C           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF

C----------
C        CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT
C----------
         CALL OPFIND(1,MYACT(1),J)
         IF (J .GT. 0) THEN
           CALL OPGET(J,12,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(2) .GE. 0) STFUEL(3,2)  = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(4,2)  = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(5,2)  = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(6,2)  = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(10,2) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(11,2) = PRMS(7)
          IF (PRMS(8) .GE. 0) STFUEL(1,2)  = PRMS(8)
          IF (PRMS(9) .GE. 0) STFUEL(2,2)  = PRMS(9)
          IF (PRMS(1) .GE. 0) THEN
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(1,2) = PRMS(1) * 0.5
              STFUEL(2,2) = PRMS(1) * 0.5
            ENDIF
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .GE. 0)) THEN
              STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
            ENDIF
            IF ((PRMS(8) .GE. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
            ENDIF
          ENDIF
          IF (PRMS(10) .GE. 0) STFUEL(7,2) = PRMS(10)
          IF (PRMS(11) .GE. 0) STFUEL(8,2) = PRMS(11)
          IF (PRMS(12) .GE. 0) STFUEL(9,2) = PRMS(12)
C----------
C          DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C          NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
           IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)
        ENDIF

        CALL OPFIND(1,MYACT(3),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,9,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(1) .GE. 0) STFUEL(1,1) = PRMS(1)
          IF (PRMS(2) .GE. 0) STFUEL(2,1) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(3,1) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(4,1) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(5,1) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(6,1) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(7,1) = PRMS(7)
          IF (PRMS(8) .GE. 0) STFUEL(8,1) = PRMS(8)
          IF (PRMS(9) .GE. 0) STFUEL(9,1) = PRMS(9)

C          DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C          NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

           IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF

C----------
C        DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C        OF BASAL AREA IN THE STAND.
C----------
         DO ISZ = 1,MXFLCL
           IF (TOTBA .GT. 0.0) THEN
             DO KSP = 1,MAXSP
               IF (TBA(KSP) .GT. 0.0) THEN
                 DO J = 1,2
                   PRCL = TBA(KSP) / TOTBA
                   IDC = DKRCLS(KSP)
                   ADD = PRCL * STFUEL(ISZ,J)
                   CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
                 ENDDO
               ENDIF
             ENDDO
           ELSE
             IDC = DKRCLS(COVTYP)
             DO J = 1,2
               CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
             ENDDO
           ENDIF
         ENDDO
C----------
C       SET IDRYB FOR THIS HABITAT TYPE; TO BE USED IN **FMCFMD**
C       NOTE IDRYB IS USED IN OTHER VARIANTS TO SIGNAL START/END YEARS
C       FOR DROUGHT; UNUSED IN THIS VARIANT, SO REUSED TO HOLD THIS
C       CATEGORY
C----------
        IDRYB = 0
        DO I = 1,NDRY
          IF (MAPDRY(1,I) .EQ. FMKOD) THEN
            IDRYB = MAPDRY(2,I)
            EXIT
          ENDIF
        ENDDO
C
      ENDIF

C     IN FIRST YEAR, SET C-REPORTING REGION FOR FOREST CODE 621
      IF (IYR.EQ.IY(1) .AND. KODFOR.EQ.621) ICHABT = 2

      RETURN
C----------
C     HOOK TO ALLOW THE HABITAT CATEGORY TO BE READ BY **FMCFMD*
C----------
      ENTRY NIFMHAB(IHB)
      IHB = IDRYB
      RETURN

      ENTRY SNGCOE
C----------
C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      RETURN
      END
