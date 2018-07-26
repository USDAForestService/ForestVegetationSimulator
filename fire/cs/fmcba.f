      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-CS $Id$
C----------
C  SINGLE-STAND VERSION
C  CALLED FROM: FMMAIN
C
C  PURPOSE:
C     FIND THE FOREST TYPE. SET THE INITIAL LIVE AND DEAD FUEL VALUES
C     THE DEAD FUELS ARE ONLY INITIALIZED IN THE FIRST YEAR, BUT THE
C     LIVE FUELS MUST BE DONE EACH YEAR.  THE ROUTINE ALSO CALCULATES
C     PERCOV, WHICH IS NEEDED FOR THE FIRE BEHAVIOR CALCS.
C----------
C  LOCAL VARIABLE DEFINITIONS:
C     BAMOST:  THE HIGHEST BASAL AREA IN A SINGLE SPECIES
C     FUINI:   THE INITIAL FUEL LOADINGS
C     FULIV:   THE HERB/SHRUB LOADINGS
C     ISWTCH:  =1 if called by SVSTART
C              =0 if called by any other subroutine (FMMAIN, FMPPHV)
C     TOTBA:   THE TOTAL BASAL AREA IN THE STAND
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
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C----------
C  LOCAL VARIABLE DECLARATIONS
C----------
      REAL  TBA(MAXSP),FULIV(2,4),FUINI(MXFLCL,7)
      REAL  PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD,BAMOST,BA1
      REAL  CAREA, BIGDBH, TOTCRA, CWIDTH, FOTOVAL(MXFLCL), FOTOVALS(9)
      INTEGER IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,IDC,ISWTCH,JYR
      INTEGER FTLIVEFU,FTDEADFU
      LOGICAL DEBUG
C----------
C  CURRENTLY THE HERB AND SHRUB VALUES ARE ONLY ESTIMATED FROM FOREST
C  TYPE, DUE TO A LACK OF DATA.  ONLY THREE FOREST TYPES ARE CONSIDERED:
C  PINE, HARDWOOD, AND REDCEDAR. (Taken from SN-FFE)
C----------
C                  herbs, shrubs
      DATA FULIV /0.1, 0.25,  ! pines
     &            0.01, 0.03, ! hardwoods
     &            1.0,  5.0,  ! redcedar
     &            0.02, 0.13/ ! oak savannah
C----------
C  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
C  FROM FIA FOREST TYPE.  
C----------
C
C                <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINI /0.18,0.93,1.77,0.27,0.75,8.38,0.0,0.0,0.0,4.10,3.82, !pines 100s
     &            0.19,0.86,1.58,0.11,0.31,0.67,0.0,0.0,0.0,4.89,4.40, !redcedar 181/402
     &            0.18,0.75,2.42,0.59,0.67,1.34,0.0,0.0,0.0,5.37,3.07, !pine-hardwood 400s
     &            0.15,0.74,1.70,0.38,0.97,2.68,0.0,0.0,0.0,5.17,4.52, !oak-hickory 500s
     &            0.20,0.92,2.19,0.41,1.46,3.80,0.0,0.0,0.0,2.49,2.80, !elm-ash-cottonwood 700s
     &            0.19,0.88,1.95,0.56,1.62,1.82,0.0,0.0,0.0,3.88,3.41, !maple-beech-birch 800s
     &            0.02,0.21,0.40,0.02,0.33,0.42,0.0,0.0,0.0,3.12,2.05/ !nonstocked 999
C                                                    
      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
 7    FORMAT(' ENTERING FMCBA CYCLE = ',I2)
C-----------
C  ZERO OUT THE CUMMULATIVE VARIABLES
C-----------
      COVTYP = 0
      PERCOV = 0
      BIGDBH = 0
      TOTBA = 0.0
C----------
C  DETERMINE FFE FOREST TYPE (1 OF 9 CATEGORIES) FROM FIA FOR. TYPE
C----------
      CALL FMCSFT(IFFEFT)
C----------
C  LOAD LIVE FUELS AS A FUNCTION OF FOREST TYPE
C----------
      SELECT CASE (IFFEFT)
      CASE(3,4,5)
        FTLIVEFU = 1 !pines
      CASE(7)
        FTLIVEFU = 3 !redcedar
      CASE(6)
        FTLIVEFU = 4 !oak savannah
      CASE DEFAULT
        FTLIVEFU = 2 !hardwoods
      END SELECT
C
      DO I=1,2
         FLIVE(I)=FULIV(I, FTLIVEFU)
      ENDDO
C
      IF (ITRN.GT.0) THEN
C----------
C  ZERO OUT THE CUMMULATIVE VARIABLES
C----------
         BAMOST = 0.0
         TOTCRA = 0.0
         DO KSP=1,MAXSP
            TBA(KSP) = 0.0
         ENDDO
C----------
C  LOOP THROUGH THE TREE LIST
C----------
         DO I=1,ITRN
            IF (FMPROB(I) .GT. 0.0) THEN
               KSP = ISP(I)
               BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
               TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)

               IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
C----------
C  CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
C  ENCOMPASSED BY ALL TREES
C----------
               CWIDTH=CRWDTH(I)

               CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
               TOTCRA = TOTCRA + CAREA*FMPROB(I)
            ENDIF
C----------
C  USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.
C----------
            CURKIL(I) = 0.0
         ENDDO
C----------
C  DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
C  -> THAT WILL BE THE COVER TYPE
C----------
         DO KSP=1,MAXSP
            IF (TBA(KSP) .GT. BAMOST) THEN
               BAMOST = TBA(KSP)
               COVTYP = KSP
            ENDIF
            TOTBA = TOTBA + TBA(KSP)
         ENDDO
C----------
C  USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
C  OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
C  CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:
C
C  PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))
C----------
        PERCOV = 1.0 - EXP(-TOTCRA/43560.)
        PERCOV = PERCOV * 100.0
      ENDIF
      IF (DEBUG) WRITE(JOSTND,*) 'PERCOV = ',PERCOV
C----------
C   IF THERE ARE NO TREES (COVTYP=0) IN CYCLE 1,
C   SET COVTYP TO A DEFAULT RED OAK COVER. AFTER THE
C   FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
C   PRESENT.
C----------
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA',
     &       /1X,'*** COVER TYPE SET TO RED OAK',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 48
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF
      ENDIF
C
      OLDCOVTYP = COVTYP
C----------
C  INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION
C----------
      IF (IYR .EQ. IY(1)) THEN
Csng      IF (IYR .EQ. IY(1)) THEN
Cppe      IF (IYR .EQ. MIY(1)) THEN
C----------
C  LOAD DEAD FUELS AS A FUNCTION OF FOREST TYPE.
C----------
         SELECT CASE (IFORTP)
         CASE(101:168)        
           FTDEADFU = 1 !pines
         CASE(181,402)        
           FTDEADFU = 2 !redcedar
         CASE(401,403:409)        
           FTDEADFU = 3 !pine-hardwood
         CASE(501:520)        
           FTDEADFU = 4 !oak-hickory
         CASE(701:709)        
           FTDEADFU = 5 !elm-ash-cottonwood
         CASE(801:809)        
           FTDEADFU = 6 !maple-beech-birch
         CASE(999)        
           FTDEADFU = 7 !nonstocked
         CASE DEFAULT
           FTDEADFU = 4 !oak-hickory
         END SELECT
C
         DO ISZ = 1,MXFLCL
            STFUEL(ISZ,2) = FUINI(ISZ,FTDEADFU)
            STFUEL(ISZ,1) = 0
         ENDDO
C----------        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT
C----------
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
C  IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
 
C-----------
C  CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C  FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT
C-----------
        CALL OPFIND(1,MYACT(1),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,12,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(2) .GE. 0) STFUEL(3,2) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(4,2) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(5,2) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(6,2) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(10,2) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(11,2) = PRMS(7)
          IF (PRMS(8) .GE. 0) STFUEL(1,2) = PRMS(8)          
          IF (PRMS(9) .GE. 0) STFUEL(2,2) = PRMS(9)           
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
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
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

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF

C----------
C  DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C  OF BASAL AREA IN THE STAND.
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

      ENDIF
C
      ENTRY SNGCOE
C----------
C  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      RETURN
      END
