      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-SN $Id$
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
C     FUINI:   THE INITIAL FUEL LOADINGS BY FOREST TYPE GROUP
C     FULIV:   THE HERB/SHRUB LOADINGS
C     ISWTCH:  =1 if called by SVSTART
C              =0 if called by any other subroutine (FMMAIN, FMPPHV, FMBURN)
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
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C----------
C  LOCAL VARIABLE DECLARATIONS
C----------
      REAL  TBA(MAXSP),FULIV(2,4),FULIV2(8,6),FULIV3(8)
      REAL  FUINI(MXFLCL,9),PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD
      REAL  BAMOST,BA1,Y(8),SHRUBAGE, FOTOVALS(9)
      REAL  CAREA, BIGDBH, TOTCRA, CWIDTH, FOTOVAL(MXFLCL),ALGSLP
      INTEGER IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,ISWTCH,IDC,JYR
      INTEGER FTLIVEFU,FTDEADFU,L
      INTEGER IFK
      LOGICAL DEBUG
C----------
C  FOR SOME ECOLOGICAL UNITS, THE HERB AND SHRUB VALUES ARE ONLY ESTIMATED FROM FOREST
C  TYPE, DUE TO A LACK OF DATA. 
C----------
C                  herbs, shrubs
      DATA FULIV /0.1, 0.25,  ! pines
     &            0.01, 0.03, ! hardwoods
     &            1.0,  5.0,  ! redcedar
     &            0.02, 0.13/ ! oak savannah

C----------
C  FOR COASTAL PLAIN, PIEDMONT, AND MOUNTAIN SITES, THE HERB AND SHRUB VALUES
C  ARE SET BASED ON UNDERSTORY HEIGHT AND THE AGE OF THE ROUGH, USING VALUES FROM 
C  "SOUTHERN FORESTRY SMOKE MANAGEMENT GUIDEBOOK" - GTR-SE-10, PG118, ALSO REPLICATED
C  IN "WILDLAND FIRE IN ECOSYSTEMS: EFFECTS OF FIRE ON FLORA" - RMRS-GTR-42, VOL2, PG63
C  SINCE UNDERSTORY HEIGHT IS NOT KNOWN, SITE INDEX IS USED AS A SURROGATE.
C  COASTAL PLAIN SITE (PALMETTO-GALBERRY) WILL GET THE VALUES BELOW, WHILE PIEDMONT AND
C  MOUNTAIN PYRIC SITES (MOUNTAIN-LAUREL AND RHODODENDRON) WILL GET 40% OF THE VALUES BELOW.
C  ALL OF THE BIOMASS WILL BE IN THE SHRUB CATEGORY AND NONE WILL GO INTO HERBS.
C----------
      DATA Y / 1, 2, 3, 5, 7, 10, 15, 20/
C                  age of rough (1,2,3,5,7,10,15,20 years)
      DATA FULIV2 /0.4, 0.4, 0.5, 0.6, 0.9, 1.4, 2.6, 4.2, ! understory ht of 1 ft  / si < 50
     &             1.2, 1.3, 1.3, 1.5, 1.7, 2.2, 3.4, 5.1, ! understory ht of 2 ft  / si 50 - 65
     &             2.6, 2.6, 2.7, 2.8, 3.1, 3.5, 4.7, 6.4, ! understory ht of 3 ft  / si 65 - 80
     &             4.5, 4.5, 4.6, 4.7, 5.0, 5.5, 6.6, 8.3, ! understory ht of 4 ft  / si 80 - 95
     &             7.0, 7.0, 7.0, 7.2, 7.4, 7.9, 9.1,10.8, ! understory ht of 5 ft  / si 95 - 110
     &            10.0,10.0,10.0,10.2,10.4,10.9,12.1,13.8/ ! understory ht of 6 ft  / si >= 110
C----------
C  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
C  FROM FIA FOREST TYPE.
C----------
C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINI /0.10,0.50,1.68,0.55,0.64,0.07,0.0,0.0,0.0,4.02,12.52, ! eastern white pine (100s)
     &            0.10,0.66,0.98,0.12,0.29,0.26,0.0,0.0,0.0,6.38,8.66,  ! longleaf-slash pine (140s)
     &            0.14,0.72,1.54,0.25,0.44,0.33,0.0,0.0,0.0,4.90,6.03,  ! loblolly-shortleaf pine (160s)
     &            0.24,1.24,2.72,0.36,0.97,0.33,0.0,0.0,0.0,3.82,3.80,  ! eastern redcedar (181 / 402)
     &            0.18,0.77,2.17,0.31,0.86,0.78,0.0,0.0,0.0,4.07,6.15,  ! oak-pine (400s)
     &            0.13,0.68,1.93,0.43,1.01,1.01,0.0,0.0,0.0,4.28,5.91,  ! oak-hickory (500s)
     &            0.13,0.67,1.83,0.18,0.57,0.77,0.0,0.0,0.0,2.49,5.68,  ! oak-gum-cypress (600s)
     &            0.22,1.09,2.68,0.26,0.76,0.43,0.0,0.0,0.0,2.33,1.60,  ! elm-ash-cottonwood (700s)
     &            0.09,0.64,2.03,0.43,1.18,3.38,0.0,0.0,0.0,3.75,4.10/  ! maple-beech-birch (800s)
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
C-----------
C  SET LIVE FUEL LEVELS
C-----------
      IF ((PCOM(1:3) .EQ. '232') .OR. (PCOM(1:3) .EQ. '231') .OR. 
     &    (PCOM(1:4) .EQ. 'M221')) THEN
        
        IF (SITEAR(ISISP) .LT. 50) THEN
          J=1
        ELSEIF (SITEAR(ISISP) .LT. 65) THEN
          J=2
        ELSEIF (SITEAR(ISISP) .LT. 80) THEN 
          J=3
        ELSEIF (SITEAR(ISISP) .LT. 95) THEN
          J=4
        ELSEIF (SITEAR(ISISP) .LT. 110) THEN 
          J=5
        ELSE 
          J=6
        ENDIF

        IF (BURNYR .GT. 0) THEN
          SHRUBAGE = IYR - BURNYR        
        ELSE
          SHRUBAGE = (IYR - IY(1) + 5) ! so default is 5 years before inventory
        ENDIF
        SHRUBAGE = MIN(20,MAX(1,INT(SHRUBAGE)))
 
        DO L=1,8
          FULIV3(L) = FULIV2(L,J)
        ENDDO
        
        FLIVE(1) = 0.0 ! no herbs       
        FLIVE(2) = ALGSLP(SHRUBAGE,Y,FULIV3,8)    
        IF ((PCOM(1:3) .EQ. '231') .OR. (PCOM(1:4) .EQ. 'M221')) THEN 
          FLIVE(2) = FLIVE(2)*0.40   
        ENDIF

      ELSE
C----------
C  DETERMINE FFE FOREST TYPE (1 OF 8 CATEGORIES) FROM FIA FOR. TYPE AND LOAD LIVE FUELS
C----------
        CALL FMSNFT(IFFEFT)
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
      ENDIF
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
            COVTYP = 75
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
         CASE(101:105)          
           FTDEADFU = 1 !eastern white pine
         CASE(141,142)          
           FTDEADFU = 2 !longleaf-slash pine
         CASE(161:168)          
           FTDEADFU = 3 !loblolly-shortleaf pine
         CASE(181,402)          
           FTDEADFU = 4 !eastern redcedar
         CASE(401,403:409)          
           FTDEADFU = 5 !oak-pine
         CASE(501:520)         
           FTDEADFU = 6 !oak-hickory
         CASE(601:608)          
           FTDEADFU = 7 !oak-gum-cypress
         CASE(701:709)          
           FTDEADFU = 8 !elm-ash-cottonwood
         CASE(801:809)          
           FTDEADFU = 9 !maple-beech-birch
         CASE DEFAULT
           FTDEADFU = 6 !oak-hickory
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

C     IN FIRST YEAR, SET C-REPORTING REGION FOR FOREST CODE
C     DEFAULT is ICHABT=1 -> SC; ICHABT=2 -> SE

      IF (IYR.EQ.IY(1)) THEN
        IFK = (KODFOR/100)  ! drop lower 2 digits
        SELECT CASE (IFK)
          CASE (803,805,808,811,812)
            ICHABT = 2
        END SELECT
      ENDIF
C
      ENTRY SNGCOE
C----------
C  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      RETURN
      END
