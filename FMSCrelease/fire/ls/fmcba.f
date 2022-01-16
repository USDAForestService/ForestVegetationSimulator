      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-LS $Id$
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
C     FUINI:   THE INITIAL DEAD FUEL LOADINGS 
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
      REAL  FULIV(2,9),FUINI(MXFLCL,26)
      REAL  PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD,BAMOST
      REAL  CAREA, TOTCRA, CWIDTH, FOTOVAL(MXFLCL), FOTOVALS(9)
      INTEGER IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,ISWTCH,IDC,JYR
      INTEGER FTLIVEFU,FTDEADFU
      LOGICAL DEBUG
C----------
C  CURRENTLY THE HERB AND SHRUB VALUES ARE ESTIMATED FROM FIA FOREST
C  TYPE AND SIZE CLASS.  ONLY THREE FOREST TYPES ARE CONSIDERED:
C  JACK PINE, RED/WHITE PINE, AND HARDWOODS.
C  THE HERB AND SHRUB VALUES COME FROM THE FOLLOWING DOCUMENTS:
C  
C  Ottmar, Roger D.; Vihnanek, Robert E. 1999. Stero photo series for 
C  quantifying natural fuels.  Volume V: midwest red and white pine, northern
C  tallgrass prairie, and mixed oak types in the Central and Lake States.
C  PMS 834.  Boise, ID: National Wildfire Coordinating Group, National
C  Interagency Fire Center.  99 p.
C
C  Ottmar, Roger D.; Vihnanek, Robert E.; Wright, Clinton S. 2002. Stero
C  photo series for quantifying natural fuels.  Volume Va: jack pine in 
C  the Lake States.  PMS 837.  Boise, ID: National Wildfire Coordinating
C  Group, National Interagency Fire Center.  49 p.  
C
C----------
C                  herbs, shrubs  
      DATA FULIV /0.12, 0.17,  ! white / red pine, sz cl 1 - use MP06
     &            0.08, 0.02,  ! white / red pine, sz cl 2 - use MP10
     &            0.06, 0.00,  ! white / red pine, sz cl 3 - use MP09
     &            0.06, 0.63,  ! jack pine, sz cl 1 - use photo JP16
     &            0.10, 0.04,  ! jack pine, sz cl 2 - use JP15
     &            0.14, 0.35,  ! jack pine, sz cl 3 - use JP11
     &            0.00, 0.00,  ! hardwoods, sz cl 1 - use MO03
     &            0.00, 0.00,  ! hardwoods, sz cl 2 - MO11 
     &            0.00, 0.01/  ! hardwoods, sz cl 3 - use MO09
     
C----------
C  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
C  FROM FIA FOREST TYPE AND SIZE CLASS.
C----------
C                  <.25 to1  1-3   3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINI /0.09,0.51,1.76,0.78,1.89,1.95,0.0,0.0,0.0,1.83,3.87, ! white / red pine (100), sz cl 1
     &            0.07,0.51,0.93,0.06,0.26,0.00,0.0,0.0,0.0,0.54,1.43, ! white / red pine (100), sz cl 2
     &            0.07,0.25,0.18,0.00,0.00,0.00,0.0,0.0,0.0,2.05,9.96, ! white / red pine (100), sz cl 3
     &            0.31,2.22,4.99,0.64,2.11,1.46,0.0,0.0,0.0,1.44,9.05, ! jack pine (101), sz cl 1
     &            0.12,0.44,1.05,0.66,1.26,0.00,0.0,0.0,0.0,0.81,2.97, ! jack pine (101), sz cl 2
     &            0.11,0.89,4.58,4.98,4.76,0.00,0.0,0.0,0.0,0.20,2.09, ! jack pine (101), sz cl 3
     &            0.40,0.95,1.79,1.28,4.47,1.46,0.0,0.0,0.0,0.90,64.29, ! spruce - fir (120), sz cl 1
     &            0.19,0.60,1.22,0.52,1.88,0.57,0.0,0.0,0.0,2.36,80.12, ! spruce - fir (120), sz cl 2
     &            0.07,0.49,2.04,0.98,2.61,0.35,0.0,0.0,0.0,1.20,36.70, ! spruce - fir (120), sz cl 3
     &            0.00,0.00,0.00,0.79,0.00,0.00,0.0,0.0,0.0,0.07,8.28, ! eastern redcedar (180)
     &            0.12,0.57,1.14,0.62,3.71,0.37,0.0,0.0,0.0,0.81,2.14, ! oak - pine (400), sz cl 1
     &            0.11,0.95,1.59,0.17,1.89,0.00,0.0,0.0,0.0,0.29,1.28, ! oak - pine (400), sz cl 2
     &            0.12,0.49,0.60,0.67,0.77,0.00,0.0,0.0,0.0,0.20,4.77, ! oak - pine (400), sz cl 3
     &            0.18,0.53,1.60,0.78,1.65,0.52,0.0,0.0,0.0,0.91,4.45, ! oak - hickory (500), sz cl 1
     &            0.24,0.59,1.41,0.74,1.61,1.30,0.0,0.0,0.0,0.44,3.02, ! oak - hickory (500), sz cl 2
     &            0.09,0.80,0.16,0.48,0.51,0.00,0.0,0.0,0.0,0.49,3.96, ! oak - hickory (500), sz cl 3
     &            0.17,0.87,2.10,0.97,2.17,4.52,0.0,0.0,0.0,1.61,40.33, ! elm - ash - cottonwood (700), sz cl 1
     &            0.22,0.48,1.78,1.01,3.09,0.89,0.0,0.0,0.0,1.00,156.25, ! elm - ash - cottonwood (700), sz cl 2
     &            0.13,0.26,0.74,0.49,1.88,0.12,0.0,0.0,0.0,1.17,21.42, ! elm - ash - cottonwood (700), sz cl 3
     &            0.22,0.62,1.74,0.99,1.77,1.41,0.0,0.0,0.0,1.38,7.82, ! maple - beech - birch (800), sz cl 1
     &            0.29,0.74,2.11,0.96,1.92,3.33,0.0,0.0,0.0,0.90,7.65, ! maple - beech - birch (800), sz cl 2
     &            0.23,0.69,2.96,1.15,2.81,0.28,0.0,0.0,0.0,0.81,2.37, ! maple - beech - birch (800), sz cl 3
     &            0.13,0.86,2.10,0.82,2.76,1.10,0.0,0.0,0.0,0.71,14.75, ! aspen - birch (900), sz cl 1
     &            0.12,0.70,2.51,1.04,2.41,2.70,0.0,0.0,0.0,0.53,6.58, ! aspen - birch (900), sz cl 2
     &            0.13,0.52,1.69,0.77,1.80,0.66,0.0,0.0,0.0,1.04,9.55, ! aspen - birch (900), sz cl 3
     &            0.07,0.32,0.34,0.19,0.63,0.00,0.0,0.0,0.0,0.00,0.36/ ! nonstocked (999)
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
      TOTBA = 0.0
C----------
C  DETERMINE FFE FOREST TYPE (1 OF 10 CATEGORIES) FROM FIA FOR. TYPE
C----------     
      CALL FMLSFT(IFFEFT)
C----------
C  LOAD LIVE FUELS AS A FUNCTION OF FOREST TYPE
C----------
      SELECT CASE (IFFEFT)
      CASE(1,3,4) ! white and red pine, spruce-fir and redcedar
        IF (ISZCL .EQ. 1) THEN
           FTLIVEFU = 1
        ELSEIF (ISZCL .EQ. 2) THEN
           FTLIVEFU = 2
        ELSE
           FTLIVEFU = 3
        ENDIF

      CASE(2) ! jack pine
        IF (ISZCL .EQ. 1) THEN
           FTLIVEFU = 4
        ELSEIF (ISZCL .EQ. 2) THEN
           FTLIVEFU = 5
        ELSE
           FTLIVEFU = 6
        ENDIF

      CASE DEFAULT
        IF (ISZCL .EQ. 1) THEN
           FTLIVEFU = 7
        ELSEIF (ISZCL .EQ. 2) THEN
           FTLIVEFU = 8
        ELSE
           FTLIVEFU = 9
        ENDIF
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
            FMTBA(KSP) = 0.0
         ENDDO
C----------
C  LOOP THROUGH THE TREE LIST
C----------
         DO I=1,ITRN
            IF (FMPROB(I) .GT. 0.0) THEN
               KSP = ISP(I)
               FMTBA(KSP) = FMTBA(KSP) +
     &                   FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
            
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
            IF (FMTBA(KSP) .GT. BAMOST) THEN
               BAMOST = FMTBA(KSP)
               COVTYP = KSP
            ENDIF 
            TOTBA = TOTBA + FMTBA(KSP)
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
C   SET COVTYP TO A DEFAULT RED PINE COVER. AFTER THE
C   FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
C   PRESENT.
C----------
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA',
     &       /1X,'*** COVER TYPE SET TO RED PINE',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 3
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
        SELECT CASE (IFFEFT)
        CASE(1) ! white / red pine
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 1
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 2
          ELSE
            FTDEADFU = 3
          ENDIF
        CASE(2) ! jack pine
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 4
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 5
          ELSE
            FTDEADFU = 6
          ENDIF
        CASE(3) ! spruce - fir
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 7
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 8
          ELSE
            FTDEADFU = 9
          ENDIF
        CASE(4) ! eastern redcedar
          FTDEADFU = 10
        CASE(5) ! oak - pine
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 11
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 12
          ELSE
            FTDEADFU = 13
          ENDIF
        CASE(6) ! oak - hickory
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 14
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 15
          ELSE
            FTDEADFU = 16
          ENDIF
        CASE(7) ! elm - ash - cottonwood
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 17
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 18
          ELSE
            FTDEADFU = 19
          ENDIF
        CASE(8) ! maple - beech - birch
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 20
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 21
          ELSE
            FTDEADFU = 22
          ENDIF
        CASE(9) ! aspen - birch
          IF (ISZCL .EQ. 1) THEN
            FTDEADFU = 23
          ELSEIF (ISZCL .EQ. 2) THEN
            FTDEADFU = 24
          ELSE
            FTDEADFU = 25
          ENDIF
        CASE(10) ! nonstocked
          FTDEADFU = 26
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
              IF (FMTBA(KSP) .GT. 0.0) THEN
                DO J = 1,2
                  PRCL = FMTBA(KSP) / TOTBA
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
C
      ENDIF
C
      ENTRY SNGCOE
C----------
C  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      RETURN
      END
