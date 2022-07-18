      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-NE $Id: fmcba.f 0000 2018-02-14 00:00:00Z gedixon $
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
      REAL  FULIV(2,1),FUINI(MXFLCL,31)
      REAL  PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD,BAMOST
      REAL  CAREA, TOTCRA, CWIDTH, FOTOVAL(MXFLCL), FOTOVALS(9)
      INTEGER IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,ISWTCH,IDC,JYR
      INTEGER FTDEADFU
      LOGICAL DEBUG      
C----------
C  CURRENTLY THE HERB AND SHRUB VALUES ARE ESTIMATED VERY CRUDELY FROM 
C  THE FOLLOWING PUBLICATION:
C
C  Chojnacky, D.C., R.A. Mickler, L.S. Heath, and C.W. Woodall.  2004.  
C  Estimates of down woody material in eastern US forests.  
C  Environmental Management 33(1): S44-S55.
C  
C  THIS NEEDS TO BE IMPROVED ONCE THE NORTHEAST PHOTOSERIES IS RELEASED.
C----------
C                  herbs, shrubs  
      DATA FULIV /0.31, 0.31/  ! all types get these loadings     
     
C----------
C  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
C  FROM FIA FOREST TYPE AND SIZE CLASS (based on FIA plot fuels data from the northeast)
C----------
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINI /0.81,1.19,1.66,0.73,2.24,2.01,0.0,0.0,0.0,3.06,11.20, ! white / red / jack pine (100), sz cl 1    
     &            0.81,1.19,1.66,0.73,2.24,2.01,0.0,0.0,0.0,3.06,11.20, ! white / red / jack pine (100), sz cl 2 
     &            0.81,1.19,1.66,0.73,2.24,2.01,0.0,0.0,0.0,3.06,11.20, ! white / red / jack pine (100), sz cl 3 
     &            0.37,0.59,1.85,0.82,2.47,5.40,0.0,0.0,0.0,3.36,48.56, ! spruce - fir (120), sz cl 1
     &            0.50,0.68,1.77,1.52,3.17,1.64,0.0,0.0,0.0,1.91,22.14, ! spruce - fir (120), sz cl 2
     &            0.31,0.65,1.55,1.45,2.82,1.08,0.0,0.0,0.0,1.57,25.87, ! spruce - fir (120), sz cl 3
     &            0.21,1.08,7.32,0.53,0.57,0.00,0.0,0.0,0.0,4.21,13.94, ! loblolly - shortleaf pine (160), sz cl 1
     &            0.21,1.08,7.32,0.53,0.57,0.00,0.0,0.0,0.0,4.21,13.94, ! loblolly - shortleaf pine (160), sz cl 2
     &            0.21,1.08,7.32,0.53,0.57,0.00,0.0,0.0,0.0,4.21,13.94, ! loblolly - shortleaf pine (160), sz cl 3
     &            0.34,0.50,0.56,0.08,0.00,0.00,0.0,0.0,0.0,0.60, 8.11, ! exotic softwoods (380), sz cl 1
     &            0.34,0.50,0.56,0.08,0.00,0.00,0.0,0.0,0.0,0.60, 8.11, ! exotic softwoods (380), sz cl 2
     &            0.34,0.50,0.56,0.08,0.00,0.00,0.0,0.0,0.0,0.60, 8.11, ! exotic softwoods (380), sz cl 3
     &            0.23,0.82,2.55,0.83,2.11,1.20,0.0,0.0,0.0,4.06,22.63, ! oak - pine (400), sz cl 1
     &            0.26,0.84,1.15,0.49,0.50,4.64,0.0,0.0,0.0,3.12,17.56, ! oak - pine (400), sz cl 2
     &            0.26,0.84,1.15,0.49,0.50,4.64,0.0,0.0,0.0,3.12,17.56, ! oak - pine (400), sz cl 3
     &            0.31,0.72,2.09,0.86,1.49,2.34,0.0,0.0,0.0,2.01, 7.64, ! oak - hickory (500), sz cl 1 
     &            0.32,1.13,2.51,0.53,0.98,0.52,0.0,0.0,0.0,1.75, 7.31, ! oak - hickory (500), sz cl 2
     &            0.17,0.77,1.43,0.45,0.54,0.06,0.0,0.0,0.0,1.35, 3.43, ! oak - hickory (500), sz cl 3
     &            0.32,0.75,1.31,0.64,2.10,0.98,0.0,0.0,0.0,1.07,15.21, ! oak - gum - cypress (600), sz cl 1  
     &            0.32,0.75,1.31,0.64,2.10,0.98,0.0,0.0,0.0,1.07,15.21, ! oak - gum - cypress (600), sz cl 2  
     &            0.32,0.75,1.31,0.64,2.10,0.98,0.0,0.0,0.0,1.07,15.21, ! oak - gum - cypress (600), sz cl 3  
     &            0.17,0.68,1.65,0.57,1.20,1.66,0.0,0.0,0.0,0.70, 5.83, ! elm - ash - cottonwood (700), sz cl 1
     &            0.17,0.68,1.65,0.57,1.20,1.66,0.0,0.0,0.0,0.70, 5.83, ! elm - ash - cottonwood (700), sz cl 2
     &            0.22,2.15,0.85,0.03,0.05,0.21,0.0,0.0,0.0,0.36, 1.38, ! elm - ash - cottonwood (700), sz cl 3
     &            0.39,0.90,2.88,0.95,2.25,1.96,0.0,0.0,0.0,2.39,13.75, ! maple - beech - birch (800), sz cl 1
     &            0.37,1.03,2.61,0.91,1.46,1.57,0.0,0.0,0.0,2.28,16.74, ! maple - beech - birch (800), sz cl 2
     &            0.33,0.73,1.25,0.54,0.92,1.99,0.0,0.0,0.0,1.71, 8.27, ! maple - beech - birch (800), sz cl 3
     &            0.48,1.66,2.80,0.87,1.70,2.97,0.0,0.0,0.0,2.72,19.61, ! aspen - birch (900), sz cl 1
     &            0.48,1.66,2.80,0.87,1.70,2.97,0.0,0.0,0.0,2.72,19.61, ! aspen - birch (900), sz cl 2
     &            0.52,0.76,2.57,1.15,0.94,0.34,0.0,0.0,0.0,1.34,10.36, ! aspen - birch (900), sz cl 3
     &            0.33,1.08,1.47,0.24,0.49,0.53,0.0,0.0,0.0,1.01, 1.07 / ! nonstocked (999)

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
      CALL FMNEFT(IFFEFT)
C----------
C  LOAD LIVE FUELS
C----------
C
      DO I=1,2
         FLIVE(I)=FULIV(I, 1)
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
C   SET COVTYP TO A DEFAULT BALSAM FIR COVER. AFTER THE
C   FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
C   PRESENT.
C----------
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA',
     &       /1X,'*** COVER TYPE SET TO BALSAM FIR',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 1
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
        FTDEADFU = (IFFEFT-1)*3 + ISZCL
        IF (IFFEFT .EQ. 11) FTDEADFU = 31 ! for non-stocked
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
