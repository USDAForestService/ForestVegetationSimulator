      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-AK-DATE OF LAST REVISION:  12/23/14
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN

*  Purpose:
*     Find the dominant species (by basal area). Set the initial live
*     and dead fuel values as well. The dead fuels are only initialized
*     in the first year, but COVTYP and the live fuels must be done
*     each year.
*
*
*  Local variable definitions:
*     BAMOST:  The highest basal area in a single species
*     CAREA:   The area covered by the crown at its widest point (sqft)
*     COVINI  The seral cover type to be used for initiating fuels in
*              bare stands
*     CRL:     Crown length
*     CWIDTH:  The maximum width of the crowns (ft)
*     FUINI:   The initial fuel loadings by FIA forest type
*     FULIVE:  The herb/shrub/regen for established stands (from JBrown)
*     FULIVI:  The herb/shrub/regen for initiating stands (from JBrown)
*     ISWTCH:  =1 if called by SVSTART
*              =0 if called by any other subroutine (FMMAIN, FMPPHV)
*     TOTBA:   The total basal area in the stand (used in the fuels calcs)
*     TOTCRA:  The sum of the area of the crowns, per acre (sqft)
*
*  Common block variables and parameters:
*
***********************************************************************

C     Parameter statements.

C     Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     Common include files.
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C     Variable declarations.

      REAL      BAMOST, TOTCRA, CWIDTH
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINI(MXFLCL,6)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2),DCYMLT, FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG

      INTEGER MYACT(3), FTDEADFU

      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      INTEGER ICT(MAXSP),IFGS,IWET
      REAL    BIGDBH,TOTBA,XX,CAREA,ALGSLP,PRCL,ADD

C     BREAKPOINTS (% CC) OF INTERPOLATION FUNCTION TO PROVIDE WEIGHTED
C     ESTIMATE OF LIVE (EVERY TIMESTEP) AND DEAD (INITIAL) FUEL

      DATA XCOV / 10.0, 60.0 /

C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     (TAKEN FROM PN-FFE)
C                  herbs, shrubs
      DATA FULIVE /
     >             0.30, 0.20, ! 1 = white spruce - use PN Engelmann spruce
     >             0.20, 0.20, ! 2 = western redcedar
     >             0.15, 0.10, ! 3 = Pacific silver fir 
     >             0.15, 0.20, ! 4 = mountain hemlock   
     >             0.20, 0.20, ! 5 = western hemlock    
     >             0.20, 0.20, ! 6 = Alaska cedar    
     >             0.20, 0.10, ! 7 = lodgepole pine  
     >             0.30, 0.20, ! 8 = Sitka spruce 
     >             0.15, 0.10, ! 9 = subalpine fir      
     >             0.20, 0.20, !10 = red alder    
     >             0.25, 0.25, !11 = black cottonwood - use QA Ottmar and others 2000b   
     >             0.25, 0.25, !12 = other hardwoods - use QA Ottmar and others 2000b             
     >             0.15, 0.10/ !13 = other softwoods (use PSF)


C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
C     (TAKEN FROM PN-FFE)
C                  herbs, shrubs
      DATA FULIVI /
     >             0.30, 2.00, ! 1 = white spruce - use PN Engelmann spruce
     >             0.40, 2.00, ! 2 = western redcedar
     >             0.30, 2.00, ! 3 = Pacific silver fir
     >             0.30, 2.00, ! 4 = mountain hemlock     
     >             0.40, 2.00, ! 5 = western hemlock     
     >             0.40, 2.00, ! 6 = Alaska cedar
     >             0.40, 1.00, ! 7 = lodgepole pine     
     >             0.30, 2.00, ! 8 = Sitka spruce      
     >             0.30, 2.00, ! 9 = subalpine fir 
     >             0.40, 2.00, !10 = red alder
     >             0.18, 1.32, !11 = black cottonwood - use QA Ottmar and others 2000b     
     >             0.18, 1.32, !12 = other hardwoods - use QA Ottmar and others 2000b      
     >             0.30, 2.00/ !13 = other softwoods (use PSF)

C     INITIAL FUEL LOADING BASED ON FIA FOREST TYPE CODE
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD)
C     <.25 to1  1-3 3-6  6-12 12-20 20-35 35-50 >50 Lit Duf
      DATA FUINI /
     &0.12, 0.24, 0.56,0.27,1.00, 2.61, 0.0, 0.0, 0.0, 2.95, 39.20, ! 1 = mountain hemlock
     &0.12, 0.24, 0.42,0.46,1.54, 5.81, 0.0, 0.0, 0.0, 3.23, 62.46, ! 2 = Alaska yellow cedar
     &0.23, 0.46, 1.14,0.66,2.01,22.47, 0.0, 0.0, 0.0, 4.53, 50.75, ! 3 = western hemlock
     &0.41, 0.72, 2.23,1.44,5.14,15.79, 0.0, 0.0, 0.0, 4.94, 30.62, ! 4 = sitka spruce 
     &0.22, 0.56, 1.25,0.07,0.11, 0.02, 0.0, 0.0, 0.0, 3.01,  8.61, ! 5 = cottonwood       
     &0.22, 0.47, 1.62,0.62,2.88, 7.99, 0.0, 0.0, 0.0, 5.35, 46.41/ ! 6 = others       

      DATA MYACT / 2521, 2548, 2553 /

C     CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)

C     BEGIN ROUTINE. ZERO OUT THE CUMMULATIVE VARIABLES

      COVTYP = 0
      PERCOV  = 0.0
      BIGDBH  = 0.0
      TOTBA   = 0.0

C     LOOP THROUGH THE TREE LIST

      IF (ITRN .GT. 0) THEN

C       ZERO OUT THE CUMMULATIVE VARIABLES

        BAMOST = 0.0
        TOTCRA = 0.0

        DO KSP = 1,MAXSP
          FMTBA(KSP) = 0.0
        ENDDO

        DO I = 1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN

            KSP = ISP(I)

            FMTBA(KSP) = FMTBA(KSP) +
     &                   FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)

C           CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
C           ENCOMPASSED BY ALL TREES

            CWIDTH=CRWDTH(I)

            CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF

C         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.

          CURKIL(I) = 0.0

        ENDDO

C       CALCULATE TOTAL COVER TYPE

        DO KSP = 1,MAXSP
          TOTBA = TOTBA + FMTBA(KSP)
        ENDDO

C       DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
C       COVTYP    - FFE-WIDE DOMINANT

        CALL RDPSRT(MAXSP,FMTBA,ICT,.TRUE.)
        IF (FMTBA(ICT(1)) .GT. 0.001) COVTYP = ICT(1)


C       Use the crown width information to determine the percent cover
C       of the stand. Use the equation sent by Nick which assumes that
C       crowns are randomly distrubuted in the stand:

C       PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

        PERCOV = 100. * (1.0 - EXP(-TOTCRA/43560.))

      ENDIF


C     IF THERE ARE NO TREES (COVTYP=0) IN CYCLE 1,
C     ISSUE A WARNING AND USE A DEFAULT WH COVER. AFTER THE
C     FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
C     PRESENT.

      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA:',
     &       /1X,'*** COVER TYPE SET TO WESTERN HEMLOCK',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 5
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF
      ENDIF

      OLDCOVTYP = COVTYP

C     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.

      DO I = 1,2  ! herbs, shrub loop
        YLOAD(1) = FULIVI(I,COVTYP)
        YLOAD(2) = FULIVE(I,COVTYP)
        FLIVE(I) = ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO

      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >    2F6.3)

C     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION

      IF (IYR .EQ. IY(1)) THEN

C----------
C  LOAD DEAD FUELS AS A FUNCTION OF FOREST TYPE.
C----------
         SELECT CASE (IFORTP)
         CASE(270)          
           FTDEADFU = 1 !mountain hemlock
         CASE(271)        
           FTDEADFU = 2 !alaska yellow cedar
         CASE(301)          
           FTDEADFU = 3 !western hemlock
         CASE(305)          
           FTDEADFU = 4 !sitka spruce
         CASE(703)          
           FTDEADFU = 5 !cottonwood
         CASE DEFAULT
           FTDEADFU = 6 !others
         END SELECT

         DO ISZ = 1,MXFLCL
            STFUEL(ISZ,2) = FUINI(ISZ,FTDEADFU)
            STFUEL(ISZ,1) = 0
         ENDDO
        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

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

C           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
          ELSE                       
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

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

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

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


C       DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C       OF BASAL AREA IN THE STAND. ASSUME THE FUELS ARE UNPILED AND HARD.
C       THIS LAST ASSUMPTION MAY CHANGE IN THE FUTURE.

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

      ENDIF

      ENTRY SNGCOE

C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

      RETURN
      END
