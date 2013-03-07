      SUBROUTINE FMGFMV(IYR, IFMD)
      IMPLICIT NONE
C----------
C  **FMGFMV  FIRE--DATE OF LAST REVISION:  02/08/13
C----------
*  PURPOSE:
*     THIS SUBROUTINE LOADS THE FUEL MODEL PARAMETER VALUES FOR
*     THE FUEL MODEL PASSED AS IFMOD.
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*    IYR:      CALENDAR YEAR
*    IFMD:     FUEL MODEL NUMBER
*
*  LOCAL VARIABLE DEFINITIONS:
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'

C     LOCAL VARIABLE DECLARATIONS

      INTEGER  IYR,IFMD,I,J,IRTNCD
      LOGICAL  DEBUG
      REAL     SUMPS
      REAL     X(2), Y(2), ALGSLP, WT
C-----------
C     FIXED VALUES FOR INTERPOLATION FUNCTION
C-----------
      DATA     Y / 0.0, 1.0 /

C----------
C     BEGIN ROUTINE
C  INITIALIZE VARIABLES
C----------
      DO I = 1,2
        DO J = 1,4
          MPS(I,J)=0
        ENDDO
C
        DO J = 1,7
          FWG(I,J)=0.0
        ENDDO
      ENDDO
C
      MEXT(1)=0.0
      MEXT(2)=0.0
      MEXT(3)=0.0
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMGFMV',6,1)
      IF (DEBUG) WRITE(JOSTND,7)
    7 FORMAT(' ENTERING ROUTINE FMGFMV ')
C
C     Number of fuel categories:
C     ND: 0=no dead, 1=1hr, 2=10hrs, 3=100hrs. NL: 0=No live, 1=live
C        Even though these values are saved in NUMD and NUML,
C        we are going to recalculate them here (because the user may
C        have changed them in the keyword DEFULMOD). If FWG > 0 then
C        the fuel category is present.
C
      ND    = 0
      NL    = 0
      SUMPS = 0.0

C     SET DEAD HERB SAV TO LIVE HERB SAV
      SURFVL(IFMD,1,4) = SURFVL(IFMD,2,2)

      DO I = 1,2
        DO J = 1,4
          MPS(I, J) = SURFVL(IFMD,I,J)
          FWG(I, J) = FMLOAD(IFMD,I,J)
        ENDDO
      ENDDO

C     PUT SOME OF THE LIVE HERBS INTO THE DEAD HERB CATEGORY (BASED ON
C     MOISTURE) FOR THE DYNAMIC FUEL MODELS (not fm 2)
      IF ((FMLOAD(IFMD,2,2) .GT. 0) .AND. (IFMD .NE. 2) .AND.
     &     (MOIS(2,2) .LT. 1.2)) THEN
    	  X(1) =  .30
        X(2) =  1.2
        WT = ALGSLP(MOIS(2,2),X,Y,2)
        FWG(1,4) = (1 - WT)*FMLOAD(IFMD,2,2)
        FWG(2,2) = WT*FMLOAD(IFMD,2,2)
      ENDIF

      DO I = 1,2
        DO J = 1,4
          IF (I .EQ. 1 .AND. FWG(I,J) .GT. 0.0) ND = ND + 1
          IF (I .EQ. 2 .AND. FWG(I,J) .GT. 0.0) NL = NL + 1
          SUMPS = SUMPS + MPS(I,J)
        ENDDO
      ENDDO
      DEPTH   = FMDEP(IFMD)
      MEXT(1) = MOISEX(IFMD)
C
C     CHECK FOR ERRORS THAT CAN HAPPEN IF THE USER WANTED TO USE A FUEL
C     MODEL (FUELMODL) THAT HAS NOT YET BEEN DEFINED OR HAS BEEN INCOMPLETELY
C     DEFINED.
C
C     IF THERE ARE NO LIVE OR DEAD CLASSES, OR IF THE SURFACE TO VOLUME
C     RATIO IS <=0 FOR ALL CLASSES. OR IF DEPTH IS ZERO, THEN SOMETHING
C     IS SERIOUSLY WRONG. EXIT WITH AN ERROR CODE.
C
      IF (DEBUG) WRITE(JOSTND,9) ND, NL, SUMPS, DEPTH
    9 FORMAT(' FMGFMV, ND=',I2,' NL=',I2,' SUMPS=',F10.3,'DEPTH=',F7.3)

      IF ((ND .LE. 0 .AND. NL .LE. 0) .OR. SUMPS .LE. 0.0 .OR.
     >    DEPTH .LE. 0.0) THEN
        WRITE(JOSTND,'("/ ")')
        WRITE(JOSTND,'("/ *** FFE: FATAL PROBLEM: YEAR = ", I4)') IYR
        WRITE(JOSTND,'("/ *** FFE: FUEL MODEL = ", I2)') IFMD
        WRITE(JOSTND,'("/ *** FFE: NO LIVE OR DEAD CLASSES, NO",
     >    " FUEL SURF/VOL, OR NO DEPTH DEFINED")')
        WRITE(JOSTND,'("/ *** FFE: CHECK ""DEFULMOD"" AND",
     >    " ""FUELMODL"" KEYWORD USE")')
        WRITE(JOSTND,'("/ *** FFE: EXITING")')
        CALL ERRGRO(.FALSE.,4)
        CALL fvsGetRtnCode(IRTNCD)
        IF (IRTNCD.NE.0) RETURN
      ENDIF
C
C     ADD THE DEPTH MODIFIER BASED ON ACTIVITY-RELATED FUEL TREATMENTS
C     SEE **FMUSRFM** FOR CALCULATION OF DPMOD
C
      DEPTH = DEPTH * DPMOD

      RETURN
      END
