      SUBROUTINE FMMOIS (FMOIS, MOIS)
      IMPLICIT NONE
C----------
C  **FMMOIS  FIRE--NE--DATE OF LAST REVISION:  05/02/06
C----------
C  CALLED FROM: FMBURN, FMPOFL, FMIN, FMTRET
C----------
C  PURPOSE:
C     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS USED WITH 
C     SIMULATED FIRES AND POTENTIAL FIRES.
C     NOTE - THESE MOISTURE VALUES WERE TAKEN FROM LS-FFE.
C     THE MOISTURE VALUES WERE PROVIDED BY JEREMY BENNETT, MENOMINEE 
C     TRIBE, FROM WEATHER STATION DATA FROM WI, MI, AND MN.  
C     DUFF MOISTURE VALUES ARE THE DEFAULTS IN FOFEM. 
C----------
C  CALL LIST DEFINITIONS:
C     FMOIS:   MOISTURE MODEL NUMBER
C     MOIS:    MOISTURE VALUES
C
C  LOCAL VARIABLE DEFINITIONS:
C     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
C           (2,*):LIVE STUFF:  live woody, live herb
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C----------
C  LOCAL VARIABLE DECLARATIONS
C----------
      INTEGER  FMOIS
      REAL     MOIS(2,5)
      LOGICAL  DEBUG
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMMOIS',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
  7   FORMAT(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)
C----------
C  BEGIN ROUTINE
C----------
      IF (FMOIS .EQ. 0) THEN
         RETURN
C
      ELSEIF (FMOIS .EQ. 1) THEN
C----------
C  VERY DRY
C----------
          MOIS(1,1) = .05    ! 1hr, 0-.25"
          MOIS(1,2) = .08    ! 10hr, .25-1"
          MOIS(1,3) = .12    ! 100hr, 1-3"
          MOIS(1,4) = .15    ! 3+
          MOIS(1,5) = .40    ! Duff
          MOIS(2,1) = .89    ! Live woody	
          MOIS(2,2) = .60    ! Live herb
C
      ELSEIF (FMOIS .EQ. 2) THEN
C----------
C  DRY
C----------
          MOIS(1,1) = .07
          MOIS(1,2) = .09
          MOIS(1,3) = .14
          MOIS(1,4) = .17
          MOIS(1,5) = .75
          MOIS(2,1) = 1.05
          MOIS(2,2) =  .82          
C
      ELSEIF (FMOIS .EQ. 3) THEN
C----------
C  MOIST
C----------
          MOIS(1,1) = .10
          MOIS(1,2) = .13
          MOIS(1,3) = .17
          MOIS(1,4) = .21
          MOIS(1,5) = 1.0
          MOIS(2,1) = 1.35
          MOIS(2,2) = 1.16          
C
      ELSEIF (FMOIS .EQ. 4) THEN
C----------
C  WET
C----------
          MOIS(1,1) = .19
          MOIS(1,2) = .29
          MOIS(1,3) = .22
          MOIS(1,4) = .25
          MOIS(1,5) = 1.75
          MOIS(2,1) = 1.40
          MOIS(2,2) = 1.20          
C
      ENDIF
C
      RETURN
      END

