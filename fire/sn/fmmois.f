      SUBROUTINE FMMOIS (FMOIS, MOIS)
      IMPLICIT NONE
C----------
C  **FMMOIS  FIRE--SN--DATE OF LAST REVISION:  06/22/05
C----------
C  CALLED FROM: FMBURN, FMPOFL, FMIN, FMTRET
C----------
C  PURPOSE:
C     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS.
C     THE MOISTURE VALUES USED FOR SEVERE AND MODERATE POTENTIAL FIRES
C     (CATEGORIES 1 $ 3) WERE PROVIDED BY BENNIE TERRELL AND GREGG
C     VICKERS.  CATEGORY 2 IS APPROX. HALF WAY BETWEEN CAT 1 & 3. CAT 4
C     IS THE SAME AS IN NI.  
C     DUFF MOISTURE VALUES ARE THE DEFAULTS IN FOFEM. 
C----------
C  CALL LIST DEFINITIONS:
C     FMOIS:   MOISTURE MODEL NUMBER
C     MOIS:    MOISTURE VALUES
C
C  LOCAL VARIABLE DEFINITIONS:
C     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
C           (2,*):LIVE STUFF
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
      LOGICAL DEBUG
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
          MOIS(1,2) = .07    ! 10hr, .25-1"
          MOIS(1,3) = .12    ! 100hr, 1-3"
          MOIS(1,4) = .17    ! 3+
          MOIS(1,5) = .40    ! Duff
          MOIS(2,1) = .55    ! Live woody
          MOIS(2,2) = .55    ! Live herb
C
      ELSEIF (FMOIS .EQ. 2) THEN
C----------
C  DRY
C----------
          MOIS(1,1) = .06
          MOIS(1,2) = .08
          MOIS(1,3) = .13
          MOIS(1,4) = .18
          MOIS(1,5) = .75
          MOIS(2,1) = .80
          MOIS(2,2) = .80
C
      ELSEIF (FMOIS .EQ. 3) THEN
C----------
C  WET
C----------
          MOIS(1,1) = .07
          MOIS(1,2) = .09
          MOIS(1,3) = .14
          MOIS(1,4) = .20
          MOIS(1,5) = 1.0
          MOIS(2,1) = 1.0
          MOIS(2,2) = 1.0          
C
      ELSEIF (FMOIS .EQ. 4) THEN
C----------
C  VERY WET
C----------
          MOIS(1,1) = .16
          MOIS(1,2) = .16
          MOIS(1,3) = .18
          MOIS(1,4) = .50
          MOIS(1,5) = 1.75
          MOIS(2,1) = 1.5
          MOIS(2,2) = 1.5          
C
      ENDIF
C
      RETURN
      END

