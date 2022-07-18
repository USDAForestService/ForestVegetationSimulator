      SUBROUTINE FMMOIS (FMOIS, MOIS)
      IMPLICIT NONE
C----------
C FIRE-UT $Id$
C----------
C
C     CALLED FROM: FMBURN
C----------------------------------------------------------------------
C  PURPOSE:
C     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS
C----------------------------------------------------------------------
C
C  CALL LIST DEFINITIONS:
C     FMOIS:   MOISTURE MODEL NUMBER
C     MOIS:    MOISTURE VALUES
C
C  LOCAL VARIABLE DEFINITIONS:
C     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
C           (2,*):LIVE STUFF
C
C  COMMON BLOCK VARIABLES AND PARAMETERS:
C

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

C.... COMMON INCLUDE FILES.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'


C     LOCAL VARIABLE DECLARATIONS

      INTEGER  FMOIS
      REAL     MOIS(2,5)
      LOGICAL  DEBUG
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMMOIS',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)

C     BEGIN ROUTINE

      IF (FMOIS .EQ. 0) THEN
         RETURN

      ELSEIF (FMOIS .EQ. 1) THEN

C         "very low moisture" / wildfire

          MOIS(1,1) = 0.04
          MOIS(1,2) = 0.04
          MOIS(1,3) = 0.05
          MOIS(1,4) = 0.10
          MOIS(1,5) = 0.15
          MOIS(2,1) = 0.70 ! live woody
          MOIS(2,2) = 0.70 ! live herb         

      ELSEIF (FMOIS .EQ. 2) THEN

C         "low moisture"

          MOIS(1,1) = 0.05
          MOIS(1,2) = 0.06
          MOIS(1,3) = 0.08
          MOIS(1,4) = 0.15
          MOIS(1,5) = 0.50
          MOIS(2,1) = 0.90
          MOIS(2,2) = 0.90          

      ELSEIF (FMOIS .EQ. 3) THEN

C         "moderate moisture"

          MOIS(1,1) = 0.08
          MOIS(1,2) = 0.10
          MOIS(1,3) = 0.12
          MOIS(1,4) = 0.16
          MOIS(1,5) = 1.25
          MOIS(2,1) = 1.20
          MOIS(2,2) = 1.20

      ELSEIF (FMOIS .EQ. 4) THEN

C         "high moisture"

          MOIS(1,1) = 0.10
          MOIS(1,2) = 0.12
          MOIS(1,3) = 0.15
          MOIS(1,4) = 0.18
          MOIS(1,5) = 2.00
          MOIS(2,1) = 1.40
          MOIS(2,2) = 1.40

      ENDIF

      RETURN
      END

