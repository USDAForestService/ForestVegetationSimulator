      SUBROUTINE BMFDBK (IYR)

C CALLED BY: BMDRV
C CALLS: nothing

***********************************************************************
*  BMFDBK Date of last revision Feb 5,1999
*         Author: Matthew Oberle (MJO May98)
*
*         Removed code for feedback output file and experimental
*         code for epidemic-endemic switching (MJO Feb99)
***********************************************************************
*
* Purpose:
*     If the landscape BKP from (LAG) year(s) ago is high enough
*     to trigger a predator/parasite/pathogen outbreak, then a 
*     negative feedback multiplier is calculated and applied to
*     each stand's BKP.
*
* Call list definitions:
*
*     IYR: Simulation year
*
* Local variable definitions:
*
*     ISTD:   Loops counter over stands within the landscape.
*     SUMBKP: The sum of the BKP of all stands within the landscape.
*             An intermiediate variable used in the calculation of the 
*             current year's approximate mean landscape BKP (LSBKP0).
*     LSBKP0: Approximate mean landscape BKP this year.
*     LSBKPN: Approximate mean landscape BKP from (LAG) year(s) ago.
*     NEGFED: Negative feedback modifier for each stand.
*
*
* Common block variables and parameters:
*
*     BMSTND: From BMCOM; number of stands in this landscape.
*     BKP(ISTD): From BMCOM; array containing the BKP (pre-flight, 
*                annual)in each stand of the landscape.
*     TFDBK:  From BMCOM; Level of LSBKPN that triggers negative feedback.
*             Field 1 in the BMFDBK keyword.
*     SFDBK:  From BMCOM; Modifies the severity of the negative
*             feedback. Field 2 of the BMFDBK keyword.
*     LSBKPA: From BMCOM; Used to initialize model with historical beetle
*             population levels. It is the mean landscape BKP from last 
*             year, and is field 3 in BMFDBK keyword.
*     LSBKPB: From BMCOM; Similar to LSBKPA, but for 2 years before the 
*             start of the simulation.  It is field 4 in the BMFDBK
*             keyword.
*     LAG:    From BMCOM; Time lag (0,1,or 2 years) after reaching the
*             trigger population level (TFDBK) before negative feedback
*             is invoked. Field 5 of the BMFDBK keyword.
*
***********************************************************************


C...Common include files:

      INCLUDE 'BMPRM.F77'
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMCOM.F77'

C...Variable declarations:

      INTEGER ISTD
      REAL SUMBKP,LSBKP0,LSBKPN,NEGFED

C...Clear some variables

      SUMBKP = 0.0
      NEGFED = 0.0

C...When stand BKP levels are very low, movement of BKP out of the stand
C   (by BMATCT.F) may result in negative BKP values. Although this is
C   caught and corrected elsewhere in the model (i.e. BMISTD.F), it
C   needs to be done here as well.

      DO 100 ISTD = 1, BMSTND
          IF (BKP(ISTD) .LT. 0) THEN
              BKP(ISTD) = 0
          ENDIF
  100 CONTINUE 

C...Find this year's approximate mean landscape BKP (doesn't adjust for
C   stand size ... assumes stands within the landscape are all the same
C   size). Also, this counts non-stockable stands.

      DO 200 ISTD = 1, BMSTND

      SUMBKP = SUMBKP + BKP(ISTD)

  200 CONTINUE

      LSBKP0 = SUMBKP / BMSTND

C...Uses LAG to set LSBKPN equal to the landscape BKP from the
C   appropriate year.

      IF (LAG .EQ. 0) LSBKPN = LSBKP0
      IF (LAG .EQ. 1) LSBKPN = LSBKPA
      IF (LAG .EQ. 2) LSBKPN = LSBKPB
      IF ((LAG .NE. 0) .AND. (LAG .NE. 1) .AND. (LAG .NE. 2)) THEN
          WRITE (*,*) 'ERROR: FEEDBACK LAG VALUE IS INCORRECT'
      ENDIF 

C...If the landscape BKP from (LAG) year(s) ago is high enough to
C   trigger a predator/parasite/pathogen outbreak, then initiate
C   negative feedback...

      IF (LSBKPN .GT. TFDBK) THEN
          NEGFED = (TFDBK / LSBKPN) / SFDBK
          IF (NEGFED .GT. 1.0) NEGFED = 1.0

C...and apply the negative feedback multiplier to all stands in the
C   landscape

      DO 800 ISTD = 1, BMSTND
C
C...TROUBLESHOOT, AJM 11/99
C
      WRITE(88,810) IYR, ISTD, BKP(ISTD), NEGFED
  810  FORMAT(I4, 2X, I9, 2X, F6.3, 2X, F6.3)
C
	  BKP(ISTD) = BKP(ISTD) * NEGFED
  
  800 CONTINUE
      ENDIF 

C..."Age" landscape BKP by one year

      LSBKPB = LSBKPA
      LSBKPA = LSBKP0 

      RETURN
      END
