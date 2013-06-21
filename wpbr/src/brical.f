      SUBROUTINE BRICAL
C**********************************************************************
C  **BRICAL       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRIBA calculates the Rust Index based on exposure time.
C
C  The functions used to determine the rust index from exposure time were
C  provided by Geral McDonald, Intermountain Research Station, Moscow,
C  ID.
C
C  Rust Index (RI) is calculated once per cycle. The function has four
C  parameters and utilizes stand age as the exposure time variable.
C  The parameters are:
C     MINRI - minumum Rust Index
C     MAXRI - maximum Rust Index
C     PKAGE - stand age (exposure time) when Rust Index peaks
C     RISHP - defines shape of curve about the peak
C
C  Called from:
C     BRTREG depending on RI assignment method.
C
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'

C.... Local variable declarations.

      REAL BRI
      INTEGER NYEAR, NAGE
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRICAL',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT('Entering subroutine BRICAL: cycle = ',I2)

C.... Get current year and age.

      NYEAR = IY(ICYC+1)
      NAGE = IAGE + NYEAR - IY(1)

      IF (RIMETH.EQ.3) THEN
C....    Calculate Rust Index with Gaussian equation.
C....    This equation provides a normal curve around the peak age.
         BRI = MINRI + MAXRI * EXP(-0.5 * ((NAGE - PKAGE)/PKSHP)**2)

      ELSEIF (RIMETH.EQ.4) THEN
C....    Calculate Rust Index with log function.
C....    This equation provides a skewed curve around the peak age
C....    so that a greater number of cankers can be attained early.
         BRI = MINRI + MAXRI * EXP(-0.5*(LOG(NAGE - PKAGE)/PKSHP)**2)
      
      ENDIF
      
      RIDEF=BRI

      IF(DEBUG) WRITE(JOSTND,60) RIDEF,RIMETH,NAGE,ICYC
   60 FORMAT(27X,'RIDEF = ',F10.8,' RIMETH=',I1,' NAGE=',I4,/,
     &       ' Leaving subroutine BRICAL: cycle = ',I2)
      RETURN
      END
