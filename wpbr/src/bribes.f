      SUBROUTINE BRIBES(REDFAC,LREDF)
C**********************************************************************
C  **BRIBES       DATE OF LAST REVISION:  12/14/2000
C----------------------------------------------------------------------
C  Purpose:
C  BRIBES calculates the reduction factor to be applied to the each
C  tree's RI as a result of the change in ribes populations, if both
C  old and new population values are present.  If only new population
C  values are present or the cycle is 0, a new stand rust index (RIDEF)
C  is calculated.
C----------------------------------------------------------------------
C
C  RIBUS(1,x) old number of ribes bushes/acre
C  RIBUS(2,x) new number of ribes bushes/acre
C
C  Revision History:
C
C  13-DEC-2000 Lance R. David (FHTET)
C     Added a divisor in calculation of rust index (see not at equation).
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

C.... Local variable declarations.

      REAL NOLD
      LOGICAL DEBUG,LREDF

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRIBES',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT(' Entering subroutine BRIBES: cycle = ',I2)

C.... Initializations

      REDFAC=0.0
      LREDF=.FALSE.

C.... Sum RI values for old and new number of bushes per acre.

      DO 40 IBUSH=1,2
         RIBSUM(IBUSH)=0.0

C....    Cycle the 3 ribes species through the 'baseline' function
C....    for estimating rust index from the ribes species.

         DO 30 ITYP=1,3
            BRCON=RSF(ITYP)
            FACTOR(ITYP)=(BRCON*
     &       (0.499675+(0.4*ATAN((RIBUS(IBUSH,ITYP)/150)-3))))/0.2652
            RIBSUM(IBUSH)=RIBSUM(IBUSH)+FACTOR(ITYP)
C
C           The divisor 0.2652 was added to the above equation based
C           on testing performed by Geral McDonald and John Schwandt
C           that showed the function generated results too high by a
C           factor of 3.78. 13-DEC-2000 Lance R. David
C

   30    CONTINUE
   40 CONTINUE

C.... Calculate reduction factor or set new stand rust index.

      NOLD=RIBUS(1,1)+RIBUS(1,2)+RIBUS(1,3)

      IF(NOLD.EQ.0.0.OR.ICYC.EQ.0) THEN
         RIDEF=RIBSUM(2)
      ELSE
         REDFAC=RIBSUM(2)/RIBSUM(1)
         LREDF=.TRUE.
      ENDIF

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,201) ICYC
  201 FORMAT (' Leaving subroutine BRIBES: cycle = ',I2)
      RETURN
      END
