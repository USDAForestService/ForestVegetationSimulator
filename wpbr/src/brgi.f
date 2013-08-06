      SUBROUTINE BRGI(IIAG,HHT,GIBR,TBSUM)
C**********************************************************************
C  **BRGI         DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRGI calculates the growth index and sum target for individual white
C  pine trees (currently used for all species).
C  Minimum age of tree is set at 2 years for GI calculation.
C----------------------------------------------------------------------
C
C  IIAG - Tree age
C  HHT  - Tree height in meters
C  GIBR - Growth Index function result
C  TBSUM - Target area
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-SEP-2000 Lance David (FHTET)
C     No changes actually made to this routine that calculates growth
C     index for white pine trees. The July, 2000 update to expand the
C     model to function on additional pine species did not address the
C     differences between species; therefore, it will likely be
C     necessary to develop species-specific sections and add species to
C     the argument list. FVS variant-specific versions of this may also
C     be necessary or desirable for future development.
C  28-FEB-2001 Lance R. David (FHTET)
C     Minimum tree age used in GI calculation set at 2 years.
C     Explicitly defined variables.
C  22-MAR-2001 Lance R. David (FHTET)
C     Added debug statment. Removed "0.0" preceding negative values in 
C     GI functions. Don't know why they were there in the first place,
C     but may have been needed for some earlier version compiler.
C  03-MAY-2001 Lance R. David (FHTET)
C     Disabled stand target area summing because it was already being
C     done in BRTREG and BRTARG.
C  07-NOV-2002 Lance R. David (FHTET)
C     Added max and min limits of 50 and 125 on growth index GIBR.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'

C.... Local variable declarations.

      LOGICAL DEBUG
      INTEGER IIAG, GIAGE, K
      REAL    HHT, GIBR, GIBRK, HITE, SVAL, TBSUM

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRGI',4,ICYC)
      IF(DEBUG) WRITE(JOSTND,111) ICYC,IIAG,HHT
  111 FORMAT('Entering subroutine BRGI: cycle = ',I2,/,
     &       27X,'IIAG=',I3,' HHT=',F6.2)

      TBSUM=0.0

C.... Calculate growth index.  This equation taken from the abstract:
C.... "Measuring Early Performance of Second Generation Resistance to
C.... Blister Rust in White Pine"; Geral McDonald, et.al.
C.... Per Geral's recommendation, minimum age is set at 2 years.

      IF(IIAG .LE. 2) THEN
        GIAGE = 2
      ELSE
        GIAGE = IIAG
      ENDIF

      GIBR=0.466*(HHT-0.05)*
     &     ((1-(1.024494*EXP((-0.024202)*GIAGE)))**(-2.071822))

C.... Min (15.24m = 50ft*.3048) and max 38.10m = 125ft limits imposed
C.... on growth index. feet to meter conversion is m=ft*0.3048. 

      IF(GIBR .LT. 15.24) GIBR = 15.24
      IF(GIBR .GT. 38.10) GIBR = 38.10

      IF(DEBUG) WRITE(JOSTND,*) ' IN BRGI: GIBR=',GIBR
C.... Calculate stand target area.

      DO 40 K=1,IIAG
         GIBRK=0.466*((1-(1.024494*EXP((-0.024202)*K)))**
     &      (-2.071822))
         HITE=0.05+GIBR/GIBRK

         CALL BRSTAR(HITE,SVAL)
C****    STSUM=STSUM+SVAL        !!! already occuring in BRTREG and BRTARG
         TBSUM=TBSUM+SVAL
  40  CONTINUE

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,113) ICYC
  113 FORMAT('Leaving subroutine BRGI: cycle = ',I2)
      RETURN
      END
