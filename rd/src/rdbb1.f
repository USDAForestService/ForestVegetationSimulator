      SUBROUTINE RDBB1
C----------
C  **RDBB1       LAST REVISION:  12/10/07
C---------- 
C
C  Purpose :
C     Determines how many races of bark beetle type 1 are eligible to
C     act in this growth cycle under current stand conditions.  The
C     mortality rates etc. specified for each beetle race that is active
C     are reported back to RDOAGM.  By default, each race  that was
C     scheduled for this time period is rescheduled for the next time
C     period (whether it was active this time or not, unless the user
C     specifies otherwise).
C
C     Keyword:  BBTYPE1
C     Inputs:  1 = first outbreak year    2 = host tree species
C              3 = DBH limit              4 = threshold density of
C                                             lg.enough host
C              5 = mortality rate         6 = 0 for reschedule, 1 for
C                                             don't
C
C     Keyword:  DNSCALC
C     Inputs:  1 = 0 for actual trees/acre count, 1 for SDI calculation
C              2 = 0 for whole stand, 1 for outside only
C              3 = 0 for live only, 1 for live + died during last 2 years
C              4 = slope of SDI function (between -0.5 and -2.5)
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [FVS]
C     OPFIND  (SUBROUTINE)   [FVS]
C     OPGET   (SUBROUTINE)   [FVS]
C     OPDONE  (SUBROUTINE)   [FVS]
C     OPCOPY  (SUBROUTINE)   [FVS]
C     OPREDT  (SUBROUTINE)   [FVS]
C     OPINCR  (SUBROUTINE)   [FVS]
C     RDCSD   (SUBROUTINE)   [FVS]
C
C  Revision History:
C    21-MAR-00 Lance David (FHTET)
C       Reduced RETURN statements to 1 at the end of routine.
C       Added Debug code.
C    06-AUG-01 Lance R. David (FHTET)
C       Change MYACT to IACTK in call to OPCOPY because of array to scalar
C       mismatch reported by LF95.
C    10-DEC-07 Lance R. David (FHTET)
C       Update to argument list in call to OPCOPY, added variable
C       DONE for debug/tracking (from Don Robinson).
C....................................................................

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations.

      LOGICAL   CRIT, DEBUG
      INTEGER   ISPI, ONLY1X, RACE, NOCOPY, DONE, IACTK
      REAL      DBHLIM, THRESH, MORT

      DIMENSION MYACT(1), PRMS(5)
      
C.... Data statements.

      DATA MYACT /2415/

C.... SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'RDBB1',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDBB1'

      IF (ITRN .LE. 0) GOTO 1000
      NOCOPY = 0
      DONE = 0

C.... Ask OPFIND how many races of this beetle type are scheduled for
C.... this time period.

      CALL OPFIND (1,MYACT,NTODO)
      IF (NTODO .LE. 0) GOTO 1000
      
C.... For each race, first get the parameter values.  Check that the
C.... beetle host species exists before proceeding.

      DO 999 RACE = 1,NTODO
         CALL OPGET (RACE,5,KDT,IACTK,NPS,PRMS)

         IF (DEBUG) WRITE (JOSTND,*)
     &      'IN RDBB1: RACE=',RACE,' PRMS=',PRMS

         ISPI = INT(PRMS(1))
         DBHLIM = PRMS(2)
         THRESH = PRMS(3)
         MORT = PRMS(4)
         ONLY1X = INT(PRMS(5))
              
         IF (ISCT(ISPI,1) .EQ. 0) GOTO 888

C....    Second, call RDCSD to calculate the density of trees that
C....    meet the user's criteria, and indicate via CRIT whether
C....    this density exceeds the threshold for an active outbreak.

         CRIT = .FALSE.
         CALL RDCSD(ISPI,DBHLIM,THRESH,CRIT)
         IF (DEBUG) WRITE (JOSTND,*) 'IN RDBB1: CRIT=',CRIT
         IF (.NOT. CRIT) GOTO 888
        
C....    If the beetle is active, call OPDONE to signal the outbreak,
C....    increment NUMBB and store the specified mortality rates for
C....    later use in killing trees.  Reschedule the beetle race by
C....    copying it to the next growth cycle, unless restricted by
C....    the user.

         CALL OPDONE (RACE,IY(ICYC))
         DONE = 1

         NUMBB = NUMBB + 1
         IF (NUMBB .GT. 3*MAXSP) NUMBB = 3*MAXSP
         HOST(NUMBB) = ISPI
         MINDBH(NUMBB) = DBHLIM
         MININF(NUMBB) = 0.0
         ORATE(NUMBB) = MORT
         OFRATE(NUMBB) = MORT
         IURATE(NUMBB) = MORT
         IIRATE(NUMBB) = MORT

         IF (DEBUG) WRITE (JOSTND,*) 'IN RDBB1: NUMBB=',NUMBB
                
C....    Check whether the Type 1 beetle races are to remain
C....    potentially active after the current growth cycle.

  888    CONTINUE

         IF (ONLY1X .EQ. 1) NOCOPY = 1

  999 CONTINUE
  
C.... If the Type 1 beetle races are to remain active, then copy them
C.... to the next growth cycle.

      IF (NOCOPY .NE. 1) THEN 
         CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
         CALL OPINCR (IY,ICYC,NCYC)
      ENDIF

 1000 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDBB1'
      RETURN
      END
