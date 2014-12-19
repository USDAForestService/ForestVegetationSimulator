      SUBROUTINE RDBB3
      IMPLICIT NONE
C----------
C  **RDBB3       LAST REVISION:  08/26/14
C----------
C
C  Purpose :
C     Determines how many races of bark beetle type 3 are eligible to
C     act in this growth cycle under the current stand conditions.
C     The mortality rates etc. specified for each beetle race that is
C     active are reported back to RDOAGM.  By default, each race that
C     was scheduled for this time period is rescheduled for the next
C     time period (whether it was active this time or not, unless the
C     user specifies otherwise).           
C
C     Keyword:  BBTYPE3
C     Inputs:  1 = first outbreak year    2 = host tree species
C              3 = DBH limit              4 = threshold density of lg.
C                                             enough sick-enough host 
C              5 = mortality rate         6 = proportion of roots
C                                             infected limit
C              7 = 0 for reschedule, 1 for don't
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Calls :
C     OPFIND  (SUBROUTINE)   [FVS]
C     OPGET   (SUBROUTINE)   [FVS]
C     OPDONE  (SUBROUTINE)   [FVS]
C     OPCOPY  (SUBROUTINE)   [FVS]
C     OPINCR  (SUBROUTINE)   [FVS]
C     OPREDT  (SUBROUTINE)   [FVS]
C
C REVISION HISTORY:
C    06-AUG-01 Lance R. David (FHTET)
C       Change MYACT to IACTK in call to OPCOPY because of array to scalar
C       mismatch reported by LF95.
C       (previous date of last revision was March 1, 1995)
C    10-DEC-07 Lance R. David (FHTET)
C       Update to argument list in call to OPCOPY, added variable
C       DONE for debug/tracking (from Don Robinson).
C   08/26/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
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

      INTEGER  I, J, IK , I1, I2, ISPI, ONLY1X, NOCOPY, RRTYPE, RACE,
     &         DONE, IACTK
      INTEGER  IP, KDT, KODE, MYACT(1), NCOPYS, NPS, NTODO

      REAL     DBHLIM, THRESH, MORT, RROTEX, STEMS

      REAL     PRMS(6)
      
      IF (ITRN .LE. 0) RETURN
      NOCOPY = 0
      DONE = 0

C.... Ask OPFIND how many races of this beetle type are scheduled for
C.... this time period.

      DATA MYACT /2417/
      CALL OPFIND (1,MYACT,NTODO)
      IF (NTODO .LE. 0) RETURN
      
C.... For each race, first get the parameter values, determine which
C.... root rot type affects the beetle's host species, and determine
C.... the first (I1) and last (I2) records of the host species.  Before
C.... proceeding, check that vulnerable host exists.

      DO 999 RACE = 1, NTODO
         CALL OPGET (RACE,6,KDT,IACTK,NPS,PRMS)

         ISPI = INT(PRMS(1))
         DBHLIM = PRMS(2)
         THRESH = PRMS(3)
         MORT = PRMS(4)
         RROTEX = PRMS(5)
         ONLY1X = INT(PRMS(6))
        
         RRTYPE = MAXRR
         IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))
         I1 = ISCT(ISPI,1)
         I2 = ISCT(ISPI,2)
        
         IF (ISCT(ISPI,1) .EQ. 0) GOTO 888
         IF (PAREA(RRTYPE) .LE. 0.0) GOTO 888
      
C....    Second, go through the host tree records and count the number
C....    of live stems that meet the user's criteria for size and % of
C....    roots infected with annosus.  Determine whether the density of
C....    these stems exceeds the user's threshold for an active beetle
C....    outbreak.       

         STEMS = 0.0
         DO 6 J = I1,I2
            I = IND1(J)
            IF (DBH(I) .LT. DBHLIM) GOTO 6

            DO 5 IK = 1,ISTEP                       
               DO 4 IP = 1,2
                  IF (PROPI(I,IK,IP) .LT. RROTEX) GOTO 4
                  STEMS = STEMS + PROBI(I,IK,IP)
    4          CONTINUE   
    5       CONTINUE
    6    CONTINUE

         IF ((STEMS / PAREA(RRTYPE)) .LT. THRESH) GOTO 888

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
         MININF(NUMBB) = RROTEX
         ORATE(NUMBB) = 0.0
         OFRATE(NUMBB) = 0.0
         IURATE(NUMBB) = 0.0
         IIRATE(NUMBB) = MORT

C....    Check whether the Type 3 beetle races are to remain
C....    potentially active after the current growth cycle.

  888    CONTINUE

         IF (ONLY1X .EQ. 1) NOCOPY = 1

  999 CONTINUE
  
C.... If the Type 3 beetle races are to remain active, then copy them
C.... to the next growth cycle.

      IF (NOCOPY .EQ. 1) GOTO 1000
      CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)      
      CALL OPINCR (IY,ICYC,NCYC)
      
 1000 CONTINUE       

      RETURN
      END
