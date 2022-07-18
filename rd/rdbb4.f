      SUBROUTINE RDBB4
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     Determines how many races of bark beetle type 4 are eligible to
C     act in this growth cycle under current stand conditions.  The
C     mortality rates etc. specified for each beetle race that is
C     active are reported back to RDOAGM.  By default, each race that
C     was scheduled for this time period is rescheduled for the next
C     time period (whether it was active this time or not, unless the
C     user specifies otherwise).
C
C     Keyword :  BBTYPE4
C     Inputs : 1 = first outbreak year          
C              2 = host tree species       
C              3 = DBH limit                          
C              4 = threshold density of lg.enough sick-enough host
C                  (live or newly dead) 
C              5 = infected mortality rate     
C              6 = proportion-of-roots-infected limit
C              7 = inside uninf. mort. rate  
C              8 = fringe mortality rate
C              9 = outside center+fringe mortality rate
C             10 = 0 for reschedule, 1 for don't
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Calls :
C     OPFIND  (SUBROUTINE)   [FVS]
C     OPGET   (SUBROUTINE)   [FVS]
C     OPDONE  (SUBROUTINE)   [FVS]
C     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
C     OPCOPY  (SUBROUTINE)   [FVS]
C     OPINCR  (SUBROUTINE)   [FVS]
C     OPREDT  (SUBROUTINE)   [FVS]
C
C  Revision History :
C     06/12/96 - Matthew K. Thompson
C                Moved the declaration of DSII to the parameter
C                include file RDPARM.
C
C     03/04/98 - Robert N. Havis
C                Increased size of arrays PREVA and PREVB from 2 to 4
C                to accomodate 4 types of root diseases. Increase 
C                corresponding number of loops in statement 500 and 501
C    06-AUG-01 Lance R. David (FHTET)
C       Change MYACT to IACTK in call to OPCOPY because of array to scalar
C       mismatch reported by LF95.
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
      INCLUDE 'PLOT.F77'

C.... Local variable declarations.
      
      INTEGER  I, J, IK, I1, I2, ISPI, ONLY1X, NOCOPY, DONE,
     &         RRTYPE, RACE, IACTK, MYACT(1)

      INTEGER  IP, KDT, KODE, NCOPYS, NPS, NTODO

      REAL     DBHLIM, THRESH, MORTII, MORTIU, MORTF, MORTO, STEMS

      REAL     PRMS(9), PREVA(4), PREVB(4), RROTEX, XXXX
      
      NOCOPY = 0
      DONE = 0
      IF (ITRN .LE. 0) RETURN

C.... Ask OPFIND how many races of this beetle type are scheduled for
C.... this time period.

      DATA MYACT /2432/
      CALL OPFIND (1,MYACT,NTODO)
      IF (NTODO .LE. 0) RETURN

C.... For each race, first get the parameter values, determine which
C.... root rot type affects the beetle host species, and determine the
C.... first (I1) and last (I2) tree records of the host species.  Check
C.... that vulnerable host exists before proceeding.

      DO 999 RACE = 1, NTODO
         CALL OPGET (RACE,9,KDT,IACTK,NPS,PRMS)

         ISPI = INT(PRMS(1))
         DBHLIM = PRMS(2)
         THRESH = PRMS(3)
         MORTII = PRMS(4)
         RROTEX = PRMS(5)
         MORTIU = PRMS(6)
         MORTF  = PRMS(7)
         MORTO  = PRMS(8)
         ONLY1X = INT(PRMS(9))
        
         RRTYPE = MAXRR
         IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))
         I1 = ISCT(ISPI,1)
         I2 = ISCT(ISPI,2)
        
         IF (ISCT(ISPI,1) .EQ. 0) GOTO 888
         IF (PAREA(RRTYPE) .LE. 0.0) GOTO 888
         IF (PCOLO(IRTSPC(ISPI),RRTYPE) .LT. RROTEX) GOTO 888

C....    Second, go through the host tree records and count the number
C....    of living stems PLUS stems that died in the last two years
C....    that meet the user's criteria for size and % of roots infected
C....    with annosus.  Determine whether the density of these stems 
C....    exceeds the user's threshold for an active beetle outbreak.

         STEMS = 0.0

         DO 6 J = I1,I2
            I = IND1(J)
            IF (DBH(I) .LT. DBHLIM) GOTO 6

            DO 5 IK = 1,ISTEP
               DO 4 IP = 1,2
                  IF (PROPI(I,IK,IP) .LT. RROTEX) GOTO 4
                  STEMS = STEMS + ((2/FINT) * PROBI(I,IK,IP))
    4          CONTINUE   
    5       CONTINUE

            STEMS = STEMS + ((2/FINT) * (PROAKL(DSII,I) + PRANKL(I)))
    6    CONTINUE

         IF ((STEMS / PAREA(RRTYPE)) .LT. THRESH) GOTO 888
      
C....    If the beetle is active, call OPDONE to signal the outbreak,
C....    increment NUMBB and store the specified mortality rates for
C....    later use in killing trees.  

         CALL OPDONE (RACE,IY(ICYC))
         DONE = 1
        
         NUMBB = NUMBB + 1
         IF (NUMBB .GT. 3*MAXSP) NUMBB = 3*MAXSP
         HOST(NUMBB) = ISPI
         MINDBH(NUMBB) = DBHLIM
         MININF(NUMBB) = RROTEX
         ORATE(NUMBB) = MORTO
         OFRATE(NUMBB) = MORTF 
         IURATE(NUMBB) = MORTIU
         IIRATE(NUMBB) = MORTII

C....    Now, if it's not already been done for a previous beetle race,
C....    calculate the "fringe" area that is within 1 root diameter of
C....    an infection center in the stand - RDBBDO will need this to
C....    know how many trees to kill at the higher rate that applies in
C....    this area.

         IF (FRINGE(RRTYPE) .GT. 0.0) GOTO 888
        
C....    This procedure is pasted from RRJUMP, to compute the areas in
C....    SAREA and PAREA() at the next timestep.  Then a bunch of
C....    values have to be set back, so that the "true" call to RRJUMP
C....    can do them. The future area of the uninfected stand is called
C....    NSAREA; the area of the new torus around existing patches is
C....    called FRINGE().

        IF (NCENTS(RRTYPE) .EQ. 0) GOTO 888

C....   Save PAREA() and OOAREA() so that they can be restored at the
C....   end.
C
C     Set number of loops to 4 rather than 2 to correspond to total
C     number of RR diseases available (4). RNH(FEB98)
C

        DO 500 I=1,4
           PREVA(I) = PAREA(I)
           PREVB(I) = OOAREA(I)
  500   CONTINUE

        XXXX = 2.0 * RRJINC(RRTYPE)

        DO 700 I=1,NCENTS(RRTYPE)
           IF (PCENTS(RRTYPE,I,3) .LE. 0) GOTO 700
           PCENTS(RRTYPE,I,3) = PCENTS(RRTYPE,I,3) + XXXX
  700   CONTINUE
        
        IRRSP = RRTYPE
        CALL RDAREA

        FRINGE(RRTYPE) = PAREA(RRTYPE) - PREVA(RRTYPE)
        FRINGE(RRTYPE) = AMAX1(0.0,FRINGE(RRTYPE))

C....   Put PAREA() and OOAREA() back to their earlier state.
C
C     Set number of loops to 4 rather than 2 to correspond to total
C     number of RR diseases available (4). RNH(FEB98)
C
        DO 501 I=1,4
           PAREA(I) = PREVA(I)
           OOAREA(I) = PREVB(I)
  501   CONTINUE

C....   Put PCENTS back to its earlier state.

        DO 701 I=1,NCENTS(RRTYPE)
           IF (PCENTS(RRTYPE,I,3) .LE. 0) GOTO 701
           PCENTS(RRTYPE,I,3) = PCENTS(RRTYPE,I,3) - XXXX
  701   CONTINUE

C....   Check whether the Type 4 beetle races are to remain potentially
C....   active after the current growth cycle.

  888   CONTINUE

        IF (ONLY1X .EQ. 1) NOCOPY = 1

  999 CONTINUE
  
C.... If the Type 4 beetle races are to remain active, then copy them
C.... to the next growth cycle.  

      IF (NOCOPY .EQ. 1) GOTO 1000
      CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)     
      CALL OPINCR (IY,ICYC,NCYC)
      
 1000 CONTINUE       

      RETURN
      END
