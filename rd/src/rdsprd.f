      SUBROUTINE RDSPRD
      IMPLICIT NONE
C----------
C RD $Id: rdsprd.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  THIS SUBROUTINE CALCULATES THE RATE OF SPREAD OF DISEASE CENTER
C  BY SIMULATING THE SPREAD THROUGH A SMALL STAND IN WHICH THE
C  TREES ARE REPRESENTED EXPLICITLY.
C
C  CALLED BY :
C     RDCNTL  [ROOT DISEASE]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
C     RDSLP   (FUNCTION)     [ROOT DISEASE]
C     RDRANN  (FUNCTION)     [ROOT DISEASE]
C
C  NOTES:
C    PNINF is here taken to be the probability of infection PER infected
C       tree contacted FOR PINT years. (where PINT is currently set to
C       10 years)
C    While the infection is growing into the tree center, the radius of
C       infection - RADINF - is negative and indicates how far the
C       infection is from the center.  RADINF is positive once the
C       infections starts spreading outwards.  RADINF will be positive
C       at the time of infection. Infection is from a neighbour whose
C       radius of infection overlaps the stump of the focal tree.
C
C SOME VARIABLES:
C   DISTNC(i,j)  the distance between the centers of each pair of trees
C   EFFSDI(1)    the effect of Stand Density Index for tree species 1,
C                equal to the proportion of the species-typical root
C                radius actually realized, given the total stand density
C   FINT         growth cycle used in the Annosus model
C   IRRSP        RR species to be modelled
C   IRSNYR       number of time periods for which to model RR spread
C   IRSTYP       1 = place trees randomly on sample plot
C                0 = regularly space trees on sample plot
C   IRTSPC(1)    maps Annosus tree species codes to FVS species codes
C   MCRATE(NMONT)  the spread rates determined by each Monte Carlo
C                simulation
C   NMONT        number of times to repeat MonteCarlo simulation of
C   NOW, NEW     pointers that indicate the present and future RADINF
C   NRSTEP       number of years in each time period
C   NTREES       actual number of trees used in the simulation
C                spread rate
C   NUMPAL(I), IDPAL(I, J)  Respectively, the number of trees with
C                which each focal tree 'i' has root contact ("pals"),
C                and the identity of those contact trees, for each
C                focal tree
C   PAREA(1)     area of the stand infected with RR species 1, in acres
C   PCOLO(1)     proportion of roots colonized after death of tree of
C                species1
C   PKILLS(1)    proportion of roots infected at death of tree of
C                species 1
C   PNINF(1)     probability of infection of tree species 1
C   RKILLS(i)    the radius of infection at which tree 'i' will die
C   ROOM         0/1 to indicate whether infection has crossed the
C                sample plot.  If it has, the rate of spread is
C                determined from the extent of infection at the end of
C                the previous period
C   RRGEN(IRRSP,2) target number of trees to put on sample plot
C   RRRATE(IRRSP), SDRATE(IRRSP)  the average and the s.d. of the
C   RRSARE       area of the sample plot used in the simulation
C   RRSFRN       the amount of random variation when trees are evenly
C                spaced
C   RRSMEN -
C   RRSRAD(i)    the species-typical root radius for a tree with the
C                DBH of tree 'i'
C   SAREA        stand area in acres
C   SFPROB       the density of uninfected trees outside desease
C                centers for the stand as a whole
C   SICK(i)      0/1 to indicate whether tree 'i' is infected
C   SINE(i)      the SIN of the angle of contact with infection for
C                each tree
C   TRURAD(i)    the realized root radius of each tree 'i', given EFFSDI
C   UPDATE       1 = update RADINF when infection from a neighbour has
C                    spread beyond the infection of the focal tree
C                0 = don't update
C   UPLTD        (UPDATE LIMITED) 1 = RADINF cannot be increased past
C                a tree's center by later contact with an infected
C                neighbour, 0 = it can be.  Where the increase in
C                RADINF is limited to the center, an increment of
C                between 0 and NRSTEP-1 years internal spread is added
C                (based on a random number).  This is because infection
C                will have been present for this long, on average,
C                before it is noted by the program
C                spread rates
C   YTK(i)       the years required for rr to kill a tree with the DBH
C                of tree 'i'.  We don't know whether this is time from
C                first infection, of time from infection of the center
C   YTKX         a multiplier of the YTK values calculated from a
C                linear equation
C
C  Revision History:
C    21-MAR-00 Lance R. David (FHTET)
C       Reduced RETURN statements to 1 at the end of routine.
C       Added Debug code.
C    07-AUG-01 Lance R. David (FHTET)
C       Initialization of variable YFORWD near statement lable 300.
C    19-AUG-14 Lance R. David (FMSC)
C       Variable NTREES declared locally is in RDCOM.F77. Local
C       declaration removed.
C   09/12/14 Lance R. David (FMSC)
C     Added implicit none and declared variables. The implicit type for
C     variable NEWDEN was not consistent with its use, declared REAL.
C
C----------------------------------------------------------------------

C.... PARAMETER INCLUDE FILES

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... COMMON INCLUDE FILES

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

      LOGICAL DEBUG
      INTEGER GCENTS, I, I1, I2, ICEN, IDI, IDPAL(50,50), II, IN,
     &        IRSSP(50), IRSTEP, IRSTP, IT, ITM, ITREES, J, JT,
     &        JTIME, JYEARS, K, KSP, KT, NEW, NOW, NUMPAL(50),
     &        ROOM, SICK(50), UPDATE, UPLTD
      REAL    DIFF, DISTNC(50,50), EFFSDI(ITOTSP), HABSP, NEWDEN,
     &        PNIN, PNSP, R, RDRANN, RDSLP, RKILLS(50), RPINT, RRSDIM,
     &        RRSMEN, SFPROB, SINE(50), TRURAD(50), YDMAX, YFORWD,
     &        YTK(50), YTKX

C.... SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'RDSPRD',6,ICYC)

      IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDSPRD'


C.... INITIALIZE CONSTANTS THAT MAY ONE DAY DEPEND ON USER INPUTS
C....
C.... IF YTKX = .5 THEN YEARS TO KILL IS FROM INFECTION TO DEATH
C.... IF YTKX = 1 THEN IT IS TIME FROM CENTER TO DEATH

      YTKX = 1

      DO 12 I = 1,ITOTSP
         EFFSDI(I) = 1.0
   12 CONTINUE

      UPDATE = 1
      UPLTD = 1
      NOW = 1
      NEW = 2

C.... ZERO INFECTION ARRAYS

      RRRATE(IRRSP) = 0.0
      SDRATE(IRRSP) = 0.0

      DO 5 JTIME = 1,NMONT
         MCRATE(IRRSP,JTIME) = 0.0
         MCTREE(IRRSP,JTIME) = 0
         SPRQMD(IRRSP,JTIME) = 0.0
    5 CONTINUE

      GCENTS = NCENTS(IRRSP) - NSCEN(IRRSP)

      IF (GCENTS .LE. 0 .OR. PAREA(IRRSP) .EQ. 0.0) THEN

C....    don't need to zero out rrates any more since if GCENTS=0
C....    rrates should be filled with shrinking values.

         IF (NCENTS(IRRSP) .GT. 0.0) THEN
            DO 10 ICEN= 1,NCENTS(IRRSP)
               RRRATE(IRRSP) = RRRATE(IRRSP) + RRATES(IRRSP,ICEN)
   10       CONTINUE

            RRRATE(IRRSP) = RRRATE(IRRSP) / NCENTS(IRRSP)
         ENDIF

         GOTO 2000   !RETURN
      ENDIF

      DIFF = SAREA - PAREA(IRRSP)
      IF (DIFF .LE. 1E-3) GOTO 2000    !RETURN

C.... IF STAND IS RUN AS ONE CENTER THEN THERE IS NO SPREAD TO
C.... OUTSIDE AREAS. IF MANUAL INITIALIZATION CALCULATE SPREAD.

      IF (LONECT(IRRSP) .EQ. 1) GOTO 2000     !RETURN


      IF (IY(ICYC) .GE. IRGEN(4)) THEN

C....    CHANGE THE TREE SPATIAL DISTRIBUTION TO THE OTHER TYPE
C....    (RANDOM <-> REGULAR)

         ITM = IRSTYP
         IF (ITM .EQ. 0) IRSTYP = 1
         IF (ITM .EQ. 1) IRSTYP = 0
      ENDIF

C.... THIS CALCULATION IS BASED ON DENSITY OUTSIDE OF DISEASED AREAS
C.... ONLY.  NOTE THAT ALL CACULATIONS ARE BASED ON THE LESSER OF LAST
C.... YEAR'S FRINGE DENSITY AFTER BARK BEETLES AND WINDTHROW HAVE BEEN
C.... APPLIED AND THIS YEAR'S OUTSIDE DENSITY. THE COMPARISON IS DONE
C.... IN RDOAGM EVERY YEAR (EVEN IF THERE ARE NO BARK BEETLES ACTIVE)

      SFPROB = 0.0
      IDI = IRRSP

      DO 30 I = 1,ITRN
         IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
         IF (IDI .EQ. IRRSP) SFPROB = SFPROB + FFPROB(I,1)
   30 CONTINUE

      NEWDEN = SFPROB

C.... GET LINEAR DIMENSIONS IN FEET

      IF (IRSTYP .EQ. 0) THEN
         RRSARE = RRGEN(IRRSP,2) / (NEWDEN + 1E-9)
      ELSE
         ITREES = IFIX(SQRT(RRGEN(IRRSP,2)))
         RRSARE = (ITREES ** 2) / (NEWDEN + 1E-9)
      ENDIF

      IF (NEWDEN .LE. 0) GOTO 1020

      RRSDIM = SQRT(RRSARE) * 208.7

      IF (DEBUG)
     &  WRITE (JOSTND,884) RRSARE,RRGEN(IRRSP,2),RRGEN(IRRSP,1)
  884 FORMAT (' RRSARE RRGEN(IRRSP,2),RRGEN(IRRSP,1)',4F10.2)

      DO 1000 JTIME = 1,NMONT

C....    SELECT TREES FOR THE AREA TO BE SIMULATED

         NTREES = 0
         IDI = IRRSP

         DO 100 KSP = 1,MAXSP
            IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 100
C
            HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)

C....       MODIFY THE TIME TO DEATH MULTIPLIER (HABSP) BASED ON THE
C....       PROPORTION OF CENTERS THAT ARE SPORE INITIATED (SPPROP).

            HABSP = HABSP * ((SPPROP(IDI) * SPYTK(IDI)) +
     &              (1 - SPPROP(IDI)))

            IF ((ISCT(KSP,1) .EQ. 0) .OR. (IDI .NE. IRRSP)) GOTO 100

            I1 = ISCT(KSP,1)
            I2 = ISCT(KSP,2)

            DO 90 J = I1,I2
               I = IND1(J)

C....          CALCULATE NUMBER OF TREES OF CLASS IN A
C....          IF TREE IS NOT OUTSIDE INFECTED AREA THEN DELETE

               IF (DEBUG) WRITE (JOSTND,778) PROBI(I,1,1), PROBIU(I),
     &                    PROBIT(I), FFPROB(I,1)
  778          FORMAT(' PROBI PROBIU PROBIT FFPROB',4F10.2)

               IF (FFPROB(I,1) .EQ. 0.0) GOTO 90

               RRSMEN = FFPROB(I,1) * RRSARE
               NUMTRE = IFIX(RRSMEN)
               PTRE = RRSMEN - NUMTRE
               R = RDRANN(0)
               IF (R .LE. PTRE) NUMTRE = NUMTRE + 1

               IF (DEBUG) WRITE (JOSTND,885) FFPROB(I,1), NUMTRE
  885          FORMAT (' FFPROB=',F10.2,'NUMTRE=',I4)

               IF (NUMTRE .EQ. 0) GOTO 90

C....          GET TREE INFORMATION FROM TREE LIST

               DO 80 K = 1,NUMTRE
                  NTREES = NTREES + 1

                  IF (NTREES .GT. 50) THEN
                     NTREES = 50
                     GOTO 90
                  ENDIF

                  RRSDBH(NTREES) = DBH(I)
                  RRSRAD(NTREES) = ROOTL(I)
                  IRSSP(NTREES) = KSP
                  YTK(NTREES) = RDSLP(RRSDBH(NTREES),XXINF,YYINF,NNINF)

                  IF (DEBUG) WRITE (JOSTND,328) RRSDBH(NTREES),
     &                       YTK(NTREES)
  328             FORMAT(' RRSDBH,YTK=',2F10.2)

                  YTK(NTREES) = (YTK(NTREES) - XMINKL(IRRSP)) * HABSP *
     &                          RRPSWT(IRTSPC(KSP)) + XMINKL(IRRSP)

   80          CONTINUE
   90       CONTINUE
  100    CONTINUE

C....    MODIFY THE ROOT RADIUS TO ACCOUNT FOR CROWDING (SDI)
C....    AND (POSSIBLY) CHANGE THE YTK TO REFLECT # YEARS FROM
C....    INFECTION TO DEATH ALSO CALCULATE THE QUADRATIC MEAN DIAMETER
C....    OF THE TREES INVOLVED IN THE SIMULATION

         DO 105 I = 1,NTREES
            TRURAD(I) = RRSRAD(I) * EFFSDI(IRTSPC(IRSSP(I)))
            RKILLS(I) = TRURAD(I) * PKILLS(IRTSPC(IRSSP(I)),IRRSP)
            YTK(I) = YTKX * YTK(I)

            SPRQMD(IRRSP,JTIME) = SPRQMD(IRRSP,JTIME) + RRSDBH(I) ** 2.0
  105    CONTINUE

         SPRQMD(IRRSP,JTIME) = SQRT(SPRQMD(IRRSP,JTIME) / FLOAT(NTREES))
         MCTREE(IRRSP,JTIME) = NTREES

C....    SET UP TREE POSITIONS


         IF (IRSTYP .NE. 1) THEN

C....       RANDOM

            DO 150 IT = 1,NTREES
               XRRS(IT) = RDRANN(0) * RRSDIM
               YRRS(IT) = RDRANN(0) * RRSDIM
  150       CONTINUE

         ELSE

C....       REGULAR

            ITREES = IFIX(SQRT(FLOAT(NTREES)))
            KT = 0

            DO 175 IT = 1,ITREES
               DO 170 JT = 1,ITREES
                  KT = KT + 1
                  XRRS(KT) = (IT - 0.5) * RRSDIM / FLOAT(ITREES) +
     &                       (RDRANN(0) * 2 - 1) * RRSFRN
                  YRRS(KT) = (JT - 0.5) * RRSDIM / FLOAT(ITREES) +
     &                       (RDRANN(0) * 2 - 1) * RRSFRN
  170          CONTINUE
  175       CONTINUE

            NTREES = KT
         ENDIF

C....    DETERMINE DISTANCE BETWEEN EVERY PAIR OF TREES

         DO 205 I = 1, NTREES
            DO 200 J = (I + 1), NTREES
               DISTNC(I,J) = SQRT((XRRS(I) - XRRS(J)) ** 2 +
     &                       (YRRS(I) - YRRS(J)) ** 2)
               DISTNC(J,I) = DISTNC(I, J)
  200       CONTINUE
  205    CONTINUE

C....    (RE)INITIALIZE TREE VARIABLES

         DO 210 I = 1, NTREES
            SICK(I) = 0
            RADINF(I, 1) = -TRURAD(I)
            RADINF(I, 2) = -TRURAD(I)
            NUMPAL(I) = 0
  210    CONTINUE

C....    FIND ALL POSSIBLE CONTACT-TREES OF EACH TREE

         DO 225 I = 1, NTREES
            DO 220 J = (I + 1), NTREES
               IF (TRURAD(I) + TRURAD(J) .GE. DISTNC(I, J)) THEN
                  NUMPAL(I) = NUMPAL(I) + 1
                  NUMPAL(J) = NUMPAL(J) + 1
                  IDPAL(I, NUMPAL(I)) = J
                  IDPAL(J, NUMPAL(J)) = I
               ENDIF
  220       CONTINUE
  225    CONTINUE

C....    LOOP OVER TIME STEPS OF SIMULATION

         ROOM = 1
         JYEARS = 0

         RPINT = PINT

         DO 300 IRSTEP = 1, IRSNYR
            IF (ROOM .GT. 0) THEN
               JYEARS = JYEARS + NRSTEP

               DO 290 IN = 1, NTREES
                  KSP = IRSSP(IN)

                  IF (RADINF(IN, NOW) .LT. RKILLS(IN)) THEN
                     PNSP = PNINF(IRTSPC(KSP),IRRSP)

C....                DETERMINE WHETHER UNINFECTED TREES ON THE BASELINE
C....                HAVE BECOME INFECTED

                     IF ((SICK(IN) .EQ. 0) .AND.
     &                   (YRRS(IN) .LE. TRURAD(IN))) THEN
                        R = RDRANN(0)

                        IF (IRSTEP .EQ. 1) THEN
                           PNIN = PNSP
                        ELSE
                           PNIN = 1 - ((1 - PNSP) ** (NRSTEP / RPINT))
                        ENDIF

                        IF (R .LE. PNIN) THEN
                           SICK(IN) = 1
                           RADINF(IN, NEW) = -YRRS(IN)
                           SINE(IN) = 1
                        ENDIF
                     ENDIF

C....                DETERMINE WHETHER UNINFECTED TREES BECOME INFECTED,
C....                AND, IF UPDATE = 1, WHETHER INFECTED TREES BECOME
C....                MORE INFECTED, THROUGH CONTACT WITH NEIGHBOURS
C....                WHOSE INFECTION IS SPREADING.  IF UPTLD=1, THEN THE
C....                INCREASE IN INFECTION IS LIMITED TO THE CENTER
C....                OF THE TREE, WITH AN INCREMENT FOR SPREAD DURING
C....                THE 0 TO (NRSTEP-1) YEARS SINCE INFECTION AT THE
C....                CENTER WILL HAVE OCCURRED.

                     IF ((IRSTEP .GT. 1) .AND.
     &                   ((SICK(IN) .EQ. 0) .OR.
     &                   ((UPDATE .EQ. 1) .AND.
     &                   ((UPLTD .EQ. 0) .OR.
     &                   (RADINF(IN, NOW) .LT. 0))))) THEN

                        DO 320 II = 1, NUMPAL(IN)
                           IT = IDPAL(IN, II)

                           IF (RADINF(IT, NOW) .GT. 0) THEN
                              IF (RADINF(IN, NEW) .LT.
     &                            -(DISTNC(IT, IN) -
     &                           RADINF(IT, NOW))) THEN

                                 R = RDRANN(0)

                                 PNIN = (1 - (1 - PNSP) **
     &                                  (NRSTEP / FINT))

                                 IF (R .LE. PNIN) THEN
                                    SICK(IN) = 1
                                    RADINF(IN,NEW) = -(DISTNC(IT,IN) -
     &                                               RADINF(IT,NOW))

                                    IF (DISTNC(IN, IT) .GT. 0) THEN
                                       SINE(IN) = (YRRS(IN) -
     &                                            YRRS(IT)) /
     &                                            DISTNC(IT, IN)
                                    ELSE
                                       SINE(IN) = 1
                                    ENDIF
                                 ENDIF

                              ENDIF
                           ENDIF
  320                   CONTINUE

                        IF ((UPLTD .EQ. 1) .AND.
     &                      (RADINF(IN,NEW) .GT.0)) THEN

                           IRSTP = IFIX(RDRANN(0) * FLOAT(NRSTEP))

                           IF (IRSTP .EQ. NRSTEP) IRSTP = NRSTEP - 1
                           RADINF(IN, NEW) = FLOAT(IRSTP) * RRSRAD(IN) *
     &                                       PKILLS(IRTSPC(KSP),IRRSP) /
     &                                       YTK(IN)
                        ENDIF

                     ENDIF

C....                SPREAD ROT THROUGH INFECTED ROOTS

                     IF (SICK(IN) .EQ. 1) THEN
                        RADINF(IN,NEW) = RADINF(IN, NEW) + NRSTEP *
     &                                   RRSRAD(IN) *
     &                                   PKILLS(IRTSPC(KSP),IRRSP) /
     &                                   YTK(IN)

                        IF (RADINF(IN, NEW) .GE. RKILLS(IN)) THEN
                           RADINF(IN,NEW) = PCOLO(IRTSPC(KSP),IRRSP) *
     &                                      TRURAD(IN)
                        ENDIF
                     ENDIF

                     IF ((YRRS(IN) + RADINF(IN,NEW)) .GT. RRSDIM)
     &                   ROOM = 0
                  ENDIF

  290          CONTINUE
            ENDIF

C....       UPDATE THE PRESENT RADII OF INFECTION, AND BEGIN NEXT
C....       TIME STEP

            IF (ROOM .GT. 0) THEN
               DO 295 I = 1, NTREES
                  RADINF(I, NOW) = RADINF(I, NEW)
  295          CONTINUE
            ENDIF

  300    CONTINUE

C....    DETERMINE MAXIMUM SPREAD OF INFECTION.  IF INFECTION HAS
C....    CROSSED THE WHOLE SAMPLE PLOT, CALCULATE THE RATE FROM ITS
C....    EXTENT IN THE PREVIOUS TIME PERIOD (SUBSTITUTING THE PLOT
C....    HEIGHT IF INFECTION CROSSED THE SAMPLE PLOT IN THE FIRST
C....    TIME PERIOD).

         YDMAX = 0.0
         YFORWD = 0.0
         
         DO 330 IN = 1, NTREES
            IF (SICK(IN) .GT. 0) THEN

               IF (RADINF(IN, NOW) .GT. 0) THEN
                  YFORWD = YRRS(IN) + RADINF(IN, NOW)
               ELSE
                  IF (SINE(IN) .GT. 0) THEN
                     YFORWD = YRRS(IN) - (SINE(IN)*(-RADINF(IN,NOW)))
                  ENDIF
               ENDIF

               IF (YFORWD .GT. YDMAX) YDMAX = YFORWD

            ENDIF
  330    CONTINUE

         IF (ROOM .GT. 0) THEN
            MCRATE(IRRSP,JTIME) = YDMAX / JYEARS
         ELSE
            IF (JYEARS .GT. NRSTEP) THEN
               MCRATE(IRRSP,JTIME) = YDMAX / (JYEARS - NRSTEP)
            ELSE
               MCRATE(IRRSP,JTIME) = RRSDIM / JYEARS
            ENDIF
         ENDIF

         RRRATE(IRRSP) = RRRATE(IRRSP) + MCRATE(IRRSP,JTIME)

 1000 CONTINUE

      RRRATE(IRRSP) = RRRATE(IRRSP) / (FLOAT(NMONT))

      DO 1010 I = 1, NMONT
         SDRATE(IRRSP) = SDRATE(IRRSP) + (MCRATE(IRRSP,I) -
     &                   RRRATE(IRRSP)) ** 2
 1010 CONTINUE

      SDRATE(IRRSP) = SQRT(SDRATE(IRRSP) / FLOAT(NMONT - 1))
      
 1020 CONTINUE
                                                         
      IF (IRSPTY .EQ. 1) THEN

C....    DETERMINE WHAT SPREAD RATE TO ASSIGN TO EACH CENTER.
C....    AVERAGE (rrrate) IS RECALCULATED FROM APPLIED RATES.

         CALL RDRATE (NMONT)
      ENDIF

 2000 CONTINUE
      IF (DEBUG) WRITE (JOSTND,2) RRRATE(IRRSP)
    2 FORMAT (' EXIT RDSPRD:  RRRATE=',F9.2)

      RETURN
      END

