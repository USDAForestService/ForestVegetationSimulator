      SUBROUTINE RDINSD
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     This subroutine models the infection of new trees on the
C     inside of infection centers by simulating all the inoculum
C     in a small area explicitly and obtaining the probability of
C     contact by simulation. Trees are infected at some point along
C     their root systems (before the center - hence they will have
C     a negative radius of infection)
C
C  Called By :
C     RDCNTL  [ROOT DISEASE]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDRANN  (FUNCTION)     [ROOT DISEASE] 
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C
C  Local Variables :           
C     RRNINF - REAL
C              New infections, by tree record, totalled over the number
C              of simulations and averaged at the end.
C     POLP   - REAL
C              Proportion of root system infected, totalled over the
C              simulations and averaged at the end. Positive POLP means
C              negative root infection.
C     NINSIM - REAL
C              Number of infection simulations.
C
C  Revision History :
C     06/12/96 - Matthew K. Thompson
C                Moved the declaration of DSO, DSII, and DSIU to the
C                parameter include file RDPARM.
C
C     25/MAR/98  RNH (FHTET)
C                Modified loop 1000 to skip operations on non-host
C                annosus species
C    21-MAR-00 Lance David (FHTET)
C                Reduced RETURN statements to 1 at the end of routine.
C                Added Debug code.
C    03-AUG-2012 Lance David (FMSC)
C                Array dimension of RRNINF and POLP changed from 500 to
C                parameter IRRTRE.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations.

      LOGICAL  DEBUG
      INTEGER  I, I1, I2, IDI, IN, IP, IRINCS, IT, ITROLP,
     &         J, K, KSP, NSIMS, NUMOLP

      REAL     AVGINF, DENNEW, DISOLP, DIST, OAMOVE(3), OVERLP,
     &         PNIOLP, PNSP,POLP(IRRTRE), PREVKL, PROPN, R, 
     &         RDRANN, RRNINF(IRRTRE), SMBI, SMIU, TEMP, XTRY,
     &         YTRY

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDINSD',6,ICYC)

      IF (DEBUG) WRITE(JOSTND,*)' ENTER RDINSD: ICYC=',ICYC

C.... Set up inoculum test plot.

      IF (ITRN .EQ. 0 .OR. PAREA(IRRSP) .EQ. 0.0) GOTO 2000     !RETURN

      SMBI = 0.0
      SMIU = 0.0

      IDI = IRRSP
      DO 443 I=1,ITRN
         IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
         IF (IDI .NE. IRRSP) GOTO 443
         
         POLP(I) = 0.0
         RRNINF(I) = 0.0

         SMBI = SMBI + PROBIT(I)

         SMIU = SMIU + PROBIU(I)
  443 CONTINUE

      IF (DEBUG) WRITE(JOSTND,888) SMIU, SMBI, PAREA
  888 FORMAT (' IN RDINSD:  SMIU SMBI PAREA=',3E15.7)

      DENNEW = (SMIU + SMBI) / PAREA(IRRSP)

C.... Get linear dimensions in feet.

      DO 50 I=1, 2
         DO 45 J=1, 5
            DENNEW = DENNEW+(PROBD(IRRSP,I,J)/(PAREA(IRRSP)+1E-9))
   45    CONTINUE
   50 CONTINUE

      IF (DENNEW .LE. 0.0) GOTO 2000          !RETURN
      
      RRIARE = RRGEN(IRRSP,9) / (DENNEW + 1E-9)
      RRIDIM = SQRT(RRIARE) * 208.7
      
      IF (DEBUG) WRITE(JOSTND,51) RRIARE,RRIDIM,RRGEN(IRRSP,9),DENNEW
   51 FORMAT(' RDINSD: RRIARE RRIDIM RRGEN(9) DENNEW=',4F12.3)
      
C.... Begin the simulation loop for the calculation of inside center
C.... infection.
      
      DO 1010 NSIMS = 1,NINSIM            
        
         ANUINF(IRRSP,NSIMS) = 0.0

C....    Select live infected trees for the area to be simulated.

         IRINCS = 0    
         IDI = MAXRR

         DO 100 KSP=1, MAXSP
            IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
            IF ((ISCT(KSP,1) .EQ. 0) .OR. (IDI .NE. IRRSP)) GOTO 100

            I1 = ISCT(KSP,1)
            I2 = ISCT(KSP,2)

            DO 95 J=I1, I2
               I = IND1(J)

               DO 90 IT=1, ISTEP
                  DO 85 IP=1,2
             
C....                Calculate number of trees of class in simulated
C....                area. Ignore trees which are infected but are not
C....                yet inoculum.

                     IF (PROPI(I,IT,IP) .LE. 0.0) GOTO 85
               
                     RRIMEN = PROBI(I,IT,IP) * RRIARE / (PAREA(IRRSP) +
     &                        1E-6)
                     IF (RRIMEN .EQ. 0.0) GOTO 85

                     NUMTRE = INT(RRIMEN)
                     PTRE = RRIMEN - NUMTRE
                     R = RDRANN(0)

                     IF (R .LT. PTRE) NUMTRE = NUMTRE + 1

                     IF (DEBUG) WRITE(JOSTND,86) NUMTRE
   86                FORMAT(' RDINSD:  NUMTRE=',I6)

                     IF (NUMTRE .LE. 0) GOTO 85

C....                Get tree information from the tree list.

                     DO 80 K=1, NUMTRE
                        IRINCS = IRINCS + 1

                        IF (DEBUG) WRITE(JOSTND,87) IRINCS
   87                   FORMAT (' RDINSD:  IRINCS=',I6)

                        IF (IRINCS .LE. IRRTRE) GOTO 75

                        WRITE(6,70)
   70                   FORMAT(' TOO MANY TREES IN INSIDE SIMULATION')
                        IRINCS = IRRTRE
                        GOTO  110

   75                   CONTINUE
                        RRIRAD(IRINCS) = ROOTL(I) * PROPI(I,IT,IP)
   80                CONTINUE
   85             CONTINUE
   90          CONTINUE
   95       CONTINUE
  100    CONTINUE

  110    CONTINUE

C....    Select infected dead trees and stumps for simulation.

         DO 605 I=1, 2
            DO 600 J=1,5

C....          Estimate number of stumps in simulated area.

               RRIMEN = PROBD(IRRSP,I,J) * RRIARE /
     &                  (PAREA(IRRSP) + 1E-6)
               IF (RRIMEN .EQ. 0.0) GOTO 600
               NUMTRE = INT(RRIMEN)
               PTRE = RRIMEN - NUMTRE
               R = RDRANN(0)

               IF (R .LT. PTRE) NUMTRE = NUMTRE + 1
               IF (NUMTRE .LE. 0) GOTO 600

C....          Get radii of inoculum.

               DO 590  K=1, NUMTRE
                  IRINCS = IRINCS + 1
                  IF (IRINCS .LE. IRRTRE) GOTO 580

                  WRITE(JOSTND,570)
  570             FORMAT(' TOO MANY TREES AND STUMPS IN INSIDE
     &                   SIMULATION')
                  IRINCS = IRRTRE
                  GOTO 610

  580             CONTINUE
                  RRIRAD(IRINCS) = ROOTD(IRRSP,I,J)
  590          CONTINUE
  600       CONTINUE
  605    CONTINUE

  610    CONTINUE

         INFISD(IRRSP,NSIMS) = IRINCS

C....    If IRINCS = 0 then we have no sources of inoculum, so there is
C....    no point in doing the rest of this simulation.

         IF (IRINCS .EQ. 0) GOTO 1010

C....    Set up inoculum positions randomly.

         DO 650 IT=1, IRINCS
            XRRI(IT) = RDRANN(0) * RRIDIM
            YRRI(IT) = RDRANN(0) * RRIDIM
  650    CONTINUE

C....    Process tree list and infect trees inside infection zones.
C....    ------------------------------------------------------------
C....
C....    Loop over tree classes in tree list.

         RRGEN(IRRSP,4) = 0  
         IDI = MAXRR

         DO 1000 KSP=1, MAXSP
            IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 1000            
C
C            
            PNSP = PNINF(IRTSPC(KSP),IDI)  

C....       First, modify probability of infection to account for the
C....       number of years in a cycle.         

            PNSP = 1 - ((1 - PNSP) ** (FINT/PINT))

C....       Modify propability of infection for root to root contact
C....       based on the proportion of centers that are spore initiated
C....       (SPPROP).

            PNSP = PNSP * ((SPPROP(IDI) * SPTRAN(IDI)) +
     &             (1 - SPPROP(IDI)))
            IF ((ISCT(KSP,1) .LE. 0) .OR. (IDI .NE. IRRSP)) GOTO 1000

            I1 = ISCT(KSP,1)
            I2 = ISCT(KSP,2)

            DO 900 J=I1, I2
               I = IND1(J)

C....          Loop over uninfected trees in simulation.
C....          IRINIT (=13500) seems to be used to set a possible 
C....          maximum on the number of trees used in the simulation.

               NUMTRE = INT(REAL(IRINIT) / (REAL(ITRN) + 1E-6))
               IF (NUMTRE .GT. PROBIU(I)) NUMTRE = INT(PROBIU(I))
               IF (NUMTRE .LE. 0) GOTO 900

               NUMOLP = 0 
               DISOLP = 0.0 
               PNIOLP = 0.0

               DO 800 IT=1, NUMTRE
                  XTRY = RDRANN(0) * RRIDIM
                  YTRY = RDRANN(0) * RRIDIM

C....             Loop over inoculum sources to see if any contact.
C....             ITROLP -  counts number of overlaps for a single tree.
C....             NUMOLP -  counts total number of places of overlap
C....                       (sum of ITROLP'S).
C....             DISOLP -  adds up the amount of overlap (used in an
C....                       average later).
C....             PNIOLP -  is total probability of no infection from
C....                       overlap (will be averaged).

                  ITROLP = 0
                  DO 750 IN=1, IRINCS
                     DIST = SQRT((XTRY - XRRI(IN))**2 +
     &                      (YTRY - YRRI(IN))**2)
                     IF (DIST .GT. (RRIRAD(IN) + ROOTL(I))) GOTO 750
 
                     ITROLP = ITROLP + 1
                     OVERLP = RRIRAD(IN) + ROOTL(I) - DIST
                     IF (OVERLP .GT. ROOTL(I)) OVERLP = ROOTL(I)
                     DISOLP = DISOLP + OVERLP
  750             CONTINUE

                  IF (ITROLP .GT. 0) THEN 
                     PNIOLP = PNIOLP + (1 - PNSP) ** ITROLP
                  ELSE
                     PNIOLP = PNIOLP + 1
                  ENDIF

                  NUMOLP = NUMOLP + ITROLP
  800          CONTINUE

C....          Old way of calculating infection.
C....
C....          RRICON = REAL(IRIHIT) / (REAL(NUMTRE) + 1E-6)
C....          RRICON = RRICON * PNSP
C....
C....          Calculate total number of new infections for tree class.
C....
C....          RRIMEN = RRICON * PROBIU(I)
C....
C....          End of old way.

C....          Calculate average probability of infection for tree
C....          class.
            
               AVGINF = 1 - (PNIOLP / (REAL(NUMTRE) + 1E-6))
            
               RRNINF(I) = RRNINF(I) + PROBIU(I) * AVGINF
               ANUINF(IRRSP,NSIMS) = ANUINF(IRRSP,NSIMS) + 
     &                               PROBIU(I) * AVGINF
            
               IF (NUMOLP .LE. 0) GOTO 900

C...           DISOLP / NUMOLP is the average distance of overlap
C....          so POLP is the average proportion of overlap
C....          then take 1-POLP so POLP is measured from the center.
            
               TEMP = 1 - (DISOLP / NUMOLP) / (ROOTL(I) + 1E-6)
               IF (TEMP .LE. 0.0) TEMP = -.001
               POLP(I) = POLP(I) + TEMP

  900       CONTINUE
 1000    CONTINUE

 1010 CONTINUE

C.... Now that simulation loop is over, apply the average results to
C.... each record.

      IDI = MAXRR
      DO 1050 KSP=1, MAXSP
         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
         IF ((ISCT(KSP,1) .LE. 0) .OR. (IDI .NE. IRRSP)) GOTO 1050

         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)

         DO 1030 J=I1, I2
            I = IND1(J)
      
C....       Note: When we use the integer variable NUMTRE, we could
C....             end up with more trees infected then there were
C....             uninfected. There is no reason why PROBI has to be
C....             given integer numbers. (sb: nov 30/93)

C....       NUMTRE = INT(RRIMEN)
C....       PTRE = RRIMEN - NUMTRE
C....       R = RDRANN(0)
C....       IF (R .LT. PTRE) NUMTRE = NUMTRE + 1
C....       IF (NUMTRE .LE. 0) GOTO 900
C....       PROBIU(I) = PROBIU(I) - NUMTRE
C....       IF (PROBIU(I) .LT. 0) PROBIU(I) = 0
C....       PROBI(I,ISTEP) = PROBI(I,ISTEP) + NUMTRE
            
C....       For printing, add # of uninfected trees (before infection
C....       occurs).

            CORINF(IDI,2) = CORINF(IDI,2) + PROBIU(I)

C....       Find average of RRNINF and POLP

            IF (RRNINF(I) .LE. 1E-4) GOTO 1030
            
            RRNINF(I) = RRNINF(I) / (REAL(NINSIM) + 1E-6)
            POLP(I) = POLP(I) / (REAL(NINSIM) + 1E-6)

            PROBIU(I) = PROBIU(I) - RRNINF(I)
            IF (PROBIU(I) .LT. 0) PROBIU(I) = 0

            PROBI(I,ISTEP,1) = PROBI(I,ISTEP,1) + RRNINF(I)

C....       Add the new infection resulting inside the core of the
C....       center.

            CORINF(IDI,1) = CORINF(IDI,1) + RRNINF(I)
            
C....       Determine the initial infection level, from a distribution
C....       about the mean infection of 0.001.

C....       IF (PROPI(I,ISTEP) .LE. 0.0) PROPI(I,ISTEP) = RDRANP(0.001)   

            PROPI(I,ISTEP,1) = -POLP(I)
             
            RRGEN(IRRSP,4) = RRGEN(IRRSP,4) + RRNINF(I)

            IF (OAKL(DSIU,I) .GT. 0.0) THEN
            
C....          OAKL must be re-arranged because some of the inside
C....          uninfected trees that have just been infected may have
C....          been previously killed by other agents. eg. If 10% of
C....          iu trees were killed previously then assume that 10% of
C....          the newly infected trees are some of those that were
C....          killed.

               PROPN = OAKL(DSIU,I) / (PROBIU(I) + RRNINF(I) + 1E-6)
               PREVKL = PROPN * RRNINF(I)

               OAMOVE(DSO) = 0.0
               OAMOVE(DSII) = PREVKL
               OAMOVE(DSIU) = -PREVKL

               CALL RDMREC (1,I,KSP,OAMOVE)
              
            ENDIF

 1030    CONTINUE
 1050 CONTINUE

C.... Call RDSUM to calculate new value of PROBIT.

      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

 2000 CONTINUE
      IF (DEBUG) WRITE(JOSTND,*) ' EXIT RDINSD'

      RETURN
      END
