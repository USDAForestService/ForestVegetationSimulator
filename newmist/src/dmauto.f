      SUBROUTINE DMAUTO (TrgDMR, RQ, D, S)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMAUTO -- NISI  Date of last revision: April 7 1994 
C--------------------------------------------------------------------
C Purpose:
C   The model allows a positive spatial autocorrelation to exist 
C between trees of similar DMR classes along with a negative spatial
C autocorrelation for unlike classes. This means that the stand-
C average density of trees in each DMR class varies in the
C neighborhood of each different DMR target tree. This routine
C adjusts those densities using a reweighting scheme that preserves
C the overall density of each DMR class. In effect, it makes it less
C likely that certain DMR classes will be neighbors. The values that
C are used as weights are held in the matrix 'SF(DMR_s,DMR_t)', which
C contains values derived from a double exponential function computed
C in DMINIT (and possibly recalculated in DMOPTS). The function has
C the form:
C
C  f(A,B,D,DMR_t,DMR_s)= EXP(A*ABS(DMR_s-DMR_t)*EXP(B*D)))
C
C where A and B are user-defined coefficients (through the DMAUTO
C keyword), D is the sampling ring from which the target sample is to
C be drawn, and DMR_t and DMR_s are the DMR classes of the target and
C source trees respectively.
C
C Within the ring 'D' containing a target tree with a DMR of 'DMR_t',
C the following relationship exists, with 'x' unknown, looping over
C the 0-6 'i' DMR  categories:
C
C              i
C   D_total = SUM { x * SF(DMR_t,D) * D_s(i) }
C
C where:  SF(T,S) is the scaling function value based on the
C                 DMR-difference of Target 'T' compared to a Source
C                 'S'
C         D_s(i)  is the trees/acre for the DMR_s in category 'i'
C         D_total is the total stems/acre for the stand
C         D       is the index to the sample ring
C
C 'x' can be solved directly:
C
C                  i
C   x = D_total / SUM { SF(DMR_t,D) * D_s(i) }
C
C Then the adjusted valued of D_s can be computed directly, giving
C back the adjusted trees/acre for each ring.
C
C   D_s'(i)= SF(DMR_t,D) * x * D_s(i)
C
C--------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     [none]
C
C Argument list definitions:                        
C
C     INTEGER TrgDMR  (I) The DMR of the target tree that lies at the
C                          centre of the local neighborhood
C     INTEGER RQ      (I) The sampling ring number for which the
C                          neighborhood density of each DMR class is
C                          computed. A value of 1 is the innermost
C                          ring (actually a circular disk of MESH
C                          radius)
C     REAL    D       (I) Array containing the unadjusted density
C                          (trees/ac) for each DMR category within a
C                          species.
C     REAL    S       (O) Array containing the adjusted density 
C                          (trees/ac) for each DMR category within a
C                          species.
C
C Local variable definitions:
C
C     INTEGER i           Loop counter.
C     REAL    ds          Sum of weighted densities across all DMRs.
C     REAL    DTot        Trees/acre for all members of the species.
C
C Common block variables and parameters:
C
C     SF      DMCOM
C     DMRDFF  DMCOM
C
C********************************************************************
 
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER   TrgDMR
      INTEGER   RQ
      REAL      D
      REAL      S

      DIMENSION D(0:6)
      DIMENSION S(0:6)

C Local variables.

      INTEGER   i
      REAL      ds, DTot

C Find the total trees/acre of all DMR classes. If there are no trees
C of this species, then blow this popstand. This is slightly inefficient,
C since it will be calculated repeatedly when the call is made within a 
C time step. Perhaps there is already a calculation of each DMR density
C done somewhere else?          

      DTot = 0.
      DO 200 i = 0, 6
	  S(i) = 0.0
        DTot = DTot + D(i)
  200 CONTINUE

      IF (DTot .LE. 0.0) GOTO 500

C Loop over the delta-DMR categories. For DMR=0 and 6, there will be
C 6 of these. For DMR=3 only 3 delta-DMR values will be used (obvious
C once you think about it). Then compute the appropriate sum of
C weighted densities for each DMR class. 'ds' is a weighted sum for
C all classes in the quadrat ring: each 'D()' Source DMR-class
C density is weighted by the value of 'SF()' appropriate to the DMR
C difference between Target 'j' and Source 'k' implied by the 
C 'DMRDFF' matrix. 

          ds = 0.
          DO 300 i = 0, 6
            ds =  ds + D(i) * SF(DMRDFF(i, TrgDMR), RQ)
  300     CONTINUE

C 'DTot' changes meaning slightly here, and is scaled by 'ds' to give
C a multiplier for the adjusted density of each DMR class.

          DTot = DTot / ds
          DO 400 i = 0, 6
            S(i) = DTot * D(i) * SF(DMRDFF(i, TrgDMR), RQ)
  400     CONTINUE

  500 CONTINUE

      RETURN
      END
