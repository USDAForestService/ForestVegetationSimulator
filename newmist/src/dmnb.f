      SUBROUTINE DMNB (RQ, D, CNB)

C********************************************************************
C **DMNB -- NISI  Date of last revision: 12/19/03
C----------------------------------------------------------------------
C Purpose:
C   Create a frequency distribution based upon the binomial family of
C distributions and the variance/mean ratio. This allows a wide range
C of dispersion, from binomial to poisson to negative binomial. The
C distributions are accurate until the mean and variance are both
C very small. This occurs in cases where the mean is less than about
C 0.1 trees/acre and the dispersion is less than 0.1 In these
C situations the distribution is biased so that both the variance and
C the mean are larger than requested. One solution to this would be
C to constrain the variance as the mean dropped, since it is arguably
C non-binomial when the mean is low.
C   The routine begins by creating cumulative distributions for
C sources around target in each quadrat ring. The answer is returned
C in an array containing the cumulative distribution for the number
C ofC trees in the ring. If densities higher than 10,000/acre occur,
C the length of the array will have to be enlarged or the method
C changed, because the range of (the upper tail of) the distribution
C will extend beyond the end of the array. If this occurs, those
C sample sizes will never be chosen.
C   The next step computes the distribution function for trees in an
C annulus formed by overlaying the inner circle over the outer
C circle. The annuli have certain properties: there is always at
C least one tree in the sample: the target tree itself, and; there
C are forbidden observations that result from prior observations in
C the inner circle. These two constraints require the distribution to
C be reweighted.The algorithm is a bit wasteful, since the
C distribution of the inner circle quadrat must always be
C recomputed on each pass. However, storage requirements get large
C any other way.
C
C PS: The name 'DMNB' is an artefact of the earlier method, in which
C only the Negative Binomial distribution was possible.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     BNDIST
C
C Argument list definitions:                        
C
C     INTEGER RQ      (I) The sampling ring number for which the
C                          cumulative distribution function is to be
C                          calculated.
C     REAL    D       (I) Total density of trees of all species
C                          (trees/ac).
C     REAL    CNB     (0) Array containing the *cumulative*
C                          distribution for trees in the sampling
C                          ring, regardless of species or DM status.
C
C Local variable definitions:
C
C     LOGICAL erflg       Unused, could set exit status for base FVS.
C     INTEGER i           General loop counter.
C     INTEGER j           Ring loop counter (inner, outer)
C     INTEGER k           General loop counter. 
C     INTEGER CUR         Flag for current ring being processed.
C     INTEGER PRV         Flag for previous ring just processed.
C     INTEGER EMark       Location of the last non-zero probability
C                          in the upper tail.
C     INTEGER TopEnd      The last non-zero probability for the
C                          current (outer) ring.
C     REAL    mu          The requested mean of the distribution.
C     REAL    var         The requested variance of the distribution.
C     REAL    NB          The array holding the distribution for 
C                          observations in the inner and the outer
C                          rings.
C
C Common block variables and parameters:
C
C     DSTLEN   DMCOM
C     CrArea   DMCOM
C     DMCLMP   DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine argument list.

      INTEGER  RQ
      REAL     D
      REAL     CNB

      DIMENSION CNB(0:DSTLEN)
      
C Local variables.
      
      LOGICAL  erflg
      INTEGER  i, j, k
      INTEGER  CUR, PRV
      INTEGER  EMark(2), TopEnd
      REAL     mu, var
      REAL     NB(0:DSTLEN, 2)
      DOUBLE PRECISION YD

C     ** A NOTE ON 'NB()' **
C
C 'NB()' holds the probability of observing 'k' individuals at
C density 'mu' with variance 'var' Two vectors are generated: PRV is
C the vector of probabilities for the circle which is interior to the
C INNER radius of the annulus; CUR is the probability vector for the
C circle which is interior to the OUTER radius of the annulus. The
C expected number of trees 'mu' in each of the circles is given by
C the area 'CrArea()' multiplied by the total trees/acre density.
C The number of trees in the annulus (the non-intersection of the two
C circles) is given by the sum of products. For example, the
C likelihood of observing exactly 2 Source trees in the annulus is:
C
C    {P(0,i) * P(2,o)} + { P(1,i) * P(3,o) } + {P(2,i) * P(4,o) } + ...
C
C where 'i' indexes the inner circle and 'o' indexes the outer circle. 
C

C Zero the distribution.

      DO 10 i = 0, DSTLEN
        CNB(i) = 0.
   10 CONTINUE

C Find the distribution for the inner and outer circles that define
C the ring quadrat. If RQ=1, then j=0 and the center quadrat is being
C done. In this case the inner ring is a point containing the target
C tree. The point has only 1 likelihood: P(1)= 1.0.

      CUR = 1
      DO 100 j = RQ - 1, RQ
        
        IF (j .EQ. 0) THEN
        
          NB(CUR,1) = 1.0
          EMark(CUR) = 1
          
        ELSE
        
          mu = CrArea(j) * D 
          var = DMCLMP * mu
          
          CALL BNDIST(mu, var, erflg, DSTLEN, NB(0,CUR),
     >                 EMark(CUR))
          
        END IF 
        
        CUR = 2
        PRV = 1

  100 CONTINUE  

C Reweight to account for absence of observations of the n=0 class.
C This is because there is always at least a sample of 1 (the target
C tree itself) for both the inner and outer disks.

      DO 1000 i = 1, 2
        x = 1.0 / (1.0 - NB(0, i)) 
        NB(0, i) = 0.
        DO 1001 j = 1, EMark(i)
          NB(j, i) = NB(j, i) * x
 1001   CONTINUE
 1000 CONTINUE
 
C Do the sum of products described above. 'PRV' indexes the inner
C disk, 'CUR' the outer. Recall that in the RQ=1 case, the 'PRV' disk
C is a point.

        TopEnd = EMark(CUR)
        
        DO 210 k = 0,TopEnd
          x = 0.             
          DO 220 j = k,MIN0(k+EMark(PRV),DSTLEN)
            YD = NB(j, CUR) * NB(j-k, PRV)
            IF (YD .GT. 1.0E-25) x = x + YD
  220     CONTINUE
          CNB(k) = x
  210   CONTINUE

C Create the cumulative probability by sums.

      DO 300 k = 1, TopEnd
        CNB(k) = CNB(k-1) + CNB(k) 
  300 CONTINUE
      
C Adjust distribution to account for forbidden values. ie: make it
C all sum to 1.
                           
      x = 1.0 / CNB(TopEnd)
      DO 400 k = 0, TopEnd
	  CNB(k) = CNB(k) * x 
  400 CONTINUE
      
      RETURN
      END
