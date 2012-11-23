      SUBROUTINE DMSAMP (TotD, D, CNB, Prop, S)

C********************************************************************
C **DMSAMP -- NISI  Date of last revision: April 12 1994 
C----------------------------------------------------------------------
C Purpose:
C   Determine the sample size 'S' to be used, based on a random selection 
C from the cumulative distribution function. 'CNB()' is the cumulative
C density function for trees of all species. 'TotD' is the total density
C of all species in the stand. 'D' is the (autocorrelated) density within
C the ring.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG
C     
C Other routines called:
C
C     DMRANN
C
C Argument list definitions:                        
C
C     REAL      TotD  (I) The total density (trees/acre) of all trees
C                          of all species.
C     REAL      D     (I) The adjusted density (trees/acre) for the 
C                          Species and DMR category from which a
C                          sample may be drawn.
C     REAL      CNB   (I) Array containing the cumulative
C                          distribution function describing the
C                          likelihood of drawing 'n' trees of any
C                          species or DMR category from within a
C                          sampling ring.
C     REAL      Prop  (I) Scalar the modifies the number drawn from
C                          the sample, depending on how much of the
C                          ring is in the stand.
C     INTEGER   S     (O) The sample size obtained from a random
C                          draw from within the appropriate species
C                          and DM class: the sample may be zero.
C
C Local variable definitions:
C
C     INTEGER   BigS      A random sample of the number of trees that
C                          are found in the sample ring. They may be
C                          of any species or DM class.
C     INTEGER   j         Loop counter for the cumulative 
C                          distribution.
C     SINGLE    RND       A unform random number.
C     SINGLE    x         The proportion of all trees that will be in
C                          the appropriate species and DM class for
C                          this sample.
C
C Common block variables and parameters:
C
C     DSTLEN   DMCOM
C
C**********************************************************************
 
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C Argument list variables.
                                      
      REAL      TotD                                     
      REAL      D
      REAL      CNB
      REAL      Prop
      INTEGER   S

      DIMENSION CNB(0:DSTLEN)

C Local variables.
      
      INTEGER   BigS, j
      REAL      RND, x

C Draw a sample of trees, based on the cumulative distribution
C function that describes tree clumping. This larger sample is
C 'BigS'.

      CALL DMRANN(RND)
      BigS = 0
      DO j = 0, DSTLEN
        IF (RND .LE. CNB(j)) THEN
          BigS = j
          GOTO 201
        END IF
      ENDDO
  
C From the larger sample representing all trees in the stand,
C determine how many will be of the required DMR, based on the ratio
C of the density of the appropriate tree class to the total tree
C density. This is stochastic: summed over all the DMR classes, the
C value of 'S' is not compelled to be 'BigS * D / TotD', but will
C tend to that value.
      
  201 S = 0
      IF (BigS .GT. 0) THEN      
        x = Prop * (D / TotD)
        DO j = 1, BigS
          CALL DMRANN(RND)
          IF (RND .LE. x) S = S + 1
        ENDDO
      END IF

      RETURN
      END
