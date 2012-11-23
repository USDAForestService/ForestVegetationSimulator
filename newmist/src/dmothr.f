      SUBROUTINE DMOTHR (Sp)

C********************************************************************
C **DMOTHR -- NISI  Date of last revision: April 12 1994 
C----------------------------------------------------------------------
C Purpose:
C   Spread and intensification are modelled in an abstract way. Real
C world S&I is based on seeds being thrown, sticking, germinating,
C etc. This routine uses some rough guesses about the probability of
C some of these events, combined with guesses about seed production
C per female plant, and the relationship between DM rating and the
C density of plants required to achieve that rating. These *guesses*
C merely get the number of new infections in the ball park. Fine 
C tuning is based on user-supplied scaling terms applied through the
C DMSTUN, DMITUN and DMETUN keywords. Once the model is more
C mature, I hope that the terms put in those keywords are hardwired,
C so that they will behave sensibly with their default values of 1.0
C
C For the record, here are the guesses:
C
C  P(adhering successfully to a branch tip): 0.05
C  P(germinating):                           0.15
C  P(being female):                          0.5
C
C Other factors accounted for here are:
C  1/  actual seed production per plant:    10.    (a guess)
C  2/  plants/DMR/observed cubic meter       0.25  (a guess)
C  3/  cubic meters per cubic MESH        MESH**3
C  4/  weight of each observation            0.001 (1e3 trajectories)
C----------------------------------------------------------------------
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
C     INTEGER   Sp    (I) Species code for the tree species being
C                          processed. 
C
C Local variable definitions:
C
C     INTEGER   i         Pointer to treelist record referenced by
C                          'IND()' array.
C     INTEGER   j         Loop counter for crown thirds.
C     INTEGER   jj        Loop counter for treelist records.
C     INTEGER   i1        Pointer to the first treelist record for
C                          the species being processed.
C     INTEGER   i2        Pointer to the last treelist record...
C     REAL      Factor    Gross model adjustment for all processes
C                          leading to successful infection.
C     REAL      FacS      Fine model adjustment for spread.
C     REAL      FacI      Fine model adjustment for intensification.
C
C Common block variables and parameters:
C
C     ISCT      CONTRL
C     MESH      DMCOM
C     TRAJWT    DMCOM
C     DMETUN    DMCOM
C     DMSTUN    DMCOM
C     DMITUN    DMCOM
C     IND1      ARRAYS
C     CRTHRD    DMCOM
C     NewSpr    DMCOM
C     NewInt    DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'                 
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'                 

C Subroutine arguments.

      INTEGER   Sp    

C Local variables.

      INTEGER   i, j, jj, i1, i2
      REAL      Factor, FacS, FacI

      i1 = ISCT(Sp, 1)
      i2 = ISCT(Sp, 2)

      IF (i1 .EQ. 0) RETURN

C General adjustment based on guesses for the processes.

      Factor = .05 * .15 * .5 * 10. * .25 * (MESH ** 3) * TRAJWT 

C Fine-tuning for user-supplied overall establishment (DMETUN),
C Spread (DMSTUN) and Intensification (DMITUN).

      FacS = DMETUN(Sp) * DMSTUN(Sp) * Factor
      FacI = DMETUN(Sp) * DMITUN(Sp) * Factor

      DO 100 jj = i1, i2
        i = IND1(jj)
        DO 200 j = 1, CRTHRD
          NewSpr(i, j) = NewSpr(i, j) * FacS
          NewInt(i, j) = NewInt(i, j) * FacI
  200     CONTINUE
  100   CONTINUE

      RETURN
      END
