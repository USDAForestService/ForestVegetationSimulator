      SUBROUTINE DMFDNS (Sp, Ptr, Index, P, D)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMFDNS -- NISI  Date of last revision April 7 1994
C--------------------------------------------------------------------
C Purpose:
C   Find the density (trees/acre) of each DM class. This information
C is required for the calculation of probability distributions used
C to simulate the likelihood of neighbor-neighbor combinations. 
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
C     INTEGER Sp      (I) Species code, meaning is variant-dependent.
C     INTEGER Ptr     (I) Matrix of **pointers to treelist, sorted
C                          by species and DM class:
C                          Index 1: Species code.
C                          Index 2: DM rating.
C                          Index 3: FST is first position in 
C                                   'Index()'.
C                          array; LST is last position in array. This
C                          mapping is analagous to the 'IND1()' array
C                          of the base model, but with two levels of
C                          indirection.
C     INTEGER Index   (I) Array containing pointers to the treelist.
C                          It is sorted by species and DM rating and
C                          is an analogue of the 'IND1()' array of
C                          the base model.
C     REAL    P       (I) The density (trees/acre) of each record of
C                          the treelist.
C     REAL    D       (O) The density of each DM category of species
C                          'Sp'.
C
C Local variable definitions:
C     
C     INTEGER i           Loop counter for DM categories.
C     INTEGER j           Loop counter for treelist elements.
C
C Common block variables and parameters:
C
C     MAXSP   PRGPRM
C     MAXTRE  PRGPRM
C     FST     DMCOM
C     LST     DMCOM 
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER   Sp
      INTEGER   Ptr
      INTEGER   Index
      REAL      P
      REAL      D

      DIMENSION Ptr(MAXSP, 0:6, LST)
      DIMENSION Index(MAXTRE)
      DIMENSION P(MAXTRE)
      DIMENSION D(0:6)

C Local variables.

      INTEGER i, j

C Begin by setting the density to zero, then accumulate density for
C by adding the density 'P()' from each appropriate entry of the 
C treelist. The appropriate trees are stored by the references
C stored in the 'Ptr()' array.

      DO 100 i = 0, 6
        D(i) = 0.
        IF (Ptr(Sp, i, FST) .GT. 0) THEN
          DO 110 j = Ptr(Sp, i, FST), Ptr(Sp, i, LST)
            D(i) = D(i) + P(Index(j))
  110     CONTINUE
        END IF
  100 CONTINUE

      RETURN
      END
