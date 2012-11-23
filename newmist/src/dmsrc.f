      SUBROUTINE DMSRC (Sp, D, Ptr, Index, P, SrcI, SrcCD, SPtr)

C********************************************************************
C  **DMSRC -- NISI  Date of last revision April 14 1994
C--------------------------------------------------------------------
C  Purpose:
C    The model requires the selection of source trees to surround
C each target. Further, it needs to select these trees from each DM
C category based on the DM category's density and the autocorellation
C that may be present. All these require a quick way to select these
C trees. This routine serves that purpose, returning three objects.
C The first, 'SPtr()', holds six array positions that are the break
C points between the DM categories. Within these breakpoint groups,
C 'SrcCD()' holds the cumulative distribution of treelist records
C within the DM category. This cumulative distribution can be used
C with a random number to select trees at random. Last, 'SrcI()' 
C holds the treelist record itself, allowing the unique attributes
C (e.g.: height, crown and DM rating) of an individual tree to be
C used. This routine may have the dubious distinction of having the
C highest comment/code ratio!
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
C     INTEGER Sp      (I) The species code being processed
C     REAL    D       (I) The density of each DM category of species
C                          'Sp'.
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
C     INTEGER SrcI    (O) Pointer to the treelist index occupying
C                          corresponding to the cumulative
C                          probability within the 'SrcCD()' array.
C                          These records are sorted by DM category
C                          into groups marked by the 'Sptr()' array.
C     REAL    SrcCD   (O) The cumulative probability of each DM group
C                          is computed by taking the relative 
C                          density (trees/acre) of each treelist
C                          record and forming a cumulative
C                          distribution.
C     INTEGER Sptr    (O) Breakpoints demarcating the DM categories
C                          ordered within the 'SrcI() and 'SrcCD()'
C                          arrays. Each value marks the *last* entry
C                          in that category: e.g.: 'Sptr(3)' contains 
C                          the position of the last position with DM
C                          rating 3; 'Sptr(2)+1' contains the first. 
C
C Local variable definitions:
C     
C     INTEGER i           Loop over DM categories.
C     INTEGER j           Loop over elements of each DM category.
C     INTEGER k           Counter for absolute position in 'SrcI()'
C                          and 'SrcCD()' arrays.
C     REAL    x           Weighting term for each treelist record,
C                          based on total density (trees/acre) within
C                          each DM category.
C     REAL    y           The current term for the cumulative
C                          distribution
C
C Common block variables and parameters:
C
C     MAXSP     PRGPRM
C     MAXTRE    PRGPRM
C     FST       DMCOM
C     LST       DMCOM
C     DMTINY    DMCOM                                                
C
C********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER Sp
      REAL    D
      INTEGER Ptr
      INTEGER Index
      REAL    P
      INTEGER SrcI
      REAL    SrcCD
      INTEGER Sptr

      DIMENSION D(0:6)
      DIMENSION Ptr(MAXSP, 0:6, LST)
      DIMENSION Index(MAXTRE)
      DIMENSION P(MAXTRE)
      DIMENSION SrcI(MAXTRE)
      DIMENSION SrcCD(MAXTRE)
      DIMENSION Sptr(0:6)

C Local variables.

      INTEGER i, j, k
      REAL    x, y

      k = 0
      DO 100 i = 0, 6
        IF (Ptr(Sp, i, FST) .GT. 0) THEN  
          x = 1.0 / (D(i) + DMTINY)
          y = 0.0
          DO 200 j = Ptr(Sp, i, FST), Ptr(Sp, i, LST)
            k = k + 1
            y = y + P(Index(j)) * x
            SrcI(k) = Index(j)
            SrcCD(k) = y 
  200     CONTINUE
        END IF
      SPtr(i) = k
  100 CONTINUE

      RETURN
      END
