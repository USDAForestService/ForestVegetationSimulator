      SUBROUTINE DMSLST (DMRCls, n, SInd, SCD, SPtr, SLst)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMSLST -- NISI  Date of last revision: April 14 1994 
C--------------------------------------------------------------------
C Purpose:
C   To simulate spread the model places source trees in sampling
C rings around each target. This routine chooses those source trees,
C and is called once for each DM category. It makes its choices
C based on the number of trees it has been 'told' to find, choosing
C at random from the relevant DM category based on the density
C (trees/acre) of each treelist record. The attributes of those
C records are subsequently used to simulate the spatial relationships
C between sources and target.
C--------------------------------------------------------------------
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
C     INTEGER DMRCls  (I) The DM category from which trees will be
C                          chosen at random.
C     INTEGER n       (I) The number of trees to choose.
C     INTEGER SInd    (I) Pointer to the treelist index occupying
C                          corresponding to the cumulative
C                          probability within the 'SrcCD()' array.
C                          These records are sorted by DM category
C                          into groups marked by the 'Sptr()' array.
C     REAL    SCD     (I) The cumulative probability of each DM 
C                          group is computed by taking the relative 
C                          density (trees/acre) of each treelist
C                          record and forming a cumulative
C                          distribution.
C     INTEGER SPtr    (I) Breakpoints demarcating the DM categories
C                          ordered within the 'SInd() and 'SCD()'
C                          arrays. Each value marks the *last* entry
C                          in that category: eg: 'Sptr(3)' contains 
C                          the position of the last position with DM
C                          rating 3; 'Sptr(2)+1' contains the first. 
C     INTEGER SLst    (O) List of trees selected to occupy the
C                          sampling ring being processed.
C                          Index 1: The index value to the treelist
C                                   record.
C                          Index 2: The number of occurrences of the
C                                   record.
C
C Local variable definitions:
C
C     LOGICAL  flg        A flag that is .TRUE. whenever a treelist
C                          record has already been chosen; otherwise
C                          it is .FALSE.
C     INTEGER  i          Loop counter for the number of trees to 
C                          include in the sample 'n'.
C     INTEGER  j          Loop counter for the relevant portion of
C                          the 'SCD()' and 'SInd' arrays.
C     INTEGER  k          Loop counter for the number of unique
C                          treelist records already included.
C     INTEGER  m          An incremental counter that records the
C                          number of unique treelist records that
C                          have been selected.
C     INTEGER  pFrst      Position of first cumulative distribution
C                          and treelist record in 'SCD()' and 'SInd()'
C                          arrays.
C     INTEGER  pLast      Position of last (etc.) in arrays.
C     REAL     RND        A uniform {0,1} random number.
C
C Common block variables and parameters:
C
C     MAXTRE   PRGPRM
C     DSTLEN   DMCOM
C     INDX     DMCOM
C     KNT      DMCOM
C
C********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER   DMRCls
      INTEGER   n
      INTEGER   SInd
      REAL      SCD
      INTEGER   SPtr
      INTEGER   SLst

      DIMENSION SInd(MAXTRE)
      DIMENSION SCD(MAXTRE)
      DIMENSION SPtr(0:6)
      DIMENSION SLst(0:DSTLEN, INDX)

C Local variables.

      LOGICAL   flg
      INTEGER   i, j, k, m
      INTEGER   pFrst, pLast
      REAL      RND

C Compute pointers to the location of the distribution within the 
C 'SCD()' array. Note that 'SPtr()' is the position of the *last*
C item in the set.

      IF(DMRCls .EQ. 0) THEN
        pFrst = 1
      ELSE
        pFrst = SPtr(DMRCls - 1) + 1
      END IF

      pLast = SPtr(DMRCls)

C Loop through the 'i' samples to be drawn, calling a random
C number each time. Then walk through the 'j' values of the 
C cumulative distribution. When a source is chosen, check to see if
C it has already been selected, and increment KNT if required.
C 'SLst(0, INDX)' holds the number of unique source trees used to
C generate the sample.

      m = 0
      DO 200 i = 1, n
        CALL DMRANN(RND)
        DO 300 j = pFrst, pLast
          IF (RND .LE. SCD(j)) THEN
            flg = .FALSE.
            DO 400 k = 1, m
              IF (SLst(k, INDX) .EQ. SInd(j)) THEN
                SLst(k, KNT) = SLst(k, KNT) + 1
                flg = .TRUE.
                GOTO 401
              END IF
  400       CONTINUE
  401       CONTINUE

C Initialize a new element of 'SLst()' if the treelist record
C 'SInd()' is new.

          IF (.NOT.flg) THEN
            m = m + 1
            SLst(m, KNT) = 1
            SLst(m, INDX) = SInd(j)
          END IF

          GOTO 200
          END IF

  300   CONTINUE
  200 CONTINUE

      SLst(0, INDX) = m

      RETURN
      END
