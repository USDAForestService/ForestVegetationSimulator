      SUBROUTINE DMTLST (Sp, tDMR, Ptr, Index, P, TLst)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  **DMTLST -- NISI  Date of last revision April 16 1994
C--------------------------------------------------------------------
C Purpose:
C   To simulate spread the model places target trees at the center 
C of sampling rings. This routine chooses those source trees,
C and is called once for each DM category. When it was first
C developed, it made its choices based on trees/acre density of each
C treelist record. This was subsequently modified (see commented-out
C code below) such that the relative proportion of the record was
C used to penalize the likelihood of choosing a record. This made it
C less likely that records with a lot of influence would skew the
C simulation. However, no *other* FVS extension behaves this way, and
C it has since been removed. Because of this change, this routine is
C now much simpler, and now only takes all the trees from the 
C specified DM category and puts those tree's indices in the proper
C place to be accessed by the calling routine. 
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
C     INTEGER Sp      (I) The species code being processed.
C     INTEGER tDMR    (I) The DM category being processed.
C     INTEGER Ptr     (I) Matrix of **pointers to treelist, sorted
C                          by species and DM class:
C                          Index 1: Species code.
C                          Index 2: DM rating.
C                          Index 3: FST is first position in 
C                                   'Index()'.
C     INTEGER Index   (I) Array containing pointers to the treelist.
C                          It is sorted by species and DM rating and
C                          is an analogue of the 'IND1()' array of
C                          the base model.
C     REAL    P       (I) The density (trees/acre) of each record of
C                          the treelist.
C     INTEGER TLst    (O) The trees to be used as targets; the
C                          zero'th entry contains the number of trees
C                          in the list.
C
C Local variable definitions:
C     
C     INTEGER i           Loop counter for treelist records.
C     INTEGER j           Index to treelist records from 'i'.
C     INTEGER k           Number of trees to place as targets.
C
C Common block variables and parameters:
C
C     MAXSP   PRGPRM
C     FST     DMCOM
C     LST     DMCOM
C     MAXTRE  PRGPRM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER Sp
      INTEGER tDMR
      INTEGER Ptr
      INTEGER Index
      REAL    P
      INTEGER TLst

      DIMENSION Ptr(MAXSP, 0:6, LST)
      DIMENSION Index(MAXTRE)
      DIMENSION P(MAXTRE)
      DIMENSION TLst(0:MAXTRE)

C Local variables.

      INTEGER i, j, k

C If there are no targets, then return empty-handed, setting TLst(0).

      k = 0
      IF (Ptr(Sp, tDMR, FST) .GT. 0) THEN

C Decide if this Target is going to play the game, receiving
C ballistic spread. 'DMGAME' [now defunct keyword] defaults to 1,
C making the likelihood the reciprocal of the trees/acre of the
C record in question. If not playing, fall through the loop.

        DO 100 i = Ptr(Sp, tDMR, FST), Ptr(Sp, tDMR, LST)
          j = Index(i)
          IF (P(j) .GT. 0.) THEN
C            x = DMGAME / (P(j) * StndArea) /* see 'Purpose' for the
C            CALL DMRANN(RND)                  explanation about this
C            IF (RND .LE. x) THEN              this */
               k = k + 1
               TLst(k) = j
C            END IF
          END IF
  100   CONTINUE
       END IF
      TLst(0) = k

      RETURN
      END
