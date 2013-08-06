      SUBROUTINE BRCREM
C**********************************************************************
C  **BRCREM       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRCREM removes cankers that have been classified as inactive
C  or non-lethal.  Also removes excisable cankers when excising
C  is scheduled and prunable cankers when pruning is scheduled.
C  The tree's height to base of crown is reset for trees that
C  have been pruned.
C  Crown ratio carried by Prognosis is not changed.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  24-MAY-1999 Lance David
C     Added debug code to track pruning.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 200) and species temp index variable (I3)
C     are new.
C  05-MAR-2001 Lance David (FHTET)
C     Added pathological pruning control so that crown base height
C     will remain unchanged when a branch canker is pruned off.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C  09-MAY-2006 Lance R. David (FHTET)
C     Changed IND2 to ICNDX.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

C.... Local variable declarations.

      INTEGER I1, I2, I3, ICNDX(10)
      LOGICAL DEBUG,PRUNED

C.... Is debug requested?

      CALL DBCHK(DEBUG,'BRCREM',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,111) ICYC
  111 FORMAT('Entering subroutine BRCREM: cycle = ',I2)

C.... If there are no trees, exit subroutine.

      IF (ITRN .EQ. 0) GO TO 300

C.... Process host species, if any.
C.... Start species loop

      DO 200 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 200

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 200
      I2=ISCT(I3,2)

      DO 30 K=I1,I2
         J=IND1(K)
         PRUNED=.FALSE.
         IVAC=0
         HITE=HT(J)
         NLCAN=ILCAN(J)

C....    Get tree's status, based on the worst canker on the tree.

         ITSTAT=IBRSTAT(J)
         IF(ITSTAT.EQ.7) THEN

C....       Tree status is dead (7).  No treatment is performed.

            GO TO 30
         ELSE IF(ITSTAT.EQ.0.AND.LPRGO.AND.LCLEN) THEN

C....       Prune clean tree.

             PRUNED=.TRUE.
         ELSE

C....       Process cankers for this tree.
C....       Tree status is anything from clean (0) to non-salvable (5).
C....       Clear array that will hold canker indexes to be removed.

            DO 10 M=1,10
               ICNDX(M)=0
   10       CONTINUE

            DO 40 M=1,NLCAN
               ICNDX(M)=M

C....          Get this canker's status.

               ICANT=ISTCAN(M,J)
               IF(ICANT.EQ.-1.OR.ICANT.EQ.1) THEN

C....             Canker is inactive or non-lethal, signal vacancy
C....             in array.  Remove inactive or non-lethal cankers for
C....             all tree statuses (except dead, of course) because
C....             even on non-salvable (4) and topkill (5) trees the
C....             non-lethal cankers continue to grow and soon become
C....             "not" non-lethal.

                  IVAC=IVAC+1
                  ICNDX(M)=-M
               ELSE IF(ICANT.EQ.3.AND.LEXGO.AND.ITSTAT.LE.3) THEN

C....             This is an excisable canker and excising is scheduled.
C....             Don't excise non-salvable (4) and topkill (5) trees.
C....             If random number is less than excising success rate,
C....             the canker will be removed.  If random number is
C....             greater than or equal to the success rate, the canker
C....             has escaped the excising process and the percent
C....             girdle is reset to 1 percent.

C....             First check to see if there are multiple excisable
C....             cankers on the tree in which case no excise will
C....             happen at all (LEXMLT is determined in BRCGRO and
C....             and BRTSTA).

                  IF(.NOT.LEXMLT(J)) THEN

C....                This must be the only excisable canker on this
C....                tree; go ahead and test against a random number.

                     CALL BRANN(VALUE)
                     IF(VALUE.LT.SRATE(2)) THEN
                        IVAC=IVAC+1
                        ICNDX(M)=-M
                     ELSE
                        GIRDL(M,J)=1.0
                     ENDIF
                  ENDIF

                  IF(LPRGO.AND.LCLEN) THEN

C....                Pruning is scheduled and pruning of clean
C....                trees is also specified. Because pruning is
C....                done on cankerless trees, it will also be
C....                done on trees that have excisable cankers.

                     PRUNED=.TRUE.
                  ENDIF
               ELSE IF(ICANT.EQ.2.AND.LPRGO.AND.ITSTAT.LE.3) THEN

C....             This is a prunable canker and pruning is scheduled.
C....             Don't prune non-salvable (4) and topkill (5) trees.
C....             If random number is less than pruning success rate,
C....             the canker will be removed.

                  PRUNED=.TRUE.
                  CALL BRANN(VALUE)
                  IF(VALUE.LT.SRATE(1)) THEN
                     IVAC=IVAC+1
                     ICNDX(M)=-M
                  ENDIF
               ENDIF
   40       CONTINUE
         ENDIF

         IF(PRUNED .AND. .NOT. LPATPR) THEN

C....       Pathological pruning (LPATPR = true) will not change
C....       the height to base of crown for a tree. If set false,
C....       reset the tree's height to base of crown, if it is
C....       not already higher than the absolute pruning height.
C....       Absolute pruning height for this tree must be
C....       calculated.
C....       NOTE: units are in centimeters

            PRHT=HTMAX(1)
            PRHTST=HITE*HTPRPR*30.48
            IF(PRHTST.GT.PRHT) PRHTST=PRHT
            IF(DEBUG) THEN
               WRITE(JOSTND,*) 
     &         ' tree pruned: BRHTBC old=',BRHTBC(J),
     &         ' new(if>)=',PRHTST
            ENDIF
            IF(BRHTBC(J).LT.PRHTST) BRHTBC(J)=PRHTST
         ENDIF

         IF(IVAC.GT.0) THEN

C....       Vacancies have been created in this tree's canker
C....       arrays by removing cankers.
C....       Call BRCDEL to reorganize arrays.

            CALL BRCDEL(IVAC,ICNDX,J)
         ENDIF
   30 CONTINUE

C.... End of species loop
  200 CONTINUE

C.... Common return.

  300 CONTINUE
      IF(DEBUG) WRITE (JOSTND,333) ICYC
  333 FORMAT('Leaving subroutine BRCREM: cycle = ',I2)
      RETURN
      END
