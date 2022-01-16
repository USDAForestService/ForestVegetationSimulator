      SUBROUTINE BRCSTA
      IMPLICIT NONE
C**********************************************************************
C  **BRCSTA       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  BRCSTA determines the status of cankers on each tree during the
C  initialization cycle.  Status of cankers during cycling is
C  determined in BRCGRO.
C
C  This routine assigns status codes to each canker which is tracked
C  by the White Pine Blister Rust Model (a maximum of ten cankers/tree).
C  Descriptions of common variables are in BRCOM.F77.
C  The canker status codes are:
C     0 - inactive or no cankers
C     1 - non-lethal
C     2 - prunable
C     3 - excisable
C     4 - non-salvable
C     5 - canker has top-killed the tree
C     6 - (this code is not currently used)
C     7 - canker has killed the tree
C
C  This routine was coded from discussions between John Schwandt,
C  Jan Savidge, and Lance David during John's visit to MAG for
C  initial investigation of the White Pine Blister Rust Model.
C  John drew the canker status assignment flow chart from which
C  Lance David wrote this code (1990).
C----------------------------------------------------------------------
C
C  Local Variables:
C  ----------------
C     BRDBH  - current tree DBH in centimeters
C     EXHT   - absolute maximum excise height (cm)
C     GIRD   - percent of tree CANKER HAS GIRDLED. value 0.0-100.0
C     HTBCR  - height (cm) to base of crown for current tree.
C     I1     - (integer) counter
C     I2     - (integer) counter
C     I3     - (integer) FVS species index
C     J      - (integer) counter used to cycle through index array,
C              IND1, for wpbr host species.
C     K      - (integer) holds value from IND1 array which identifies
C              the appropriate array index for the current "tree"
C     L      - (integer) counter used to cycle through all cankers
C              for the current tree.
C     NLCAN  - (integer) number of lethal cankers for a single tree
C              loaded from array ILCAN.
C     OUT    - holds current canker's distance out on the branch in
C              centimeters. loaded from DOUT array.
C     PRHT   - absolute maximum prune height (cm)
C     PHTST  - max prune height (cm) for current tree.
C     UP     - holds current canker's distance up the tree in
C              centimeters. loaded from DUP array.
C     EXCNCT - this is a counter for the number of excisable cankers
C              on the current tree.
C----------
C  Conversion equations:
C     centimeters = inches * 2.54
C     centimeters = feet * 30.48
C----------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 475) and species temp index variable (I3)
C     are new.
C  07-MAY-2001 Lance R. David (FHTET)
C     Added debug code.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'


C.... Local variable declarations.

      REAL BRDBH,EXHT,GIRD,HTBCR,OUT,PHTST,PRHT,UP
      INTEGER I1,I2,I3,J,K,L,NLCAN,EXCNCT
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCSTA',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
  10  FORMAT('Entering subroutine BRCSTA: cycle = ',I2)

C.... Set prune and excise maximum height guidelines.  Heights are in
C.... centimeters.

      PRHT=HTMAX(1)
      EXHT=HTMAX(2)

      IF(DEBUG) WRITE(JOSTND,*) ' IN BRCSTA: PRHT=',PRHT,' EXHT=',
     & EXHT,' GIRMAX=',GIRMAX
C.... Process host trees. If no trees, return.

      IF (ITRN .EQ. 0) GO TO 500

C.... Start species loop

      DO 475 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 475

      IF(DEBUG) WRITE(JOSTND,*) ' IN BRCSTA: PROCESSING SPECIES: ',I3

      I1=ISCT(1,1)
      IF(I1.EQ.0) GO TO 475
      I2=ISCT(1,2)

C.... Top of tree loop.

      DO 450 J=I1,I2
         K=IND1(J)
         NLCAN=ILCAN(K)

C....    If this tree has no cankers, go to bottom of tree loop.

         IF(NLCAN.EQ.0) GO TO 450

C....    Set local variables:
C....    Height to base of crown (HTBCR) from Blister Rust
C....    tree variable (BRHTBC) and trees per acre.

         HTBCR=BRHTBC(K)

C....    If tree was input as top killed, set value of
C....    UPMARK to top kill height in centimeters.
C....    UPMARK is centimeters, ITRUNC is in 1/100s of feet.

         IF(ITRUNC(K).GT.0) UPMARK(K)=(FLOAT(ITRUNC(K))/100.0)*30.48

C....    Set prune height for the tree; prune height is the lesser of
C....    the absolute maximum pruning height and the proportion of total
C....    pruning height value.

         PHTST=HTPRPR*HT(K)*30.48
         IF(PHTST.GT.PRHT) PHTST=PRHT

C....    Get the current tree DBH; used in testing the tree for
C....    excising; convert it to centimeters.

         BRDBH=DBH(K)*2.54

C....    Initialize the exciseable canker counter and the logical flag
C....    for multiple excisable cankers on the current tree.

         EXCNCT=0
         LEXMLT(K)=.FALSE.

C....    Process all cankers for the tree. Top of canker loop.

         DO 400 L=1,NLCAN
            UP=DUP(L,K)
            OUT=DOUT(L,K)
            GIRD=GIRDL(L,K)

C....       Assign a status code to the canker.
C....       NOTE: This same code is used in subroutine BRCGRO to
C....       assign status codes to cankers after they have been grown.
C....       Some variable names differ, but the logic is the same.
C....       DON'T change this code without changing BRCGRO.F77 as well.

            IF(OUT.EQ.0.0) THEN

C....          This is a bole canker.

               IF(UP.LE.EXHT.AND.UP.GE.HTMIN.AND.BRDBH.GE.EXDMIN) THEN

C....             Canker is within excisable height range and
C....             minimum tree DBH excise threshold.

                  IF(GIRD.LE.GIRMAX) THEN

C....                Canker is excisable.

                     ISTCAN(L,K)=3
                  ELSE IF(GIRD.GE.GIRMRT) THEN

C....                This canker has girdled enough to kill.

                     IF(UP.LE.HTBCR) THEN

C....                   The canker is below the crown which kills
C....                   the tree.

                        ISTCAN(L,K)=7
                     ELSE

C....                   Top kill.

                        ISTCAN(L,K)=5
                     ENDIF
                  ELSE

C....                Canker is too large to excise, but has not
C....                reached the lethal limit GIRMRT.
C....                Status is non-salvable.

                     ISTCAN(L,K)=4
                  ENDIF
               ELSE

C....             Canker is not within the excisable height range.

                  IF(GIRD.GE.GIRMRT) THEN

C....                Girdling has reached lethal level

                     IF(UP.LE.HTBCR) THEN

C....                   Canker is below crown; kill tree.

                        ISTCAN(L,K)=7
                     ELSE

C....                   Top kill.

                        ISTCAN(L,K)=5
                     ENDIF
                  ELSE

C....                Canker status non-salvable.

                     ISTCAN(L,K)=4
                  ENDIF
               ENDIF
            ELSE IF(OUT.GE.OUTNLD) THEN

C....          Canker far enough out on the branch to be non-lethal.

               ISTCAN(L,K)=1
            ELSE IF(OUT.GE.OUTDST) THEN

C....          Canker is far enough out on the branch to be
C....          pruned, but must also pass pruning height test.

               IF(UP.LE.PHTST) THEN

C....             Canker is within pruning height - prunable.

                  ISTCAN(L,K)=2
               ELSE

C....             Canker is too high on the tree to prune -
C....             non-salvable.

                  ISTCAN(L,K)=4
               ENDIF
            ELSE

C....          This is a branch canker, but it is too close to the
C....          bole for pruning. So, it will be tested against
C....          excising specifications as if it were a bole canker.
C....          Branch cankers have no girdling, so only height and
C....          tree DBH are checked.

               IF(UP.LE.EXHT.AND.UP.GE.HTMIN.AND.BRDBH.GE.EXDMIN) THEN

C....             Canker is within excising height; excisable.

                  ISTCAN(L,K)=3
               ELSE

C....             Canker status non-salvable.

                  ISTCAN(L,K)=4
               ENDIF
            ENDIF

C....       Determine if there are multiple excisable cankers on the
C....       current tree.  If so, set LEXMLT to TRUE.  This will be
C....       used in the routine BRCREM; if excising is scheduled and
C....       there is more than 1 excisable canker on the tree, then
C....       no excise will happen at all.

            IF(ISTCAN(L,K).EQ.3) THEN
               EXCNCT=EXCNCT+1
               LEXMLT(K)=(EXCNCT.GT.1)
            ENDIF

C....    End of code in common with BRCGRO.F77.
C....    Bottom of canker loop.

         IF(DEBUG) WRITE(JOSTND,*) ' IN BRCSTA: TREE:',K,' CANKER:',L,
     &   'UP,OUT,GIRD,STAT:',UP,OUT,GIRD,ISTCAN(L,K)
  400    CONTINUE

C.... Bottom of tree loop.

  450 CONTINUE

C.... End species loop
  475 CONTINUE

C.... Common return.

  500 CONTINUE
      IF(DEBUG) WRITE(JOSTND, 501) ICYC
  501 FORMAT('Leaving subroutine BRCSTA: cycle = ',I2)
      RETURN
      END
