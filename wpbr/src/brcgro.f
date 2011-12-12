      SUBROUTINE BRCGRO(K,PROP,BRHT,BRHTG,BRDG,NLCAN,HNEW,DNEW)
C**********************************************************************
C  **BRCGRO       DATE OF LAST REVISION:  11/07/2002
C----------------------------------------------------------------------
C  Purpose:
C  BRCGRO grows cankers for one year and determines if any active
C  cankers will become inactive based on the specified inactivation
C  rate and a random number.  Cankers are also inactivated when:
C     - a branch or bole canker is located above the actual height
C       of the tree (current height or topkill height). If new cankers,
C       in addition to pre-existing cankers, are to be processed,
C       the actual height should be the height at end of cycle.
C     - a branch canker is located below the tree's base of crown,
C       because natural pruning can occur in Prognosis.
C
C  New cankers added during the current cycle are not processed
C  and this is accomplished with the parameter NLCAN.  If you want
C  to grow and inactivate newly added cankers, the common variable
C  ILCAN(K) could be used in place of NLCAN.
C
C  The processing order in BRCGRO is as follows:
C     - If canker is inactive, skip to next canker.
C     - Random number is drawn to determine if the current canker
C       will be inactivated.  If the canker is inactivated, set
C       status code and skip to next canker.
C     - Grow canker.
C     - Assign canker's status code.
C     - If canker status is topkill or tree kill, adjust tree
C       attributes: for tree kill - adjust BRPB(K), WK2(K);
C       for topkill - adjust UPMARK(K), ITRUNC(K), NORMHT(K), ICRED(K);
C       (If tree has more than one canker which has caused topkill,
C       the lowest one is used for the final calculation.)
C
C  Note: BRCGRO is not called if a tree is dead already.
C
C  Parameters passed:
C  ------------------
C     K      - (integer) holds value from IND1 array which identifies
C              the appropriate array index for the current "tree"
C     PROP   - (real) proportion of full cycle represented at current
C              year.  This is used because current year values are
C              calculated from values at start of cycle and adding
C              the appropriate proportion of the growths (height,
C              diameter). PROP values for yearly processing of a
C              5 year cycle would be: 1/5, 2/5, 3/5, 4/5, 5/5.
C     BRHT   - (real) tree height in meters at start of cycle.
C              If tree is top killed before or during the cycle,
C              this value is calculated based on the truncation
C              height in BRTREG.
C     BRHTG  - (real) tree height growth in meters for this year.
C              If tree is top killed before or during the cycle,
C              BRHTG is set to 0 in BRTREG.
C     BRDG   - (real) tree diameter growth in cm for cycle.
C     NLCAN  - (integer) number of lethal cankers for a single tree
C              before new cankers are added.
C     HNEW   - (real) tree height in feet for this year.  A proportion
C              of the height growth predicted by FVS has been added.
C              Top kill is not reflected.
C     DNEW   - (real) tree DBH in inches for this year.  A proportion
C              of the diameter growth predicted by FVS has been added.
C
C  Local Variables:
C  ----------------
C     BCL    - beginning crown length (i.e. before topkill) in cm.
C     BRGDY  - tree ground diameter in cm this year.
C     BRHYR  - tree height in cm this year.
C     DGPROP - tree diameter growth (cm) for this year.
C     EXHT   - absolute maximum excise height (cm)
C     EXPCT  - percent used to calculate excise hgt for each tree
C     GIRAMT - circumference of tree in cm at canker height.
C     GIRD   - percent of tree CANKER HAS GIRDLED. value 0.0-100.0
C     GROBOL - the cankers circumference growth this year.
C     HNEWCM - (real) value of HNEW converted to centimeters.
C     DNEWCM - (real) value of DNEW converted to centimeters.
C     HTBCR  - height (cm) to base of crown for current tree.
C     HTGPRP - height growth in cm for this year.
C     JCSTAT - holds current canker's status code.
C     NCAN   - (integer) used to cycle through cankers.
C     OUT    - holds current canker's distance out on the branch in
C              centimeters. loaded from DOUT array.
C     PCTREM - percent crown remaining after topkill has occurred.
C     POTST  - this is the maximum distance out (in cm) that a canker
C              can be on a branch to avoid natural pruning when below
C              the base of the crown.
C     PRHT   - absolute maximum prune height (cm)
C     PRPCT  - percent used to calculate pruning hgt for each tree
C     PHTST  - max prune height (cm) for current tree.
C     RCL    - remaining crown length (i.e. after topkill) in cm.
C     UP     - holds current canker's distance up the tree in
C              centimeters. loaded from DUP array.
C     XRAN   - holds random number used to determine if a canker
C              will become inactive.
C     EXCNCT - this is a counter for the number of excisable cankers
C              on the current tree in the current cycle.
C
C----------
C  Conversion equations:
C     centimeters = inches * 2.54
C     centimeters = feet * 30.48
C----------
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  16-APR-1999 Lance R. David (FHTET)
C     Corrected canker inactivation logic that put bole cankers in
C     double jeopardy.
C  06-MAR-2001 Lance R. David (FHTET)
C     Constant growth rate values in growth equations for branch and
C     bole cankers (5.0 and 4.5 respectively) has been replaced by
C     variables BRGRTH and BOGRTH.
C  11-MAY-2001 Lance R. David (FHTET)
C     Added accumulator for total BR historical mortality (TBRHMR).
C  16-MAY-2001 Lance R. David (FHTET)
C     Expanded canker growth rate variables by species and stock type.
C  07-NOV-2002 Lance R. David (FHTET)
C     Added condition so that circumference of the stem at the canker
C     must be greater than the bole canker growth rate with additional
C     buffer of 25% before bole canker growth is applied.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

C.... Local variable declarations.

      INTEGER JCSTAT,K,NCAN,NLCAN,EXCNCT,I4,I5
      REAL BRDG,BRHT,BRHTG,HNEW,HNEWCM,DNEW,DNEWCM,PROP,BRGDY,BRHYR,
     &   DGPROP,EXHT,GIRAMT,GIRD,GROBOL,HTBCR,HTGPRP,OUT,PRHT,PHTST,
     &   UP,XRAN
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCGRO',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT (' Entering subroutine BRCGRO: cycle = ',I2)
      IF(DEBUG) WRITE(JOSTND,*) ' PARAMETERS=',
     &          K,PROP,BRHT,BRHTG,BRDG,NLCAN,HNEW,DNEW

C.... Set Blister rust host species and Stock type indices.

      I4=BRSPM(ISP(K))
      I5=ISTOTY(K)

C.... Calculate height thresholds for pruning this tree. Maximum height
C.... to prune is the lesser value of a maximum absolute height and a
C.... percentage of the tree's height.
C.... Heights are in centimeters.

      PRHT=HTMAX(1)
      PHTST=HTPRPR*HNEW*30.48
      IF(PHTST.GT.PRHT) PHTST=PRHT

C.... Set maximum height threshold for excising this tree.  Convert the
C.... current DBH to centimeters (excising also has a minimum DBH
C.... threshold).

      EXHT=HTMAX(2)
      DNEWCM=DNEW*2.54

C.... Set height to base of crown guideline for other excise and prune
C.... limits used with the heights calculated above to determine canker
C.... status.

      HTBCR=BRHTBC(K)

C.... Set pruning distance out guideline (in cm) for whether a branch
C.... canker below the base of the crown gets naturally pruned; i.e. if
C.... a branch canker is about to be naturally pruned because it's below
C.... the base of the crown, if it's within 12 inches of the bole it
C.... will be changed to a bole canker so it will not be lost.

      POTST=12*2.54

C.... Calculate yearly height and ground diameter.

      HTGPRP=BRHTG*PROP*100.0
      BRHYR=(BRHT*100.0)+HTGPRP
      DGPROP=BRDG*PROP
      BRGDY=BRGD(K)+DGPROP

C.... Calculate beginning crown length this year; if topkill occurs
C.... in this year then we will use this value to determine the
C.... remaining crown length - if 75% or greater of the crown gets
C.... topkilled then the tree will be considered dead.  Beginning
C.... crown length is the tree height minus height to base of crown.

      BCL=BRHYR-HTBCR

C.... Initialize the exciseable canker counter and the logical flag
C.... for multiple excisable cankers on the current tree in the
C.... current cycle.

      EXCNCT=0
      LEXMLT(K)=.FALSE.

C.... Process pre-existing cankers for the current tree.
C.... New cankers are not grown in the cycle they are added by
C.... subroutine BRECAN.F77. That is why the canker count is passed
C.... to BRCGRO as a parameter (NLCAN) to represent number of cankers
C.... at the start of the current cycle.  Only active cankers are
C.... are grown.  Active cankers have status codes 0 - 4.

      DO 400 NCAN=1,NLCAN
         JCSTAT=ISTCAN(NCAN,K)
         UP=DUP(NCAN,K)
         OUT=DOUT(NCAN,K)
         GIRD=GIRDL(NCAN,K)

         IF(JCSTAT.NE.-1) THEN
            IF(UP.GT.UPMARK(K).OR.UP.GT.BRHYR) THEN

C....          This canker is located above the actual height of the
C....          tree.  The status is set to -1 so it will be removed.

               ISTCAN(NCAN,K)=-1
               JCSTAT=-1
            ELSE IF(OUT.GT.0.0.AND.UP.LT.HTBCR) THEN

C....          This branch canker is below the base of crown.

               IF(OUT.LE.POTST) THEN

C....             This branch canker is close enough to the bole that
C....             we don't want to lose it to natural pruning (the FVS
C....             maximum of 85% for crown ratio is very hard on low
C....             branches which is where many of the cankers live in
C....             real life) so change the close branch canker to a bole
C....             canker to keep it.  Cankers out beyond this distance
C....             (12 inches) will be pruned as usual.

                  OUT=0
                  DOUT(NCAN,K)=0
                  GIRD=0
                  GIRDL(NCAN,K)=0
               ELSE

C....             Natural pruning has occured.  Status is set to -1 for
C....             removal.

                  ISTCAN(NCAN,K)=-1
                  JCSTAT=-1
               ENDIF
            ENDIF
         ENDIF

         IF(JCSTAT.GE.0.AND.JCSTAT.LE.4) THEN

C....       This is an active canker.  Continue processing.
C....       Draw a random number to determine if canker will
C....       become inactive.  Inactivation rate is different for
C....       branch cankers than for bole cankers.

            CALL BRANN(XRAN)
            IF(OUT.GT.0.0.AND.XRAN.LT.RATINV(1)) THEN

C....          Branch canker has been inactivated.  Set canker's status
C....          code and continue to next active canker.

               ISTCAN(NCAN,K)=-1
            ELSE IF(OUT.EQ.0.0.AND.XRAN.LT.RATINV(2)) THEN

C....          Bole canker has been inactivated.  Set canker's status
C....          code and continue to next active canker.

               ISTCAN(NCAN,K)=-1
            ELSE

C....          Canker is active.  Growth for branch or bole canker
C....          canker must be calulated.

               IF(OUT.GT.0.0) THEN

C....             This is a branch canker. Growth is calculated
C....             and applied as a reduction in the distance out.
C....             Uncertain how the 5.0 constant was derived, but
C....             according to Geral Mcdonald and his notes, this
C....             value is correct.  The distance out is calculated
C....             based on the canker growing in to meet the bole,
C....             and the bole growing out to meet the canker.
C....             Calculate new distance from bole.
C....             The growth rate constant of 5.0 has been replaced by
C....             variable BRGRTH.

                  OUT=OUT-(BRGRTH(I4,I5)-(0.5*DGPROP))
                  IF(OUT.LT.0.0) OUT = 0.0
                  DOUT(NCAN,K)=OUT
               ELSE

C....             This is a bole canker. Growth is calculated and
C....             applied as an increase in the canker's girdle
C....             percentage.
C....             GIRAMT is circumference of tree in centimeters
C....             at height of canker, based on current year
C....             diameter at ground and normal height (no top kill).
C....             GROBOL is amount of that circumference which
C....             will be invaded by one year of canker growth.
C....             Growth rate constant of 4.5 has been replaced by
C....             variable BOGRTH.

                  IF(DEBUG) WRITE(JOSTND,*) ' GROW BOLE CANKER'

                  HNEWCM=HNEW*30.48
                  GIRAMT=BRPI*BRGDY*((HNEWCM-UP)/HNEWCM)

C....             07-NOV-2002
C....             If current stem circumference is less than canker
C....             growth rate with 25% buffer, do not grow the canker.

                  IF(GIRAMT .LT. (BOGRTH(I4,I5)*1.25)) THEN
                    GROBOL=0.0
                  ELSE
                    GROBOL=BOGRTH(I4,I5)-(DGPROP*BRPI)
                  ENDIF

                  IF(GROBOL.LT.0.0) GROBOL=0.0
                  IF(GROBOL.GT.GIRAMT) GROBOL=GIRAMT

C....             Add canker growth percent to canker girdle percent
C....             for new value of percent girdled. The value can not
C....             be greater the 100 percent.

                  IF(DEBUG) WRITE(JOSTND,*) ' GIRD,GROBOL,GIRAMT=',
     &                                        GIRD,GROBOL,GIRAMT
                  IF(GROBOL .EQ. 0.0 .OR. GIRAMT .EQ. 0.0) THEN
                    CONTINUE
                  ELSE
                    GIRD=GIRD+((GROBOL/GIRAMT)*100.0)
                  ENDIF

                  IF(GIRD.GT.100.0) GIRD=100.0
                  GIRDL(NCAN,K)=GIRD
               ENDIF

C....          Assign canker status code.
C....          NOTE: This same code is used in subroutine BRCSTA to
C....          assign status codes to cankers after they have been
C....          grown. Some variable names differ, but the logic is the
C....          same.  DON'T change this code without changing
C....          BRCSTA.F77 as well.

               IF(OUT.EQ.0.0) THEN

C....             This is a bole canker.

                  IF(UP.LE.EXHT.AND.UP.GE.HTMIN.AND.
     &               DNEWCM.GE.EXDMIN) THEN

C....                Canker is within excisable height range and DBH
C....                of the tree is above the minimum for excising.

                     IF(GIRD.LE.GIRMAX) THEN

C....                   Canker is excisable.

                        ISTCAN(NCAN,K)=3
                     ELSE IF(GIRD.GE.GIRMRT) THEN

C....                   This canker has girdled enough to kill.

                        IF(UP.LE.HTBCR) THEN

C....                      The canker is below the crown which kills
C....                      the tree.

                           ISTCAN(NCAN,K)=7
                        ELSE

C....                      Top kill.

                           ISTCAN(NCAN,K)=5
                        ENDIF
                     ELSE

C....                   Canker is too large to excise, but has not
C....                   reached the lethal limit GIRMRT.
C....                   Status is non-salvable.

                        ISTCAN(NCAN,K)=4
                     ENDIF
                  ELSE

C....                Canker is not within the excisable height and
C....                DBH thresholds.

                     IF(GIRD.GE.GIRMRT) THEN

C....                   Girdling has reached lethal level.

                        IF(UP.LE.HTBCR) THEN

C....                      Canker is below crown. Kill the tree.

                           ISTCAN(NCAN,K)=7
                        ELSE

C....                      Top kill.

                           ISTCAN(NCAN,K)=5
                        ENDIF
                     ELSE

C....                   Canker status non-salvable.

                        ISTCAN(NCAN,K)=4
                     ENDIF
                  ENDIF
               ELSE IF(OUT.GE.OUTNLD) THEN

C....             Canker far enough out on the branch to be non-lethal.

                  ISTCAN(NCAN,K)=1

               ELSE IF(OUT.GE.OUTDST) THEN

C....             Canker is far enough out on the branch to be
C....             pruned, but must also pass pruning height test.

                  IF(UP.LE.PHTST) THEN

C....                Canker is within pruning height - prunable.

                     ISTCAN(NCAN,K)=2
                  ELSE

C....                Canker is too high on the tree to prune -
C....                non-salvable.

                     ISTCAN(NCAN,K)=4
                  ENDIF
               ELSE

C....             This is a branch canker, but it is too close to the
C....             bole for pruning. So, it will be tested against
C....             excising specifications as if it were a bole canker.
C....             Branch cankers have no girdling, so only height and
C....             minimum DBH are checked.

                  IF(UP.LE.EXHT.AND.UP.GE.HTMIN.AND.
     &               DNEWCM.GE.EXDMIN) THEN

C....                Canker is within excising height and tree DBH
C....                threshold; excisable.

                     ISTCAN(NCAN,K)=3
                  ELSE

C....                Canker status non-salvable.

                     ISTCAN(NCAN,K)=4
                  ENDIF
               ENDIF

C....          End of code in common with BRCSTA.F77.
C....          Adjust tree attributes as indicated by new status code.

               IF(ISTCAN(NCAN,K).EQ.5) THEN

C....             Adjust for topkill.
C....             Determine if this is the lowest status 5 canker
C....             on the tree.  The lowest topkilling canker is
C....             used to calculate ITRUNC.  ITRUNC equation adds 1.0
C....             to force rounding up because the real number is
C....             truncated when stored in the integer variable.
C....             NORMHT is calculated as the normal height of the
C....             tree if topkill had not occurred.  FVS variables
C....             ITRUNC, NORMHT are in 1/100s of feet, HT is in feet.
C....             Setting ICRED to 1 signals that this tree's crown
C....             needs to be reduced in subroutine BRCRED.

                  IF(UP.LT.UPMARK(K)) THEN
                     UPMARK(K)=UP
                     ITRUNC(K)=((UP/30.48)*100.0)+1.0
                     IF(NORMHT(K).LE.0.0) NORMHT(K)=(HT(K)*100.0)+0.5
                     ICRED(K)=1
                  ENDIF

C....             Determine if this topkill canker has left the tree
C....             with 25% or less of the beginning crown length; i.e.
C....             if 75% of the crown is lost to topkill, then kill the
C....             tree. Remaining crown length is: height to the lowest
C....             topkill canker minus height to base of crown.  Percent
C....             remaining crown is: remaining crown length divided by
C....             beginning crown length.

                  RCL=UP-HTBCR
                  PCTREM=RCL/BCL*100.0
                  IF(PCTREM.LE.25.0) THEN

C....                Kill the tree.
C....                Adjust mortality arrays.
C....                Add to accumulator for total historical mortality
C....                for the host species.
C....                Set canker and tree status codes.
C....                Skip out of the loop.

                     WK2(K)=PROB(K)*0.99999
                     BRPB(K)=BRPB(K)+PROB(K)
                     TBRHMR(I4)= TBRHMR(I4)+PROB(K)
                     ISTCAN(NCAN,K)=7
                     IBRSTAT(K)=7
                     GO TO 500
                  ENDIF
               ELSE IF(ISTCAN(NCAN,K).EQ.7) THEN

C....             Adjust for tree kill.
C....             The canker has girdled the tree below the base
C....             of the crown. When a status code 7 canker is
C....             encountered for the tree, the loop for processing
C....             all cankers for the current tree is exited.
C....             There is no need to process remaining cankers
C....             for a tree that has already been killed.
C....             Load WK2 with new mortality value and BRPB
C....             with blister rust mortality.
C....             Add to accumulator for total historical mortality
C....             for the host species.
C....             Set tree status to 7 indicate its death so it
C....             will not be processed for following years, if any,
C....             for the current cycle.

   	          IF(UP.LE.HTBCR) THEN
                     WK2(K)=PROB(K)*0.99999
                     BRPB(K)=BRPB(K)+PROB(K)
                     TBRHMR(I4)= TBRHMR(I4)+PROB(K)
                     IBRSTAT(K)=7
                     GO TO 500
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

C....    Determine if there are multiple excisable cankers on the
C....    current tree for the current cycle.  If so, set LEXMLT
C....    to TRUE.  This will be used in the routine BRCREM; if
C....    excising is scheduled and there is more than 1 excisable
C....    canker on the tree, then no excise will happen at all.

         IF(ISTCAN(NCAN,K).EQ.3) THEN
            EXCNCT=EXCNCT+1
            LEXMLT(K)=(EXCNCT.GT.1)
         ENDIF
  400 CONTINUE

C.... Common return.

  500 CONTINUE
      IF(DEBUG) WRITE(JOSTND,501) ICYC
  501 FORMAT (' Leaving subroutine BRCGRO: cycle = ', I2)
      RETURN
      END
