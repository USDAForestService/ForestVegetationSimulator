      SUBROUTINE BRTSTA
      IMPLICIT NONE
C**********************************************************************
C  **BRTSTA       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  BRTSTA determines the status of each tree based on the worst
C  canker for the tree.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  15-SEP-2000 Lance R. David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 53) and species temp index variable (I3)
C     are new.
C  16-MAR-2001 Lance R. David (FHTET)
C     Corrected tree status (ITSTAT) assignment so that lowest status
C     for trees that have a total canker count (ITCAN) greater than 0
C     is 1 (non-lethal).
C  21-MAR-2001 Lance R. David (FHTET)
C     Added handling of escape tree status code 9. Once a tree is tagged
C     as an escape/reserve tree, it will never become infected.
C  01-MAY-2001 Lance R. David (FHTET)
C     Expanded tree category scalar variables to arrays.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      INTEGER I1, I2, I3, I4, II, J, KT, MAXNO, NTIM
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRTSTA',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,20) ICYC
   20 FORMAT('Entering subroutine BRTSTA: cycle = ',I2)

C.... If no trees, return.
      IF (ITRN .EQ. 0) GO TO 55

C.... Process host pines in the treelist.
C.... Start species loop

      DO 53 I3 = 1, MAXSP

      IF(BRSPM(I3) .EQ. 0) GO TO 53
C.... Set blister rust species index
      I4=BRSPM(I3)

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 53
      I2=ISCT(I3,2)
      DO 50 J=I1,I2
         KT=IND1(J)

C....    Initialize worst canker variable.
C....    Minimum status for trees with any cankers is 1 non-lethal.

         IF(ITCAN(KT) .GT. 0) THEN
            MAXNO=1
         ELSE
            MAXNO=0
         ENDIF

C....    Loop through cankers for the tree and find the worst one.
C....    Canker categories: 0=clean, 1=non-lethal, 2=prunable,
C....    3=excisable, 4=non-salvable or lethal, 5=topkilled,
C....    7=tree killed.

         DO 30 NTIM=1,ILCAN(KT)
            IF(ISTCAN(NTIM,KT).GT.MAXNO) THEN
               MAXNO=ISTCAN(NTIM,KT)
            ENDIF
   30    CONTINUE

C....    Trees are not killed/topkilled on initialization.
C....    Highest status code possible is 4 (non-salvable).
C....    Status codes of 9 for reserved excape trees are not changed.
         IF(ICYC.EQ.0.AND.MAXNO.GT.4) MAXNO=4
         IF(IBRSTAT(KT).NE.9) IBRSTAT(KT)=MAXNO

C....    Add tree count to total number in that category. If tree has
C....    lethal cankers then reset tree class code.

         IF(IBRSTAT(KT).EQ.0 .OR. IBRSTAT(KT).EQ.9) THEN

C....       Tree is clean (no cankers) or reserved escape, add count

            TBRCLN(I4)=TBRCLN(I4)+PROB(KT)

C....       Loop through 2-inch DBH categories; add to the total for
C....       clean trees; if DBH not < 19", must be => 19"

            DO 31 II=1,9
               IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
                  D2CLN(II)=D2CLN(II)+PROB(KT)
                  GO TO 32
               ENDIF
   31       CONTINUE
            D2CLN(10)=D2CLN(10)+PROB(KT)
   32       CONTINUE
         ELSE IF(IBRSTAT(KT).EQ.1) THEN

C....       Status of worst canker on tree is non-lethal, add count

            TBRNOL(I4)=TBRNOL(I4)+PROB(KT)

C....       Loop through 2-inch DBH categories; add to the total for
C....       non-lethal trees; if DBH not < 19", must be > 19"

            DO 33 II=1,9
               IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
                  D2NOL(II)=D2NOL(II)+PROB(KT)
                  GO TO 34
               ENDIF
   33       CONTINUE
            D2NOL(10)=D2NOL(10)+PROB(KT)
   34       CONTINUE
         ELSE IF(IBRSTAT(KT).EQ.2) THEN

C....       Status of worst canker on tree is prunable, add count

            TBRPRN(I4)=TBRPRN(I4)+PROB(KT)

C....       Loop through 2-inch DBH categories; add to the total for
C....       prunable trees; if DBH not < 19", must be > 19"

            DO 35 II=1,9
               IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
                  D2PRN(II)=D2PRN(II)+PROB(KT)
                  GO TO 36
               ENDIF
   35       CONTINUE
            D2PRN(10)=D2PRN(10)+PROB(KT)
   36       CONTINUE
         ELSE IF(IBRSTAT(KT).EQ.3) THEN

C....       Status of worst canker on tree is excisable, add count

            TBREXC(I4)=TBREXC(I4)+PROB(KT)

C....       Loop through 2-inch DBH categories; add to the total for
C....       excisable trees; if DBH not < 19", must be > 19"

            DO 37 II=1,9
               IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
                  D2EXC(II)=D2EXC(II)+PROB(KT)
                  GO TO 38
               ENDIF
   37       CONTINUE
            D2EXC(10)=D2EXC(10)+PROB(KT)
   38       CONTINUE
         ELSE IF(IBRSTAT(KT).EQ.4) THEN

C....       Status of worst canker on tree is non-salvable, add count

            TBRNOS(I4)=TBRNOS(I4)+PROB(KT)

C....       Loop through 2-inch DBH categories; add to the total for
C....       non-salvable trees; if DBH not < 19", must be > 19"

            DO 39 II=1,9
               IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
                  D2NOS(II)=D2NOS(II)+PROB(KT)
                  GO TO 40
               ENDIF
   39       CONTINUE
            D2NOS(10)=D2NOS(10)+PROB(KT)
   40       CONTINUE
         ELSE IF(IBRSTAT(KT).EQ.5) THEN

C....       Status of worst canker on tree is topkilled, add count

            TBRGIR(I4)=TBRGIR(I4)+PROB(KT)
            IMC(KT)=3

C....       Loop through 2-inch DBH categories; add to the total for
C....       topkill trees; if DBH not < 19", must be > 19"

            DO 41 II=1,9
               IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
                  D2GIR(II)=D2GIR(II)+PROB(KT)
                  GO TO 42
               ENDIF
   41       CONTINUE
            D2GIR(10)=D2GIR(10)+PROB(KT)
   42       CONTINUE
         ENDIF

C....    After a compression, it is possible for a record to represent
C....    both live and dead trees; therefore, BRPB must be added for
C....    all trees.  BRPB will be 0 if the tree is not dead.

         TBRMRT(I4)=TBRMRT(I4)+BRPB(KT)

C....    Loop through 2-inch DBH categories; add to the total for
C....    dead trees; if DBH not < 19" or less, must be > 19"

         DO 43 II=1,9
            IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
               D2DED(II)=D2DED(II)+BRPB(KT)
               GO TO 44
            ENDIF
   43    CONTINUE
         D2DED(10)=D2DED(10)+BRPB(KT)
   44    CONTINUE

C....    Calculate total number of trees per acre that are white pine.
C....    This should be the sum of all the canker status categories
C....    listed above excluding trees killed by blister rust this cycle.

         TBRHST(I4)=TBRHST(I4)+PROB(KT)+BRPB(KT)

C....    Loop through 2-inch DBH categories; add to the total for
C....    all white pines; if DBH not < 19" or less, must be > 19"

         DO 45 II=1,9
            IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
               D2WP(II)=D2WP(II)+PROB(KT)
               GO TO 46
            ENDIF
   45    CONTINUE
         D2WP(10)=D2WP(10)+PROB(KT)
   46    CONTINUE
   50 CONTINUE

C.... End species loop.
   53 CONTINUE

C.... Common return.

   55 CONTINUE
      IF(DEBUG) WRITE(JOSTND,60) ICYC
   60 FORMAT('Leaving subroutine BRTSTA: cycle = ',I2)
      RETURN
      END
