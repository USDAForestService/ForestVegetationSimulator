      SUBROUTINE BRSTYP
C**********************************************************************
C  **BRSTYP       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRSTYP assigns stock types to individual tree records based on
C  parameters specified with the STOCK keyword.
C  Called from BRSETP and BRTREG.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  20-APR-1999 Lance David
C     --(this is not a problem for LF95 and was removed 18-APR-01)--
C     Added the function INT() to IF statements checking equality just
C     to eliminate the warning messages from Lahey FORTRAN 90 (LF90).
C  21-APR-1999 Lance David
C     Changed some GOTO code to block IF statments. Added some comments
C     and reworked the stock type assignment code because too many 
C     records would be assigned to the highest type included in a mixed
C     stock type situation.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 100) and species temp index variable (I3)
C     are new.
C  17-APR-2001 Lance David (FHTET)
C     Added debug statements.
C     Change stock type proportions to all stock type 1 if sum of 
C     proportions is >1.0.
C     Reworked the stock type assignment method for mixed stock types.
C  24-APR-2001 Lance R. David (FHTET)
C     Added species dimension to PRPSTK array and specie DO loop around
C     stock proportion validation process.
C  01-MAY-2001 Lance R. David (FHTET)
C     Added TBRHST for accumulation of host trees in the stand.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      LOGICAL DEBUG
      INTEGER STOCK, I1, I2, I3, I4
      REAL SSUM, TXP1, TXP2, TXP3, TXP4, TXPS1, TXPS2, TXPS3, TXPS4

C.... Is debug requested?

      CALL DBCHK(DEBUG,'BRSTYP',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT('Entering subroutine BRSTYP: cycle = ', I2)

C.... If no trees, then exit.
      IF (ITRN .EQ. 0) GO TO 120

C.... Planted trees have been assigned an initial value of 5 for
C.... stocking type in BRESTB. This is so that the planted trees
C.... can be identified in this routine and assigned valid stock types
C.... according to the proportions specified by keyword. If planted
C.... trees have been introduced into the model but no stock mix has
C.... been specified, then change all the newly planted trees to type 1
C.... (wild).  Make sure that the sum of stock type proportions equals 1.0
C.... If the sum of the mix is less than 1.0, the difference is added
C.... to the "wild" category.  If the sum is greater than 1.0, the
C.... the proportions revert back to default of all type 1.

      DO 20 I3=1,MAXSP

        IF (BRSPM(I3) .EQ. 0) GO TO 20

C....   Set blister rust species index
        I4 = BRSPM(I3)
        SSUM = 0.0

        DO 16 M=1,4
           SSUM=SSUM+PRPSTK(I4,M)
   16   CONTINUE

        IF(SSUM.GT.1.0) THEN
           PRPSTK(I4,1) = 1.0
           PRPSTK(I4,2) = 0.0
           PRPSTK(I4,3) = 0.0
           PRPSTK(I4,4) = 0.0
        ELSE IF(SSUM.LT.1.0) THEN
           PRPSTK(I4,1)=PRPSTK(I4,1)+(1.0-SSUM)
        ENDIF
        IF(DEBUG) WRITE(JOSTND,*) ' SP=',I4,' PRPSTK 1=',PRPSTK(I4,1),
     &     ' 2=',PRPSTK(I4,2),' 3=',PRPSTK(I4,3),' 4=',PRPSTK(I4,4)

   20 CONTINUE

C.... Process host species.
C.... Start species loop

      DO 100 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 100

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 100
      I2=ISCT(I3,2)

C.... Set blister rust species index
      I4 = BRSPM(I3)

      IF(PRPSTK(I4,1).EQ.1.0 .OR. PRPSTK(I4,2).EQ.1.0 .OR. 
     &   PRPSTK(I4,3).EQ.1.0 .OR. PRPSTK(I4,4).EQ.1.0) THEN

C....    No mixing of stock types is specified.

         IF(PRPSTK(I4,1).EQ.1.0) THEN
            STOCK=1
         ELSE IF(PRPSTK(I4,2).EQ.1.0) THEN
            STOCK=2
         ELSE IF(PRPSTK(I4,3).EQ.1.0) THEN
            STOCK=3
         ELSE
            STOCK=4
         ENDIF

         IF(DEBUG) WRITE(JOSTND,*) ' ALL STOCK SET TO: ',STOCK

C....    Set stock type of planted trees to type specified.

         DO 110 J=I1,I2
            K=IND1(J)
            IF(DEBUG) WRITE(JOSTND,*) ' IDTREE:',IDTREE(K),' IBRTID:',
     &        IBRTID(K),' ISTOTY:',ISTOTY(K)

            IF(ISTOTY(K).EQ.5) THEN
C....          Add this new tree to the total TPA of this host species.
C+++++++++++++ TBRHST(I4) = TBRHST(I4) + PROB(K)
               ISTOTY(K)=STOCK
            ENDIF

  110    CONTINUE
      ELSE

C....    Assign stock types based on specified mix.
C....    All natural trees ('NATURAL' and excess) have already
C....    been assigned stock type 1 and will not be changed.
C....
C....    Loop through index for this host specie and sum the
C....    PROB (number of trees) for all records that need
C....    a stock type assignment.

         TXP=0.0
         DO 30 L=I1,I2
            K=IND1(L)
            IF(ISTOTY(K).EQ.5) THEN
               TXP = TXP + PROB(K)
            ENDIF
   30    CONTINUE

C....    Calculate the target TPA for each stock type.
         TXP1 = PRPSTK(I4,1) * TXP
         TXP2 = PRPSTK(I4,2) * TXP
         TXP3 = PRPSTK(I4,3) * TXP
         TXP4 = PRPSTK(I4,4) * TXP

C....    Initialize summing variables for stock type TPA.
         TXPS1 = 0.0
         TXPS2 = 0.0
         TXPS3 = 0.0
         TXPS4 = 0.0

C....    loop through index to make stock type assignments.
C....    In this simplistic method, the accuracy of the proportional
C....    stock type assignments depends on the TPAs of the individual
C....    tree records and number of tree records in the simulation.
C....    Excess goes to stock type 1, wild stock. Since the sum of
C....    proportions is tested to be exactly 1.0, this should not
C....    be a possibility. But... if all conditions fail, stock is 1.
C....    If random number drawn indicates a stock type for which the
C....    TPA target has already been met, another random number will
C....    be drawn.

         DO 40 L=I1,I2
            K=IND1(L)

            IF(ISTOTY(K).EQ.5) THEN

C....          Add this new tree to the total TPA of this host species.
C+++++++++++++ TBRHST(I4) = TBRHST(I4) + PROB(K)

C....          draw a random number
   35          CALL BRANN(X)

               IF(X.LE.PRPSTK(I4,1)) THEN
                  IF(TXPS1.LT.TXP1) THEN
                     J=1
                     TXPS1 = TXPS1 + PROB(K)
                  ELSE
                     J=0
                  ENDIF
               ELSE IF(X.LE.(PRPSTK(I4,1)+PRPSTK(I4,2))) THEN
                  IF(TXPS2.LT.TXP2) THEN
                     J=2
                     TXPS2 = TXPS2 + PROB(K)
                  ELSE
                     J=0
                  ENDIF
               ELSE IF(X.LE.(PRPSTK(I4,1)+PRPSTK(I4,2)+
     &                 PRPSTK(I4,3))) THEN
                  IF(TXPS3.LT.TXP3) THEN
                     J=3
                     TXPS3 = TXPS3 + PROB(K)
                  ELSE
                     J=0
                  ENDIF
               ELSE IF(X.LE.(PRPSTK(I4,1)+PRPSTK(I4,2)+PRPSTK(I4,3)
     &                       +PRPSTK(I4,4))) THEN
                  IF(TXPS4.LT.TXP4) THEN
                     J=4
                     TXPS4 = TXPS4 + PROB(K)
                  ELSE
                     J=0
                  ENDIF
               ELSE
                  J=1
               ENDIF

               IF(J.EQ.0) THEN
C....             Need to draw another random number because stock type
C....             indicated already has TPA target met.
                  IF(DEBUG) WRITE(JOSTND,*) ' RANDOM NUMBER REDRAW'
                  GOTO 35
               ELSE
                  IF(DEBUG) WRITE(JOSTND,*) ' IDTREE:',IDTREE(K),
     &              ' IBRTID:',IBRTID(K),' ISTOTY:',ISTOTY(K),
     &              ' RESET TO ',J,' X=',X
                  ISTOTY(K)=J
               ENDIF
            ENDIF
   40    CONTINUE
      ENDIF

C.... End of species loop
  100 CONTINUE

C.... Common return.

  120 CONTINUE
      IF(DEBUG) WRITE (JOSTND, 300) ICYC
  300 FORMAT('Leaving subroutine  BRSTYP: cycle = ', I2)
      RETURN
      END
