      SUBROUTINE BRTARG
      IMPLICIT NONE
C**********************************************************************
C  **BRTARG       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  BRTARG calculates STAND TARGET, individual tree GROWTH INDEX,
C  individual tree RUST INDEX, and STAND DEVIATION FACTOR and a
C  RUST INDEX ADJUSTMENT FACTOR.  Called during initialization.
C
C  * * * Special note regarding 15-MAR-2001 update * * *
C  Given the current method in which a new deviation factor is
C  calculated, only species is isolated and the new DFACT value is
C  calculated using all stock types for the species and assigned to
C  stock type 1 (wild stock) of that species, stock types 2-4 remain
C  unchanged.
C  
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  20-APR-1999 Lance David
C     Added debug statement to code that calculates the stand deviation
C     factor (variable DFACT).
C     The Rust Index Adjustment Factor (RIAF) calculation did not allow
C     for the possibility of no cankers resulting in a adjustment factor
C     of zero which caused the individual tree RI to be set to zero.
C     Added check that set RIAF to 1.0 if calculated RIAF is 0.0.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 90) and species temp index variable (I3)
C     are new.
C  14-DEC-2000 Lance David (FHTET)
C     Added code to skip calculation of RIAF when RIMETH = 0 and 
C     initial cankers are present at initialization.
C  15-MAR-2001 Lance R. David (FHTET)
C     Stand Deviation Factor variable (DFACT) updated to array by
C     species and stock type. Given the method that a new deviation 
C     factor is calculated at this time, only species is isolated and
C     the new DFACT value is assigned to stock type 1 (wild stock), 
C     stock types 2-4 remain unchanged.
C  01-MAY-2001 Lance R. David (FHTET)
C     Change stand level/averages variables to arrays for handling of
C     multiple host species treated independently.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

C.... Local variable declarations.

      INTEGER I1, I2, I3, I4, IIAG, ICLS, L, LL, M, MM
      REAL    ACTRI, ARI, ARISUM, DFD, DFN, GIBR, GISUM,
     &        HHT, RISUM, TBSUM
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRTARG',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,11) ICYC
   11 FORMAT('Entering subroutine BRTARG: cycle = ',I2)

C.... If no tree records, then return.

      IF(ITRN.EQ.0) GO TO 100

C.... Process hosts trees in the treelist. If none, then return.

C.... Start species loop

      DO 90 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 90
C.... Set blister rust species index
      I4=BRSPM(I3)

C.... Initializations.

      RISUM=0.0
      GISUM=0.0
      ARISUM=0.0

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 90
      I2=ISCT(I3,2)
      DO 30 L=I1,I2
         M=IND1(L)

C....    skip trees with no age.
         IF (BRAGE(M) .EQ. 0) GO TO 30
         IIAG=IFIX(BRAGE(M))
         HHT=HT(M)*0.3048

C....    Call BRGI to calculate growth index and sum target.

         CALL BRGI(IIAG,HHT,GIBR,TBSUM)
         GI(M)=GIBR
         GISUM=GISUM+GI(M)
         TSTARG(M)=TBSUM
         STSUM(I4)=STSUM(I4)+TBSUM

C....    Calculate an actual rust index at initialization based on
C....    # cankers/target area for each tree.  This value will be
C....    used as an adjustment factor to the model rust index (RIDEF).

         IF(TSTARG(M).GT.0.0) THEN
            ARI=ITCAN(M)/TSTARG(M)
         ELSE
            ARI=0.0
         ENDIF
         ARISUM=ARISUM+ARI
  30  CONTINUE

C.... Calculate the average actual rust index (cankers/target area)
C.... at initialization and the rust index adjustment factor.
C.... If there were no canker provided at initialization, this value
C.... can not be calculated and will be set to 1.0 (no adjustment).
C.... Do not calculate the adjustment factor when the user specifies
C.... a rust index value to use, even if there are initial cankers.

      IF (RIMETH .EQ. 0 .AND. ICYC .EQ. 0) THEN
         CONTINUE
      ELSE
         ACTRI=ARISUM/FLOAT(BRNTRECS(I4))
         RIAF(I4)=ACTRI/RIDEF
         IF(RIAF(I4) .EQ. 0.0) RIAF(I4)=1.0
      ENDIF

C.... Loop through the trees again to apply the rust index adjustment
C.... factor.

      DO 35 LL=I1,I2
         MM=IND1(LL)

C....    skip trees with no age.
C+++++++ IF (BRAGE(MM) .EQ. 0) GO TO 35

C....    Calculate rust index. Modify default value by resistance factor
C....    for the stock type of this tree and multiply by the rust index
C....    adjustment factor.

         ICLS=ISTOTY(MM)
         RI(MM)=RIDEF*RESIST(I4,ICLS)*RIAF(I4)
         RISUM=RISUM+RI(MM)
   35 CONTINUE

C.... Calculate average rust index, growth index, and sum target.

      AVGRI(I4)=RISUM/FLOAT(BRNTRECS(I4))
      AVGGI(I4)=GISUM/FLOAT(BRNTRECS(I4))
      AVGSTS(I4)=STSUM(I4)/FLOAT(BRNTRECS(I4))

C.... Calculate stand deviation factor.
C.... If a stand deviation factor was supplied by keyword, that
C.... value will be used.
C.... If stand contains no cankers or if stand age < 10 years,
C.... the default stand deviation factor will be used.
C.... Otherwise, a stand deviation factor will be calculated.
C.... Stand deviation factor cannot be less than 0.01.

      IF(LDFACT) THEN

C....    The deviation factor was supplied by keyword. Use it.

         IF(DEBUG) THEN
            WRITE(JOSTND,40)
   40       FORMAT('BRTARG: deviation factor supplied by keyword.')
         ENDIF

      ELSE IF(INCAN.GT.0.OR.IAGE.GE.10) THEN

C....    Calculate stand deviation factor.
C....    If a new deviation factor can not be calculated due to a zero
C....    denominator, DFACT will remain unchanged.
C....    Variable PITCA has a 0.999 upper limit imposed in subroutine BRSTAT
C....    so this log function will not error.

         DFN=AVTCPT(I4)-(-ALOG(1.0-PITCA(I4)))
         DFD=AVTCPT(I4)*(-ALOG(1.0-PITCA(I4)))

         IF (DFD.GT.0.0) THEN
           DFACT(I4,1)=DFN/DFD
         ENDIF

         IF(DEBUG) THEN
            WRITE(JOSTND,50) DFN, DFD, DFACT(I4,1), I4
   50       FORMAT('BRTARG: deviation factor calculated. DFN=',F12.6,
     &             ' DFD=',F12.6,' DFACT=',F12.6,' SP=',I4)
         ENDIF
      ENDIF

C.... Minimum stand deviation factor allowed regardless of being provided
C.... by keyword, or calculation.

      IF(DFACT(I4,1).LT.0.01) THEN
         DFACT(I4,1)=0.01
         IF(DEBUG) WRITE(JOSTND,*) ' RESET MIN DFACT=', DFACT(I4,1)
      ENDIF

C.... End species loop
   90 CONTINUE

C.... Common return.

  100 CONTINUE
      IF(DEBUG) WRITE(JOSTND,101) ICYC
  101 FORMAT('Leaving subroutine BRTARG: cycle = ',I2)
      RETURN
      END
