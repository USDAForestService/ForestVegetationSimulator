      SUBROUTINE BRSETP
      IMPLICIT NONE
C**********************************************************************
C  **BRSETP       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  BRSETP initializes the Blister Rust Model tree-specific variables
C  after FVS tree data and blister rust canker data has been read.
C  It is called from MAIN.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 110) and species temp index variable (I3)
C     are new.
C  02-APR-2001 Lance R. David (FHTET)
C     Deactivated initialization of stock type (ISTOTY) because it is 
C     now a canker input file variable.
C  02-MAY-2001 Lance R. David (FHTET)
C     Added species dimension to BRNTRECS variable.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C**********************************************************************

C.... Common include files

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

C.... Local variable declarations

      INTEGER I1, I2, I3, I4, J, K
      REAL    BRDBH, BRHT
      LOGICAL DEBUG,BRGO

C.... See if we need to do some debug

      CALL DBCHK (DEBUG,'BRSETP',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,111) ICYC
  111 FORMAT('Entering subroutine BRSETP: cycle = ',I2)

C.... Call BRATV to see if the Blister Rust model is being used in this
C.... simulation. Return if not active or there are no trees.

      CALL BRATV(BRGO)
      IF(.NOT. BRGO .OR. ITRN .EQ. 0) GO TO 115

C.... Process host pines. If none then return.
C.... Start species loop

      DO 110 I3 = 1, MAXSP
      IF (BRSPM(I3) .EQ. 0) GO TO 110
      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 110
      I2=ISCT(I3,2)

C.... Set blister rust species index
      I4=BRSPM(I3)
      BRNTRECS(I4)=I2-I1+1

      DO 100 J=I1,I2
         K=IND1(J)

C....    Change height to meters, DBH to centimeters, calculate ground
C....    level diameter, and calculate height to base of crown in cm.
C....    If tree age not recorded, then assign stand age as tree age.
C....
C....    Assign stock type of existing inventory trees to 5 so they
C....    will be treated as planted trees during the assignment of
C....    stock types in subroutine BRSTYP.
C....
C....    Stock type is now a variable in the canker list input file.
C....    but still must be initialized here for the host trees that
C....    do not have a corresponding record in the canker file.
C....    (LRD 04/02/01)

         BRHT=HT(K)*0.3048
         BRDBH=DBH(K)*2.54
         BRGD(K)=(100*BRHT*BRDBH)/(100*(BRHT-1.14))
         IF(BRGD(K).LT.BRDBH) BRGD(K)=BRDBH
         IF(BRAGE(K).EQ.0.0) BRAGE(K)=IAGE
         IF(ISTOTY(K).EQ.0) ISTOTY(K)=5
         BRHTBC(K)=(BRHT-(BRHT*(FLOAT(ICR(K))/100.0)))*100.0

C....    Initialize canker and infection conditions for each tree
C....    based on canker counts read from canker data if requested
C....    on the CANKDATA keyword.

         IF(CKINIT) CALL BRCINI(K,BRHT)
  100 CONTINUE

C.... End of species loop
  110 CONTINUE

C.... Assign stock types to trees.
C.... Any trees that were not assigned a stock type via the canker
C.... input file will be rectified.

      CALL BRSTYP

C.... Assign status to cankers.

      CALL BRCSTA

C.... Calculate statistics.

      CALL BRSTAT

C.... Reduce crowns of trees that have been girdled by cankers.

      CALL BRCRED

C.... Calculate STAND RUST INDEX using basal area if specified by
C.... the rust index assignment method.

      IF(RIMETH.NE.0) CALL BRIBA

C.... Calculate STAND TARGET, individual tree GROWTH INDEX,
C.... individual tree RUST INDEX, and STAND DEVIATION FACTOR.

      CALL BRTARG

C.... Common return.

  115 CONTINUE
      IF(DEBUG) WRITE(JOSTND,120) ICYC
  120 FORMAT('Leaving subroutine BRSETP: cycle = ',I2)
      RETURN
      END
