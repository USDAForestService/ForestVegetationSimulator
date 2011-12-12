      SUBROUTINE BRCRED
C**********************************************************************
C  **BRCRED       DATE OF LAST REVISION:  05/09/2001
C----------------------------------------------------------------------
C  Purpose:
C  BRCRED reduces the crowns of white pines that have been girdled
C  by Blister Rust at initialization from canker data provided or
C  during the current growth cycle.
C  Reduction in crowns during the initialization phase of FVS tree data
C  is accomplished in CROWN.
C  This subroutine is called by:
C     BRSETP
C     BRTREG
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  11-MAY-1999 Lance David
C     Added debug for calculation of new crown ratio.
C  13-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 95) and species temp index variable (I3)
C     are new.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      INTEGER I1, I2, I3
      LOGICAL DEBUG,BRGO

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCRED',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,111) ICYC
  111 FORMAT(' Entering subroutine BRCRED: cycle = ',I2)

C.... If the Blister Rust model is not active, then return.

      CALL BRATV(BRGO)
      IF(.NOT.BRGO) GO TO 100

C.... Process host species. If no trees, return.

      IF (ITRN .EQ. 0) GO TO 100

C.... Start species loop

      DO 95 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 95

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 95
      I2=ISCT(I3,2)
      DO 90 K=I1,I2
         I=IND1(K)
         ICCR=ICR(I)

C....    If tree is dead, go to the next one.

         IF(IBRSTAT(I).EQ.7) GO TO 90

C....    If tree is not girdled, go to the next one.

         IF(ITRUNC(I).EQ.0.OR.ICRED(I).EQ.0) GO TO 90

C....    Reduce crowns of trees flagged as top-killed.
C....    NORMHT and ITRUNC are integer representing 1/100s feet.
C....    Min and Max crown ratio is set to 5 and 95 percent.
C....    HN is normal height (feet)
C....    HD is length of top that is dead (feet)

         HN=NORMHT(I)/100.0
         HD=HN-(ITRUNC(I)/100.0)
         CL=(FLOAT(ICCR)/100.)*HN-HD
         IICI=IFIX((CL*100./HN)+.5)
         IF(IICI.LT.5) IICI=5
         IF(IICI.GT.95) IICI=95
         ICR(I) = IICI
         ICRED(I)=0

C....    Write debug information.
         IF(DEBUG) THEN
            WRITE (JOSTND,85) I,IDTREE(I),NORMHT(I),ITRUNC(I),CL,
     &                        ICCR,ICR(I)
   85       FORMAT (' I=',I4,' IDTREE=',I5,' NORMHT=',I5,' ITRUNC=',I5,
     &              ' CL=',F5.1,' OLDCR=',I3,' NEWCR=',I3)
         ENDIF
   90 CONTINUE

C.... End species loop
   95 CONTINUE

C.... Common return.

  100 CONTINUE
      IF(DEBUG) WRITE(JOSTND,114) ICYC
  114 FORMAT(' Leaving subroutine BRCRED: cycle = ',I2)
      RETURN
      END
