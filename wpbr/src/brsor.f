      SUBROUTINE BRSOR
      IMPLICIT NONE
C**********************************************************************
C  **BRSOR        DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C   This subroutine is called to sort and match tree IDs to insure
C   proper mapping of Blister Rust Model canker data and FVS Model
C   tree data.  This subroutine is called by TRESOR which is called
C   from the the PROCESS KEYWORD section in INITRE.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  09-APR-2001 Lance R. David
C     Update for stock type that can now be supplied through canker file.
C  09-MAY-2006 Lance R. David
C     Added exit if blister rust not active.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      INTEGER IDTR, IID, STK, IC1, IC2, IERR, IPOS, J, K, L, N
      REAL    AGE, DO1, DU1, GI1
      LOGICAL DEBUG

C.... Exit if Blister Rust not active
      IF(.NOT. BRYES) GOTO 300

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRSOR',5,ICYC)
      IF(DEBUG) WRITE(JOSTND,11) ICYC
   11 FORMAT('Entering subroutine BRSOR: cycle = ',I2)

C.... Sort the FVS list of tree IDs.

      CALL I4PSRT(IREC1,IDTREE,IND1,.TRUE.)

C.... Initializations.

      IERR=0
      K=1

C.... Top of BRUST tree loop. If the tree count is greater than the
C.... number of unique BRUST tree IDs, then skip out.

  100 CONTINUE
      IF(K.GT.INCAN) GO TO 200
      IDTR=IBRTID(K)

C.... Invalid BRUST tree ID.

      IF(IDTR.LE.0) THEN
         K=K+1
         GO TO 100
      ENDIF

C.... Call binary search routine to find the position of the BRUST
C.... tree ID that matches the FVS tree ID.

      CALL I4PBSR(IREC1,IDTREE,IND1,IDTR,IPOS)

C.... If tree ID cannot be found in the canker file that matches the
C.... tree ID in the tree data file, then delete the canker info.

      IF(IPOS.LE.0) THEN
         IERR=IERR+1
         DO 123 J=1,10
            DUP(J,K)=0.0
            DOUT(J,K)=0.0
            GIRDL(J,K)=0.0
  123    CONTINUE
         ILCAN(K)=0
         ITCAN(K)=0
         IBRTID(K)=0
         BRAGE(K)=0.0
         ISTOTY(K)=0
         K=K+1
         GO TO 100
      ENDIF

C.... If cankers are already in their proper position based on the
C.... input data (a chance occurrance) then go on to next canker.

      IF(IPOS.EQ.K) THEN
         K=K+1
         GO TO 100
      ENDIF

C.... Swap canker data.

      N=MAX(ILCAN(K),ILCAN(IPOS))
      DO 150 L=1,N

C....    Store contents of identified position in temporary variables.

         DU1=DUP(L,IPOS)
         DO1=DOUT(L,IPOS)
         GI1=GIRDL(L,IPOS)

C....    Load BRUST values into FVS position.

         DUP(L,IPOS)=DUP(L,K)
         DOUT(L,IPOS)=DOUT(L,K)
         GIRDL(L,IPOS)=GIRDL(L,K)

C....    Load temporary variables into position vacated by
C....    "Load BRUST values . . "

         DUP(L,K)=DU1
         DOUT(L,K)=DO1
         GIRDL(L,K)=GI1
  150 CONTINUE

C.... Free up indicated position by saving the values.

      IC1=ILCAN(IPOS)
      IC2=ITCAN(IPOS)
      IID=IBRTID(IPOS)
      AGE=BRAGE(IPOS)
      STK=ISTOTY(IPOS)

C.... Load appropriate values into indicated position.

      ILCAN(IPOS)=ILCAN(K)
      ITCAN(IPOS)=ITCAN(K)
      IBRTID(IPOS)=IDTR
      BRAGE(IPOS)=BRAGE(K)
      ISTOTY(IPOS)=ISTOTY(K)

C.... Store values that used to be in position IPOS

      ILCAN(K)=IC1
      ITCAN(K)=IC2
      IBRTID(K)=IID
      BRAGE(K)=AGE
      ISTOTY(K)=STK

C.... Process next tree ID.

      GO TO 100

C.... Common return.

  200 CONTINUE

C.... If there are IDs that have no match then print error message.

      IF(IERR.GT.0) WRITE (JOSTND,250) IERR
  250 FORMAT(/,'**** WARNING ****', I5,' Canker IDs have been found',
     >      ' that have no match in the tree ID list.  These cankers'
     >      ' will be ignored.')

      IF(DEBUG) WRITE(JOSTND,255) ICYC
  255 FORMAT('Leaving subroutine BRSOR: cycle = ',I2)
  300 CONTINUE
      RETURN
      END
