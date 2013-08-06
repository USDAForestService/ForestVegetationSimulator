      SUBROUTINE BRCDEL(IVACT,INDXBR,J)
C**********************************************************************
C  **BRCDEL       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  This routine deletes cankers by moving them from the bottom of the
C  list to fill in empty canker record locations at the top of the list.
C  Vacant records were labelled in the calling routine by setting the
C  sign of those spots in the index negative.  The value of IVACT is
C  passed in as the number of negative members of the index.
C  This routine is called by BRCREM.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  10-MAY-1999 Lance R. David
C     Changed variable INDEX (which is a function name) to INDXBR.
C  08-NOV-2002 Lance R. David (FHTET)
C     Removed subtraction of deleted cankers from the total canker count.
C  09-MAY-2006 Lance R. David (FHTET)
C     Change INDXBR to inherit dimension. Initilization was missing (I)
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      LOGICAL DEBUG
      INTEGER INDXBR(*)

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCDEL',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,9) ICYC
   9  FORMAT('Entering subroutine BRCDEL: cycle = ',I2)

C.... Sort the index list such that vacancy pointers are at the top
C.... of the list.  The absolute values of these pointers point to
C.... the vacancies in descending order.

      CALL IQRSRT(INDXBR,ILCAN(J))

C.... Initialize the pointers to the indices of vacancies and
C.... trees.  IVACT points to the end of the vacancy pointers.

      IV=IVACT+1
      IR=ILCAN(J)+1

C.... Top of the tree/vacancy loop.

   10 CONTINUE

C.... Decrement the index to the vacancy pointers.
C.... If there are no more vacancies, skip out.

      IV=IV-1
      IF(IV.LT.1) GO TO 20

C.... Decrement the index to tree pointers.
C.... If there are no more trees, skip out.

      IR=IR-1
      IF(IR.LE.IVACT) GO TO 20

C.... Load the pointers to the vacancy and the tree records.
C.... If the vacancy pointer is greater than the tree pointer, skip out.

      IVAC=-INDXBR(IV)
      IREC=INDXBR(IR)
      IF(IVAC.GT.IREC) GO TO 20

C.... Move the data from position IREC to position IVAC.

      DUP(IVAC,J)=DUP(IREC,J)
      DOUT(IVAC,J)=DOUT(IREC,J)
      GIRDL(IVAC,J)=GIRDL(IREC,J)
      ISTCAN(IVAC,J)=ISTCAN(IREC,J)

C.... Go on to the next tree/vacancy.

      GO TO 10

C.... Update the lethal and total canker counts.

   20 CONTINUE
      ILCAN(J)=ILCAN(J)-IVACT

C.... Total canker count represents a cummulative value, so deleted
C.... cankers should not be subtracted per Geral Mcdonald 08-NOV-2002
C.... Lance David
C.... ITCAN(J)=ITCAN(J)-IVACT

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,22) ICYC
   22 FORMAT('Leaving subroutine BRCDEL: cycle = ',I2)
      RETURN
      END
