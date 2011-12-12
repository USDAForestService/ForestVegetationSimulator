      SUBROUTINE I4PBSR (N,A,IORD,F,IP)
C**********************************************************************
C  **I4PBSR       DATE OF LAST REVISION:  01/01/1981
C----------------------------------------------------------------------
C  Purpose:
C  This is an INTEGER*4 keyed binary search.
C  Part of the Stand Prognisis Model (Forest Vegetation Simulator)
C  adapted from INTEGER search - NL Crookston - Jan. 1981 - Moscow, ID
C
C  Binary search routine:  finds the subscript location of F in
C  array A, if IORD is filled with the ascending order indices of
C  array A, rather than the descending order indices.  Array A need
C  not be in ascending order as long as the keys in IORD are in
C  ascending order over A.
C----------------------------------------------------------------------
C
C  Local variable definitions:
C    N    = the length of A.
C    A    = a list of INTEGER*4 values
C    IORD = an array of keys sorted in ascengin order over A.
C    F    = an INTEGER*4 value which you wish to find in A.
C    IP   = the position in A where F was found; or 0 when F is not
C           a member of A.
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Local variable declarations.

      INTEGER*4 A,F
      INTEGER IORD,ITOP,IBOT,IMID,IP
      DIMENSION A(1),IORD(1)

C.... Initializations.

      IMID=1
      I1=IORD(1)

C.... If the value we search for is less than the smallest value in the
C.... array, then skip out.

      IF(F.LE.A(I1)) GO TO 40
      IMID=N
      IN=IORD(N)

C.... If the value we search for is greater tha  the largest value in
C.... the array, then skip out.

      IF(F.GE.A(IN)) GO TO 40

C.... Initialize the top and bottom of the search partition to the top
C.... and bottom of the array.

      ITOP=1
      IBOT=N

C.... Test the middle value of the partition.

   20 CONTINUE
      IMID=(IBOT+ITOP)/2
      IM=IORD(IMID)

C.... If the middle value is greater than or equal to the search value,
C.... set the top of the partition to the middle value plus one.

      IF(F.GT.A(IM)) GO TO 30

C.... If the middle value is less than what we search for, set the
C.... bottom of the partition to the middle value minus one.

      IBOT=IMID-1
      IB=IORD(IBOT)

C.... If the value of the bottom of the partition is greater than or
C.... equal to the search value, then skip out.

      IF(F.GT.A(IB)) GO TO 40

C.... Re-search the partition.

      GO TO 20

C.... Set the top of the partition to the middle value plus one.

   30 CONTINUE
      ITOP=IMID+1
      IT=IORD(ITOP)

C.... If the value of the top of the partition is less than or equal
C.... to the search value, the skip out.

      IF(F.LT.A(IT)) GO TO 40

C.... Re-search the partition.

      GO TO 20

C.... This is the exit point for the routine.  If we got here, we
C.... either found the search value as the middle of a partition,
C.... or partitioned to the point where the search value could
C.... not be contained in the partition.  At this point, we don't know
C.... which so we make a final check, and return zero if not found.

   40 CONTINUE
      IP=IORD(IMID)
      IF(F.NE.A(IP)) IP=0
      RETURN
      END
