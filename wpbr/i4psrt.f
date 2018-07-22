      SUBROUTINE I4PSRT(N,A,INDXBR,LSEQ)
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C  Purpose:
C  I4PSRT is an INTEGER*4 ascending identification sort.
C
C  The vector INDXBR is initially loaded with values from 1 to N
C  inclusive.  IAPSRT rearranges the elements of INDXBR so that
C  INDXBR(1) is the subscript of the smallest element in the vector A,
C  INDXBR(2) is the subscript of the second smallest element in A,...,
C  and INDXBR(N) is the subscript of the largest element in A.  The
C  physical arrangement of the vector A is not altered.  This
C  algorithm is an adaptation of the technique described in:
C
C     Scowen, R.A. 1965. Algorithm 271; Quickersort. Comm ACM.
C     8(11) 669-670.
C----------------------------------------------------------------------
C  Parameters:
C  N     - The index value for the FVS and blist rust tree-specific arrays
C          (current tree/record sequence).
C  A     - Array of tree IDs (tree numbers, FVS array IDTREE).
C  INDXBR - Array of index values for FVS and blister rust arrays
C  LSEQ  - True if sorting is to be performed.
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  10-MAY-1999 Lance R. David
C     Added comments for parameters.
C     Changed array named "INDEX" (which is a function name and should not
C     be used as a variable) to "INDXBR".
C     Changed parameter arrays to inherit dimension "(*)".
C     Added debug code.
C
C**********************************************************************

C.... Local variable declarations.

      LOGICAL LSEQ,DEBUG
      INTEGER T,N
      INTEGER*4 A(*)
      INTEGER INDXBR(*),IPUSH(33)
      INTEGER I, ICYC, IL, INDIL, INDIP, INDIU, INDKL, INDKU, IP,
     &        ITOP, IU, JL, JU, KL, KU

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'I4PSRT',6,ICYC)
      IF(DEBUG) THEN
         WRITE(16,*)' ENTER I4PSRT:  N=',N,' LSEQ=',LSEQ
         WRITE(16,*)'    BEFORE SORTING'
         DO 8 I=1,N
            WRITE(16,*)'           INDXBR=',INDXBR(I),'    A=',A(I)
    8    CONTINUE
      ENDIF

C.... If LSEQ is FALSE, assume that A is partially sorted.
C.... Otherwise, load INDXBR with values from 1 to N.

      IF(LSEQ) THEN
         DO 10 I=1,N
            INDXBR(I)=I
   10    CONTINUE
      ENDIF

C.... Return if fewer than two elements in array A.

      IF(N.LT.2) GO TO 200

C.... Sort.

      ITOP=0
      IL=1
      IU=N

   30 CONTINUE
      IF(IU.LE.IL) GO TO 40
      INDIL=INDXBR(IL)
      INDIU=INDXBR(IU)
      IF(IU.GT.IL+1) GO TO 50
      IF(A(INDIL).LE.A(INDIU)) GO TO 40
      INDXBR(IL)=INDIU
      INDXBR(IU)=INDIL

   40 CONTINUE
      IF(ITOP.EQ.0) GO TO 200
      IL=IPUSH(ITOP-1)
      IU=IPUSH(ITOP)
      ITOP=ITOP-2
      GO TO 30

   50 CONTINUE
      IP=(IL+IU)/2
      INDIP=INDXBR(IP)
      T=A(INDIP)
      INDXBR(IP)=INDIL
      KL=IL
      KU=IU

   60 CONTINUE
      KL=KL+1
      IF(KL.GT.KU) GO TO 90
      INDKL=INDXBR(KL)
      IF(A(INDKL).LE.T) GO TO 60

   70 CONTINUE
      INDKU=INDXBR(KU)
      IF(KU.LT.KL) GO TO 100
      IF(A(INDKU).LT.T) GO TO 80
      KU=KU-1
      GO TO 70

   80 CONTINUE
      INDXBR(KL)=INDKU
      INDXBR(KU)=INDKL
      KU=KU-1
      GO TO 60

   90 CONTINUE
      INDKU=INDXBR(KU)

  100 CONTINUE
      INDXBR(IL)=INDKU
      INDXBR(KU)=INDIP
      IF(KU.LE.IP) GO TO 110
      JL=IL
      JU=KU-1
      IL=KU+1
      GO TO 120

  110 CONTINUE
      JL=KU+1
      JU=IU
      IU=KU-1

  120 CONTINUE
      ITOP=ITOP+2
      IPUSH(ITOP-1)=JL
      IPUSH(ITOP)=JU
      GO TO 30

C.... Common return.

  200 CONTINUE

      IF(DEBUG) THEN
         WRITE(16,*)'    AFTER  SORTING'
         DO 208 I=1,N
            WRITE(16,*)'           INDXBR=',INDXBR(I),'    A=',A(I)
  208    CONTINUE
         WRITE(16,*) ' EXIT I4PSRT'
      ENDIF
      RETURN
      END
