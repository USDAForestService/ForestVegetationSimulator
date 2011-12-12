      FUNCTION BWESLP(XX,X,Y,N)
      IMPLICIT NONE
C----------
C  **BWESLP                 DATE OF LAST REVISION:  07/14/10
C----------
C
C     A LINEAR-INTERPOLATION FUNCTION
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
C     INT-MOSCOW FORESTRY SC. LAB. AUG 1980
C
C     DESCRIPTION :
C
C     SUPPLIED WITH THE BWMOD AS WRITTEN AT THE MODELING
C     WORKSHOP.  THE NAME IS CHANGED FROM 'SLP' TO 'BWESLP'.
C     THE FUNCTION RETURNS THE VALUE OF A LINEAR SEGMENTED
C     FUNCTION DEFINED BY THE PAIRS X(I), Y(I) EVALUATED AT
C     X=XX FOR X INCLUDED IN THE INTERVAL ?X(1),X(N)!
C     IF XX < X(1), F(XX)=X(1).  IF XX > X(N), XX=X(N).
C
C     PARAMETERS :
C
C     XX - POINT AT WHICH FUNCTION IS TO BE EVALUATED.
C     X  - ARRAY OF X(I), SEGMENT ENDPOINTS FOR X VARIABLE.
C     Y  - ARRAY OF Y(I), SEGMENT ENDPOINTS FOR Y VARIABLE.
C     N  - SIZE OF ARRAYS X AND Y
C
C Revision History:
C   05-MAY-00 Lance David (FHTET)
C      .The last occurence of SLP had been translated to BWSLP instead
C       of BWESLP. Fixed it.
C      .Added weather and outbreak random number seeds WSEEDR and OBSEER.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C----------
      INTEGER I, N, NN
      REAL    XX, X(N), Y(N), BWESLP

C     WRITE (16,*) 'FUNCTION BWESLP: XX=',XX,' N=',N               ! TEMP DEBUG
C     WRITE (16,*) '                  X=',X                        ! TEMP DEBUG
C     WRITE (16,*) '                  Y=',Y                        ! TEMP DEBUG

      NN=N-1
      DO 30 I=1,NN
      IF(XX.LT.X(I).OR.XX.GT.X(I+1)) GO TO 30
      BWESLP=Y(I)+((Y(I+1)-Y(I))/(X(I+1)-X(I)))*(XX-X(I))
      GOTO 9000
 30   CONTINUE
      BWESLP=Y(N)
      IF (XX.LT.X(1)) BWESLP=Y(1)

 9000 CONTINUE

C     WRITE (16,*) 'FUNCTION BWESLP=',BWESLP                       ! TEMP DEBUG

      RETURN
      END
