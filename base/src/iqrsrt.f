      SUBROUTINE IQRSRT(LIST,N)
      IMPLICIT NONE
C----------
C  **IQRSRT DATE OF LAST REVISION:  07/23/08
C----------
C
C     THIS ASCENDING SORT IS AN FORTRAN ADAPTATION OF THE SORT
C     DESCRIBED IN:
C
C       SCOWEN, R.A. 1965. ALGORITHM 271; QUICKERSORT. COMM ACM.
C                    8(11) 669-670.
C
C     (THIS IS THE SAME TECHNIQUE USED IN RDPSRT)
C     (THE CODE WAS SUPPLIED BY WSUCSC, PULLMAN WA.)
C
      INTEGER T,TT
      INTEGER LIST(N)
      INTEGER IU(33),IL(33)
      INTEGER N,M,I,J,K,IJ,L
      M=1
      I=1
      J=N
5     IF (I.GE.J) GOTO 70
10    K=I
      IJ=(I+J)/2
      T=LIST(IJ)
      IF (LIST(I).LE.T) GOTO 20
      LIST(IJ)=LIST(I)
      LIST(I)=T
      T=LIST(IJ)
20    L=J
      IF (LIST(J).GE.T) GOTO 40
      LIST(IJ)=LIST(J)
      LIST(J)=T
      T=LIST(IJ)
      IF (LIST(I).LE.T) GOTO 40
      LIST(IJ)=LIST(I)
      LIST(I)=T
      T=LIST(IJ)
      GOTO 40
30    LIST(L)=LIST(K)
      LIST(K)=TT
40    L=L-1
      IF (LIST(L).GT.T) GOTO 40
      TT=LIST(L)
50    K=K+1
      IF (LIST(K).LT.T) GOTO 50
      IF (K.LE.L) GOTO 30
      IF (L-I.LE.J-K) GOTO 60
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GOTO 80
60    IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GOTO 80
70    M=M-1
      IF (M.LE.0) RETURN
      I=IL(M)
      J=IU(M)
80    IF (J-I.GE.11) GOTO 10
      IF (I.EQ.1) GOTO 5
      I=I-1
90    I=I+1
      IF (I.EQ.J) GOTO 70
      T=LIST(I+1)
      IF (LIST(I).LE.T) GOTO 90
      K=I
100   LIST(K+1)=LIST(K)
      K=K-1
      IF (T.LT.LIST(K)) GOTO 100
      LIST(K+1)=T
      GOTO 90
      END
