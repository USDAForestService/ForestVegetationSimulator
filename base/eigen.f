      SUBROUTINE EIGEN(A,R,N,MV)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     FROM: SYSTEM/360 SCIENTIFIC SUBROUTINE PACKAGE VERSION III
C     PROGRAMMERS MANUAL. IBM 1966.
C
C     PARTS RECODED TO COMPLY TO REMOVED FORTRAN FEATURES BY
C     NCROOKSTON JAN 2020
C
C     ..................................................................
C
C        SUBROUTINE EIGEN
C
C        PURPOSE
C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
C           MATRIX
C
C        USAGE
C           CALL EIGEN(A,R,N,MV)
C
C        DESCRIPTION OF PARAMETERS
C           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
C               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
C               MATRIX A IN DESCENDING ORDER.
C           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
C               IN SAME SEQUENCE AS EIGENVALUES)
C           N - ORDER OF MATRICES A AND R
C           MV- INPUT CODE
C                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
C                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
C                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
C                       SEQUENCE)
C
C        REMARKS
C           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
C           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
C           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
C           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
C           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
C
C     ..................................................................
C
      INTEGER MV,N,IQ,J,I,IJ,IA,IND,L,M,MQ,LQ,LM,LL,MM,ILQ,IMQ,IM,IL
      INTEGER ILR,IMR,JQ,K
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
      DOUBLE PRECISION A(*),R(*),ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,
     1                 COSX2,SINCS,RANGE,DN,ONE,TWO
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
C        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
C        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
C        BE CHANGED TO 1.0D-12.
C
C
C        ...............................................................
      DATA ONE/1.0D00/,TWO/2.0D00/
C
C        GENERATE IDENTITY MATRIX
C
C   5
      RANGE=1.0D-12
C   6
      DN=FLOAT(N)
      IF (MV-1 .EQ. 0) GOTO 25
      IQ=-N
      DO J=1,N
        IQ=IQ+N
        DO I=1,N
          IJ=IQ+I
          R(IJ)=0.0
          IF (I-J .EQ. 0) R(IJ)=ONE
        ENDDO
      ENDDO
C
C        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
C
   25 CONTINUE
      ANORM=0.0
      DO I=1,N
        DO J=I,N
          IF (I-J .EQ. 0) CYCLE
          IA=I+(J*J-J)/2
          ANORM=ANORM+A(IA)*A(IA)
        ENDDO
      ENDDO
      IF(ANORM.LE.0) GOTO 165
      ANORM=DSQRT(ANORM*TWO)
      ANRMX=ANORM*RANGE/DN
C
C        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
C
      IND=0
      THR=ANORM
   45 THR=THR/DN
   50 L=1
   55 M=L+1
C
C        COMPUTE SIN AND COS
C
   60 MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
C  62
      IF(DABS(A(LM))-THR .LT. 0) GOTO 130
      IND=1
      LL=L+LQ
      MM=M+MQ
      X=(A(LL)-A(MM))/TWO
C  68 
      Y=-A(LM)/ DSQRT(A(LM)*A(LM)+X*X)
      IF(X.LE.0.) Y=-Y
      SINX=Y/ DSQRT(TWO*(ONE+( DSQRT(ONE-Y*Y))))
      SINX2=SINX*SINX
C  78
      COSX= DSQRT(ONE-SINX2)
      COSX2=COSX*COSX
      SINCS =SINX*COSX
C
C        ROTATE L AND M COLUMNS
C
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO I=1,N
        IQ=(I*I-I)/2
        IF(I-L .EQ. 0 .OR. I-M .EQ. 0) GOTO 115
        IF(I-M .GT. 0) GOTO 90
        IM=I+MQ
        GO TO 95
   90   IM=M+IQ
   95   IF(I-L .GE. 0) GOTO 105
        IL=I+LQ
        GO TO 110
  105   IL=L+IQ
  110   X=A(IL)*COSX-A(IM)*SINX
        A(IM)=A(IL)*SINX+A(IM)*COSX
        A(IL)=X
  115   IF(MV-1 .EQ. 0) CYCLE
        ILR=ILQ+I
        IMR=IMQ+I
        X=R(ILR)*COSX-R(IMR)*SINX
        R(IMR)=R(ILR)*SINX+R(IMR)*COSX
        R(ILR)=X
      ENDDO
      X=TWO*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
C
C        TESTS FOR COMPLETION
C
C        TEST FOR M = LAST COLUMN
C
  130 IF(M-N .EQ. 0) GOTO 140
      M=M+1
      GO TO 60
C
C        TEST FOR L = SECOND FROM LAST COLUMN
C
  140 IF(L-(N-1) .EQ. 0) GOTO 150
      L=L+1
      GO TO 55
  150 IF(IND-1 .NE. 0) GOTO 160
      IND=0
      GO TO 50
C
C        COMPARE THRESHOLD WITH FINAL NORM
C
  160 IF(THR-ANRMX .GT. 0.) GOTO 45
C
C        SORT EIGENVALUES AND EIGENVECTORS
C
  165 CONTINUE
      IQ=-N
      DO I=1,N
      IQ=IQ+N
      LL=I+(I*I-I)/2
      JQ=N*(I-2)
      DO J=I,N
        JQ=JQ+N
        MM=J+(J*J-J)/2
        IF(A(LL)-A(MM) .GE. 0) CYCLE
        X=A(LL)
        A(LL)=A(MM)
        A(MM)=X
        IF(MV-1 .EQ. 0) CYCLE
          DO K=1,N
            ILR=IQ+K
            IMR=JQ+K
            X=R(ILR)
            R(ILR)=R(IMR)
            R(IMR)=X
          ENDDO
        ENDDO
      ENDDO
      RETURN     
      END
