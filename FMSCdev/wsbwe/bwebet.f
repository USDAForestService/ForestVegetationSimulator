      SUBROUTINE BWEBET (A0,B0,X,N,KODE)
      IMPLICIT NONE
C----------
C WSBWE $Id$
C----------
C
C     A0   = FIRST PARAMETER OF THE BETA DISTRIBUTION: A>0.
C     B0   = SECOND PARAMETER: B>0.
C     X    = A VECTOR OF BETA RANDOM VARIATES.
C     N    = THE LENGTH OF X AND THE NUMBER OF RANDOM VARIATES REQUESTED.
C     KODE = -1 FOR MIN(A,B)<0, OTHERWISE, KODE IS EQUAL TO THE NUMBER
C            OF PASSES TO FIND ALL OF THE MEMBERS OF X.
C
C     RETURNS N BETA RANDOM NUMBERS.
C
C     REFERENCE: R.C.H. CHENG, 1978 ACM 21(4):317-322.
C
C     N.L. CROOKSTON, FORESTRY SCIENCES LABORATORY, MOSCOW, ID, 4/1989.
C     PART OF THE SPRUCE BUDWORM MODELING SYSTEM.
C
C     TESTING THIS GENERATOR INDICATES A BIAS...A DIFFERENT GENERATOR
C     NEEDS TO BE FOUND.
C
C Revision History:
C  14-JUL-2010 Lance R. David (FMSC)
C     Added IMPLICIT NONE and declared variables as needed.
C
C----------
      INTEGER I, KODE, N
      REAL A0, B0, X(N)
      REAL A, ALF, B, BET, C1, C2, DEL, GAM, R, S, T, U1, U2, V,
     &     W, Y, Z

      IF (A0.LE.0.00001 .OR. B0.LE.0.00001) THEN
         KODE=-1
         GOTO 70
      ELSE
         KODE=0
         I=0
      ENDIF
C
C     USE TWO DIFFERENT METHODS, DEPENDING ON THE VALUES OF A0 AND B0.
C
      IF (AMIN1(A0,B0).GT.1.0) THEN
         A=AMIN1(A0,B0)
         B=AMAX1(A0,B0)
         ALF=A+B
         BET=SQRT((ALF-2.)/(2.*A*B-ALF))
         GAM=A+(1./B)
   10    CONTINUE
         KODE=KODE+1
         CALL BWERAN(U1)
C        write (16,*) 'in bwebet: damage random number: ',u1    ! TEMP DEBUG
         CALL BWERAN(U2)
C        write (16,*) 'in bwebet: damage random number: ',u2    ! TEMP DEBUG
         V=BET*ALOG(U1/(1.-U1))
C
C        THE FOLLOWING LIMIT ON V INSERTED BY N.CROOKSTON
C
         IF (V.GT.20.0) V=20.0
         W=A*EXP(V)
         Z=U1*U1*U2
         R=GAM*V-1.3862944
         S=A+R-W
         IF (S+2.609438 .GE. 5.*Z) GOTO 15
         T=ALOG(Z)
         IF (S.GE.T) GOTO 15
         IF (R+ALF*ALOG(ALF/(B+W)) .LT. T) GOTO 10
   15    CONTINUE
         I=I+1
         IF (A.EQ.A0) THEN
            X(I)=W/(B+W)
         ELSE
            X(I)=B/(B+W)
         ENDIF
         IF (I.EQ.N) GOTO 70
         GOTO 10
C
      ELSE
C
         A=AMAX1(A0,B0)
         B=AMIN1(A0,B0)
         ALF=A+B
         BET=1./B
         DEL=1.+A-B
         C1=DEL*(0.0138889+0.0416667*B)/(A*BET-0.777778)
         C2=0.25+(0.5+0.25/DEL)*B
   20    CONTINUE
         KODE=KODE+1
         CALL BWERAN(U1)
C        write (16,*) 'in bwebet: damage random number: ',u1     ! TEMP DEBUG
         CALL BWERAN(U2)
C        write (16,*) 'in bwebet: damage random number: ',u2     !TEMP DEBUG
         IF (U1.GE. 0.5) GOTO 30
         Y=U1*U2
         Z=U1*Y
         IF (0.25*U2+Z-Y .GE. C1) THEN
            GOTO 20
         ELSE
            GOTO 50
         ENDIF
   30    CONTINUE
         Z=U1*U1*U2
         IF (Z.LE. 0.25) THEN
            V=BET*ALOG(U1/(1.-U1))
C
C           THE FOLLOWING LIMIT ON V INSERTED BY N.CROOKSTON
C
            IF (V.GT.20.0) V=20.0
            W=A*EXP(V)
            GOTO 60
         ENDIF
         IF (Z.GE.C2) GOTO 20
   50    CONTINUE
         V=BET*ALOG(U1/(1.-U1))
C
C        THE FOLLOWING LIMIT ON V INSERTED BY N.CROOKSTON
C
         IF (V.GT.20.0) V=20.0
         W=A*EXP(V)
         IF (ALF*(ALOG(ALF/(B+W))+V)-1.3862944 .LT. ALOG(Z)) GOTO 20
   60    CONTINUE
         I=I+1
         IF (A.EQ.A0) THEN
            X(I)=W/(B+W)
         ELSE
            X(I)=B/(B+W)
         ENDIF
         IF (I.EQ.N) GOTO 70
         GOTO 20
      ENDIF
   70 CONTINUE
C     WRITE (16,*) 'IN BWEBET: KODE=',KODE,' X=',X(1),' U1,U2=',U1,U2
      RETURN
      END
