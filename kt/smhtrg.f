      SUBROUTINE SMHTRG(ISPFL,DEBUG,HTKEEP)
      IMPLICIT NONE
C----------
C KT $Id$
C----------
C  THIS SUBROUTINE IS CALLED FROM CRATET.  IF ALL OF THE TREES IN A
C  STAND ARE LESS THAN 3 INCHES DBH, THEN THIS SUBROUTINE COMPUTES
C  NEW COEFFICIENTS FOR THE SMALL-TREE LINEAR HEIGHT DUBBING
C  EQUATION.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
COMMONS
C
C----------
C  INTERNAL VARIABLES:
C
C----------
      INTEGER ISPFL(MAXSP),I,ISPC,I1,I2,K1,K2,I3,NH,JJ
      REAL HTKEEP(MAXTRE),SMSMYY,SMSMXX,SMSMXY,SMSMX2,H,D
      REAL SMYY,SMXX,SMXY,SMX2,XN,DEN,BB,AA
      LOGICAL DEBUG
C----------
C  CHECK TO SEE IF STAND IS COMPRISED OF TREES 3 INCHES DBH AND LESS
C----------
      DO 50 I=1,ITRN
      IF (DBH(I) .GE. 3.0) THEN
         RETURN
      ENDIF
   50 CONTINUE
      DO 100 I=1,MAXSP
      ISPFL(I)=1
  100 CONTINUE
C----------
C  ENTER LOOP TO ADJUST SMALL-TREE HEIGHT-DBH MODEL FOR LOCAL
C  CONDITIONS.
C----------
      DO 500 ISPC=1,MAXSP
      I1 = ISCT(ISPC,1)
      IF(I1.LE.0) GO TO 500
      I2=ISCT(ISPC,2)
C----------
C  INITIALIZE SUMS FOR THIS SPECIES
C----------
      K1=0
      K2=0
      SMSMYY=0.0
      SMSMXX=0.0
      SMSMXY=0.0
      SMSMX2=0.0
C----------
C  ENTER TREE LOOP WITHIN SPECIES
C----------
      DO 200 I3=I1,I2
      I=IND1(I3)
      H=HT(I)
      NH=NORMHT(I)
      D=DBH(I)
C----------
C  BYPASS TREES WITH MISSING HEIGHTS
C----------
      IF(H .GT. 0.0) THEN
        K1=K1+1
        SMYY = H
        SMXY = D*H
        SMX2 = D*D
        SMXX = D
        SMSMYY = SMSMYY + SMYY
        SMSMXX = SMSMXX + SMXX
        SMSMXY = SMSMXY + SMXY
        SMSMX2 = SMSMX2 + SMX2
      ENDIF
      IF(H .GT. 0.0 .AND. NH .EQ. 0.0) GO TO 200
      K2 = K2 + 1
      IND2(K2)=I
C----------
C  END OF SUMMATION LOOP FOR THIS SPECIES
C----------
  200 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' SMSMYY=',SMSMYY,' SMSMXX=',SMSMXX,
     &       ' SMSMXY=',SMSMXY,' SMSMX2=',SMSMX2,' K2=',K2
C----------
C  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE
C  THEN DUB HEIGHTS USING DEFAULS COEFFICIENTS FOR THIS SPECIES
C----------
        IF (K1 .LT. 3 .OR. .NOT. LHTDRG(ISPC)) THEN
           ISPFL(ISPC)=0
           GO TO 300
        ENDIF
        XN = FLOAT(K1)
        DEN=XN*SMSMX2-SMSMXX*SMSMXX
        IF(DEN .EQ. 0.0) THEN
          ISPFL(ISPC)=0
          GO TO 300
        ENDIF
        BB = (XN*SMSMXY-SMSMXX*SMSMYY)/DEN
        AA = (SMSMX2*SMSMYY-SMSMXX*SMSMXY)
     &        /DEN
        IF(BB .LE. 1.0) ISPFL(ISPC)=0
  300   CONTINUE
        IF(DEBUG)WRITE(JOSTND,*)' XN=',XN,' BB=',BB,' AA=',AA,' ISPC=',
     &         ISPC,' DEN=',DEN,' ISPFL=',ISPFL(ISPC)
        DO 400 JJ=1,K2
        I=IND2(JJ)
        H=HT(I)
        IF(DEBUG)WRITE(JOSTND,*)' H=',H,' BB=',BB,' DEN=',
     &       DEN,' I=',I,' ISPC=',ISPC
        IF(ISPFL(ISPC) .EQ. 0) GO TO 400
        IF(H .GT. 0.0) GO TO 400
        D=DBH(I)
        H=AA+BB*D
        IF(DEBUG)WRITE(JOSTND,*)' I=',I,' D=',D,' H=',H
C----------
C  CHECKS TO ENSURE THAT NEW HEIGHT ISN'T WAY OUT OF LINE
C----------
        IF(H.LT.1.0 .OR. H.GT.20.0*D)THEN
         ISPFL(ISPC)=0
         GO TO 400
        ELSEIF(D .GT. .1 .AND. H .LT. 4.5)THEN
         ISPFL(ISPC)=0
         GO TO 400
        ELSE
         HTKEEP(I)=H
        ENDIF
  400   CONTINUE
  500   CONTINUE
       RETURN
       END
