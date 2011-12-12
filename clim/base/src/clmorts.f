      SUBROUTINE CLMORTS
      IMPLICIT NONE
C----------
C  **CLMORTS CLIMATE--DATE OF LAST REVISION:  03/25/2010
C----------
C
C     CLIMATE EXTENSION - COMPUTES CLIMATE-CAUSED MORTALITY
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CLIMATE.F77'
C
COMMONS
C
      INTEGER I,I1,I2
      REAL THISYR,ALGSLP,X,XV,FYRMORT(MAXSP)
      LOGICAL DEBUG

      CALL DBCHK (DEBUG,'CLMORTS',7,ICYC)

      IF (DEBUG) WRITE (JOSTND,1) LCLIMATE
    1 FORMAT (' IN CLMORTS, LCLIMATE=',L2)

      IF (.NOT.LCLIMATE) RETURN
      
      SPMORT=0.
      FYRMORT=0.
      THISYR=FLOAT(IY(ICYC))+(FINT/2)
      DO I=1,MAXSP 

        IF (INDXSPECIES(I).EQ.0) CYCLE         
        I2 = INDXSPECIES(I) 
        
C       X IS THE SPECIES VIABILITY SCORE.
        
        XV = ALGSLP (THISYR,FLOAT(YEARS),ATTRS(1,I2),NYEARS)  
        X = XV

C       CONVERT X TO A 10-YR SURVIVAL RATE.

        IF (X.LT. 0.2) THEN
          X=0.
        ELSE IF (X.GT. 0.5) THEN
          X=1.
        ELSE 
          X=-.66666667 + X*3.3333333
          IF (X.GT.1.0) X=1.0  ! ROUNDING ERROR?
        ENDIF

C       CONVERT TO A MORTALITY RATE AND APPLY MULTIPLIER.
C       SAVE THIS VERSION FOR REPORTING (10 YR).
        
        SPMORT(I)=(1.-X)*CLMORTMULT(I)
        
C       CONVERT TO A FINT-YR SURVIVAL RATE.

        IF (X.GT. 1E-5) THEN  ! VERY LOW SURVIVAL SET TO 0
          X=EXP(LOG(X)/10.)**FINT
          IF (X.LT.0) THEN
            X=0.
          ELSE IF (X.GT.1.0) THEN
            X=1.0
          ENDIF
        ELSE
          X=0.
        ENDIF
         
C       CONVERT TO A MORTALITY RATE AND APPLY MULTIPLIER.

        FYRMORT(I)=(1.-X)*CLMORTMULT(I)

        IF (DEBUG) WRITE (JOSTND,10) IFIX(THISYR),I,JSP(I)(1:2),XV,X,
     >                    CLMORTMULT(I),SPMORT(I),FYRMORT(I)
   10   FORMAT (' IN CLMORTS, THISYR=',I5,' I=',I3,1X,A3,' XV=',
     >          2F10.4,' CLMORTMULT=',F10.4,' MORT=',2F10.4)
      ENDDO
      DO I=1,ITRN 
        ! X IS THE FVS PERIODIC MORTALITY RATE (FINT-YEARS)...              
        IF (PROB(I)-WK2(I) .LE. 1E-10) THEN
          X=1.
        ELSE IF (PROB(I).LT. 1E-10) THEN
          X=1.
        ELSE
          X = WK2(I)/PROB(I)
        ENDIF
        IF (FYRMORT(ISP(I)).GT.X) WK2(I)=PROB(I)*FYRMORT(ISP(I))
      ENDDO      
      
      RETURN
      END
