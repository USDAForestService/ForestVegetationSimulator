      SUBROUTINE BMPHER (IYR)

C     CALLED FROM:  BMDRV      
C     ENTRY POINTS: BMAPH
C     CALLS:        SPLAAR
***********************************************************************
*     **BMPHER     Date of last revision:  June 21, 1994
*
* This subroutine simulates application of attracting or repelling 
* pheromones. Attracting pheromone makes "special" trees in the stand.
* Repelling pheromone reduces the amount of host basal area that the 
* beetles can sense. 
*      'Keyword: APHERO yrstart, %efficiency     (attracting)
*      '         RPHERO yrstart, %efficiency     (repelling)
*
* Definitions:   
*     PROPEFF: Proportion of trees which are sprayed with repelling
*              pheromone
*     REPPHE:  Multiplier reducing the attractiveness of host BA due to
*              the repelling pheromone (i.e. if 30% of trees are not
*              sprayed then only 30% of BA is sensed as attractive).
***********************************************************************
      
C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'BMCOM.F77' 
      
      PARAMETER(MAXPRM=7)

C.... Variable declarations.

      LOGICAL LOK
      REAL    INFEST
      INTEGER SCNT
      INTEGER IRC
      INTEGER ISIZ
      INTEGER MYLST(MXSTND)
      REAL    PRMS(MAXPRM)
      REAL    PROPEFF
      REAL    TOTREE       
      
C     Initializations

      IYR1 = 0
      
      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMPHERO: Year= ',I5, 'Stand= ', I6)
      
      DO 15 ISTD = 1, BMSTND
         IF (STOCK(ISTD)) REPPHE(ISTD) = 1.0
   15 CONTINUE

      NPRMS = 1
      IYR1 = IYR
      CALL GPGET2 (302,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)      
      IF (LOK) THEN 
c         convert from a percent to a proportion
        PROPEFF = PRMS(1) / 100 
        IF (PROPEFF .LE. 0.0) RETURN
        
        DO 33 I = 1, SCNT

          ISTD = MYLST(I)
          IF (.NOT.STOCK(ISTD) .OR. ISTD .LE. 0) GOTO 33

          INFEST = 0.0       
          TOTREE = 0.0
          DO 20 ISIZ = 1,NSCL
            INFEST = INFEST + PBKILL(ISTD,ISIZ)
            TOTREE = TOTREE + TREE(ISTD,ISIZ,1)            
   20     CONTINUE
           
C         Repelling pheromone
C         only works in large MPB stands that are less than 10% infested
C         (Note that TOTREE does not include PBKilled trees)
      
          CALL SPLAAR(ISTD,SAREA,IRC)

          IF (TOTREE .GT. 0.0) THEN
            IF ((PBSPEC .EQ. 1) .AND. (SAREA .GT. 2.5) .AND. 
     &                     (INFEST / (INFEST + TOTREE) .LE. 0.1)) THEN

C             Reduce host BA attractiveness by prop trees sprayed

              REPPHE(ISTD) = (1.0 - PROPEFF)       
            ELSE 
              IF (LBMDEB) WRITE(JBMBPR,50)
   50         FORMAT('  IN BMPHERO: Conditions not right for',
     >               ' repelling pheromone.')
              GO TO 80          
            END IF
          END IF

   80     CONTINUE  

   33   CONTINUE
      ENDIF
      
      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMPHERO: Year= ',I5, 'Stand= ', I6)
      
      RETURN

C     Entry for Attracting pheromone

      ENTRY BMAPH (IYR)

C     Zero the array.
      
      DO 100 ISTD = 1, BMSTND
        IF (STOCK(ISTD)) ATRPHE(ISTD) = 0.0      
  100 CONTINUE

C     See if the option needs to be done. If it does, copy the
C     *proportion* (converted from %) to stand locations in the
C     ATRPHE array

      IYR1 = 0
      NPRMS = 1
      IYR1 = IYR
      CALL GPGET2 (303,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)      
      IF (LOK) THEN 
        X = PRMS(1) * 0.01
        DO 200 I = 1, SCNT
          ISTD = MYLST(I)
          IF (STOCK(ISTD) .AND. ISTD .GT. 0) ATRPHE(ISTD) = X
  200   CONTINUE
      ENDIF
      
      IF(LBMDEB) WRITE(JBMBPR,900) IYR 
  900 FORMAT('Did BMAPHER: Year= ',I5)

      RETURN
      END
