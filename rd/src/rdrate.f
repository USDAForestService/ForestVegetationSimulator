      SUBROUTINE RDRATE (NSIM)
C----------
C  **RDRATE      LAST REVISION:  08/06/01
C----------
C
C  THIS SUBROUTINE CALCULATES SPREAD RATE TO APPLY TO EACH CENTER
C  USING THE RANGE OF VALUES CALCULATED BY THE SPREAD RATE MODEL.
C  CENTERS THAT HAD A ZERO SPREAD RATE LAST YEAR ARE MORE LIKELY TO
C  HAVE A ZERO SPREAD RATE THIS YEAR UNLESS THOSE CENTERS ARE SHRINKING
C  CENTERS.
C
C  CALLED BY :
C     RDSPRD  [ROOT DISEASE]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C
C  DEFINITIONS:
C     RRA:    CALCULATED SPREAD RATE FOR EACH CENTER 
C     NSIM:   NUMBER OF SIMULATIONS THAT WERE DONE BY THE MONTE CARLO SPREAD MODEL
C     MCRATE: THE RESULT OF EACH OF THE NSIM SIMULATIONS
C     GCENTS: THE NUMBER OF NON-SHRINKING CENTERS (POTENTIALLY GROWING CENTERS)
C
C  Revision History:
C    25-APR-00 Lance R. David (FHTET)
C       Reduced RETURN statements to 1 at the end of routine.
C       Added Debug code.
C    06-AUG-01 Lance R. David (FHTET)
C       Initialization of entire arrays RRA and IPNT instead of just the number
C       of elements represented equal to the current number of centers. That
C       method left calculations using uninitialized elements when the number of
C       disease centers exceeded the number of Monte Carlo simulations.
C....................................................................

C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'
C
      LOGICAL DEBUG

      REAL    RRA(100)                          
      INTEGER GCENTS
      INTEGER IPNT(100)   
      LOGICAL LZERO
C
C     SEE IF WE NEED TO DO SOME DEBUG.
C
      CALL DBCHK (DEBUG,'RDRATE',6,ICYC)

      IF (DEBUG) WRITE (JOSTND,*)
     &   'BEGIN RDRATE:  IRRSP=',IRRSP,' NCENTS=',NCENTS(IRRSP)

         
C     BEGIN PROGRAM

C     INITIALIZE VARIABLES
      DO 50 I=1,100
        RRA(I) = 0.0
        IPNT(I) = I
   50 CONTINUE
      
      GCENTS = NCENTS(IRRSP) - NSCEN(IRRSP)

      IF (DEBUG) WRITE (JOSTND,*)
     &  'IN RDRATE: GCENTS=',GCENTS,' NSIM=',NSIM 

      IF (GCENTS .LE. 0) THEN
         RRRATE(IRRSP) = 0.0
         GOTO 900                ! RETURN
      ENDIF
      
      LZERO = .FALSE.
      REM = 0.0
      K = 1
      IF (GCENTS .LE. NSIM) THEN

C       THERE ARE FEWER CENTERS THAN THERE WERE SIMULATIONS SO TAKE THE
C       AVERAGE OF VARIOUS SIMULATIONS AS A SPREAD RATE TO APPLY
      
        DIV = FLOAT(NSIM) / (FLOAT(GCENTS) + 1E-9)
        
        DO 150 I= 1, GCENTS         
          NUM = INT(DIV + REM)
          REM = DIV + REM - NUM 
          
          DO 100 J= 1, NUM
            RRA(I) = RRA(I) + MCRATE(IRRSP,K)
            K = K + 1         
            IF (K .GT. NSIM) NUM = J
  100     CONTINUE        
                      
            RRA(I) = RRA(I) / (FLOAT(NUM) + 1E-9)
            IF ((.NOT. LZERO) .AND. (RRA(I) .EQ. 0.0)) LZERO = .TRUE.
  150   CONTINUE
      ELSE

C       THERE ARE MORE CENTERS THAN THERE WERE SIMULATIONS SO APPLY EACH 
C       SIMULATED VALUE TO ONE OR MORE CENTERS.
        
        I = 1
        DIV = FLOAT(GCENTS) / FLOAT(NSIM)
  200   CONTINUE

        NUM = INT(DIV + REM)
        REM = DIV + REM - NUM 
          
        DO 250 J= 1, NUM
          RRA(I) = MCRATE(IRRSP,K)
          I = I + 1         
          IF ((.NOT. LZERO) .AND. (RRA(I) .EQ. 0.0)) LZERO = .TRUE.
  250   CONTINUE
          
        K = MIN0(NSIM,K + 1)
        IF (I .LE. GCENTS) GOTO 200
      
      ENDIF   

C     NOW SORT RRA ARRAY ENOUGH SO THAT THE ZEROS COME TO THE TOP (OTHER
C     VALUES MUST REMAIN UNSORTED)

      IF (LZERO) THEN
        DO 350 I= 1,GCENTS
          IF (RRA(I) .EQ. 0.0) THEN
            DO 300 J= I,2, -1
              IF (RRA(J-1) .GT. 0.0) THEN
                TEMP = RRA(J)
                RRA(J) = RRA(J-1)
                RRA(J-1) = TEMP
              ELSE
                GOTO 350
              ENDIF
  300       CONTINUE
          ENDIF
  350   CONTINUE
            
C     SET UP POINTERS TO LAST YEAR'S SPREAD RATES TO GET ZEROS AT THE TOP
C     AND SHRINKING CENTERS AT THE BOTTOM. IGNORE ZEROS FROM RECENTLY CREATED 
C     SPORE CENTERS
C     (IPNT IS INITIALIZED ABOVE TO BE IPNT(I) = I) 

        DO 450 I= 1,NCENTS(IRRSP)
           IF (SHCENT(IRRSP,I,2) .GT. 0.0) THEN
              DO 425 J= I,NCENTS(IRRSP)-1 
                 IPNT(J) = IPNT(J+1)
  425         CONTINUE
              IPNT(NCENTS(IRRSP)) = I
           ELSEIF ((RRATES(IRRSP,I) .EQ. 0.0) .AND. 
     &                    (ICENSP(IRRSP,I) .EQ. 0)) THEN
             DO 400 J= I,2, -1
               IPNT(J) = IPNT(J-1)
  400        CONTINUE
             IPNT(1) = I
           ENDIF
  450   CONTINUE       
      ENDIF

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDRATE: MCRATE=',MCRATE
      IF (DEBUG) WRITE (JOSTND,*) 'IN RDRATE: RRA=',RRA

C     ASSIGN SPREAD RATES TO CENTERS BASED ON THE POINTER ARRAY 

      TRR = 0.0
      DO 500 I=1,GCENTS
        RRATES(IRRSP,IPNT(I)) = RRA(I)   
        TRR = TRR + RRA(I)
  500 CONTINUE  

      IF (DEBUG) WRITE (JOSTND,*)
     &  'IN RDRATE: TRR=',TRR,' GCENTS=',GCENTS

      RRRATE(IRRSP) = TRR / (FLOAT(GCENTS) + 1E-9)

  900 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDRATE: RRRATE=',RRRATE(IRRSP)
      RETURN
      END
