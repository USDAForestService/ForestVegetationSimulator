      SUBROUTINE BMLITE (ISTD,IYR)
C----------
C WWPB $Id$
C----------
C     CALLED FROM: BMDRV  
C     CALLS:  BMRANN
C             SPLAAR
C             GPGET2
***********************************************************************      
* **BMLITE    Date of last modification:  June 8, 1994
*
* This is a landscape level model that takes the density of the lightning
* strike and, by comparison with a random number, determines if a strike
* will occur. More than one strike can occur if the stand is large or if
* the strike density is high. If a strike occurs, a cummulative distribution 
* is found based on the relative heights of trees in each size class.
* If a random number indicates a class, one tree in that size class is struck.
* Currently, there is no provision in the model for the fact that not all strikes
* are on trees, or for the fact that only 1/100 strikes (approx) make an
* attractor tree. 
*
* Lightning is always active unless the user sets the strike density to 0
*      
* Definitions:
*     DENS:   from BMCOM; Strike density (strikes/acre)                
*     STRIKE: from BMCOM; Proportion of trees in class struck
*
***********************************************************************      
C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.

      PARAMETER(MAXPRM=7)

      LOGICAL LOK
      INTEGER ISIZ, NUMSTR
      INTEGER DUM(1)
      REAL    TOTHT, TOTSTR
      REAL    PRMS(MAXPRM)
      REAL    REMAIN

      SAVE
      
C.... Check for debug.

      IF(LBMDEB) WRITE(JBMBPR,15) IYR, ISTD
   15 FORMAT(' Begin BMLITE: Year = ',I5,' Stand = ',I6)

      TOTHT = 0.0
      DO 10 ISIZ= 1,NSCL
         STRIKE(ISTD,ISIZ) = 0.0
   10 CONTINUE

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.   
   
      IF (ICNT .GE. BMEND) ICNT = 0      
      ICNT = ICNT + 1

      IF (ICNT .EQ. 1) THEN

        IYR1= IYR
        NPRMS= 2

C     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
C     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
C     SET OF PARAMETERS.

        CALL GPGET2 (306, IYR1, MAXPRM, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN

          DENS= PRMS(2)

          IF (LBMDEB) WRITE (JBMBPR,71) MICYC, IYR1, IYR2, PRMS(2)
   71     FORMAT (/' BMLITE: MICYC=', I5, 'IYR1=',I5,'
     >      IYR2=',I5, 'Strikes/acre=', F7.4)

          IYR2= IYR1 + IFIX(PRMS(1)) - 1
          IF (IYR2.GE.MIY(MICYC)) THEN
            PRMS(1) = IYR2 - MIY(MICYC) + 1
            CALL GPADD (KODE, MIY(MICYC), 306, NPRMS, PRMS(1), 1, DUM) 
            IYR2 = MIY(MICYC) - 1
            IF (LBMDEB) WRITE (JBMBPR,81) PRMS(1),IYR2
   81     FORMAT (/' IN BMLITE:  Lightning has been SCHEDULED FOR THE ',
     >        'NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,
     >        '  NEW ILTYR2=',I5)
          ENDIF
        ENDIF

C     IF THE CURRENT PARAMETERS HAVE FINISHED, RETURN
C     TO THE DEFAULT VALUES.

c        IF (IYR .EQ. IYR2+1) THEN
c          DENS= 0. 
c        ENDIF

      ENDIF

C     IF THE STRIKE DENSITY = 0 THEN LIGHTNING DOES NOT OCCUR.
      IF (DENS .LE. 0.0) RETURN

C     Choose a random number and find the area of the stand     

      CALL BMRANN(X)        
      CALL SPLAAR(ISTD,SAREA,IRC)
            
c     Calculate the total number of strikes that may occur in the stand (TOTSTR)
c     Then find out how many will always occur (integer portion of TOTSTR), and
c     use the remainder of TOTSTR to give the probability of another strike occuring.

      TOTSTR = DENS * SAREA
      NUMSTR = INT(TOTSTR)
      REMAIN = TOTSTR - NUMSTR
      
      IF (X .LE. REMAIN) NUMSTR = NUMSTR + 1
      IF (NUMSTR .EQ. 0) GOTO 40
      
c     Calculate the sum of the average ht in each size class to use for making
c     a cummulative distribution.
      DO 20 ISIZ= 1,NSCL
         TOTHT = TOTHT + HTS(ISTD,ISIZ,1)
   20 CONTINUE
      IF (TOTHT .LE. 1E-3) GOTO 40
      
      DO 35 I = 1,NUMSTR                          
        CALL BMRANN(X)        
                                        
c       scale up the random number to use for picking a size class (so that X
c       won't be biased towards smaller dbh classes.

        IF (I .EQ. NUMSTR .AND. REMAIN .GT. 0.0) X = X / REMAIN 
        PSTRIK = 0.0
      
        DO 30 ISIZ = 1, NSCL

            PSTRIK = PSTRIK + HTS(ISTD,ISIZ,1) / TOTHT
            IF (X .LE. PSTRIK .AND. TREE(ISTD,ISIZ,1) .GT. 1E-6) THEN 
c              one tree in the stand in this size class is hit
c                  (strike is a proportion)
               PROPN = (1.0 / SAREA) / TREE(ISTD,ISIZ,1)     
               IF (PROPN .GT. 1.0) PROPN = 1.0              
               IF (STRIKE(ISTD,ISIZ) .LT. 1.0) THEN
                 STRIKE(ISTD,ISIZ) = STRIKE(ISTD,ISIZ) + PROPN
                 GOTO 35
               ENDIF
            END IF
   30   CONTINUE
   35 CONTINUE   

   40 CONTINUE

      IF(LBMDEB) WRITE(JBMBPR,90) IYR, ISTD
   90 FORMAT(' End BMLITE: Year = ',I5,' Stand = ',I6)

      RETURN
      END
