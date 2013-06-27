      SUBROUTINE TMOTPR (PROTBK)    
      IMPLICIT NONE
C---------- 
C  **TMOTPR DATE OF LAST REVISION:  04/01/13 
C---------- 
C     
C     PROTBK= THE CONDITIONAL PROBABILITY THAT THIS STAND WILL    
C             SUSTAIN A TUSSOCK MOTH OUTBREAK GIVEN THAT AN OUTBREAK    
C             IS OCCURING IN THE GENERAL VICINITY.    
C     N. L. CROOKSTON, PROGRAMMER. 5/1978 MODIFIED 3/79 INT -- MOSCOW   
C     
C
C Revision History:
C   01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C
C----------
C
COMMONS     
C     
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'TMCOM1.F77'
C     
COMMONS     
C     
      INTEGER I, I1, I2, II
     &        
      REAL AVCRDI, HOST,
     &     PGFBA,  PROTBK, STDCLO, TMB0, TMB1,
     &     TMR0

      IF (IPRBMT .GT. 1) GOTO 50    
C     
C     THIS EQUATION IS FROM R. C. HELLER, UNIVERSITY OF IDAHO,    
C     COLLEGE OF FORESTRY WILDLIFE AND RANGE SCIENCES, MOSCOW 83843     
C     
C     VARIABLES:  
C     
C     ELEV  = ELEVATION IN HUNDREDS OF FEET. EQUATIONS CALL 
C             FOR FEET:  ELEV*100.0 
C     SLOPE = SCALED SLOPE (0-1). EQUATIONS CALL FOR PERCENT
C             SLOPE:  SLOPE*100.0   
C     ASPECT= SCALED ASPECT(0-8). EQUATIONS CALL FOR AZIMUTH
C             FROM NORTH:  THE UNITS USED HERE RADIANS
C     TOPO  = TOPOGRAPHIC POSITION. 1=RIDGETOP,2=SIDEHILL,3=BOTTOM.     
C             INPUT BY KEYWORD, DEFAULTED IN TMINIT.  
C     STDCLO= PERCENTAGE OF CROWN CLOSURE AS VIEWED ON AERIAL     
C             PHOTOGRAPHS;  ESTIMATED AS A FUNCTION OF CCF. 
C     HOST  = PERCENTAGE OF CROWN AREA COMPOSED OF HOST.    
C             ESTIMATED AS A FUNCTION OF CCF PER SPECIES AND CCF. 
C     AVCRDI= AVERAGE CROWN DIAMETER. ESTIMATED USING CCF AND TREES     
C             PER ACRE POST CUTTING.
C     PGFBA = PROPORTION OF BASAL AREA IN GRAND FIR.  
C     THASHD= SOIL ASH DEPTH. DEFAULTED IN TMINIT.    
C     
C     CALCULATE MODEL PARAMETERS    
C     

      IF (ISCT(IDFCOD,1) .EQ. 0) THEN
         PROTBK = 0.0
         GOTO 110
      ENDIF

      STDCLO = 100.0    
      IF (RELDEN .LT. 150.0) STDCLO = (RELDEN / 150.0) * 100.0    
      HOST = ((RELDSP(3) + RELDSP(4)) / RELDEN) * 100.0     
      AVCRDI = 2.0 * SQRT((435.6 * (RELDEN / TPROB)) / 3.141593)  

C     
C     CALCULATE THE CONDITIONAL PROBABILITY     
C     
      PROTBK = 1.0 / (1.0 + EXP(-(-0.852926 -   
     >         (0.00023762 * ELEV  * 100.0) -   
     >         (0.00052207 * SLOPE * 100.0) +   
     >         (0.00348125 * SLOPE * 100.0 * COS(ASPECT)) + 
     >         (0.00733782 * SLOPE * 100.0 * SIN(ASPECT)) - 
     >         (0.37562400 * TOPO) +
     >         (0.02049930 * STDCLO) +    
     >         (0.01866930 * HOST) +
     >         (0.01685520 * AVCRDI))))   

      IF (TMDEBU) THEN
         WRITE (JODFTM,20) ELEV, SLOPE, ASPECT, TOPO, HOST, AVCRDI,
     >         TPROB, RELDEN, RELDSP(3), RELDSP(4)
   20    FORMAT (//,' SUBROUTINE TMOTPR:  ELEV     SLOPE     ASPECT  ',
     >         '  TOPO      HOST      AVCRDI    TPROB     RELDEN   ',
     >         'RELDSP(3) RELDSP(4)',/,T17,10F10.4)
      ENDIF
      GOTO 110

   50 CONTINUE    
C     
C     THESE EQUATIONS ARE FROM PETER MIKA AND JIM MOORE, UN OF IDAHO,   
C     COLLEGE OF FORESTRY WILDLIFE AND RANGE SCIENCES, MOSCOW 83843     
C     
C     CALCULATE GRAND FIR BASAL AREA.
C
      PGFBA = 0.0
      I1 = ISCT(IGFCOD,1)
      IF (I1 .EQ. 0) GOTO 70
      I2 = ISCT(IGFCOD,2)

      DO 60 II = I1,I2  
        I = IND1(II)    
        PGFBA = PGFBA + (0.005454154 * DBH(I)**2 * PROB(I)) 
   60 CONTINUE    

   70 CONTINUE    
C     
C     CALCULATE PROPORTION IF BASAL AREA IN GRAND FIR.
C     
      PGFBA = PGFBA / BA
      IF (IPRBMT .GT. 2) GOTO 80    

      PROTBK = 1.0 / (1.0 + EXP(-(-12.7746 -    
     >         1.4764 * (TOPO - 1.0) -    
     >         0.0992 * TMASHD +    
     >         4.8334 * PGFBA +     
     >         2.5529 * ALOG(BA)))) 

      GO TO 90    

   80 CONTINUE    
      PROTBK = 1.0 / (1.0 + EXP(-(-12.3652 -    
     >         1.4050 * (TOPO - 1.0) +    
     >         3.8230 * PGFBA +     
     >         2.2426 * ALOG(BA)))) 

   90 CONTINUE    
      IF (TMDEBU) THEN
         WRITE (JODFTM,100) PROTBK, IPRBMT, TOPO, TMASHD, PGFBA, BA
  100    FORMAT (//,' SUBROUTINE TMOTPR:  PROTBK   IPRBMT    TOPO    ',
     >        '  TMASHD    PGFBA     BA',/,T17,F10.4,I10,4F10.4)
      ENDIF

  110 CONTINUE    
      RETURN
      END   
