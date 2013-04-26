      SUBROUTINE TMOUT  
      IMPLICIT NONE
C----------
C  **TMOUT  DATE OF LAST REVISION:  04/01/13
C----------
C
C     PRINT FINAL TUSSOCK MOTH ACTIVITY SUMMARY
C
C     PART OF THE DFTM EXTENSION OF THE PROGNOSIS SYSTEM.
C
C Revision History:
C   23-DEC-99; Lance R. David (FHTET-FC)
C      Updated for expansion of FVS stand id (variable NPLT)
C      from 8 to 26 characters.
C   01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'TMCOM1.F77'
C
COMMONS
C
      CHARACTER*3 YES, NO, WASTHR

      CHARACTER*132 PRNT

      INTEGER I

      DATA YES /'YES'/,  NO /'NO'/
      LOGICAL LOPEN

C
C     CHECK TO SEE IF OUTPUT FILE IS OPEN.  IF NOT OPEN THEN EXIT.
C
      INQUIRE (UNIT=JODFTM, OPENED=LOPEN)
      IF (.NOT. LOPEN) GOTO 200

C     
C     IF THE DFTM OUTPUT FILE DATA SET REFERENCE NUMBER IS  
C     EQUAL TO THE MAJOR PRINT FILE NUMBER; THEN RETURN.    
C     
      IF (JODFTM .EQ. JOSTND) RETURN

C     
C     REWIND DFTM OUTPUT FILE.
C     
      REWIND JODFTM     

C     
C     IF THE REPORT LEVEL IS ZERO; RETURN 
C     
      IF (ITMREP .EQ. 0) RETURN     

      CALL TMHED (JOSTND, NPLT, MGMID)    
      IF (ITMREP .EQ. 1) GOTO 50    

   10 CONTINUE    
      READ (JODFTM,20,END=40) PRNT  
   20 FORMAT (A132)     

      WRITE (JOSTND,20) PRNT  
      GOTO 10     

   40 CONTINUE    
C     
C     READY THE OUTPUT FILE FOR ANOTHER STAND.  
C     
      REWIND JODFTM     
      GOTO 60     

   50 CONTINUE    
      I = 0 
      GOTO 70     

   60 CONTINUE    
      I = 1 

   70 CONTINUE    
      WRITE (JOSTND,9016) I, NPLT   
 9016 FORMAT (I1,22('-'),'  DFTM OUTBREAK SUMMARY TABLE  ',26('-'),/,
     >      ' STAND ID = ',A26,//,    
     >      ' ------ CYCLE ------',8X,'YEAR OF',T47,'CONDITIONAL',T68,  
     >      'WAS THERE AN'/' NUMBER',T16,'YEARS',5X,'REGIONAL DFTM',    
     >      T47,'PROBABILITY',10X,'OUTBREAK IN'/T29,'OUTBREAK',7X,
     >      'OF STAND OUTBREAK         STAND?'/1X,79('-')/)   

      DO 140 I=1,NCYC   
        WASTHR = NO     
        IF (TMWORK(I)) WASTHR = YES 
        IF (TMYRS(I) .EQ. 0) WRITE (JOSTND,9017) I, IY(I), IY(I+1),     
     >                       TMPRB(I), WASTHR   
        IF (TMYRS(I) .NE. 0) WRITE (JOSTND,9018) I, IY(I), IY(I+1),     
     >                       TMYRS(I), TMPRB(I), WASTHR     
 9017   FORMAT(1X,I3,I9,' -',I5,T45,F10.3,T71,A3)     
 9018   FORMAT(1X,I3,I9,' -',I5,I14,T45,F10.3,T71,A3) 
  140 CONTINUE    

  200 CONTINUE

      RETURN
      END   
