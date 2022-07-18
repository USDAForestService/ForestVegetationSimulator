      SUBROUTINE DFTMGO(L)    
      IMPLICIT NONE
C---------- 
C  **DFTMGO DATE OF LAST REVISION:  05/13/13
C---------- 
C     
C     PARAMETERS: 
C       L     = TRUE WHEN THE TUSSOCK MOTH MODEL IS TO BE CALLED  
C     
C     NICK CROOKSTON                     JAN 1978     
C     
C     EVENTS CONTROLED BY THIS ROUTINE:   
C     
C     1) DECIDES IF AN OUTBREAK WILL TAKE PLACE DURING THIS CYCLE 
C        ITMETH CONTROLS THE METHOD THAT THE DECISION IS MADE:    
C        1 = ACTIVITY SCHEDULE IS UNDERCONTROL...MANSTART WAS SPECIFIED 
C        2 = THE OUTBREAK PROBABILTIY IS COMPAIRED TO A RANDOM NUMBER   
C            (RANSTART).
C        3 = THE OUTBREAK PROBABILITY IS COMPAIRED TO 0.5 (CRTSTART).   
C     2) SETS LOGICAL VARIABLES 'LDF' AND 'LGF' 
C     3) SETS NUMBER OF CLASSES TO NUMBER 'MIN0(POSSABLE,REQUESTED)'    
C     
C  REVISION HISTORY:
C    01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
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
C     TMBASE= THE TUSSOCK MOTH MODEL BASE PREIOD
C     
      INTEGER TMBASE    
      LOGICAL L   
      INTEGER NPRMS,IDT,IDF,I1,I2,II,I,IGF,ISPOT,J
      INTEGER IYR1,IYR2,IACTK,KODE
      REAL X
      REAL PRMS(2),PROTBK

      DATA TMBASE /  5  /     


      L = .FALSE. 

      IF (TMDEBU) WRITE (JOSTND,*) '*** IN SUBROUTINE DFTMGO '   
C     
C     CALCULATE THE PROBABILITY OF AN OUTBREAK  
C     
      CALL TMOTPR (PROTBK)    
C     
C     ARE WE GOING TO SIMULATE AN OUTBREAK?     
C     
C     FIND OUT IF ONE OF THE TUSSOCK MODE ACTIVITY CODES ARE
C     PRESENT FOR THE PERIOD COVERED BY THIS CYCLE (NOTE THAT THE 
C     YEARS COVERED DIFFER FROM THE CYCLE BOUNDARYS, THEREFORE,   
C     OPGET2 IS USED RATHER THAN OPGET TO FIND THE ACTIVITY.
C     NOTE THAT IDT IS RETURNED AS A ZERO IF THERE IS NO OUTBREAK.
C     
      IYR1 = IY(ICYC) - 2     
      IYR2 = IY(ICYC) + 2     
      IACTK = 810 

      CALL OPGET2(IACTK,IDT,IYR1,IYR2,1,2,NPRMS,PRMS,KODE)  

      IF (KODE .GT. 0) IACTK = 811  
      IF (KODE .GT. 0) CALL OPGET2(IACTK,IDT,IYR1,IYR2,1,2, 
     >                             NPRMS,PRMS,KODE)   

C     
C     SCALE AND SAVE THE OUTBREAK PROBABILITY   
C     
      PROTBK = PROTBK * PRBSCL
      TMPRB(ICYC) = PROTBK    
C     
C     IF THERE IS NO OUTBREAK SCHEDULED, RETURN.
C     
      IF (KODE .GT. 0) THEN   
        IF (TMDEBU) WRITE (JOSTND,*) '*** LEAVING SUBROUTINE DFTMGO; ',
     &     ' NO OUTBREAK SCHEDULED' 
        RETURN    
      ENDIF 
C     
C     LET TMYRS KEEP A RECORD OF REGIONAL OUTBREAKS.  
C     
      TMYRS(ICYC) = IDT 
      GOTO (60,25,24),ITMETH  

   24 CONTINUE    
      CALL TMRANN(X)    
      IF (X .GE. PROTBK) GO TO 50   
      GO TO 60    

   25 CONTINUE    
      IF (PROTBK .LE. 0.5) GO TO 50 
      GO TO 60    

   50 CONTINUE    
      CALL OPDEL2(IYR1,IYR2,IACTK,1)
      IF (TMDEBU) WRITE (JOSTND,*) '*** LEAVING SUBROUTINE DFTMGO'     
      RETURN

   60 CONTINUE    
C     
C     ARE WE GOING TO SIMULATE TM ON DOUGLAS FIR?     
C     
      IDF = 0     
      CNTDF = 0.0 
      IF (.NOT. LDF) GO TO 100
      I1 = ISCT(IDFCOD,1)
      IF (I1 .EQ. 0) GO TO 90
      I2 = ISCT(IDFCOD,2)
      IDF = I2-I1+1     
      IF (NCLAS(1) .LE. 0) GO TO 90 
      IF (IDF .LT. 1) GO TO 90

      DO 80 II = I1, I2 
        I = IND1(II)    
        CNTDF = CNTDF + PROB(I)     
   80 CONTINUE    

      IF (CNTDF .LT. .01) GO TO 90  
      GO TO 100   

   90 CONTINUE    
      LDF = .FALSE.     

  100 CONTINUE    
C     
C     ARE WE GOING TO SIMULATE TM ON GRAND FIR? 
C     
      IGF = 0     
      CNTGF = 0.0 
      IF (.NOT. LGF) GO TO 195

      I1 = ISCT(IGFCOD,1)
      IF (I1 .EQ. 0) GO TO 190

      I2 = ISCT(IGFCOD,2)
      IGF = I2 - I1 + 1 
      IF (IGF .LT. 1) GO TO 190     
      IF (NCLAS(2) .LE. 0) GO TO 190

      DO 130 II = I1, I2
        I = IND1(II)    
        CNTGF = CNTGF + PROB(I)     
  130 CONTINUE    

      IF ( CNTGF .LT. .01 ) GO TO 190     
      GO TO 195   

  190 CONTINUE    
      LGF = .FALSE.     

  195 CONTINUE    
      L = (LDF .OR. LGF)
C     
C     SET NUMBER OF ACTUAL CLASSES PER SPECIES. 
C     
      NACLAS(1) = 0     
      NACLAS(2) = 0     
      IF (LDF) NACLAS(1) = MIN0(IDF, NCLAS(1))  
      IF (LGF) NACLAS(2) = MIN0(IGF, NCLAS(2))  

      IF (L) GO TO 210  

      WRITE (JODFTM,200)
  200 FORMAT (/,'***** WARNING:  A REQUESTED OR PREDICTED',
     >        ' TUSSOCK MOTH OUTBREAK CAN NOT BE SIMULATED.')     
      CALL OPDEL2 (IYR1, IYR2, IACTK, 1)  
      GO TO 220   

  210 CONTINUE    
      IF (.NOT. TMDEBU) GO TO 230   

  220 CONTINUE    
      WRITE (JODFTM,225) CNTDF, IDF, NACLAS(1), NCLAS(1), LDF,    
     >                   CNTGF, IGF, NACLAS(2), NCLAS(2), LGF     
  225 FORMAT (/,'DUMP OF CONTROLING VARIABLES:',//,  
     >        T14,'TREES/ACRE  NUMBER OF TREE  NUMBER OF CLASSES',
     >        '      T=GO',/,T29,'RECORDS      ACTUAL    REQUESTED',    
     >        '    F=NO GO',//
     >        ' DOUGLAS FIR:',T13,F10.3,I11,2I13,L8,//,     
     >        ' GRAND FIR:',  T13,F10.3,I11,2I13,L8)  

  230 CONTINUE    
C     
C     INSURE TMWORK IS KEEPING A HISTORY OF OUTBREAKS 
C     
      TMWORK(ICYC) = L  
C     
C     INSURE THAT AN ENDING CYCLE EXISTS AND THAT NO CYCLES 
C     EXIST IN THE MEANTIME.  
C     
      IF (.NOT. L) GO TO 245  
C     
C     SIGNAL THE OPTION PROCESSING ROUTINES THAT THE OUTBREAK "HAS"     
C     HAPPENED.   
C     
      CALL OPDON2(IACTK,IY(ICYC),IYR1,IYR2,1,KODE)    
C     
C     IF MANUAL SCHEDULING WAS USED (MANSCHED) ALLOW IFINT TO BE  
C     EQUAL TO TMBASE + OR - 1
C     
      IF ( ITMSCH .EQ. 1 .AND. IABS(IFINT-TMBASE) .LE. 1 ) GO TO 245    
C     
C     INSURE THAT IFINT EQUALS TMBASE     
C     
      CALL INSCYC (ICYC,TMBASE,ISPOT,.TRUE.,.FALSE.,JODFTM,TMDEBU)
      IF (ISPOT .EQ. 0) GO TO 235   

      WRITE (JODFTM,9001) TMBASE, IY(ISPOT)     
 9001 FORMAT (/'***** WARNING:  TO INSURE A',I2,' YEAR TUSSOCK MOTH',  
     >        ' CYCLE LENGTH A CYCLE WAS INSERTED AT YEAR',I5,'.')

  235 CONTINUE    
      IF (IFINT .GE. TMBASE) GO TO 245    
C     
C     IFINT IS TOO SMALL:  ONE OR MORE CYCLES MUST BE DELETED     
C     
      J = ICYC+1  
      WRITE (JODFTM,9002) TMBASE, IY(J)   
 9002 FORMAT (/'***** WARNING:  TO INSURE A',I2,' YEAR TUSSOCK MOTH',  
     >        ' CYCLE LENGTH, THE CYCLE SCHEDULED TO BEGIN IN',I5,
     >        ' WAS CANCELED.')     

      NCYC = NCYC - 1   

      DO 240 I = J, NCYC
C       LDEBUG(I) = LDEBUG(I+1)     
C       LIST(I) = LIST(I+1)   
        IY(I) = IY(I+1) 
  240 CONTINUE    

      IY(NCYC+1) = IY(NCYC+2) 
      IFINT = IY(ICYC+1) - IY(ICYC) 
      FINT = IFINT
C     DEBUG = LDEBUG(ICYC)    
      CALL OPCYCL (NCYC,IY)   
      CALL OPCSET (ICYC)
      GO TO 235   

  245 CONTINUE    
      IF (TMDEBU) THEN  
        WRITE (JODFTM,250) L  
  250   FORMAT (/,'T=SIMULATION WILL GO; F=NO GO   " ',L1,' "')  
        WRITE (JOSTND,*) '*** LEAVING SUBROUTINE DFTMGO'    
      ENDIF 

      RETURN
      END   
