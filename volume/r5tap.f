C----------
C VOLUME $Id$
C----------
!== last modified  4-9-2002
      SUBROUTINE R5TAP(VOLEQ,DBHOB,HTTOT,HT2,D2,errflg)   
C       (TAPEQU-INT  DBHOB-R  HTTOT-R  HT2-R  D2-R)
C   
      CHARACTER*10 VOLEQ
      REAL DBHOB, HTTOT, HT2, D2, DM
      REAL*8 R5WKC(5,9), R5WKB(2,9)    
      REAL*8 DBH, TOTHT, HTUP, DIBCOR   
      REAL*8 TERM1, TERM2, TERM3, TERM4
      REAL*8 E   
      PARAMETER(E=2.7182818284)   
      INTEGER SP,errflg
C       
C          COFFICIENTS FOR WENSEL AND KRUMLAND MODEL (C1,C2,C3,C4,C5) 
C  DOUGLAS FIR (1)   
      DATA ((R5WKC(I,J),I=1,5),J=1,9)
     >      /0.84292,0.97062,-0.38163,-0.0074002,0.0,
C  PONDEROSA PINE (2)     
     >       0.87278,1.26066,-1.91214,0.020445,0.0,
C  SUGAR PINE (3)   
     >       0.90051,0.91588,-0.92964,0.0077119,-0.0011019,
C  WHITE FIR (4)   
     >       0.86039,1.45196,-2.42273,-0.15848,0.036947,
C  RED FIR (5)   
     >       0.87927,0.91350,-0.56617,-0.014480,0.0037262,
C  INCENSE CEDAR (6)   
     >       1.0,0.31550,-0.34316,0.0,-0.00039283,
C  JEFFERY PINE (7)
     >       0.82932,1.50831,-4.08016,0.047053,0.0,
C  LODGEPOLE PINE (8)
     >       1.0,0.84257,-0.98434,0.0,0.0,
C  REDWOOD (9)
     >       0.955,0.387,-0.362,-0.00581,0.00122/      
C     
C          COFFICIENTS FOR WENSEL AND KRUMLAND MODEL (B1,B2)    
C   
C  DOUGLAS FIR  (1) 
      DATA ((R5WKB(I,J),I=1,2),J=1,9)  
     >                   / 0.1420, 0.04302,
C  PONDEROSA PINE (2)    
     >                     0.1031, 0.03068,   
C  SUGAR PINE (3)   
     >                     0.0743, 0.02936,     
C  WHITE FIR (4)   
     >                     0.0844, 0.03320,         
C  RED FIR (5)   
     >                     0.1105, 0.05061,     
C  INCENSE CEDAR (6)   
     >                     0.1177, 0.03894,     
C  JEFFERY PINE (7)
     >                     0.1472, 0.03880,
C  LODGEPOLE PINE (8)
     >                     0.0147, 0.03223,
C  REDWOOD (9)
     >                     0.153, 0.035/
C     
C   
C ############################################################  
C   THE SECTION BETWEEN THE LINES OF "###" SHOULD NORMALLY BE   
C   COMMENTED OUT AS IT IS ONLY NECESSARY WHEN CHECKING COEFFICENTS
C    
C    
C      IF (TNIRP .EQ. 'YES') THEN   
C      NEWPG = CHAR(12)  
C      WRITE (2,5) NEWPG  
C   5  FORMAT(A1)  
C      WRITE (2,10)    
C  10  FORMAT(T20,'WENSEL AND KRUMLAND PROFILE MODEL COEFFICENTS',  
C    >  ' DEVELOPED FOR REGION FIVE',/,T40,'C1',T55,'C2',T70,   
C    >  'C3',T85,'C4',T100,'C5',T115,'B1',T130,'B2')   
C      WRITE (2,15) ((R5WKC(I,J),I=1,5),(R5WKB(I,J),I=1,2),   
C    >              J=1,6)   
C  15  FORMAT (' DOUGLAS FIR ' ,T30,6(F15.6),/,   
C    >  ' PONDEROSA PINE '            ,T30,7(F15.6),/,   
C    >  ' SUGAR PINE '                ,T30,7(F15.6),/,   
C    >  ' WHITE FIR '                 ,T30,7(F15.6),/,   
C    >  ' RED FIR '                   ,T30,7(F15.6),/,   
C    >  ' INCENSE CEDAR '             ,T30,7(F15.6),/,   
C   
C      TNIRP = 'NO '   
C      ENDIF   
C ############################################################# 
C   
C ************************************************************* 
C     THIS IS THE START OF THE MAIN LOGIC   
C ************************************************************* 
C   
C     CONVERT VALUES TO DOUBLE PRECISION FOR CACULATIONS   
      
      DBH = DBHOB   
      TOTHT = HTTOT   
      HTUP = HT2   
      DM = 0.0
      IF(VOLEQ(8:10).EQ.'202')THEN
         SP = 1
      ELSEIF(VOLEQ(8:10).EQ.'122')THEN
         SP = 2
      ELSEIF(VOLEQ(8:10).EQ.'117')THEN
         SP = 3
      ELSEIF(VOLEQ(8:10).EQ.'015')THEN
         SP = 4
      ELSEIF(VOLEQ(8:10).EQ.'020')THEN
         SP = 5
      ELSEIF(VOLEQ(8:10).EQ.'081')THEN
         SP = 6
      ELSEIF(VOLEQ(8:10).EQ.'116')THEN
         SP = 7
      ELSEIF(VOLEQ(8:10).EQ.'108')THEN
         SP = 8
      ELSEIF(VOLEQ(8:10).EQ.'211')THEN
         SP = 9
      else
         errflg = 1
         d2 = 0
         return
      ENDIF         

C   
C   WENSEL AND KRUMLAND PORTION TO CALCULATE UPPER STEM DIB *****
C   
      IF (HTUP .GT. TOTHT) then
          d2 = 0.0
          return
c          HTUP = TOTHT
      endif
      IF (HTUP.GE.4.499)THEN
         TERM1 = R5WKC(1,SP)   
         TERM2 = R5WKC(3,SP)+R5WKC(4,SP)*(DBH)+R5WKC(5,SP)*(TOTHT)     

         IF (SP.EQ.4.AND.TERM2.GT.-1.0) TERM2=-1.0

         TERM3 = ((HTUP-1)/(TOTHT-1))**R5WKC(2,SP)
         TERM4 = LOG(1-TERM3*(1-EXP(R5WKC(1,SP)/TERM2-DM/(DBH*TERM2))))
         DIBCOR = DBH * (TERM1 - TERM2 * TERM4)
C   
C    STUMP PORTION **********************************************
C   
       ELSE
          TERM1 = (1-R5WKB(1,SP))*DBH
          TERM2 = EXP(R5WKB(2,SP)*(4.5-HTUP))
          DIBCOR = TERM1*TERM2

       ENDIF
        
       D2 = REAL(DIBCOR)
C   
C*****************************************************************
C    THIS IS THE END OF THE MAIN LOGIC   
C*****************************************************************
C   
C        
      RETURN   
      END   
