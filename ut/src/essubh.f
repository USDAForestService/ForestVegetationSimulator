      SUBROUTINE ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV2,ISER,GENTIM,
     &                   TRAGE)
      IMPLICIT NONE
C----------
C UT $Id: essubh.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
C     CREATED BY THE ESTABLISHMENT MODEL.
C
C
C     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON 
C     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER 
C     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE 
C     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING 
C     THE TREE.
C     CALLED FORM **ESTAB
C----------
C  COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'PLOT.F77'
C----------
C  DECLARATIONS
C----------
      INTEGER I,ISER,N,ITIME
      REAL    HHT,DELAY,DILATE,ELEV2,EMSQR,GENTIM,TRAGE,AGE
C----------       
C SPECIES ORDER FOR UTAH VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
C 21=BI, 22=BE, 23=OS, 24=OH
C
C VARIANT EXPANSION:
C GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
C NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM UT
C----------
      N=INT(DELAY+0.5)
      IF(N.LT.-3) N=-3
      DELAY=FLOAT(N)
      ITIME=INT(TIME+0.5)
      IF(N.GT.ITIME) DELAY=TIME
      AGE=TIME-DELAY-GENTIM+TRAGE
      IF(AGE.LT.1.0) AGE=1.0
      TRAGE=TIME-DELAY
C
      SELECT CASE (I)      
C----------
C     HEIGHT OF TALLEST SUBSEQUENT SPECIES 1 (WB)
C----------
      CASE (1) 
      HHT = 1.0
C----------
C     HEIGHT OF TALLEST SUBS. SPECIES 2 (LM)
C----------
      CASE (2) 
      HHT = 0.5
C----------
C     HT OF TALLEST SUBS. SPECIES 3 (DF)
C----------
      CASE (3) 
      HHT = 2.0
C----------
C     HT OF TALLEST SUBS. SPECIES 4 (WF)
C----------
      CASE (4) 
      HHT = 2.0
C----------
C     HT OF TALLEST SUBS. SPECIES 5 (BS)
C----------
      CASE (5) 
      HHT = 1.0
C----------
C     HT OF TALLEST SUBS. SPECIES 6 (AS)
C----------
      CASE (6) 
      HHT = 5.0
C----------
C     HT OF TALLEST SUBS. SPECIES 7 (LP)
C----------
      CASE (7) 
      HHT = 3.0
C----------
C     HT OF TALLEST SUBS. SPECIES 8 (ES)
C----------
      CASE (8) 
      HHT = 1.5
C----------
C     HT OF TALLEST SUBS. SPECIES 9 (AF)
C----------
      CASE (9) 
      HHT = 0.75
C----------
C     HEIGHT OF TALLEST SUBS. SPECIES 10 (PP)
C----------      
      CASE (10) 
      HHT = 3.0
C----------      
C     HT OF TALLEST SUBS. SPECIES 11 (PI), 14 (PM), 17 (GB)
C----------
      CASE (11,14,17)          
      HHT = 0.5
C----------         
C     HT OF TALLEST SUBS. SPECIES 12 (WJ), 15 (RM), 16 (UJ)
C----------
      CASE (12,15,16)          
      HHT = 0.5
C----------         
C     HT OF TALLEST SUBS. SPECIES 13 (GO)
C----------
      CASE (13)          
      HHT = 5.0
C----------         
C     HT OF TALLEST SUBS. SPECIES 18 (NC), 19 (FC), 22 (BE)
C----------
      CASE (18,19,22)          
      HHT = 10.0
C----------
C     HT OF TALLEST SUBS. SPECIES 20 (MC), 21 (BI)
C----------
      CASE (20,21)   
C      SI= SITEAR(I)
C      HHT = ((1.47043 + 0.23317*SI)/(31.56252 - 0.05586*SI))*AGE
      HHT=1.0
C----------         
C     HT OF TALLEST SUBS. SPECIES 23 (OS)
C----------
      CASE (23)          
      HHT = 1.0   
C----------         
C     HT OF TALLEST SUBS. SPECIES 24 (OH)
C----------
      CASE (24)          
      HHT = 5.0
C                    
      END SELECT  
C
      RETURN
      END 
C**END OF CODE SEGMENT 
 
 
