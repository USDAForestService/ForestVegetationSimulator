      SUBROUTINE DFTMIN(LKECHO)
      IMPLICIT NONE
C---------- 
C  **DFTMIN            DATE OF LAST REVISION:  06/30/10
C---------- 
C     
C     OPTION PROCESSOR FOR DOUGLAS FIR TUSSOCK MOTH MODEL   
C     
C     NICK CROOKSTON, PROGRAMMER           JAN 1978  & JULY 1981  
C
C REVISION HISTORY:
C   06-AUG-01 LANCE R. DAVID (FHTET)
C     MOVED LOGICAL VARIABLES LNPV2, LNPV3, LRANSD, LTMPRM, LCHEM TO TMCOM1
C     AND INITIALIZE IN TMINIT.
C     (PREVIOUSE REVISION DATE WAS 01/06/89)
C   10-NOV-2003 - Lance R. David (FHTET)
C     Added LFLAG to KEYRDR call statement argument list.
C-----------------------------------------------------------------------------
COMMONS     
C
      INCLUDE 'PRGPRM.F77'
C     
      INCLUDE 'CONTRL.F77'
C     
      INCLUDE 'TMEGGS.F77'    
C     
      INCLUDE 'TMCOM1.F77'
C     
      INCLUDE 'UPPER.F77'
C     
      INCLUDE 'LOWER.F77'
C     
COMMONS     
C     
      CHARACTER*4 SPEC(2), PRMTYP(3)
      CHARACTER*8 TABLE, KEYWRD, PASKEY   
      CHARACTER*10 KARD 
      CHARACTER*80 PNAME
      INTEGER ITMETH,IPRBMT,ITMSCH,ITMSLV,JODFTM,JOTMDK,ITMREP
      INTEGER IDFCOD,IGFCOD,JODFEC,NCLAS,IEGTYP,NACLAS
      INTEGER IBMTYP,IDT,I,I1,I2,ISP,J,IPH,K1,INSTAR
      INTEGER K2,K3,KEY,ISIZE,KODE,NUMBER
      REAL G1,R1,B1,Z1,X1,R0,B0,X0,TMPN1,TMASHD,TMPRB,TMDEFL
      REAL TOPO,CNTGF,CNTDF,PRBSCL,GFFBIO,DFFBIO,GFPNEW
      REAL DFPNEW,WEIGHT,GFREGG,DFREGG,GFEGG,DFEGG
      REAL REGG,DEGG,PRMS,ARRAY,EFFIC,Y1,F1
      REAL         RSEED
      LOGICAL LNOTBK,LFIRST,LKECHO
      LOGICAL DEBUG     

      DIMENSION DEGG(6), REGG(6), KARD(7)    
      DIMENSION ARRAY(7), TABLE(50), LNOTBK(7), PRMS(1)  

C     This equivalence enables arrays DFEGG and GFEGG to be referenced
C     by DEGG, and arrays DFREGG and GFREGG to be referenced by REGG.
C     This requires that the order of the variables DFEGG, GFEGG, DFREGG,
C     and GFREGG remain in the order listed here.  The results of this
C     statement are:
C                    DEGG(1)-(3) = DFEGG(1)-(3)
C                    DEGG(4)-(6) = GFEGG(1)-(3)
C                    REGG(1)-(3) = DFREGG(1)-(3)
C                    REGG(4)-(6) = GFREGG(1)-(3)

      EQUIVALENCE (DEGG(1), DFEGG(1)), (REGG(1), DFREGG(1))    

      DATA SPEC / ' (DF', ' (GF' /  
      DATA LFIRST / .TRUE. /  
      DATA PRMTYP / ' B0(', ' R0(', ' B1(' /    
      DATA ISIZE / 50 / 
      DATA DEBUG /.FALSE./    

      DATA TABLE /
     >     'END     ','REPORT  ','DATELIST','NODFRUN ','NOGFRUN ',
     >     'MANSCHED','DEBUG   ','RANNSEED','WEIGHT  ','NUMCLASS',
     >     'RANLARVA','DETLARVA','DFTMECHO','REDIST  ','NOREDIST',
     >     '        ','        ','        ','        ','NPV2    ',
     >     'NPV3    ','        ','        ','        ','        ',
     >     'TMPARMS ','CHEMICAL','DEBUTREE','        ','PUNCH   ',
     >     '        ','BIOMASS ','DFBIOMAS','GFBIOMAS','        ',
     >     'RANSCHED','MANSTART','ASHDEPTH','        ','CRTSTART',
     >     'RANSTART','PROBMETH','TOPO    ','        ','SALVAGE ',
     >      5 * ' ' /   


      IF (.NOT. LFIRST) GOTO 10     
      LFIRST = .FALSE.  
      LRANSD = .FALSE.  
C06aug01      LNPV2  = .FALSE.  
C06aug01      LNPV3  = .FALSE.  
      LTMPRM = .FALSE.  
      LCHEM  = .FALSE.  
C     
C     OPEN JODFTM FILE. 
C     
      CALL MYOPEN (10, ' ', 4, 132, 0, 1, 1, 0, KODE)

C
C     CALL CH8SRT TO CREATE A SORTED INDEX FOR TABLE.
C
C     CALL CH8SRT (ISIZE,TABLE,INDEX,.TRUE.)

   10 CONTINUE
C
C     READ KEYWORDS
C
      CALL KEYRDR (IREAD, JOSTND, DEBUG, KEYWRD, LNOTBK, ARRAY,
     +             IRECNT, KODE, KARD, LFLAG,LKECHO)
C
C     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF
C
      IF (KODE .EQ. 0) GO TO 30
      IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
      CALL ERRGRO (.TRUE.,6)  
      GOTO 10     

   30 CONTINUE    
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)   
C     
C     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.  
C     
      IF (KODE .EQ. 0) GO TO 40     
      IF (KODE .EQ. 1) CALL ERRGRO (.TRUE., 1)  
      IF (KODE .EQ. 1) GOTO 10
      CALL ERRGRO (.TRUE., 5) 

   40 CONTINUE    

C     GO TO 60    
C
C     SPECIAL END OF FILE TARGET (CURRENTLY NOT USED).
C     
C  50 CONTINUE    
C     CALL ERRGRO (.FALSE.,2) 
C  60 CONTINUE    

C     
C     PROCESS OPTIONS   
C     
      GO TO( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000    
     >      ,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000    
     >      ,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000    
     >      ,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000    
     >      ,5100,5200,5300,5400,5500,5600,5700,5800,5900,6000    
     >      ),  NUMBER  

  900 FORMAT (/1X,A8)   

 1000 CONTINUE    
C
C======================  OPTION NUMBER 1: END  ================== 
C     
      IF(LKECHO)WRITE(JOSTND,1010) KEYWRD    
 1010 FORMAT (/1X,A8,'   END OF DFTM OPTIONS.') 
      RETURN

 1200 CONTINUE    
C     
C======================  OPTION NUMBER 2: REPORT  =============== 
C     
      IF (LNOTBK(1)) ITMREP = IFIX(ARRAY(1))    
      IF(LKECHO)WRITE(JOSTND,1220) KEYWRD,ITMREP   
 1220 FORMAT (/1X,A8,'   REPORT LEVEL=',I3,     
     >        ' (0=NONE,1=SUMMARY ONLY,2=NORMAL)')    
      GO TO 10    

 1300 CONTINUE    
C     
C======================  OPTION NUMBER 3: DATELIST  ============= 
C
      IF(LKECHO)WRITE(JOSTND,2) KEYWRD 
      CALL TMDTLS (JOSTND)    
      GO TO 10    

 1400 CONTINUE    
C     
C======================  OPTION NUMBER 4: NODFRUN  ============== 
C     
      LDF = .FALSE.     
      IF(LKECHO)WRITE(JOSTND,1410) KEYWRD    
 1410 FORMAT (/1X,A8,'   EXCLUDE DOUGLAS-FIR')  
      GO TO 10    

 1500 CONTINUE    
C     
C======================  OPTION NUMBER 5: NOGFRUN  ============== 
C     
      LGF = .FALSE.     
      IF(LKECHO)WRITE(JOSTND,1510) KEYWRD    
 1510 FORMAT (/1X,A8,'   EXCLUDE GRAND FIR')
      GO TO 10    

 1600 CONTINUE    
C     
C======================  OPTION NUMBER 6: MANSCHED  ============= 
C     
      IDT = 1     
      IF (LNOTBK(1)) IDT = IFIX(ARRAY(1)) 
      ITMSCH = 1  

      CALL OPNEW (KODE,IDT,810,0,PRMS)    
      IF (KODE.GT.0) GOTO 10  

      IF(LKECHO)WRITE(JOSTND,1610) KEYWRD,IDT
 1610 FORMAT (/1X,A8,'   DATE/CYCLE=',I5, 
     >        '; REGIONAL OUTBREAKS SCHEDULED BY USER.')    
C     
C     KEEP TRACK OF OUTBREAKS SCHEDULED BY DATE.
C     
      IF (IDT .LE. 40) GOTO 10
      TMYRS(41) = TMYRS(41) + 1     
      I = TMYRS(41)     
      TMYRS(I) = IDT    
      GO TO 10    

 1700 CONTINUE    
C     
C======================  OPTION NUMBER 7: DEBUG  ================ 
C     
      TMDEBU = .TRUE.   
      ITMREP = 2  
      JODFTM = JOSTND   
      IF(LKECHO)WRITE(JOSTND,900) KEYWRD     
      GO TO 10    

 1800 CONTINUE    
C     
C======================  OPTION NUMBER 8  RANNSEED  ============= 
C
C BECAUSE OF PROBLEMS WITH THE DFTM RANDOM NUMBER GENERATOR WHEN RUNNING
C ON A PC, IT HAS BEEN REPLACED BY CODE FROM THE GENERATOR USED IN THE
C MOUNTAIN PINE BEETLE MODEL, WHICH USES ONLY ONE SEED VALUE.
C LANCE R. DAVID 4/4/90

      LRANSD = .TRUE.   

C     IF (LNOTBK(1)) IR1=ARRAY(1)   
C     IF (LNOTBK(2)) IR2=ARRAY(2)   
C     IF (LNOTBK(3)) IR3=ARRAY(3)   
C
C     CALL TMRNSD(IR1, IR2, IR3)    
C
C     IF(LKECHO)WRITE(JOSTND,1810) KEYWRD,IR1,IR2,IR3    
C1810 FORMAT (/1X,A8,'   DFTM RANDOM NUMBER GENERATOR WAS RESEEDED:',   
C    >        3I13)     

      RSEED=REAL(ARRAY(1))
      CALL TMRNSD (LNOTBK(1), RSEED)
      IF(LKECHO)WRITE(JOSTND,1810) KEYWRD, RSEED
 1810 FORMAT (/1X,A8,'   DFTM RANDOM NUMBER SEED IS: ',F10.0)
      GO TO 10    

 1900 CONTINUE    
C     
C======================  OPTION NUMBER 9: WEIGHT  =============== 
C     
      IF (LNOTBK(1)) WEIGHT(1) = ARRAY(1) 
      IF (LNOTBK(2)) WEIGHT(2) = ARRAY(2) 

      IF(LKECHO)WRITE(JOSTND,1920) KEYWRD,WEIGHT(1),WEIGHT(2)  
 1920 FORMAT (/1X,A8,'   THE CLASSIFICATION WEIGHTING ',    
     >        'FACTORS ARE: ',F6.2,' FOR % NEW FOLIAGE, AND ',    
     >        F6.2,' FOR FOLIAGE BIOMASS.')     
      GO TO 10    

 2000 CONTINUE    
C     
C======================  OPTION NUMBER 10: NUMCLASS  ============ 
C     
      I1 = NCLAS(1)
      I2 = NCLAS(2)     
      IF (LNOTBK(1)) I1 = ARRAY(1)  
      IF (LNOTBK(2)) I2 = ARRAY(2)  
      IF ((I1 + I2) .LE. 100) GOTO 2010   

      CALL KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)     
      CALL ERRGRO (.TRUE., 4) 
      GOTO 10     

 2010 CONTINUE    
      NCLAS(1) = I1     
      NCLAS(2) = I2     
      IF (LNOTBK(3)) TMPN1 = ARRAY(3)     

      IF(LKECHO)WRITE(JOSTND,2020) KEYWRD, NCLAS, TMPN1  
 2020 FORMAT (/1X,A8,'   NUMBER OF REQUESTED CLASSES OF ',  
     >     'DOUGLAS-FIR= ',I3,'; GRAND FIR=',I3/T13,'PROPORTION OF ',   
     >        'CLASSES DEFINED BY FINDING DIFFERENCES BETWEEN',   
     >        ' TREES = ',F5.2/T13,'THE REMAINING CLASSES ARE FOUND',   
     >        ' BY HALVING THE CLASSES WITH THE MOST TREE RECORDS.')
      GOTO 10     

 2100 CONTINUE    
C     
C======================  OPTION NUMBER 11: RANLARVA  ============ 
C     
      ISP = IFIX(ARRAY(1))    
      IF (ISP .GE. 1 .AND. ISP .LE. 2) GOTO 2110

      CALL KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)     
      CALL ERRGRO (.TRUE., 4) 
      GOTO 10     

 2110 CONTINUE    
      IEGTYP = 1  

      DO 2120 I=2,4     
        IF (LNOTBK(I)) REGG(3 * ISP + I - 4) = ARRAY(I)     
 2120 CONTINUE    

      IF(LKECHO)WRITE(JOSTND,2130) KEYWRD, ISP, SPEC(ISP),     
     >                    (REGG(3 * ISP + I - 3), I=1,3)    
 2130 FORMAT (/1X,A8,'   RANDOM FIRST INSTAR LARVAE ASSIGNMENT ', 
     >        'FOR SPECIES',I3,A4,')'/T13,'AVERAGE =',F7.2, 
     >        '; WITHIN-OUTBREAK STANDARD DEVIATION =',F6.2,
     >        '; BETWEEN-OUTBREAK STANDARD DEVIATION =',F6.2)     
      GOTO 10     

 2200 CONTINUE    
C     
C======================  OPTION NUMBER 12: DETLARVA  ============ 
C     
      ISP = IFIX(ARRAY(1))    
      IF (ISP .GE. 1 .AND. ISP .LE. 2) GOTO 2210
      CALL KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)     
      CALL ERRGRO (.TRUE.,4)  
      GOTO 10     

 2210 CONTINUE    
      IEGTYP = 2

      DO 2220 I=2,4     
        IF (LNOTBK(I)) DEGG(3 * ISP + I - 4) = ARRAY(I)     
 2220 CONTINUE    

      IF(LKECHO)WRITE(JOSTND,2230) KEYWRD, ISP, SPEC(ISP),
     >                    (DEGG(3 * ISP + I - 3), I=1,3)    
 2230 FORMAT(/1X,A8,'   FIRST INSTAR LARVAE ALLOCATION BY THIRDS OF ',  
     >        'AVERAGE DBH DISTRIBUTION.'/T13,  
     >        'THREE LEVELS ON SPECIES',I3,A4,') ARE:',3F7.1)     
      GOTO 10     

 2300 CONTINUE    
C     
C======================  OPTION NUMBER 13: DFTMECHO  ============
C

C
C     SET JODFEC TO THE UNIT NUMBER FOR THE POST PROCESSOR OUTPUT.
C
      JODFEC = 11

C
C     READ IN THE FILE NAME FROM THE SUPPLEMENTAL RECORD.
C
      READ (IREAD,2310) PNAME
 2310 FORMAT (A)
      IF (PNAME .EQ. ' ') PNAME = 'DFTMOUT'

C
C     TRY TO OPEN THE FILE.
C
      CALL MYOPEN (JODFEC, PNAME, 1, 133, 0, 1, 1, 0, KODE)

C
C     PRINT OUT KEYWORD MESSAGE BASED ON IF THE FILE OPENS OR NOT.
C
      IF (KODE .EQ. 1) THEN
         WRITE(JOSTND,2320) KEYWRD, PNAME
 2320    FORMAT (/1X,A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,
     &           12X,'WAS NOT OPENED DUE TO AN ERROR!!!!')
         JODFEC = 0
      ELSE
         IF(LKECHO)WRITE(JOSTND,2330) KEYWRD, PNAME
 2330    FORMAT (/1X,A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,
     &           12X,'WAS OPENED.')
      ENDIF

      GO TO 10

 2400 CONTINUE
C
C======================  OPTION NUMBER 14: REDIST  ============== 
C     
      IF (LNOTBK(1)) B0(62) = ARRAY(1)    
      IF(LKECHO)WRITE(JOSTND,2410) KEYWRD, B0(62)  
 2410 FORMAT (/1X,A8,'   ANNUAL TUSSOCK MOTH REDISTRIBUTION ',    
     >        'RATE = ',F6.2) 
      GO TO 10    

 2500 CONTINUE    
C     
C======================  OPTION NUMBER 15: NOREDIST  ============ 
C     
      B0(62) = 0.0
      IF(LKECHO)WRITE(JOSTND,2410) KEYWRD, B0(62)  
      GO TO 10    

 2600 CONTINUE    
C     
C======================  OPTION NUMBER 16:  ===================== 
C     
      GO TO 10    

 2700 CONTINUE    
C     
C======================  OPTION NUMBER 17:  ===================== 
C     
      GO TO 10    

 2800 CONTINUE    
C     
C======================  OPTION NUMBER 18:  ===================== 
C     
      GO TO 10    

 2900 CONTINUE    
C     
C======================  OPTION NUMBER 19:  ===================== 
C     
      GO TO 10    

 3000 CONTINUE    
C     
C======================  OPTION NUMBER 20: NPV2  ================ 
C     
      LNPV2 = .TRUE.    
      B0(9) = 0.036     
      IF (LNOTBK(1)) B0(9) = ARRAY(1)     
      B0(10) = 0.039    
      IF (LNOTBK(2)) B0(10) = ARRAY(2)    
      B0(11) = 0.042    
      IF (LNOTBK(3)) B0(11) = ARRAY(3)    
      B0(12) = 0.072    
      IF (LNOTBK(4)) B0(12) = ARRAY(4)    

      IF(LKECHO)WRITE(JOSTND,3010) KEYWRD, (PRMTYP(1), I, B0(I), I=9,12)   
 3010 FORMAT (/1X,A8,'   OUTBREAK MODEL PARAMETER VALUE(S):  ',   
     >        4(A4,I2,')= ',F10.4)) 
      GOTO 10     

 3100 CONTINUE    
C     
C======================  OPTION NUMBER 21: NPV3  ================ 
C     
      LNPV3 = .TRUE.    
      B0(15) = 0.036    
      IF (LNOTBK(1)) B0(15) = ARRAY(1)    
      B0(16) = 0.039    
      IF (LNOTBK(2)) B0(16) = ARRAY(2)    
      B0(17) = 0.042    
      IF (LNOTBK(3)) B0(17) = ARRAY(3)    
      B0(18) = 0.072    
      IF (LNOTBK(4)) B0(18) = ARRAY(4)    

      IF(LKECHO)WRITE(JOSTND,3010) KEYWRD, (PRMTYP(1), I, B0(I),I=15,18)   
      GO TO 10    

 3200 CONTINUE    
C     
C======================  OPTION NUMBER 22:  ===================== 
C     
      GO TO 10    

 3300 CONTINUE    
C     
C======================  OPTION NUMBER 23:  ===================== 
C     
      GO TO 10    

 3400 CONTINUE    
C     
C======================  OPTION NUMBER 24:  ===================== 
C     
      GO TO 10    

 3500 CONTINUE    
C     
C======================  OPTION NUMBER 25:  ===================== 
C     
      GO TO 10    

 3600 CONTINUE    
C     
C======================  OPTION NUMBER 26: TMPARMS  ============= 
C     
      I = IFIX(ARRAY(1))
      J = IFIX(ARRAY(2))
      IF (LNOTBK(1) .AND. LNOTBK(2) .AND. LNOTBK(3)) GO TO 3610   

 3602 CONTINUE    
      IF(LKECHO)WRITE(JOSTND,3605) I, J, ARRAY(3)  
 3605 FORMAT (/T13,'I=',I2,' J=',I2,' VALUE=',F5.2)   
      CALL KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)     
      CALL ERRGRO (.TRUE., 4) 
      GOTO 10     

 3610 CONTINUE    
      LTMPRM = .TRUE.   
      IF (J .EQ. 0 .OR. J .GT. 6) GO TO 3602    
      IF (I .GE. 2 .AND. I .LE. 12 ) GO TO 3620 
      IF (I .GE. 19.AND. I .LE. 21 ) GO TO 3630 
      IF (I .GE. 22.AND. I .LE. 25 ) GO TO 3640 
      GO TO 3602  

 3620 CONTINUE    
      IPH = 1     
      K1 = (I - 2) * 6 + J    
      B0(K1) = ARRAY(3) 
      GOTO 3650   

 3630 CONTINUE    
      IPH = 2     
      K1 = (I - 19) * 6 + J   
      R0(K1) = ARRAY(3) 
      GOTO 3650   

 3640 CONTINUE    
      IPH = 3     
      K1 = (I - 21) * 6 + J   
      B1(K1) = ARRAY(3) 

 3650 CONTINUE    
      IF(LKECHO)WRITE(JOSTND,3060) KEYWRD, I, J, PRMTYP(IPH),
     &                              K1, ARRAY(3) 
 3060 FORMAT(/1X,A8,'   RECORD NUMBER= ',I2,' COLUMN= ',I2, 
     >       ' MODEL PARAMETER',A4,I2,')= ',F10.4)    
      GO TO 10    

 3700 CONTINUE    
C     
C======================  OPTION NUMBER 27: CHEMICAL  ============ 
C     
      LCHEM = .TRUE.    
      IPH = 3     
      I = ARRAY(1)
      IF (I .GE. 1 .AND. I .LE. 4) IPH = I
      INSTAR = 4  
      I = ARRAY(2)
      IF (I .GE. 1 .AND. I .LE. 6) INSTAR = I   
      EFFIC = 0.95
      IF (ARRAY(3) .GE. 0.0 .AND. ARRAY(3) .LE. 1.0 .AND. LNOTBK(3))    
     >    EFFIC = ARRAY(3)    

      K1 = (IPH - 1) * 6 + INSTAR   
      K2 = K1 + 24
      K3 = INSTAR + 12  
      B0(K2) = 1.0 - (((1.0 - EFFIC)**0.1) / ((1.0 - R0(K3)) *    
     >         (1.0 - B0(K1))))     

      IF(LKECHO)WRITE(JOSTND,3750)KEYWRD,IPH,INSTAR,EFFIC,K2,B0(K2)  
 3750 FORMAT (/1X,A8,'   PHASE=',I2,'; INSTAR=',I2,' EFFICACY=',F6.2,   
     >        /T13,'MODEL PARAMETER B0(',I2,') IS NOW=',F10.4)    
      GO TO 10    

 3800 CONTINUE    
C     
C======================  OPTION NUMBER 28: DEBUTREE  ============ 
C     
      TMDTRE = .TRUE.   
      IF(LKECHO)WRITE(JOSTND,900) KEYWRD     
      GO TO 10    

 3900 CONTINUE    
C     
C======================  OPTION NUMBER 29  ====================== 
C     
      GO TO 10    

 4000 CONTINUE    
C     
C======================  OPTION NUMBER 30: PUNCH  =============== 
C     
      LPUNCH = .TRUE.   
      JOTMDK = IFIX(ARRAY(1)) 
      IF (JOTMDK .GT. 0) GOTO 4005  

      CALL KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)     
      CALL ERRGRO (.TRUE., 4) 
      LPUNCH = .FALSE.  
      GOTO 10     

 4005 CONTINUE    
      IF(LKECHO)WRITE(JOSTND,4010) KEYWRD, JOTMDK  
 4010 FORMAT (/1X,A8,'   DATA SET REFERENCE NUMBER=',I3)    
      GO TO 10    

 4100 CONTINUE    
C     
C======================  OPTION NUMBER 31  ====================== 
C     
      GO TO 10    

 4200 CONTINUE    
C     
C======================  OPTION NUMBER 32: BIOMASS  ============= 
C     
      I = IFIX(ARRAY(1))
      IF (I .GE. 1 .AND. I .LE. 4) IBMTYP = I   
      GO TO (4211,4212,4213,4214), IBMTYP 

 4211 IF(LKECHO)WRITE(JOSTND,4215) KEYWRD     
      GO TO 10    
 4212 IF(LKECHO)WRITE(JOSTND,4216) KEYWRD     
      GO TO 10    
 4213 IF(LKECHO)WRITE(JOSTND,4217) KEYWRD     
      GO TO 10    
 4214 IF(LKECHO)WRITE(JOSTND,4218) KEYWRD     
      GOTO 10     

 4215 FORMAT (/1X,A8,T13,'RANDOM BIOMASS ASSIGNMENT (METHOD 1)')  
 4216 FORMAT (/1X,A8,T13,'DETERMINISTIC ASSIGNMENT USING TREE',   
     >       ', SITE AND STAND VARIABLES (METHOD 2)') 
 4217 FORMAT (/1X,A8,T13,'DETERMINSTIC ASSIGNMENT USING BASAL',   
     >       ' AREA PERCENTILE AND SPECIES (METHOD 3)')     
 4218 FORMAT (/1X,A8,T13,'ASSIGNMENT USING BASAL AREA ',    
     >       'PERCENTILE AND SPECIES, WITH ADDITIVE RANDOM',
     >       ' VARIATION (METHOD 4)')     

 4300 CONTINUE    
C     
C======================  OPTION NUMBER 33: DFBIOMAS  ============ 
C     
      IF (LNOTBK(1)) DFFBIO(1) = ARRAY(1) 
      IF (LNOTBK(2)) DFFBIO(2) = ARRAY(2) 
      IF (LNOTBK(3)) DFPNEW(1) = ARRAY(3) 
      IF (LNOTBK(4)) DFPNEW(2) = ARRAY(4) 

      IF(LKECHO)WRITE(JOSTND,4310) KEYWRD, DFFBIO, DFPNEW
 4310 FORMAT(/1X,A8,T13,'MEAN BIOMASS =',F8.2,  
     >      '; STD DEV =',F7.2,'; MEAN %NEW FOLIAGE =',F7.2,
     >      '; STD DEV =',F7.2)     
      GO TO 10    

 4400 CONTINUE    
C     
C======================  OPTION NUMBER 34: GFBIOMAS  ============ 
C     
      IF (LNOTBK(1)) GFFBIO(1) = ARRAY(1) 
      IF (LNOTBK(2)) GFFBIO(2) = ARRAY(2) 
      IF (LNOTBK(3)) GFPNEW(1) = ARRAY(3) 
      IF (LNOTBK(4)) GFPNEW(2) = ARRAY(4) 
      IF(LKECHO)WRITE(JOSTND,4310) KEYWRD, GFFBIO, GFPNEW
      GO TO 10    

 4500 CONTINUE    
C     
C======================  OPTION NUMBER 35:  ===================== 
C     
      GO TO 10    

 4600 CONTINUE    
C     
C======================  OPTION NUMBER 36: RANSCHED  ============ 
C     
      ITMSCH = 2  
      IF (LNOTBK(1)) TMWAIT = IFIX(ARRAY(1))    
      IF (LNOTBK(2)) TMEVNT = ARRAY(2)    
      IF (LNOTBK(3)) TMPAST = IFIX(ARRAY(3))    

      IF(LKECHO)WRITE(JOSTND,4610) KEYWRD, TMWAIT, TMEVNT, TMPAST    
 4610 FORMAT (/1X,A8,'   REGIONAL OUTBREAKS AUTOMATICALLY ',
     >      'SCHEDULED.'/T13,'MINIMUM WAITING PERIOD IS',I5,
     >       ' YEARS; EVENT PROBABILITY IS ',F6.3 / T13,    
     >       'LAST RECORDED TUSSOCK MOTH OUTBREAK WAS IN YEAR: ',I5)    
      GO TO 10    

 4700 CONTINUE    
C     
C======================  OPTION NUMBER 37: MANSTART  ============ 
C     
      ITMETH = 1  
      IF(LKECHO)WRITE(JOSTND,4710) KEYWRD    
 4710 FORMAT (/1X,A8,'   STAND IS INCLUDED IN ALL REGIONAL OUTBREAKS.') 
      GO TO 10    

 4800 CONTINUE    
C     
C======================  OPTION NUMBER 38: ASHDEPTH  ============ 
C     
      IF (LNOTBK(1)) TMASHD = ARRAY(1)    
      IF(LKECHO)WRITE(JOSTND,4910) KEYWRD, TMASHD  
 4910 FORMAT (/1X,A8,'   SOIL ASH DEPTH IN INCHES =',F9.3)  
      GO TO 10    

 4900 CONTINUE    
C     
C======================  OPTION NUMBER 39:  ===================== 
C     
      GO TO 10    

 5000 CONTINUE    
C     
C======================  OPTION NUMBER 40: CRTSTART  ============ 
C     
      ITMETH = 2  
      IF(LKECHO)WRITE(JOSTND,900) KEYWRD     
      GO TO 10    

 5100 CONTINUE    
C     
C======================  OPTION NUMBER 41: RANSTART  ============ 
C     
      ITMETH = 3  
      IF(LKECHO)WRITE(JOSTND,5110) KEYWRD    
 5110 FORMAT (/1X,A8,'   STAND INCLUSION IN REGIONAL OUTBREAKS IS ',    
     >        'STOCHASTICALLY DETERMINED (SEE PROBMETH).')  
      GO TO 10    

 5200 CONTINUE    
C     
C======================  OPTION NUMBER 42: PROBMETH  ============ 
C     
      I = IFIX(ARRAY(1))
      IF (I .GT. 0 .AND. I .LE. 3) IPRBMT = I   
      IF (LNOTBK(2)) PRBSCL = ARRAY(2)    

      IF(LKECHO)WRITE(JOSTND,5230) KEYWRD     
 5230 FORMAT(/1X,A8,T13,'CONDITIONAL PROBABILITY OF STAND', 
     >    ' BEING INCLUDED IN A REGIONAL OUBREAK IS A FUNCTION OF:')    
      GO TO (5231,5232,5233), IPRBMT

 5231 IF(LKECHO)WRITE(JOSTND,5234)     
      GOTO 5250   
 5232 IF(LKECHO)WRITE(JOSTND,5235)     
      GOTO 5250   
 5233 IF(LKECHO)WRITE(JOSTND,5236)     

 5234 FORMAT (T13,'ELEV, SLOPE, ASPECT, TOPO, CROWN CLOSURE, CROWN ',   
     >       'WIDTH, AND %HOST (METHOD 1)')     
 5235 FORMAT (T13,'TOPO, BASAL AREA, %GRAND FIR, ASHDEPTH ',
     >      '(METHOD 2)')     
 5236 FORMAT (T13,'TOPO, BASAL AREA, %GRAND FIR (METHOD 3)')

 5250 CONTINUE    
      IF(LKECHO)WRITE(JOSTND,5260) PRBSCL    
 5260 FORMAT (T13,'PROBABILITY SCALING FACTOR =',F6.3)
      GO TO 10    

 5300 CONTINUE    
C     
C======================  OPTION NUMBER 43: TOPO  ================ 
C     
      IF (LNOTBK(1) .AND. ARRAY(1) .LE. 3.0) TOPO = ARRAY(1)
      IF(LKECHO)WRITE(JOSTND,5310) KEYWRD, TOPO    
 5310 FORMAT (/1X,A8,'   TOPOGRAPHIC POSITION CODE = ',     
     >        F6.2,';  1=RIDGETOP,2=SIDEHILL,3=BOTTOM.')    
      GO TO 10    

 5400 CONTINUE    
C     
C======================  OPTION NUMBER 44  ====================== 
C     
      GO TO 10    

 5500 CONTINUE    
C     
C======================  OPTION NUMBER 45: SALVAGE  ============= 
C     
      ITMSLV = 1  
      IF (LNOTBK(1)) TMDEFL = ARRAY(1)    
      IF(LKECHO)WRITE(JOSTND,5510) KEYWRD, TMDEFL  
 5510 FORMAT (/,' ',A8,'   SALVAGE SURVIVORS AFTER EVERY OUTBREAK.',    
     >        '  CRITICAL TREE DEFOLITION LEVEL = ',F5.1,'%')     
      GO TO 10    

 5600 CONTINUE    
C     
C======================  OPTION NUMBER 46:  ===================== 
C     
      GO TO 10    

 5700 CONTINUE    
C     
C======================  OPTION NUMBER 47:  ===================== 
C     
      GO TO 10    

 5800 CONTINUE    
C     
C======================  OPTION NUMBER 48:  ===================== 
C     
      GO TO 10    

 5900 CONTINUE    
C     
C======================  OPTION NUMBER 49:  ===================== 
C     
      GO TO 10    

 6000 CONTINUE    
C     
C======================  OPTION NUMBER 50:  ===================== 
C     
      GO TO 10    


      ENTRY TMOPS 
C-------------------------------    
C     
C-------------------------------    

      IF (.NOT. LFIRST) GOTO 110    
      ITMREP = 0  
      RETURN

  110 CONTINUE    
      LFIRST = .TRUE.   
      IF (IBMTYP .EQ. 2) LDUBDG = .TRUE.  

C     
C===  SCHEDULE THE OUTBREAKS  =================================== 
C     TMSCHD IS USED TO SCHEDULE OUTBREAKS
C     IF 'RANSCHED' WAS SPECIFIED OR TO INSURE A CYCLE EXISTS WITHIN    
C     2 YRS OF AN OUTBREAK IF DATES ARE USED ON THE MANSCHED OPTION.    
C     
      CALL TMSCHD 

      IF (ITMREP .LT. 2) RETURN     

      WRITE (JODFTM,1)  
    1 FORMAT (/1X,46('-'),'  DFTM OPTIONS AND INPUT TABLE  ',53('-')    
     >      //' KEYWORD    KEYWORD DISCRIPTION AND PARAMETER VALUES ',  
     >      'USED'/' --------   ',120('-')/)    
    2 FORMAT (/1X,A8)   

C     
C===  PROGRAM EXECUTION OPTIONS  ================================ 
C     
      IF (LPUNCH) WRITE (JODFTM,131) TABLE(30), JOTMDK
  131 FORMAT (/1X,A8,'   DATA SET REFERENCE NUMBER= ',I2)   

      IF (.NOT. LDF) WRITE (JODFTM,2) TABLE(04) 
      IF (.NOT. LGF) WRITE (JODFTM,2) TABLE(05) 

C     
C===  OUTBREAK TIMING OPTIONS.  SCHEDULING:  ==================== 
C     
      IF (ITMSCH .EQ. 1 ) WRITE (JODFTM,118) TABLE(06)
  118 FORMAT(/1X,A8,'   REGIONAL OUTBREAKS SCHEDULED ',     
     >      'BY USER.') 

      IF (ITMSCH .EQ. 2) WRITE (JODFTM,117) TABLE(36), TMWAIT,    
     >                   TMEVNT, TMPAST   
  117 FORMAT (/1X,A8,'   REGIONAL OUTBREAKS AUTOMATICALLY ',
     >      'SCHEDULED.'/T13,'MINIMUM WAITING PERIOD IS',I5,
     >       ' YEARS; EVENT PROBABILITY IS ',F6.3 / T13,    
     >       'LAST RECORDED TUSSOCK MOTH OUTBREAK WAS IN YEAR: ',I5)    

C     
C===  OUTBREAK TIMING OPTIONS.  STARTING:  ====================== 
C     
      GO TO (221,222,223), ITMETH   
  221 WRITE(JODFTM,224) TABLE(37)   
      GO TO 227   
  222 WRITE(JODFTM,2) TABLE(40)     
      GO TO 227   
  223 WRITE(JODFTM,226) TABLE(41)   
  224 FORMAT(/1X,A8,'   STAND IS INCLUDED IN ALL ',   
     >      'REGIONAL OUTBREAKS.')  
  226 FORMAT(/1X,A8,'   STAND INCLUSION IN REGIONAL ',
     >      'OUTBREAKS IS STOCHASTICALLY DETERMINED (SEE PROBMETH).')   

  227 CONTINUE    

C     
C===  OUTBREAK TIMING OPTIONS.  PROBABILITY CALCULATION:  ======= 
C     
      WRITE(JODFTM,230) TABLE(42)   
  230 FORMAT(/1X,A8,'   CONDITIONAL PROBABILITY OF STAND',  
     >    ' BEING INCLUDED IN A REGIONAL OUBREAK IS A FUNCTION OF:')    

      GO TO (231,232,233), IPRBMT   
  231 WRITE (JODFTM,5234)     
      GO TO 240   
  232 WRITE (JODFTM,5235)     
      GO TO 240   
  233 WRITE (JODFTM,5236)     

  240 CONTINUE    
      WRITE (JODFTM,241) PRBSCL     
  241 FORMAT(T13,'PROBABILITY SCALING FACTOR =',F6.3) 

      IF (IPRBMT .EQ. 1 .OR. TOPO .LE. 2.) GO TO 148  
      WRITE (JODFTM,147)
  147 FORMAT (/' ********   WARNING:  TOPO POSITION CODE WAS SET',
     >     ' TO 2; CODES GREATER THAN 2 ARE INVALID WHEN PROBMETH ',    
     >        'IS NOT EQUAL TO 1.') 
      TOPO = 2.0  

  148 CONTINUE    
      WRITE (JODFTM,150) TABLE(43), TOPO  
  150 FORMAT (/1X,A8,'   TOPOGRAPHIC POSITION CODE = ',     
     >        F6.2,';  1=RIDGETOP,2=SIDEHILL,3=BOTTOM.')    
      WRITE (JODFTM,155) TABLE(38), TMASHD
  155 FORMAT (/1X,A8,'   SOIL ASH DEPTH IN INCHES = ',F9.3) 

C     
C===  OUTBREAK INITIAL CONDITIONS.  EGG ALLOCATION:  ============ 
C     
      DO 204 ISP=1,2    
        IF (IEGTYP .EQ. 2) GOTO 203 
        WRITE (JODFTM,2130) TABLE(11), ISP, SPEC(ISP),
     >                      (REGG(3 * ISP + I - 3), I=1,3)  
        GOTO 204  
  203   CONTINUE  
        WRITE (JODFTM,2230) TABLE(12), ISP, SPEC(ISP),
     >                    (DEGG(3 * ISP + I - 3), I=1,3)    
  204 CONTINUE    

  140 CONTINUE    
C     
C===  OUTBREAK INITIAL CONDITIONS.  BIOMASS ALLOCATION:  ======== 
C     
      GO TO (211,212,213,214), IBMTYP     
  211 WRITE (JODFTM,215) TABLE(32)  
      WRITE (JODFTM,220) TABLE(33), DFFBIO, DFPNEW    
      WRITE (JODFTM,220) TABLE(34), GFFBIO, GFPNEW    
      GO TO 219   
  212 WRITE (JODFTM,216) TABLE(32)  
      GO TO 219   
  213 WRITE (JODFTM,217) TABLE(32)  
      GO TO 219   
  214 WRITE (JODFTM,218) TABLE(32)  

  215 FORMAT (/1X,A8,'   RANDOM BIOMASS ASSIGNMENT (METHOD 1)')   
  216 FORMAT (/1X,A8,'   DETERMINISTIC ASSIGNMENT USING TREE',    
     >       ', SITE AND STAND VARIABLES (METHOD 2)') 
  217 FORMAT (/1X,A8,'   DETERMINSTIC ASSIGNMENT USING BASAL',    
     >       ' AREA PERCENTILE AND SPECIES (METHOD 3)')     
  218 FORMAT (/1X,A8,'   ASSIGNMENT USING BASAL AREA ',     
     >       'PERCENTILE AND SPECIES, WITH ADDITIVE RANDOM',
     >       ' VARIATION (METHOD 4)')     
  220 FORMAT (/1X,A8,10X,A3,T13,'MEAN BIOMASS =',F8.2,
     >       '; STD DEV =',F7.2,'; MEAN %NEW FOLIAGE =',F7.2,     
     >       '; STD DEV =',F7.2)    

  219 CONTINUE    

C     
C===  OUTBREAK INITIAL CONDITIONS.  NUMCLASS, WEIGHT, REDIST:  == 
C     
      WRITE (JODFTM,134) TABLE(10), NCLAS, TMPN1
  134 FORMAT (/1X,A8,'   NUMBER OF REQUESTED CLASSES OF ',  
     >       'DOUGLAS-FIR= ',I3,'; GRAND FIR=',I3/T13,'PROPORTION OF ', 
     >       'CLASSES DEFINED BY FINDING DIFFERENCES BETWEEN',    
     >       ' TREES = ',F5.2/T13,'THE REMAINING CLASSES ARE FOUND',    
     >       ' BY HALVING THE CLASSES WITH THE MOST TREE RECORDS.')     

      WRITE (JODFTM,135) TABLE(09), (WEIGHT(I),I=1,2) 
  135 FORMAT (/1X,A8,'   THE CLASSIFICATION WEIGHTING ',    
     >       'FACTORS ARE: ',F6.2,' FOR % NEW FOLIAGE, AND ',     
     >       F6.2,' FOR FOLIAGE BIOMASS.')

      IF (.NOT. LREDIS) WRITE (JODFTM,2) TABLE(15)    
      IF (LREDIS) WRITE (JODFTM,144) TABLE(14), B0(62)

  144 FORMAT (/1X,A8,'   ANNUAL TUSSOCK MOTH REDISTRIBUTION RATE =',    
     >        F6.2)     

C     
C===  CONTROL AND SALVAGE OPTIONS:  ============================= 
C     
  170 FORMAT (/1X,A8,'   OUTBREAK MODEL PARAMETER VALUES: ',
     >        6F10.4)   

      IF (LNPV2) WRITE (JODFTM,170) TABLE(20), (B0(I),I=9,12)     
      IF (LNPV3) WRITE (JODFTM,170) TABLE(21), (B0(I),I=15,18)    
      IF (ITMSLV .GT. 0) WRITE (JODFTM,180) TABLE(45), TMDEFL     

  180 FORMAT (/1X,A8,'   SALVAGE SURVIVORS AFTER EVERY OUTBREAK.',
     >       '  CRITICAL TREE DEFOLITION LEVEL = ',F5.1,'%')
      IF (LTMPRM) WRITE (JODFTM,190) TABLE(26)  
  190 FORMAT (/1X,A8,'   AT LEAST 1 DFTM SUB-MODEL PARAMETER WAS ',     
     >       'REDEFINED.  SEE: OPTIONS SELECTED BY INPUT.') 
      IF (LCHEM) WRITE (JODFTM,190) TABLE(27)   
      IF (LRANSD) WRITE (JODFTM,250) TABLE(08), RSEED
C     IF (LRANSD) WRITE (JODFTM,250) TABLE(08), IR1, IR2, IR3     
  250 FORMAT (/1X,A8,'   DFTM RANDOM NUMBER GENERATOR ',    
     &        'WAS RESEEDED: ',F10.0)

      WRITE (JODFTM,4)  
    4 FORMAT (/,1X,131('-'))  

      RETURN

C     
C===  SPECIAL ENTRY TO RETURN A KEYWORD TO SUBROUTINE OPLIST.  ===
C     
      ENTRY TMKEY(KEY,PASKEY) 
      PASKEY = TABLE(KEY)     

      RETURN
      END   
