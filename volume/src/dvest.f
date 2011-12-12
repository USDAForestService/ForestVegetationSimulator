!== last modified 09-19-2006
C     SUBROUTINE FINDS VOLUMES USING DIRECT VOLUME ESTIMATORS
      SUBROUTINE DVEST(VOLEQ,DBHOB,DRC,HTTOT,MTOPP,FCLASS,HTLOG,HT1PRD,
     >           HT2PRD,FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,CDPFLG,
     >           SPFLG,PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,SI,
     >           CTYPE,ERRFLAG)
      
      CHARACTER*10 VOLEQ
      CHARACTER*1 HTTYPE,LIVE,CTYPE
      CHARACTER*2 FORST,PROD
      
      INTEGER FCLASS,UNIT,HTLOG,BA,SI
      INTEGER CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
      integer HTTFLL,ERRFLAG
      REAL DBHOB,HTTOT,VOL(15),HT1PRD,HT2PRD,NOLOGP,BTR
      REAL MTOPP,STUMP,BDVOL,CUVOL,TCVOL,DRC

      DO 100, I=1,15
        VOL(I) = 0.0
 100  CONTINUE

C     DIRECT VOLUME ESTIMATOR LOGIC
      IF(VOLEQ(1:1).EQ.'1') THEN
C*****************************
C      REGION 1 D2H ROUTINES *
C*****************************
         IF (VOLEQ(2:3).EQ.'02' .OR. VOLEQ(2:3).EQ.'03' .OR.
     >           VOLEQ(2:3).EQ.'04' .OR. VOLEQ(2:3).EQ.'05' .OR.
     >           VOLEQ(2:3).EQ.'06') THEN
c        KEMP
            CALL R1KEMP(VOLEQ,HTTOT,DBHOB,VOL,LIVE,PROD,NOLOGP,ERRFLAG)
         ENDIF
C     BOARD FOOT KEMP
         IF (VOLEQ(2:3).EQ.'01' .AND. BFPFLG.EQ.1) THEN
           IF((VOLEQ(8:10).EQ.'108' .AND. DBHOB.GE.6) 
     >                                          .OR. DBHOB.GE.7)THEN
              STUMP=1.0
              CALL R1ALLENB (VOLEQ,DBHOB,HTTOT,MTOPP,BTR,BDVOL,STUMP,
     >                      ERRFLAG)
              VOL(2) = BDVOL 
           ELSE
              VOL(2) = 0.0
           ENDIF
        ENDIF
C     CUBIC  KEMP
        IF(VOLEQ(2:3).EQ.'01' .AND. (CUPFLG.EQ.1 .or. CUTFLG.EQ.1))THEN
           STUMP = 1.0
c          byrne
           CALL R1ALLENC(VOLEQ,DBHOB,HTTOT,MTOPP,BTR,CUVOL,TCVOL,STUMP,
     >                   ERRFLAG)
           VOL(1) = TCVOL
           VOL(4) = CUVOL
        ENDIF

        IF(VOLEQ(2:3).EQ.'07')THEN
           STUMP = 1.0
        
        ENDIF    

      ELSEIF(VOLEQ(1:1).EQ.'2') THEN
C*****************************
C      REGION 2 D2H ROUTINES *
C*****************************
         CALL R2OLDV(VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG)
          
      ELSEIF(VOLEQ(1:1).EQ.'3') THEN
C*****************************
C      REGION 3 D2H ROUTINES * 
C*****************************
C--------- SAWTIMBER TREES
        IF (PROD.EQ.'01') THEN
           UNIT = 1
           CALL R3D2HV (VOLEQ,UNIT,HTTOT,HT1PRD,DBHOB,DRC,FCLASS,HTTFLL,
     >                  VOL,ERRFLAG)  
C-------------- PULPWOOD TREES
        ELSE
           UNIT = 3
           CALL R3D2HV (VOLEQ,UNIT,HTTOT,HT1PRD,DBHOB,DRC,FCLASS,HTTFLL,
     >                  VOL,ERRFLAG)
        ENDIF
      ELSEIF(VOLEQ(1:1).EQ.'4') THEN
C*****************************
C      REGION 4 D2H ROUTINES * 
C*****************************
         CALL R4D2H (VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG)

      ELSEIF(VOLEQ(1:1).EQ.'5') THEN
C*****************************
C      REGION 5 D2H ROUTINES * 
C*****************************
         CALL R5HARV (VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,ERRFLAG)
          
      ELSEIF(VOLEQ(1:1).EQ.'9') THEN
C*****************************
C      REGION 9 D2H ROUTINES * 
C*****************************
         CALL R9VOL(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,FORST,SI,BA,
     *            PROD,CTYPE,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG)
      
      ELSEIF(VOLEQ(1:1).EQ.'A' .or. VOLEQ(1:1).EQ.'a') THEN
C*****************************
C      REGION 10 D2H ROUTINES *
C*****************************

          CALL R10D2H(VOLEQ,DBHOB,HTTOT,VOL,CUTFLG,CUPFLG,BFPFLG,
     >                                                         ERRFLAG)

      ELSEIF(VOLEQ(1:1).EQ.'M' .or. voleq(1:1).eq.'m') THEN
C******************************
C      ARMY BASE D2H ROUTINES * 
C******************************
        IF(HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
          IF(VOLEQ(1:3).EQ.'M01' .OR. VOLEQ(1:3).eq.'m01') THEN
            CALL DOYAL78(DBHOB,HT1PRD,VOL,ERRFLAG)
          ELSEIF(VOLEQ(1:3).EQ.'M02' .or. VOLEQ(1:3).eq.'m02') THEN
            CALL INTL78(DBHOB,HT1PRD,VOL,ERRFLAG)
          ENDIF
        ELSE
          ERRFLAG = 7
        ENDIF
      
      ENDIF
      
C     ROUND ALL VOLUMES 10TH CUBIC AND CORD, NEAREST BDFT
      VOL(1) =  ANINT(VOL(1)*10.0)/10.0
	VOL(2) = ANINT(VOL(2))
	VOL(4) = ANINT(VOL(4)*10.0)/10.0
      VOL(6) = ANINT(VOL(6)*1000.0)/1000.0
	VOL(7) = ANINT(VOL(7)*10.0)/10.0
	VOL(9) = ANINT(VOL(9)*1000.0)/1000.0
	VOL(10) = ANINT(VOL(10))

      RETURN
      END
                                                                     
