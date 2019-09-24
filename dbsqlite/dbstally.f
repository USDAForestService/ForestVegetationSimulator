      SUBROUTINE DBSTALLY(PRBSTK,NTALLY,SPECCD,SUMTPA,SUMPCT,BESTTPA,
     & BESTPCT,AVEHT,PASTPA,PASPCT,IYEAR,WTAVEHT,TOT,TOTTTPAA,
     & TOTTTPAB,TOTTTPA3)
      IMPLICIT NONE
C----------
C DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              PLOT HABITAT TYPE SUMMARY OUTPUT 
C     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
      INCLUDE 'PLOT.F77'
C
COMMONS
C
      INTEGER ColNumber,iret,NTALLY,SUMTPA1,SUMPCT1,BESTTPA1
      INTEGER PASTPA1,PASPCT1,BESTPCT1,IYEAR,TOT,IYEAR1
      REAL PRBSTK,AVEHT,WTAVEHT,TOTTTPAA,TOTTTPAB,TOTTTPA3
      REAL SUMTPA,SUMPCT,BESTTPA,PASTPA,PASPCT,BESTPCT
      DOUBLE PRECISION PRBSTK1,AVEHT1
      CHARACTER*2000 SQLStmtStr
      CHARACTER*3 SPECCD      
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_bind_text

      IF(IREG4.NE.1) RETURN
C
      CALL DBSCASE(1)
      
      IF(PRBSTK.GT.1) GOTO 100
      
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Tally'//CHAR(0))
C
C       SITE PREP SUMMARY
C                  
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_Tally('//
     -              'CaseID char(36) null,'//
     -              'StandID char(26) null,'//
     -              'Year int null,'//
     -              'ProbStock real null,'//
     -              'TallyNumber int null,'//
     -              'Species Char(2) null,'//
     -              'TpaAll int null,'//
     -              'PctOfTotalAll int null,'//
     -              'TpaBest int null,'//
     -              'PctOfTotalBest int null,'//
     -              'AverageHt real null,'//
     -              'TpaLt3in int null,'//
     -              'PctOfTotalLt3in int null);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG4 = 0
        RETURN
      ENDIF
      ENDIF                
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Tally',
     -         ' (CaseID,StandID,Year,ProbStock,TallyNumber,',
     -         'Species,TpaAll,PctOfTotalAll,TpaBest,',
     -         'PctOfTotalBest,AverageHt,TpaLt3in,PctOfTotalLt3in)',
     -         'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,',
     -         '?,?,?,?,?,?,?);'    
          
        iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            IREG4 = 0                                    
            RETURN
          ENDIF
          
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C        

        PRBSTK1=PRBSTK
        IF(TOT.EQ.1)THEN
        AVEHT1=AVEHT
        ELSE
        AVEHT1=WTAVEHT
        ENDIF
C
C     ASSIGN INTEGER VALUES TO REAL VARS
C 
        
        IF(TOT.EQ.1)THEN
        SUMTPA1=NINT(SUMTPA)
        BESTTPA1=NINT(BESTTPA)
        PASTPA1=NINT(PASTPA)
        PASPCT1=NINT(PASPCT)
        BESTPCT1=NINT(BESTPCT)
        SUMPCT1=NINT(SUMPCT) 
        ELSE
        SUMTPA1=NINT(TOTTTPAA)
        BESTTPA1=NINT(TOTTTPAB)
        PASTPA1=NINT(TOTTTPA3)
        PASPCT1=100
        BESTPCT1=100
        SUMPCT1=100
        ENDIF     
        IYEAR1=IYEAR+1                                          
                          
        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,PRBSTK1) 
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,NTALLY)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,SPECCD,
     >                                      LEN_TRIM(SPECCD))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMTPA1) 
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMPCT1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTTPA1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTPCT1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,AVEHT1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PASTPA1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PASPCT1)                       
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)        
        IF (iRet.ne.0) then
          IREG4 = 0 
        ENDIF
        RETURN 
  100 CONTINUE
  
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Tally'//CHAR(0))
C
C       SITE PREP SUMMARY
C                  
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_Tally('//
     -              'CaseID char(36) null,'//
     -              'StandID char(26) null,'//
     -              'Year int null,'//
     -              'TallyNumber int null,'//
     -              'Species Char(3) null,'//
     -              'TpaAll int null,'//
     -              'PctOfTotalAll int null,'//
     -              'TpaBest int null,'//
     -              'PctOfTotalBest int null,'//
     -              'AverageHt real null,'//
     -              'TpaLt3in int null,'//
     -              'PctOfTotalLt3in int null);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG4 = 0
        RETURN
      ENDIF
      ENDIF                
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Tally',
     -         ' (CaseID,StandID,Year,TallyNumber,',
     -         'Species,TpaAll,PctOfTotalAll,TpaBest,',
     -         'PctOfTotalBest,AverageHt,TpaLt3in,PctOfTotalLt3in)',
     -         'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',
     -         '?,?,?,?,?,?,?);'    
          
        iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            IREG4 = 0                                    
            RETURN
          ENDIF
          
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C        
        IF(TOT.EQ.1)THEN
        AVEHT1=AVEHT
        ELSE
        AVEHT1=WTAVEHT
        ENDIF
C
C     ASSIGN INTEGER VALUES TO REAL VARS
C        
        IF(TOT.EQ.1)THEN
        SUMTPA1=NINT(SUMTPA)
        BESTTPA1=NINT(BESTTPA)
        PASTPA1=NINT(PASTPA)
        PASPCT1=NINT(PASPCT)
        BESTPCT1=NINT(BESTPCT)
        SUMPCT1=NINT(SUMPCT) 
        ELSE
        SUMTPA1=NINT(TOTTTPAA)
        BESTTPA1=NINT(TOTTTPAB)
        PASTPA1=NINT(TOTTTPA3)
        PASPCT1=100
        BESTPCT1=100
        SUMPCT1=100
        ENDIF   
        IYEAR1=IYEAR+1           
                          
        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,NTALLY)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,SPECCD,
     >                                      LEN_TRIM(SPECCD))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMTPA1) 
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMPCT1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTTPA1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTPCT1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,AVEHT1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PASTPA1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PASPCT1)                       
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)        
        IF (iRet.ne.0) then
          IREG4 = 0 
        ENDIF 
        
                 
      END



