      SUBROUTINE DBSSITEPREP(NOYEAR,MECHYEAR,BURNYEAR,PCTNONE,
     & PCTMECH,PCTBURN)
      IMPLICIT NONE
C----------
C DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              SITE PREP SUMMARY OUTPUT 
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
      INTEGER ColNumber,iret,NOYEAR,MECHYEAR,BURNYEAR
      INTEGER PCTNONE,PCTMECH,PCTBURN
      CHARACTER*2000 SQLStmtStr
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_finalize

      IF(IREG2.NE.1) RETURN
C
      CALL DBSCASE(1)   
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_SitePrep'//CHAR(0))
C
C       SITE PREP SUMMARY
C                  
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_SitePrep('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'YearNone int null,'//
     -              'YearMech int null,'//
     -              'YearBurn int null,'//
     -              'PcntNone int null,'//
     -              'PcntMech int null,'//
     -              'PcntBurn int null);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG2 = 0
        RETURN
      ENDIF
      ENDIF                
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_SitePrep',
     -         ' (CaseID,StandID,YearNone,YearMech,YearBurn,',
     -         'PcntNone,PcntMech,PcntBurn)',
     -         'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'
     
          
        iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            IREG2 = 0                                    
            RETURN
          ENDIF                   

        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,NOYEAR)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,MECHYEAR)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,BURNYEAR)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PCTNONE)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PCTMECH)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,PCTBURN)                             
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)        
        IF (iRet.ne.0) then
          IREG2 = 0 
        ENDIF
        RETURN          
      END



