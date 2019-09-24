      SUBROUTINE DBSINGROW(IYEAR,INGROW,SPECCD,TPAIN)
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
      INTEGER ColNumber,iret,IYEAR,IYEAR1
      REAL INGROW,TPAIN
      DOUBLE PRECISION INGROW1,TPAIN1
      CHARACTER*2000 SQLStmtStr
      CHARACTER*2 SPECCD      
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_bind_text

      IF(IREG5.NE.1) RETURN
C
      CALL DBSCASE(1)
      
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Ingrow'//CHAR(0))
C
C       SITE PREP SUMMARY
C                  
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_Ingrow('//
     -              'CaseID char(36) null,'//
     -              'StandID char(26) null,'//
     -              'Year int null,'//
     -              'IngrowthTotalTpa real null,'//
     -              'Species Char(2) null,'//
     -              'IngrowthTpa int null);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG5 = 0
        RETURN
      ENDIF
      ENDIF                
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow',
     -         ' (CaseID,StandID,Year,IngrowthTotalTpa,Species,',
     -         'IngrowthTpa)',
     -         'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?);'    
          
        iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            IREG5 = 0                                    
            RETURN
          ENDIF
          
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C        

        INGROW1=INGROW
        TPAIN1=TPAIN
        IYEAR1=IYEAR

        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,INGROW1) 
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,SPECCD,
     >                                      LEN_TRIM(SPECCD))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,TPAIN1)                     
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)        
        IF (iRet.ne.0) then
          IREG5 = 0 
        ENDIF
        RETURN          
      END



