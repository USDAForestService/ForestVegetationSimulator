      SUBROUTINE DBSSPRT(SPECCD,TPASUM,HTAVE,TPATOT,
     &  WTHTAVE,TOT,IYEAR)
      IMPLICIT NONE
C----------
C DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
C----------C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              SPROUTING OUTPUT 
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
      INTEGER ColNumber,iret,TOT,IYEAR,TPASUM1,IYEAR1
      REAL TPASUM,HTAVE,TPATOT,WTHTAVE
      DOUBLE PRECISION HTAVE1
      CHARACTER*2000 SQLStmtStr
      CHARACTER*4 SPECCD
C
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_bind_text

      IF(IREG1.NE.1) RETURN
      
      CALL DBSCASE(1) 
      
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Sprouts'//CHAR(0))
C
C       REGENERATION FROM STUMP & ROOT SPROUTS
C         
         
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_Sprouts('//
     -              'CaseID Char(36) null,'//
     -              'StandID Char(26) null,'//
     -              'Year int null,'//
     -              'Species text null,'//
     -              'SprtTpa int,'//
     -              'SprtAveHt real);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG1 = 0
        RETURN
      ENDIF
      ENDIF                
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Sprouts',
     -        ' (CaseID,StandID,Year,',
     -        'Species,SprtTpa,SprtAveHt)',
     -        'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?);'     
          
        iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            IREG1 = 0                                    
            RETURN
          ENDIF                   
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C        
        IF(TOT.EQ.1)THEN
        TPASUM1=NINT(TPASUM)
        HTAVE1=HTAVE
        ELSE
        TPASUM1=NINT(TPATOT)
        HTAVE1=WTHTAVE
        ENDIF
        IYEAR1=IYEAR+1
        
        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,SPECCD,
     >                                      LEN_TRIM(SPECCD))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,TPASUM1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,HTAVE1)                         
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)        
        IF (iRet.ne.0) then
          IREG1 = 0 
        ENDIF
        RETURN                    
      END



