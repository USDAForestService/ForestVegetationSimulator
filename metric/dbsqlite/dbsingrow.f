      SUBROUTINE DBSINGROW(IYEAR,INGROW,CSP,TPAIN)
      IMPLICIT NONE
C----------
C METRIC-DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
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
      INCLUDE 'METRIC.F77'
C
COMMONS
C
      INTEGER ColNumber,I,iret,IYEAR,IYEAR1
      REAL INGROW,TPAIN
      DOUBLE PRECISION INGROW1,TPAIN1
      CHARACTER*2000 SQLStmtStr
      CHARACTER*2 CSP
      CHARACTER*8 CSP1,CSP2,CSP3
C
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS 
#endif

COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_bind_double,fsql3_finalize,
     >        fsql3_bind_text

      IF(IREG5.NE.1) RETURN
C
      CALL DBSCASE(1)
      
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,
     -  'FVS_Regen_Ingrow_Metric'//CHAR(0))
C
C       SITE PREP SUMMARY
C                  
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_Ingrow_Metric('//
     -              'CaseID           text not null,'//
     -              'StandID          text not null,'//
     -              'Year             int  null,'//
     -              'IngrowthTotalTph real null,'//
     -              'SpeciesFVS       text null,'//
     -              'SpeciesPLANTS    text null,'//
     -              'SpeciesFIA       text null,'//
     -              'IngrowthTph      int  null);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG5 = 0
        RETURN
      ENDIF
      ENDIF                
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow_Metric',
     -     ' (CaseID,StandID,Year,IngrowthTotalTph,',
     -     ' SpeciesFVS,SpeciesPLANTS,SpeciesFIA,IngrowthTph)',
     -     ' VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'
          IF (iRet .NE. 0) THEN
            IREG5 = 0                                    
            RETURN
          ENDIF

C     ASSIGN FVS, PLANTS AND FIA SPECIES CODE
C
      DO I = 1,MAXSP
        IF (CSP .EQ. JSP(I)) THEN
          CSP1 = JSP(I)
          CSP2 = PLNJSP(I)
          CSP3 = FIAJSP(2)
        ENDIF
      ENDDO          
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
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,
     >                                  LEN_TRIM(CSP1))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,
     >                                  LEN_TRIM(CSP2))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,
     >                                  LEN_TRIM(CSP3))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,TPAIN1)
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)        
        IF (iRet.ne.0) then
          IREG5 = 0 
        ENDIF
        RETURN          
      END