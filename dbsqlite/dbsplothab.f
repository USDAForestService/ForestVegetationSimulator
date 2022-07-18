      SUBROUTINE DBSPLOTHAB(NUMPTS,HABTYP,SERIES,M)
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

C
COMMONS
C
      INTEGER ColNumber,iret,M,NUMPTS
      CHARACTER*2000 SQLStmtStr
      CHARACTER*47 HABTYP
      CHARACTER*7 SERIES
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_finalize,
     >        fsql3_bind_text

      IF(IREG3.NE.1) RETURN
C
      CALL DBSCASE(1)
C
C     DEFINE TABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_HabType'//CHAR(0))
C
C       SITE PREP SUMMARY
C
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_HabType('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Series text null,'//
     -              'GroupNum int null,'//
     -              'HabitatType text null,'//
     -              'NumPlots int null);'//CHAR(0)
     
      iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        IREG3 = 0
        RETURN
      ENDIF
      ENDIF
          
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_HabType',
     -         ' (CaseID,StandID,Series,GroupNum,HabitatType,',
     -         'NumPlots)',
     -         'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?);'    
          
        iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            IREG3 = 0
            RETURN
          ENDIF
          
        ColNumber=1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,SERIES,
     >                                      LEN_TRIM(SERIES))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,M)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,HABTYP,
     >                                      LEN_TRIM(HABTYP))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,NUMPTS)
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_finalize(IoutDBref)
        IF (iRet.ne.0) then
          IREG3 = 0
        RETURN
        ENDIF
      END
