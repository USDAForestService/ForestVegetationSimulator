      SUBROUTINE DBSCLSUM(CNPLT,IYR,SP,SPVIAB,
     >          SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >          SPSITGM,MXDENMLT,POTESTAB)
      IMPLICIT NONE
C
C METRIC-DBSQLITE $Id$
C
C     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'PLOT.F77'      

      INCLUDE 'DBSCOM.F77'      

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
      INTEGER I,IYR,IRCODE
      CHARACTER(LEN=*) CNPLT,SP
      REAL SPVIAB,SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >     SPSITGM,MXDENMLT,POTESTAB

      CHARACTER*8    CSP1,CSP2,CSP3
      CHARACTER*2000 SQLStmtStr
      
      INTEGER fsql3_tableexists,fsql3_exec
      IF (ICLIM.EQ.0) RETURN

C     MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

      IRCODE = fsql3_tableexists(IoutDBref,
     -  "FVS_Climate_Metric"//CHAR(0))
      IF(IRCODE.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Climate_Metric('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Year Int null,'//
     -              'SpeciesFVS    text null,'//
     -              'SpeciesPLANTS text null,'//
     -              'SpeciesFIA    text null,'//
     -              'Viability real null,'//
     -              'BA real null,'//
     -              'TPH real null,'//
     -              'ViabMort real null,'//
     -              'dClimMort real null,'//
     -              'GrowthMult real null,'//
     -              'SiteMult real null,'//
     -              'MxDenMult real null,'//
     -              'AutoEstbTPH real null)'//CHAR(0)
        IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (IRCODE .NE. 0) THEN
          ICLIM = 0
          RETURN
        ENDIF
      ENDIF

C     ASSIGN FVS, PLANTS AND FIA SPECIES CODES
C
      DO I = 1,MAXSP
        IF (SP .EQ. JSP(I)) THEN
          CSP1 = JSP(I)
          CSP2 = PLNJSP(I)
          CSP3 = FIAJSP(I)
        ENDIF
      ENDDO

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Climate (CaseID,'//
     -   'StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,'//
     -   'Viability,BA,TPH,ViabMort,'//
     -   'dClimMort,GrowthMult,SiteMult,MxDenMult '//
     -   'VALUES(''',CASEID,''',''',TRIM(CNPLT),''',',IYR,',',
     -   '''',TRIM(CSP1),''',',
     -   '''',TRIM(CSP2),''',',
     -   '''',TRIM(CSP3),''',',
     -   SPVIAB,',',SPBA,',',SPTPA,',',
     -   SPMORT1,',',SPMORT2,',',SPGMULT,',',
     -   SPSITGM,',',MXDENMLT,',',POTESTAB,')'

      IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (IRCODE .NE. 0) ICLIM = 0
      RETURN
      END


