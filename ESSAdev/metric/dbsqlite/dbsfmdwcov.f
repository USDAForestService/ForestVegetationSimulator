      SUBROUTINE DBSFMDWCOV(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C METRIC-DBSQLITE $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD COVER REPORT
C              INFORMATION
C     AUTH: S. REBAIN
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C
C       DBH BREAKPOINGS IMMEDIATELY BELOW ARE INCHES; THESE ARE
C       CONVERTED TO CM IN THE TABLE; e.g. "3to6" becomes "76to152",
C       DROPPING THE DECIMAL POINT IN THE METRIC VERSION
C
C       UNITS ARE % COVER
C         1 = DOWN WOOD 3 - 6
C         2 = DOWN WOOD 6 - 12 HARD
C         3 = DOWN WOOD 12 - 20 HARD
C         4 = DOWN WOOD 20 - 35 HARD
C         5 = DOWN WOOD 35 - 50 HARD
C         6 = DOWN WOOD 50+ HARD
C         7 = DOWN WOOD TOTAL HARD
C         8 = DOWN WOOD 3 - 6 SOFT
C         9 = DOWN WOOD 6 - 12 SOFT
C         10 = DOWN WOOD 12 - 20 SOFT
C         11 = DOWN WOOD 20 - 35 SOFT
C         12 = DOWN WOOD 35 - 50 SOFT
C         13 = DOWN WOOD 50+ SOFT
C         14 = DOWN WOOD TOTAL SOFT
C       KODE   - RETURN CODE

COMMONS
C
      INCLUDE 'DBSCOM.F77'
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

COMMONS

      INTEGER IYEAR,KODE,VARDIM,iRet
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER I,ColNumber
      CHARACTER*2000    SQLStmtStr

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      IF(IDWDCOV.EQ.0) RETURN
      IF(IDWDCOV.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)
      iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Down_Wood_Cov_Metric"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Down_Wood_Cov_Metric ('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Year Int null,'//
     -              'DWD_Cover_76to152_Hard real null,'//
     -              'DWD_Cover_152to305_Hard real null,'//
     -              'DWD_Cover_305to508_Hard real null,'//
     -              'DWD_Cover_508to889_Hard real null,'//
     -              'DWD_Cover_889to1270_Hard real null,'//
     -              'DWD_Cover_ge_1270_Hard real null,'//
     -              'DWD_Cover_Total_Hard real null,'//
     -              'DWD_Cover_76to152_Soft real null,'//
     -              'DWD_Cover_152to305_Soft real null,'//
     -              'DWD_Cover_305to508_Soft real null,'//
     -              'DWD_Cover_508to889_Soft real null,'//
     -              'DWD_Cover_889to1270_Soft real null,'//
     -              'DWD_Cover_ge_1270_Soft real null,'//
     -              'DWD_Cover_Total_Soft real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          IDWDCOV = 0
          RETURN
        ENDIF
      ENDIF
      
      VARD = VAR

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Down_Wood_Cov_Metric',
     >  ' (CaseID,StandID,Year,DWD_Cover_76to152_Hard,',
     >  'DWD_Cover_152to305_Hard,DWD_Cover_305to508_Hard,',
     >  'DWD_Cover_508to889_Hard,DWD_Cover_889to1270_Hard,',
     >  'DWD_Cover_ge_1270_Hard,DWD_Cover_Total_Hard,',
     >  'DWD_Cover_76to152_Soft,',
     >  'DWD_Cover_152to305_Soft,DWD_Cover_305to508_Soft,',
     >  'DWD_Cover_508to889_Soft,DWD_Cover_889to1270_Soft,',
     >  'DWD_Cover_ge_1270_Soft,DWD_Cover_Total_Soft) VALUES (''',
     >  CASEID,''',''',TRIM(NPLT),
     >   ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

      iRet = fsql3_prepare(IoutDBref,SQLStmtStr)

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      DO I=1,VARDIM
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
      ENDDO

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IDWDCOV = 0
      ENDIF
      iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
      RETURN
      END
