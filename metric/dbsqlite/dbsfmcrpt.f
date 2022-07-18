      SUBROUTINE DBSFMCRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE MAIN CARBON REPORT
C              INFORMATION
C     AUTH: D. ROBINSON - ESSA
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C       UNITS ARE TONS/ACRE IF USER HAS REQUESTED IMPERIAL UNITS; TONNES/HA IF METRIC
C         1 = ABOVEGROUND TOTAL LIVE
C         2 = ABOVEGOURND MERCH LIVE
C         3 = BELOWGROUND LIVE
C         4 = BELOWGROUND DEAD (WHICH DECAYS)
C         5 = STANDING DEAD
C         6 = FOREST DOWN DEAD WOOD
C         7 = FOREST FLOOR (SOILS?)
C         8 = FOREST SHRUB+HERB
C         9 = TOTAL STAND CARBON
C        10 = TOTAL CARBON REMOVED THIS REPORTING PERIOD
C        11 = CARBON RELEASED THRU FIRE
C       KODE   - RETURN CODE
C
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

      INTEGER IYEAR, KODE,VARDIM
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           iRet,I,ColNumber
      CHARACTER*2000    SQLStmtStr

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

C     Initialize variables

      IF(ICMRPT.EQ.0) RETURN
      IF(ICMRPT.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     INSURE MAIN CARBON TABLE EXISTS IN DATBASE

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Carbon_Metric"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Carbon_Metric('//
     -         'CaseID text not null,'//
     -         'StandID text not null,'//
     -         'Year Int null,'//
     -         'Aboveground_Total_Live real null,'//
     -         'Aboveground_Merch_Live real null,'//
     -         'Belowground_Live real null,'//
     -         'Belowground_Dead real null,'//
     -         'Standing_Dead real null,'//
     -         'Forest_Down_Dead_Wood real null,'//
     -         'Forest_Floor real null,'//
     -         'Forest_Shrub_Herb real null,'//
     -         'Total_Stand_Carbon real null,'//
     -         'Total_Removed_Carbon real null,'//
     -         'Carbon_Released_From_Fire real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ICMRPT = 0
          RETURN
        ENDIF
      ENDIF

C     COPY INPUT VECTOR TO DOUBLE-PRECISION

      VARD = VAR

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Carbon_Metric(CaseID,',
     >  'StandID,Year,Aboveground_Total_Live,Aboveground_Merch_Live,',
     >  'Belowground_Live,Belowground_Dead,Standing_Dead,',
     >  'Forest_Down_Dead_Wood,Forest_Floor,Forest_Shrub_Herb,',
     >  'Total_Stand_Carbon,Total_Removed_Carbon,',
     >  'Carbon_Released_From_Fire) VALUES (''',
     >  CASEID,''',''',TRIM(NPLT),
     >  ''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

C     PREPARE THE SQL QUERY

      iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
      if (iRet.ne.0) then
         ICMRPT = 0
         RETURN
      ENDIF

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
         ICMRPT = 0
      ENDIF

      END
