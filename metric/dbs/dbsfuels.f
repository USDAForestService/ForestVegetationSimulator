      SUBROUTINE DBSFUELS(IYEAR,NPLT,LITTER,DUFF,SDEADLT3,SDEADGT3,
     -  SDEAD3TO6,SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3,
     -  SNAGSGT3,FOLIAGE,STANDLT3,STANDGT3,STANDTOTAL,BIOMASS,CONSUMED,
     -  REMOVED,KODE)
      IMPLICIT NONE
C----------
C METRIC-DBS $Id$
C----------
C     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS REPORT INFORMATION
C     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
C     INPUT:
C              THE ALL FUELS OUTPUT FROM THE FIRE MODEL.
C              1: SURFACE LITTER
C              2: SURFACE DUFF
C              3: SURFACE DEAD FUEL LESS THAN 3"
C              4: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 3"
C              5: SURFACE DEAD FUEL BETWEEN 3" AND 6"
C              6: SURFACE DEAD FUEL BETWEEN 6" AND 12"
C              7: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 12"
C              8: SURFACE HERB FUEL
C              9: SURFACE SHRUB FUEL
C             10: SURFACE TOTAL FUEL
C             11: STANDING SNAGS LESS THAN 3"
C             12: STANDING SNAGS GREATER THAN OR EQUAL TO 3"
C             13: STANDING FOLIAGE
C             14: STANDING LIVE LESS THAN 3"
C             15: STANDING LIVE GREATER THAN OR EQUAL TO 3"
C             16: STANDING TOTAL
C             17: TOTAL BIOMASS
C             18: TOTAL CONSUMED
C             19: BIOMASS REMOVED
C             20: KODE FOR WHETHER REPORT ALSO DUMPS TO FILE
C
C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
C---
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C---


      INTEGER IYEAR,BIOMASS,CONSUMED,REMOVED,ID,KODE,STANDGT3,STANDTOTAL
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL LITTER,DUFF,SDEADLT3,SDEADGT3,SDEAD3TO6,
     -  SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3,SNAGSGT3,
     -  FOLIAGE,STANDLT3
      DOUBLE PRECISION LITTERB,DUFFB,SDEADLT3B,SDEADGT3B,SDEAD3TO6B,
     -  SDEAD6TO12B,SDEADGT12B,HERBB,SHRUBB,SURFTOTALB,SNAGSLT3B,
     -  SNAGSGT3B,FOLIAGEB,STANDLT3B
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
C
C
COMMONS END

C---
C     Initialize variables
C
      IF(IFUELS.EQ.0) RETURN
      IF(IFUELS.EQ.2) KODE = 0

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IFUELS = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSSUMRY:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE POTFIRE TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Fuels$]'
      ELSE
        TABLENAME = 'FVS_Fuels'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Fuels('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Surface_Litter double null,'//
     -              'Surface_Duff double null,'//
     -              'Surface_lt76 double null,'//
     -              'Surface_ge76 double null,'//
     -              'Surface_76to152 double null,'//
     -              'Surface_152to305 double null,'//
     -              'Surface_ge305 double null,'//
     -              'Surface_Herb double null,'//
     -              'Surface_Shrub double null,'//
     -              'Surface_Total double null,'//
     -              'Standing_Snag_lt76 double null,'//
     -              'Standing_Snag_ge76 double null,'//
     -              'Standing_Foliage double null,'//
     -              'Standing_Live_lt76 double null,'//
     -              'Standing_Live_ge76 double null,'//
     -              'Standing_Total double null,'//
     -              'Total_Biomass Int null,'//
     -              'Total_Consumed Int null,'//
     -              'Biomass_Removed Int null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Fuels('//
     -              'ID Int,'//
     -              'CaseID int,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'Surface_Litter Number,'//
     -              'Surface_Duff Number,'//
     -              'Surface_lt76 Number,'//
     -              'Surface_ge76 Number,'//
     -              'Surface_76to152 Number,'//
     -              'Surface_152to305 Number,'//
     -              'Surface_ge305 Number,'//
     -              'Surface_Herb Number,'//
     -              'Surface_Shrub Number,'//
     -              'Surface_Total Number,'//
     -              'Standing_Snag_lt76 Number,'//
     -              'Standing_Snag_ge76 Number,'//
     -              'Standing_Foliage Number,'//
     -              'Standing_Live_lt76 Number,'//
     -              'Standing_Live_ge76 Number,'//
     -              'Standing_Total Number,'//
     -              'Total_Biomass Int,'//
     -              'Total_Consumed Int,'//
     -              'Biomass_Removed Int)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_Fuels('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Surface_Litter real null,'//
     -              'Surface_Duff real null,'//
     -              'Surface_lt76 real null,'//
     -              'Surface_ge76 real null,'//
     -              'Surface_76to152 real null,'//
     -              'Surface_152to305 real null,'//
     -              'Surface_ge305 real null,'//
     -              'Surface_Herb real null,'//
     -              'Surface_Shrub real null,'//
     -              'Surface_Total real null,'//
     -              'Standing_Snag_lt76 real null,'//
     -              'Standing_Snag_ge76 real null,'//
     -              'Standing_Foliage real null,'//
     -              'Standing_Live_lt76 real null,'//
     -              'Standing_Live_ge76 real null,'//
     -              'Standing_Total real null,'//
     -              'Total_Biomass Int null,'//
     -              'Total_Consumed Int null,'//
     -              'Biomass_Removed Int null)'
        ENDIF
        
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSFUELS:Creating Table: '//trim(SQLStmtStr))
        FUELID = 0
      ENDIF

C---------
C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE
C---------
      IF(FUELID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        FUELID = ID
      ENDIF
      FUELID = FUELID + 1
C
C     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C
      IF(FUELID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      LITTERB=LITTER
      DUFFB=DUFF
      SDEADLT3B=SDEADLT3
      SDEADGT3B=SDEADGT3
      SDEAD3TO6B=SDEAD3TO6
      SDEAD6TO12B=SDEAD6TO12
      SDEADGT12B=SDEADGT12
      HERBB=HERB
      SHRUBB=SHRUB
      SURFTOTALB=SURFTOTAL
      SNAGSLT3B=SNAGSLT3
      SNAGSGT3B=SNAGSGT3
      FOLIAGEB=FOLIAGE
      STANDLT3B=STANDLT3

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (Id,CaseID,
     -  StandID,Year,Surface_Litter,Surface_Duff,Surface_lt76,
     -  Surface_ge76,Surface_76to152,Surface_152to305,Surface_ge305,
     -  Surface_Herb,Surface_Shrub,Surface_Total,Standing_Snag_lt76,
     -  Standing_Snag_ge76,Standing_Foliage,Standing_Live_lt76,
     -  Standing_Live_ge76,Standing_Total,Total_Biomass,
     -  Total_Consumed,Biomass_Removed) VALUES(?,?,',
     -  CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,?,?,?,?,?,?,?,?,?,?,?
     -  ,?,?,?,?,?,?)'

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),FUELID,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ICASE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),LITTERB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DUFFB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SDEADLT3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SDEADGT3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SDEAD3TO6B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SDEAD6TO12B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SDEADGT12B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),HERBB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SHRUBB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SURFTOTALB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SNAGSLT3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SNAGSGT3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),FOLIAGEB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),STANDLT3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),STANDGT3,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),STANDTOTAL,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),BIOMASS,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),CONSUMED,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),REMOVED,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

  100 CONTINUE
      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSFUELS:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
     

