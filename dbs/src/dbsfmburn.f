      SUBROUTINE DBSFMBURN(IYEAR,NPLT,ONEHR,TENHR,HUNDHR,THOUSHR,DUFF,
     -  LIVEW,LIVEH,MFWIND,SLOPE,FLAME,SCORCH,FTYPE,FM,WT,KODE)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE BURN CONDITIONS REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE BURN CONDITIONS OUTPUT FROM THE FIRE MODEL.
C              1: ONE HOUR FUEL MOISTURE
C              2: TEN HOUR FUEL MOISTURE
C              3: HUNDRED HOUR FUEL MOISTURE
C              4: THOUSAND HOUR FUEL MOISTURE
C              5: DUFF MOISTURE
C              6: LIVE WOODY MOISTURE
C              7: LIVE HERB MOISTURE
C              8: MID FLAME WIND SPEED
C              9: SLOPE
C             10: FLAME LENGTH
C             11: SCORCH HEIGHT
C             12: FIRE TYPE
C             13: FUEL MODEL
C             14: WEIGHT GIVEN EACH FUEL MODEL
C             15: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,IRCODE,KODE,FM,SLOPE
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL ONEHR, TENHR, HUNDHR, THOUSHR, DUFF, LIVEW, LIVEH, MFWIND,
     -     FLAME, SCORCH, WT
      DOUBLE PRECISION ONEHRB, TENHRB, HUNDHRB, THOUSHRB, DUFFB, LIVEWB,
     -     LIVEHB, MFWINDB, FLAMEB, SCORCHB
      DIMENSION FM(4), WT(4)
      DOUBLE PRECISION,DIMENSION(4)::WTB
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
      CHARACTER(len=8) FTYPE

C     Initialize variables

      IF(IBURN.EQ.0) RETURN
      IF(IBURN.EQ.2) KODE = 0

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IBURN = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSFMBURN:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE FUEL CONSUMP. TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_BurnReport$]'
      ELSE
        TABLENAME = 'FVS_BurnReport'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        IBURN = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_BurnReport('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'One_Hr_Moisture double null,'//
     -              'Ten_Hr_Moisture double null,'//
     -              'Hundred_Hr_Moisture double null,'//
     -              'Thousand_Hr_Moisture double null,'//
     -              'Duff_Moisture double null,'//
     -              'Live_Woody_Moisture double null,'//
     -              'Live_Herb_Moisture double null,'//
     -              'Midflame_Wind double null,'//
     -              'Slope double null,'//
     -              'Flame_length double null,'//
     -              'Scorch_height double null,'//
     -              'Fire_Type Text null,'//
     -              'FuelModl1 double null,'//
     -              'Weight1 double null,'//
     -              'FuelModl2 double null,'//
     -              'Weight2 double null,'//
     -              'FuelModl3 double null,'//
     -              'Weight3 double null,'//
     -              'FuelModl4 double null,'//
     -              'Weight4 double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_BurnReport('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'One_Hr_Moisture Number,'//
     -              'Ten_Hr_Moisture Number,'//
     -              'Hundred_Hr_Moisture Number,'//
     -              'Thousand_Hr_Moisture Number,'//
     -              'Duff_Moisture Number,'//
     -              'Live_Woody_Moisture Number,'//
     -              'Live_Herb_Moisture Number,'//
     -              'Midflame_Wind Number,'//
     -              'Slope Number,'//
     -              'Flame_length Number,'//
     -              'Scorch_height Number,'//
     -              'Fire_Type Text,'//
     -              'FuelModl1 Number,'//
     -              'Weight1 Number,'//
     -              'FuelModl2 Number,'//
     -              'Weight2 Number,'//
     -              'FuelModl3 Number,'//
     -              'Weight3 Number,'//
     -              'FuelModl4 Number,'//
     -              'Weight4 Number)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_BurnReport('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'One_Hr_Moisture real null,'//
     -              'Ten_Hr_Moisture real null,'//
     -              'Hundred_Hr_Moisture real null,'//
     -              'Thousand_Hr_Moisture real null,'//
     -              'Duff_Moisture real null,'//
     -              'Live_Woody_Moisture real null,'//
     -              'Live_Herb_Moisture real null,'//
     -              'Midflame_Wind real null,'//
     -              'Slope real null,'//
     -              'Flame_length real null,'//
     -              'Scorch_height real null,'//
     -              'Fire_Type char(8) null,'//
     -              'FuelModl1 real null,'//
     -              'Weight1 real null,'//
     -              'FuelModl2 real null,'//
     -              'Weight2 real null,'//
     -              'FuelModl3 real null,'//
     -              'Weight3 real null,'//
     -              'FuelModl4 real null,'//
     -              'Weight4 real null)'

        ENDIF
          iRet = fvsSQLCloseCursor(StmtHndlOut)

          iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
          CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSFMBURN:Creating Table: '//trim(SQLStmtStr))
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C

      ONEHRB = ONEHR
      TENHRB = TENHR
      HUNDHRB = HUNDHR
      THOUSHRB = THOUSHR
      DUFFB = DUFF
      LIVEWB = LIVEW
      LIVEHB = LIVEH
      MFWINDB = MFWIND
      FLAMEB = FLAME
      SCORCHB = SCORCH
      WTB(1) = INT((WT(1)*100.)+0.5)
      WTB(2) = INT((WT(2)*100.)+0.5)
      WTB(3) = INT((WT(3)*100.)+0.5)
      WTB(4) = INT((WT(4)*100.)+0.5)

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,',
     -  'StandID,Year,One_Hr_Moisture,Ten_Hr_Moisture,',
     -  'Hundred_Hr_Moisture,Thousand_Hr_Moisture,Duff_Moisture,',
     -  'Live_Woody_Moisture,Live_Herb_Moisture,Midflame_Wind,Slope,',
     -  'Flame_length,Scorch_height,Fire_Type,FuelModl1,',
     -  'Weight1,FuelModl2,Weight2,FuelModl3,Weight3,',
     -  'FuelModl4,Weight4) VALUES(''',CASEID,''',''',
     -  TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,''',
     -  TRIM(FTYPE),''',?,?,?,?,?,?,?,?)'

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C

      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -           SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),ONEHRB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),TENHRB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),HUNDHRB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),THOUSHRB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),DUFFB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),LIVEWB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),LIVEHB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),MFWINDB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SLOPE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),FLAMEB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCORCHB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),FM(1),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),WTB(1),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),FM(2),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),WTB(2),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -         SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),FM(3),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -         SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),WTB(3),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -         SQL_PARAM_INPUT,
     -         SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),FM(4),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -         SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(0,SQLSMALLINT_KIND),WTB(4),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)


  100 CONTINUE

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSFMBURN:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


