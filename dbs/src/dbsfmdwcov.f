      SUBROUTINE DBSFMDWCOV(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C DBS $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD COVER REPORT
C              INFORMATION
C     AUTH: S. REBAIN
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C       UNITS ARE % COVER
C         1 = DOWN WOOD 3 - 6" HARD
C         2 = DOWN WOOD 6 - 12" HARD
C         3 = DOWN WOOD 12 - 20" HARD
C         4 = DOWN WOOD 20 - 35" HARD
C         5 = DOWN WOOD 35 - 50" HARD
C         6 = DOWN WOOD 50"+ HARD
C         7 = DOWN WOOD TOTAL HARD
C         8 = DOWN WOOD 3 - 6" SOFT
C         9 = DOWN WOOD 6 - 12" SOFT
C         10 = DOWN WOOD 12 - 20" SOFT
C         11 = DOWN WOOD 20 - 35" SOFT
C         12 = DOWN WOOD 35 - 50" SOFT
C         13 = DOWN WOOD 50"+ SOFT
C         14 = DOWN WOOD TOTAL SOFT
C       KODE   - RETURN CODE

COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR, KODE,VARDIM,IRCODE
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           I
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      CHARACTER*2000    SQLStmtStr
      CHARACTER(len=20) TABLENAME

C     Initialize variables

      IF(IDWDCOV.EQ.0) RETURN
      IF(IDWDCOV.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDWDCOV = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     >    'DBSFMDWCOV:DSN Connection')
        GOTO 200
      ENDIF

C     CHECK TO SEE IF THE DOWN WOOD COVER TABLE EXISTS IN DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Down_Wood_Cov$]'
      ELSE
        TABLENAME = 'FVS_Down_Wood_Cov'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        IDWDCOV = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Cov('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'DWD_Cover_3to6_Hard double null,'//
     -              'DWD_Cover_6to12_Hard double null,'//
     -              'DWD_Cover_12to20_Hard double null,'//
     -              'DWD_Cover_20to35_Hard double null,'//
     -              'DWD_Cover_35to50_Hard double null,'//
     -              'DWD_Cover_ge_50_Hard double null,'//
     -              'DWD_Cover_Total_Hard double null,'//
     -              'DWD_Cover_3to6_Soft double null,'//
     -              'DWD_Cover_6to12_Soft double null,'//
     -              'DWD_Cover_12to20_Soft double null,'//
     -              'DWD_Cover_20to35_Soft double null,'//
     -              'DWD_Cover_35to50_Soft double null,'//
     -              'DWD_Cover_ge_50_Soft double null,'//
     -              'DWD_Cover_Total_Soft double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Cov('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int ,'//
     -              'DWD_Cover_3to6_Hard Number,'//
     -              'DWD_Cover_6to12_Hard Number,'//
     -              'DWD_Cover_12to20_Hard Number,'//
     -              'DWD_Cover_20to35_Hard Number,'//
     -              'DWD_Cover_35to50_Hard Number,'//
     -              'DWD_Cover_ge_50_Hard Number,'//
     -              'DWD_Cover_Total_Hard Number,'//
     -              'DWD_Cover_3to6_Soft Number,'//
     -              'DWD_Cover_6to12_Soft Number,'//
     -              'DWD_Cover_12to20_Soft Number,'//
     -              'DWD_Cover_20to35_Soft Number,'//
     -              'DWD_Cover_35to50_Soft Number,'//
     -              'DWD_Cover_ge_50_Soft Number,'//
     -              'DWD_Cover_Total_Soft Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Cov('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'DWD_Cover_3to6_Hard real null,'//
     -              'DWD_Cover_6to12_Hard real null,'//
     -              'DWD_Cover_12to20_Hard real null,'//
     -              'DWD_Cover_20to35_Hard real null,'//
     -              'DWD_Cover_35to50_Hard real null,'//
     -              'DWD_Cover_ge_50_Hard real null,'//
     -              'DWD_Cover_Total_Hard real null,'//
     -              'DWD_Cover_3to6_Soft real null,'//
     -              'DWD_Cover_6to12_Soft real null,'//
     -              'DWD_Cover_12to20_Soft real null,'//
     -              'DWD_Cover_20to35_Soft real null,'//
     -              'DWD_Cover_35to50_Soft real null,'//
     -              'DWD_Cover_ge_50_Soft real null,'//
     -              'DWD_Cover_Total_Soft real null)'
        ENDIF

        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     >    'DBSFMDWCOV:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     COPY INPUT VECTOR TO DOUBLE-PRECISION

      DO I=1,VARDIM
        VARD(I) = VAR(I)
      ENDDO

      WRITE(SQLStmtStr,*)'INSERT INTO ',TRIM(TABLENAME),
     >  ' (CaseID,StandID,Year,DWD_Cover_3to6_Hard,',
     >  'DWD_Cover_6to12_Hard,DWD_Cover_12to20_Hard,',
     >  'DWD_Cover_20to35_Hard,DWD_Cover_35to50_Hard,',
     >  'DWD_Cover_ge_50_Hard,DWD_Cover_Total_Hard,',
     >  'DWD_Cover_3to6_Soft,',
     >  'DWD_Cover_6to12_Soft,DWD_Cover_12to20_Soft,',
     >  'DWD_Cover_20to35_Soft,DWD_Cover_35to50_Soft,',
     >  'DWD_Cover_ge_50_Soft,DWD_Cover_Total_Soft) VALUES (''',
     >  CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

C     CLOSE CURSOR

      iRet = fvsSQLCloseCursor(StmtHndlOut)

C     PREPARE THE SQL QUERY

      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      DO I=1,VARDIM
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),VARD(I),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      ENDDO

      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSFMDWCOV:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
