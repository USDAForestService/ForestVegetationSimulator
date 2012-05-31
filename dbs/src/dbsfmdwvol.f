      SUBROUTINE DBSFMDWVOL(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD VOLUME REPORT
C              INFORMATION
C     AUTH: S. REBAIN
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C       UNITS ARE CUFT/ACRE
C         1 = DOWN WOOD 0 - 3" HARD
C         2 = DOWN WOOD 3 - 6" HARD
C         3 = DOWN WOOD 6 - 12" HARD
C         4 = DOWN WOOD 12 - 20" HARD
C         5 = DOWN WOOD 20 - 35" HARD
C         6 = DOWN WOOD 35 - 50" HARD
C         7 = DOWN WOOD 50"+ HARD
C         8 = DOWN WOOD TOTAL HARD
C         9 = DOWN WOOD 0 - 3" SOFT
C         10 = DOWN WOOD 3 - 6" SOFT
C         11 = DOWN WOOD 6 - 12" SOFT
C         12 = DOWN WOOD 12 - 20" SOFT
C         13 = DOWN WOOD 20 - 35" SOFT
C         14 = DOWN WOOD 35 - 50" SOFT
C         15 = DOWN WOOD 50"+ SOFT
C         16 = DOWN WOOD TOTAL SOFT
C       KODE   - RETURN CODE

COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR, KODE,VARDIM
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           ID,I
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      CHARACTER*2000    SQLStmtStr
      CHARACTER(len=20) TABLENAME

C     Initialize variables

      IF(IDWDVOL.EQ.0) RETURN
      IF(IDWDVOL.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDWDVOL = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     >    'DBSFMDWVOL:DSN Connection')
        GOTO 200
      ENDIF

C     CHECK TO SEE IF THE DOWN WOOD VOLUME TABLE EXISTS IN DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Down_Wood_Vol$]'
      ELSE
        TABLENAME = 'FVS_Down_Wood_Vol'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Vol('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'DWD_Volume_0to3_Hard double null,'//
     -              'DWD_Volume_3to6_Hard double null,'//
     -              'DWD_Volume_6to12_Hard double null,'//
     -              'DWD_Volume_12to20_Hard double null,'//
     -              'DWD_Volume_20to35_Hard double null,'//
     -              'DWD_Volume_35to50_Hard double null,'//
     -              'DWD_Volume_ge_50_Hard double null,'//
     -              'DWD_Volume_Total_Hard double null,'//
     -              'DWD_Volume_0to3_Soft double null,'//
     -              'DWD_Volume_3to6_Soft double null,'//
     -              'DWD_Volume_6to12_Soft double null,'//
     -              'DWD_Volume_12to20_Soft double null,'//
     -              'DWD_Volume_20to35_Soft double null,'//
     -              'DWD_Volume_35to50_Soft double null,'//
     -              'DWD_Volume_ge_50_Soft double null,'//
     -              'DWD_Volume_Total_Soft double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Vol('//
     -              'ID Int,'//
     -              'CaseID Int,'//
     -              'StandID Text,'//
     -              'Year Int ,'//
     -              'DWD_Volume_0to3_Hard Number,'//
     -              'DWD_Volume_3to6_Hard Number,'//
     -              'DWD_Volume_6to12_Hard Number,'//
     -              'DWD_Volume_12to20_Hard Number,'//
     -              'DWD_Volume_20to35_Hard Number,'//
     -              'DWD_Volume_35to50_Hard Number,'//
     -              'DWD_Volume_ge_50_Hard Number,'//
     -              'DWD_Volume_Total_Hard Number,'//
     -              'DWD_Volume_0to3_Soft Number,'//
     -              'DWD_Volume_3to6_Soft Number,'//
     -              'DWD_Volume_6to12_Soft Number,'//
     -              'DWD_Volume_12to20_Soft Number,'//
     -              'DWD_Volume_20to35_Soft Number,'//
     -              'DWD_Volume_35to50_Soft Number,'//
     -              'DWD_Volume_ge_50_Soft Number,'//
     -              'DWD_Volume_Total_Soft Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Vol('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'DWD_Volume_0to3_Hard real null,'//
     -              'DWD_Volume_3to6_Hard real null,'//
     -              'DWD_Volume_6to12_Hard real null,'//
     -              'DWD_Volume_12to20_Hard real null,'//
     -              'DWD_Volume_20to35_Hard real null,'//
     -              'DWD_Volume_35to50_Hard real null,'//
     -              'DWD_Volume_ge_50_Hard real null,'//
     -              'DWD_Volume_Total_Hard real null,'//
     -              'DWD_Volume_0to3_Soft real null,'//
     -              'DWD_Volume_3to6_Soft real null,'//
     -              'DWD_Volume_6to12_Soft real null,'//
     -              'DWD_Volume_12to20_Soft real null,'//
     -              'DWD_Volume_20to35_Soft real null,'//
     -              'DWD_Volume_35to50_Soft real null,'//
     -              'DWD_Volume_ge_50_Soft real null,'//
     -              'DWD_Volume_Total_Soft real null)'
        ENDIF

        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     >    'DBSFMDWVOL:Creating Table: '//trim(SQLStmtStr))
        DWDVID = 0
      ENDIF

C     CREATE ENTRY FROM DATA FOR DOWN WOOD VOLUME TABLE

      IF(DWDVID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        DWDVID = ID
      ENDIF
      DWDVID = DWDVID + 1

C     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

      IF(DWDVID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C     COPY INPUT VECTOR TO DOUBLE-PRECISION

      DO I=1,VARDIM
        VARD(I) = VAR(I)
      ENDDO

      WRITE(SQLStmtStr,*)'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID,
     >  StandID,Year,DWD_Volume_0to3_Hard,DWD_Volume_3to6_Hard,
     >  DWD_Volume_6to12_Hard,DWD_Volume_12to20_Hard,
     >  DWD_Volume_20to35_Hard,DWD_Volume_35to50_Hard,
     >  DWD_Volume_ge_50_Hard,DWD_Volume_Total_Hard,
     >  DWD_Volume_0to3_Soft,DWD_Volume_3to6_Soft,
     >  DWD_Volume_6to12_Soft,DWD_Volume_12to20_Soft,
     >  DWD_Volume_20to35_Soft,DWD_Volume_35to50_Soft,
     >  DWD_Volume_ge_50_Soft,DWD_Volume_Total_Soft)
     >  VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,
     >  ?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

C     CLOSE CURSOR

      iRet = fvsSQLCloseCursor(StmtHndlOut)

C     PREPARE THE SQL QUERY

      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),DWDVID,int(4,SQLLEN_KIND),
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

      DO I=1,VARDIM
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),VARD(I),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      ENDDO

  100 CONTINUE


      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSDWVOL:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
