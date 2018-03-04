      SUBROUTINE DBSFMHRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C DBS $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE CARBON HARVEST
C              PRODUCTS REPORT INFORMATION
C     AUTH: D. ROBINSON - ESSA
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C         1 = PRODUCTS
C         2 = LANDFILL
C         3 = ENERGY
C         4 = EMISSIONS
C         5 = MERCH CARBON STORED
C         6 = MERCH CARBON REMOVED
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,KODE,VARDIM,IRCODE
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           I
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      CHARACTER*2000    SQLStmtStr
      CHARACTER(len=20) TABLENAME

C     Initialize variables

      IF(ICHRPT.EQ.0) RETURN
      IF(ICHRPT.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ICMRPT = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     >    'DBSFMHRPT:DSN Connection')
        GOTO 200
      ENDIF

C     CHECK TO SEE IF THE CARBON HARVEST TABLE EXISTS IN DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Hrv_Carbon$]'
      ELSE
        TABLENAME = 'FVS_Hrv_Carbon'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        ICMRPT = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Hrv_Carbon('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,' //
     -              'Products double null,' //
     -              'Landfill double null,' //
     -              'Energy double null,' //
     -              'Emissions double null,' //
     -              'Merch_Carbon_Stored double null,' //
     -              'Merch_Carbon_Removed double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Hrv_Carbon('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,' //
     -              'Products Number,' //
     -              'Landfill Number,' //
     -              'Energy Number,' //
     -              'Emissions Number,' //
     -              'Merch_Carbon_Stored Number,' //
     -              'Merch_Carbon_Removed Number)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_Hrv_Carbon('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year int null,' //
     -              'Products real null,' //
     -              'Landfill real null,' //
     -              'Energy real null,' //
     -              'Emissions real null,' //
     -              'Merch_Carbon_Stored real null,' //
     -              'Merch_Carbon_Removed real null)'

        ENDIF
            !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     >    'DBSFMHRPT:Creating Table: '//trim(SQLStmtStr))
       ENDIF

C     COPY INPUT VECTOR TO DOUBLE-PRECISION

      DO I=1,VARDIM
        VARD(I) = VAR(I)
      ENDDO

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,',
     >  'StandID,Year,Products,Landfill,Energy,Emissions,',
     >  'Merch_Carbon_Stored,Merch_Carbon_Removed) VALUES(''',
     >  CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?)'

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
     -              'DBSFMHRPT:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
