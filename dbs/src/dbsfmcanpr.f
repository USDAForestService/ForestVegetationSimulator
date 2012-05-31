      SUBROUTINE DBSFMCANPR(IYEAR,CRFILL,NPLT)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH CANOPY PROFILE
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- JUNE 2006
C     INPUT:
C              THE CANOPY PROFILE INFO FROM THE FIRE MODEL.
C              1: BIOMASS OF CANOPY FUELS AT VARIOUS HEIGHTS
C                  ABOVE THE GROUND IN LBS/ACRE/FOOT.
C
C     THIS TABLE IS UNIQUE IN THAT THERE IS NOT A CORRESPONDING
C     TEXT FILE TABLE.
C
C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C---
      INTEGER IYEAR,ID, I
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL CRFILL
      DOUBLE PRECISION CRFILLB, CRFILLKG, HTFT, HTM
      DIMENSION CRFILL(200), CRFILLB(200), CRFILLKG(200)
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
C
C     Initialize variables
C
      IF(ICANPR.EQ.0) RETURN

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ICANPR = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSFMCANPR:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE CANOPY PROFILE TABLE EXISTS IN DATABASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_CanProfile$]'
      ELSE
        TABLENAME = 'FVS_CanProfile'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      !PRINT*, SQLStmtStr
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))


      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_CanProfile('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Height_m double null,'//
     -              'Canopy_Fuel_kg_m3 double null,'//
     -              'Height_ft double null,'//
     -              'Canopy_Fuel_lbs_acre_ft double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_CanProfile('//
     -              'ID Int,'//
     -              'CaseID int,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'Height_m Number,'//
     -              'Canopy_Fuel_kg_m3 Number,'//
     -              'Height_ft Number,'//
     -              'Canopy_Fuel_lbs_acre_ft Number)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_CanProfile('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Height_m real null,'//
     -              'Canopy_Fuel_kg_m3 real null,'//
     -              'Height_ft real null,'//
     -              'Canopy_Fuel_lbs_acre_ft real null)'

        ENDIF
        !PRINT*, SQLStmtStr

            !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSFMCANPR:Creating Table: '//trim(SQLStmtStr))
        CANPRID = 0
      ENDIF

      DO I = 1,200

        IF (CRFILL(I) .LE. 0) GOTO 150
C
C       CREATE ENTRY FROM DATA FOR CANOPY PROFILE TABLE
C
        IF(CANPRID.EQ.-1) THEN
          CALL DBSGETID(TABLENAME,'Id',ID)
          CANPRID = ID
        ENDIF
        CANPRID = CANPRID + 1
C
C       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C
        IF(CANPRID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C
C       ASSIGN VALUES TO DOUBLE PRECISION VARS
C
        CRFILLB(I) = CRFILL(I)
        CRFILLKG(I) = CRFILL(I)*0.45359237 / (4046.856422 * 0.3048)
        HTFT = I
        HTM = I*0.3048

        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (Id,CaseID,
     -    StandID,Year,Height_m,Canopy_Fuel_kg_m3,Height_ft,
     -    Canopy_Fuel_lbs_acre_ft)
     -    VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,?,?)'

        !PRINT*, SQLStmtStr
C
C       CLOSE CURSOR
C
        iRet = fvsSQLCloseCursor(StmtHndlOut)
C
C       PREPARE THE SQL QUERY
C
        iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C

        ColNumber=1
        iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -    SQL_PARAM_INPUT,SQL_F_INTEGER, SQL_INTEGER,
     -    INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -    CANPRID,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -    SQL_PARAM_INPUT,SQL_F_INTEGER, SQL_INTEGER,
     -    INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -    ICASE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -    SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,
     -    INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -    IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -    SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -    INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -    HTM,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -    INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -    CRFILLKG(I),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,
     -    SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -    INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -    HTFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -    INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -    CRFILLB(I),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

  100   CONTINUE
        !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecute(StmtHndlOut)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                'DBSFMCANPR:Inserting Row')
  150   CONTINUE

      ENDDO

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


