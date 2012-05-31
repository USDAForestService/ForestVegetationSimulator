      SUBROUTINE DBSBMVOL(NPLT,IYEAR,TVOL,HVOL,VOLK,NUMSC,CID)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: POPULATES A DATABASE TABLE WITH WWPBM OUTPUT
C              PERTAINING TO VOLUME-BY-SIZE-CLASS.
C     AUTH: AJ MCMAHAN -- ITX, INC. SEPT. 2005
C     INPUT:
C     TVOL  ARRAY HOLDING STAND VOLUME BY WWPBM-SIZE CLASS
C     HVOL  ARRAY HOLDING STAND HOST VOLUME BY WWPBM-SIZE CLASS
C     VOLK  ARRAY HOLDING VOLUME BEETLE-KILLED BY WWPBM-SIZE CLASS
C
C     CID/ICASE - CASE NUMBER FROM THE FVSRUN TABLE
C
C---
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
C     DECLARATIONS
C---
      INTEGER IYEAR,NUMSC,ID,X,CID
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL TVOL(NUMSC),HVOL(NUMSC),VOLK(NUMSC)
      DOUBLE PRECISION TVOLB(NUMSC),HVOLB(NUMSC),VOLKB(NUMSC)
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
C
C
C     CASE ID WAS FETCHED IN BMSDIT.
C     THE WWPBM IS UNIQUE IN THIS REGARD BECAUSE OF ITS PPE MODE 2 OPERABILITY
C     WE BOOKKEEP CASE ID WITHIN THE WWPBM AND PASS IT HERE.
C
      ICASE=CID
C
C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO)THEN
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSBMVOL:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE WWPBM VOLUME BY SIZE CLASS TABLE EXISTS IN DB
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_BM_Vol$]'
      ELSE
        TABLENAME = 'FVS_BM_Vol'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME
C
      !PRINT*, SQLStmtStr
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_BM_Vol('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'TV_SC1 double null,'//
     -              'TV_SC2 double null,'//
     -              'TV_SC3 double null,'//
     -              'TV_SC4 double null,'//
     -              'TV_SC5 double null,'//
     -              'TV_SC6 double null,'//
     -              'TV_SC7 double null,'//
     -              'TV_SC8 double null,'//
     -              'TV_SC9 double null,'//
     -              'TV_10  double null,'//
     -              'HV_SC1 double null,'//
     -              'HV_SC2 double null,'//
     -              'HV_SC3 double null,'//
     -              'HV_SC4 double null,'//
     -              'HV_SC5 double null,'//
     -              'HV_SC6 double null,'//
     -              'HV_SC7 double null,'//
     -              'HV_SC8 double null,'//
     -              'HV_SC9 double null,'//
     -              'HV_10  double null,'//
     -              'VK_SC1 double null,'//
     -              'VK_SC2 double null,'//
     -              'VK_SC3 double null,'//
     -              'VK_SC4 double null,'//
     -              'VK_SC5 double null,'//
     -              'VK_SC6 double null,'//
     -              'VK_SC7 double null,'//
     -             ' VK_SC8 double null,'//
     -              'VK_SC9 double null,'//
     -              'VK_10  double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_BM_Vol('//
     -              'ID Int,'//
     -              'CaseID int,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'TV_SC1 Number,'//
     -              'TV_SC2 Number,'//
     -              'TV_SC3 Number,'//
     -              'TV_SC4 Number,'//
     -              'TV_SC5 Number,'//
     -              'TV_SC6 Number,'//
     -              'TV_SC7 Number,'//
     -              'TV_SC8 Number,'//
     -              'TV_SC9 Number,'//
     -              'TV_10  Number,'//
     -              'HV_SC1 Number,'//
     -              'HV_SC2 Number,'//
     -              'HV_SC3 Number,'//
     -              'HV_SC4 Number,'//
     -              'HV_SC5 Number,'//
     -              'HV_SC6 Number,'//
     -              'HV_SC7 Number,'//
     -              'HV_SC8 Number,'//
     -              'HV_SC9 Number,'//
     -              'HV_10  Number,'//
     -              'VK_SC1 Number,'//
     -              'VK_SC2 Number,'//
     -              'VK_SC3 Number,'//
     -              'VK_SC4 Number,'//
     -              'VK_SC5 Number,'//
     -              'VK_SC6 Number,'//
     -              'VK_SC7 Number,'//
     -             ' VK_SC8 Number,'//
     -              'VK_SC9 Number,'//
     -              'VK_10  Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_BM_Vol('//
     -              'Id int        primary key,'//
     -              'CaseID        int not null,'//
     -              'StandID       char(26) not null,'//
     -              'Year          Int null,'//
     -              'TV_SC1 real null,'//
     -              'TV_SC2 real null,'//
     -              'TV_SC3 real null,'//
     -              'TV_SC4 real null,'//
     -              'TV_SC5 real null,'//
     -              'TV_SC6 real null,'//
     -              'TV_SC7 real null,'//
     -              'TV_SC8 real null,'//
     -              'TV_SC9 real null,'//
     -              'TV_10  real null,'//
     -              'HV_SC1 real null,'//
     -              'HV_SC2 real null,'//
     -              'HV_SC3 real null,'//
     -              'HV_SC4 real null,'//
     -              'HV_SC5 real null,'//
     -              'HV_SC6 real null,'//
     -              'HV_SC7 real null,'//
     -              'HV_SC8 real null,'//
     -              'HV_SC9 real null,'//
     -              'HV_10  real null,'//
     -              'VK_SC1 real null,'//
     -              'VK_SC2 real null,'//
     -              'VK_SC3 real null,'//
     -              'VK_SC4 real null,'//
     -              'VK_SC5 real null,'//
     -              'VK_SC6 real null,'//
     -              'VK_SC7 real null,'//
     -             ' VK_SC8 real null,'//
     -              'VK_SC9 real null,'//
     -              'VK_10  real null)'
        ENDIF
         iRet = fvsSQLCloseCursor(StmtHndlOut)
C
         iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
         CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSBMVOL:Creating Table: '//trim(SQLStmtStr))
        BMVOLID = 0
      ENDIF
C
C---------
C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE
C---------
      IF(BMVOLID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        BMVOLID = ID
      ENDIF
      BMVOLID = BMVOLID + 1
C
C     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C
      IF(BMVOLID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS/ARRAYS
C
      TVOLB=TVOL
      HVOLB=HVOL
      VOLKB=VOLK
C
      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (Id,CaseID,
     -  StandID,Year,
     - TV_SC1, TV_SC2,TV_SC3, TV_SC4,TV_SC5,TV_SC6,TV_SC7,TV_SC8,
     - TV_SC9,TV_10 ,HV_SC1,HV_SC2,HV_SC3,HV_SC4,HV_SC5,HV_SC6,HV_SC7,
     - HV_SC8,HV_SC9,HV_10 ,VK_SC1,VK_SC2,VK_SC3,VK_SC4,VK_SC5,VK_SC6,
     - VK_SC7,VK_SC8,VK_SC9,VK_10)VALUES(?,?,',
     -  CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,?,?,?,?,?,?,?,?,?,?,?
     -  ,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
C
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),BMVOLID,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ICASE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      DO X=1,NUMSC
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),TVOLB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

      DO X=1,NUMSC
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),HVOLB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

      DO X=1,NUMSC
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),VOLKB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

  100 CONTINUE
      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSBMVOL:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END

