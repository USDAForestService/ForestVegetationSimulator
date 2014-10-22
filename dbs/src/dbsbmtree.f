      SUBROUTINE DBSBMTREE(NSCL,NPLT,IYEAR,TPA,TPAH,TPAKLL,SPCLTRE,
     >   SAN1,SAN2,SAN3,SAN4,SAN5,SAN6,SAN7,SAN8,SAN9,SAN10,CID)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: POPULATES A DATABASE TABLE WITH DETAILED WWPBM OUTPUT VARS.
C              REPORTED BY TREE SIZE CLASS (SC)
C     AUTH: AJ MCMAHAN -- ITX, INC. SEPT. 2005
C     INPUT:
C
C     TPA:      TREES PER ACRE (TPA) IN EACH SIZE CLASS (BEGINNING OF YR)
C     TPAH:     TPA HOST BY SIZE CLASS (@ BEGINNING OF YEAR)
C     TPAKLL:   TPA BEETLE-KILLED BY SIZE CLASS (DURING THIS YEAR)
C     SPCLTRE:  TPA OF WWPBM "SPECIAL" TREES (BEGINNING OF YEAR)
C     SAN1(-10):LIVE, UNATTACKED TPA REMOVED VIA SANITATION CUTS THIS YR
C     NSCL:     # WWPBM SIZE CLASSES (CURRENTLY =10)
C     CID: CASE ID FROM THE FVSCases TABLE
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
      INTEGER IYEAR,NSCL,ID,X
      CHARACTER (len=36) CID
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL TPA(NSCL),TPAH(NSCL),TPAKLL(NSCL),SPCLTRE(NSCL),
     >   SAN1,SAN2,SAN3,SAN4,SAN5,SAN6,SAN7,SAN8,SAN9,SAN10
      DOUBLE PRECISION TPAb(NSCL),TPAHb(NSCL),TPAKLLb(NSCL),
     >                 SPCLTREb(NSCL),SAN1b,SAN2b,SAN3b,SAN4b,
     >                 SAN5b,SAN6b,SAN7b,SAN8b,SAN9b,SAN10b
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
C
C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSBMTREE:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE WWPBM "TREEOUT" TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_BM_Tree$]'
      ELSE
        TABLENAME = 'FVS_BM_Tree'
      ENDIF
      SQLStmtStr= 'SELECT Count(*) FROM ' // TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_BM_Tree('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'TPA_SC1  double null,'//
     -              'TPA_SC2  double null,'//
     -              'TPA_SC3  double null,'//
     -              'TPA_SC4  double null,'//
     -              'TPA_SC5  double null,'//
     -              'TPA_SC6  double null,'//
     -              'TPA_SC7  double null,'//
     -              'TPA_SC8  double null,'//
     -              'TPA_SC9  double null,'//
     -              'TPA_10   double null,'//
     -              'HOST1    double null,'//
     -              'HOST2    double null,'//
     -              'HOST3    double null,'//
     -              'HOST4    double null,'//
     -              'HOST5    double null,'//
     -              'HOST6    double null,'//
     -              'HOST7    double null,'//
     -              'HOST8    double null,'//
     -              'HOST9    double null,'//
     -              'HOST10   double null,'//
     -              'TKLD1    double null,'//
     -              'TKLD2    double null,'//
     -              'TKLD3    double null,'//
     -              'TKLD4    double null,'//
     -              'TKLD5    double null,'//
     -              'TKLD6    double null,'//
     -              'TKLD7    double null,'//
     -              'TKLD8    double null,'//
     -              'TKLD9    double null,'//
     -              'TKLD10   double null,'//
     -              'SPCL1    double null,'//
     -              'SPCL2    double null,'//
     -              'SPCL3    double null,'//
     -              'SPCL4    double null,'//
     -              'SPCL5    double null,'//
     -              'SPCL6    double null,'//
     -              'SPCL7    double null,'//
     -              'SPCL8    double null,'//
     -              'SPCL9    double null,'//
     -              'SPCL10   double null,'//
     -              'SAN1     double null,'//
     -              'SAN2     double null,'//
     -              'SAN3     double null,'//
     -              'SAN4     double null,'//
     -              'SAN5     double null,'//
     -              'SAN6     double null,'//
     -              'SAN7     double null,'//
     -              'SAN8     double null,'//
     -              'SAN9     double null,'//
     -              'SAN10    double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_BM_Tree('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'TPA_SC1  Number,'//
     -              'TPA_SC2  Number,'//
     -              'TPA_SC3  Number,'//
     -              'TPA_SC4  Number,'//
     -              'TPA_SC5  Number,'//
     -              'TPA_SC6  Number,'//
     -              'TPA_SC7  Number,'//
     -              'TPA_SC8  Number,'//
     -              'TPA_SC9  Number,'//
     -              'TPA_10   Number,'//
     -              'HOST1    Number,'//
     -              'HOST2    Number,'//
     -              'HOST3    Number,'//
     -              'HOST4    Number,'//
     -              'HOST5    Number,'//
     -              'HOST6    Number,'//
     -              'HOST7    Number,'//
     -              'HOST8    Number,'//
     -              'HOST9    Number,'//
     -              'HOST10   Number,'//
     -              'TKLD1    Number,'//
     -              'TKLD2    Number,'//
     -              'TKLD3    Number,'//
     -              'TKLD4    Number,'//
     -              'TKLD5    Number,'//
     -              'TKLD6    Number,'//
     -              'TKLD7    Number,'//
     -              'TKLD8    Number,'//
     -              'TKLD9    Number,'//
     -              'TKLD10   Number,'//
     -              'SPCL1    Number,'//
     -              'SPCL2    Number,'//
     -              'SPCL3    Number,'//
     -              'SPCL4    Number,'//
     -              'SPCL5    Number,'//
     -              'SPCL6    Number,'//
     -              'SPCL7    Number,'//
     -              'SPCL8    Number,'//
     -              'SPCL9    Number,'//
     -              'SPCL10   Number,'//
     -              'SAN1     Number,'//
     -              'SAN2     Number,'//
     -              'SAN3     Number,'//
     -              'SAN4     Number,'//
     -              'SAN5     Number,'//
     -              'SAN6     Number,'//
     -              'SAN7     Number,'//
     -              'SAN8     Number,'//
     -              'SAN9     Number,'//
     -              'SAN10    Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_BM_Tree('//
     -              'CaseID        char(36) not null,'//
     -              'StandID       char(26) not null,'//
     -              'Year          Int null,'//
     -              'TPA_SC1  real null,'//
     -              'TPA_SC2  real null,'//
     -              'TPA_SC3  real null,'//
     -              'TPA_SC4  real null,'//
     -              'TPA_SC5  real null,'//
     -              'TPA_SC6  real null,'//
     -              'TPA_SC7  real null,'//
     -              'TPA_SC8  real null,'//
     -              'TPA_SC9  real null,'//
     -              'TPA_10   real null,'//
     -              'HOST1    real null,'//
     -              'HOST2    real null,'//
     -              'HOST3    real null,'//
     -              'HOST4    real null,'//
     -              'HOST5    real null,'//
     -              'HOST6    real null,'//
     -              'HOST7    real null,'//
     -              'HOST8    real null,'//
     -              'HOST9    real null,'//
     -              'HOST10   real null,'//
     -              'TKLD1    real null,'//
     -              'TKLD2    real null,'//
     -              'TKLD3    real null,'//
     -              'TKLD4    real null,'//
     -              'TKLD5    real null,'//
     -              'TKLD6    real null,'//
     -              'TKLD7    real null,'//
     -              'TKLD8    real null,'//
     -              'TKLD9    real null,'//
     -              'TKLD10   real null,'//
     -              'SPCL1    real null,'//
     -              'SPCL2    real null,'//
     -              'SPCL3    real null,'//
     -              'SPCL4    real null,'//
     -              'SPCL5    real null,'//
     -              'SPCL6    real null,'//
     -              'SPCL7    real null,'//
     -              'SPCL8    real null,'//
     -              'SPCL9    real null,'//
     -              'SPCL10   real null,'//
     -              'SAN1     real null,'//
     -              'SAN2     real null,'//
     -              'SAN3     real null,'//
     -              'SAN4     real null,'//
     -              'SAN5     real null,'//
     -              'SAN6     real null,'//
     -              'SAN7     real null,'//
     -              'SAN8     real null,'//
     -              'SAN9     real null,'//
     -              'SAN10    real null)'
        ENDIF
        !PRINT*, SQLStmtStr

            !Close Cursor
            iRet = fvsSQLCloseCursor(StmtHndlOut)

            iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSBMTREE:Creating Table: '//trim(SQLStmtStr))
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS & ARRAYS
C
      TPAb=TPA
      TPAHb=TPAH
      TPAKLLb=TPAKLL
      SPCLTREb=SPCLTRE
      SAN1B= SAN1
      SAN2B= SAN2
      SAN3B= SAN3
      SAN4B= SAN4
      SAN5B= SAN5
      SAN6B= SAN6
      SAN7B= SAN7
      SAN8B= SAN8
      SAN9B= SAN9
      SAN10B= SAN10
C
      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,
     - ' (CaseID,StandID,Year,',
     - 'TPA_SC1,TPA_SC2,TPA_SC3,TPA_SC4,TPA_SC5,',
     - 'TPA_SC6,TPA_SC7,TPA_SC8,TPA_SC9,TPA_10 ,',
     - 'HOST1  ,HOST2  ,HOST3  ,HOST4  ,HOST5  ,',
     - 'HOST6  ,HOST7  ,HOST8  ,HOST9  ,HOST10 ,',
     - 'TKLD1  ,TKLD2  ,TKLD3  ,TKLD4  ,TKLD5  ,',
     - 'TKLD6  ,TKLD7  ,TKLD8  ,TKLD9  ,TKLD10 ,',
     - 'SPCL1  ,SPCL2  ,SPCL3  ,SPCL4  ,SPCL5  ,',
     - 'SPCL6  ,SPCL7  ,SPCL8  ,SPCL9  ,SPCL10 ,',
     - 'SAN1   ,SAN2   ,SAN3   ,SAN4   ,SAN5   ,',
     - 'SAN6   ,SAN7   ,SAN8   ,SAN9   ,SAN10  ) VALUES("',
     - CID,'","',TRIM(NPLT),'",?,?,?,?,?,?,?,?,?,?,?,?,?,?",
     - ',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?',
     - ',?,?,?,?,?,?,?,?)'

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      DO X=1,NSCL
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),TPAB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

      DO X=1,NSCL
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),TPAHB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

      DO X=1,NSCL
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),TPAKLLB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

      DO X=1,NSCL
      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SPCLTREB(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      END DO

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN1B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN2B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN4B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN5B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN6B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN7B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN8B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN9B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SAN10B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

  100 CONTINUE

      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSBMTREE:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END

