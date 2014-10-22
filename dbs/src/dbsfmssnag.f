      SUBROUTINE DBSFMSSNAG(IYEAR,NPLT,HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,
     -  HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,SCL6,SCL7,HDSF,KODE)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE SUMMARY SNAG REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE SUMMARY SNAG OUTPUT FROM THE FIRE MODEL.
C              1: HARD SNAGS - DBH CLASS 1 (> 0" BY DEFAULT)
C              2: HARD SNAGS - DBH CLASS 2 (> 12" BY DEFAULT)
C              3: HARD SNAGS - DBH CLASS 3 (> 18" BY DEFAULT)
C              4: HARD SNAGS - DBH CLASS 4 (> 24" BY DEFAULT)
C              5: HARD SNAGS - DBH CLASS 5 (> 30" BY DEFAULT)
C              6: HARD SNAGS - DBH CLASS 6 (> 36" BY DEFAULT)
C              7: HARD SNAGS - TOTAL       (> 0")
C              8: SOFT SNAGS - DBH CLASS 1 (> 0" BY DEFAULT)
C              9: SOFT SNAGS - DBH CLASS 2 (> 12" BY DEFAULT)
C             10: SOFT SNAGS - DBH CLASS 3 (> 18" BY DEFAULT)
C             11: SOFT SNAGS - DBH CLASS 4 (> 24" BY DEFAULT)
C             12: SOFT SNAGS - DBH CLASS 5 (> 30" BY DEFAULT)
C             13: SOFT SNAGS - DBH CLASS 6 (> 36" BY DEFAULT)
C             14: SOFT SNAGS - TOTAL       (> 0")
C             15: HARD+SOFT  - TOTAL       (> 0")
C             16: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
C     NOTE: THE DBH CLASS BREAKS CAN BE CHANGED BY THE SNAGCLAS KEYWORD
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,KODE
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,
     -  SCL6,SCL7,HDSF
      DOUBLE PRECISION HCL1B,HCL2B,HCL3B,HCL4B,HCL5B,HCL6B,HCL7B,
     -  SCL1B,SCL2B,SCL3B,SCL4B,SCL5B,SCL6B,SCL7B,HDSFB
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT

C     Initialize variables

      IF(ISSUM.EQ.0) RETURN
      IF(ISSUM.EQ.2) KODE = 0

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ISSUM = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSFMSSNAG:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE SUMMARY SNAG TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_SnagSum$]'
      ELSE
        TABLENAME = 'FVS_SnagSum'
      ENDIF
      SQLStmtStr= 'SELECT Count(*) FROM ' // TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_SnagSum('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Hard_snags_class1 double null,'//
     -              'Hard_snags_class2 double null,'//
     -              'Hard_snags_class3 double null,'//
     -              'Hard_snags_class4 double null,'//
     -              'Hard_snags_class5 double null,'//
     -              'Hard_snags_class6 double null,'//
     -              'Hard_snags_total  double null,'//
     -              'Soft_snags_class1 double null,'//
     -              'Soft_snags_class2 double null,'//
     -              'Soft_snags_class3 double null,'//
     -              'Soft_snags_class4 double null,'//
     -              'Soft_snags_class5 double null,'//
     -              'Soft_snags_class6 double null,'//   
     -              'Soft_snags_total  double null,'//   
     -              'Hard_soft_snags_total double null)'   

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_SnagSum('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'Hard_snags_class1 Number,'//
     -              'Hard_snags_class2 Number,'//
     -              'Hard_snags_class3 Number,'//
     -              'Hard_snags_class4 Number,'//
     -              'Hard_snags_class5 Number,'//
     -              'Hard_snags_class6 Number,'//
     -              'Hard_snags_total  Number,'//
     -              'Soft_snags_class1 Number,'//
     -              'Soft_snags_class2 Number,'//
     -              'Soft_snags_class3 Number,'//
     -              'Soft_snags_class4 Number,'//
     -              'Soft_snags_class5 Number,'//
     -              'Soft_snags_class6 Number,'//
     -              'Soft_snags_total  Number,'//
     -              'Hard_soft_snags_total Number)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_SnagSum('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Hard_snags_class1 real null,'//
     -              'Hard_snags_class2 real null,'//
     -              'Hard_snags_class3 real null,'//
     -              'Hard_snags_class4 real null,'//
     -              'Hard_snags_class5 real null,'//
     -              'Hard_snags_class6 real null,'//
     -              'Hard_snags_total  real null,'//
     -              'Soft_snags_class1 real null,'//
     -              'Soft_snags_class2 real null,'//
     -              'Soft_snags_class3 real null,'//
     -              'Soft_snags_class4 real null,'//
     -              'Soft_snags_class5 real null,'//
     -              'Soft_snags_class6 real null,'//
     -              'Soft_snags_total  real null,'//
     -              'Hard_soft_snags_total real null)'
        ENDIF

            iRet = fvsSQLCloseCursor(StmtHndlOut)
            iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSFMSSNAG:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      HCL1B = HCL1
      HCL2B = HCL2
      HCL3B = HCL3
      HCL4B = HCL4
      HCL5B = HCL5
      HCL6B = HCL6
      HCL7B = HCL7
      SCL1B = SCL1
      SCL2B = SCL2
      SCL3B = SCL3
      SCL4B = SCL4
      SCL5B = SCL5
      SCL6B = SCL6
      SCL7B = SCL7
      HDSFB = HDSF

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,',
     -  'StandID,Year,Hard_snags_class1,Hard_snags_class2,',
     -  'Hard_snags_class3,Hard_snags_class4,Hard_snags_class5,',
     -  'Hard_snags_class6,Hard_snags_total,Soft_snags_class1,',
     -  'Soft_snags_class2,Soft_snags_class3,Soft_snags_class4,',
     -  'Soft_snags_class5,Soft_snags_class6,Soft_snags_total,',
     -  'Hard_soft_snags_total) VALUES("',CASEID,'","',TRIM(NPLT),
     -  '",?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

C
C     CLOSE CURSOR
C
      iRet = fvsSQLCloseCursor(StmtHndlOut)
C
C     PREPARE THE SQL QUERY
C
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C

      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),HCL1B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),HCL2B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),HCL3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),HCL4B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),HCL5B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),HCL6B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),HCL7B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL1B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL2B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL4B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL5B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL6B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),SCL7B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),HDSFB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
  100 CONTINUE
      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecute(StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS) ISSUM=0

      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSFMSSNAG:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


