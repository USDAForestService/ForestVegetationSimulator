      SUBROUTINE DBSSTRCLASS(IYEAR,NPLT,RCODE,S1DBH,S1NHT,S1LHT,S1SHT,
     &  S1CB,S1CC,S1MS1,S1MS2,S1SC,S2DBH,S2NHT,S2LHT,S2SHT,S2CB,S2CC,
     &  S2MS1,S2MS2,S2SC,S3DBH,S3NHT,S3LHT,S3SHT,S3CB,S3CC,S3MS1,S3MS2,
     &  S3SC,NS,TOTCOV,SCLASS,KODE,NTREES)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE STRUCTURE CLASS OUTPUT.
C     AUTH: S. REBAIN -- FMSC -- AUGUST 2005
C     INPUT:
C            THE STRUCTURE CLASS OUTPUT.
C              1: REMOVAL CODE
C              2: STRATUM 1 DBH
C              3: STRATUM 1 NOMINAL HEIGHT
C              4: STRATUM 1 LARGEST HEIGHT
C              5: STRATUM 1 SMALLEST HEIGHT
C              6: STRATUM 1 CROWN BASE
C              7: STRATUM 1 CROWN COVER
C              8: STRATUM 1 MAJOR SPECIES 1
C              9: STRATUM 1 MAJOR SPECIES 2
C             10: STRATUM 1 STATUS CODE
C             11: STRATUM 2 DBH
C             12: STRATUM 2 NOMINAL HEIGHT
C             13: STRATUM 2 LARGEST HEIGHT
C             14: STRATUM 2 SMALLEST HEIGHT
C             15: STRATUM 2 CROWN BASE
C             16: STRATUM 2 CROWN COVER
C             17: STRATUM 2 MAJOR SPECIES 1
C             18: STRATUM 2 MAJOR SPECIES 2
C             19: STRATUM 2 STATUS CODE
C             20: STRATUM 3 DBH
C             21: STRATUM 3 NOMINAL HEIGHT
C             22: STRATUM 3 LARGEST HEIGHT
C             23: STRATUM 3 SMALLEST HEIGHT
C             24: STRATUM 3 CROWN BASE
C             25: STRATUM 3 CROWN COVER
C             26: STRATUM 3 MAJOR SPECIES 1
C             27: STRATUM 3 MAJOR SPECIES 2
C             28: STRATUM 3 STATUS CODE
C             29: NUMBER OF STRATA
C             30: TOTAL COVER
C             31: STRUCTURE CLASS
C             32: KODE FOR WHETHER OR NOT THE REPORT ALSO DUMPS TO FILE
C             33: THE NUMBER OF TREE RECORDS
C
C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C

      INTEGER IYEAR,ID,KODE,RCODE,S1NHT,S1LHT,S1SHT,S1CB,S1CC,S1SC
      INTEGER S2NHT,S2LHT,S2SHT,S2CB,S2CC,S3NHT,S3LHT,S3SHT,S3CB,S3CC
      INTEGER S2SC, S3SC, TOTCOV,NS, NTREES
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL S1DBH,S2DBH,S3DBH
      DOUBLE PRECISION BS1DBH,BS2DBH,BS3DBH
      CHARACTER*2000 SQLStmtStr
      CHARACTER*3 S1MS1,S1MS2,S2MS1,S2MS2,S3MS1,S3MS2
      CHARACTER*4 SCLASS
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
C
C
COMMONS END

C---
C     Initialize variables
C
      IF (ISTRCLAS .EQ. 0) RETURN
      IF (ISTRCLAS .EQ. 2) KODE = 0
      IF (NTREES .EQ. 0) RETURN
C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ISTRCLAS = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSSTRCLASS:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE STRUCTURE CLASS TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_StrClass$]'
      ELSE
        TABLENAME = 'FVS_StrClass'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))


      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN

        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_StrClass('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID Text null,'//
     -              'Year int null,'//
     -              'Removal_Code double null,'//
     -              'Stratum_1_DBH double null,'//
     -              'Stratum_1_Nom_Ht double null,'//
     -              'Stratum_1_Lg_Ht double null,'//
     -              'Stratum_1_Sm_Ht double null,'//
     -              'Stratum_1_Crown_Base double null,'//
     -              'Stratum_1_Crown_Cover double null,'//
     -              'Stratum_1_Species_1 text null,'//
     -              'Stratum_1_Species_2 text null,'//
     -              'Stratum_1_Status_Code double null,'//
     -              'Stratum_2_DBH double null,'//
     -              'Stratum_2_Nom_Ht double null,'//
     -              'Stratum_2_Lg_Ht double null,'//
     -              'Stratum_2_Sm_Ht double null,'//
     -              'Stratum_2_Crown_Base double null,'//
     -              'Stratum_2_Crown_Cover double null,'//
     -              'Stratum_2_Species_1 text null,'//
     -              'Stratum_2_Species_2 text null,'//
     -              'Stratum_2_Status_Code double null,'//
     -              'Stratum_3_DBH double null,'//
     -              'Stratum_3_Nom_Ht double null,'//
     -              'Stratum_3_Lg_Ht double null,'//
     -              'Stratum_3_Sm_Ht double null,'//
     -              'Stratum_3_Crown_Base double null,'//
     -              'Stratum_3_Crown_Cover double null,'//
     -              'Stratum_3_Species_1 text null,'//
     -              'Stratum_3_Species_2 text null,'//
     -              'Stratum_3_Status_Code double null,'//
     -              'Number_of_Strata double null,'//
     -              'Total_Cover double null,'//
     -              'Structure_Class text null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_StrClass('//
     -              'Id int,'//
     -              'CaseID int,'//
     -              'StandID Text,'//
     -              'Year int,'//
     -              'Removal_Code Number,'//
     -              'Stratum_1_DBH Number,'//
     -              'Stratum_1_Nom_Ht Number,'//
     -              'Stratum_1_Lg_Ht Number,'//
     -              'Stratum_1_Sm_Ht Number,'//
     -              'Stratum_1_Crown_Base Number,'//
     -              'Stratum_1_Crown_Cover Number,'//
     -              'Stratum_1_Species_1 Text,'//
     -              'Stratum_1_Species_2 Text,'//
     -              'Stratum_1_Status_Code Number,'//
     -              'Stratum_2_DBH Number,'//
     -              'Stratum_2_Nom_Ht Number,'//
     -              'Stratum_2_Lg_Ht Number,'//
     -              'Stratum_2_Sm_Ht Number,'//
     -              'Stratum_2_Crown_Base Number,'//
     -              'Stratum_2_Crown_Cover Number,'//
     -              'Stratum_2_Species_1 Text,'//
     -              'Stratum_2_Species_2 Text,'//
     -              'Stratum_2_Status_Code Number,'//
     -              'Stratum_3_DBH Number,'//
     -              'Stratum_3_Nom_Ht Number,'//
     -              'Stratum_3_Lg_Ht Number,'//
     -              'Stratum_3_Sm_Ht Number,'//
     -              'Stratum_3_Crown_Base Number,'//
     -              'Stratum_3_Crown_Cover Number,'//
     -              'Stratum_3_Species_1 Text,'//
     -              'Stratum_3_Species_2 Text,'//
     -              'Stratum_3_Status_Code Number,'//
     -              'Number_of_Strata Number,'//
     -              'Total_Cover Number,'//
     -              'Structure_Class Text)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_StrClass('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID char(26) null,'//
     -              'Year int null,'//
     -              'Removal_Code real null,'//
     -              'Stratum_1_DBH real null,'//
     -              'Stratum_1_Nom_Ht real null,'//
     -              'Stratum_1_Lg_Ht real null,'//
     -              'Stratum_1_Sm_Ht real null,'//
     -              'Stratum_1_Crown_Base real null,'//
     -              'Stratum_1_Crown_Cover real null,'//
     -              'Stratum_1_Species_1 char(3) null,'//
     -              'Stratum_1_Species_2 char(3) null,'//
     -              'Stratum_1_Status_Code real null,'//
     -              'Stratum_2_DBH real null,'//
     -              'Stratum_2_Nom_Ht real null,'//
     -              'Stratum_2_Lg_Ht real null,'//
     -              'Stratum_2_Sm_Ht real null,'//
     -              'Stratum_2_Crown_Base real null,'//
     -              'Stratum_2_Crown_Cover real null,'//
     -              'Stratum_2_Species_1 char(3) null,'//
     -              'Stratum_2_Species_2 char(3) null,'//
     -              'Stratum_2_Status_Code real null,'//
     -              'Stratum_3_DBH real null,'//
     -              'Stratum_3_Nom_Ht real null,'//
     -              'Stratum_3_Lg_Ht real null,'//
     -              'Stratum_3_Sm_Ht real null,'//
     -              'Stratum_3_Crown_Base real null,'//
     -              'Stratum_3_Crown_Cover real null,'//
     -              'Stratum_3_Species_1 char(3) null,'//
     -              'Stratum_3_Species_2 char(3) null,'//
     -              'Stratum_3_Status_Code real null,'//
     -              'Number_of_Strata real null,'//
     -              'Total_Cover real null,'//
     -              'Structure_Class char(4) null)'
        ENDIF

        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -       'DBSSTRCLASS:Creating Table: '//trim(SQLStmtStr))
        STRCLID = 0
      ENDIF

C---------
C     CREATE ENTRY FROM DATA FOR STRUCTURE CLASS TABLE
C---------
      IF(STRCLID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        STRCLID = ID
      ENDIF
      STRCLID = STRCLID + 1
C----------
C     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C----------
      IF(STRCLID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

      BS1DBH=0D0
      BS2DBH=0D0
      BS3DBH=0D0
      BS1DBH=S1DBH
      BS2DBH=S2DBH
      BS3DBH=S3DBH

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,
     -       StandID,Year,Removal_Code,Stratum_1_DBH,Stratum_1_Nom_Ht,
     -       Stratum_1_Lg_Ht,Stratum_1_Sm_Ht,Stratum_1_Crown_Base,
     -       Stratum_1_Crown_Cover,Stratum_1_Species_1,
     -       Stratum_1_Species_2,Stratum_1_Status_Code,Stratum_2_DBH,
     -       Stratum_2_Nom_Ht,Stratum_2_Lg_Ht,Stratum_2_Sm_Ht,
     -       Stratum_2_Crown_Base,Stratum_2_Crown_Cover,
     -       Stratum_2_Species_1,Stratum_2_Species_2,
     -       Stratum_2_Status_Code,Stratum_3_DBH,Stratum_3_Nom_Ht,
     -       Stratum_3_Lg_Ht,Stratum_3_Sm_Ht,Stratum_3_Crown_Base,
     -       Stratum_3_Crown_Cover,Stratum_3_Species_1,
     -       Stratum_3_Species_2,Stratum_3_Status_Code,Number_of_Strata,
     -       Total_Cover,Structure_Class)
     -       VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,?,?,?,?,
     -       ?,',CHAR(39),S1MS1,CHAR(39),',',CHAR(39),S1MS2,CHAR(39),',
     -       ?,?,?,?,?,?,?,',CHAR(39),S2MS1,CHAR(39),',
     -       ',CHAR(39),S2MS2,CHAR(39),',?,?,?,?,?,?,?,
     -       ',CHAR(39),S3MS1,CHAR(39),',',CHAR(39),S3MS2,CHAR(39),',?,
     -       ?,?,',CHAR(39),SCLASS,CHAR(39),')'
C
C     CLOSE CURSOR
C
      iRet = fvsSQLCloseCursor(StmtHndlOut)
C
C     PREPARE THE SQL QUERY
C
      iRet = fvsSQLPrepare(StmtHndlOut, trim (SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(0,SQLSMALLINT_KIND),STRCLID,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(0,SQLSMALLINT_KIND),ICASE,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),RCODE,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),BS1DBH,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S1NHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S1LHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S1SHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S1CB,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S1CC,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S1SC,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),BS2DBH,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S2NHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S2LHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S2SHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S2CB,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S2CC,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S2SC,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),BS3DBH,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S3NHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S3LHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S3SHT,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S3CB,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S3CC,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),S3SC,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),NS,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -          SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -          INT(5,SQLSMALLINT_KIND),TOTCOV,int(4,SQLLEN_KIND),
     -          SQL_NULL_PTR)

  100 CONTINUE
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSSTRCLASS:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


