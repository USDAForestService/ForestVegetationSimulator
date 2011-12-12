      SUBROUTINE DBSSUMRY(IYEAR,IAGE,NPLT,ITPA,IBA,ISDI,ICCF,
     -  ITOPHT,FQMD,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -  IATBA,IATSDI,IATCCF,IATTOPHT,FATQMD,IPRDLEN,IACC,IMORT,YMAI,
     -  IFORTP,ISZCL,ISTCL)
      IMPLICIT NONE
C----------
C  **DBSUMSTAT--DBS  DATE OF LAST REVISION: 10/31/2011
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              OUTPUT.
C     AUTH: D. GAMMEL -- SEM -- JUNE 2002
C     INPUT:
C     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
C              1: YEAR
C              2: AGE
C              3: TREES/ACRE
C              4: TOTAL CU FT
C     *        4: MERCH CU FT (PULP AND SAWLOG)
C              5: MERCH CU FT
C     *        5: MERCH CU FT (SAWLOG)
C              6: MERCH BD FT
C     *        6: MERCH BD FT (SAWLOG)
C              7: REMOVED TREES/ACRE
C              8: REMOVED TOTAL CU FT
C     *        8: REMOVED MERCH CU FT (PULP AND SAWLOG)
C              9: REMOVED MERCH CU FT
C     *        9: REMOVED MERCH CU FT (SAWLOG)
C             10: REMOVED MERCH BD FT
C     *       10: REMOVED MERCH BD FT (SAWLOG)
C             11: BASAL AREA/ACRE
C             12: CCF
C             13: AVERAGE DOMINATE HEIGHT
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (ANNUAL IN CU FT/ACRE)
C             16: MORTALITY  (ANNUAL IN CU FT/ACRE)
C             17: SAMPLE WEIGHT
C
C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER IYEAR,IAGE,IPRDLEN,IACC,IMORT,ITPA,IBA,ISDI,ICCF,
     -        ITOPHT,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -        IATBA,IATSDI,IATCCF,IATTOPHT,ID,IFORTP,ISZCL,ISTCL
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      DOUBLE PRECISION FQMDB,FATQMDB,YMAIB
      REAL FQMD,FATQMD,YMAI
      CHARACTER*2000 SQLStmtStr
      CHARACTER*7   VVER
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=*) NPLT
C
C
COMMONS END
C---
C     Initialize variables
C
      IF(ISUMARY.EQ.0) RETURN
C
      CALL VARVER (VVER)

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ISUMARY = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSSUMRY:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE SUMMARY STATS TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1      (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN

            TABLENAME = '[FVS_Summary_East$]'
        ELSE
            TABLENAME = '[FVS_Summary$]'
        ENDIF
      ELSE
        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1      (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN

            TABLENAME = 'FVS_Summary_East'
        ELSE
            TABLENAME = 'FVS_Summary'
        ENDIF
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
C
C  EASTERN VARIANT VOLUME NOMENCLATURE
C
        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1      (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
C
          IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
            SQLStmtStr='CREATE TABLE FVS_Summary_East('//
     -                 'Id int primary key,'//
     -                 'CaseID int not null,'//
     -                 'StandID text not null,'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'Tpa double null,'//
     -                 'BA double null,'//
     -                 'SDI double null,'//
     -                 'CCF double null,'//
     -                 'TopHt double null,'//
     -                 'QMD double null,'//
     -                 'MCuFt double null,'//
     -                 'SCuFt double null,'//
     -                 'SBdFt double null,'//
     -                 'RTpa double null,'//
     -                 'RMCuFt double null,'//
     -                 'RSCuFt double null,'//
     -                 'RSBdFt double null,'//
     -                 'ATBA double null,'//
     -                 'ATSDI double null,'//
     -                 'ATCCF double null,'//
     -                 'ATTopHt double null,'//
     -                 'ATQMD double null,'//
     -                 'PrdLen int null,'//
     -                 'Acc double null,'//
     -                 'Mort double null,'//
     -                 'MAI double null,'//
     -                 'ForTyp int null,'//
     -                 'SizeCls int null,'//
     -                 'StkCls int null)'
          ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
            SQLStmtStr="CREATE TABLE FVS_Summary_East("//
     -                 'Id int,'//
     -                 'CaseID int,'//
     -                 'StandID Text,'//
     -                 'Year int,'//
     -                 'Age int,'//
     -                 'Tpa NUMBER,'//
     -                 'BA NUMBER,'//
     -                 'SDI NUMBER,'//
     -                 'CCF NUMBER,'//
     -                 'TopHt NUMBER,'//
     -                 'QMD NUMBER,'//
     -                 'MCuFt NUMBER,'//
     -                 'SCuFt NUMBER,'//
     -                 'SBdFt NUMBER,'//
     -                 'RTpa NUMBER,'//
     -                 'RMCuFt NUMBER,'//
     -                 'RSCuFt NUMBER,'//
     -                 'RSBdFt NUMBER,'//
     -                 'ATBA NUMBER,'//
     -                 'ATSDI NUMBER,'//
     -                 'ATCCF NUMBER,'//
     -                 'ATTopHt NUMBER,'//
     -                 'ATQMD NUMBER,'//
     -                 'PrdLen int,'//
     -                 'Acc NUMBER,'//
     -                 'Mort NUMBER,'//
     -                 'MAI NUMBER,'//
     -                 'ForTyp int,'//
     -                 'SizeCls int,'//
     -                 'StkCls int)'
C
          ELSE
            SQLStmtStr='CREATE TABLE FVS_Summary_East('//
     -                 'Id int primary key,'//
     -                 'CaseID int not null,'//
     -                 'StandID Char(26) null,'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'Tpa real null,'//
     -                 'BA real null,'//
     -                 'SDI real null,'//
     -                 'CCF real null,'//
     -                 'TopHt real null,'//
     -                 'QMD real null,'//
     -                 'MCuFt real null,'//
     -                 'SCuFt real null,'//
     -                 'SBdFt real null,'//
     -                 'RTpa real null,'//
     -                 'RMCuFt real null,'//
     -                 'RSCuFt real null,'//
     -                 'RSBdFt real null,'//
     -                 'ATBA real null,'//
     -                 'ATSDI real null,'//
     -                 'ATCCF real null,'//
     -                 'ATTopHt real null,'//
     -                 'ATQMD real null,'//
     -                 'PrdLen int null,'//
     -                 'Acc real null,'//
     -                 'Mort real null,'//
     -                 'MAI real null,'//
     -                 'ForTyp int null,'//
     -                 'SizeCls int null,'//
     -                 'StkCls int null)'
          ENDIF
        ELSE
C----------
C  WESTERN VARIANT VOLUME NOMENCLATURE
C----------
          IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
            SQLStmtStr='CREATE TABLE FVS_Summary('//
     -                 'Id int primary key,'//
     -                 'CaseID int not null,'//
     -                 'StandID text not null,'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'Tpa double null,'//
     -                 'BA double null,'//
     -                 'SDI double null,'//
     -                 'CCF double null,'//
     -                 'TopHt double null,'//
     -                 'QMD double null,'//
     -                 'TCuFt double null,'//
     -                 'MCuFt double null,'//
     -                 'BdFt double null,'//
     -                 'RTpa double null,'//
     -                 'RTCuFt double null,'//
     -                 'RMCuFt double null,'//
     -                 'RBdFt double null,'//
     -                 'ATBA double null,'//
     -                 'ATSDI double null,'//
     -                 'ATCCF double null,'//
     -                 'ATTopHt double null,'//
     -                 'ATQMD double null,'//
     -                 'PrdLen int null,'//
     -                 'Acc double null,'//
     -                 'Mort double null,'//
     -                 'MAI double null,'//
     -                 'ForTyp int null,'//
     -                 'SizeCls int null,'//
     -                 'StkCls int null)'
          ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
            SQLStmtStr="CREATE TABLE FVS_Summary("//
     -                 'Id int,'//
     -                 'CaseID int,'//
     -                 'StandID Text,'//
     -                 'Year int,'//
     -                 'Age int,'//
     -                 'Tpa NUMBER,'//
     -                 'BA NUMBER,'//
     -                 'SDI NUMBER,'//
     -                 'CCF NUMBER,'//
     -                 'TopHt NUMBER,'//
     -                 'QMD NUMBER,'//
     -                 'TCuFt NUMBER,'//
     -                 'MCuFt NUMBER,'//
     -                 'BdFt NUMBER,'//
     -                 'RTpa NUMBER,'//
     -                 'RTCuFt NUMBER,'//
     -                 'RMCuFt NUMBER,'//
     -                 'RBdFt NUMBER,'//
     -                 'ATBA NUMBER,'//
     -                 'ATSDI NUMBER,'//
     -                 'ATCCF NUMBER,'//
     -                 'ATTopHt NUMBER,'//
     -                 'ATQMD NUMBER,'//
     -                 'PrdLen int,'//
     -                 'Acc NUMBER,'//
     -                 'Mort NUMBER,'//
     -                 'MAI NUMBER,'//
     -                 'ForTyp int,'//
     -                 'SizeCls int,'//
     -                 'StkCls int)'
          ELSE
            SQLStmtStr='CREATE TABLE FVS_Summary('//
     -                 'Id int primary key,'//
     -                 'CaseID int not null,'//
     -                 'StandID Char(26) null,'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'Tpa real null,'//
     -                 'BA real null,'//
     -                 'SDI real null,'//
     -                 'CCF real null,'//
     -                 'TopHt real null,'//
     -                 'QMD real null,'//
     -                 'TCuFt real null,'//
     -                 'MCuFt real null,'//
     -                 'BdFt real null,'//
     -                 'RTpa real null,'//
     -                 'RTCuFt real null,'//
     -                 'RMCuFt real null,'//
     -                 'RBdFt real null,'//
     -                 'ATBA real null,'//
     -                 'ATSDI real null,'//
     -                 'ATCCF real null,'//
     -                 'ATTopHt real null,'//
     -                 'ATQMD real null,'//
     -                 'PrdLen int null,'//
     -                 'Acc real null,'//
     -                 'Mort real null,'//
     -                 'MAI real null,'//
     -                 'ForTyp int null,'//
     -                 'SizeCls int null,'//
     -                 'StkCls int null)'
          ENDIF
        ENDIF
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                 'DBSSumry:Creating Table')
        SUMRYID = 0
      ENDIF

C---------
C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE
C---------
      IF(SUMRYID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        SUMRYID = ID
      ENDIF
      SUMRYID = SUMRYID + 1
C----------
C     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C----------
      IF(SUMRYID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      FQMDB=FQMD
      FATQMDB=FATQMD
      YMAIB=YMAI

      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
C
        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,StandID,
     -              Year,Age,Tpa,BA,SDI,CCF,TopHt,QMD,MCuFt,SCuFt,SBdFt,
     -              RTpa,RMCuFt,RSCuFt,RSBdFt,ATBA,ATSDI,ATCCF,ATTopHt,
     -              ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)
     -              VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,
     -              ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
      ELSE
        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,StandID,
     -              Year,Age,Tpa,BA,SDI,CCF,TopHt,QMD,TCuFt,MCuFt,BdFt,
     -              RTpa,RTCuFt,RMCuFt,RBdFt,ATBA,ATSDI,ATCCF,ATTopHt,
     -              ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)
     -              VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,
     -              ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
      ENDIF
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
     -           INT(0,SQLSMALLINT_KIND),SUMRYID,int(4,SQLLEN_KIND),
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
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IAGE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ITPA,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IBA,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ISDI,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ICCF,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ITOPHT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

       ColNumber=ColNumber+1
       iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),FQMDB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ITCUFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IMCUFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IBDFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRTPA,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRTCUFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRMCUFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRBDFT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATBA,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATSDI,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATCCF,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATTOPHT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

       ColNumber=ColNumber+1
       iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),FATQMDB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IPRDLEN,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IACC,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IMORT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

       ColNumber=ColNumber+1
       iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),YMAIB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IFORTP,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ISZCL,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ISTCL,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

  100 CONTINUE
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSSumry:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


