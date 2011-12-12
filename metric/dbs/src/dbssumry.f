      SUBROUTINE DBSSUMRY(IYEAR,IAGE,NPLT,ITPA,IBA,ISDI,ICCF,
     -  TOPHT,FQMD,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -  IATBA,IATSDI,IATCCF,ATTOPHT,FATQMD,IPRDLEN,IACC,IMORT,YMAI,
     -  IFORTP,ISZCL,ISTCL)
C----------
C  **DBSUMSTAT--DBS/M  DATE OF LAST REVISION:  10/07/03
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              OUTPUT.
C     AUTH: D. GAMMEL -- SEM -- JUNE 2002
C     INPUT:
C     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
C              1: YEAR
C              2: AGE
C              3: TREES/HA
C              4: TOTAL CU M
C     *        4: MERCH CU M (PULP AND SAWLOG)
C              5: MERCH CU M
C     *        5: MERCH CU M (SAWLOG)
C              6: MERCH BD FT
C     *        6: MERCH BD FT (SAWLOG)
C              7: REMOVED TREES/HA
C              8: REMOVED TOTAL CU M
C     *        8: REMOVED MERCH CU M (PULP AND SAWLOG)
C              9: REMOVED MERCH CU M
C     *        9: REMOVED MERCH CU M (SAWLOG)
C             10: REMOVED MERCH BD FT
C     *       10: REMOVED MERCH BD FT (SAWLOG)
C             11: BASAL AREA/HA
C             12: CCF
C             13: AVERAGE DOMINATE HEIGHT
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (ANNUAL IN CU M/HA)
C             16: MORTALITY  (ANNUAL IN CU M/HA)
C             17: SAMPLE WEIGHT
C
C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
COMMONS
      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      IMPLICIT NONE
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER IYEAR,IAGE,IPRDLEN,IACC,IMORT,ITPA,IBA,ISDI,ICCF,
     -        ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -        IATBA,IATSDI,IATCCF,ID,IFORTP,ISZCL,ISTCL
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      DOUBLE PRECISION FQMDB,FATQMDB,YMAIB,TOPHTB,ATTOPHTB

      REAL FQMD,FATQMD,YMAI,TOPHT,ATTOPHT
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
      CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut,
     -                        iRet)
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
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3      (VVER(:2) .EQ. 'ON')) THEN

            TABLENAME = '[FVS_Summary_East$]'
        ELSE
            TABLENAME = '[FVS_Summary$]'
        ENDIF
      ELSE
        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1      (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3      (VVER(:2) .EQ. 'ON')) THEN

            TABLENAME = 'FVS_Summary_East'
        ELSE
            TABLENAME = 'FVS_Summary'
        ENDIF
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      !PRINT*, SQLStmtStr
      CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
C
      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
C
C  EASTERN VARIANT VOLUME NOMENCLATURE
C
        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1      (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3      (VVER(:2) .EQ. 'ON')) THEN
C
          IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
            SQLStmtStr='CREATE TABLE FVS_Summary_East('//
     -                 'Id int primary key,'//
     -                 'CaseID int not null,'//
     -                 'StandID text not null,'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'TPH double null,'//
     -                 'BA double null,'//
     -                 'SDI double null,'//
     -                 'CCF double null,'//
     -                 'TopHt double null,'//
     -                 'QMD double null,'//
     -                 'GTV double null,'//
     -                 'GMV double null,'//
     -                 'NMV double null,'//
     -                 'RTPH double null,'//
     -                 'RGTV double null,'//
     -                 'RGMV double null,'//
     -                 'RNMV double null,'//
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
     -                 'TPH NUMBER,'//
     -                 'BA NUMBER,'//
     -                 'SDI NUMBER,'//
     -                 'CCF NUMBER,'//
     -                 'TopHt NUMBER,'//
     -                 'QMD NUMBER,'//
     -                 'GTV NUMBER,'//
     -                 'GMV NUMBER,'//
     -                 'NMV NUMBER,'//
     -                 'RTPH NUMBER,'//
     -                 'RGTV NUMBER,'//
     -                 'RGMV NUMBER,'//
     -                 'RNMV NUMBER,'//
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
     -                 'TPH real null,'//
     -                 'BA real null,'//
     -                 'SDI real null,'//
     -                 'CCF real null,'//
     -                 'TopHt real null,'//
     -                 'QMD real null,'//
     -                 'GTV real null,'//
     -                 'GMV real null,'//
     -                 'NMV real null,'//
     -                 'RTPH real null,'//
     -                 'RGTV real null,'//
     -                 'RGMV real null,'//
     -                 'RNMV real null,'//
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
     -                 'TPH double null,'//
     -                 'BA double null,'//
     -                 'SDI double null,'//
     -                 'CCF double null,'//
     -                 'TopHt double null,'//
     -                 'QMD double null,'//
     -                 'TCuM double null,'//
     -                 'MCuM double null,'//
     -                 'BdFt double null,'//
     -                 'RTPH double null,'//
     -                 'RTCuM double null,'//
     -                 'RMCuM double null,'//
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
     -                 'TPH NUMBER,'//
     -                 'BA NUMBER,'//
     -                 'SDI NUMBER,'//
     -                 'CCF NUMBER,'//
     -                 'TopHt NUMBER,'//
     -                 'QMD NUMBER,'//
     -                 'TCuM NUMBER,'//
     -                 'MCuM NUMBER,'//
     -                 'BdFt NUMBER,'//
     -                 'RTPH NUMBER,'//
     -                 'RTCuM NUMBER,'//
     -                 'RMCuM NUMBER,'//
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
     -                 'TPH real null,'//
     -                 'BA real null,'//
     -                 'SDI real null,'//
     -                 'CCF real null,'//
     -                 'TopHt real null,'//
     -                 'QMD real null,'//
     -                 'TCuM real null,'//
     -                 'MCuM real null,'//
     -                 'BdFt real null,'//
     -                 'RTPH real null,'//
     -                 'RTCuM real null,'//
     -                 'RMCuM real null,'//
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
        !PRINT*, SQLStmtStr

            !Close Cursor
            CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)

            CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                     'DBSSumry:Creating Table')
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
      TOPHTB=TOPHT
      ATTOPHTB=ATTOPHT

      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3    (VVER(:2) .EQ. 'ON')) THEN
C
        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,StandID,
     -              Year,Age,TPH,BA,SDI,CCF,TopHt,QMD,GTV,GMV,NMV,
     -              RTPH,RGTV,RGMV,RNMV,ATBA,ATSDI,ATCCF,ATTopHt,
     -              ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)
     -              VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,
     -              ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
      ELSE
        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,StandID,
     -              Year,Age,TPH,BA,SDI,CCF,TopHt,QMD,TCuM,MCuM,BdFt,
     -              RTPH,RTCuM,RMCuM,RBdFt,ATBA,ATSDI,ATCCF,ATTopHt,
     -              ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)
     -              VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,
     -              ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
      ENDIF
C
C     CLOSE CURSOR
C
      CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
C
C     PREPARE THE SQL QUERY
C
      CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),SUMRYID,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IAGE,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ITPA,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IBA,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ISDI,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ICCF,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),TOPHTB,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),FQMDB,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ITCUFT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IMCUFT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IBDFT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRTPA,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRTCUFT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRMCUFT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IRBDFT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATBA,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATSDI,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IATCCF,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),ATTOPHTB,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
       CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),FATQMDB,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IPRDLEN,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IACC,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IMORT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),YMAIB,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IFORTP,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ISZCL,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1
      CALL f90SQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),ISTCL,f90SQL_NULL_PTR,iRet)

      !PRINT*, SQLStmtStr
  100 CONTINUE
      !Close Cursor
      CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)

      CALL f90SQLExecute(StmtHndlOut,iRet)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSSumry:Inserting Row')

  200 CONTINUE
      !Release statement handle
      CALL f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut, iRet)

      END


