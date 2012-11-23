      SUBROUTINE DBSMIS1(IYEAR,NPLT,CSP,
     -  SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4,
     -  KODE)
C----------
C  **DBSMIS1--DBS  DATE OF LAST REVISION:  08/31/04
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST MISTLETOE REPORT INFORMATION
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
C
C     1:     Year
C     2:     Mean DMR for species - SPDMR4
C     3:     Mean DMI for species - SPDMI4
C     4:     Infected trees/acre for species - SPINF4
C     5:     Mortality trees/acre for species - SPMRT4
C     6:     Infected trees/acre % for species - SPPIN4
C     7:     Mortality trees/acre % for species - SPPMR4
C     8:     Stand trees/acre % for species - SPPOC4

      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      IMPLICIT NONE

      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
      REAL    SPPIN4(4),SPPMR4(4),SPPOC4(4)
	INTEGER KODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber
	INTEGER           ID,I

      DOUBLE PRECISION  SPDMR4B, SPDMI4B
	INTEGER           ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME
      CHARACTER(LEN=26) NPLT
      CHARACTER(LEN=2)  CSP(4)

C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE

C     INITIALIZE VARIABLES

      IF(IDM1.EQ.0) RETURN
      IF(IDM1.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut,
     -  iRet)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDM1 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSMIS1:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_DM_Spp_Sum$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_DM_Spp_Sum"'
      ELSE
        TABLENAME = 'FVS_DM_Spp_Sum'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -  iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID text null,'//
     -      'Year Int null,'//
     -      'Spp Text null,'//
     -      'Mean_DMR double null,'//
     -      'Mean_DMI double null,'//
     -      'Inf_TPA int null,'//
     -      'Mort_TPA int null,'//
     -      'Inf_TPA_Pct int null,'//
     -      'Mort_TPA_Pct int null,'//
     -      'Stnd_TPA_Pct int null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('//
     -      'ID Int,'//
     -      'CaseID Int,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'Spp Text,'//
     -      'Mean_DMR Number,'//
     -      'Mean_DMI Number,'//
     -      'Inf_TPA Int,'//
     -      'Mort_TPA Int,'//
     -      'Inf_TPA_Pct Int,'//
     -      'Mort_TPA_Pct Int,'//
     -      'Stnd_TPA_Pct Int)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID char(26) not null,'//
     -      'Year Int null,'//
     -      'Spp char(2) null,'//
     -      'Mean_DMR real null,'//
     -      'Mean_DMI real null,'//
     -      'Inf_TPA int null,'//
     -      'Mort_TPA int  null,'//
     -      'Inf_TPA_Pct int null,'//
     -      'Mort_TPA_Pct int null,'//
     -      'Stnd_TPA_Pct int null)'
        ENDIF

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
        CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS1:Creating Table: '//trim(SQLStmtStr))
        DM1ID = 0
      ENDIF

C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

      IF(DM1ID .EQ. -1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        DM1ID = ID
      ENDIF

C     LOOP OVER 4 TOP SPECIES

      DO I = 1,4

C       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

        DM1ID = DM1ID + 1
        IF(DM1ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
C       INTEGER COPIES OF REAL VECTOR INPUTS

        SPDMR4B = SPDMR4(I)
        SPDMI4B = SPDMI4(I)
		
        ISPINF4 = NINT(SPINF4(I))
        ISPMRT4 = NINT(SPMRT4(I))
        ISPPIN4 = NINT(SPPIN4(I))
        ISPPMR4 = NINT(SPPMR4(I))
        ISPPOC4 = NINT(SPPOC4(I))

        WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID,
     -    StandID,Year,Spp,Mean_DMR,Mean_DMI,Inf_TPA,
     -    Mort_TPA,Inf_TPA_Pct,Mort_TPA_Pct,Stnd_TPA_Pct) VALUES 
     -    (?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,
     -    ',CHAR(39),TRIM(CSP(I)),CHAR(39),',?,?,?,?,?,?,?)'

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

C       PREPARE QUERY

        CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 ID
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),DM1ID,f90SQL_NULL_PTR,iRet) 

        ColNumber=ColNumber+1       ! 2 CASEID
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 3 YEAR
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 4 MEAN DMR
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),SPDMR4B,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 5 MEAN DMI
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),SPDMI4B,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 6 TPA INFECTED
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPINF4,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 7 TPA MORTALITY
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPMRT4,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 8 % TPA INFECTED
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPPIN4,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 9 % TPA MORTALITY
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPPMR4,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 10 % STAND TPA
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPPOC4,f90SQL_NULL_PTR,iRet)

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
        CALL f90SQLExecute(StmtHndlOut,iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS1:Inserting Row')

	ENDDO

C     RELEASE STATEMENT HANDLE

  100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

	RETURN
      END

C-------------------------------------------------------------------------------

      SUBROUTINE DBSMIS2(IYEAR,NPLT,NAGE,
     -  ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM,
     -  ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)
C----------
C  **DBSMIS2--DBS  DATE OF LAST REVISION:  04/20/04
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND MISTLETOE REPORT INFORMATION
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
C
C     1:     Year - IYEAR
C     2:     Age - NAGE
C     3:     Stand trees/acre - ISTTPAT
C     4:     Stand basal area - IBA
C     5:     Stand volume - ISTVOL
C     6:     Infected trees/acre - ISTTPAI
C     7:     Infected basal area - ISTBAI
C     8:     Infected volume - ISTVOLI
C     9:     Mortality trees/acre - ISTTPAM
C     10:    Mortality basal area - ISTBAM
C     11:    Mortality volume - ISTVOLM
C     12:    Infected trees/acre % - ISTPIT
C     13:    Infected volume % - ISTPIV
C     14:    Mortality trees/acre % - ISTPMT
C     15:    Mortality volume % - ISTPMV
C     16:    Mean DMR - STDMR
C     17:    Mean DMI - STDMI

      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      IMPLICIT NONE

      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR,NAGE
      CHARACTER(LEN=26) NPLT
      INTEGER ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
	INTEGER ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
      REAL    STDMR,STDMI
	INTEGER KODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber
	INTEGER           ID

      DOUBLE PRECISION  STDMRB, STDMIB

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     INITIALIZE VARIABLES

      IF(IDM2.EQ.0) RETURN
      IF(IDM2.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut,
     -  iRet)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDM2 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSMIS2:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_DM_Stnd_Sum$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_DM_Stnd_Sum"'
      ELSE
        TABLENAME = 'FVS_DM_Stnd_Sum'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -  iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID text null,'//
     -      'Year int null,'//
     -      'Age int null,'//
     -      'Stnd_TPA int null,'//
     -      'Stnd_BA int null,'//
     -      'Stnd_Vol int null,'//
     -      'Inf_TPA int null,'//
     -      'Inf_BA int null,'//
     -      'Inf_Vol int null,'//
     -      'Mort_TPA int null,'//
     -      'Mort_BA int null,'//
     -      'Mort_Vol int null,'//
     -      'Inf_TPA_Pct int null,'//
     -      'Inf_Vol_Pct int null,'//
     -      'Mort_TPA_Pct int null,'//
     -      'Mort_Vol_Pct int null,'//
     -      'Mean_DMR double null,'//
     -      'Mean_DMI double null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('//
     -      'ID Int,'//
     -      'CaseID Int,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'Age Int,'//
     -      'Stnd_TPA Int,'//
     -      'Stnd_BA Int,'//
     -      'Stnd_Vol Int,'//
     -      'Inf_TPA Int,'//
     -      'Inf_BA Int,'//
     -      'Inf_Vol Int,'//
     -      'Mort_TPA Int,'//
     -      'Mort_BA Int,'//
     -      'Mort_Vol Int,'//
     -      'Inf_TPA_Pct Int,'//
     -      'Inf_Vol_Pct Int,'//
     -      'Mort_TPA_Pct Int,'//
     -      'Mort_Vol_Pct Int,'//
     -      'Mean_DMR Number,'//
     -      'Mean_DMI Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID char(26) not null,'//
     -      'Year int null,'//
     -      'Age int null,'//
     -      'Stnd_TPA int null,'//
     -      'Stnd_BA int null,'//
     -      'Stnd_Vol int null,'//
     -      'Inf_TPA int null,'//
     -      'Inf_BA int null,'//
     -      'Inf_Vol int null,'//
     -      'Mort_TPA int null,'//
     -      'Mort_BA int null,'//
     -      'Mort_Vol int null,'//
     -      'Inf_TPA_Pct int null,'//
     -      'Inf_Vol_Pct int null,'//
     -      'Mort_TPA_Pct int null,'//
     -      'Mort_Vol_Pct int null,'//
     -      'Mean_DMR real null,'//
     -      'Mean_DMI real null)'
        ENDIF

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
        CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS2:Creating Table: '//trim(SQLStmtStr))
        DM2ID = 0
      ENDIF

C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

      IF(DM2ID .EQ. -1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        DM2ID = ID
      ENDIF

C     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

      DM2ID = DM2ID + 1
      IF(DM2ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

      STDMRB = STDMR
      STDMIB = STDMI
		
      WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID,
     -  StandID,Year,Age,Stnd_TPA,Stnd_BA,Stnd_Vol,Inf_TPA,Inf_BA,
     -  Inf_Vol,Mort_TPA,Mort_BA,Mort_Vol,Inf_TPA_Pct,Inf_Vol_Pct,
     -  Mort_TPA_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)
     -  VALUES (?,?,',CHAR(39),TRIM(NPLT),CHAR(39),
     -  ',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

C     CLOSE CURSOR

      CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

C     PREPARE QUERY

      CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1                 ! 1 ID
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),DM2ID,f90SQL_NULL_PTR,iRet) 

      ColNumber=ColNumber+1       ! 2 CASEID
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 3 YEAR
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 4 AGE
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),NAGE,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 5 STAND TPA
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTTPAT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 6 STAND BA
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IBA,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 7 STAND VOL
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTVOL,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 8 INF TPA
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTTPAI,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 9 INF BA
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTBAI,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 10 INF VOL
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTVOLI,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 11 MORT TPA
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTTPAM,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 12 MORT BA
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTBAM,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 13 MORT VOL
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTVOLM,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 14 INF TPA %
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPIT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 15 INF VOL %
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPIV,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 16 MORT TPA %
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPMT,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 17 MORT VOL %
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPMV,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 18 MEAN DMR
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STDMRB,f90SQL_NULL_PTR,iRet)

      ColNumber=ColNumber+1       ! 19 MEAN DMI
      CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STDMIB,f90SQL_NULL_PTR,iRet)

C     CLOSE CURSOR

      CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
      CALL f90SQLExecute(StmtHndlOut,iRet)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -  'DBSMIS2:Inserting Row')

C     RELEASE STATEMENT HANDLE

  100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

	RETURN
      END
C-------------------------------------------------------------------------------

      SUBROUTINE DBSMIS3(IYEAR,NPLT,NLABS,
     -  DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)

C----------
C  **DBSMIS3--DBS  DATE OF LAST REVISION:  04/20/04
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 3RD MISTLETOE REPORT INFORMATION
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
C
C     1:     Year - IYEAR
C     2:     Age - NAGE
C     3:     Stand trees/acre - ISTTPAT
C     4:     Stand basal area - IBA
C     5:     Stand volume - ISTVOL
C     6:     Infected trees/acre - ISTTPAI
C     7:     Infected basal area - ISTBAI
C     8:     Infected volume - ISTVOLI
C     9:     Mortality trees/acre - ISTTPAM
C     10:    Mortality basal area - ISTBAM
C     11:    Mortality volume - ISTVOLM
C     12:    Infected trees/acre % - ISTPIT
C     13:    Infected volume % - ISTPIV
C     14:    Mortality trees/acre % - ISTPMT
C     15:    Mortality volume % - ISTPMV
C     16:    Mean DMR - STDMR
C     17:    Mean DMI - STDMI

      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      IMPLICIT NONE

      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      CHARACTER(LEN=26) NPLT
	CHARACTER(LEN=3)  NLABS(5)
	REAL    DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
	INTEGER KODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber
	INTEGER           ID,I,J

      DOUBLE PRECISION  X(10)

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     INITIALIZE VARIABLES

      IF(IDM3.EQ.0) RETURN
      IF(IDM3.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut,
     -  iRet)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDM3 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSMIS3:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_DM_Sz_Sum$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_DM_Sz_Sum"'
      ELSE
        TABLENAME = 'FVS_DM_Sz_Sum'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -  iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID text null,'//
     -      'Year int null,'//
     -      'Type text null,'//
     -      char(39)//'0-3in'  //char(39)//' double null,'//
     -      char(39)//'3-5in' //char(39)//' double null,'//
     -      char(39)//'5-7in'//char(39)//' double null,'//
     -      char(39)//'7-9in'//char(39)//' double null,'//
     -      char(39)//'9-11in'//char(39)//' double null,'//
     -      char(39)//'11-13in'//char(39)//' double null,'//
     -      char(39)//'13-15in'//char(39)//' double null,'//
     -      char(39)//'15-17in'//char(39)//' double null,'//
     -      char(39)//'17-19in'//char(39)//' double null,'//
     -      char(39)//'gt19in' //char(39)//' double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('//
     -      'ID Int,'//
     -      'CaseID Int,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'Type Text,'//
     -      char(39)//'0-3in'   //char(39)//' Number,'//
     -      char(39)//'3-5in'  //char(39)//' Number,'//
     -      char(39)//'5-7in' //char(39)//' Number,'//
     -      char(39)//'7-9in' //char(39)//' Number,'//
     -      char(39)//'9-11in' //char(39)//' Number,'//
     -      char(39)//'11-13in' //char(39)//' Number,'//
     -      char(39)//'13-15in' //char(39)//' Number,'//
     -      char(39)//'15-17in' //char(39)//' Number,'//
     -      char(39)//'17-19in' //char(39)//' Number,'//
     -      char(39)//'gt19in'  //char(39)//' Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID char(26) not null,'//
     -      'Year int null,'//
     -      'Type char(3) not null,'//
     -      char(39)//'0-3in'   //char(39)//' real null,'//
     -      char(39)//'3-5in'  //char(39)//' real null,'//
     -      char(39)//'5-7in' //char(39)//' real null,'//
     -      char(39)//'7-9in' //char(39)//' real null,'//
     -      char(39)//'9-11in' //char(39)//' real null,'//
     -      char(39)//'11-13in' //char(39)//' real null,'//
     -      char(39)//'13-15in' //char(39)//' real null,'//
     -      char(39)//'15-17in' //char(39)//' real null,'//
     -      char(39)//'17-19in' //char(39)//' real null,'//
     -      char(39)//'gt19in'  //char(39)//' real null)'
        ENDIF

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
        CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS3:Creating Table: '//trim(SQLStmtStr))
        DM3ID = 0
      ENDIF

C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

      IF(DM3ID .EQ. -1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        DM3ID = ID
      ENDIF

C     LOOP OVER 5 TYPES

      DO I = 1,5

C       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

        DM3ID = DM3ID + 1
        IF(DM3ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C     LOOP OVER 5 TYPES, MAKING DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

        SELECT CASE (I)
	    CASE (1)
	      DO J = 1,10
	        X(J) = DCTPA(J)
	      ENDDO
	    CASE (2)
	      DO J = 1,10
	        X(J) = DCINF(J)
	      ENDDO
	    CASE (3)
	      DO J = 1,10
	        X(J) = DCMRT(J)
	      ENDDO
	    CASE (4)
	      DO J = 1,10
	        X(J) = DCDMR(J)
	      ENDDO
	    CASE (5)
	      DO J = 1,10
	        X(J) = DCDMI(J)
	      ENDDO
        END SELECT

        WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID,
     -    StandID,Year,Type,'//
     -    char(39),'0-3in',  char(39),','//
     -    char(39),'3-5in', char(39),','//
     -    char(39),'5-7in',char(39),','//
     -    char(39),'7-9in',char(39),','//
     -    char(39),'9-11in',char(39),','//
     -    char(39),'11-13in',char(39),','//
     -    char(39),'13-15in',char(39),','//
     -    char(39),'15-17in',char(39),','//
     -    char(39),'17-19in',char(39),','//
     -    char(39),'gt19in', char(39),') VALUES ',
     -    '(?,?,',char(39),TRIM(NPLT),char(39),
     -    ',?,',char(39),TRIM(NLABS(I)),char(39),
     -    ',?,?,?,?,?,?,?,?,?,?)'

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

C       PREPARE QUERY

        CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 ID
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),DM3ID,f90SQL_NULL_PTR,iRet) 

        ColNumber=ColNumber+1       ! 2 CASEID
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 3 YEAR
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

        DO J = 1,10
          ColNumber=ColNumber+1     ! SIZE CLASSES 1-10
          CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -      INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -      X(J),f90SQL_NULL_PTR,iRet)
        ENDDO

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
        CALL f90SQLExecute(StmtHndlOut,iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS3:Inserting Row')

	ENDDO

C     RELEASE STATEMENT HANDLE

  100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

	RETURN
      END
C----------------------

      SUBROUTINE DBSMIS5(IYEAR,KODE)

C----------
C  **DBSMIS5--DBS  DATE OF LAST REVISION:  04/20/04
C----------
C     PURPOSE: TO OUTPUT THE TREELIST DM DATA TO THE DATABASE
C
C     AUTH: D. ROBINSON, WITH INSPIRATION FROM
C           D. GAMMEL -- SEM -- JULY 2002
C
C            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
C                     REDIRECT OF THE FLAT FILE REPORT OR IN
C                     ADDITION TO
C
C---
      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      IMPLICIT NONE
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ESTREE.F77'
      INCLUDE 'VARCOM.F77'
      INCLUDE 'WORKCM.F77'
	INCLUDE 'MISCOM.F77'
      INCLUDE 'DMCOM.F77'
      INCLUDE 'DBSCOM.F77'

C     COMMONS

	INTEGER IYEAR,KODE

C     LOCAL VARIABLES

      CHARACTER*8 TID
      INTEGER   ISPC,I,I1,I2,I3,ID,J,K,L
      INTEGER*4 IDCMP1,IDCMP2
      REAL      CW

      INTEGER(SQLSMALLINT_KIND)::ColNumber

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

      DOUBLE PRECISION PB,DBHB,HTB,CWB
	DOUBLE PRECISION BRKPNTB(BPCNT),NEWSPRB(3),NEWINTB(3)
      DOUBLE PRECISION DMINFB(CRTHRD,DEAD_BC),DMINFB_BC(CRTHRD,ACTIVE)

      DATA IDCMP1,IDCMP2/10000000,20000000/

      IF(IDM5.EQ.0) RETURN
      IF(IDM5.EQ.2) KODE = 0
    
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut,
     -  iRet)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDM5 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -    'DBSMIS5:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ.'EXCEL') THEN
        TABLENAME = '[FVS_DM_TreeList$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_DM_TreeList"'
      ELSE
        TABLENAME = 'FVS_DM_TreeList'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

      IF(iRet.NE.SQL_SUCCESS.AND.
     -    iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_TreeList('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID text null,'//
     -      'Year int null,'//
     -      'TreeId text null,'//
     -      'Species text null,'//
     -      'TPA double null,'//
     -      'DBH double null,'//
     -      'Ht double null,'//
     -      'CrWidth double null,'//
     -      'BrkPnt1 double null,'//
     -      'BrkPnt2 double null,'//
     -      'BrkPnt3 double null,'//
     -      'BrkPnt4 double null,'//
     -      'DMR int null,'//
     -      'NewSpr1 double null,'//
     -      'NewSpr2 double null,'//
     -      'NewSpr3 double null,'//
     -      'NewInt1 double null,'//
     -      'NewInt2 double null,'//
     -      'NewInt3 double null,'//
     -      'Immtr1  double null,'//
     -      'Immtr2  double null,'//
     -      'Immtr3  double null,'//
     -      'Latnt1  double null,'//
     -      'Latnt2  double null,'//
     -      'Latnt3  double null,'//
     -      'Suppr1  double null,'//
     -      'Suppr2  double null,'//
     -      'Suppr3  double null,'//
     -      'Actv1   double null,'//
     -      'Actv2   double null,'//
     -      'Actv3   double null,'//
     -      'BCImmtr1  double null,'//
     -      'BCImmtr2  double null,'//
     -      'BCImmtr3  double null,'//
     -      'BCLatnt1  double null,'//
     -      'BCLatnt2  double null,'//
     -      'BCLatnt3  double null,'//
     -      'BCSuppr1  double null,'//
     -      'BCSuppr2  double null,'//
     -      'BCSuppr3  double null,'//
     -      'BCActv1   double null,'//
     -      'BCActv2   double null,'//
     -      'BCActv3   double null,'//
     -      'BCKill1   double null,'//
     -      'BCKill2   double null,'//
     -      'BCKill3   double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_TreeList('//
     -      'Id Int null,'//
     -      'CaseID Int null,'//
     -      'StandID Text null,'//
     -      'Year INT null,'//
     -      'TreeId Text null,'//
     -      'Species Text null,'//
     -      'TPA Number null,'//
     -      'DBH Number null,'//
     -      'Ht Number null,'//
     -      'CrWidth Number null,'//
     -      'BrkPnt1 Number null,'//
     -      'BrkPnt2 Number null,'//
     -      'BrkPnt3 Number null,'//
     -      'BrkPnt4 Number null,'//
     -      'DMR Int null,'//
     -      'NewSpr1 Number null,'//
     -      'NewSpr2 Number null,'//
     -      'NewSpr3 Number null,'//
     -      'NewInt1 Number null,'//
     -      'NewInt2 Number null,'//
     -      'NewInt3 Number null,'//
     -      'Immtr1  Number null,'//
     -      'Immtr2  Number null,'//
     -      'Immtr3  Number null,'//
     -      'Latnt1  Number null,'//
     -      'Latnt2  Number null,'//
     -      'Latnt3  Number null,'//
     -      'Suppr1  Number null,'//
     -      'Suppr2  Number null,'//
     -      'Suppr3  Number null,'//
     -      'Actv1   Number null,'//
     -      'Actv2   Number null,'//
     -      'Actv3   Number null,'//
     -      'BCImmtr1  Number null,'//
     -      'BCImmtr2  Number null,'//
     -      'BCImmtr3  Number null,'//
     -      'BCLatnt1  Number null,'//
     -      'BCLatnt2  Number null,'//
     -      'BCLatnt3  Number null,'//
     -      'BCSuppr1  Number null,'//
     -      'BCSuppr2  Number null,'//
     -      'BCSuppr3  Number null,'//
     -      'BCActv1   Number null,'//
     -      'BCActv2   Number null,'//
     -      'BCActv3   Number null,'//
     -      'BCKill1   Number null,'//
     -      'BCKill2   Number null,'//
     -      'BCKill3   Number null)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_DM_TreeList('//
     -      'Id int primary key,'//
     -      'CaseID int null,'//
     -      'StandID char(26) null,'//
     -      'Year int null,'//
     -      'TreeId char(8) null,'//
     -      'Species char(3) null,'//
     -      'TPA real null,'//
     -      'DBH real null,'//
     -      'Ht real null,'//
     -      'CrWidth real null,'//
     -      'BrkPnt1 real null,'//
     -      'BrkPnt2 real null,'//
     -      'BrkPnt3 real null,'//
     -      'BrkPnt4 real null,'//
     -      'DMR int null,'//
     -      'NewSpr1 real null,'//
     -      'NewSpr2 real null,'//
     -      'NewSpr3 real null,'//
     -      'NewInt1 real null,'//
     -      'NewInt2 real null,'//
     -      'NewInt3 real null,'//
     -      'Immtr1  real null,'//
     -      'Immtr2  real null,'//
     -      'Immtr3  real null,'//
     -      'Latnt1  real null,'//
     -      'Latnt2  real null,'//
     -      'Latnt3  real null,'//
     -      'Suppr1  real null,'//
     -      'Suppr2  real null,'//
     -      'Suppr3  real null,'//
     -      'Actv1   real null,'//
     -      'Actv2   real null,'//
     -      'Actv3   real null,'//
     -      'BCImmtr1  real null,'//
     -      'BCImmtr2  real null,'//
     -      'BCImmtr3  real null,'//
     -      'BCLatnt1  real null,'//
     -      'BCLatnt2  real null,'//
     -      'BCLatnt3  real null,'//
     -      'BCSuppr1  real null,'//
     -      'BCSuppr2  real null,'//
     -      'BCSuppr3  real null,'//
     -      'BCActv1   real null,'//
     -      'BCActv2   real null,'//
     -      'BCActv3   real null,'//
     -      'BCKill1   real null,'//
     -      'BCKill2   real null,'//
     -      'BCKill3   real null)'

        ENDIF

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
        CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS5:Creating Table: '//trim(SQLStmtStr))
        DM5ID = 0
      ENDIF

      IF(DM5ID .EQ. -1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        DM5ID = ID
      ENDIF

C     SEND OUTPUT TO DATABASE ONLY FOR TREES THAT HOST DM

      DO ISPC = 1,MAXSP
        I1 = ISCT(ISPC,1)
        IF (MISFIT(ISPC) .NE. 0 .AND. I1 .NE. 0) THEN
          I2 = ISCT(ISPC,2)
          DO I3 = I1,I2
            I = IND1(I3)

            DM5ID = DM5ID + 1
            IF (DM5ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN
C           COMPRESSED OR GENERATED THROUGH THE ESTAB SYSTEM.

            IF (IDTREE(I) .GT. IDCMP1) THEN
              IF (IDTREE(I) .GT. IDCMP2) THEN
                WRITE(TID,'(''CM'',I6.6)') IDTREE(I) - IDCMP2
              ELSE
                WRITE(TID,'(''ES'',I6.6)') IDTREE(I) - IDCMP1
              ENDIF
            ELSE
              WRITE(TID,'(I8)') IDTREE(I)
            ENDIF

            WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME) //
     -        ' (Id,CaseID,StandID,Year,TreeID,Species,TPA,' //
     -        'DBH,Ht,CrWidth,BrkPnt1,BrkPnt2,BrkPnt3,BrkPnt4,' //
     -        'DMR,NewSpr1,NewSpr2,NewSpr3,NewInt1,NewInt2,NewInt3,' //
     -        'Immtr1,Immtr2,Immtr3,Latnt1,Latnt2,Latnt3,Suppr1,' //
     -        'Suppr2,Suppr3,Actv1,Actv2,Actv3,BCImmtr1,BCImmtr2,'//
     -        'BCImmtr3,BCLatnt1,BCLatnt2,BCLatnt3,BCSuppr1,BCSuppr2,'//
     -        'BCSuppr3,BCActv1,BCActv2,BCActv3,BCKill1,BCKill2,'//
     -        'BCKill3) VALUES ' //
     -        '(?,?,',char(39),TRIM(NPLT),char(39),',?,' //
     -        char(39),TRIM(TID),char(39),',' //
     -        char(39),NSP(ISPC,1)(1:2),char(39),',' //
     -        '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,' //
     -        '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

C           SET CROWN WIDTH 'CW' (feet)

            CW = CRWDTH(I)

C           DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

            PB   = (PROB(I) / GROSPC)
            DBHB = DBH(I)
	      HTB  = HT(I)
	      CWB  = CW
	      DO J = 1,BPCNT
	        BRKPNTB(J) = BRKPNT(I,J)
	      ENDDO
	      DO J = 1,CRTHRD
	        NEWSPRB(J) = NEWSPR(I,J)
	        NEWINTB(J) = NEWINT(I,J)
	      ENDDO

	      DO J = 1,CRTHRD
	        DO K = 1,DEAD_BC
	          DMINFB(J,K) = DMINF(I,J,K)
	        ENDDO
	        DO K = 1,ACTIVE
	          DMINFB_BC(J,K) = 0.0
	          DO L = 1,MAXBC
	            DMINFB_BC(J,K) = DMINFB_BC(J,K) + DMINF_BC(I,J,K,L)
	          ENDDO
	        ENDDO
	      ENDDO

C           CLOSE CURSOR

            CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

C           PREPARE QUERY

            CALL f90SQLPrepare(StmtHndlOut,SQLStmtStr,iRet)

C           BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

            ColNumber=1                 ! 1 ID
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -        SQL_PARAM_INPUT,SQL_F_INTEGER,SQL_INTEGER,
     -        INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -        DM5ID,f90SQL_NULL_PTR,iRet) 

            ColNumber=ColNumber+1       ! 2 CASEID
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_INTEGER,SQL_INTEGER,
     -      INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -      ICASE,f90SQL_NULL_PTR,iRet)

	      ! 3 StandID inserted manually

            ColNumber=ColNumber+1       ! 4 YEAR
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_INTEGER,SQL_INTEGER,
     -      INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -      IYEAR,f90SQL_NULL_PTR,iRet)

	      ! 5 TreeID  inserted manually
	      ! 6 Species inserted manually

            ColNumber=ColNumber+1     ! 7 TPA
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -      INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -      PB,f90SQL_NULL_PTR,iRet)

            ColNumber=ColNumber+1     ! 8 DBH
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -      INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -      DBHB,f90SQL_NULL_PTR,iRet)

            ColNumber=ColNumber+1     ! 9 HT
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -      INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -      HTB,f90SQL_NULL_PTR,iRet)

            ColNumber=ColNumber+1     ! 10 CW
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -      INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -      CWB,f90SQL_NULL_PTR,iRet)

            DO J = 1,BPCNT
              ColNumber=ColNumber+1     ! 11-14 BRKPNT
              CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -        SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -        INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -        BRKPNTB(J),f90SQL_NULL_PTR,iRet)
            ENDDO

            ColNumber=ColNumber+1       ! 15 DMR
            CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_INTEGER,SQL_INTEGER,
     -      INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -      DMRATE(I),f90SQL_NULL_PTR,iRet)

            DO J = 1,CRTHRD
              ColNumber=ColNumber+1     ! 16-18 NEWSPR
              CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -        SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -        INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -        NEWSPRB(J),f90SQL_NULL_PTR,iRet)
            ENDDO

            DO J = 1,CRTHRD
              ColNumber=ColNumber+1     ! 19-21 NEWINT
              CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -        SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -        INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -        NEWINTB(J),f90SQL_NULL_PTR,iRet)
            ENDDO

            DO K = 1,ACTIVE
              DO J = 1,CRTHRD
	          ColNumber=ColNumber+1     ! 22-33 DMINF
                CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          DMINFB(J,K),f90SQL_NULL_PTR,iRet)
              ENDDO
            ENDDO

            DO K = 1,ACTIVE
              DO J = 1,CRTHRD
	          ColNumber=ColNumber+1     ! 34-45 DMINF_BC
                CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          DMINFB_BC(J,K),f90SQL_NULL_PTR,iRet)
              ENDDO
            ENDDO

            K = DEAD_BC
            DO J = 1,CRTHRD
	        ColNumber=ColNumber+1     ! 46-48 DMINF-DEAD
              CALL f90SQLBindParameter(StmtHndlOut,ColNumber,
     -        SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -        INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -        DMINFB(J,K),f90SQL_NULL_PTR,iRet)
            ENDDO

C           CLOSE CURSOR

            CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
            CALL f90SQLExecute(StmtHndlOut,iRet)
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -        'DBSMIS5:Inserting Row')

          ENDDO
        ENDIF
      ENDDO

C     RELEASE STATEMENT HANDLE

  100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

      END
c------------------------

      SUBROUTINE DBSMIS6(IYEAR,KODE)
C----------
C  **DBSMIS6--DBS  DATE OF LAST REVISION:  12/02/03
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE LIGHT PENETRATION
C     AT EACH 2M POSITION IN THE STAND
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      IMPLICIT NONE

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'DBSCOM.F77'
      INCLUDE 'DMCOM.F77'
      
C     ARGUMENT LIST

      INTEGER IYEAR,KODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber
	INTEGER           ID,I,IHT
	REAL              XLIGHT(MXHT)

      DOUBLE PRECISION  LIGHTB

      CHARACTER*1000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     INITIALIZE VARIABLES

      IF(IDM6.EQ.0) RETURN
      IF(IDM6.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut,
     -  iRet)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IDM6 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSMIS6:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_DM_Light$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_DM_Light"'
      ELSE
        TABLENAME = 'FVS_DM_Light'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

C     PRINT THE HEADER INFORMATION FOR THE TREELIST CYCLE, YEAR, LIGHT.

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -  iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Light ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID text null,'//
     -      'Year int null,'//
     -      'Ht int null,'//
     -      'Light double null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Light ('//
     -      'ID Int,'//
     -      'CaseID Int,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'Ht Int,'//
     -      'Light Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_DM_Light ('//
     -      'Id int primary key,'//
     -      'CaseID int not null,'//
     -      'StandID char(26) not null,'//
     -      'Year int null,'//
     -      'Ht int null,'//
     -      'Light real null)'
        ENDIF

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
        CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS6:Creating Table: '//trim(SQLStmtStr))
        DM6ID = 0
      ENDIF

C     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

      IF(DM6ID .EQ. -1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        DM6ID = ID
      ENDIF

      WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID,
     -  StandID,Year,Ht,Light)
     -  VALUES (?,?,',CHAR(39),TRIM(NPLT),CHAR(39),
     -  ',?,?,?)'

C     CLOSE CURSOR

      CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

C     PREPARE QUERY

      CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

C     CALCULATE CUMULATIVE LIGHT (AS IN **DMFSHD**)

      XLIGHT(MXHT) = 1.0 - SHADE(MXHT)
      DO I = MXHT - 1, 1, -1
        XLIGHT(I) = XLIGHT(I + 1) * (1.0 - SHADE(I))
      ENDDO

C     LOOP OVER HEIGHTS

      DO I = 1,MXHT

C       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

        DM6ID = DM6ID + 1
        IF(DM6ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

C       INTEGER AND DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

        IHT    = I*MESH
        LIGHTB = XLIGHT(I)

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 ID
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),DM6ID,f90SQL_NULL_PTR,iRet) 

        ColNumber=ColNumber+1       ! 2 CASEID
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 3 YEAR
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 4 HEIGHT
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),IHT,f90SQL_NULL_PTR,iRet)

        ColNumber=ColNumber+1       ! 5 LIGHT
        CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),LIGHTB,f90SQL_NULL_PTR,iRet)

C       CLOSE CURSOR

        CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
        CALL f90SQLExecute(StmtHndlOut,iRet)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS6:Inserting Row')

	ENDDO

C       RELEASE STATEMENT HANDLE

  100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

	RETURN
      END
