      SUBROUTINE DBSMIS1(IYEAR,NPLT,CSP,
     -  SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4,
     -  KODE)
      IMPLICIT NONE
C
C $Id$
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


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
      REAL    SPPIN4(4),SPPMR4(4),SPPOC4(4)
      INTEGER KODE,IRCODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber
      INTEGER           I

      DOUBLE PRECISION  SPDMR4B, SPDMI4B
      INTEGER           ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME
      CHARACTER(LEN=26) NPLT
      CHARACTER(LEN=2)  CSP(4)

C     INITIALIZE VARIABLES

      IF(IDM1.EQ.0) RETURN
      IF(IDM1.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
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
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        IDM1 = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('//
     -      'CaseID text not null,'//
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
     -      'CaseID Text,'//
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
     -      'CaseID char(36) not null,'//
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

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS1:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     LOOP OVER 4 TOP SPECIES

      DO I = 1,4

C       IF THERE ARE LESS THAN 4 SPECIES INFECTED IN THE STAND,
C       DO NOT WRITE THE BLANK RECORDS TO THE DATABASE. 

        IF (CSP(I) .EQ. '**') GOTO 100
        
C       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
C       INTEGER COPIES OF REAL VECTOR INPUTS

        SPDMR4B = SPDMR4(I)
        SPDMI4B = SPDMI4(I)

        ISPINF4 = NINT(SPINF4(I))
        ISPMRT4 = NINT(SPMRT4(I))
        ISPPIN4 = NINT(SPPIN4(I))
        ISPPMR4 = NINT(SPPMR4(I))
        ISPPOC4 = NINT(SPPOC4(I))

        WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),
     -    ' (CaseID,StandID,Year,Spp,Mean_DMR,Mean_DMI,Inf_TPA,',
     -    'Mort_TPA,Inf_TPA_Pct,Mort_TPA_Pct,Stnd_TPA_Pct) ',
     -    ' VALUES (''',CASEID,''',''',TRIM(NPLT),
     -    ''',''',TRIM(CSP(I)),''',?,?,?,?,?,?,?)'

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLPrepare(StmtHndlOut, SQLStmtStr,
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 YEAR
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! MEAN DMR
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),SPDMR4B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! MEAN DMI
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),SPDMI4B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! TPA INFECTED
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPINF4,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! TPA MORTALITY
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPMRT4,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! % TPA INFECTED
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPPIN4,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! % TPA MORTALITY
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPPMR4,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        ColNumber=ColNumber+1       ! % STAND TPA
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),ISPPOC4,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

C       CLOSE CURSOR

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecute(StmtHndlOut)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS1:Inserting Row')

      ENDDO

C     RELEASE STATEMENT HANDLE

  100 CONTINUE
      iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

      RETURN
      END

C-------------------------------------------------------------------------------

      SUBROUTINE DBSMIS2(IYEAR,NPLT,NAGE,
     -  ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM,
     -  ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)
      IMPLICIT NONE
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

      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR,NAGE
      CHARACTER(LEN=26) NPLT
      INTEGER ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
      INTEGER ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
      REAL    STDMR,STDMI
      INTEGER KODE,IRCODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber

      DOUBLE PRECISION  STDMRB, STDMIB

      CHARACTER*1000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     INITIALIZE VARIABLES

      IF(IDM2.EQ.0) RETURN
      IF(IDM2.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
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
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        IDM2 = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('//
     -      'CaseID text not null,'//
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
     -      'CaseID Text,'//
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
     -      'CaseID char(26) not null,'//
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

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS2:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

      STDMRB = STDMR
      STDMIB = STDMI

      WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (CaseID,',
     -  'StandID,Year,Age,Stnd_TPA,Stnd_BA,Stnd_Vol,Inf_TPA,Inf_BA,',
     -  'Inf_Vol,Mort_TPA,Mort_BA,Mort_Vol,Inf_TPA_Pct,Inf_Vol_Pct,',
     -  'Mort_TPA_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)',
     -  'VALUES (''',CASEID,''',''',TRIM(NPLT),
     -  ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1                 ! YEAR
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! AGE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),NAGE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! STAND TPA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTTPAT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! STAND BA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IBA,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! STAND VOL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTVOL,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! INF TPA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTTPAI,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! INF BA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTBAI,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! INF VOL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTVOLI,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! MORT TPA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTTPAM,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! MORT BA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTBAM,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! MORT VOL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTVOLM,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! INF TPA %
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPIT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! INF VOL %
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPIV,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! 16 MORT TPA %
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPMT,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! MORT VOL %
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),ISTPMV,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! MEAN DMR
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STDMRB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1       ! MEAN DMI
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STDMIB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

C     CLOSE CURSOR

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -  'DBSMIS2:Inserting Row')

C     RELEASE STATEMENT HANDLE

  100 iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

      RETURN
      END
C-------------------------------------------------------------------------------

      SUBROUTINE DBSMIS3(IYEAR,NPLT,NLABS,
     -  DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)
      IMPLICIT NONE

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


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      CHARACTER(LEN=26) NPLT
      CHARACTER(LEN=3)  NLABS(5)
      REAL    DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
      INTEGER KODE,IRCODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber
      INTEGER           I,J

      DOUBLE PRECISION  X(10)

      CHARACTER*1000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     INITIALIZE VARIABLES

      IF(IDM3.EQ.0) RETURN
      IF(IDM3.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
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
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        IDM3 = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('//
     -      'CaseID text not null,'//
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
     -      'CaseID Text,'//
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
     -      'CaseID char(36) not null,'//
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

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS3:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     LOOP OVER 5 TYPES

      DO I = 1,5

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

        WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),
     -    ' (CaseID,StandID,Year,Type,',
     -    char(39),'0-3in',  char(39),',',
     -    char(39),'3-5in', char(39),',',
     -    char(39),'5-7in',char(39),',',
     -    char(39),'7-9in',char(39),',',
     -    char(39),'9-11in',char(39),',',
     -    char(39),'11-13in',char(39),',',
     -    char(39),'13-15in',char(39),',',
     -    char(39),'15-17in',char(39),',',
     -    char(39),'17-19in',char(39),',',
     -    char(39),'gt19in', char(39),') VALUES ',
     -    '(''',CASEID,''',''',TRIM(NPLT),
     -    ''',?,''',TRIM(NLABS(I)),
     -    ''',?,?,?,?,?,?,?,?,?,?)'

C       CLOSE CURSOR

        iRet=fvsSQLCloseCursor(StmtHndlOut)

C       PREPARE QUERY

        iRet=fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 YEAR
        iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -    SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -    INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

        DO J = 1,10
          ColNumber=ColNumber+1     ! SIZE CLASSES 2-11
          iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -      SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,
     -      INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -      X(J),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        ENDDO

C       CLOSE CURSOR

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecute(StmtHndlOut)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSMIS3:Inserting Row')

      ENDDO

C     RELEASE STATEMENT HANDLE

  100 iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

      RETURN
      END
