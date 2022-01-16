      SUBROUTINE DBSRD1(IYEAR,NPLT,IAGE,DTYPE,NCENT,DAREA,SRATE,
     -  STUTPA,STUBA,MRTTPA,MRTCUFT,UNTPA,INTPA,PCTROOT,
     -  MRCUFT,DABA,NEWIN,NEWEXP,NEWTOT)

      IMPLICIT NONE
C
C $Id: dbsrd.f 1154 2014-12-30 12:00:00Z rhavis@msn.com $
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST ROOT DISEASE MODEL REPORT,
C              SUMMARY STATISTICS FOR ROOT DISEASE AREAS (PER ACRE BASED ON
C              DISEASE AREA ONLY).
C     AUTH: L. DAVID -- FMSC (METI) -- 12/30/2014
C
C     ARGUMENT LIST
C      1: IYEAR   -- YEAR
C      2: NPLT    -- STAND ID
C      3: IAGE    -- STAND AGE
C      4: DTYPE   -- DISEASE TYPE 
C      5: NCENT   -- NUMBER OF DISEASE CENTERS
C      6: DAREA   -- DISEASE AREA (ACRES)
C      7: SRATE   -- SPREAD RATE FT/YR
C      8: STUTPA  -- STUMPS/ACRE, DISEASE AREA ONLY
C      9: STUBA   -- STUMP BA/ACRE, DISEASE AREA ONLY
C     10: MRTTPA  -- TPA KILLED, DISEASE AREA ONLY
C     11: MRTCUFT -- CUFT/ACRE KILLED, DISEASE AREA ONLY
C     12: UNTPA   -- UNINFECTED LIVE TPA IN DISEASE AREA
C     13: INTPA   -- INFECTED LIVE TPA IN DISEASE AREA
C     14: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED
C     15: MRCUFT  -- TOTAL MERCH CUFT IN DISEASE AREA
C     16: DABA    -- LIVE BA/ACRE IN DISEASE AREA
C     17: NEWIN   -- NEWLY INFECTED PROPORTION WITHIN DISEASE AREA
C     18: NEWEXP  -- NEWLY INFECTED PROPORTION DUE TO AREA EXPANSION
C     19: NEWTOT  -- NEWLY INFECTED PROPORTION OF TOTAL STAND


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR, IAGE, NCENT
      REAL    DAREA, SRATE, STUTPA, STUBA, MRTTPA, MRTCUFT, UNTPA,
     -        INTPA, PCTROOT, MRCUFT, DABA, NEWIN, NEWEXP, NEWTOT
      CHARACTER(LEN=1)  DTYPE
      CHARACTER(LEN=26) NPLT
      INTEGER KODE,IRCODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber, I1

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
C     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
C
      DOUBLE PRECISION 
     -  DAREAD, SRATED, STUTPAD, STUBAD, MRTTPAD, MRTCUFTD, UNTPAD,
     -  INTPAD, PCTROOTD, MRCUFTD, DABAD, NEWIND, NEWEXPD, NEWTOTD

C     If RD Summary not selected for DB output, return.
C      WRITE(23,*) "IN DBSRD1: IRD1 = ",IRD1               ! DEBUG
      IF(IRD1 .EQ. 0) RETURN

C     This variable is used for ColNumber set and increment
C     to eliminate the number translation warning being reported
C     when compiling with gfortran. LD 01/15/2015
      I1 = 1

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
C       Turn off this table DB output
        IRD1 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSRD1:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE RD SUMMARY TABLE EXISTS IN THE DATBASE
C      WRITE(23,*) "IN DBSRD1: DBMSOUT = ",DBMSOUT                ! DEBUG

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_RD_Sum$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_RD_Sum"'
      ELSE
        TABLENAME = 'FVS_RD_Sum'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
C      WRITE(23,*) "IN DBSRD1: DBSKNROWS IRCODE = ",IRCODE        ! DEBUG
C      WRITE(23,*) "IN DBSRD1: CASEID = ",CASEID                  ! DEBUG

      IF(IRCODE.EQ.2) THEN
C       Turn off this table DB output
        IRD1 = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Sum ('//
     -      'CaseID text not null,'//
     -      'StandID text null,'//
     -      'Year Int null,'//
     -      'Age Int null,'//
     -      'RD_Type Text null,'//
     -      'Num_Centers Int null,'//
     -      'RD_Area double null,'//
     -      'Spread_Ft_per_Year double null,'//
     -      'Stumps_per_Acre double null,'//
     -      'Stumps_BA double null,'//
     -      'Mort_TPA double null,'//
     -      'Mort_CuFt double null,'//
     -      'UnInf_TPA double null,'//
     -      'Inf_TPA double null,'//
     -      'Ave_Pct_Root_Inf double null,'//
     -      'Live_Merch_CuFt double null,'//
     -      'Live_BA double null,'//
     -      'New_Inf_Prp_Ins double null,'//
     -      'New_Inf_Prp_Exp double null,'//
     -      'New_Inf_Prp_Tot double null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Sum ('//
     -      'CaseID Text,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'Age Int,'//
     -      'RD_Type Text,'//
     -      'Num_Centers Int,'//
     -      'RD_Area Number,'//
     -      'Spread_Ft_per_Year Number,'//
     -      'Stumps_per_Acre Number,'//
     -      'Stumps_BA Number,'//
     -      'Mort_TPA Number,'//
     -      'Mort_CuFt Number,'//
     -      'UnInf_TPA Number,'//
     -      'Inf_TPA Number,'//
     -      'Ave_Pct_Root_Inf Number,'//
     -      'Live_Merch_CuFt Number,'//
     -      'Live_BA Number,'//
     -      'New_Inf_Prp_Ins Number,'//
     -      'New_Inf_Prp_Exp Number,'//
     -      'New_Inf_Prp_Tot Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_RD_Sum ('//
     -      'CaseID char(36) not null,'//
     -      'StandID char(26) not null,'//
     -      'Year Int null,'//
     -      'Age Int null,'//
     -      'RD_Type char(1) null,'//
     -      'Num_Centers Int null,'//
     -      'RD_Area real null,'//
     -      'Spread_Ft_per_Year real null,'//
     -      'Stumps_per_Acre real null,'//
     -      'Stumps_BA real null,'//
     -      'Mort_TPA real null,'//
     -      'Mort_CuFt real null,'//
     -      'UnInf_TPA real null,'//
     -      'Inf_TPA real null,'//
     -      'Ave_Pct_Root_Inf real null,'//
     -      'Live_Merch_CuFt real null,'//
     -      'Live_BA real null,'//
     -      'New_Inf_Prp_Ins real null,'//
     -      'New_Inf_Prp_Exp real null,'//
     -      'New_Inf_Prp_Tot real null)'
        ENDIF

C       CLOSE CURSOR

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSRD1:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
C     WRITE RECORD TO DATABASE

      DAREAD   = DAREA
      SRATED   = SRATE
      STUTPAD  = STUTPA
      STUBAD   = STUBA
      MRTTPAD  = MRTTPA
      MRTCUFTD = MRTCUFT
      UNTPAD   = UNTPA
      INTPAD   = INTPA
      PCTROOTD = PCTROOT
      MRCUFTD  = MRCUFT
      DABAD    = DABA
      NEWIND   = NEWIN
      NEWEXPD  = NEWEXP
      NEWTOTD  = NEWTOT

C     DEFINE DATABASE INSERT STATEMENT

      WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),
     -  ' (CaseID,StandID,Year,Age,RD_Type,Num_Centers,RD_Area,',
     -  'Spread_Ft_per_Year,Stumps_per_Acre,Stumps_BA,',
     -  'Mort_TPA,Mort_CuFt,UnInf_TPA,Inf_TPA,Ave_Pct_Root_Inf,',
     -  'Live_Merch_CuFt,Live_BA,',
     -  'New_Inf_Prp_Ins,New_Inf_Prp_Exp,New_Inf_Prp_Tot) ',
     -  ' VALUES (''',CASEID,''',''',TRIM(NPLT),''',
     -  ?,?,''',DTYPE,''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLPrepare(StmtHndlOut, SQLStmtStr,
     -     int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=I1                 ! YEAR
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! AGE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IAGE,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! NUMBER OF DISEASE CENTERS
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),NCENT,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! DISEASE AREA IN ACRES
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DAREAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! SPREAD RATE FEET PER YEAR
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),SRATED,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! DEAD STUMPS PER ACRE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STUTPAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! STUMPS BA SQFT PER ACRE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STUBAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! TREES KILLED TPA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTTPAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! TREES KILLED CUFT PER ACRE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTCUFTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! UNINFECTED LIVE TREES TPA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),UNTPAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INFECTED LIVE TREES TPA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),INTPAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! AVERAGE % ROOTS INFECTED
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),PCTROOTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! LIVE MERCH CUFT PER ACRE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRCUFTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! LIVE BASAL AREA PER ACRE
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DABAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! NEWLY INFECTED PROP INSIDE DIS AREA
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),NEWIND,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! NEWLY INFECTED PROP DUE TO EXPANSION
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),NEWEXPD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! NEWLY INFECTED PROPORTION TOTAL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),NEWTOTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

C     CLOSE CURSOR

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -  'DBSRD1:Inserting Row')

C     RELEASE STATEMENT HANDLE

  100 CONTINUE
      iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

      RETURN
      END

C-------------------------------------------------------------------------------

      SUBROUTINE DBSRD2(IYEAR,NPLT,DTYPE,DAREA,CSP,DDBHCL,
     -  DTPA,LDBHCL,LUNTPA,LINTPA,PCTROOT)

      IMPLICIT NONE
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND ROOT DISEASE MODEL REPORT,
C              DBH PERCENTILE DETAIL FOR ROOT DISEASE AREAS (PER ACRE BASED ON
C              DISEASE AREA ONLY).
C     AUTH: L. DAVID -- FMSC (METI) -- 01/06/2015
C
C     ARGUMENT LIST
C      1: IYEAR   -- YEAR
C      2: NPLT    -- STAND ID
C      3: DTYPE   -- DISEASE TYPE 
C      4: DAREA   -- DISEASE AREA (ACRES)
C      5: CSP     -- TREE SPECIES CHARACTER ABBREVIATION
C      6: DDBHCL  -- DEAD %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      7: DTPA    -- DEAD TREES/ACRE TOTAL, DISEASE AREA ONLY
C      8: LDBHCL  -- LIVE %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      9: LUNTPA  -- LIVE UNINFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     10: LINTPA  -- LIVE INFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     11: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    DAREA, DDBHCL(6), DTPA, LDBHCL(6), LUNTPA, LINTPA,
     -        PCTROOT
      CHARACTER(LEN=1)  DTYPE
      CHARACTER(LEN=2)  CSP
      CHARACTER(LEN=26) NPLT
      INTEGER KODE,IRCODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber, I1
      INTEGER           I

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
C     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
C
      DOUBLE PRECISION 
     -  DAREAD, DDBHCLD(6), DTPAD, LDBHCLD(6), LUNTPAD, LINTPAD,
     -  PCTROOTD

C     If RD Detail not selected for DB output, return.

      IF(IRD2 .EQ .0) RETURN

C     This variable is used for ColNumber set and increment
C     to eliminate the number translation warning being reported
C     when compiling with gfortran. LD 01/15/2015
      I1 = 1

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
C       Turn off this table DB output
        IRD2 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSRD2:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE RD SUMMARY TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_RD_Det$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_RD_Det"'
      ELSE
        TABLENAME = 'FVS_RD_Det'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
C       Turn off this table DB output
        IRD2 = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Det ('//
     -      'CaseID text not null,'//
     -      'StandID text null,'//
     -      'Year Int null,'//
     -      'RD_Type Text null,'//
     -      'RD_Area double null,'//
     -      'Species Text null,'//
     -      'Mort_10Pctile_DBH double null,'//
     -      'Mort_30Pctile_DBH double null,'//
     -      'Mort_50Pctile_DBH double null,'//
     -      'Mort_70Pctile_DBH double null,'//
     -      'Mort_90Pctile_DBH double null,'//
     -      'Mort_100Pctile_DBH double null,'//
     -      'Mort_TPA_Total double null,'//
     -      'Live_10Pctile_DBH double null,'//
     -      'Live_30Pctile_DBH double null,'//
     -      'Live_50Pctile_DBH double null,'//
     -      'Live_70Pctile_DBH double null,'//
     -      'Live_90Pctile_DBH double null,'//
     -      'Live_100Pctile_DBH double null,'//
     -      'UnInf_TPA_Total double null,'//
     -      'Inf_TPA_Total double null,'//
     -      'Pct_Roots_Inf double null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Det ('//
     -      'CaseID Text,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'RD_Type Text,'//
     -      'RD_Area Number,'//
     -      'Species Text,'//
     -      'Mort_10Pctile_DBH Number,'//
     -      'Mort_30Pctile_DBH Number,'//
     -      'Mort_50Pctile_DBH Number,'//
     -      'Mort_70Pctile_DBH Number,'//
     -      'Mort_90Pctile_DBH Number,'//
     -      'Mort_100Pctile_DBH Number,'//
     -      'Mort_TPA_Total Number,'//
     -      'Live_10Pctile_DBH Number,'//
     -      'Live_30Pctile_DBH Number,'//
     -      'Live_50Pctile_DBH Number,'//
     -      'Live_70Pctile_DBH Number,'//
     -      'Live_90Pctile_DBH Number,'//
     -      'Live_100Pctile_DBH Number,'//
     -      'UnInf_TPA_Total Number,'//
     -      'Inf_TPA_Total Number,'//
     -      'Pct_Roots_Inf Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_RD_Det ('//
     -      'CaseID char(36) not null,'//
     -      'StandID char(26) not null,'//
     -      'Year Int null,'//
     -      'RD_Type char(1) not null,'//
     -      'RD_Area real null,'//
     -      'Species char(2) not null,'//
     -      'Mort_10Pctile_DBH real null,'//
     -      'Mort_30Pctile_DBH real null,'//
     -      'Mort_50Pctile_DBH real null,'//
     -      'Mort_70Pctile_DBH real null,'//
     -      'Mort_90Pctile_DBH real null,'//
     -      'Mort_100Pctile_DBH real null,'//
     -      'Mort_TPA_Total real null,'//
     -      'Live_10Pctile_DBH real null,'//
     -      'Live_30Pctile_DBH real null,'//
     -      'Live_50Pctile_DBH real null,'//
     -      'Live_70Pctile_DBH real null,'//
     -      'Live_90Pctile_DBH real null,'//
     -      'Live_100Pctile_DBH real null,'//
     -      'UnInf_TPA_Total real null,'//
     -      'Inf_TPA_Total real null,'//
     -      'Pct_Roots_Inf real null)'
        ENDIF

C       CLOSE CURSOR

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSRD2:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
C     WRITE RECORD TO DATABASE

      DAREAD   = DAREA
      DTPAD    = DTPA
      LUNTPAD  = LUNTPA
      LINTPAD  = LINTPA
      PCTROOTD = PCTROOT
      DO I=1,6
        DDBHCLD(I) = DDBHCL(I)
        LDBHCLD(I) = LDBHCL(I)
      END DO

C     DEFINE DATABASE INSERT STATEMENT

      WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),
     -  ' (CaseID,StandID,Year,RD_Type,RD_Area,Species,',
     -  'Mort_10Pctile_DBH,Mort_30Pctile_DBH,Mort_50Pctile_DBH,',
     -  'Mort_70Pctile_DBH,Mort_90Pctile_DBH,Mort_100Pctile_DBH,',
     -  'Mort_TPA_Total,Live_10Pctile_DBH,Live_30Pctile_DBH,',
     -  'Live_50Pctile_DBH,Live_70Pctile_DBH,Live_90Pctile_DBH,',
     -  'Live_100Pctile_DBH,UnInf_TPA_Total,Inf_TPA_Total,',
     -  'Pct_Roots_Inf) ',
     -  ' VALUES (''',CASEID,''',''',TRIM(NPLT),''',?,''',DTYPE,''',
     -  ?,''',CSP,''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLPrepare(StmtHndlOut, SQLStmtStr,
     -     int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=I1                 ! YEAR
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! DISEASE AREA IN ACRES
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DAREAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality 10 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DDBHCLD(1),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality 30 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DDBHCLD(2),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality 50 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DDBHCLD(3),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality 70 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DDBHCLD(4),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality 90 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DDBHCLD(5),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality 100 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DDBHCLD(6),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Mortality TPA total
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),DtpaD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live trees 10 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LDBHCLD(1),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live trees 30 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LDBHCLD(2),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live trees 50 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LDBHCLD(3),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live trees 70 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LDBHCLD(4),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live trees 90 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LDBHCLD(5),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live trees 100 percentile DBH
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LDBHCLD(6),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live uninfected TPA total
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LUNTPAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! Live infected TPA total
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),LINTPAD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! AVERAGE % ROOTS INFECTED
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),PCTROOTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

C     CLOSE CURSOR

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -  'DBSRD2:Inserting Row')

C     RELEASE STATEMENT HANDLE

  100 CONTINUE
      iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

      RETURN
      END

C-------------------------------------------------------------------------------

      SUBROUTINE DBSRD3(IYEAR, NPLT, CSP, MRTININ, ININTOT, ININLIV,
     -  MRTINUN, INUNTOT, INUNLIV, MRTOUT, OUTTOT, OUTLIV, STDMRT)

      IMPLICIT NONE
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND ROOT DISEASE MODEL REPORT,
C              DBH PERCENTILE DETAIL FOR ROOT DISEASE AREAS (PER ACRE BASED ON
C              DISEASE AREA ONLY).
C     AUTH: L. DAVID -- FMSC (METI) -- 01/06/2015
C
C     ARGUMENT LIST
C      1: IYEAR   -- YEAR
C      2: NPLT    -- STAND ID
C      3: DTYPE   -- DISEASE TYPE 
C      4: DAREA   -- DISEASE AREA (ACRES)
C      5: CSP     -- TREE SPECIES CHARACTER ABBREVIATION
C      6: DDBHCL  -- DEAD %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      7: DTPA    -- DEAD TREES/ACRE TOTAL, DISEASE AREA ONLY
C      8: LDBHCL  -- LIVE %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      9: LUNTPA  -- LIVE UNINFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     10: LINTPA  -- LIVE INFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     11: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    MRTININ(7), MRTINUN(7), MRTOUT(7),
     -        ININTOT, INUNTOT, OUTTOT, STDMRT,
     -        ININLIV, INUNLIV, OUTLIV
      CHARACTER(LEN=2)  CSP
      CHARACTER(LEN=26) NPLT
      INTEGER KODE,IRCODE

C     LOCAL VARIABLES

      INTEGER(SQLSMALLINT_KIND)::ColNumber, I1
      INTEGER           I

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=20) TABLENAME

C     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
C     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
C
      DOUBLE PRECISION 
     -  MRTININD(7), MRTINUND(7), MRTOUTD(7), 
     -  ININTOTD, INUNTOTD, OUTTOTD, STDMRTD,
     -  ININLIVD, INUNLIVD, OUTLIVD

C     If RD Bark Beetle not selected for DB output, return.

      IF(IRD3 .EQ. 0) RETURN

C     This variable is used for ColNumber set and increment
C     to eliminate the number translation warning being reported
C     when compiling with gfortran. LD 01/15/2015
      I1 = 1

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
C       Turn off this table DB output
        IRD3 = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSRD3:DSN Connection')
        GOTO 100
      ENDIF

C     CHECK TO SEE IF THE RD SUMMARY TABLE EXISTS IN THE DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_RD_Beetle$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_RD_Beetle"'
      ELSE
        TABLENAME = 'FVS_RD_Beetle'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
C       Turn off this table DB output
        IRD3 = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Beetle ('//
     -      'CaseID text not null,'//
     -      'StandID text null,'//
     -      'Year Int null,'//
     -      'Species Text null,'//
     -      'In_Inf_0_5_DBH       double null,'//
     -      'In_Inf_5_10_DBH      double null,'//
     -      'In_Inf_10_15_DBH     double null,'//
     -      'In_Inf_15_20_DBH     double null,'//
     -      'In_Inf_20_25_DBH     double null,'//
     -      'In_Inf_25_30_DBH     double null,'//
     -      'In_Inf_30_DBH        double null,'//
     -      'In_Inf_Mort          double null,'//
     -      'In_Inf_Live_before   double null,'//
     -      'In_UnInf_0_5_DBH     double null,'//
     -      'In_UnInf_5_10_DBH    double null,'//
     -      'In_UnInf_10_15_DBH   double null,'//
     -      'In_UnInf_15_20_DBH   double null,'//
     -      'In_UnInf_20_25_DBH   double null,'//
     -      'In_UnInf_25_30_DBH   double null,'//
     -      'In_UnInf_30_DBH      double null,'//
     -      'In_UnInf_Mort        double null,'//
     -      'In_UnInf_Live_Before double null,'//
     -      'Outside_0_5_DBH      double null,'//
     -      'Outside_5_10_DBH     double null,'//
     -      'Outside_10_15_DBH    double null,'//
     -      'Outside_15_20_DBH    double null,'//
     -      'Outside_20_25_DBH    double null,'//
     -      'Outside_25_30_DBH    double null,'//
     -      'Outside_30_DBH       double null,'//
     -      'Outside_Mort         double null,'//
     -      'Outside_Live_Before  double null,'//
     -      'Stand_Mort_Total     double null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Beetle ('//
     -      'CaseID Text,'//
     -      'StandID Text,'//
     -      'Year Int,'//
     -      'Species Text,'//
     -      'In_Inf_0_5_DBH       number,'//
     -      'In_Inf_5_10_DBH      number,'//
     -      'In_Inf_10_15_DBH     number,'//
     -      'In_Inf_15_20_DBH     number,'//
     -      'In_Inf_20_25_DBH     number,'//
     -      'In_Inf_25_30_DBH     number,'//
     -      'In_Inf_30_DBH        number,'//
     -      'In_Inf_Mort          number,'//
     -      'In_Inf_Live_before   number,'//
     -      'In_UnInf_0_5_DBH     number,'//
     -      'In_UnInf_5_10_DBH    number,'//
     -      'In_UnInf_10_15_DBH   number,'//
     -      'In_UnInf_15_20_DBH   number,'//
     -      'In_UnInf_20_25_DBH   number,'//
     -      'In_UnInf_25_30_DBH   number,'//
     -      'In_UnInf_30_DBH      number,'//
     -      'In_UnInf_Mort        number,'//
     -      'In_UnInf_Live_Before number,'//
     -      'Outside_0_5_DBH      number,'//
     -      'Outside_5_10_DBH     number,'//
     -      'Outside_10_15_DBH    number,'//
     -      'Outside_15_20_DBH    number,'//
     -      'Outside_20_25_DBH    number,'//
     -      'Outside_25_30_DBH    number,'//
     -      'Outside_30_DBH       number,'//
     -      'Outside_Mort         number,'//
     -      'Outside_Live_Before  number,'//
     -      'Stand_Mort_Total     number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_RD_Beetle ('//
     -      'CaseID char(36) not null,'//
     -      'StandID char(26) not null,'//
     -      'Year Int null,'//
     -      'Species char(2) not null,'//
     -      'In_Inf_0_5_DBH       real null,'//
     -      'In_Inf_5_10_DBH      real null,'//
     -      'In_Inf_10_15_DBH     real null,'//
     -      'In_Inf_15_20_DBH     real null,'//
     -      'In_Inf_20_25_DBH     real null,'//
     -      'In_Inf_25_30_DBH     real null,'//
     -      'In_Inf_30_DBH        real null,'//
     -      'In_Inf_Mort          real null,'//
     -      'In_Inf_Live_before   real null,'//
     -      'In_UnInf_0_5_DBH     real null,'//
     -      'In_UnInf_5_10_DBH    real null,'//
     -      'In_UnInf_10_15_DBH   real null,'//
     -      'In_UnInf_15_20_DBH   real null,'//
     -      'In_UnInf_20_25_DBH   real null,'//
     -      'In_UnInf_25_30_DBH   real null,'//
     -      'In_UnInf_30_DBH      real null,'//
     -      'In_UnInf_Mort        real null,'//
     -      'In_UnInf_Live_Before real null,'//
     -      'Outside_0_5_DBH      real null,'//
     -      'Outside_5_10_DBH     real null,'//
     -      'Outside_10_15_DBH    real null,'//
     -      'Outside_15_20_DBH    real null,'//
     -      'Outside_20_25_DBH    real null,'//
     -      'Outside_25_30_DBH    real null,'//
     -      'Outside_30_DBH       real null,'//
     -      'Outside_Mort         real null,'//
     -      'Outside_Live_Before  real null,'//
     -      'Stand_Mort_Total     real null)'
        ENDIF

C       CLOSE CURSOR

        iRet=fvsSQLCloseCursor(StmtHndlOut)
        iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSRD3:Creating Table: '//trim(SQLStmtStr))
      ENDIF

C     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
C     WRITE RECORD TO DATABASE

      DO I=1,7
        MRTININD(I) = MRTININ(I)
        MRTINUND(I) = MRTINUN(I)
        MRTOUTD(I)  = MRTOUT(I)
      END DO

      ININTOTD = ININTOT
      ININLIVD = ININLIV
      INUNTOTD = INUNTOT
      INUNLIVD = INUNLIV
      OUTTOTD  = OUTTOT 
      OUTLIVD  = OUTLIV 
      STDMRTD  = STDMRT 

C     DEFINE DATABASE INSERT STATEMENT

      WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),
     -  ' (CaseID,StandID,Year,Species,',
     -  'In_Inf_0_5_DBH,In_Inf_5_10_DBH,In_Inf_10_15_DBH,',
     -  'In_Inf_15_20_DBH,In_Inf_20_25_DBH,In_Inf_25_30_DBH,',
     -  'In_Inf_30_DBH,In_Inf_Mort,In_Inf_Live_before,',
     -  'In_UnInf_0_5_DBH,In_UnInf_5_10_DBH,In_UnInf_10_15_DBH,',
     -  'In_UnInf_15_20_DBH,In_UnInf_20_25_DBH,In_UnInf_25_30_DBH,',
     -  'In_UnInf_30_DBH,In_UnInf_Mort,In_UnInf_Live_Before,',
     -  'Outside_0_5_DBH,Outside_5_10_DBH,Outside_10_15_DBH,',
     -  'Outside_15_20_DBH,Outside_20_25_DBH,Outside_25_30_DBH,',
     -  'Outside_30_DBH,Outside_Mort,Outside_Live_Before,',
     -  'Stand_Mort_Total) ',
     -  ' VALUES (''',CASEID,''',''',TRIM(NPLT),''',?,''',CSP,''',
     -  ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLPrepare(StmtHndlOut, SQLStmtStr,
     -     int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=I1                 ! YEAR
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -  INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 0-5
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(1),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 5-10
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(2),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 10-15
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(3),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 15-20
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(4),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 20-25
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(5),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 25-30
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(6),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED 30+
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTININD(7),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED TOTAL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),ININTOTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE INFECTED LIVE BEFORE ATTACK
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),ININLIVD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 0-5
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(1),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 5-10
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(2),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 10-15
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(3),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 15-20
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(4),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 20-25
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(5),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 25-30
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(6),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED 30+
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTINUND(7),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED TOTAL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),INUNTOTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! INSIDE UNINFECTED LIVE BEFORE ATTACK
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),INUNLIVD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 0-5
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(1),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 5-10
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(2),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 10-15
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(3),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 15-20
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(4),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 20-25
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(5),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 25-30
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(6),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT 30+
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),MRTOUTD(7),int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE MORT TOTAL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),OUTTOTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! OUTSIDE LIVE BEFORE ATTACK
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),OUTLIVD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

      ColNumber=ColNumber+I1       ! STAND MORT TOTAL
      iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -  SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -  INT(5,SQLSMALLINT_KIND),STDMRTD,int(4,SQLLEN_KIND),
     -         SQL_NULL_PTR)

C     CLOSE CURSOR

      iRet=fvsSQLCloseCursor(StmtHndlOut)
      iRet=fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -  'DBSRD3:Inserting Row')

C     RELEASE STATEMENT HANDLE

  100 CONTINUE
      iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

      RETURN
      END
