      SUBROUTINE DBSFMDSNAG(IYEAR,SDBH,SHTH,SHTS,SVLH,SVLS,
     -  SDH,SDS,YRLAST,KODE)
      IMPLICIT NONE
C
C $Id: dbsfmdsnag.f 1389 2014-12-19 21:46:29Z rhavis@msn.com $
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE DETAILED SNAG REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE DETAILED SNAG OUTPUT FROM THE FIRE MODEL.
C              1: AVERAGE SNAG DIAMETER FOR THE RECORD
C              2: CURRENT HEIGHT OF HARD SNAGS
C              3: CURRENT HEIGHT OF SOFT SNAGS
C              4: CURRENT VOLUME OF HARD SNAGS
C              5: CURRENT VOLUME OF SOFT SNAGS
C              6: DENSITY OF HARD SNAGS
C              7: DENSITY OF SOFT SNAGS
C              8: YRLAST
C              9: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS

      INTEGER IYEAR,IRCODE,KODE,YRDEAD,SVLH,SVLS,SVLT,YRLAST,JYR,IDC,JCL
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL SDBH, SHTH, SHTS, SDH, SDS, SDT
      DOUBLE PRECISION SDBHB, SHTHB, SHTSB, SDHB, SDSB, SDTB
      DIMENSION SVLH(MAXSP,100,6), SVLS(MAXSP,100,6), SVLT(MAXSP,100,6),
     -  SDBH(MAXSP,100,6), SDBHB(MAXSP,100,6), SHTH(MAXSP,100,6),
     -  SHTHB(MAXSP,100,6), SHTS(MAXSP,100,6), SHTSB(MAXSP,100,6),
     -  SDH(MAXSP,100,6),SDS(MAXSP,100,6),SDT(MAXSP,100,6),
     -  SDHB(MAXSP,100,6),SDSB(MAXSP,100,6),SDTB(MAXSP,100,6)
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=3) CSP
      CHARACTER(LEN=8) CSPECIES

C     Initialize variables

      IF(ISDET.EQ.0) RETURN
      IF(ISDET.EQ.2) KODE = 0

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ISDET = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSFMDSNAG:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE DETAILED SNAG TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_SnagDet$]'
      ELSE
        TABLENAME = 'FVS_SnagDet'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,MAXSP*YRLAST*6,
     >     TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        ISDET = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_SnagDet('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Species Text null,'//
     -              'DBH_Class int null,'//
     -              'Death_DBH double null,'//
     -              'Current_Ht_Hard double null,'//
     -              'Current_Ht_Soft double null,'//
     -              'Current_Vol_Hard double null,'//
     -              'Current_Vol_Soft double null,'//
     -              'Total_Volume double null,'//
     -              'Year_Died int null,'//
     -              'Density_Hard double null,'//
     -              'Density_Soft double null,'//
     -              'Density_Total double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_SnagDet('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'Species Text,'//
     -              'DBH_Class int,'//
     -              'Death_DBH Number,'//
     -              'Current_Ht_Hard Number,'//
     -              'Current_Ht_Soft Number,'//
     -              'Current_Vol_Hard Number,'//
     -              'Current_Vol_Soft Number,'//
     -              'Total_Volume Number,'//
     -              'Year_Died int,'//
     -              'Density_Hard Number,'//
     -              'Density_Soft Number,'//
     -              'Density_Total Number)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_SnagDet('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Species char(3) null,'//
     -              'DBH_Class int null,'//
     -              'Death_DBH real null,'//
     -              'Current_Ht_Hard real null,'//
     -              'Current_Ht_Soft real null,'//
     -              'Current_Vol_Hard real null,'//
     -              'Current_Vol_Soft real null,'//
     -              'Total_Volume real null,'//
     -              'Year_Died int null,'//
     -              'Density_Hard real null,'//
     -              'Density_Soft real null,'//
     -              'Density_Total real null)'

        ENDIF
 
        !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -          int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSFMDSNAG:Creating Table: '//trim(SQLStmtStr))
      ENDIF

      DO JYR= 1,YRLAST
         DO IDC= 1,MAXSP
            DO JCL= 1,6

              CSP = JSP(IDC)
              YRDEAD = IYEAR - JYR + 1
              SDT(IDC,JYR,JCL) = SDH(IDC,JYR,JCL) + SDS(IDC,JYR,JCL)
              SVLT(IDC,JYR,JCL) = SVLH(IDC,JYR,JCL) + SVLS(IDC,JYR,JCL)
              IF (SDT(IDC,JYR,JCL) .LE. 0) GOTO 150
C
C             DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C             KEYWORD OVER RIDES
C
              IF(JSPIN(IDC).EQ.1)THEN
                CSPECIES=ADJUSTL(JSP(IDC))
              ELSEIF(JSPIN(IDC).EQ.2)THEN
                CSPECIES=ADJUSTL(FIAJSP(IDC))
              ELSEIF(JSPIN(IDC).EQ.3)THEN
                CSPECIES=ADJUSTL(PLNJSP(IDC))
              ELSE
                CSPECIES=ADJUSTL(PLNJSP(IDC))
              ENDIF
C
              IF(ISPOUT23.EQ.1)CSPECIES=ADJUSTL(JSP(IDC))
              IF(ISPOUT23.EQ.2)CSPECIES=ADJUSTL(FIAJSP(IDC))
              IF(ISPOUT23.EQ.3)CSPECIES=ADJUSTL(PLNJSP(IDC))
C
C             ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
              SDBHB(IDC,JYR,JCL) = SDBH(IDC,JYR,JCL)
              SHTHB(IDC,JYR,JCL) = SHTH(IDC,JYR,JCL)
              SHTSB(IDC,JYR,JCL) = SHTS(IDC,JYR,JCL)
              SDHB(IDC,JYR,JCL) = SDH(IDC,JYR,JCL)
              SDSB(IDC,JYR,JCL) = SDS(IDC,JYR,JCL)
              SDTB(IDC,JYR,JCL) = SDT(IDC,JYR,JCL)


              WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,
     -          '(CaseID,StandID,Year,Species,DBH_Class,Death_DBH,',
     -          'Current_Ht_Hard,Current_Ht_Soft,Current_Vol_Hard,',
     -          'Current_Vol_Soft,Total_Volume,Year_Died,Density_Hard,',
     -          'Density_Soft,Density_Total) VALUES (''',
     -          CASEID,''',''',TRIM(NPLT),''',?,''',
     -          TRIM(CSPECIES),''',?,?,?,?,?,?,?,?,?,?,?)'

              !PRINT*, SQLStmtStr
C
C             CLOSE CURSOR
C
              iRet = fvsSQLCloseCursor(StmtHndlOut)
C
C             PREPARE THE SQL QUERY
C
              iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C             BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C

              ColNumber=1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,
     -          INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -          IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,
     -          INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),
     -          JCL,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SDBHB(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT,SQL_F_DOUBLE, SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND), INT(5,SQLSMALLINT_KIND),
     -          SHTHB(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SHTSB(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT,SQL_F_INTEGER, SQL_INTEGER,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SVLH(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT,SQL_F_INTEGER, SQL_INTEGER,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SVLS(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SVLT(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,
     -          INT(15,SQLUINTEGER_KIND), INT(5,SQLSMALLINT_KIND),
     -          YRDEAD,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND), INT(5,SQLSMALLINT_KIND),
     -          SDHB(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SDSB(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

              ColNumber=ColNumber+1
              iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -          SQL_PARAM_INPUT,SQL_F_DOUBLE, SQL_DOUBLE,
     -          INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),
     -          SDTB(IDC,JYR,JCL),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

  100         CONTINUE

              iRet = fvsSQLCloseCursor(StmtHndlOut)

              iRet = fvsSQLExecute(StmtHndlOut)
              CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                      'DBSFMDSNAG:Inserting Row')
  150         CONTINUE
            ENDDO
        ENDDO
      ENDDO

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


