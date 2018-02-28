      SUBROUTINE DBSFMCRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C DBS $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE MAIN CARBON REPORT
C              INFORMATION
C     AUTH: D. ROBINSON - ESSA
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C       UNITS ARE TONS/ACRE IF USER HAS REQUESTED IMPERIAL UNITS; TONNES/HA IF METRIC
C         1 = ABOVEGROUND TOTAL LIVE
C         2 = ABOVEGOURND MERCH LIVE
C         3 = BELOWGROUND LIVE
C         4 = BELOWGROUND DEAD (WHICH DECAYS)
C         5 = STANDING DEAD
C         6 = FOREST DOWN DEAD WOOD
C         7 = FOREST FLOOR (SOILS?)
C         8 = FOREST SHRUB+HERB
C         9 = TOTAL STAND CARBON
C        10 = TOTAL CARBON REMOVED THIS REPORTING PERIOD
C        11 = CARBON RELEASED THRU FIRE
C       KODE   - RETURN CODE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR, KODE,VARDIM
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           IRCODE,I
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      CHARACTER*2000    SQLStmtStr
      CHARACTER(len=20) TABLENAME

C     Initialize variables

      IF(ICMRPT.EQ.0) RETURN
      IF(ICMRPT.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ICMRPT = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     >    'DBSFMCRPT:DSN Connection')
        GOTO 200
      ENDIF

C     CHECK TO SEE IF THE MAIN CARBON TABLE EXISTS IN DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Carbon$]'
      ELSE
        TABLENAME = 'FVS_Carbon'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) THEN
        ICMRPT = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Carbon('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Aboveground_Total_Live double null,'//
     -              'Aboveground_Merch_Live double null,'//
     -              'Belowground_Live double null,'//
     -              'Belowground_Dead double null,'//
     -              'Standing_Dead double null,'//
     -              'Forest_Down_Dead_Wood double null,'//
     -              'Forest_Floor double null,'//
     -              'Forest_Shrub_Herb double null,'//
     -              'Total_Stand_Carbon double null,'//
     -              'Total_Removed_Carbon double null,'//
     -              'Carbon_Released_From_Fire double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Carbon('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int ,'//
     -              'Aboveground_Total_Live Number,'//
     -              'Aboveground_Merch_Live Number,'//
     -              'Belowground_Live Number,'//
     -              'Belowground_Dead Number,'//
     -              'Standing_Dead Number,'//
     -              'Forest_Down_Dead_Wood Number,'//
     -              'Forest_Floor Number,'//
     -              'Forest_Shrub_Herb Number,'//
     -              'Total_Stand_Carbon Number,'//
     -              'Total_Removed_Carbon Number,'//
     -              'Carbon_Released_From_Fire Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_Carbon('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Aboveground_Total_Live real null,'//
     -              'Aboveground_Merch_Live real null,'//
     -              'Belowground_Live real null,'//
     -              'Belowground_Dead real null,'//
     -              'Standing_Dead real null,'//
     -              'Forest_Down_Dead_Wood real null,'//
     -              'Forest_Floor real null,'//
     -              'Forest_Shrub_Herb real null,'//
     -              'Total_Stand_Carbon real null,'//
     -              'Total_Removed_Carbon real null,'//
     -              'Carbon_Released_From_Fire real null)'
        ENDIF
            !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     >    'DBSFMCRPT:Creating Table: '//trim(SQLStmtStr))
      ENDIF
C     COPY INPUT VECTOR TO DOUBLE-PRECISION

      DO I=1,VARDIM
        VARD(I) = VAR(I)
      ENDDO

      WRITE(SQLStmtStr,*)'INSERT INTO ',TRIM(TABLENAME),' (CaseID,',
     >  'StandID,Year,Aboveground_Total_Live,Aboveground_Merch_Live,',
     >  'Belowground_Live,Belowground_Dead,Standing_Dead,',
     >  'Forest_Down_Dead_Wood,Forest_Floor,Forest_Shrub_Herb,',
     >  'Total_Stand_Carbon,Total_Removed_Carbon,',
     >  'Carbon_Released_From_Fire) VALUES (''',
     >  CASEID,''',''',TRIM(NPLT),
     >  ''',?,?,?,?,?,?,?,?,?,?,?,?)'

C     CLOSE CURSOR

      iRet = fvsSQLCloseCursor(StmtHndlOut)

C     PREPARE THE SQL QUERY

      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      DO I=1,VARDIM
        ColNumber=INT(ColNumber+1)
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),VARD(I),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      ENDDO


      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSFMCRPT:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
