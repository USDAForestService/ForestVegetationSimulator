      subroutine DBSECSUM(beginAnalYear, endTime, pretend, costUndisc,
     &      revUndisc, costDisc, revDisc, npv, irr, irrCalculated,
     &      bcRatio, bcRatioCalculated, rrr, rrrCalculated,
     &      sev, sevCalculated, forestValue, forestValueCalculated,
     &      reprodValue, reprodValueCalculated, ft3Volume, bfVolume,
     &      discountRate, sevInput, sevInputUsed)

      implicit none

      include 'DBSCOM.F77'

      character(len=*), parameter :: tableName = 'FVS_EconSummary'
      character(len=*)            :: pretend
      character(len=2000)         :: SQLStmtStr
        logical success
        integer(SQLPOINTER_KIND) :: maybeNullNeg
        character :: decorateTableName
        character(len=45) :: columnDecl
        logical :: tooManyRows
        character(len=3) :: tInt
        character(len=6) :: tReal
        character(len=9) :: tText
        integer(SQLPOINTER_KIND) :: maybeNullLog
        character :: omitForExcel

      integer, parameter :: zero = 0
      integer :: beginAnalYear, endTime, pretendLength, status,
     &           ft3Volume, bfVolume

      logical :: sevCalculated, rrrCalculated,
     &           forestValueCalculated, reprodValueCalculated,
     &           irrCalculated, bcRatioCalculated, sevInputUsed

      real :: costUndisc, revUndisc, costDisc, revDisc, npv, irr,
     &        bcRatio, rrr, sev, forestValue, reprodValue, discountRate,
     &        sevInput

      if (IDBSECON == 0) return

!    Make sure we have an up-to-date case ID.
      call DBSCASE(1)

!    Allocate a statement handle.
      iRet = fvsSQLAllocHandle(
     &      SQL_HANDLE_STMT,
     &      ConnHndlOut,
     &      StmtHndlOut)
      if(.not. success(iRet)) then
         IDBSECON = 0
         print *, 'Error connnecting to data source'
         call DBSDIAGS(
     &         SQL_HANDLE_DBC,
     &         ConnHndlOut,
     &         'DBSECSUM:DSN Connection')
         goto 100
      end if

!    Ensure that the FVS_EcSummary table exists in the DB.
      SQLStmtStr = 'SELECT * FROM ' // decorateTableName(tableName)
      iRet = fvsSQLExecDirect(
     &            StmtHndlOut,SQLStmtStr,
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      if(.not. success(iRet)) then
         sqlStmtStr = 'CREATE TABLE ' // tableName // ' ('
     &      // columnDecl('CaseID',tInt(),'') // ','
     &      // columnDecl('Year',tInt(),'null') // ','
     &      // columnDecl('Period',tInt(),'null') // ','
     &      // columnDecl('Pretend_Harvest',tText('3'),'null') // ','
     &      // columnDecl('Undiscounted_Cost',tReal(),'null')//','
     &      // columnDecl('Undiscounted_Revenue',tReal(),'null') // ','
     &      // columnDecl('Discounted_Cost',tReal(),'null') // ','
     &      // columnDecl('Discounted_Revenue',tReal(),'null') // ','
     &      // columnDecl('PNV',tReal(),'null') // ','
     &      // columnDecl('IRR',tReal(),'null') // ','
     &      // columnDecl('BC_Ratio',tReal(),'null') // ','
     &      // columnDecl('RRR',tReal(),'null') // ','
     &      // columnDecl('SEV',tReal(),'null') // ','
     &      // columnDecl('Value_of_Forest',tReal(),'null') // ','
     &      // columnDecl('Value_of_Trees',tReal(),'null') // ','
     &      // columnDecl('Mrch_Cubic_Volume',tInt(),'null') // ','
     &      // columnDecl('Mrch_BoardFoot_Volume',tInt(),'null') // ','
     &      // columnDecl('Discount_Rate',tReal(),'null') // ','
     &      // columnDecl('Known_SEV',tReal(),'null')
     &      // omitForExcel(
     &            ',CONSTRAINT ' // tableName // '_PK '
     &             // 'PRIMARY KEY(CaseID,Year)')
     &      // ')'
         iRet = fvsSQLFreeStmt(StmtHndlOut, SQL_CLOSE)
         iRet = fvsSQLExecDirect(
     &         StmtHndlOut,
     &         trim(SQLStmtStr),
     -         int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
         call DBSDIAGS(
     &         SQL_HANDLE_STMT,
     &         StmtHndlOut,
     &         'DBSECSUM:Creating Table: ' // trim(sqlStmtStr))
      end if

!    Make sure we do not exceed Excel's maximum table size.
      if(tooManyRows(tableName)) then
            goto 100
      end if

!    Insert a row of data into the summary table.
      write(SQLStmtStr, *) 'INSERT INTO ',
     &   decorateTableName(tableName),'(CaseID, Year, Period,',
     &   'Pretend_Harvest, Undiscounted_Cost, Undiscounted_Revenue,',
     &   'Discounted_Cost, Discounted_Revenue, PNV, IRR, BC_Ratio,',
     &   'RRR, SEV, Value_of_Forest, Value_of_Trees,', 
     &   'Mrch_Cubic_Volume, Mrch_BoardFoot_Volume, Discount_Rate,',
     &   'Known_SEV)',
     &   'VALUES (',ICASE,',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      call DBSDIAGS(
     &   SQL_HANDLE_STMT,
     &   StmtHndlOut,
     &   'DBSECSUM_open:Preparing Statement: ' // trim(SQLStmtStr))

      pretendLength = len_trim(pretend)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(1, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_INTEGER,
     &         SQL_INTEGER,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         beginAnalYear,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(2, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_INTEGER,
     &         SQL_INTEGER,
     &         int(2, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         endTime,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(3, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_CHAR,
     &         SQL_CHAR,
     &         int(3, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         trim(pretend),
     &         loc(pretendLength))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(4, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(1, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         costUndisc,
     &         maybeNullNeg(costUndisc))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(5, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(1, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         revUndisc,
     &         maybeNullNeg(revUndisc))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(6, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(1, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         costDisc,
     &         maybeNullNeg(costDisc))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(7, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         revDisc,
     &         maybeNullNeg(revDisc))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(8, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         npv,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(9, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         irr,
     &         maybeNullLog(irr, irrCalculated))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(10, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         bcRatio,
     &         maybeNullLog(bcRatio, bcRatioCalculated))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(11, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         rrr,
     &         maybeNullLog(rrr, rrrCalculated))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(12, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         sev,
     &         maybeNullLog(sev, sevCalculated))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(13, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         forestValue,
     &         maybeNullLog(forestValue, forestValueCalculated))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(14, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         reprodValue,
     &         maybeNullLog(reprodValue, reprodValueCalculated))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(15, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_INTEGER,
     &         SQL_INTEGER,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         ft3Volume,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(16, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_INTEGER,
     &         SQL_INTEGER,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         bfVolume,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(17, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         discountRate,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(18, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         sevInput,
     &         maybeNullLog(sevInput, sevInputUsed))
      iRet = fvsSQLFreeStmt(StmtHndlOut, SQL_CLOSE)
      iRet = fvsSQLExecute(StmtHndlOut)
      call DBSDIAGS(
     &      SQL_HANDLE_STMT,
     &      StmtHndlOut,
     &      'DBSECSUM:Executing Prepared Statement')

  100 continue
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
      return

      entry getDbsEconStatus(status)
         if(IDBSECON == 0) then
            status = 0   !DB-FVS output not requested
         else if(IDBSECON == 1) then
            status = 1   ! Write to summary output to DB.
         else
            status = 2   ! Write to summary and harvest output to DB.
         end if
      return

      end
