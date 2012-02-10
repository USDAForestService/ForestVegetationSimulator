      subroutine DBSECSUM(beginAnalYear, endTime, pretend, costUndisc,
     &      revUndisc, costDisc, revDisc, npv, irr, irrCalculated,
     &      bcRatio, bcRatioCalculated, rrr, rrrCalculated,
     &      sev, sevCalculated, forestValue, forestValueCalculated,
     &      reprodValue, reprodValueCalculated, ft3Volume, bfVolume,
     &      discountRate, sevInput, sevInputUsed)

      implicit none

      include 'DBSCOM.F77'

      character(len=*), parameter :: tableName = 'FVS_EconSummary'
      
      character(len=30)   :: decoratedTableName
      character(len=*)    :: pretend
      character(len=2000) :: SQLStmtStr
        logical success
        integer(SQLPOINTER_KIND) :: maybeNullNeg
        integer(SQLPOINTER_KIND) :: maybeNullLog

      integer, parameter :: zero = 0
      integer :: beginAnalYear, endTime, status,
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

!    Ensure that the FVS_EconSummary table exists in the DB.
      if(trim(DBMSOUT) .eq. 'EXCEL') then
        decoratedTableName = '[' // tableName // '$]'
      else
        decoratedTableName = tableName
      end if
      SQLStmtStr = 'SELECT * FROM ' // decoratedTableName
      iRet = fvsSQLExecDirect(
     &            StmtHndlOut,SQLStmtStr,
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      if(.not. success(iRet)) then
        if(trim(DBMSOUT) .eq. 'ACCESS') then
            SQLStmtStr = 'CREATE TABLE ' // tableName // ' ('
     &          // 'CaseID int, '
     &          // 'Year int null, '
     &          // 'Period int null, '
     &          // 'Pretend_Harvest text null, '
     &          // 'Undiscounted_Cost double null, '
     &          // 'Undiscounted_Revenue double null, '
     &          // 'Discounted_Cost double null, '
     &          // 'Discounted_Revenue double null, '
     &          // 'PNV double null, '
     &          // 'IRR double null, '
     &          // 'BC_Ratio double null, '
     &          // 'RRR double null, '
     &          // 'SEV double null, '
     &          // 'Value_of_Forest double null, '
     &          // 'Value_of_Trees double null, '
     &          // 'Mrch_Cubic_Volume int null, '
     &          // 'Mrch_BoardFoot_Volume int null, '
     &          // 'Discount_Rate double null, '
     &          // 'Known_SEV double null, '
     &          // 'CONSTRAINT ' // tableName // '_PK '
     &          // 'PRIMARY KEY(CaseID, Year))'
        elseif(trim(DBMSOUT) .eq. 'EXCEL') then
            SQLStmtStr = 'CREATE TABLE ' // tableName // ' ('
     &          // 'CaseID Int, '
     &          // 'Year Int, '
     &          // 'Period Int, '
     &          // 'Pretend_Harvest Text, '
     &          // 'Undiscounted_Cost Number, '
     &          // 'Undiscounted_Revenue Number, '
     &          // 'Discounted_Cost Number, '
     &          // 'Discounted_Revenue Number, '
     &          // 'PNV Number, '
     &          // 'IRR Number, '
     &          // 'BC_Ratio Number, '
     &          // 'RRR Number, '
     &          // 'SEV Number, '
     &          // 'Value_of_Forest Number, '
     &          // 'Value_of_Trees Number, '
     &          // 'Mrch_Cubic_Volume Int, '
     &          // 'Mrch_BoardFoot_Volume Int, '
     &          // 'Discount_Rate Number, '
     &          // 'Known_SEV Number)'
        else
            SQLStmtStr = 'CREATE TABLE ' // tableName // ' ('
     &          // 'CaseID int, '
     &          // 'Year int null, '
     &          // 'Period int null, '
     &          // 'Pretend_Harvest char(3) null, '
     &          // 'Undiscounted_Cost real null, '
     &          // 'Undiscounted_Revenue real null, '
     &          // 'Discounted_Cost real null, '
     &          // 'Discounted_Revenue real null, '
     &          // 'PNV real null, '
     &          // 'IRR real null, '
     &          // 'BC_Ratio real null, '
     &          // 'RRR real null, '
     &          // 'SEV real null, '
     &          // 'Value_of_Forest real null, '
     &          // 'Value_of_Trees real null, '
     &          // 'Mrch_Cubic_Volume int null, '
     &          // 'Mrch_BoardFoot_Volume int null, '
     &          // 'Discount_Rate real null, '
     &          // 'Known_SEV real null, '
     &          // 'CONSTRAINT ' // tableName // '_PK '
     &          // 'PRIMARY KEY(CaseID, Year))'
        end if
         
         iRet = fvsSQLFreeStmt(StmtHndlOut, SQL_CLOSE)
         iRet = fvsSQLExecDirect(
     &         StmtHndlOut,
     &         trim(SQLStmtStr),
     -         int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
         call DBSDIAGS(
     &         SQL_HANDLE_STMT,
     &         StmtHndlOut,
     &         'DBSECSUM:Creating Table: ' // trim(SQLStmtStr))
      end if

!    Make sure we do not exceed Excel's maximum table size.
      !if(tooManyRows(decoratedTableName)) then
      !      goto 100
      !end if

!    Insert a row of data into the summary table.
      write(SQLStmtStr, *) 'INSERT INTO ',
     &   decoratedTableName,'(CaseID, Year, Period,',
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
     &   'DBSECSUM:Preparing Statement: ' // trim(SQLStmtStr))

      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(1, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_INTEGER,
     &         SQL_INTEGER,
     &         int(0, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         beginAnalYear,
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
     &         SQL_NULL_PTR)
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(3, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_CHAR,
     &         SQL_CHAR,
     &         int(3, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         pretend,
     &         int(len_trim(pretend), SQLLEN_KIND),
     &         int(len_trim(pretend), SQLLEN_KIND))
      iRet = fvsSQLBindParameter(
     &         StmtHndlOut,
     &         int(4, SQLSMALLINT_KIND),
     &         SQL_PARAM_INPUT,
     &         SQL_F_FLOAT,
     &         SQL_REAL,
     &         int(1, SQLUINTEGER_KIND),
     &         int(0, SQLSMALLINT_KIND),
     &         costUndisc,
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
     &         SQL_NULL_PTR,
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
