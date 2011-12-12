      function success(returnCode)
      INCLUDE 'DBSTYPEDEFS.F77'
            integer(SQLRETURN_KIND), intent(in) :: returnCode
            logical success
            success =    returnCode == SQL_SUCCESS
     -              .or. returnCode == SQL_SUCCESS_WITH_INFO
      end function success

      function maybeNullNeg(n)
        implicit none
        INCLUDE 'DBSTYPEDEFS.F77'
            integer(SQLPOINTER_KIND) :: maybeNullNeg
            integer(SQLPOINTER_KIND) :: maybeNullLog
            real, intent(in) :: n
            
            maybeNullNeg = maybeNullLog(n, n >= 0.0)
      end function maybeNullNeg

      function maybeNullLog(n, p)
         implicit none
         INCLUDE 'DBSTYPEDEFS.F77'
         integer(SQLPOINTER_KIND) :: maybeNullLog
         integer, parameter :: zero = 0
            real, intent(in) :: n
            logical, intent(in) :: p

            if(p) then
                maybeNullLog = zero
            else
                maybeNullLog = SQL_NULL_DATA
            end if
      end function maybeNullLog
        
      function decorateTableName(name)
         implicit none
            character(len=*), intent(in) :: name
            character(len=len_trim(name) + 3) :: decorateTableName
            character ::  byDbms
              
            decorateTableName = byDbms('['//trim(name)//'$]',name,name)
      end function decorateTableName
        
      function tooManyRows(tableName)
         implicit none
         INCLUDE 'DBSCOM.F77'
            character(len=*), intent(in) :: tableName
            character :: decorateTableName
            character(len=300) SQLStmtStr
            integer(SQLUINTEGER_KIND) :: rowCount
            logical :: tooManyRows
              
            iRet = fvsSQLAllocHandle(
     &          SQL_HANDLE_STMT,
     &          ConnHndlOut,
     &          StmtHndlIn)
     
             SQLStmtStr  = 'SELECT COUNT(CaseID) FROM '
     &               // decorateTableName(tableName)
     
            iRet = fvsSQLExecDirect(
     &            StmtHndlIn,SQLStmtStr,
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            iRet = fvsSQLFetch(StmtHndlIn)
            iRet = fvsSQLGetData(
     &            StmtHndlIn,
     &            int(1, SQLUSMALLINT_KIND),
     &            SQL_F_ULONG,
     &            loc(rowCount),
     &            0,
     &            SQL_NULL_PTR)
            iRet = fvsSQLFreeStmt(StmtHndlIn, SQL_CLOSE)
            iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlIn)
                 
            tooManyRows =     rowCount >= 65535
     &                  .and. trim(DBMSOUT) == 'EXCEL'
      end function tooManyRows
        
      function columnDecl(name, type, attr)
          implicit none
            character(len=45) :: columnDecl
            character(len=*), intent(in) :: name, type, attr
            character :: omitForExcel
              
            columnDecl = name // ' ' // type
     &                        // omitForExcel(' ' // attr)
      end function columnDecl
        
      function tInt()
         implicit none
         character(len=3) :: tInt
         character ::  byDbms
         tInt = byDbms('Int', 'int', 'int')
      end function tInt
        
      function tReal()
         implicit none
         character(len=6) :: tReal
         character ::  byDbms
         tReal = byDbms('Number', 'double', 'real')
      end function tReal
        
      function tText(size)
         implicit none
         character(len=*), intent(in) :: size
         character(len=9) :: tText
         character ::  byDbms
            
         tText = byDbms('Text', 'text', 'char(' // size // ')')
      end function tText
        
      function omitForExcel(s)
         implicit none
         character(len=*), intent(in) :: s
         character(len=len_trim(s)) :: omitForExcel
         character ::  byDbms
              
         omitForExcel = byDbms('', s, s)
      end function omitForExcel
        
      function byDbms(excel, access, other)
         implicit none
         include 'DBSCOM.F77'
            character(len=*), intent(in) :: excel, access, other
            character(len=max(
     &            len_trim(excel),
     &            len_trim(access),
     &            len_trim(other))) :: byDbms
              
            if(trim(DBMSOUT) == 'EXCEL') then
                  byDbms = excel
            else if(trim(DBMSOUT) == 'ACCESS') then
                  byDbms = access
            else
                  byDbms = other
            end if
      end function byDbms
