      subroutine DBSECHARV_open()

        implicit none

        include 'DBSCOM.F77'

        character(len=*), parameter :: TABLENAME='FVS_EconHarvestValue'

        character(len=1000) :: SQLStmtStr
        logical success
        integer(SQLPOINTER_KIND) :: maybeNullNeg
        character :: decorateTableName
        character(len=45) :: columnDecl
        logical :: tooManyRows
        character(len=3) :: tInt
        character(len=6) :: tReal
        character(len=9) :: tText

        if(IDBSECON < 2) return

        ! Make sure we have an up-to-date case ID.
         call DBSCASE(1)

        ! Allocate a statement handle.
         iRet = fvsSQLAllocHandle(
     &         SQL_HANDLE_STMT,
     &         ConnHndlOut,
     &         StmtHndlOut)
         if(.not. success(iRet)) then
            IDBSECON = 0
            print *, 'Error connnecting to data source'
            call DBSDIAGS(
     &            SQL_HANDLE_DBC,
     &            ConnHndlOut,
     &            'DBSECHARV_open:DSN Connection')
            return
         end if

        ! Ensure that the FVS_EcHarvest table exists in the DB.
         SQLStmtStr = 'SELECT * FROM ' // decorateTableName(TABLENAME)
         iRet = fvsSQLExecDirect(
     &         StmtHndlOut,SQLStmtStr,
     -         int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

         if(.not. success(iRet)) then
            sqlStmtStr = 'CREATE TABLE ' // TABLENAME // ' ('
     &         // columnDecl('CaseID',tInt(),'not null') // ','
     &         // columnDecl('Year',tInt(),'not null') // ','
     &         // columnDecl('Species',tText('8'),'not null') // ','
     &         // columnDecl('Min_DIB',tReal(),'null') // ','
     &         // columnDecl('Max_DIB',tReal(),'null') // ','
     &         // columnDecl('Min_DBH',tReal(),'null') //','
     &         // columnDecl('Max_DBH',tReal(),'null') // ','
     &         // columnDecl('TPA_Removed',tInt(),'null') // ','
     &         // columnDecl('TPA_Value',tInt(),'null') // ','
     &         // columnDecl('Tons_Per_Acre',tInt(),'null') // ','
     &         // columnDecl('Ft3_Removed',tInt(),'null') // ','
     &         // columnDecl('Ft3_Value',tInt(),'null') // ','
     &         // columnDecl('Board_Ft_Removed',tInt(),'null') //','
     &         // columnDecl('Board_Ft_Value',tInt(),'null') // ','
     &         // columnDecl('Total_Value',tInt(),'null') !// ','
!     &         // omitForExcel(
!     &               'CONSTRAINT ' // TABLENAME // '_PK '
!     &                  // 'PRIMARY KEY(CaseID,Year,Species,Min_DIB,'
!     &                  // 'Max_DIB,Min_DBH,Max_DBH)')
     &         // ')'
            iRet = fvsSQLFreeStmt(StmtHndlOut, SQL_CLOSE)
            iRet = fvsSQLExecDirect(
     &            StmtHndlOut, trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            call DBSDIAGS(
     &            SQL_HANDLE_STMT,
     &            StmtHndlOut,
     &            'DBSECHARV_open:Creating Table: ' // trim(SQLStmtStr))
         end if

         write(SQLStmtStr, *) 'INSERT INTO ',
     &      decorateTableName(TABLENAME),'(CaseID,Year,Species,',
     &      'Min_DIB,Max_DIB,Min_DBH,Max_DBH,TPA_Removed,TPA_Value,',
     &      'Tons_Per_Acre,Ft3_Removed,Ft3_Value,Board_Ft_Removed,',
     &      'Board_Ft_Value,Total_Value) VALUES (',ICASE,
     &      ',?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
         iRet = fvsSQLCloseCursor(StmtHndlOut)
         iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
         call DBSDIAGS(
     &      SQL_HANDLE_STMT,
     &      StmtHndlOut,
     &      'DBSECHARV_open:Preparing Statement: ' // trim(SQLStmtStr))
      end subroutine DBSECHARV_open


      subroutine DBSECHARV_insert(beginAnalYear, speciesId, minDia,
     &      maxDia, minDbh, maxDbh, tpaCut, tpaValue, tonsPerAcre,
     &      ft3Volume, ft3Value, bfVolume, bfValue, totalValue)

         implicit none

         include 'PRGPRM.F77'                                            !Contains MAXSP
         include 'PLOT.F77'                                              !Contains JSPIN(speciesId,speciesSymboltype), JSP, FIAJSP, & PLNJSP
         include 'DBSCOM.F77'


         character(len=*), parameter :: TABLENAME='FVS_EconHarvestValue'
         integer, parameter :: zero = 0

         character(len=8) :: species
         real    :: minDia, maxDia, minDbh, maxDbh
         integer :: beginAnalYear, tpaCut, tpaValue, tonsPerAcre,
     &              ft3Volume, ft3Value, bfVolume, bfValue, totalValue
         integer :: speciesLength
         integer, intent(in) :: speciesId
         logical :: tooManyRows
         integer(SQLPOINTER_KIND) :: maybeNullNeg
         
         if(IDBSECON < 2) return                                         !ECON harvest table was not requested

         if(tooManyRows(TABLENAME)) return                               !database table will exceed Excel's maximum row count

         !Determine preferred species output format - keyword has precedence
         select case (ISPOUT30)
           case (1)
              species = adjustl(trim(JSP(speciesId)))
           case (2)
              species = adjustl(trim(FIAJSP(speciesId)))
           case (3)
              species = adjustl(trim(PLNJSP(speciesId)))
           case default
              if (JSPIN(speciesId) == 1) then
                 species = adjustl(trim(JSP(speciesId)))
              else if (JSPIN(speciesId) == 2) then
                 species = adjustl(trim(FIAJSP(speciesId)))
              else if (JSPIN(speciesId) == 3) then
                 species = adjustl(trim(PLNJSP(speciesId)))
              else
                 species = adjustl(trim(PLNJSP(speciesId)))
               endif
          end select

          speciesLength = len_trim(species)
          iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(1, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            beginAnalYear,
     &            SQL_NULL_PTR)
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(2, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_CHAR,
     &            SQL_CHAR,
     &            int(8, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            trim(species),
     &            loc(speciesLength))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(3, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_FLOAT,
     &            SQL_REAL,
     &            int(1, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            minDia,
     &            maybeNullNeg(minDia))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(4, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_FLOAT,
     &            SQL_REAL,
     &            int(1, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            maxDia,
     &            maybeNullNeg(maxDia))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(5, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_FLOAT,
     &            SQL_REAL,
     &            int(1, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            minDbh,
     &            maybeNullNeg(minDbh))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(6, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_FLOAT,
     &            SQL_REAL,
     &            int(1, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            maxDbh,
     &            maybeNullNeg(maxDbh))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(7, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            tpaCut,
     &            maybeNullNeg(real(tpaCut)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(8, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            tpaValue,
     &            maybeNullNeg(real(tpaValue)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(9, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            tonsPerAcre,
     &            maybeNullNeg(real(tonsPerAcre)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(10, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            ft3Volume,
     &            maybeNullNeg(real(ft3Volume)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(11, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            ft3Value,
     &            maybeNullNeg(real(ft3Value)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(12, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            bfVolume,
     &            maybeNullNeg(real(bfVolume)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(13, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            bfValue,
     &            maybeNullNeg(real(bfValue)))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(14, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            totalValue,
     &            maybeNullNeg(real(totalValue)))
         iRet = fvsSQLFreeStmt(StmtHndlOut, SQL_CLOSE)
         iRet = fvsSQLExecute(StmtHndlOut)
         call DBSDIAGS(
     &      SQL_HANDLE_STMT,
     &      StmtHndlOut,
     &      'DBSECHARV_insert:Executing Prepared Statement')
      end subroutine DBSECHARV_insert

      subroutine DBSECHARV_close()

         include 'DBSCOM.F77'

         if(IDBSECON == 2)
     &         iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      end subroutine DBSECHARV_close
