      subroutine DBSECHARV_open()

C $Id$

        implicit none

        include 'DBSCOM.F77'

        character(len=*), parameter :: TABLENAME='FVS_EconHarvestValue'

        character(len=30)   :: decoratedTableName
        character(len=1000) :: SQLStmtStr
        logical success
        integer(SQLPOINTER_KIND) :: maybeNullNeg
        integer IRCODE
        
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

        ! Ensure that the FVS_EconHarvestValue table exists in the DB.
         if(trim(DBMSOUT) .eq. 'EXCEL') then
            decoratedTableName = '[' // TABLENAME // '$]'
         else
            decoratedTableName = TABLENAME
         end if
         CALL DBSCKNROWS(IRCODE,decoratedTableName,1,
     >                TRIM(DBMSOUT).EQ.'EXCEL')
         IF(IRCODE.EQ.2) THEN
           IDBSECON = 0
           RETURN
         ENDIF
         IF(IRCODE.EQ.1) THEN
            if(trim(DBMSOUT) .eq. 'ACCESS') then
                SQLStmtStr = 'CREATE TABLE ' // TABLENAME // ' ('
     &              // 'CaseID Text not null,'
     &              // 'Year int not null,'
     &              // 'Species text not null,'
     &              // 'Min_DIB double null,'
     &              // 'Max_DIB double null,'
     &              // 'Min_DBH double null,'
     &              // 'Max_DBH double null,'
     &              // 'TPA_Removed int null,'
     &              // 'TPA_Value int null,'
     &              // 'Tons_Per_Acre int null,'
     &              // 'Ft3_Removed int null,'
     &              // 'Ft3_Value int null,'
     &              // 'Board_Ft_Removed int null,'
     &              // 'Board_Ft_Value int null,'
     &              // 'Total_Value int null)'
            elseif(trim(DBMSOUT) .eq. 'EXCEL') then
                SQLStmtStr = 'CREATE TABLE ' // TABLENAME // ' ('
     &              // 'CaseID Text,'
     &              // 'Year Int,'
     &              // 'Species Text,'
     &              // 'Min_DIB Number,'
     &              // 'Max_DIB Number,'
     &              // 'Min_DBH Number,'
     &              // 'Max_DBH Number,'
     &              // 'TPA_Removed Int,'
     &              // 'TPA_Value Int,'
     &              // 'Tons_Per_Acre Int,'
     &              // 'Ft3_Removed Int,'
     &              // 'Ft3_Value Int,'
     &              // 'Board_Ft_Removed Int,'
     &              // 'Board_Ft_Value Int,'
     &              // 'Total_Value Int)'
            else
                SQLStmtStr = 'CREATE TABLE ' // TABLENAME // ' ('
     &              // 'CaseID char(36) not null,'
     &              // 'Year int not null,'
     &              // 'Species char(8) not null,'
     &              // 'Min_DIB real null,'
     &              // 'Max_DIB real null,'
     &              // 'Min_DBH real null,'
     &              // 'Max_DBH real null,'
     &              // 'TPA_Removed int null,'
     &              // 'TPA_Value int null,'
     &              // 'Tons_Per_Acre int null,'
     &              // 'Ft3_Removed int null,'
     &              // 'Ft3_Value int null,'
     &              // 'Board_Ft_Removed int null,'
     &              // 'Board_Ft_Value int null,'
     &              // 'Total_Value int null)'
            end if

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
     &      decoratedTableName,'(CaseID,Year,Species,',
     &      'Min_DIB,Max_DIB,Min_DBH,Max_DBH,TPA_Removed,TPA_Value,',
     &      'Tons_Per_Acre,Ft3_Removed,Ft3_Value,Board_Ft_Removed,',
     &      'Board_Ft_Value,Total_Value) VALUES (''',CASEID,
     &      ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
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

         include 'PRGPRM.F77' !Contains MAXSP
         include 'PLOT.F77'   !Contains JSPIN(speciesId,speciesSymboltype), JSP, FIAJSP, & PLNJSP
         include 'DBSCOM.F77'


         character(len=*), parameter :: TABLENAME='FVS_EconHarvestValue'
         integer, parameter :: zero = 0

         character(len=8) :: species
         real    :: minDia, maxDia, minDbh, maxDbh
         integer :: beginAnalYear, tpaCut, tpaValue, tonsPerAcre,
     &              ft3Volume, ft3Value, bfVolume, bfValue, totalValue
         integer, intent(in) :: speciesId
         integer(SQLPOINTER_KIND) :: maybeNullNeg

         if(IDBSECON < 2) return   !ECON harvest table was not requested

         !if(tooManyRows(decoratedTableName)) return    !database table will exceed Excel's maximum row count

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

          iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(1, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_INTEGER,
     &            SQL_INTEGER,
     &            int(0, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            beginAnalYear,
     &            SQL_NULL_PTR,
     &            SQL_NULL_PTR)
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(2, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_CHAR,
     &            SQL_CHAR,
     &            int(8, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            species,
     &            int(len_trim(species), SQLLEN_KIND),
     &            int(len_trim(species), SQLLEN_KIND))
         iRet = fvsSQLBindParameter(
     &            StmtHndlOut,
     &            int(3, SQLSMALLINT_KIND),
     &            SQL_PARAM_INPUT,
     &            SQL_F_FLOAT,
     &            SQL_REAL,
     &            int(1, SQLUINTEGER_KIND),
     &            int(0, SQLSMALLINT_KIND),
     &            minDia,
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
     &            SQL_NULL_PTR,
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
