!----------
! DBSQLITE $Id$
!----------
!     API routines that read the FVS output tables when the output database is
!     held in memory ("memory mode", see fvsSetMemoryTables).
!
!     Tables are named as in the output database (e.g. "FVS_Summary2"). Every
!     column whose declared type is text (TEXT, CHAR, CLOB) is a text column,
!     every other column is a numeric column returned as real(real64), with
!     NULL returned as NaN. Columns keep table order within each group. Rows
!     are numbered from 1 in the order written.
!
!     Name and text lists are returned in one character buffer, each entry
!     followed by char(0). The buffer length is passed in; on return it holds
!     the number of characters used, or needed if the buffer was too small
!     (rtnCode = 2).
!
!     Return codes: 0 OK,
!                   1 table not found, or memory mode is off,
!                   2 a buffer is too small,
!                   4 the table name is empty or too long.
!
!     The getters share the output connection's one prepared-statement slot
!     (stmtset in fvsqlite3.c) with the table writers. That is safe because each
!     getter finalizes its statement before it returns, and getters are only
!     called at stop points or after FVS returns, when no writer has a statement
!     open.
!
!     Every argument is passed by reference and is C-compatible, so the
!     routines are called directly: from Python with ctypes (fvstabledims_
!     etc.), and from R with .Fortran(), passing table names and buffers
!     as raw vectors (e.g. charToRaw(name)), because R strings can't hold
!     char(0). No .C() shims are needed.
!
!     The file is fixed form because it includes DBSCOM.F77.

!     Names of the C routines in fvsqlite3.c: lower case with a trailing
!     underscore for gfortran (CMPgcc), upper case for Intel (CMake build).
#ifdef CMPgcc
#define FSQL3(gccname,intelname) gccname
#else
#define FSQL3(gccname,intelname) intelname
#endif

      module dbstables_mod
      use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char,
     &  c_null_char
      use, intrinsic :: iso_fortran_env, only: real64
      implicit none
      private

      public :: RC_OK, RC_NOT_FOUND, RC_TOO_SMALL, RC_BAD_NAME, NAMELEN
      public :: mem_on, tbl_check, tbl_names, tbl_columns, tbl_nrow
      public :: tbl_select, tbl_step, tbl_finalize, tbl_double
      public :: tbl_text, tbl_put, tbl_drop

      integer, parameter :: RC_OK = 0
      integer, parameter :: RC_NOT_FOUND = 1
      integer, parameter :: RC_TOO_SMALL = 2
      integer, parameter :: RC_BAD_NAME = 4

!     Longest table or column name.
      integer, parameter :: NAMELEN = 64
!     Longest text value returned; longer values are truncated.
      integer(c_int), parameter :: TXTLEN = 2000

      interface
        function fsql3_prepare(dbnum, sql) result(rc)
     &    bind(c, name=FSQL3("fsql3_prepare_","FSQL3_PREPARE"))
          import :: c_int, c_char
          integer(c_int), intent(in) :: dbnum
          character(kind=c_char), intent(in) :: sql(*)
          integer(c_int) :: rc
        end function fsql3_prepare

        function fsql3_step(dbnum) result(isrow)
     &    bind(c, name=FSQL3("fsql3_step_","FSQL3_STEP"))
          import :: c_int
          integer(c_int), intent(in) :: dbnum
          integer(c_int) :: isrow
        end function fsql3_step

        function fsql3_finalize(dbnum) result(rc)
     &    bind(c, name=FSQL3("fsql3_finalize_","FSQL3_FINALIZE"))
          import :: c_int
          integer(c_int), intent(in) :: dbnum
          integer(c_int) :: rc
        end function fsql3_finalize

        function fsql3_exec(dbnum, sql) result(rc)
     &    bind(c, name=FSQL3("fsql3_exec_","FSQL3_EXEC"))
          import :: c_int, c_char
          integer(c_int), intent(in) :: dbnum
          character(kind=c_char), intent(in) :: sql(*)
          integer(c_int) :: rc
        end function fsql3_exec

        function fsql3_tableexists(dbnum, tname) result(exists)
     &    bind(c, name=FSQL3("fsql3_tableexists_","FSQL3_TABLEEXISTS"))
          import :: c_int, c_char
          integer(c_int), intent(in) :: dbnum
          character(kind=c_char), intent(in) :: tname(*)
          integer(c_int) :: exists
        end function fsql3_tableexists

        function fsql3_colint(dbnum, col, ifnull) result(ival)
     &    bind(c, name=FSQL3("fsql3_colint_","FSQL3_COLINT"))
          import :: c_int
          integer(c_int), intent(in) :: dbnum, col, ifnull
          integer(c_int) :: ival
        end function fsql3_colint

        function fsql3_coldouble(dbnum, col, ifnull) result(dval)
     &    bind(c, name=FSQL3("fsql3_coldouble_","FSQL3_COLDOUBLE"))
          import :: c_int, c_double
          integer(c_int), intent(in) :: dbnum, col
          real(c_double), intent(in) :: ifnull
          real(c_double) :: dval
        end function fsql3_coldouble

        function fsql3_coltext(dbnum, col, txt, mxlen, ifnull)
     &    result(n)
     &    bind(c, name=FSQL3("fsql3_coltext_","FSQL3_COLTEXT"))
          import :: c_int, c_char
          integer(c_int), intent(in) :: dbnum, col, mxlen
          character(kind=c_char), intent(inout) :: txt(*)
          character(kind=c_char), intent(in) :: ifnull(*)
          integer(c_int) :: n
        end function fsql3_coltext
      end interface

      contains

!     True when memory mode is on.
      logical function mem_on()
      include 'DBSCOM.F77'
      mem_on = IMEMDB == 1
      end function mem_on

!     Copies the table name to tname and checks that it exists in the
!     in-memory output database. Only letters, digits and underscores
!     are accepted, so the name is safe to quote in SQL.
      subroutine tbl_check(name, nch, tname, rtncode)
      include 'DBSCOM.F77'
      character(len=1), intent(in) :: name(*)
      integer, intent(in) :: nch
      character(len=NAMELEN), intent(out) :: tname
      integer, intent(out) :: rtncode
      integer :: i

      tname = ' '
      rtncode = RC_BAD_NAME
      if (nch < 1 .or. nch > NAMELEN) return
      rtncode = RC_NOT_FOUND
      do i = 1, nch
        if (verify(name(i), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' //
     &      'abcdefghijklmnopqrstuvwxyz0123456789_') /= 0) return
        tname(i:i) = name(i)
      end do
      if (.not. mem_on() .or. IoutDBref < 0) return
      if (fsql3_tableexists(IoutDBref, trim(tname) // c_null_char)
     &    == 0) return
      rtncode = RC_OK
      end subroutine tbl_check

!     Names of the tables in the in-memory output database, sorted.
      subroutine tbl_names(tnames)
      include 'DBSCOM.F77'
      character(len=NAMELEN), allocatable, intent(out) :: tnames(:)
      integer(c_int) :: irc

      allocate(tnames(0))
      if (.not. mem_on() .or. IoutDBref < 0) return
      irc = fsql3_prepare(IoutDBref, 'SELECT name FROM sqlite_master '
     &  // "WHERE type='table' ORDER BY name;" // c_null_char)
      do while (tbl_step())
        tnames = [character(len=NAMELEN) :: tnames, tbl_text(0)]
      end do
      call tbl_finalize()
      end subroutine tbl_names

!     Column names of table tname, in table order, whether each one's
!     declared type is text, and optionally the declared types, upper
!     case (e.g. "INT", "REAL", "TEXT"; empty if none was declared).
      subroutine tbl_columns(tname, colnm, istxt, coltype)
      include 'DBSCOM.F77'
      character(len=*), intent(in) :: tname
      character(len=NAMELEN), allocatable, intent(out) :: colnm(:)
      logical, allocatable, intent(out) :: istxt(:)
      character(len=NAMELEN), allocatable, intent(out), optional ::
     &  coltype(:)
      character(len=:), allocatable :: ctype
      integer(c_int) :: irc

      allocate(colnm(0), istxt(0))
      if (present(coltype)) allocate(coltype(0))
      irc = fsql3_prepare(IoutDBref, 'SELECT name, upper(type) FROM '
     &  // 'pragma_table_info("' // trim(tname) // '");' // c_null_char)
      do while (tbl_step())
        colnm = [character(len=NAMELEN) :: colnm, tbl_text(0)]
        ctype = tbl_text(1)
        istxt = [istxt, index(ctype, 'CHAR') > 0 .or.
     &    index(ctype, 'CLOB') > 0 .or. index(ctype, 'TEXT') > 0]
        if (present(coltype))
     &    coltype = [character(len=NAMELEN) :: coltype, ctype]
      end do
      call tbl_finalize()
      end subroutine tbl_columns

!     Number of rows in table tname.
      integer function tbl_nrow(tname)
      include 'DBSCOM.F77'
      character(len=*), intent(in) :: tname
      integer(c_int) :: irc

      tbl_nrow = 0
      irc = fsql3_prepare(IoutDBref, 'SELECT count(*) FROM "'
     &  // trim(tname) // '";' // c_null_char)
      if (tbl_step()) tbl_nrow = fsql3_colint(IoutDBref, 0_c_int,
     &  0_c_int)
      call tbl_finalize()
      end function tbl_nrow

!     Prepares a statement that selects rows row0 to row0+nrows-1 of
!     table tname. Read them with tbl_step, then call tbl_finalize.
      subroutine tbl_select(tname, row0, nrows)
      include 'DBSCOM.F77'
      character(len=*), intent(in) :: tname
      integer, intent(in) :: row0, nrows
      character(len=200) :: sql
      integer(c_int) :: irc

      write (sql, '(3a,i0,a,i0,a)') 'SELECT * FROM "', trim(tname),
     &  '" ORDER BY rowid LIMIT ', nrows, ' OFFSET ', max(0, row0 - 1),
     &  ';'
      irc = fsql3_prepare(IoutDBref, trim(sql) // c_null_char)
      end subroutine tbl_select

!     Steps the prepared statement; true while there is a row.
      logical function tbl_step()
      include 'DBSCOM.F77'
      tbl_step = fsql3_step(IoutDBref) == 1
      end function tbl_step

!     Finalizes the prepared statement.
      subroutine tbl_finalize()
      include 'DBSCOM.F77'
      integer(c_int) :: irc
      irc = fsql3_finalize(IoutDBref)
      end subroutine tbl_finalize

!     Numeric value of column icol (0-based) of the current row; ifnull
!     if it is NULL or not a number.
      real(real64) function tbl_double(icol, ifnull)
      include 'DBSCOM.F77'
      integer, intent(in) :: icol
      real(real64), intent(in) :: ifnull
      tbl_double = fsql3_coldouble(IoutDBref, int(icol, c_int),
     &  real(ifnull, c_double))
      end function tbl_double

!     Text of column icol (0-based) of the current row; NULL and empty
!     values give an empty string.
      function tbl_text(icol) result(str)
      include 'DBSCOM.F77'
      integer, intent(in) :: icol
      character(len=:), allocatable :: str
!     fsql3_coltext copies at most TXTLEN characters without a closing
!     char(0), so buf has one more, kept as char(0).
      character(kind=c_char, len=TXTLEN + 1) :: buf
      integer(c_int) :: n

      buf = repeat(c_null_char, len(buf))
      n = fsql3_coltext(IoutDBref, int(icol, c_int), buf, TXTLEN,
     &  c_null_char)
      str = buf(1:n)
      end function tbl_text

!     Appends str and char(0) to buf if they fit in mxch characters.
!     nch counts the characters needed either way; rtncode is set to
!     RC_TOO_SMALL when they don't fit.
      subroutine tbl_put(buf, mxch, nch, str, rtncode)
      character(len=1), intent(inout) :: buf(*)
      integer, intent(in) :: mxch
      integer, intent(inout) :: nch, rtncode
      character(len=*), intent(in) :: str
      integer :: i

      if (nch + len(str) + 1 <= mxch) then
        do i = 1, len(str)
          buf(nch + i) = str(i:i)
        end do
        buf(nch + len(str) + 1) = c_null_char
      else
        rtncode = RC_TOO_SMALL
      end if
      nch = nch + len(str) + 1
      end subroutine tbl_put

!     Drops table tname.
      subroutine tbl_drop(tname)
      include 'DBSCOM.F77'
      character(len=*), intent(in) :: tname
      integer(c_int) :: irc
      irc = fsql3_exec(IoutDBref, 'DROP TABLE "' // trim(tname) // '";'
     &  // c_null_char)
      end subroutine tbl_drop

      end module dbstables_mod

!     ---- API routines ----

!     Turns memory mode on (on = 1) or off (on = 0). Call before the first FVS 
!     call. Changing the setting closes the current output database, so 
!     switching memory mode off discards the tables. rtnCode is always 0.
      subroutine fvsSetMemoryTables(on, rtnCode)
      implicit none
      include 'DBSCOM.F77'

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSSETMEMORYTABLES'::FVSSETMEMORYTABLES
!DEC$ ATTRIBUTES REFERENCE :: ON, RTNCODE

      integer, intent(in) :: on
      integer, intent(out) :: rtnCode
      integer :: inew

      inew = merge(1, 0, on /= 0)
      if (inew /= IMEMDB) then
        IMEMDB = 0
        call DBSCLOSE(.TRUE., .FALSE.)
        IMEMDB = inew
        CASEID = ""
      end if
      rtnCode = 0
      end subroutine fvsSetMemoryTables

!     Names of the tables in the in-memory output database.
!     names   = buffer for the names, each followed by char(0)
!     nch     = in: length of names; out: characters used or needed
!     ntables = number of tables
!     rtnCode = 0: OK (also when no table exists yet)
!               1: memory mode off
!               2: names too small
      subroutine fvsTableList(names, nch, ntables, rtnCode)
      use dbstables_mod
      implicit none

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSTABLELIST'::FVSTABLELIST
!DEC$ ATTRIBUTES REFERENCE :: NAMES, NCH, NTABLES, RTNCODE

      character(len=1), intent(out) :: names(*)
      integer, intent(inout) :: nch
      integer, intent(out) :: ntables, rtnCode
      character(len=NAMELEN), allocatable :: tnames(:)
      integer :: i, mxch

      mxch = nch
      nch = 0
      ntables = 0
      rtnCode = RC_NOT_FOUND
      if (.not. mem_on()) return
      rtnCode = RC_OK
      call tbl_names(tnames)
      ntables = size(tnames)
      do i = 1, ntables
        call tbl_put(names, mxch, nch, trim(tnames(i)), rtnCode)
      end do
      end subroutine fvsTableList

!     Size of table name (nch characters): nrow rows, nnum numeric columns and 
!     ntxt text columns.
      subroutine fvsTableDims(name, nch, nrow, nnum, ntxt, rtnCode)
      use dbstables_mod
      implicit none

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSTABLEDIMS'::FVSTABLEDIMS
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, NROW, NNUM, NTXT, RTNCODE

      character(len=1), intent(in) :: name(*)
      integer, intent(in) :: nch
      integer, intent(out) :: nrow, nnum, ntxt, rtnCode
      character(len=NAMELEN) :: tname
      character(len=NAMELEN), allocatable :: colnm(:)
      logical, allocatable :: istxt(:)

      nrow = 0
      nnum = 0
      ntxt = 0
      call tbl_check(name, nch, tname, rtnCode)
      if (rtnCode /= RC_OK) return
      nrow = tbl_nrow(tname)
      call tbl_columns(tname, colnm, istxt)
      ntxt = count(istxt)
      nnum = size(istxt) - ntxt
      end subroutine fvsTableDims

!     Column names and declared types of table name (nch characters). Each type 
!     list matches its name list entry for entry; types are upper case as 
!     declared (e.g. "INT", "REAL", "TEXT").
!     numcols  = buffer for the numeric column names
!     nnumch   = in: length of numcols; out: characters used or needed
!     txtcols  = buffer for the text column names
!     ntxtch   = in: length of txtcols; out: characters used or needed
!     numtypes = buffer for the numeric columns' declared types
!     nnumtch  = in: length of numtypes; out: characters used or needed
!     txttypes = buffer for the text columns' declared types
!     ntxttch  = in: length of txttypes; out: characters used or needed
      subroutine fvsTableColumns(name, nch, numcols, nnumch, txtcols,
     &                           ntxtch, numtypes, nnumtch, txttypes,
     &                           ntxttch, rtnCode)
      use dbstables_mod
      implicit none

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSTABLECOLUMNS'::FVSTABLECOLUMNS
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, NUMCOLS, NNUMCH, TXTCOLS
!DEC$ ATTRIBUTES REFERENCE :: NTXTCH, NUMTYPES, NNUMTCH, TXTTYPES
!DEC$ ATTRIBUTES REFERENCE :: NTXTTCH, RTNCODE

      character(len=1), intent(in) :: name(*)
      integer, intent(in) :: nch
      character(len=1), intent(out) :: numcols(*), txtcols(*)
      character(len=1), intent(out) :: numtypes(*), txttypes(*)
      integer, intent(inout) :: nnumch, ntxtch, nnumtch, ntxttch
      integer, intent(out) :: rtnCode
      character(len=NAMELEN) :: tname
      character(len=NAMELEN), allocatable :: colnm(:), coltype(:)
      logical, allocatable :: istxt(:)
      integer :: i, mxnum, mxtxt, mxnumt, mxtxtt

      mxnum = nnumch
      mxtxt = ntxtch
      mxnumt = nnumtch
      mxtxtt = ntxttch
      nnumch = 0
      ntxtch = 0
      nnumtch = 0
      ntxttch = 0
      call tbl_check(name, nch, tname, rtnCode)
      if (rtnCode /= RC_OK) return
      call tbl_columns(tname, colnm, istxt, coltype)
      do i = 1, size(colnm)
        if (istxt(i)) then
          call tbl_put(txtcols, mxtxt, ntxtch, trim(colnm(i)), rtnCode)
          call tbl_put(txttypes, mxtxtt, ntxttch, trim(coltype(i)),
     &      rtnCode)
        else
          call tbl_put(numcols, mxnum, nnumch, trim(colnm(i)), rtnCode)
          call tbl_put(numtypes, mxnumt, nnumtch, trim(coltype(i)),
     &      rtnCode)
        end if
      end do
      end subroutine fvsTableColumns

!     Numeric columns of rows row0 .. row0+nrows-1 of table name.
!     nrows  = in: maximum rows to return; out: rows returned
!     nnum   = number of numeric columns; must equal the table's, otherwise 
!              rtnCode = 2 and nnum is set to the table's
!     values = values(nnum, nrows); NULL is returned as NaN
      subroutine fvsTableNum(name, nch, row0, nrows, nnum, values,
     &                       rtnCode)
      use, intrinsic :: ieee_arithmetic, only: ieee_value,
     &  ieee_quiet_nan
      use, intrinsic :: iso_fortran_env, only: real64
      use dbstables_mod
      implicit none

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSTABLENUM'::FVSTABLENUM
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ROW0, NROWS, NNUM, VALUES
!DEC$ ATTRIBUTES REFERENCE :: RTNCODE

      character(len=1), intent(in) :: name(*)
      integer, intent(in) :: nch, row0
      integer, intent(inout) :: nrows, nnum
      real(real64), intent(out) :: values(nnum, *)
      integer, intent(out) :: rtnCode
      character(len=NAMELEN) :: tname
      character(len=NAMELEN), allocatable :: colnm(:)
      logical, allocatable :: istxt(:)
      real(real64) :: nan
      integer :: mxrow, icol, j

      mxrow = nrows
      nrows = 0
      call tbl_check(name, nch, tname, rtnCode)
      if (rtnCode /= RC_OK) return
      call tbl_columns(tname, colnm, istxt)
      if (nnum /= count(.not. istxt)) then
        nnum = count(.not. istxt)
        rtnCode = RC_TOO_SMALL
        return
      end if
      if (mxrow <= 0) return
      nan = ieee_value(1.0_real64, ieee_quiet_nan)
      call tbl_select(tname, row0, mxrow)
      do while (tbl_step())
        nrows = nrows + 1
        j = 0
        do icol = 1, size(istxt)
          if (istxt(icol)) cycle
          j = j + 1
          values(j, nrows) = tbl_double(icol - 1, nan)
        end do
      end do
      call tbl_finalize()
      end subroutine fvsTableNum

!     Text columns of rows row0 .. row0+nrows-1 of table name.
!     nrows   = in: maximum rows to return; out: rows returned
!     ntxt    = number of text columns; must equal the table's, otherwise 
!               rtnCode = 2 and ntxt is set to the table's
!     text    = buffer for the values, row by row and in column order within a 
!               row, each followed by char(0)
!     ntextch = in: length of text; out: characters used or needed
      subroutine fvsTableTxt(name, nch, row0, nrows, ntxt, text,
     &                       ntextch, rtnCode)
      use dbstables_mod
      implicit none

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSTABLETXT'::FVSTABLETXT
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ROW0, NROWS, NTXT, TEXT
!DEC$ ATTRIBUTES REFERENCE :: NTEXTCH, RTNCODE

      character(len=1), intent(in) :: name(*)
      integer, intent(in) :: nch, row0
      integer, intent(inout) :: nrows, ntxt, ntextch
      character(len=1), intent(out) :: text(*)
      integer, intent(out) :: rtnCode
      character(len=NAMELEN) :: tname
      character(len=NAMELEN), allocatable :: colnm(:)
      logical, allocatable :: istxt(:)
      integer :: mxrow, mxch, icol

      mxrow = nrows
      mxch = ntextch
      nrows = 0
      ntextch = 0
      call tbl_check(name, nch, tname, rtnCode)
      if (rtnCode /= RC_OK) return
      call tbl_columns(tname, colnm, istxt)
      if (ntxt /= count(istxt)) then
        ntxt = count(istxt)
        rtnCode = RC_TOO_SMALL
        return
      end if
      if (mxrow <= 0) return
      call tbl_select(tname, row0, mxrow)
      do while (tbl_step())
        nrows = nrows + 1
        do icol = 1, size(istxt)
          if (.not. istxt(icol)) cycle
          call tbl_put(text, mxch, ntextch, tbl_text(icol - 1), rtnCode)
        end do
      end do
      call tbl_finalize()
      end subroutine fvsTableTxt

!     Drops table name and its rows. Next writer to the table recreates it.
      subroutine fvsClearTable(name, nch, rtnCode)
      use dbstables_mod
      implicit none

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSCLEARTABLE'::FVSCLEARTABLE
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, RTNCODE

      character(len=1), intent(in) :: name(*)
      integer, intent(in) :: nch
      integer, intent(out) :: rtnCode
      character(len=NAMELEN) :: tname

      call tbl_check(name, nch, tname, rtnCode)
      if (rtnCode == RC_OK) call tbl_drop(tname)
      end subroutine fvsClearTable
