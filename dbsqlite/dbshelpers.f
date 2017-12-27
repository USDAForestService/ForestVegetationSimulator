      function success(returnCode)

C $Id: dbshelpers.f 295 2012-05-31 18:52:14Z ncrookston.fs@gmail.com $

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
