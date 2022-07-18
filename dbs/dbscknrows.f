      SUBROUTINE DBSCKNROWS(IRCODE,TABLENAME,NRESERV,ISEXCEL)
C
C DBS $Id$
C
C     PURPOSE: SEE IF THE NUMBER OF ROWS ON AN OUTPUT DATABASE
C              IS TOO LARGE FOR EXCEL...OR EVEN IF THE DATA BASE 
C              TABLE EXISTS.
C     IRCODE: return code where:
C            0 = table exists, ok to add rows
C            1 = table does not exist
C            2 = ISEXCEL is true and there are too many rows
C     TABLENAME is the table name, with decorations if needed
C     NRESERV is an estimate of how many more rows will be inserted,
C           ignored if ISEXCEL is false
C     ISEXCEL true if database type is EXCEL
C
      IMPLICIT NONE
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
      INTEGER*4 IRCNT
      INTEGER(SQLLEN_KIND)::IRCNT_LI
      LOGICAL ISEXCEL
      CHARACTER*20 TABLENAME
      CHARACTER*100 SQLStmtStr
      INTEGER  IRCODE,NRESERV
      IRCODE = 0

C     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
C     IF IT DOESNT THEN WE NEED TO CREATE IT

      IF (ISEXCEL) THEN
        SQLStmtStr= 'SELECT Count(*) FROM '//TABLENAME

      ELSEIF (TRIM(DBMSOUT) .EQ. 'ACCESS') THEN
        SQLStmtStr= 'SELECT Count(*) FROM '//
     >  '(SELECT TOP 1 * FROM '//TRIM(TABLENAME)//')'

      ELSE
        SQLStmtStr= 'SELECT Count(*) FROM '//
     >  '(SELECT * FROM '//TRIM(TABLENAME)//' LIMIT 1 )'
      ENDIF

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     >            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

c*      PRINT *,SQLStmtStr,"iRet=",iRet

      IF(iRet.NE.SQL_SUCCESS.AND.
     >   iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
        IRCODE = 1
      else
        if (ISEXCEL) then
          iRet = fvsSQLBindCol (StmtHndlOut,1,SQL_F_INTEGER,
     >           IRCNT,int(4,SQLLEN_KIND),IRCNT_LI)
          iRet = fvsSQLFetch(StmtHndlOut) 
          if (IRCNT .GE. 1048576-NRESERV) then
            IRCODE = 2
            iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
            return
          endif
        endif
      endif
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      return
      end
 
