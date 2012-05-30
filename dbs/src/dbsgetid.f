      SUBROUTINE DBSGETID(TABLENAME,IDNAME,ID)
      IMPLICIT NONE
C
C $Id$
C
C
C     PURPOSE: TO SIMPLY RETURN THE MAX ID NUMBER IN THE SPECIFIED
C              TABLE
C
C     INPUT: TABLENAME - THE SPECIFIED TABLE TO BE QUERIED
C            IDNAME    - THE ID COLUMN NAME OF THE TABLE
C
C     RETURN: ID - THE MAX CASE NUMBER THAT WAS CREATED OR IN THE
C                     TABLE
C
C     AUTH: D. GAMMEL -- SEM -- JULY 2002
C---
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS

      INTEGER(SQLPOINTER_KIND):: ID
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      INTEGER(SQLINTEGER_KIND)::ID_LI
      CHARACTER*700 SQLStmtStr
      CHARACTER(LEN=*) TABLENAME,IDNAME

      WRITE(SQLStmtStr,*)'select max(',TRIM(IDNAME),') from ',
     -  TRIM(TABLENAME)

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'GetID:Retrieving CASEID: '//trim(SQLStmtStr))

      IF (iRet.EQ.SQL_SUCCESS .OR. iRet.EQ.SQL_SUCCESS_WITH_INFO) THEN
        ColNumber = 1
        iRet = fvsSQLBindCol (StmtHndlOut,ColNumber,SQL_F_INTEGER,
     -        ID,int(4,SQLLEN_KIND),ID_LI)
        iRet = fvsSQLFetch(StmtHndlOut)
      ELSE
        PRINT *,'Error executing SQL query'
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                'GetID:SQLBindCol ')
      ENDIF
      END
