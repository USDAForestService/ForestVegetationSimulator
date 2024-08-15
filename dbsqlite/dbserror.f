      SUBROUTINE DBSERROR(NPLT,CMSG)
      IMPLICIT NONE
C----------
C DBSQLITE
C----------
C
C     PURPOSE: CREATE AND POPULATE A DATABASE TABLE WITH WARNING AND
C              ERROR MESSAGES.
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER iRet
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=*) NPLT,CMSG
C
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_step,
     >        fsql3_prepare,fsql3_finalize

      CALL DBSCASE(1)

C     DEFINE TABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Error'//CHAR(0))

      IF(iRet.EQ.0) THEN
C
C       TABLE CREATION
C
        SQLStmtStr='CREATE TABLE FVS_Error('//
     -             'CaseID text not null,'//
     -             'StandID text not null,'//
     -             'Message text);'//CHAR(0)

        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
      ENDIF
C
C     INSERT RECORD
C
       WRITE(SQLStmtStr,*)'INSERT INTO FVS_Error',
     -      ' (CaseID,StandID,Message)',
     -      'VALUES(''',CASEID,''',''',TRIM(NPLT),''',',
     -      '''',CMSG,''');'
      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      RETURN
      END
