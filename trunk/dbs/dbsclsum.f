      SUBROUTINE DBSCLSUM(NPLT,IYR,SP,SPVIAB,
     >          SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >          SPSITGM,MXDENMLT,POTESTAB)
      IMPLICIT NONE
C
C DBS $Id$
C
C     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

      INCLUDE 'DBSCOM.F77'
C
      INTEGER IYR,IRCODE
      CHARACTER(LEN=*) NPLT,SP
      REAL SPVIAB,SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >     SPSITGM,MXDENMLT,POTESTAB

      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME

      IF (ICLIM.EQ.0) RETURN

C     MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSCLSUM:DSN Connection')
        ICLIM = 0
        GOTO 200
      ENDIF

C     CHECK TO SEE IF THE Climate TABLE EXISTS IN DATBASE

      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Climate$]'
      ELSE
        TABLENAME = 'FVS_Climate'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) RETURN
      IF(IRCODE.EQ.1) THEN
         IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Climate('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Species Text null,'//
     -              'Viability double null,'//
     -              'BA double null,'//
     -              'TPA double null,'//
     -              'ViabMort double null,'//
     -              'dClimMort double null,'//
     -              'GrowthMult double null,'//
     -              'SiteMult double null,'//
     -              'MxDenMult double null,'//
     -              'AutoEstbTPA double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Climate('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'Species Text,'//
     -              'Viability Number,'//
     -              'BA Number,'//
     -              'TPA Number,'//
     -              'ViabMort Number,'//
     -              'dClimMort Number,'//
     -              'GrowthMult Number,'//
     -              'SiteMult Number,'//
     -              'MxDenMult Number,'//
     -              'AutoEstbTPA Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_Climate('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Species char(3) null,'//
     -              'Viability real null,'//
     -              'BA real null,'//
     -              'TPA real null,'//
     -              'ViabMort real null,'//
     -              'dClimMort real null,'//
     -              'GrowthMult real null,'//
     -              'SiteMult real null,'//
     -              'MxDenMult real null,'//
     -              'AutoEstbTPA real null)'
        ENDIF
        PRINT*, trim(SQLStmtStr)

        !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -         int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -      'DBSCLSUM:Creating Table: '//trim(SQLStmtStr))
        IF (iRet.NE.SQL_SUCCESS) GOTO 200
      ENDIF

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,'//
     -   'StandID,Year,Species,Viability,BA,TPA,ViabMort,'//
     -   'dClimMort,GrowthMult,SiteMult,MxDenMult,AutoEstbTPA) '//
     -   'VALUES(''',CASEID,''',''',trim(NPLT),''',',IYR,',''',
     -   trim(SP),''',',SPVIAB,',',SPBA,',',SPTPA,',',
     -   SPMORT1,',',SPMORT2,',',SPGMULT,',',
     -   SPSITGM,',',MXDENMLT,',',POTESTAB,')'

      ! PRINT*, trim(SQLStmtStr)
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -       int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      ! PRINT*, iRet
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -             'DBSCLSUM:Inserting Row')


  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


