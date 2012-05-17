      SUBROUTINE DBSOPEN(CONNECT,EnvHndl,ConnHndl,DBMS,KODE)
      IMPLICIT NONE
C
C  **DBSOPEN--DBS  DATE OF LAST REVISION:  12/02/2011
C
C     PURPOSE: TO OPEN A DATABASE CONNECTION
C     INPUT: CONNECT  - CONNECTION STRING
C            EnvHndl  - ENVIRONMENT HANDLE
C            ConnHndl - CONNECTION HANDLE
C            DBMS     - DATABASE MANAGEMENT SYSTEM NAME
C            KODE     - RETURN CODE 0: CONNECTION UN-SUCCESSFUL
C                                   1: CONNECTION SUCCESSFUL
C---
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER(SQLHENV_KIND):: EnvHndl
      INTEGER(SQLHDBC_KIND):: ConnHndl
      INTEGER(SQLSMALLINT_KIND)::ConnStrLength,ConnStrLengthOut,LenDBMS
      INTEGER(SQLSMALLINT_KIND), PARAMETER::LenConnStr=500
      INTEGER(SQLSMALLINT_KIND)::DriverComplete
      INTEGER I,KODE
      CHARACTER(LEN=*) CONNECT,DBMS
      CHARACTER(LEN=LEN_TRIM(CONNECT)) SOURCE
      CHARACTER(LEN=LenConnStr) ConnStr,ConnStrOut
      CHARACTER*256 DSN
      CHARACTER*40 USER,PSSWRD,SUFFIX
C
C     CLOSE ANY OPEN CONNECTION
C
      IF(ConnHndl.NE.-1 .AND. ConnHndl.EQ.ConnHndlOut)
     >            CALL DBSCLOSE(.TRUE.,.FALSE.)
      IF(ConnHndl.NE.-1 .AND. ConnHndl.EQ.ConnHndlIn)
     >            CALL DBSCLOSE(.FALSE.,.TRUE.)
      KODE = 0
C
C     ALLOCATE AND ENVIRONMENT HANDLE
C
      iRet = fvsSQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl)
C
C     SET ODBC VERSION TO USE 3.X
C
      iRet = fvsSQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION,
     -  SQL_OV_ODBC3)
C
C     ALLOCATE A CONNECTION HANDLE
C
      iRet = fvsSQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl)
C
C     ENABLE AUTOCOMMIT ATTRIBUTE
C
      iRet = fvsSQLSetConnectAttr(ConnHndl,SQL_AUTOCOMMIT,
     -  SQL_AUTOCOMMIT_ON,SQLINTEGER_KIND)
      iRet = fvsSQLSetConnectAttr (ConnHndl, SQL_ACCESS_MODE,
     -  SQL_MODE_READ_WRITE,SQLINTEGER_KIND)
C
C     PARSE OUT NEEDED DATA FROM DSNOUT CHECKING FOR QUOTED STRINGS
C
      SOURCE = TRIM(CONNECT)
      PSSWRD=' '
      USER=' '
      IF(SOURCE(1:1).EQ.CHAR(34)) THEN
        SOURCE = TRIM(SOURCE(2:))
        CALL DBSPRS(DSN,SOURCE,CHAR(34))
      ELSE
        CALL DBSPRS(DSN,SOURCE,' ')
      END IF
      IF(SOURCE(1:1).EQ.CHAR(34)) THEN
        SOURCE = TRIM(SOURCE(2:))
        CALL DBSPRS(USER,SOURCE,CHAR(34))
      ELSE
        CALL DBSPRS(USER,SOURCE,' ')
      END IF
      IF(SOURCE(1:1).EQ.CHAR(34)) THEN
        SOURCE = TRIM(SOURCE(2:))
        CALL DBSPRS(PSSWRD,SOURCE,CHAR(34))
      ELSE
        CALL DBSPRS(PSSWRD,SOURCE,' ')
      END IF
C
C     IF THERE IS A FILE NAME SUFFIX, GET IT (LOAD INTO SUFFIX)
C
      SUFFIX=' '
      DO I=LEN_TRIM(DSN),1,-1
        IF (DSN(I:I).EQ.'.') THEN
          SUFFIX=DSN(I+1:)
          EXIT
        ENDIF
      ENDDO
      ConnStr = ' '
      IF (SUFFIX.EQ.' ') THEN
        ConnStr='DSN='//TRIM(DSN)//';UID='//TRIM(USER)//
     -          ';PWD='//TRIM(PSSWRD)
        DriverComplete = SQL_DRIVER_COMPLETE
      ELSE
        DO I = 1,LEN_TRIM(SUFFIX)
          CALL UPCASE(SUFFIX(I:I))
        ENDDO
        IF (DSN(2:2).NE.':') THEN
          CALL GETCWD(ConnStr)
          DSN = TRIM(ConnStr)//"/"//TRIM(DSN)
         ENDIF
        IF(SUFFIX.EQ.'DB') THEN
          ConnStr='DRIVER=SQLite3 ODBC Driver'//
     -            ';Database='//TRIM(DSN)//
     -            ';Version=3;LongNames=0;Timeout=1000;NoTXN=0'//
     -            ';SyncPragma=NORMAL;StepAPI=0;NoWCHAR=1'
        ELSE
          IF(SUFFIX.EQ.'XLS') THEN
            ConnStr='DRIVER={Microsoft Excel Driver (*.xls)}'//
     -                    ';ReadOnly=False'        
          ELSEIF (SUFFIX.EQ.'XLSX') THEN
            ConnStr='DRIVER={Microsoft Excel Driver (*.xls, '//
     -                      '*.xlsx, *.xlsm, *.xlsb)}'//
     -                      ';ReadOnly=False'
          ELSEIF(SUFFIX.EQ.'MDB'.OR.SUFFIX.EQ.'ACCDB')THEN
            ConnStr='DRIVER={Microsoft Access Driver (*.mdb, '//
     -              '*.accdb)}'
          ENDIF
          IF (ConnStr.NE.' ') THEN
            ConnStr=trim(ConnStr)//';DBQ='//TRIM(DSN)//
     -              ';UID='//TRIM(USER)//
     -              ';PWD='//TRIM(PSSWRD)
          ENDIF
        ENDIF
        DriverComplete = SQL_DRIVER_NOPROMPT
      ENDIF
      ConnStrLength = LEN_TRIM(ConnStr)
      IF (ConnStrLength.GT.0) THEN
        iRet = fvsSQLDriverConnect(ConnHndl, SQL_NULL_PTR,
     -         ConnStr,ConnStrLength,ConnStrOut,LenConnStr,
     -         ConnStrLengthOut,DriverComplete)
C
C       MAKE SURE WE CONNECTED OK 
C
        IF(iRet.NE.SQL_SUCCESS.AND.iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
          print *,"Data base connection error using connection: "
          print *,trim(ConnStr) 
          CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndl,
     -                   'DBOPEN: Connect string: '//trim(ConnStr) )
          RETURN
        ENDIF
          
      ELSE
        print *,"Data base connection error: connection string ",
     -          "could not be formed."
        RETURN
      ENDIF
C
C     CONNECTION SUCCESSFULLY OPENED
C
      KODE = 1
C
C     DETERMINE THE DBMS NAME
C
      iRet = fvsSQLGetInfo(ConnHndl,SQL_DBMS_NAME,DBMS,
     -                   int(LEN(DBMS),SQLSMALLINT_KIND),
     -                   LenDBMS)
      DBMS(LenDBMS+1:)=' '
C
C     SET CONNECTION ATTRIBUTES TO ALLOW READ/WRITE
C      (USUALLLY THIS IS SET BEFORE THE CONNECTION IS ESTABLISHED
C      BUT THERE SEEMS TO BE A BUG IN THE EXCEL ODBC DRIVER THAT
C      DOES NOT RECOGNIZE THIS SETTING IF DONE BEFORE CONNECTION)
C
      IF(DBMS.EQ.'EXCEL') THEN
        iRet = fvsSQLSetConnectAttr (ConnHndl, SQL_ACCESS_MODE,
     -     SQL_MODE_READ_WRITE,int(4,SQLINTEGER_KIND))
      ENDIF

      END

