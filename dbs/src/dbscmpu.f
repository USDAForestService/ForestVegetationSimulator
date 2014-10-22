      SUBROUTINE DBSCMPU
      IMPLICIT NONE
C
C $Id$
C
C     AUTH: D. GAMMEL -- SEM -- JUNE 2002
C     PURPOSE: TO POPULATE A DATABASE WITH THE COMPUTE TABLE
C              INFORMATION
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'KEYCOM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C

      CHARACTER*4000 SQLStmtStr, TABLESTR
      CHARACTER*20 TABLENAME,DTYPE
      CHARACTER*20 COLNAME
      CHARACTER*8 KEYWRD
      CHARACTER(LEN=1) LWRAP,RWRAP
      CHARACTER(LEN = 8),DIMENSION (MXTST5)::KWLIST,KWINSRT
      REAL,DIMENSION(MXTST5)::KWVALS
      INTEGER NUMKW,I,I1,I2,I3,I4,INSRTNUM,II,X,ICY,THISYR
      LOGICAL KWINLIST,LDUPKW
      INTEGER(SQLSMALLINT_KIND)::ColNumber,NameLen,ColumnCount,DTp,
     -       NDecs, Nullable
      INTEGER(SQLUINTEGER_KIND) NColSz
      NUMKW = ITST5
C
C     IF COMPUTE IS NOT TURNED ON OR THE NUMBER OF VARIABLES IS 0
C     THEN RETURN
C
      IF(ICOMPUTE.EQ.0.OR.NUMKW.EQ.0) RETURN
C
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C
      CALL DBSCASE(1)

      IF(TRIM(DBMSOUT).EQ.'EXCEL') THEN
        TABLENAME = '[FVS_Compute$]'
        DTYPE = 'Number'
        LWRAP = '['
        RWRAP = ']'
      ELSEIF(TRIM(DBMSOUT).EQ.'ACCESS') THEN
        TABLENAME = 'FVS_Compute'
        DTYPE = 'Double'
        LWRAP = '['
        RWRAP = ']'
      ELSEIF(TRIM(DBMSOUT).EQ.'ORACLE') THEN
        TABLENAME = 'FVS_Compute'
        DTYPE = 'real'
        LWRAP = '"'
        RWRAP = '"'
      ELSE
        TABLENAME = 'FVS_Compute'
        DTYPE = 'real'
        LWRAP = ' '
        RWRAP = ' '
      ENDIF
C
C     ALLOCATE A STATEMENT HANDLE
C
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ICOMPUTE = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                 'DBSCMPU:Connecting to DSN')
        GOTO 10
      ENDIF
C
C     CHECK TO SEE IF THE COMPUTE TABLE EXISTS IN DATBASE
C     IF IT DOES NOT THEN WE NEED TO CREATE IT
C
      SQLStmtStr= 'SELECT Count(*) FROM '//TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        KWLIST = CTSTV5
        IF(TRIM(DBMSOUT).EQ."EXCEL".OR.TRIM(DBMSOUT).EQ."ACCESS") THEN
          TABLESTR='CREATE TABLE FVS_Compute('//
     -             'CaseID Text,'//
     -             'StandID Text null,'//
     -             'Year INT null'

          DO I=1,NUMKW
            !CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
            IF(.NOT.(KWLIST(I)(1:1).EQ.'_'.AND.I_CMPU.LT.1)) THEN
              SQLStmtStr=TRIM(TABLESTR)//',['//
     -               TRIM(KWLIST(I))//'] '//TRIM(DTYPE)//' null'
              TABLESTR = SQLStmtStr
            ENDIF
          ENDDO
        ELSE
          TABLESTR='CREATE TABLE FVS_Compute('//
     -             'CaseID char(36) ,'//
     -             'StandID char(26) null,'//
     -             'Year int null'
          DO I=1,NUMKW
            !CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
            IF(.NOT.(KWLIST(I)(1:1).EQ.'_'.AND.I_CMPU.LT.1)) THEN
             SQLStmtStr=TRIM(TABLESTR)//','//LWRAP//
     -               TRIM(KWLIST(I))//RWRAP//' '//TRIM(DTYPE)//' null'
             TABLESTR = SQLStmtStr
            ENDIF
          ENDDO
        ENDIF

        SQLStmtStr = TRIM(TABLESTR)//')'
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -       'DBSCMPU:Creating Table: '//trim(SQLStmtStr))
      ELSE
C
C       POPULATE THE KWLIST WITH THE CURRENT COLUMN NAMES
C
        iRet = fvsSQLNumResultCols(StmtHndlOut,ColumnCount)

        DO ColNumber = 1,ColumnCount
          iRet = fvsSQLDescribeCol (StmtHndlOut, ColNumber, ColName,
     -         int(LEN(ColName),SQLUSMALLINT_KIND), NameLen, DTp,
     -         NColSz, NDecs, Nullable)
          COLNAME(NameLen+1:)=' '
          IF (ColNumber.GT.4) KWLIST(ColNumber-4) = COLNAME
        ENDDO
        NUMKW = ColumnCount - 4
      ENDIF
      iRet = fvsSQLCloseCursor(StmtHndlOut)
C
C     CREATE AND EXECUTE THE INSERT COMMANDS FOR EACH YEAR
C
      THISYR = -1
      INSRTNUM = 0
      DO ICY=1,NCYC
        I1=IMGPTS(ICY,1)
        I2=IMGPTS(ICY,2)
        DO II=I1,I2
          I=IOPSRT(II)
          !MAKE SURE WE ARE GRABBING ONLY THE COMPUTES

          IF(IACT(I,1).EQ.33 .AND. IACT(I,4).GT.0) THEN
            IF(THISYR.EQ.-1) THISYR = IACT(I,4)
            IF(IACT(I,4).NE.THISYR) THEN
              !CHECK TO SEE IF WE HAVE SOMETHING TO INSERT
              IF(INSRTNUM.GT.0) THEN

                !BUILD THE INSERT QUERY
                CALL INSERTCMPU(KWINSRT,KWVALS,THISYR,NPLT,INSRTNUM)

              ENDIF
              INSRTNUM = 0
              THISYR = IACT(I,4)
            ENDIF
            X=IFIX(PARMS(IACT(I,2)+1))
            IF (X.GT.500) X=X-500
            KEYWRD=CTSTV5(X)
            !CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
            IF(KEYWRD(1:1).EQ.'_'.AND.I_CMPU.LT.1) THEN
              KWINLIST = .TRUE.
            ELSE
              KWINLIST=.FALSE.
              !CHECK TO SEE IF KEYWORD IS IN LIST ALREADY
              DO I3=1,NUMKW
                !IF IT IS IN THE LIST THEN ADD THE VALUE TO THE ARRAYS
                IF(TRIM(KWLIST(I3)).EQ.TRIM(KEYWRD)) THEN
                  !NEED TO CHECK FOR DUPLICATES
                  LDUPKW = .FALSE.
                  DO I4=1,INSRTNUM
                    IF(TRIM(KWINSRT(I4)).EQ.TRIM(KEYWRD)) THEN
                      KWVALS(I4)=PARMS(IACT(I,2))
                      LDUPKW =.TRUE.
                    ENDIF
                  ENDDO
                  !IF NOT A DUPLICATE THEN ADD NEW ENTRY TO ARRAYS
                  IF(.NOT.LDUPKW) THEN
                    INSRTNUM = INSRTNUM + 1
                    KWINSRT(INSRTNUM)=KEYWRD
                    KWVALS(INSRTNUM)=PARMS(IACT(I,2))
                  ENDIF
                  KWINLIST=.TRUE.
                  EXIT
                ENDIF
              ENDDO
            ENDIF
            
            !IF KW NOT IN LIST THEN DETERMINE IF WE WANT TO ALTER TABLE
            IF((.NOT.KWINLIST).AND.TRIM(DBMSOUT).NE.'EXCEL'
     -          .AND.IADDCMPU.LT.1) THEN
              SQLStmtStr='ALTER TABLE '//trim(TABLENAME)//' ADD '//
     -          LWRAP//TRIM(KEYWRD)//RWRAP//' '//trim(DTYPE)//' null'
              !Close Cursor
              iRet = fvsSQLCloseCursor(StmtHndlOut)
              iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
              CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -            'DBSCMPU:Alter Table: '//trim(SQLStmtStr))

              !ADD NEW COLUMN TO MASTER LIST
              NUMKW = NUMKW+1
              KWLIST(NUMKW) = KEYWRD
              !ADD THE KEYWORD AND VALUES TO THE SQL ARRAYS
              KWINSRT(INSRTNUM+1)=KEYWRD
              KWVALS(INSRTNUM+1)=PARMS(IACT(I,2))
              INSRTNUM = INSRTNUM + 1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C     CHECK TO SEE IF WE HAVE SOMETHING MORE TO INSERT
C
      IF(INSRTNUM.GT.0) THEN

        !DO NOT INSERT 0 YEAR ENTRIES
        IF(.NOT.THISYR.GT.0)GOTO 10

        CALL INSERTCMPU(KWINSRT,KWVALS,THISYR,NPLT,INSRTNUM)

      ENDIF
   10 CONTINUE
      !make sure transactions are committed
      iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndlOut, SQL_COMMIT)
      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END



      SUBROUTINE INSERTCMPU(KWINSRT,KWVALS,THISYR,STANDID,NUMCMPU)
      IMPLICIT NONE
C
C     BUILDCMPUSQL
C     AUTH: D. GAMMEL -- SEM -- JUNE 2002
C     PURPOSE: TO POPULATE A DATABASE WITH THE COMPUTE TABLE
C              INFORMATION
C
C     INPUT: KWINSRT - THE ARRAY OF COMPUTE VARS TO INSERT
C            KWVALS  - THE ARRAY OF VALUES ASSOCIATED W/ THE VARS
C            NUMCMPU - THE NUMBER OF COMPUTE VARS TO INSERT
C
C     RETURN:SQLStmtStr - THE QUERY STRING TO USE FOR THE INSERT
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
      INTEGER NUMCMPU,X,THISYR,ID
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      CHARACTER*3000 SQLStmtStr,SQLStr1,SQLStr2
      CHARACTER*20 TABLENAME,DTYPE
      CHARACTER(LEN=26) STANDID
      CHARACTER(LEN=1) RWRAP,LWRAP
      CHARACTER(LEN = 8),DIMENSION (NUMCMPU)::KWINSRT
      REAL,DIMENSION(NUMCMPU)::KWVALS
      DOUBLE PRECISION,DIMENSION(NUMCMPU)::CURVAL
      SELECT CASE(TRIM(DBMSOUT))
      CASE('EXCEL')
        TABLENAME = '[FVS_Compute$]'
        DTYPE = 'Number'
        LWRAP = '['
        RWRAP = ']'
      CASE('ACCESS')
        TABLENAME = 'FVS_Compute'
        DTYPE = 'Double'
        LWRAP = '['
        RWRAP = ']'
      CASE('ORACLE')
        TABLENAME = 'FVS_Compute'
        DTYPE = 'real'
        LWRAP = '"'
        RWRAP = '"'
      CASE DEFAULT
        TABLENAME = 'FVS_Compute'
        DTYPE = 'real'
        LWRAP = ' '
        RWRAP = ' '
      END SELECT

      !BUILD THE EXECUTE QUERY
      SQLStr1 = 'INSERT INTO '//TABLENAME//
     -  '(CaseID,StandID,Year,'
      DO X=1,NUMCMPU
        SQLStr2 =TRIM(SQLStr1)//LWRAP//TRIM(KWINSRT(X))
     -    //RWRAP//','
        SQLStr1 = TRIM(SQLStr2)
      ENDDO
      SQLStr2 = SQLStr1(1:(LEN_TRIM(SQLStr1)-1))
      WRITE(SQLStr1,*)TRIM(SQLStr2),') VALUES("',
     -      CASEID,'","',TRIM(STANDID),'",?,'

      DO X=1,NUMCMPU
        SQLStr2 = TRIM(SQLStr1)//'?,'
        SQLStr1 = TRIM(SQLStr2)
      ENDDO
      SQLStr2 = SQLStr1(1:(LEN(TRIM(SQLStr1))-1))
      SQLStmtStr = TRIM(SQLStr2)//')'

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      !BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),THISYR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      DO X=1,(NUMCMPU)
        CURVAL(X)=KWVALS(X)
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -           SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),CURVAL(X),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
      ENDDO

      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSCMPU:Inserting Row, CMD='//trim(SQLStmtStr))

      END
