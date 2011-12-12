      SUBROUTINE DBSFMMORT(IYEAR,KILLED,TOTAL,BAKILL,
     -  VOKILL,KODE)
      IMPLICIT NONE
C----------
C  **FMMORT--DBS  DATE OF LAST REVISION: 10/31/2011
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE MORTALITY REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE MORTALITY OUTPUT FROM THE FIRE MODEL.
C              1: KILLED TREES PER ACRE FOR EACH SIZE CLASS
C              2: TOTAL TREES PER ACRE FOR EACH SIZE CLASS
C              3: MORTALITY IN TERMS OF BASAL AREA
C              4: MORTALITY IN TERMS OF CUFT VOLUME
C              5: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
C     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
C
COMMONS
C
C      
      INCLUDE 'PRGPRM.F77'    
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'PLOT.F77'        
C
COMMONS
C---
      INTEGER MXSP1
      PARAMETER (MXSP1 = MAXSP + 1)
      INTEGER IYEAR,ID,KODE,I,J
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL KILLED,TOTAL,BAKILL,VOKILL
      DOUBLE PRECISION KILLEDB,TOTALB,BAKILLB,VOKILLB
      DIMENSION KILLED(MXSP1,8),TOTAL(MXSP1,8),KILLEDB(MXSP1,8),
     -          TOTALB(MXSP1,8)
      DIMENSION BAKILL(MXSP1),VOKILL(MXSP1),BAKILLB(MXSP1),
     -          VOKILLB(MXSP1)
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=3) CSP
      CHARACTER(LEN=8) CSPECIES

C
C
COMMONS END

C---
C     Initialize variables
C
      IF(IMORTF.EQ.0) RETURN
      IF(IMORTF.EQ.2) KODE = 0

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)

C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IMORTF = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSFMMORT:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE MORTALITY TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Mortality$]'
      ELSE
        TABLENAME = 'FVS_Mortality'
      ENDIF
      SQLStmtStr= 'SELECT * FROM ' // TABLENAME

      !PRINT*, SQLStmtStr
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))


      IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR.
     -    iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_Mortality('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'Species Text null,'//
     -              'Killed_class1 double null,'//
     -              'Total_class1 double null,'//
     -              'Killed_class2 double null,'//
     -              'Total_class2 double null,'//
     -              'Killed_class3 double null,'//
     -              'Total_class3 double null,'//
     -              'Killed_class4 double null,'//
     -              'Total_class4 double null,'//
     -              'Killed_class5 double null,'//
     -              'Total_class5 double null,'//
     -              'Killed_class6 double null,'//
     -              'Total_class6 double null,'//
     -              'Killed_class7 double null,'//
     -              'Total_class7 double null,'//
     -              'Bakill double null,'//     
     -              'Volkill double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_Mortality('//
     -              'ID Int,'//
     -              'CaseID int,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'Species Text,'//
     -              'Killed_class1 Number,'//
     -              'Total_class1 Number,'//
     -              'Killed_class2 Number,'//
     -              'Total_class2 Number,'//
     -              'Killed_class3 Number,'//
     -              'Total_class3 Number,'//
     -              'Killed_class4 Number,'//
     -              'Total_class4 Number,'//
     -              'Killed_class5 Number,'//
     -              'Total_class5 Number,'//
     -              'Killed_class6 Number,'//
     -              'Total_class6 Number,'//
     -              'Killed_class7 Number,'//
     -              'Total_class7 Number,'//
     -              'Bakill Number,'//     
     -              'Volkill Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_Mortality('//
     -              'Id int primary key,'//
     -              'CaseID int not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Species char(3) null,'//
     -              'Killed_class1 real null,'//
     -              'Total_class1 real null,'//
     -              'Killed_class2 real null,'//
     -              'Total_class2 real null,'//
     -              'Killed_class3 real null,'//
     -              'Total_class3 real null,'//
     -              'Killed_class4 real null,'//
     -              'Total_class4 real null,'//
     -              'Killed_class5 real null,'//
     -              'Total_class5 real null,'//
     -              'Killed_class6 real null,'//
     -              'Total_class6 real null,'//
     -              'Killed_class7 real null,'//
     -              'Total_class7 real null,'//
     -              'Bakill real null,'//     
     -              'Volkill real null)'
        ENDIF
        !PRINT*, SQLStmtStr

            !Close Cursor
            iRet = fvsSQLCloseCursor(StmtHndlOut)

            iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'FMMORT:Creating Table: '//trim(SQLStmtStr))
        MORTID = 0
      ENDIF

      DO 150 J = 1,MXSP1
      
        IF (J .EQ. MXSP1) THEN
        	CSP = 'ALL'
        ELSE
        	CSP = JSP(J)
        ENDIF		 

C       ONLY WRITE INFO FOR SPECIES IN THE STAND

        IF (TOTAL(J,8) .LE. 0) GOTO 150
C
C       DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C       KEYWORD OVER RIDES
C     
        IF(JSPIN(J).EQ.1)THEN
          CSPECIES=ADJUSTL(JSP(J))
        ELSEIF(JSPIN(J).EQ.2)THEN
          CSPECIES=ADJUSTL(FIAJSP(J))
        ELSEIF(JSPIN(J).EQ.3)THEN
          CSPECIES=ADJUSTL(PLNJSP(J))
        ELSE
          CSPECIES=ADJUSTL(PLNJSP(J))
        ENDIF
C     
        IF(ISPOUT21.EQ.1)CSPECIES=ADJUSTL(JSP(J))
        IF(ISPOUT21.EQ.2)CSPECIES=ADJUSTL(FIAJSP(J))
        IF(ISPOUT21.EQ.3)CSPECIES=ADJUSTL(PLNJSP(J))
        IF(CSP.EQ.'ALL')CSPECIES='ALL'
C       
C       CREATE ENTRY FROM DATA FOR MORTALITY TABLE
C       
        IF(MORTID.EQ.-1) THEN
          CALL DBSGETID(TABLENAME,'Id',ID)
          MORTID = ID
        ENDIF
        MORTID = MORTID + 1
C       
C       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C       
        IF(MORTID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100
        
C       
C       ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C       
        BAKILLB(J) = BAKILL(J)
        VOKILLB(J) = VOKILL(J)
         
        DO I = 1,8
        KILLEDB(J,I) = KILLED(J,I)
        TOTALB(J,I) = TOTAL(J,I)
        ENDDO
        
        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (Id,CaseID,
     -    StandID,Year,Species,Killed_class1,Total_class1,Killed_class2,
     -    Total_class2,Killed_class3,Total_class3,Killed_class4,
     -    Total_class4,Killed_class5,Total_class5,Killed_class6,
     -    Total_class6,Killed_class7,Total_class7,Bakill,Volkill) 
     -    VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,
     -   ',CHAR(39),TRIM(CSPECIES),CHAR(39),',?,?,?,?,?,?,?,?,?,?,?,
     -    ?,?,?,?,?)'
        
        !PRINT*, SQLStmtStr
C       
C       CLOSE CURSOR
C       
        iRet = fvsSQLCloseCursor(StmtHndlOut)
C       
C       PREPARE THE SQL QUERY
C       
        iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C       
C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C       
        
        ColNumber=1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),MORTID,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             
     -             SQL_PARAM_INPUT,
     -             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),ICASE,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             
     -             SQL_PARAM_INPUT,
     -             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,1),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),TOTALB(J,1),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,2),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),TOTALB(J,2),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,3),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),TOTALB(J,3),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,4),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),TOTALB(J,4),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,5),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),TOTALB(J,5),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
     
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,6),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),TOTALB(J,6),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -        SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -        INT(5,SQLSMALLINT_KIND),KILLEDB(J,7),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),TOTALB(J,7),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -       SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -       INT(5,SQLSMALLINT_KIND),BAKILLB(J),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -             SQL_PARAM_INPUT,
     -       SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -       INT(0,SQLSMALLINT_KIND),VOKILLB(J),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        
  100   CONTINUE
        !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        
        iRet = fvsSQLExecute(StmtHndlOut)
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                'FMMORT:Inserting Row')
        
        
  150 CONTINUE

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


