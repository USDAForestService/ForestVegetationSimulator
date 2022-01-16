      SUBROUTINE DBSBMBKP(NPLT,IYEAR,OLDBKP,NEWBKP,SELFBKP,
     >           TO_LS,FRM_LS,BKPIN,BKPOUT,BKPSV,STRPBKP,
     >           STRP_SC,REMBKP,RV,DVRV1,DVRV2,DVRV3,DVRV4,DVRV5,
     >           DVRV6,DVRV7,DVRV8,DVRV9,TPAFAST,BAFAST,VOLFAST,CID)
C
C DBS $Id$
C
C     PURPOSE: POPULATES A DATABASE TABLE WITH DETAILED WWPBM OUTPUT VARS.
C              PERTAINING TO BKP AND RATING VALUES (RVs)
C     AUTH: AJ MCMAHAN -- ITX, INC. SEPT. 2005
C     INPUT:
C
C     OLDBKP:  PRE-DISPERSAL (BUT POST-REPRODUCTION) BKP.
C              SAME AS "MAINOUT"'s PREDBKP
C     NEWBKP:  POST-DISPERSAL (& POST-INFLIGHT MORTALITY) BKP.
C              SAME AS "MAINOUT"'s POSTDBKP
C     SELFBKP: BKP ALLOCATED TO SELF
C     TO_LS:   BKP EMMIGRATING FROM SELF TO OTHER STANDS IN THE LANDSCAPE
C     FRM_LS:  BKP IMMIGRATING FROM OTHER STANDS IN LS
C     BKPIN:   BKP IMMIGRATING FROM OUTSIDE WORLD (OW)
C     BKPOUT:  BKP EMMIGRATING FROM SELF TO OW
C     BKPSV:   PERCENT SURVIVAL OF BKP (IN-FLIGHT MORTALITY RATE = (1-SURVIVAL)
C     STRPBKP: BKP USED IN FINAL STRIPKILL/PITCHOUT TREE
C     SRTP_SC: SIZE CLASS OF FINAL STRIPKILL/PITCHOUT TREE
C     REMBKP:  BKP REMOVED VIA SANITATION CUTS
C     RV:      STAND RATING VALUE
C     DVRV1-10:RATING VALUES OF THE 9 COMPONENT DRIVING VARIABLES (DVs)
C     TPAFAST: TPA KILLED BY WWPBM "FAST" DVs
C     BAFAST:  BASAL AREA KILLED BY WWPBM "FAST" DVs
C     VOLFAST: VOLUME KILLED BY WWPBM "FAST" DVs
C
C
C---
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS 
C
C     DECLARATIONS
C---
      INTEGER IYEAR,IRCODE
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      REAL OLDBKP,NEWBKP,SELFBKP,TO_LS,FRM_LS,BKPIN,BKPOUT,
     > BKPSV,STRPBKP,STRP_SC,RV,DVRV1,DVRV2,DVRV3,DVRV4,DVRV5,
     > DVRV6,DVRV7,DVRV8,DVRV9,TPAFAST,BAFAST,VOLFAST,REMBKP
      DOUBLE PRECISION OLDBKPb,NEWBKPb,SELFBKPb,TO_LSb,FRM_LSb,
     > BKPINb,BKPOUTb,BKPSVb,STRPBKPb,STRP_SCb,RVb,DVRV1b,DVRV2b,
     > DVRV3b,DVRV4b,DVRV5b,DVRV6b,DVRV7b,DVRV8b,DVRV9b,
     > TPAFASTb,BAFASTb,VOLFASTb,REMBKPB
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME
      CHARACTER(len=26) NPLT
C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSBMBKP:DSN Connection')
        GOTO 200
      ENDIF
C---------
C     CHECK TO SEE IF THE WWPBM "BKPOUT" TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_BM_BKP$]'
      ELSE
        TABLENAME = 'FVS_BM_BKP'
      ENDIF

      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) RETURN
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_BM_BKP('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year Int null,'//
     -              'OLDBKP    double null,'//
     -              'NEWBKP    double null,'//
     -              'SELFBKP   double null,'//
     -              'TO_LS     double null,'//
     -              'FRM_LS    double null,'//
     -              'IN_OW     double null,'//
     -              'OUT2OW    double null,'//
     -              'PER_SURV  double null,'//
     -              'STRPBKP   double null,'//
     -              'STRP_SC   double null,'//
     -              'REMBKP    double null,'//
     -              'RV        double null,'//
     -              'DVRV1     double null,'//
     -              'DVRV2     double null,'//
     -              'DVRV3     double null,'//
     -              'DVRV4     double null,'//
     -              'DVRV5     double null,'//
     -              'DVRV6     double null,'//
     -              'DVRV7     double null,'//
     -              'DVRV8     double null,'//
     -              'DVRV9     double null,'//
     -              'TPAFAST   double null,'//
     -              'BAFAST    double null,'//
     -              'VOLFAST   double null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_BM_BKP('//
     -              'CaseID Text,'//
     -              'StandID Text,'//
     -              'Year Int,'//
     -              'OLDBKP    Number,'//
     -              'NEWBKP    Number,'//
     -              'SELFBKP   Number,'//
     -              'TO_LS     Number,'//
     -              'FRM_LS    Number,'//
     -              'IN_OW     Number,'//
     -              'OUT2OW    Number,'//
     -              'PER_SURV  Number,'//
     -              'STRPBKP   Number,'//
     -              'STRP_SC   Number,'//
     -              'REMBKP    Number,'//
     -              'RV        Number,'//
     -              'DVRV1     Number,'//
     -              'DVRV2     Number,'//
     -              'DVRV3     Number,'//
     -              'DVRV4     Number,'//
     -              'DVRV5     Number,'//
     -              'DVRV6     Number,'//
     -              'DVRV7     Number,'//
     -              'DVRV8     Number,'//
     -              'DVRV9     Number,'//
     -              'TPAFAST   Number,'//
     -              'BAFAST    Number,'//
     -              'VOLFAST   Number)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_BM_BKP('//
      -              'CaseID       char(36) not null,'//
     -              'StandID       char(26) not null,'//
     -              'Year          Int null,'//
     -              'OLDBKP    real null,'//
     -              'NEWBKP    real null,'//
     -              'SELFBKP   real null,'//
     -              'TO_LS     real null,'//
     -              'FRM_LS    real null,'//
     -              'IN_OW     real null,'//
     -              'OUT2OW    real null,'//
     -              'PER_SURV  real null,'//
     -              'STRPBKP   real null,'//
     -              'STRP_SC   real null,'//
     -              'REMBKP    real null,'//
     -              'RV        real null,'//
     -              'DVRV1     real null,'//
     -              'DVRV2     real null,'//
     -              'DVRV3     real null,'//
     -              'DVRV4     real null,'//
     -              'DVRV5     real null,'//
     -              'DVRV6     real null,'//
     -              'DVRV7     real null,'//
     -              'DVRV8     real null,'//
     -              'DVRV9     real null,'//
     -              'TPAFAST   real null,'//
     -              'BAFAST    real null,'//
     -              'VOLFAST   real null)'

        ENDIF
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSBMBKP:Creating Table: '//trim(SQLStmtStr))
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS & ARRAYS
C
       OLDBKPB  = OLDBKP
       NEWBKPB  = NEWBKP
       SELFBKPB = SELFBKP
       TO_LSB   = TO_LS
       FRM_LSB  = FRM_LS
       BKPINB   = BKPIN
       BKPOUTB  = BKPOUT
       BKPSVB   = BKPSV
       STRPBKPB = STRPBKP
       STRP_SCB = STRP_SC
       REMBKPB  = REMBKP
       RVB      = RV
       DVRV1B   = DVRV1
       DVRV2B   = DVRV2
       DVRV3B   = DVRV3
       DVRV4B   = DVRV4
       DVRV5B   = DVRV5
       DVRV6B   = DVRV6
       DVRV7B   = DVRV7
       DVRV8B   = DVRV8
       DVRV9B   = DVRV9
       TPAFASTB = TPAFAST
       BAFASTB  = BAFAST
       VOLFASTB = VOLFAST

C
      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,
     -  ' (CaseID,StandID,Year,',
     -  'OLDBKP,NEWBKP,SELFBKP,TO_LS,FRM_LS,IN_OW,OUT2OW,PER_SURV,',
     -  'STRPBKP,STRP_SC,REMBKP,RV,DVRV1,DVRV2,DVRV3,DVRV4,DVRV5,',
     -  'DVRV6,DVRV7,DVRV8,DVRV9,TPAFAST,BAFAST,VOLFAST) VALUES (''',
     -  CID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?',
     -  ',?,?,?,?,?,?,?,?,?,?,?)'

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT,
     -           SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -           INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),OLDBKPB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),NEWBKPB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),SELFBKPB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),TO_LSB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),FRM_LSB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),BKPINB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),BKPOUTB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),BKPSVB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),STRPBKPB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),STRP_SCB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -         SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -         INT(5,SQLSMALLINT_KIND),REMBKPB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),RVB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV1B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV2B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV3B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV4B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV5B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV6B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV7B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV8B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),DVRV9B,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),TPAFASTB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),BAFASTB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

      ColNumber=ColNumber+1
      iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -           SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -           INT(5,SQLSMALLINT_KIND),VOLFASTB,int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)

  100 CONTINUE
      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecute(StmtHndlOut)
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -              'DBSBMBKP:Inserting Row')

  200 CONTINUE
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END

