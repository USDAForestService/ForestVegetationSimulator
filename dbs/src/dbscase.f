      SUBROUTINE DBSCASE(IFORSURE)
      IMPLICIT NONE
C
C $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              OUTPUT.
C
C     INPUT: IFORSURE - 1 NEED CONNECTION, 0 CHECK IF CONNECTION IS
C                       NEEDED.
C
C---
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'KEYCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C---

      INTEGER(SQLINTEGER_KIND),parameter:: MaxStringLen=255

      CHARACTER*2000 SQLStmtStr
      CHARACTER*10  DATO
      CHARACTER*8   TIM
      CHARACTER*(*) CFN
      CHARACTER(len=MaxStringLen) TIMESTAMP
      INTEGER IFORSURE, IFORSR, I, KODE, IRCODE
      CHARACTER(len=36) CID

      CHARACTER*20 TABLENAME

      CHARACTER*7 VVER

      CHARACTER*2 VAR

      CALL VARVER(VVER)
      VAR=VVER(:2)
      IF(VAR.EQ.'BP' .OR. VAR.EQ.'LP' .OR. VAR.EQ.'SF' .OR.
     & VAR.EQ.'SM' .OR. VAR.EQ.'SP')VAR='CR'

C-----
C     CHECK TO SEE IF WE ARE NEEDING TO CONTINUE
C-----
      IFORSR=IFORSURE
      IF(IFORSR.EQ.0) THEN
        IF(ISUMARY.GE.1.OR.
     -     ICOMPUTE.GE.1.OR.
     -     IATRTLIST.GE.1.OR.
     -     ITREELIST.GE.1.OR.
     -     ICUTLIST.GE.1.OR.
     -     IDM1.GE.1.OR.IDM2.GE.1.OR.IDM3.GE.1.OR.
     -     IDM5.GE.1.OR.IDM6.GE.1.OR.
     -     IPOTFIRE.GE.1.OR.
     -     IFUELS.GE.1.OR.
     -     ICMRPT.GE.1.OR.
     -     ICHRPT.GE.1.OR.
     -     ISTRCLAS.GE.1.OR.
     -     IFUELC.GE.1.OR.
     -     IBURN.GE.1.OR.
     -     IMORTF.GE.1.OR.
     -     ISSUM.GE.1.OR.
     -     ISDET.GE.1.OR.
     -     ICANPR.GE.1.OR.
     -     IDWDVOL.GE.1.OR.
     -     IDWDCOV.GE.1.OR.
     -     IBMMAIN.GE.1.OR.
     -     IBMBKP.GE.1.OR.
     -     IBMTREE.GE.1.OR.
     -     IBMVOL.GE.1) IFORSR = 1
       ENDIF
       IF(IFORSR.EQ.0) RETURN

C---------
C     IF ALREADY HAVE A CURRENT CASE NUMBER, JUST BAIL
C---------
      IF (CASEID.NE."") RETURN
C---
C     Initialize variables
C     CREATE DATETIME AND KEYFNAME
      CALL GRDTIM(DATO,TIM)
      TIMESTAMP = DATO(7:10)//'-'//DATO(1:5)//'-'//TIM
C---------
C     MAKE SURE WE HAVE AN OPEN CONNECTION
C---------
      IF(ConnHndlOut.EQ.-1) CALL DBSOPEN(DSNOUT,EnvHndlOut,
     -                      ConnHndlOut,DBMSOUT,0,.FALSE.,KODE)
C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ICOMPUTE  = 0
        ISUMARY   = 0
        IATRTLIST = 0
        ITREELIST = 0
        ICUTLIST  = 0
        IDM1      = 0
        IDM2      = 0
        IDM3      = 0
        IDM5      = 0
        IDM6      = 0
        IPOTFIRE  = 0
        IFUELS    = 0
        ICMRPT    = 0
        ICHRPT    = 0
        ISTRCLAS  = 0
        IFUELC    = 0
        IBURN     = 0
        IMORTF    = 0
        ISSUM     = 0
        ISDET     = 0
        ICANPR    = 0
        IDWDVOL   = 0
        IDWDCOV   = 0
        IBMMAIN   = 0
        IBMBKP    = 0
        IBMTREE   = 0
        IBMVOL    = 0
C
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                 'DBSRun:Connecting to DSN')
        GOTO 200
      ENDIF

C---------
C     MAKE SURE WE HAVE AN OPEN CONNECTION
C---------
      IF(ConnHndlOut.EQ.-1) CALL DBSOPEN(DSNOUT,EnvHndlOut,
     -                      ConnHndlOut,DBMSOUT,0,.FALSE.,KODE)
C---------
C     CHECK TO SEE IF THE FVS_Cases TABLE EXISTS IN DATBASE
C---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_Cases$]'
      ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
        TABLENAME = '"FVS_Cases"'
      ELSE
        TABLENAME = 'FVS_Cases'
      ENDIF
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
      IF(IRCODE.EQ.2) RETURN
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr="CREATE TABLE FVS_Cases("//
     -              "CaseID Text primary key,"//
     -              "Stand_CN Text null,"//
     -              "StandID Text null,"//
     -              "MgmtID Text null,"//
     -              "RunTitle Text null,"//
     -              "KeywordFile Text null,"//
     -              "SamplingWt double null,"//
     -              "Variant Text null,"//
     -              "Groups Text null,"//
     -              "RunDateTime Text null)"
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr="CREATE TABLE FVS_Cases("//
     -              "CaseID Text,"//
     -              "Stand_CN Text,"//
     -              "StandID Text,"//
     -              "MgmtID Text,"//
     -              "RunTitle Text,"//
     -              "KeywordFile Text,"//
     -              "SamplingWt Number,"//
     -              "Variant Text,"//
     -              "Groups Text,"//
     -              "RunDateTime Text);"
        ELSE
          SQLStmtStr="CREATE TABLE "//TABLENAME//
     -              " (CaseID char(36) primary key,"//
     -              "Stand_CN char(40),"//
     -              "StandID char(26),"//
     -              "MgmtID char(4),"//
     -              "RunTitle char(72),"//
     -              "KeywordFile char(50),"//
     -              "SamplingWt real,"//
     -              "Variant char(2),"//
     -              "Groups char(250),"//
     -              "RunDateTime char(19))"
        ENDIF

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -       'DBSCase:Creating Table: '//trim(SQLStmtStr))
        !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

      ENDIF
C---------
C     CREATE ENTRY FROM DATA FOR FVSRUN TABLE
C---------
      CALL UUIDGEN(CASEID)
C----------
C           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C----------
      if (LENSLS.EQ.-1) SLSET =""
      IF (KEYFNAME.EQ.' ') KEYFNAME='Unknown'
      WRITE(SQLStmtStr,*)"INSERT INTO ",trim(TABLENAME),
     - " (CaseID,Stand_CN,StandID,MgmtID,RunTitle,KeywordFile,",
     - "SamplingWt,Variant,Groups,RunDateTime) VALUES('",CASEID,"','",
     - TRIM(DBCN),"','",TRIM(NPLT),"','",TRIM(MGMID),"','",
     - TRIM(ITITLE),"','",TRIM(KEYFNAME),"',",SAMWT,",'",VAR,"','",
     - TRIM(SLSET),"','",TRIM(TIMESTAMP),"')"

      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)

      !Execute Query
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     >              'DBSCase:Inserting Row: '//trim(SQLStmtStr))

      !Release statement handle
  200 CONTINUE
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
      RETURN

C
C     CALLED BY FILOPN: ENTRY TO SAVE THE KEYWORD FILE NAME AND TO SET
C                       THE DEFAULT DBS CONNECTIONS
C
      ENTRY DBSVKFN (CFN)
      I=LEN_TRIM(CFN)
      IF (I.GT.LEN(KEYFNAME)) THEN
         KEYFNAME=CFN(1:4)//'...'//CFN(I+8-LEN(KEYFNAME):)
      ELSE
         KEYFNAME = CFN
      ENDIF
      RETURN
C
C======================================================================
C     ENTRY FOR WWPBM, FETCHING CASEID
C    (NOTE: THE WWPBM NEEDS TO KNOW AND SAVE (INTERNALLY) CASEID, BECAUSE
C     IT IS DOING ITS DB-WRITING FROM WITHIN ITS OWN INTERNAL STAND LOOP
C
      ENTRY DBSWW2(CID)
      CID=CASEID
      RETURN
C======================================================================
      END

