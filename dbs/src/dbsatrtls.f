      SUBROUTINE DBSATRTLS(IWHO,KODE,TEM)
C
C $Id$
C
C     PURPOSE: TO OUTPUT THE ATRTLIST DATA TO THE DATABASE
C
C     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 3
C                     INORDER FOR US TO CONTINUE
C            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
C                     REDIRECT OF THE FLAT FILE REPORT OR IN
C                     ADDITION TO
C
      IMPLICIT NONE
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ESTREE.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
      CHARACTER*8 TID,CSPECIES
      CHARACTER*2000 SQLStmtStr
      CHARACTER*20 TABLENAME,DTYPE
      INTEGER IWHO,I,JYR,IP,ITPLAB,ID,IDMR,ICDF,IBDF,IPTBAL,KODE
      INTEGER ISPC,I1,I2,I3
      INTEGER*4 IDCMP1,IDCMP2
      DATA IDCMP1,IDCMP2/10000000,20000000/
      REAL CW,P,CCFT,DGI,DP,TEM,ESTHT

C---------
C     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
C     THEN JUST RETURN

      IF(IATRTLIST.EQ.0.OR.IWHO.NE.3) RETURN

C     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

      IF(IATRTLIST.EQ.2) KODE = 0


C     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

      CALL DBSCASE(1)

      IF(TRIM(DBMSOUT).EQ.'EXCEL') THEN
        TABLENAME = '[FVS_ATRTList$]'
        DTYPE = 'Number'
      ELSEIF(TRIM(DBMSOUT).EQ.'ACCESS') THEN
        TABLENAME = 'FVS_ATRTList'
        DTYPE = 'Double'
      ELSE
        TABLENAME = 'FVS_ATRTList'
        DTYPE = 'real'
      ENDIF


C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IATRTLIST = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                 'DBSATRTLS:Connecting to DSN')
        GOTO 100
      ENDIF


C     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
C     IF IT DOESN'T THEN WE NEED TO CREATE IT

      SQLStmtStr= 'SELECT * FROM '//TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(iRet.NE.SQL_SUCCESS.AND.
     -    iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_ATRTList('//
     -             'Id int primary key,'//
     -             'CaseID int not null,'//
     -             'StandID Text null,'//
     -             'Year int null,'//
     -             'PrdLen int null,'//
     -             'TreeId Text null,'//
     -             'TreeIndex int null,'//
     -             'Species Text null,'//
     -             'TreeVal int null,'//
     -             'SSCD int null,'//
     -             'PtIndex int null,'//
     -             'TPA double null,'//
     -             'MortPA double null,'//
     -             'DBH double null,'//
     -             'DG double null,'//
     -             'Ht double null,'//
     -             'HtG double null,'//
     -             'PctCr int null,'//
     -             'CrWidth double null,'//
     -             'MistCD int null,'//
     -             'BAPctile double null,'//
     -             'PtBAL double null,'//
     -             'TCuFt double null,'//
     -             'MCuFt double null,'//
     -             'BdFt double null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'EstHt double null,'//
     -             'ActPt int null,'//
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_ATRTList('//
     -             'Id INT null,'//
     -             'CaseID INT null,'//
     -             'StandID Text null,'//
     -             'Year INT null,'//
     -             'PrdLen int null,'//
     -             'TreeId Text null,'//
     -             'TreeIndex int null,'//
     -             'Species Text null,'//
     -             'TreeVal int null,'//
     -             'SSCD int null,'//
     -             'PtIndex int null,'//
     -             'TPA Number null,'//
     -             'MortPA Number null,'//
     -             'DBH Number null,'//
     -             'DG Number null,'//
     -             'Ht Number null,'//
     -             'HtG Number null,'//
     -             'PctCr int null,'//
     -             'CrWidth Number null,'//
     -             'MistCD int null,'//
     -             'BAPctile Number null,'//
     -             'PtBAL Number null,'//
     -             'TCuFt Number null,'//
     -             'MCuFt Number null,'//
     -             'BdFt Number null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'EstHt Number null,'//
     -             'ActPt int null,'//
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_ATRTList('//
     -             'Id int primary key,'//
     -             'CaseID int null,'//
     -             'StandID char(26) null,'//
     -             'Year int null,'//
     -             'PrdLen int null,'//
     -             'TreeId char(8) null,'//
     -             'TreeIndex int null,'//
     -             'Species char(3) null,'//
     -             'TreeVal int null,'//
     -             'SSCD int null,'//
     -             'PtIndex int null,'//
     -             'TPA real null,'//
     -             'MortPA real null,'//
     -             'DBH real null,'//
     -             'DG real null,'//
     -             'Ht real null,'//
     -             'HtG real null,'//
     -             'PctCr int null,'//
     -             'CrWidth real null,'//
     -             'MistCD int null,'//
     -             'BAPctile real null,'//
     -             'PtBAL real null,'//
     -             'TCuFt real null,'//
     -             'MCuFt real null,'//
     -             'BdFt real null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'EstHt real null,'//
     -             'ActPt int null,'//
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null)'
        ENDIF
        !PRINT*,SQLStmtStr
        !Close Cursor
        iRet = fvsSQLCloseCursor(StmtHndlOut)

        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -       'DBSATRTLS:Creating Table: '//trim(SQLStmtStr))
        IATRTLID = 0
      ENDIF

C     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C     AND THE OUTPUT REPORTING YEAR.
C
      JYR=IY(ICYC)
      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            IP=0
            DO I=1,ITRN
            IF (PROB(I).GT.0) IP=IP+1
            ENDDO
            I=IND1(I3)
            ITPLAB=4
            P=PROB(I)/GROSPC
            DP = 0.0
C           SKIP OUTPUT IF P <= 0
            IF (P.LE.0.0) GOTO 50

C           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C           GENERATED THROUGH THE ESTAB SYSTEM.

            IF (IDTREE(I) .GT. IDCMP1) THEN
              IF (IDTREE(I) .GT. IDCMP2) THEN
                WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              ELSE
                WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              ENDIF
            ELSE
              WRITE(TID,'(I8)') IDTREE(I)
            ENDIF

C           GET MISTLETOE RATING FOR CURRENT TREE RECORD.

            CALL MISGET(I,IDMR)

C           SET CROWN WIDTH.

            CW=CRWDTH(I)

C           DECODE DEFECT AND ROUND OFF POINT BAL.

            ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
            IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
            IPTBAL=NINT(PTBALT(I))

C           DETERMINE ESTIMATED HEIGHT
C           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HASN'T
C           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

            IF (NORMHT(I) .NE. 0) THEN
              ESTHT = (REAL(NORMHT(I))+5)/100
            ELSE
              ESTHT = HT(I)
            ENDIF

C           GET DG INPUT

            DGI=DG(I)
            IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)
C
C           DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C           KEYWORD OVER RIDES
C
            IF(JSPIN(ISP(I)).EQ.1)THEN
              CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
            ELSEIF(JSPIN(ISP(I)).EQ.2)THEN
              CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
            ELSEIF(JSPIN(ISP(I)).EQ.3)THEN
              CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
            ELSE
              CSPECIES=ADJUSTL(PLNJSP(ISP(I)))
            ENDIF

            IF(ISPOUT31.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
            IF(ISPOUT31.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
            IF(ISPOUT31.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))

C           CREATE ENTRY FROM DATA FOR ATRTLIST TABLE

            IF(IATRTLID.EQ.-1) THEN
              CALL DBSGETID(TABLENAME,'Id',ID)
              IATRTLID = ID
            ENDIF
            IATRTLID = IATRTLID + 1

C           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

            IF(IATRTLID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

            WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(
     -           Id,CaseID,StandID,Year,PrdLen,
     -           TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,
     -           MortPA,DBH,DG,
     -           HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,TCuFt,
     -           MCuFt,BdFt,MDefect,BDefect,TruncHt,
     -           EstHt,ActPt,Ht2TDCF,Ht2TDBF) VALUES(',
     -           IATRTLID,',',ICASE,',',CHAR(39),TRIM(NPLT),CHAR(39),
     -           ',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,",'",
     -           CSPECIES,"',",IMC(I),',',ISPECL(I),',',ITRE(I),
     -           ',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),
     -           ',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',
     -           CFV(I),',',WK1(I),',',BFV(I),',',ICDF,',',IBDF,',',
     -           ((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),
     -           ',',HT2TD(I,2),',',HT2TD(I,1),')'

            !PRINT*, SQLStmtStr

            !Close Cursor
            iRet = fvsSQLCloseCursor(StmtHndlOut)

            iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                  'DBSATRTLS:Inserting Row: '//trim(SQLStmtStr))
  50        CONTINUE
          ENDDO
        ENDIF
      ENDDO

 100  CONTINUE

      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
