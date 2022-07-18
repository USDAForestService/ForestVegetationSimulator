      SUBROUTINE DBSCUTS(IWHO,KODE)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C     PURPOSE: TO OUTPUT THE CUTS LIST DATA TO THE DATABASE
C 
C     AUTH: D. GAMMEL -- RMRS -- JULY 2003
C
C     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 2
C                     INORDER FOR US TO CONTINUE
C            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
C                     REDIRECT OF THE FLAT FILE REPORT OR IN
C                     ADDITION TO
C
C---
COMMONS

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ESTREE.F77'
      INCLUDE 'VARCOM.F77'
      INCLUDE 'WORKCM.F77'
      INCLUDE 'DBSCOM.F77'
      INCLUDE 'METRIC.F77'      

COMMONS
C
      CHARACTER*8 TID,CSPECIES
      CHARACTER*2000 SQLStmtStr
      CHARACTER*20 TABLENAME,DTYPE
      INTEGER IWHO,I,JYR,IP,ITPLAB,ID,IDMR,ICDF,IBDF,IPTBAL,KODE
      INTEGER ISPC,I1,I2,I3,J
      INTEGER*4 IDCMP1,IDCMP2
      DATA IDCMP1,IDCMP2/10000000,20000000/
      REAL CW,P,CCFT,DGI,DP,Ht2TDCF,Ht2TDBF
C---------
C     IF CUTSOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 2
C     THEN JUST RETURN
C---------
      IF(ICUTLIST.EQ.0.OR.IWHO.NE.2) RETURN
C---------
C     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0
C---------
      IF(ICUTLIST.EQ.2) KODE = 0
C---------
C     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER
C---------
      CALL DBSCASE(1)

      IF(TRIM(DBMSOUT).EQ.'EXCEL') THEN
        TABLENAME = '[FVS_CutList$]'
        DTYPE = 'Number'
      ELSEIF(TRIM(DBMSOUT).EQ.'ACCESS') THEN
        TABLENAME = 'FVS_CutList'
        DTYPE = 'Double'
      ELSE
        TABLENAME = 'FVS_CutList'
        DTYPE = 'real'
      ENDIF
C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        ICUTLIST = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                 'DBSCUTS:Connecting to DSN')
        GOTO 100
      ENDIF

C---------
C     CHECK TO SEE IF THE CUTS LIST TABLE EXISTS IN DATBASE
C     IF IT DOESN'T THEN WE NEED TO CREATE IT
C---------
      SQLStmtStr= 'SELECT * FROM '//TABLENAME

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

      IF(iRet.NE.SQL_SUCCESS.AND.
     -    iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_CutList'//
     -             '(Id int primary key,'//
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
     -             'TPH double null,'//
     -             'MortPH double null,'//
     -             'DBH double null,'//
     -             'DG double null,'//
     -             'Ht double null,'//
     -             'HtG double null,'//
     -             'PctCr int null,'//
     -             'CrWidth double null,'//
     -             'MistCD int null,'//
     -             'BAPctile double null,'//
     -             'PtBAL double null,'//
     -             'TCuM double null,'//
     -             'MCuM double null,'//
     -             'NCuM double null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null)'

        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
          SQLStmtStr='CREATE TABLE FVS_CutList'//
     -             '(Id INT null,'//
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
     -             'TPH Number null,'//
     -             'MortPH Number null,'//
     -             'DBH Number null,'//
     -             'DG Number null,'//
     -             'Ht Number null,'//
     -             'HtG Number null,'//
     -             'PctCr int null,'//
     -             'CrWidth Number null,'//
     -             'MistCD int null,'//
     -             'BAPctile Number null,'//
     -             'PtBAL Number null,'//
     -             'TCuM Number null,'//
     -             'MCuM Number null,'//
     -             'NCuM Number null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null)'
        ELSE
          SQLStmtStr='CREATE TABLE FVS_CutList'//
     -             '(Id int primary key,'//
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
     -             'TPH real null,'//
     -             'MortPH real null,'//
     -             'DBH real null,'//
     -             'DG real null,'//
     -             'Ht real null,'//
     -             'HtG real null,'//
     -             'PctCr int null,'//
     -             'CrWidth real null,'//
     -             'MistCD int null,'//
     -             'BAPctile real null,'//
     -             'PtBAL real null,'//
     -             'TCuM real null,'//
     -             'MCuM real null,'//
     -             'NCuM real null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null)'
        ENDIF
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
        CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -       'DBSCUTS:Creating Table: '//trim(SQLStmtStr))
        CUTSID = 0
      ENDIF
C---------
C     SET THE CUTS LIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C     AND THE OUTPUT REPORTING YEAR.
C---------
      JYR=IY(ICYC)
      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            IP=0
            DO I=1,ITRN
              IF (WK3(I).GT.0) IP=IP+1
            ENDDO
            I=IND1(I3)
            ITPLAB=3
            P = WK3(I)/GROSPC
            DP = 0.0
C           SKIP OUTPUT IF P <= 0
            IF (P.LE.0.0) GOTO 50

C----------
C           TRANSLATE TREES IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C           GENERATED THROUGH THE ESTAB SYSTEM.
C----------
            IF (IDTREE(I) .GT. IDCMP1) THEN
              IF (IDTREE(I) .GT. IDCMP2) THEN
                WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              ELSE
                WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              ENDIF
            ELSE
              WRITE(TID,'(I8)') IDTREE(I)
            ENDIF
C----------
C           GET MISTLETOE RATING FOR CURRENT TREE RECORD.
C----------
            CALL MISGET(I,IDMR)
C----------
C           SET CROWN WIDTH.
C----------
            CW=CRWDTH(I)
C----------
C           DECODE DEFECT AND ROUND OFF POINT BAL.
C----------
            ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
            IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
            IPTBAL=NINT(PTBALT(I)*FT2pACRtoM2pHA)
C----------
C           GET DG INPUT
C----------
            DGI=DG(I)

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
C
            IF(ISPOUT17.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
            IF(ISPOUT17.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
            IF(ISPOUT17.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
C---------
C           CREATE ENTRY FROM DATA FOR CUTSLIST TABLE
C---------
            IF(CUTSID.EQ.-1) THEN
              CALL DBSGETID(TABLENAME,'Id',ID)
              CUTSID = ID
            ENDIF
            CUTSID = CUTSID + 1
C----------
C           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
C----------
            IF(CUTSID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

            WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(
     -           Id,CaseID,StandID,Year,PrdLen,
     -           TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,
     -           TPH,MortPH,DBH,DG,
     -           HT,HTG,PctCr,CrWidth,MistCD,
     -           BAPctile,PtBAL,TCuM,
     -           MCuM,NCuM,MDefect,BDefect,TruncHt,
     -           Ht2TDCF,Ht2TDBF) 
     -           VALUES(',CUTSID,',',ICASE,',',CHAR(39),
     -           TRIM(NPLT),CHAR(39),',',JYR,',',IFINT,",'",A
     -           DJUSTL(TID),"',",I,",'",ADJUSTL(NSP(ISP(I),1)(1:2)),
     -           "',",IMC(I),',',ISPECL(I),',',ITRE(I),
     -           ',',P/ACRtoHA,',',DP/ACRtoHA,',',DBH(I)*INtoCM,',',
     -           DGI*INtoCM,',',HT(I)*FTtoM,',',HTG(I)*FTtoM,',',
     -           ICR(I),',',CW*FTtoM,',',IDMR,',',PCT(I),',',IPTBAL,',',
     -           CFV(I)*FT3toM3,',',WK1(I)*FT3toM3,',',BFV(I)*FT3toM3,
     -           ',',ICDF,',',IBDF,',',
     -           NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),
     -           ',',HT2TD(I,1)*FTtoM,',',HT2TD(I,2)*FTtoM*0.0,')'

            iRet = fvsSQLCloseCursor(StmtHndlOut)
            iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -                  'DBSCUTS:Inserting Row')
  50        CONTINUE
          ENDDO
        ENDIF
      ENDDO

 100  CONTINUE

C
C     DO NOT ALLOW MORE THAN ONE CUTS LIST OUTPUT TO THE DATABASE PER RUN
C
C      ICUTLIST = 0     !This is incorrect since you can have
C                       !thinning in many different cycles - SAR 01/05

      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END
