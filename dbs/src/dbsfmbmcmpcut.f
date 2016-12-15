      SUBROUTINE DBSFMBMCMPCUT(IYR)
      IMPLICIT NONE
C
C $Id
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE CUT-TREE BIOMASS
C              COMPONENT ESTIMATES
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C----------
COMMONS
C----------
      INTEGER IYR,IRCODE,KODE,I,J,IT
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      DOUBLE PRECISION  VAR(47),TREB,PB,DPB,DBHB,HTB,CWB,JENKTREB(8)
      REAL CW,DP,P
      INTEGER I1,I2,I3,ISPC
      INTEGER*4 IDCMP1,IDCMP2
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=20) TABLENAME,DTYPE
      CHARACTER(len=3) CSP
      CHARACTER(LEN=8) CSPECIES,TID
      DATA IDCMP1,IDCMP2/10000000,20000000/
C----------
C     Initialize variables
C----------
      IF(IFMBMCMPCUT.EQ.0) RETURN
C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)
C---------
C     ALLOCATE A STATEMENT HANDLE
C---------
      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
      IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        IFMBMCMPCUT = 0
        PRINT *,'Error connecting to data source'
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,
     -                  'DBSBMCMP:DSN Connection')
        GOTO 100
      ENDIF
C---------
C     CHECK TO SEE IF THE Component Biomass TABLE EXISTS IN DATBASE
c---------
      IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
        TABLENAME = '[FVS_CutBiocmp$]'
        DTYPE = 'Number'
      ELSEIF(TRIM(DBMSOUT).EQ.'ACCESS') THEN
        TABLENAME = 'FVS_CutBiocmp'
        DTYPE = 'Double'
      ELSE
        TABLENAME = 'FVS_CutBiocmp'
        DTYPE = 'real'
      ENDIF
C
C  CHECK TO MAKE SURE DON'T EXCEED MAX RECORDS IN EXCEL
C
      CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
C
      IF(IRCODE.EQ.2) THEN
        IFMBMCMPCUT = 0
        RETURN
      ENDIF
      IF(IRCODE.EQ.1) THEN
        IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
          SQLStmtStr='CREATE TABLE FVS_CutBiocmp('//
     -              'CaseID Text not null,'//
     -              'StandID Text null,'//
     -              'Year int null,'//
     -              'TreeId Text null,'//
     -              'TreeIndex int null,'//
     -              'Species Text null,'//
     -              'TreeVal int null,'//
     -              'SSCD int null,'//
     -              'PtIndex int null,'//
     -              'TPA double null,'//
     -              'MortPA double null,'//
     -              'DBH double null,'//
     -              'Ht double null,'//
     -              'PctCr int null,'//
     -              'CrWidth double null,'//
     -              'ABT double null,'//
     -              'Wood double null,'//
     -              'Bark double null,'//
     -              'Foliage double null,'//
     -              'Roots double null,'//
     -              'Branches double null,'//
     -              'CompleteTree double null,'//
     -              'WholeTreeAboveGround double null,'//
     -              'WholeTreeAboveStump double null,'//
     -              'StemWood double null,'//
     -              'StemBark double null,'//
     -              'StemTotal double null,'//
     -              'StemTop double null,'//
     -              'BranchesLive double null,'//
     -              'BranchesLive_0_1 double null,'//
     -              'BranchesLive_1_3 double null,'//
     -              'BranchesLive_3_plus double null,'//
     -              'BranchesDead double null,'//
     -              'BranchesTotal double null,'//
     -              'StemBranchesBarkOnly double null,'//
     -              'StemBranchesWoodOnly double null,'//
     -              'StemBranchesLive double null,'//
     -              'StemBranchesTotal double null,'//
     -              'FoliageTotal double null,'//
     -              'FoliageNew double null,'//
     -              'FoliageOld double null,'//
     -              'TwigsTotal double null,'//
     -              'TwigsOld double null,'//
     -              'FoliageTwigs double null,'//
     -              'Crown double null,'//
     -              'RootsCoarse double null,'//
     -              'CoarseStumpRoots double null,'//
     -              'CoarseLateralRoots double null,'//
     -              'FineRoots double null,'//
     -              'RootsTotal double null,'//
     -              'StumpWood double null,'//
     -              'StumpBark double null,'//
     -              'StumpTotal double null,'//
     -              'Stump_Roots double null,'//
     -              'Cones double null,'//
     -              'LiveCrown double null,'//
     -              'DeadCrown double null,'//
     -              'SmallBranches double null,'//
     -              'merchStemTotal double null,'//
     -              'merchStemWood double null,'//
     -              'merchStemBark double null,'//
     -              'StemTopWood double null,'//
     -              'StemTopBark double null,'//
     -              'Stump_Branches_Tip double null,'//
     -              'Branches_Tip double null,'//
     -              'AboveStumpWoodBark double null,'//
     -              'Wood_Bark_to_min_Branch double null,'//
     -              'BranchesLive_1_plus double null)'
        ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
         SQLStmtStr='CREATE TABLE FVS_CutBiocmp('//
     -              'CaseID Text null,'//
     -              'StandID Text null,'//
     -              'Year int null,'//
     -              'TreeId Text null,'//
     -              'TreeIndex int null,'//
     -              'Species Text null,'//
     -              'TreeVal int null,'//
     -              'SSCD int null,'//
     -              'PtIndex int null,'//
     -              'TPA Number null,'//
     -              'MortPA Number null,'//
     -              'DBH Number null,'//
     -              'Ht Number null,'//
     -              'PctCr int null,'//
     -              'CrWidth Number null,'//
     -              'ABT Number null,'//
     -              'Wood Number null,'//
     -              'Bark Number null,'//
     -              'Foliage Number null,'//
     -              'Roots Number null,'//
     -              'Branches Number null,'//
     -              'CompleteTree Number null,'//
     -              'WholeTreeAboveGround Number null,'//
     -              'WholeTreeAboveStump Number null,'//
     -              'StemWood Number null,'//
     -              'StemBark Number null,'//
     -              'StemTotal Number null,'//
     -              'StemTop Number null,'//
     -              'BranchesLive Number null,'//
     -              'BranchesLive_0_1 Number null,'//
     -              'BranchesLive_1_3 Number null,'//
     -              'BranchesLive_3_plus Number null,'//
     -              'BranchesDead Number null,'//
     -              'BranchesTotal Number null,'//
     -              'StemBranchesBarkOnly Number null,'//
     -              'StemBranchesWoodOnly Number null,'//
     -              'StemBranchesLive Number null,'//
     -              'StemBranchesTotal Number null,'//
     -              'FoliageTotal Number null,'//
     -              'FoliageNew Number null,'//
     -              'FoliageOld Number null,'//
     -              'TwigsTotal Number null,'//
     -              'TwigsOld Number null,'//
     -              'FoliageTwigs Number null,'//
     -              'Crown Number null,'//
     -              'RootsCoarse Number null,'//
     -              'CoarseStumpRoots Number null,'//
     -              'CoarseLateralRoots Number null,'//
     -              'FineRoots Number null,'//
     -              'RootsTotal Number null,'//
     -              'StumpWood Number null,'//
     -              'StumpBark Number null,'//
     -              'StumpTotal Number null,'//
     -              'Stump_Roots Number null,'//
     -              'Cones Number null,'//
     -              'LiveCrown Number null,'//
     -              'DeadCrown Number null,'//
     -              'SmallBranches Number null,'//
     -              'merchStemTotal Number null,'//
     -              'merchStemWood Number null,'//
     -              'merchStemBark Number null,'//
     -              'StemTopWood Number null,'//
     -              'StemTopBark Number null,'//
     -              'Stump_Branches_Tip Number null,'//
     -              'Branches_Tip Number null,'//
     -              'AboveStumpWoodBark Number null,'//
     -              'Wood_Bark_to_min_Branch Number null,'//
     -              'BranchesLive_1_plus Number null)'

        ELSE
          SQLStmtStr='CREATE TABLE FVS_CutBiocmp('//
     -              'CaseID char(36),'//
     -              'StandID char(26),'//
     -              'Year int null,'//
     -              'TreeId char(8) null,'//
     -              'TreeIndex int null,'//
     -              'Species char(8) null,'//
     -              'TreeVal int null,'//
     -              'SSCD int null,'//
     -              'PtIndex int null,'//
     -              'TPA real null,'//
     -              'MortPA real null,'//
     -              'DBH real null,'//
     -              'Ht real null,'//
     -              'PctCr int null,'//
     -              'CrWidth real null,'//
     -              'ABT real null,'//
     -              'Wood real null,'//
     -              'Bark real null,'//
     -              'Foliage real null,'//
     -              'Roots real null,'//
     -              'Branches real null,'//
     -              'CompleteTree real null,'//
     -              'WholeTreeAboveGround real null,'//
     -              'WholeTreeAboveStump real null,'//
     -              'StemWood real null,'//
     -              'StemBark real null,'//
     -              'StemTotal real null,'//
     -              'StemTop real null,'//
     -              'BranchesLive real null,'//
     -              'BranchesLive_0_1 real null,'//
     -              'BranchesLive_1_3 real null,'//
     -              'BranchesLive_3_plus real null,'//
     -              'BranchesDead real null,'//
     -              'BranchesTotal real null,'//
     -              'StemBranchesBarkOnly real null,'//
     -              'StemBranchesWoodOnly real null,'//
     -              'StemBranchesLive real null,'//
     -              'StemBranchesTotal real null,'//
     -              'FoliageTotal real null,'//
     -              'FoliageNew real null,'//
     -              'FoliageOld real null,'//
     -              'TwigsTotal real null,'//
     -              'TwigsOld real null,'//
     -              'FoliageTwigs real null,'//
     -              'Crown real null,'//
     -              'RootsCoarse real null,'//
     -              'CoarseStumpRoots real null,'//
     -              'CoarseLateralRoots real null,'//
     -              'FineRoots real null,'//
     -              'RootsTotal real null,'//
     -              'StumpWood real null,'//
     -              'StumpBark real null,'//
     -              'StumpTotal real null,'//
     -              'Stump_Roots real null,'//
     -              'Cones real null,'//
     -              'LiveCrown real null,'//
     -              'DeadCrown real null,'//
     -              'SmallBranches real null,'//
     -              'merchStemTotal real null,'//
     -              'merchStemWood real null,'//
     -              'merchStemBark real null,'//
     -              'StemTopWood real null,'//
     -              'StemTopBark real null,'//
     -              'Stump_Branches_Tip real null,'//
     -              'Branches_Tip real null,'//
     -              'AboveStumpWoodBark real null,'//
     -              'Wood_Bark_to_min_Branch real null,'//
     -              'BranchesLive_1_plus real null)'
        ENDIF
C        PRINT*, SQLStmtStr
        !Close Cursor
            iRet = fvsSQLCloseCursor(StmtHndlOut)
            iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -            int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
            CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -           'DBSBMCMP:Creating Table: '//trim(SQLStmtStr))
      ENDIF
      DO ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.NE.0) THEN
        I2=ISCT(ISPC,2)
        DO I3=I1,I2
        I=IND1(I3)
        P=REMP(I)/GROSPC
        IF (P.LE.0.0) GOTO 50
        DP = 0.
C
C       TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C       GENERATED THROUGH THE ESTAB SYSTEM.
C
        IF (IDTREE(I) .GT. IDCMP1) THEN
          IF (IDTREE(I) .GT. IDCMP2) THEN
            WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
          ELSE
            WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
          ENDIF
        ELSE
          WRITE(TID,'(I8)') IDTREE(I)
        ENDIF
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
        IF(ISPOUT38.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
        IF(ISPOUT38.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
        IF(ISPOUT38.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
C
        IT=I
C
C       COPY INPUT VARIABLES TO DOUBLE-PRECISION
C
        DO J=1,47
          VAR(J) = BMCMPCUT(I,J)
          IF(VAR(J).LT.1.E-10)VAR(J)=0.
        ENDDO
        DO J=1,6
          JENKTREB(J)=JENKTRE(I,J)
          IF(JENKTREB(J).LT.1.E-10)JENKTREB(J)=0.
        ENDDO
        PB=P
        DPB=DP
        DBHB=DBH(I)
        HTB=HT(I)
        CWB=CRWDTH(I)
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
        ColNumber=1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),IYR,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),IT,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),IMC(I),int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),ISPECL(I),int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),ITRE(I),int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),PB,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),DPB,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),DBHB,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),HTB,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),ICR(I),int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
        ColNumber=ColNumber+1
        iRet =fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,
     -            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -             INT(0,SQLSMALLINT_KIND),CWB,int(4,SQLLEN_KIND),
     -             SQL_NULL_PTR)
C
        DO J=1,6
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),JENKTREB(J),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        ENDDO
        DO J=1,47
        ColNumber=ColNumber+1
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,
     -    SQL_PARAM_INPUT,
     -    SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),
     -    INT(5,SQLSMALLINT_KIND),VAR(J),int(4,SQLLEN_KIND),
     -           SQL_NULL_PTR)
        ENDDO
        WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(CaseID,StandID,',
     -           'Year,TreeId,TreeIndex,Species,TreeVal,SSCD,',
     -           'PtIndex,TPA,MortPA,DBH,HT,PctCr,CrWidth,',
     -           'ABT,Wood,Bark,Foliage,Roots,Branches,',
     -           'CompleteTree,WholeTreeAboveGround,',
     -           'WholeTreeAboveStump,StemWood,',
     -           'StemBark,StemTotal,StemTop,',
     -           'BranchesLive,BranchesLive_0_1,BranchesLive_1_3,',
     -           'BranchesLive_3_plus,BranchesDead,BranchesTotal,',
     -           'StemBranchesBarkOnly,StemBranchesWoodOnly,',
     -           'StemBranchesLive,StemBranchesTotal,',
     -           'FoliageTotal,FoliageNew,FoliageOld,',
     -           'TwigsTotal,TwigsOld,FoliageTwigs,',
     -           'Crown,RootsCoarse,CoarseStumpRoots,',
     -           'CoarseLateralRoots,FineRoots,RootsTotal,',
     -           'StumpWood,StumpBark,StumpTotal,',
     -           'Stump_Roots,Cones,LiveCrown,',
     -           'DeadCrown,SmallBranches,merchStemTotal,',
     -           'merchStemWood,merchStemBark,StemTopWood,',
     -           'StemTopBark,Stump_Branches_Tip,Branches_Tip,',
     -           'AboveStumpWoodBark,Wood_Bark_to_min_Branch,',
     -           'BranchesLive_1_plus) VALUES(''',CASEID,''',''',
     -            TRIM(NPLT),''',?,''',ADJUSTL(TID),''',?,''',
     -            TRIM(CSPECIES),''',?,?,
     -            ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,
     -            ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,
     -            ?,?,?,?,?,?,?,?,?,?,?)'
C
        iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),
     -                int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
C      print*,'SQLStmtStr= ',SQLStmtStr
C      print*,' in CUT iRet= ',iRet
        iRet = fvsSQLCloseCursor(StmtHndlOut)
        iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),
     -         int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
         IF (iRet.NE.SQL_SUCCESS) IFMBMCMPCUT=0
         CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,
     -    'DBSBMCMP:Inserting Row: '//trim(SQLStmtStr))
C
  50    CONTINUE
        ENDDO                 ! END OF TREE LOOP
      ENDIF
      ENDDO                   ! END OF SPECIES LOOP
  100 CONTINUE
C
      !Release statement handle
      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

      END


