      SUBROUTINE DBSRD1(IYEAR,CNPLT,IAGE,DTYPE,NCENT,DAREA,SRATE,
     -  STUTPA,STUBA,MRTTPA,MRTCUFT,UNTPA,INTPA,PCTROOT,
     -  MRCUFT,DABA,NEWIN,NEWEXP,NEWTOT)

      IMPLICIT NONE
C
C METRIC-DBSQLITE $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST ROOT DISEASE MODEL REPORT,
C              SUMMARY STATISTICS FOR ROOT DISEASE AREAS (PER ACRE BASED ON
C              DISEASE AREA ONLY).
C     AUTH: L. DAVID -- FMSC (METI) -- 12/30/2014
C
C     ARGUMENT LIST
C      1: IYEAR   -- YEAR
C      2: NPLT    -- STAND ID
C      3: IAGE    -- STAND AGE
C      4: DTYPE   -- DISEASE TYPE 
C      5: NCENT   -- NUMBER OF DISEASE CENTERS
C      6: DAREA   -- DISEASE AREA (ACRES)
C      7: SRATE   -- SPREAD RATE FT/YR
C      8: STUTPA  -- STUMPS/ACRE, DISEASE AREA ONLY
C      9: STUBA   -- STUMP BA/ACRE, DISEASE AREA ONLY
C     10: MRTTPA  -- TPA KILLED, DISEASE AREA ONLY
C     11: MRTCUFT -- CUFT/ACRE KILLED, DISEASE AREA ONLY
C     12: UNTPA   -- UNINFECTED LIVE TPA IN DISEASE AREA
C     13: INTPA   -- INFECTED LIVE TPA IN DISEASE AREA
C     14: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED
C     15: MRCUFT  -- TOTAL MERCH CUFT IN DISEASE AREA
C     16: DABA    -- LIVE BA/ACRE IN DISEASE AREA
C     17: NEWIN   -- NEWLY INFECTED PROPORTION WITHIN DISEASE AREA
C     18: NEWEXP  -- NEWLY INFECTED PROPORTION DUE TO AREA EXPANSION
C     19: NEWTOT  -- NEWLY INFECTED PROPORTION OF TOTAL STAND

      INCLUDE 'DBSCOM.F77'

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS 
#endif
      
C     ARGUMENT LIST

      INTEGER IYEAR, IAGE, NCENT
      REAL    DAREA, SRATE, STUTPA, STUBA, MRTTPA, MRTCUFT, UNTPA,
     -        INTPA, PCTROOT, MRCUFT, DABA, NEWIN, NEWEXP, NEWTOT
      CHARACTER(LEN=1)  DTYPE
      CHARACTER(LEN=26) CNPLT
      INTEGER iRet

C     LOCAL VARIABLES

      INTEGER ColNumber

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      CHARACTER*2000    SQLStmtStr

C     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
C     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
C
      DOUBLE PRECISION 
     -  DAREAD, SRATED, STUTPAD, STUBAD, MRTTPAD, MRTCUFTD, UNTPAD,
     -  INTPAD, PCTROOTD, MRCUFTD, DABAD, NEWIND, NEWEXPD, NEWTOTD

C     If RD Summary not selected for DB output, return.
C      WRITE(23,*) "IN DBSRD1: IRD1 = ",IRD1               ! DEBUG

      IF(IRD1 .EQ. 0) RETURN

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_RD_Sum_Metric"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Sum_Metric ('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year Int null,'//
     -      'Age Int null,'//
     -      'RD_Type char(1) null,'//
     -      'Num_Centers Int null,'//
     -      'RD_Area real null,'//
     -      'Spread_M_per_Year real null,'//
     -      'Stumps_per_Ha real null,'//
     -      'Stumps_BA real null,'//
     -      'Mort_TPH real null,'//
     -      'Mort_CuM real null,'//
     -      'UnInf_TPH real null,'//
     -      'Inf_TPH real null,'//
     -      'Ave_Pct_Root_Inf real null,'//
     -      'Live_Merch_CuM real null,'//
     -      'Live_BA real null,'//
     -      'New_Inf_Prp_Ins real null,'//
     -      'New_Inf_Prp_Exp real null,'//
     -      'New_Inf_Prp_Tot real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          IRD1 = 0
          RETURN
        ENDIF
      ENDIF

C     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
C     WRITE RECORD TO DATABASE

      DAREAD   = DAREA
      SRATED   = SRATE
      STUTPAD  = STUTPA
      STUBAD   = STUBA
      MRTTPAD  = MRTTPA
      MRTCUFTD = MRTCUFT
      UNTPAD   = UNTPA
      INTPAD   = INTPA
      PCTROOTD = PCTROOT
      MRCUFTD  = MRCUFT
      DABAD    = DABA
      NEWIND   = NEWIN
      NEWEXPD  = NEWEXP
      NEWTOTD  = NEWTOT

C     DEFINE DATABASE INSERT STATEMENT

      WRITE(SQLStmtStr,*) 'INSERT INTO FVS_RD_Sum_Metric',
     -  ' (CaseID,StandID,Year,Age,RD_Type,Num_Centers,RD_Area,',
     -  'Spread_M_per_Year,Stumps_per_Ha,Stumps_BA,',
     -  'Mort_TPH,Mort_CuM,UnInf_TPH,Inf_TPH,Ave_Pct_Root_Inf,',
     -  'Live_Merch_CuM,Live_BA,',
     -  'New_Inf_Prp_Ins,New_Inf_Prp_Exp,New_Inf_Prp_Tot) ',
     -  ' VALUES (''',CASEID,''',''',TRIM(CNPLT),''',
     -  ?,?,''',DTYPE,''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IRD1 = 0
         RETURN
      ENDIF

      ColNumber=1                 ! YEAR
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1       ! AGE
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IAGE)

      ColNumber=ColNumber+1       ! NUMBER OF DISEASE CENTERS
      iRet=fsql3_bind_int(IoutDBref,ColNumber,NCENT)

      ColNumber=ColNumber+1       ! DISEASE AREA IN ACRES
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DAREAD)

      ColNumber=ColNumber+1       ! SPREAD RATE FEET PER YEAR
      iRet=fsql3_bind_double(IoutDBref,ColNumber,SRATED)

      ColNumber=ColNumber+1       ! DEAD STUMPS PER ACRE
      iRet=fsql3_bind_double(IoutDBref,ColNumber,STUTPAD)

      ColNumber=ColNumber+1       ! STUMPS BA SQFT PER ACRE
      iRet=fsql3_bind_double(IoutDBref,ColNumber,STUBAD)

      ColNumber=ColNumber+1       ! TREES KILLED TPA
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTTPAD)

      ColNumber=ColNumber+1       ! TREES KILLED CUFT PER ACRE
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTCUFTD)

      ColNumber=ColNumber+1       ! UNINFECTED LIVE TREES TPA
      iRet=fsql3_bind_double(IoutDBref,ColNumber,UNTPAD)

      ColNumber=ColNumber+1       ! INFECTED LIVE TREES TPA
      iRet=fsql3_bind_double(IoutDBref,ColNumber,INTPAD)

      ColNumber=ColNumber+1       ! AVERAGE % ROOTS INFECTED
      iRet=fsql3_bind_double(IoutDBref,ColNumber,PCTROOTD)

      ColNumber=ColNumber+1       ! LIVE MERCH CUFT PER ACRE
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRCUFTD)

      ColNumber=ColNumber+1       ! LIVE BASAL AREA PER ACRE
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DABAD)

      ColNumber=ColNumber+1       ! NEWLY INFECTED PROP INSIDE DIS AREA
      iRet=fsql3_bind_double(IoutDBref,ColNumber,NEWIND)

      ColNumber=ColNumber+1       ! NEWLY INFECTED PROP DUE TO EXPANSION
      iRet=fsql3_bind_double(IoutDBref,ColNumber,NEWEXPD)

      ColNumber=ColNumber+1       ! NEWLY INFECTED PROPORTION TOTAL
      iRet=fsql3_bind_double(IoutDBref,ColNumber,NEWTOTD)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IRD1 = 0
      ENDIF
      RETURN
      END

C-------------------------------------------------------------------------------

      SUBROUTINE DBSRD2(IYEAR,CNPLT,DTYPE,DAREA,CSP,DDBHCL,
     -  DTPA,LDBHCL,LUNTPA,LINTPA,PCTROOT)

      IMPLICIT NONE
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND ROOT DISEASE MODEL REPORT,
C              DBH PERCENTILE DETAIL FOR ROOT DISEASE AREAS (PER ACRE BASED ON
C              DISEASE AREA ONLY).
C     AUTH: L. DAVID -- FMSC (METI) -- 01/06/2015
C
C     ARGUMENT LIST
C      1: IYEAR   -- YEAR
C      2: NPLT    -- STAND ID
C      3: DTYPE   -- DISEASE TYPE 
C      4: DAREA   -- DISEASE AREA (ACRES)
C      5: CSP     -- TREE SPECIES CHARACTER ABBREVIATION
C      6: DDBHCL  -- DEAD %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      7: DTPA    -- DEAD TREES/ACRE TOTAL, DISEASE AREA ONLY
C      8: LDBHCL  -- LIVE %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      9: LUNTPA  -- LIVE UNINFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     10: LINTPA  -- LIVE INFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     11: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'DBSCOM.F77'

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS 
#endif      

C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    DAREA, DDBHCL(6), DTPA, LDBHCL(6), LUNTPA, LINTPA,
     -        PCTROOT
      CHARACTER(LEN=1)  DTYPE
      CHARACTER(LEN=2)  CSP
      CHARACTER(LEN=26) CNPLT
      INTEGER iRet

C     LOCAL VARIABLES

      CHARACTER(LEN=8)  CSP1,CSP2,CSP3
      INTEGER ColNumber,I,I2

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      CHARACTER*2000    SQLStmtStr

C     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
C     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
C
      DOUBLE PRECISION 
     -  DAREAD, DDBHCLD(6), DTPAD, LDBHCLD(6), LUNTPAD, LINTPAD,
     -  PCTROOTD

C     If RD Detail not selected for DB output, return.

      IF(IRD2 .EQ .0) RETURN
      
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_RD_Det_Metric"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Det_Metric ('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year Int null,'//
     -      'RD_Type char(1) not null,'//
     -      'RD_Area real null,'//
     -      'SpeciesFVS text not null,'//
     -      'SpeciesPLANTS text not null,'//
     -      'SpeciesFIA text not null,'//
     -      'Mort_10Pctile_DBH real null,'//
     -      'Mort_30Pctile_DBH real null,'//
     -      'Mort_50Pctile_DBH real null,'//
     -      'Mort_70Pctile_DBH real null,'//
     -      'Mort_90Pctile_DBH real null,'//
     -      'Mort_100Pctile_DBH real null,'//
     -      'Mort_TPH_Total real null,'//
     -      'Live_10Pctile_DBH real null,'//
     -      'Live_30Pctile_DBH real null,'//
     -      'Live_50Pctile_DBH real null,'//
     -      'Live_70Pctile_DBH real null,'//
     -      'Live_90Pctile_DBH real null,'//
     -      'Live_100Pctile_DBH real null,'//
     -      'UnInf_TPH_Total real null,'//
     -      'Inf_TPH_Total real null,'//
     -      'Pct_Roots_Inf real null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           IRD2 = 0
           RETURN
         ENDIF
      ENDIF

C     ASSIGN FVS, PLANTS AND FIA SPECIES CODE
C
      DO I2 = 1,MAXSP
        IF (CSP .EQ. JSP(I2)) THEN
          CSP1 = JSP(I2)
          CSP2 = PLNJSP(I2)
          CSP3 = FIAJSP(I2)
        ENDIF
      ENDDO
      
C     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
C     WRITE RECORD TO DATABASE

      DAREAD   = DAREA
      DTPAD    = DTPA
      LUNTPAD  = LUNTPA
      LINTPAD  = LINTPA
      PCTROOTD = PCTROOT
      DO I=1,6
        DDBHCLD(I) = DDBHCL(I)
        LDBHCLD(I) = LDBHCL(I)
      END DO

C     DEFINE DATABASE INSERT STATEMENT

      WRITE(SQLStmtStr,*) 'INSERT INTO FVS_RD_Det_Metric',
     -  ' (CaseID,StandID,Year,RD_Type,RD_Area,',
     -  'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',
     -  'Mort_10Pctile_DBH,Mort_30Pctile_DBH,Mort_50Pctile_DBH,',
     -  'Mort_70Pctile_DBH,Mort_90Pctile_DBH,Mort_100Pctile_DBH,',
     -  'Mort_TPH_Total,Live_10Pctile_DBH,Live_30Pctile_DBH,',
     -  'Live_50Pctile_DBH,Live_70Pctile_DBH,Live_90Pctile_DBH,',
     -  'Live_100Pctile_DBH,UnInf_TPH_Total,Inf_TPH_Total,',
     -  'Pct_Roots_Inf) ',
     -  " VALUES ('",CASEID,"','",TRIM(CNPLT),"',?,'",DTYPE,"'",
     -  ",?,'",TRIM(CSP1),"','",TRIM(CSP2),"','",TRIM(CSP3),"'",
     -  ",?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IRD2 = 0
         RETURN
      ENDIF

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1                 ! YEAR
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1       ! DISEASE AREA IN ACRES
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DAREAD)

      ColNumber=ColNumber+1       ! Mortality 10 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(1))

      ColNumber=ColNumber+1       ! Mortality 30 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(2))

      ColNumber=ColNumber+1       ! Mortality 50 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(3))

      ColNumber=ColNumber+1       ! Mortality 70 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(4))

      ColNumber=ColNumber+1       ! Mortality 90 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(5))

      ColNumber=ColNumber+1       ! Mortality 100 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(6))

      ColNumber=ColNumber+1       ! Mortality TPA total
      iRet=fsql3_bind_double(IoutDBref,ColNumber,DtpaD)

      ColNumber=ColNumber+1       ! Live trees 10 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(1))

      ColNumber=ColNumber+1       ! Live trees 30 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(2))

      ColNumber=ColNumber+1       ! Live trees 50 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(3))

      ColNumber=ColNumber+1       ! Live trees 70 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(4))

      ColNumber=ColNumber+1       ! Live trees 90 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(5))

      ColNumber=ColNumber+1       ! Live trees 100 percentile DBH
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(6))

      ColNumber=ColNumber+1       ! Live uninfected TPA total
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LUNTPAD)

      ColNumber=ColNumber+1       ! Live infected TPA total
      iRet=fsql3_bind_double(IoutDBref,ColNumber,LINTPAD)

      ColNumber=ColNumber+1       ! AVERAGE % ROOTS INFECTED
      iRet=fsql3_bind_double(IoutDBref,ColNumber,PCTROOTD)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IRD2 = 0
      ENDIF
      RETURN

      END


C-------------------------------------------------------------------------------

      SUBROUTINE DBSRD3(IYEAR, CNPLT, CSP, MRTININ, ININTOT, ININLIV,
     -  MRTINUN, INUNTOT, INUNLIV, MRTOUT, OUTTOT, OUTLIV, STDMRT)

      IMPLICIT NONE
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND ROOT DISEASE MODEL REPORT,
C              DBH PERCENTILE DETAIL FOR ROOT DISEASE AREAS (PER ACRE BASED ON
C              DISEASE AREA ONLY).
C     AUTH: L. DAVID -- FMSC (METI) -- 01/06/2015
C
C     ARGUMENT LIST
C      1: IYEAR   -- YEAR
C      2: NPLT    -- STAND ID
C      3: DTYPE   -- DISEASE TYPE 
C      4: DAREA   -- DISEASE AREA (ACRES)
C      5: CSP     -- TREE SPECIES CHARACTER ABBREVIATION
C      6: DDBHCL  -- DEAD %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      7: DTPA    -- DEAD TREES/ACRE TOTAL, DISEASE AREA ONLY
C      8: LDBHCL  -- LIVE %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
C      9: LUNTPA  -- LIVE UNINFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     10: LINTPA  -- LIVE INFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
C     11: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'DBSCOM.F77'

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS 
#endif      


C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    MRTININ(7), MRTINUN(7), MRTOUT(7),
     -        ININTOT, INUNTOT, OUTTOT, STDMRT,
     -        ININLIV, INUNLIV, OUTLIV
      CHARACTER(LEN=2)  CSP
      CHARACTER(LEN=26) CNPLT
      INTEGER iRet

C     LOCAL VARIABLES

      CHARACTER(LEN=8)  CSP1,CSP2,CSP3
      INTEGER ColNumber,I,I2

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      CHARACTER*2000    SQLStmtStr

C     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
C     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
C
      DOUBLE PRECISION 
     -  MRTININD(7), MRTINUND(7), MRTOUTD(7), 
     -  ININTOTD, INUNTOTD, OUTTOTD, STDMRTD,
     -  ININLIVD, INUNLIVD, OUTLIVD

C     If RD Bark Beetle not selected for DB output, return.

      IF(IRD3 .EQ. 0) RETURN

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_RD_Beetle_Metric"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_RD_Beetle_Metric ('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year Int null,'//
     -      'SpeciesFVS         text not null,'//
     -      'SpeciesPLANTS      text not null,'//
     -      'SpeciesFIA         text not null,'//
     -      'In_Inf_0_127_DBH       real null,'//
     -      'In_Inf_127_254_DBH     real null,'//
     -      'In_Inf_254_381_DBH     real null,'//
     -      'In_Inf_381_508_DBH     real null,'//
     -      'In_Inf_508_635_DBH     real null,'//
     -      'In_Inf_635_762_DBH     real null,'//
     -      'In_Inf_762_DBH         real null,'//
     -      'In_Inf_Mort            real null,'//
     -      'In_Inf_Live_before     real null,'//
     -      'In_UnInf_0_127_DBH     real null,'//
     -      'In_UnInf_127_254_DBH   real null,'//
     -      'In_UnInf_254_381_DBH   real null,'//
     -      'In_UnInf_381_508_DBH   real null,'//
     -      'In_UnInf_508_635_DBH   real null,'//
     -      'In_UnInf_635_762_DBH   real null,'//
     -      'In_UnInf_762_DBH       real null,'//
     -      'In_UnInf_Mort          real null,'//
     -      'In_UnInf_Live_Before   real null,'//
     -      'Outside_0_127_DBH      real null,'//
     -      'Outside_127_254_DBH    real null,'//
     -      'Outside_254_381_DBH    real null,'//
     -      'Outside_381_508_DBH    real null,'//
     -      'Outside_508_635_DBH    real null,'//
     -      'Outside_635_762_DBH    real null,'//
     -      'Outside_762_DBH        real null,'//
     -      'Outside_Mort           real null,'//
     -      'Outside_Live_Before    real null,'//
     -      'Stand_Mort_Total       real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          IRD3 = 0
          RETURN
        ENDIF
      ENDIF

C     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

      DO I2 = 1,MAXSP
        IF (CSP .EQ. JSP(I2)) THEN
          CSP1 = JSP(I2)
          CSP2 = PLNJSP(I2)
          CSP3 = FIAJSP(I2)
        ENDIF
      ENDDO
      
C     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
C     WRITE RECORD TO DATABASE

      DO I=1,7
        MRTININD(I) = MRTININ(I)
        MRTINUND(I) = MRTINUN(I)
        MRTOUTD(I)  = MRTOUT(I)
      END DO

      ININTOTD = ININTOT
      ININLIVD = ININLIV
      INUNTOTD = INUNTOT
      INUNLIVD = INUNLIV
      OUTTOTD  = OUTTOT 
      OUTLIVD  = OUTLIV 
      STDMRTD  = STDMRT 

C     DEFINE DATABASE INSERT STATEMENT

      WRITE(SQLStmtStr,*) 'INSERT INTO FVS_RD_Beetle_Metric',
     -  ' (CaseID,StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',
     -  'In_Inf_0_127_DBH,In_Inf_127_254_DBH,In_Inf_254_381_DBH,',
     -  'In_Inf_381_508_DBH,In_Inf_508_635_DBH,In_Inf_635_762_DBH,',
     -  'In_Inf_762_DBH,In_Inf_Mort,In_Inf_Live_before,',
     -  'In_UnInf_0_127_DBH,In_UnInf_127_254_DBH,In_UnInf_254_381_DBH,',
     -  'In_UnInf_381_508_DBH,In_UnInf_508_635_DBH,',
     -  'In_UnInf_635_762_DBH,',
     -  'In_UnInf_762_DBH,In_UnInf_Mort,In_UnInf_Live_Before,',
     -  'Outside_0_127_DBH,Outside_127_254_DBH,Outside_254_381_DBH,',
     -  'Outside_381_508_DBH,Outside_508_635_DBH,Outside_635_762_DBH,',
     -  'Outside_762_DBH,Outside_Mort,Outside_Live_Before,',
     -  'Stand_Mort_Total) ',
     -  " VALUES (","'",CASEID,"','",TRIM(CNPLT),"',?",
     -  ",'",TRIM(CSP1),"','",TRIM(CSP2),"','",TRIM(CSP3),"',",
     -  "?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IRD3= 0
         RETURN
      ENDIF

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1                 ! YEAR
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1       ! INSIDE INFECTED 0-5
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(1))

      ColNumber=ColNumber+1       ! INSIDE INFECTED 5-10
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(2))

      ColNumber=ColNumber+1       ! INSIDE INFECTED 10-15
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(3))

      ColNumber=ColNumber+1       ! INSIDE INFECTED 15-20
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(4))

      ColNumber=ColNumber+1       ! INSIDE INFECTED 20-25
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(5))

      ColNumber=ColNumber+1       ! INSIDE INFECTED 25-30
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(6))

      ColNumber=ColNumber+1       ! INSIDE INFECTED 30+
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(7))

      ColNumber=ColNumber+1       ! INSIDE INFECTED TOTAL
      iRet=fsql3_bind_double(IoutDBref,ColNumber,ININTOTD)

      ColNumber=ColNumber+1       ! INSIDE INFECTED LIVE BEFORE ATTACK
      iRet=fsql3_bind_double(IoutDBref,ColNumber,ININLIVD)

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 0-5
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(1))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 5-10
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(2))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 10-15
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(3))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 15-20
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(4))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 20-25
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(5))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 25-30
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(6))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED 30+
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(7))

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED TOTAL
      iRet=fsql3_bind_double(IoutDBref,ColNumber,INUNTOTD)

      ColNumber=ColNumber+1       ! INSIDE UNINFECTED LIVE BEFORE ATTACK
      iRet=fsql3_bind_double(IoutDBref,ColNumber,INUNLIVD)

      ColNumber=ColNumber+1       ! OUTSIDE MORT 0-5
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(1))

      ColNumber=ColNumber+1       ! OUTSIDE MORT 5-10
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(2))

      ColNumber=ColNumber+1       ! OUTSIDE MORT 10-15
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(3))

      ColNumber=ColNumber+1       ! OUTSIDE MORT 15-20
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(4))

      ColNumber=ColNumber+1       ! OUTSIDE MORT 20-25
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(5))

      ColNumber=ColNumber+1       ! OUTSIDE MORT 25-30
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(6))

      ColNumber=ColNumber+1       ! OUTSIDE MORT 30+
      iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(7))

      ColNumber=ColNumber+1       ! OUTSIDE MORT TOTAL
      iRet=fsql3_bind_double(IoutDBref,ColNumber,OUTTOTD)

      ColNumber=ColNumber+1       ! OUTSIDE LIVE BEFORE ATTACK
      iRet=fsql3_bind_double(IoutDBref,ColNumber,OUTLIVD)

      ColNumber=ColNumber+1       ! STAND MORT TOTAL
      iRet=fsql3_bind_double(IoutDBref,ColNumber,STDMRTD)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IRD3 = 0
      ENDIF
      RETURN
      END
