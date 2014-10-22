      SUBROUTINE DBSSTANDIN(SQLSTR,LKECHO)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE FVS STAND LEVEL DATA FROM THE DATABASE
C     AUTH: D. GAMMEL -- SEM -- AUGUST 2002
C     OVERHAUL: NL CROOKTON -- RMRS MOSCOW -- SEPTEMBER 2004
C---
COMMONS
C
C
      INCLUDE  'PRGPRM.F77'
C
C
      INCLUDE  'ARRAYS.F77'
C
C
      INCLUDE  'COEFFS.F77'
C
C
      INCLUDE  'CONTRL.F77'
C
C
      INCLUDE  'PLOT.F77'
C
C
      INCLUDE  'OUTCOM.F77'
C
C
      INCLUDE  'HTCAL.F77'
C
C
      INCLUDE  'ECON.F77'
C
C
      INCLUDE  'KEYCOM.F77'
C
C
      INCLUDE  'MULTCM.F77'
C
C
      INCLUDE  'VOLSTD.F77'
C
C
      INCLUDE  'SCREEN.F77'
C
C
      INCLUDE  'VARCOM.F77'
C
C
      INCLUDE  'DBSCOM.F77'
C
COMMONS

      CHARACTER*100 ColName
      CHARACTER*5000 SQLSTR
      CHARACTER*10 KARD2
      CHARACTER(LEN=11) CHAB,CECOREG
      CHARACTER(LEN=14) CFotoCode
      CHARACTER(LEN=LEN(DBCN)+1) TMP_DBCN
      CHARACTER(LEN=LEN(NPLT)+1) CSTAND
      CHARACTER*7 VVER
      CHARACTER*9 CSITECODE
      CHARACTER*40 PHOTOREF(32), REF
      REAL ARRAY2,X(1)
      REAL XXG,FOTODATA(2)
      REAL   (KIND=4) RSTANDDATA(63)
      REAL DUM1,XTMP
      INTEGER(KIND=4) ISTANDDATA(63)
      EQUIVALENCE (RSTANDDATA,ISTANDDATA)
      INTEGER J,I,KODE, FKOD,NUMPVREF,IXTMP,IXF
      INTEGER(SQLUSMALLINT_KIND)::ColNumber
      INTEGER(SQLSMALLINT_KIND)::NameLen,ColumnCount,DType,
     -       NDecs, Nullable
      INTEGER(SQLULEN_KIND) NColSz
      LOGICAL LSITEISNUM,LHABISNUM,LSTDISNUM,LFMLK,LFMYES,LKECHO,LFMD
      LOGICAL LFOTO, LFOTO2, LECOISNUM, LFMYES2
      INTEGER(SQLLEN_KIND)::tmpNotUsed
      INTEGER(SQLLEN_KIND)::IY_LI,Lat_LI,Long_LI,Location_LI,
     -        Habitat_LI,Age_LI,Aspect_LI,Slope_LI,MaxSDI_LI,
     -        Elev_LI,Basal_LI,PlotArea_LI,BPDBH_LI,NumPlots_LI,
     -        NonStock_LI,SamWt_LI,Stock_LI,DGT_LI,DGM_LI,HTT_LI,HTM_LI,
     -        SiteSp_LI,SiteIndx_LI,Mort_LI,MaxB_LI,Model_LI,PhysioR_LI,
     -        ForType_LI,Stand_LI,DBCN_LI,Region_LI,Forest_LI,
     -        District_LI,Compartment_LI,Ecoregion_LI,ElevFT_LI,
     -        State_LI,Connty_LI,Fuel0_LI,Fuel1_LI,Fuel3_LI,
     -        Fuel6_LI,Fuel12_LI,FuelLt_LI,FuelDf_LI,FuelModel_LI,
     -        Fuel025_LI,Fuel251_LI,Fuel20_LI,Fuel35_LI,Fuel50_LI,
     -        FotoRef_LI,FotoCode_LI,PvRefCode_LI,
     -        FuelS025_LI,FuelS251_LI,FuelS1_LI,FuelS3_LI,FuelS6_LI,
     -        FuelS12_LI,FuelS20_LI,FuelS35_LI,FuelS50_LI

      DATA PHOTOREF / 'Fischer INT-96                      ',
     >                'Fischer INT-97                      ',
     >                'Fischer INT-98                      ',
     >                '                                    ',
     >                'Koski and Fischer INT-46            ',
     >                'Maxwell and Ward PNW-52             ',
     >                'Blonski and Schramel PSW-56         ',
     >                'Maxwell and Ward PNW-105            ',
     >                'Ottmar and Hardy PNW-GTR-231        ',
     >                '                                    ',
     >                'Maxwell A-89-6-82                   ',
     >                'Southwestern region compilation     ',
     >                'Maxwell and Ward PNW-51             ',
     >                'Ottmar and others Volume I          ',
     >                'Ottmar and others Volume I          ',
     >                'Ottmar and Vihnanek Volume II / IIa ',
     >                'Ottmar and others Volume III        ',
     >                'Ottmar and others Volume V / Va     ',
     >                'Ottmar and others Volume VI / VIa   ',
     >                'Maxwell A-89-1-90                   ',
     >                'Ottmar and others Volume IV         ',
     >                'Wright and others PNW-GTR-545       ',
     >                'Ottmar and others PNW-GTR-258       ',
     >                'Lynch and Horton NA-FR-25           ',
     >                'Wilcox and others NA-FR-22          ',
     >                'Scholl and Waldrop GTR-SRS-26       ',
     >                'Ottmar and others Volume VII        ',
     >                'Maxwell and Ward PNW-95             ',
     >                'Sanders and Van Lear GTR-SE-49      ',
     >                'Wade and others GTR-SE-82           ',
     >                'Blank GTR-NC-77                     ',
     >                'Popp and Lundquist RMRS-GTR-172     ' /
      IY_LI          = SQL_NULL_DATA
      Lat_LI         = SQL_NULL_DATA
      Long_LI        = SQL_NULL_DATA
      Location_LI    = SQL_NULL_DATA
      Habitat_LI     = SQL_NULL_DATA
      Age_LI         = SQL_NULL_DATA
      Aspect_LI      = SQL_NULL_DATA
      Slope_LI       = SQL_NULL_DATA
      MaxSDI_LI      = SQL_NULL_DATA
      Elev_LI        = SQL_NULL_DATA
      Basal_LI       = SQL_NULL_DATA
      PlotArea_LI    = SQL_NULL_DATA
      BPDBH_LI       = SQL_NULL_DATA
      NumPlots_LI    = SQL_NULL_DATA
      NonStock_LI    = SQL_NULL_DATA
      SamWt_LI       = SQL_NULL_DATA
      Stock_LI       = SQL_NULL_DATA
      DGT_LI         = SQL_NULL_DATA
      DGM_LI         = SQL_NULL_DATA
      HTT_LI         = SQL_NULL_DATA
      HTM_LI         = SQL_NULL_DATA
      SiteSp_LI      = SQL_NULL_DATA
      SiteIndx_LI    = SQL_NULL_DATA
      Mort_LI        = SQL_NULL_DATA
      MaxB_LI        = SQL_NULL_DATA
      Model_LI       = SQL_NULL_DATA
      PhysioR_LI     = SQL_NULL_DATA
      ForType_LI     = SQL_NULL_DATA
      Stand_LI       = SQL_NULL_DATA
      DBCN_LI        = SQL_NULL_DATA
      Region_LI      = SQL_NULL_DATA
      Forest_LI      = SQL_NULL_DATA
      District_LI    = SQL_NULL_DATA
      Compartment_LI = SQL_NULL_DATA
      Ecoregion_LI   = SQL_NULL_DATA
      ElevFT_LI      = SQL_NULL_DATA
      State_LI       = SQL_NULL_DATA
      Connty_LI      = SQL_NULL_DATA
      Fuel0_LI       = SQL_NULL_DATA
      Fuel1_LI       = SQL_NULL_DATA
      Fuel3_LI       = SQL_NULL_DATA
      Fuel6_LI       = SQL_NULL_DATA
      Fuel12_LI      = SQL_NULL_DATA
      FuelLt_LI      = SQL_NULL_DATA
      FuelDf_LI      = SQL_NULL_DATA
      Fuel025_LI     = SQL_NULL_DATA
      Fuel251_LI     = SQL_NULL_DATA
      FuelModel_LI   = SQL_NULL_DATA
      PvRefCode_LI   = SQL_NULL_DATA
      FotoRef_LI     = SQL_NULL_DATA
      FotoCode_LI    = SQL_NULL_DATA
      Fuel20_LI      = SQL_NULL_DATA
      Fuel35_LI      = SQL_NULL_DATA
      Fuel50_LI      = SQL_NULL_DATA
      FuelS025_LI    = SQL_NULL_DATA
      FuelS251_LI    = SQL_NULL_DATA
      FuelS1_LI      = SQL_NULL_DATA
      FuelS3_LI      = SQL_NULL_DATA
      FuelS6_LI      = SQL_NULL_DATA
      FuelS12_LI     = SQL_NULL_DATA
      FuelS20_LI     = SQL_NULL_DATA
      FuelS35_LI     = SQL_NULL_DATA
      FuelS50_LI     = SQL_NULL_DATA

C     MAKE SURE WE HAVE AN OPEN CONNECTION

      IF(ConnHndlIn.EQ.-1) CALL DBSOPEN(DSNIN,EnvHndlIn,
     -                          ConnHndlIn,DBMSIN,0,.FALSE.,KODE)

C     ALLOCATE A STATEMENT HANDLE

      iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlIn,StmtHndlIn)
      IF (iRet.NE.SQL_SUCCESS .AND.
     -    iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
        CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlIn,
     -             'STANDIN:DSN Connection')
      ELSE

C       EXECUTE QUERY

        iRet=fvsSQLExecDirect(StmtHndlIn,trim(SQLSTR),
     -            int(len_trim(SQLSTR),SQLINTEGER_KIND))
        IF (iRet.NE.0) CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlIn,
     -             'STANDIN:Query: '//trim(SQLSTR))
      ENDIF

C     GET NUMBER OF COLUMNS RETURNED

      iRet = fvsSQLNumResultCols(StmtHndlIn,ColumnCount)

C     INITIALIZE DATA ARRAY THAT BINDS TO COLUMNS

      DO ColNumber = 1,ColumnCount

        iRet = fvsSQLDescribeCol (StmtHndlIn, ColNumber, ColName,
     -   int(LEN(ColName),SQLSMALLINT_KIND), NameLen, DType,
     -   NColSz, NDecs, Nullable)

        DO I = 1, NameLen
          CALL UPCASE(ColName(I:I))
        END DO

C       BIND COLUMNS TO THEIR VARIABLES

        SELECT CASE(ColName(1:NameLen))

         CASE('STAND_CN')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,
     -           TMP_DBCN,int(LEN(TMP_DBCN),SQLLEN_KIND),
     -           DBCN_LI)

         CASE('STAND_ID')
          IF(DType.EQ.SQL_CHAR.OR.DType.EQ.SQL_VARCHAR.OR.
     -       DType.EQ.SQL_LONGVARCHAR) THEN

            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,
     -        CSTAND,int(LEN(CSTAND),SQLLEN_KIND),Stand_LI)
            LSTDISNUM=.FALSE.
          ELSE
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(28),int(4,SQLLEN_KIND),Stand_LI)
            LSTDISNUM=.TRUE.
          ENDIF

         CASE('INV_YEAR')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(1),int(4,SQLLEN_KIND),IY_LI)

         CASE('LATITUDE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(2),int(4,SQLLEN_KIND),Lat_LI)

         CASE('LONGITUDE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(3),int(4,SQLLEN_KIND),Long_LI)

         CASE('REGION')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(29),int(4,SQLLEN_KIND),Region_LI)

         CASE('FOREST')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(30),int(4,SQLLEN_KIND),Forest_LI)

         CASE('DISTRICT')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(31),int(4,SQLLEN_KIND),District_LI)

         CASE('COMPARTMENT')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(32),int(4,SQLLEN_KIND),Compartment_LI)

         CASE('ECOREGION')
          IF(DType.EQ.SQL_CHAR.OR.DType.EQ.SQL_VARCHAR.OR.
     -       DType.EQ.SQL_LONGVARCHAR) THEN
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,
     -        CECOREG,int(LEN(CECOREG),SQLLEN_KIND),Ecoregion_LI)
            LECOISNUM =.FALSE.
          ELSE
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(54),int(4,SQLLEN_KIND),Ecoregion_LI)
            LECOISNUM=.TRUE.
          ENDIF

         CASE('LOCATION')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(4),int(4,SQLLEN_KIND),Location_LI)

         CASE('HABITAT','PV_CODE')
          IF(DType.EQ.SQL_CHAR.OR.DType.EQ.SQL_VARCHAR.OR.
     -       DType.EQ.SQL_LONGVARCHAR) THEN
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,
     -        CHAB,int(LEN(CHAB),SQLLEN_KIND),Habitat_LI)
            LHABISNUM =.FALSE.
          ELSE
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(5),int(4,SQLLEN_KIND),Habitat_LI)
            LHABISNUM=.TRUE.
          ENDIF

         CASE('PV_REF_CODE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        NUMPVREF,int(4,SQLLEN_KIND),PvRefCode_LI)

         CASE('AGE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(6),int(4,SQLLEN_KIND),Age_LI)

         CASE('ASPECT')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(7),int(4,SQLLEN_KIND),Aspect_LI)

         CASE('SLOPE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(8),int(4,SQLLEN_KIND),Slope_LI)

         CASE('ELEVATION')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(9),int(4,SQLLEN_KIND),Elev_LI)

         CASE('ELEVFT')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(33),int(4,SQLLEN_KIND),ElevFt_LI)

         CASE('BASAL_AREA_FACTOR')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(10),int(4,SQLLEN_KIND),Basal_LI)

         CASE('INV_PLOT_SIZE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(11),int(4,SQLLEN_KIND),PlotArea_LI)

         CASE('BRK_DBH')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(12),int(4,SQLLEN_KIND),BPDBH_LI)

         CASE('NUM_PLOTS')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(13),int(4,SQLLEN_KIND),NumPlots_LI)

         CASE('NONSTK_PLOTS')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(14),int(4,SQLLEN_KIND),NonStock_LI)

         CASE('SAM_WT')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(15),int(4,SQLLEN_KIND),SamWt_LI)

         CASE('STK_PCNT')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(16),int(4,SQLLEN_KIND),Stock_LI)

         CASE('DG_TRANS')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(17),int(4,SQLLEN_KIND),DGT_LI)

         CASE('DG_MEASURE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(18),int(4,SQLLEN_KIND),DGM_LI)

         CASE('HTG_TRANS')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(19),int(4,SQLLEN_KIND),HTT_LI)

         CASE('HTG_MEASURE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(20),int(4,SQLLEN_KIND),HTM_LI)

         CASE('MORT_MEASURE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(21),int(4,SQLLEN_KIND),Mort_LI)

         CASE('SITE_SPECIES')
          IF(DType.EQ.SQL_CHAR.OR.DType.EQ.SQL_VARCHAR.OR.
     -       DType.EQ.SQL_LONGVARCHAR) THEN
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,
     -        CSITECODE,int(LEN(CSITECODE),SQLLEN_KIND),SiteSp_LI)
            LSITEISNUM=.FALSE.
          ELSE
            iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(34),int(4,SQLLEN_KIND),SiteSp_LI)
            LSITEISNUM=.TRUE.
          ENDIF

         CASE('SITE_INDEX')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -      RSTANDDATA(35),int(4,SQLLEN_KIND),SiteIndx_LI)

         CASE('MAX_BA')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(22),int(4,SQLLEN_KIND),MaxB_LI)

         CASE('MAX_SDI')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -      RSTANDDATA(36),int(4,SQLLEN_KIND),MaxSDI_LI)

         CASE('MODEL_TYPE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(25),int(4,SQLLEN_KIND),Model_LI)

         CASE('PHYSIO_REGION')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(26),int(4,SQLLEN_KIND),PhysioR_LI)

         CASE('FOREST_TYPE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(27),int(4,SQLLEN_KIND),ForType_LI)

         CASE('STATE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(37),int(4,SQLLEN_KIND),State_LI)

         CASE('COUNTY')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(38),int(4,SQLLEN_KIND),Connty_LI)

         CASE('FUEL_0_1')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(39),int(4,SQLLEN_KIND), Fuel0_LI)
         CASE('FUEL_1_3','FUEL_1_3_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(40),int(4,SQLLEN_KIND), Fuel1_LI)
         CASE('FUEL_3_6','FUEL_3_6_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(41),int(4,SQLLEN_KIND), Fuel3_LI)
         CASE('FUEL_6_12','FUEL_6_12_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(42),int(4,SQLLEN_KIND), Fuel6_LI)
         CASE('FUEL_GT_12','FUEL_12_20','FUEL_12_20_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(43),int(4,SQLLEN_KIND), Fuel12_LI)
         CASE('FUEL_LITTER')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(44),int(4,SQLLEN_KIND), FuelLt_LI)
         CASE('FUEL_DUFF')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(45),int(4,SQLLEN_KIND), FuelDf_LI)
         CASE('FUEL_0_25','FUEL_0_25_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(46),int(4,SQLLEN_KIND), Fuel025_LI)
         CASE('FUEL_25_1','FUEL_25_1_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(47),int(4,SQLLEN_KIND), Fuel251_LI)
         CASE('FUEL_20_35','FUEL_20_35_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(48),int(4,SQLLEN_KIND), Fuel20_LI)
         CASE('FUEL_35_50','FUEL_35_50_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(49),int(4,SQLLEN_KIND), Fuel35_LI)
         CASE('FUEL_GT_50','FUEL_GT_50_H')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(50),int(4,SQLLEN_KIND), Fuel50_LI)

         CASE('FUEL_0_25_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(55),int(4,SQLLEN_KIND), FuelS025_LI)
         CASE('FUEL_25_1_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(56),int(4,SQLLEN_KIND), FuelS251_LI)
         CASE('FUEL_1_3_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(57),int(4,SQLLEN_KIND), FuelS1_LI)
         CASE('FUEL_3_6_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(58),int(4,SQLLEN_KIND), FuelS3_LI)
         CASE('FUEL_6_12_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(59),int(4,SQLLEN_KIND), FuelS6_LI)
         CASE('FUEL_12_20_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(60),int(4,SQLLEN_KIND), FuelS12_LI)
         CASE('FUEL_20_35_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(61),int(4,SQLLEN_KIND), FuelS20_LI)
         CASE('FUEL_35_50_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(62),int(4,SQLLEN_KIND), FuelS35_LI)
         CASE('FUEL_GT_50_S')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,
     -        RSTANDDATA(63),int(4,SQLLEN_KIND), FuelS50_LI)

         CASE('FUEL_MODEL')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(51),int(4,SQLLEN_KIND), FuelModel_LI)
         CASE('PHOTO_REF')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,
     -        ISTANDDATA(52),int(4,SQLLEN_KIND), FotoRef_LI)
         CASE('PHOTO_CODE')
          iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,
     -        CFotoCode,int(LEN(CFotoCode),SQLLEN_KIND), FotoCode_LI)
        END SELECT

      ENDDO

      IF(LKECHO)WRITE(JOSTND,'(/T12,''STAND-LEVEL DATA BASE READ:'')')

C     Fetch the Row Data

      iRet = fvsSQLFetch(StmtHndlIn)
      IF (iRet.NE.SQL_SUCCESS.AND.
     -    iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
        IF (iRet.EQ.SQL_NO_DATA) THEN
          CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlIn,
     -     'STANDIN:No data returned on fetch: '//trim(SQLSTR))
        ELSE
          CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlIn,
     -      'STANDIN:Fetch error: '//trim(SQLSTR))
        ENDIF
      ENDIF

      iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlIn)

C     DEFINE VVER IN CASE IT IS NEEDED.

      CALL VARVER(VVER)

C     CHECK FOR NULLS AND ASSIGN VALUES THAT WERE NOT NULL

C     PROCESS STAND IDENTIFICATION CODE.
C     STAND CONTROL NUMBER SET BY KEYWORD (STANDCN OR STDIDENT)
C     IS HIGHEST PROIORITY. OTHERWISE USE VALUE FROM DATA BASE
C
      IF(DBCN.EQ.' ')THEN
        IF(DBCN_LI.NE.SQL_NULL_DATA) THEN
           I=INDEX(TMP_DBCN,CHAR(0))
           IF (I.GT.0) TMP_DBCN(I:)=' '
           DBCN = ADJUSTL(TMP_DBCN)
           DBCN = ADJUSTL(TMP_DBCN(:DBCN_LI))
           IF(LKECHO)WRITE(JOSTND,'(T12,''STAND_CN: '',A)') TRIM(DBCN)
        ENDIF
      ELSE
         DBCN=TRIM(ADJUSTL(DBCN))
      ENDIF
      IF(NPLT.EQ.' ')THEN
        IF(Stand_LI.NE.SQL_NULL_DATA) THEN
           IF(LSTDISNUM) THEN
              WRITE (CSTAND,'(I26)') ISTANDDATA(28)
           ELSE
              I=INDEX(CSTAND,CHAR(0))
              IF (I.GT.0) CSTAND(I:)=' '
           ENDIF
           NPLT=ADJUSTL(CSTAND)
           IF(LKECHO)WRITE(JOSTND,'(T12,''STAND_ID: '',A)') TRIM(NPLT)
        ENDIF
      ELSE
        NPLT=TRIM(ADJUSTL(NPLT))
      ENDIF
C
      IF(IY_LI.NE.SQL_NULL_DATA) THEN
         IY(1) = ISTANDDATA(1)
         IF(LKECHO)WRITE(JOSTND,'(T12,''INV_YEAR: '',T34,I6)') IY(1)
      ENDIF
      IF(Long_LI.NE.SQL_NULL_DATA) THEN
         TLONG = RSTANDDATA(3)
         IF(LKECHO)WRITE(JOSTND,'(T12,''LONGITUDE: '',T29,F11.4)') TLONG
      ENDIF
      IF((Region_LI.NE.SQL_NULL_DATA).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''REGION: '',T34,I6)')
     >    ISTANDDATA(29)
      IF((Forest_LI.NE.SQL_NULL_DATA).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''FOREST: '',T34,I6)')
     >    ISTANDDATA(30)
      IF((District_LI.NE.SQL_NULL_DATA).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''DISTRICT: '',T34,I6)')
     >    ISTANDDATA(31)
      IF((Compartment_LI.NE.SQL_NULL_DATA).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''COMPARTMENT: '',T34,I6)')
     >    ISTANDDATA(32)

C     CONVERT REGION, FOREST, DISTRICT, COMPARTMENT INTO LOCATION
C     FOLLOWING VARIANT-SPECIFIC RULES.

      IF(Region_LI.NE.SQL_NULL_DATA .AND.   ! Start with RFF
     >   Forest_LI.NE.SQL_NULL_DATA) THEN
         KODFOR = ISTANDDATA(29) * 100 + ISTANDDATA(30)
         IF(VVER(:2).EQ.'KT' .OR.            ! Convert to RFFDD
     >      VVER(:2).EQ.'WS' .OR.
     >      VVER(:2).EQ.'SE' .OR.
     >      VVER(:2).EQ.'SN') THEN
            KODFOR = KODFOR * 100
            IF (District_LI.NE.SQL_NULL_DATA)
     >          KODFOR=KODFOR + ISTANDDATA(31)
         ENDIF
         IF(VVER(:2).EQ.'KT') THEN           ! Convert to RFFDDCCC
            KODFOR = KODFOR * 1000
            IF (Compartment_LI.NE.SQL_NULL_DATA)
     >          KODFOR=KODFOR + ISTANDDATA(32)
         ENDIF
         IF(LKECHO)WRITE(JOSTND,'(T12,'' COMPOSITE LOC: '',T32,I8)')
     >   KODFOR
         CALL FORKOD
      ENDIF

C     Location code overrides.

      IF(Location_LI.NE.SQL_NULL_DATA) THEN
         KODFOR = ISTANDDATA(4)
         IF(LKECHO)WRITE(JOSTND,'(T12,''LOCATION: '',T32,I8)') KODFOR
         CALL FORKOD
      ENDIF

C     SET DEFAULT LOCATION CODE IF NOT PRESENT IN INPUT DATA

      IF(KODFOR.EQ.0)CALL FORKOD
C
      IF(Lat_LI.NE.SQL_NULL_DATA) THEN
         TLAT = RSTANDDATA(2)
         IF(LKECHO)WRITE(JOSTND,'(T12,''LATITUDE: '',T29,F11.4)') TLAT
      ENDIF

      IF(PvRefCode_LI.NE.SQL_NULL_DATA)THEN
        IF(NUMPVREF.LE.0)THEN
          CPVREF='          '
        ELSE
          WRITE(CPVREF,'(I10)')NUMPVREF
        ENDIF
      ENDIF

      IF(Habitat_LI.NE.SQL_NULL_DATA) THEN
         IF (LHABISNUM) THEN   ! HABITAT CODE IS NUMBER.
            WRITE (CHAB,'(I10)') ISTANDDATA(5)
         GOTO 45
         ELSE
            I=INDEX(CHAB,CHAR(0))
            IF (I.EQ.1) GOTO 45
            IF (I.GT.0) CHAB(I:)=' '
            CHAB = ADJUSTL(CHAB)
            READ (CHAB,*,ERR=40)  ISTANDDATA(5)
            GOTO 45
   40       CONTINUE
            ISTANDDATA(5)=0
         ENDIF
   45   CONTINUE
         KARD2  = ADJUSTL(CHAB(1:10))
         ARRAY2 = ISTANDDATA(5)
         IF((VVER(:2).EQ.'SN').AND.
     >   (Ecoregion_LI.NE.SQL_NULL_DATA)) THEN
           KODTYP=0
           ICL5=0
           IF(LKECHO)WRITE(JOSTND,'(T12,'' HABITAT/PV_CODE IGNORED.'')')
         ELSE
           KODTYP=IFIX(ARRAY2)
           ICL5=KODTYP
           CALL HABTYP (KARD2,ARRAY2)
           IF (ICL5.LE.0) ICL5=KODTYP
           IF (KODTYP.NE.ICL5) THEN
              IF(LKECHO)WRITE (JOSTND,50) ADJUSTR(CHAB),KODTYP
              IF(LKECHO)WRITE (JOSTND,62) ADJUSTR(CPVREF)
           ELSE
              IF(LKECHO)WRITE (JOSTND,50) ADJUSTR(CHAB)
              IF(LKECHO)WRITE (JOSTND,62) ADJUSTR(CPVREF)
          ENDIF
   50      FORMAT (T12,'HABITAT/PV_CODE:',T29,A:
     >             ' CONVERTED TO CODE: ',I4)
   62      FORMAT (T12,'PV REFERENCE CODE:',T30,A)
         ENDIF
      ENDIF

      IF(Ecoregion_LI.NE.SQL_NULL_DATA) THEN
        IF (VVER(:2).EQ.'SN') THEN
          IF (LECOISNUM) THEN   ! ECOREGION CODE IS NUMBER.
            WRITE (CECOREG,'(I10)') ISTANDDATA(54)
            GOTO 46
          ELSE
            I=INDEX(CECOREG,CHAR(0))
            IF (I.EQ.1) GOTO 46
            IF (I.GT.0) CECOREG(I:)=' '
            CECOREG = ADJUSTL(CECOREG)
            READ (CECOREG,*,ERR=41)  ISTANDDATA(54)
            GOTO 46
   41       CONTINUE
            ISTANDDATA(54)=0
          ENDIF
   46     CONTINUE
          KARD2  = ADJUSTL(CECOREG(1:10))
          ARRAY2 = ISTANDDATA(54)
          KODTYP=IFIX(ARRAY2)
          ICL5=KODTYP
          CALL HABTYP (KARD2,ARRAY2)
          IF (ICL5.LE.0) ICL5=KODTYP
          IF (KODTYP.NE.ICL5) THEN
             IF(LKECHO)WRITE (JOSTND,51) ADJUSTR(CECOREG),KODTYP
          ELSE
             IF(LKECHO)WRITE (JOSTND,51) ADJUSTR(CECOREG)
          ENDIF
   51      FORMAT (T12,'ECOLOGICAL UNIT:',T29,A:
     >              ' CONVERTED TO CODE: ',I4)
        ENDIF
      ENDIF

      IF(Age_LI.NE.SQL_NULL_DATA) THEN
         IAGE = ISTANDDATA(6)
         IF(LKECHO)WRITE(JOSTND,'(T12,''AGE: '',T34,I6)') IAGE
      ENDIF
      IF(Aspect_LI.NE.SQL_NULL_DATA) THEN
         ASPECT = RSTANDDATA(7)
         IF(LKECHO)WRITE(JOSTND,'(T12,''ASPECT: '',T34,F6.1)') ASPECT
      ENDIF
      IF(Slope_LI.NE.SQL_NULL_DATA) THEN
         SLOPE = RSTANDDATA(8)
         IF(LKECHO)WRITE(JOSTND,'(T12,''SLOPE: '',T34,F6.2)') SLOPE
      ENDIF
      IF(Elev_LI.NE.SQL_NULL_DATA .AND.
     >   ElevFT_LI.EQ.SQL_NULL_DATA) THEN
         IF(RSTANDDATA(9).GT.0)ELEV = RSTANDDATA(9)
         IF(LKECHO)WRITE(JOSTND,'(T12,''ELEVATION: '',T34,F6.1)') ELEV
      ENDIF
      IF(ElevFT_LI.NE.SQL_NULL_DATA ) THEN
         IF (VVER(1:2).EQ.'AK') THEN
            IF(RSTANDDATA(33).GT.0.)ELEV = RSTANDDATA(33)*.1
         ELSE
            IF(RSTANDDATA(33).GT.0.)ELEV = RSTANDDATA(33)*.01
         ENDIF
         IF(LKECHO)WRITE(JOSTND,10) RSTANDDATA(33),ELEV
   10    FORMAT (T12,'ELEVFT: ',T34,F6.1,' CONVERTED TO: ',F6.1)
      ENDIF
      IF(Basal_LI.NE.SQL_NULL_DATA) THEN
         BAF = RSTANDDATA(10)
         IF(LKECHO)WRITE(JOSTND,'(T12,''BASAL_AREA_FACTOR: '',
     >   T34,F6.1)') BAF
      ENDIF
      IF(PlotArea_LI.NE.SQL_NULL_DATA) THEN
         FPA = RSTANDDATA(11)
         IF(LKECHO)WRITE(JOSTND,'(T12,''INV_PLOT_SIZE: '',T34,F6.0)')FPA
      ENDIF
      IF(BPDBH_LI.NE.SQL_NULL_DATA) THEN
         BRK = RSTANDDATA(12)
         IF(LKECHO)WRITE(JOSTND,'(T12,''BRK_DBH: '',T34,F6.1)') BRK
      ENDIF
      IF(NumPlots_LI.NE.SQL_NULL_DATA) THEN
         IPTINV = ISTANDDATA(13)
         IF(LKECHO)WRITE(JOSTND,'(T12,''NUM_PLOTS: '',T34,I6)') IPTINV
      ENDIF
      IF(NonStock_LI.NE.SQL_NULL_DATA) THEN
         NONSTK = ISTANDDATA(14)
         IF(LKECHO)WRITE(JOSTND,'(T12,''NONSTK_PLOTS: '',T34,I6)')
     >   NONSTK
      ENDIF
      IF(SamWt_LI.NE.SQL_NULL_DATA) THEN
         SAMWT = RSTANDDATA(15)
         IF(LKECHO)WRITE(JOSTND,'(T12,''SAM_WT: '',T24,F16.6)') SAMWT
      ENDIF
      IF(Stock_LI.NE.SQL_NULL_DATA) THEN
      IF (RSTANDDATA(16).GT.1.0 .AND. RSTANDDATA(16).LE.100.)
     >    GROSPC=RSTANDDATA(16)*.01
      IF (RSTANDDATA(16).GT.0.0 .AND. RSTANDDATA(16).LE.1.0)
     >    GROSPC  =RSTANDDATA(16)
      IF (GROSPC.LT.0.) THEN
         XXG=1.0
         IF (IPTINV.GT.0 .AND. NONSTK.GT.0 .AND.
     >       IPTINV-NONSTK.GT.0) XXG=(FLOAT(IPTINV)-FLOAT(NONSTK))/
     >                                FLOAT(IPTINV)
      ELSE
         XXG=GROSPC
      ENDIF
         IF(LKECHO)WRITE(JOSTND,'(T12,''STK_PCNT: '',T34,F6.3)') XXG
      ENDIF
      IF(DGT_LI.NE.SQL_NULL_DATA) THEN
         IDG = ISTANDDATA(17)
         IF(LKECHO)WRITE(JOSTND,'(T12,''DG_TRANS: '',T34,I6)') IDG
      ENDIF
      IF(DGM_LI.NE.SQL_NULL_DATA) THEN
         IF(ISTANDDATA(18).GT.0.)IFINT = ISTANDDATA(18)
         FINT = FLOAT(IFINT)
         IF(LKECHO)WRITE(JOSTND,'(T12,''DG_MEASURE: '',T34,I6)') IFINT
      ENDIF
      IF(HTT_LI.NE.SQL_NULL_DATA) THEN
         IHTG = ISTANDDATA(19)
         IF(LKECHO)WRITE(JOSTND,'(T12,''HTG_TRANS: '',T34,I6)') IHTG
      ENDIF
      IF(HTM_LI.NE.SQL_NULL_DATA) THEN
         IF(ISTANDDATA(20).GT.0.)IFINTH = ISTANDDATA(20)
         FINTH = FLOAT(IFINTH)
         IF(LKECHO)WRITE(JOSTND,'(T12,''HTG_MEASURE: '',T34,I6)') IFINTH
      ENDIF
      IF(Mort_LI.NE.SQL_NULL_DATA) THEN
         FINTM = FLOAT(ISTANDDATA(21))
         IF(FINTM.LE.0.)FINTM=5.
         IF(LKECHO)WRITE(JOSTND,'(T12,''MORT_MEASURE: '',T34,I6)')
     >   IFIX(FINTM)
      ENDIF

C     SITE SPECIES CODE PROCESSING

      IF(SiteSp_LI.NE.SQL_NULL_DATA) THEN
         IF (LSITEISNUM) THEN   ! SITE SPECIES CODE IS NUMBER.
            WRITE (CSITECODE,'(I8)') ISTANDDATA(34)
         ENDIF

         I=INDEX(CSITECODE,CHAR(0))
         IF (I.GT.0) CSITECODE(I:)=' '
         CSITECODE=ADJUSTL(CSITECODE)
         NAMELEN=LEN_TRIM(CSITECODE)
         DO J=1,NAMELEN
         CALL UPCASE(CSITECODE(J:J))
         ENDDO
         DO I=1,MAXSP
            IF(CSITECODE.EQ.NSP(I,1)(1:2).OR.CSITECODE.EQ.PLNJSP(I).OR.
     >         CSITECODE.EQ.FIAJSP(I)) THEN
               ISISP=I
               LSITE=.TRUE.
               EXIT
            ENDIF
         ENDDO
         IF (LSITE) THEN
            IF(LKECHO)WRITE (JOSTND,20) ADJUSTR(CSITECODE),
     >                                  NSP(ISISP,1)(1:2)
   20       FORMAT (T12,'SITE_SPECIES: ',T31,A9,
     >                  ' MAPPED TO INTERNAL CODE: ',A)
         ELSE
            WRITE (JOSTND,25) ADJUSTR(CSITECODE)
   25       FORMAT (T12,'SITE_SPECIES: ',T31,A9,' WAS NOT RECOGNIZED')
         ENDIF
      ENDIF

C     SITE INDEX PROCESSING

      IF(SiteIndx_LI.NE.SQL_NULL_DATA) THEN
         IF (RSTANDDATA(35).LE. 7.) THEN   ! DUNNING CODE.
            SELECT CASE (VVER(:2))
            CASE('CA','NC','SO','WS')
               IF(LKECHO)WRITE(JOSTND,27)
   27          FORMAT(38X,'   SITE INDEX IS THAN 8 AND ',
     &                'WILL BE INTERPRETED AS A DUNNING CODE.')
            END SELECT
            CALL DUNN(RSTANDDATA(35))
            IF(LKECHO)WRITE(JOSTND,
     >             '(T12,''SITE_INDEX (DUNNING CODE): '',T34,F6.1)')
     >             RSTANDDATA(35)
            CSITECODE='ALL'
         ELSE
            IF (ISISP.EQ.0) THEN
               DO I=1,MAXSP
                  SITEAR(I)=RSTANDDATA(35)
               ENDDO
               CSITECODE='ALL'
            ELSE
               SITEAR(ISISP)=RSTANDDATA(35)
               CSITECODE=NSP(ISISP,1)(1:2)
            ENDIF
        ENDIF
         IF(LKECHO)WRITE(JOSTND,30) RSTANDDATA(35),TRIM(CSITECODE)
   30    FORMAT (T12,'SITE_INDEX: ',T34,F6.1,' FOR SPECIES: ',A)

      ENDIF

      IF(MaxB_LI.NE.SQL_NULL_DATA) THEN
         BAMAX = RSTANDDATA(22)
         IF(BAMAX.GT.0.)LBAMAX=.TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''MAX_BA: '',T34,F6.1)') BAMAX
      ENDIF
      IF(MaxSDI_LI.NE.SQL_NULL_DATA) THEN
         DO I=1,MAXSP
           SDIDEF(I) = RSTANDDATA(36)
         ENDDO
         IF(LKECHO)WRITE(JOSTND,'(T12,''MAX_SDI: '',T34,F6.1)')SDIDEF(1)
      ENDIF
      IF(Model_LI.NE.SQL_NULL_DATA) THEN
         IMODTY = ISTANDDATA(25)
         IF(LKECHO)WRITE(JOSTND,'(T12,''MODEL_TYPE: '',T34,I6)') IMODTY
      ENDIF
      IF(PhysioR_LI.NE.SQL_NULL_DATA) THEN
         IPHREG = ISTANDDATA(26)
         IF(LKECHO)WRITE(JOSTND,'(T12,''PHYSIO_REGION: '',T34,I6)')
     >   IPHREG
      ENDIF
      IF(ForType_LI.NE.SQL_NULL_DATA) THEN
        IFORTP = ISTANDDATA(27)
        IF(IFORTP .GT. 999) THEN
C----------
C  THE LAST 3 CHARACTERS INDICATE THE FOREST TYPE AND THE FIRST
C  CHARACTER INDICATES THAT THE USER SET THE FOREST TYPE TO BE
C  CONSTANT FOR ALL CYCLES.  THE FIELD 3 INPUT IS DECODED
C  AND LFLAGV IS SET TO TRUE TO INDICATE CONSTANT FOREST TYPE
C----------
          XTMP= FLOAT(IFORTP)
          XTMP= XTMP/1000. + 0.00001
          IXTMP= XTMP
          IFORTP= (XTMP-IXTMP)*1000
          LFLAGV= .TRUE.
        ENDIF
C----------
C  CALL FORTYP TO CHECK FOR VALID USER INPUT OF IFORTP VALUE
C----------
         DUM1=0.
         IXF=1
         CALL FORTYP(IXF,DUM1)
         IF(LKECHO)WRITE(JOSTND,'(T12,''FOREST_TYPE: '',T34,I6)') IFORTP
      ENDIF
      IF(State_LI.NE.SQL_NULL_DATA) THEN
         ISTATE = ISTANDDATA(37)
         IF(LKECHO)WRITE(JOSTND,'(T12,''STATE: '',T34,I6)') ISTATE
      ENDIF
      IF(Connty_LI.NE.SQL_NULL_DATA) THEN
         ICNTY = ISTANDDATA(38)
         IF(LKECHO)WRITE(JOSTND,'(T12,''COUNTY: '',T34,I6)') ICNTY
      ENDIF

C     FUEL LOAD PARAMETERS

      LFMYES = .FALSE.
      IF(Fuel0_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_0_1_H: '',T32,F8.3)')
     >   RSTANDDATA(39)
      ELSE
         RSTANDDATA(39) = -1.
      ENDIF
      IF(Fuel1_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_1_3_H: '',T32,F8.3)')
     >   RSTANDDATA(40)
      ELSE
         RSTANDDATA(40) = -1.
      ENDIF
      IF(Fuel3_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_3_6_H: '',T32,F8.3)')
     >   RSTANDDATA(41)
      ELSE
         RSTANDDATA(41) = -1.
      ENDIF
      IF(Fuel6_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_6_12_H: '',T32,F8.3)')
     >   RSTANDDATA(42)
      ELSE
         RSTANDDATA(42) = -1.
      ENDIF
      IF(Fuel12_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_12_20_H: '',T32,F8.3)')
     >   RSTANDDATA(43)
      ELSE
         RSTANDDATA(43) = -1.
      ENDIF
      IF(FuelLt_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_LITTER: '',T32,F8.3)')
     >   RSTANDDATA(44)
      ELSE
         RSTANDDATA(44) = -1.
      ENDIF
      IF(FuelDf_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_DUFF: '',T32,F8.3)')
     >   RSTANDDATA(45)
      ELSE
         RSTANDDATA(45) = -1.
      ENDIF
      IF(Fuel025_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_0_25_H: '',T32,F8.3)')
     >   RSTANDDATA(46)
      ELSE
         RSTANDDATA(46) = -1.
      ENDIF
      IF(Fuel251_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_25_1_H: '',T32,F8.3)')
     >   RSTANDDATA(47)
      ELSE
         RSTANDDATA(47) = -1.
      ENDIF
      IF(Fuel20_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_20_35_H: '',T32,F8.3)')
     >   RSTANDDATA(48)
      ELSE
         RSTANDDATA(48) = -1.
      ENDIF
      IF(Fuel35_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_35_50_H: '',T32,F8.3)')
     >   RSTANDDATA(49)
      ELSE
         RSTANDDATA(49) = -1.
      ENDIF
      IF(Fuel50_LI.NE.SQL_NULL_DATA) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_GT_50_H: '',T32,F8.3)')
     >   RSTANDDATA(50)
      ELSE
         RSTANDDATA(50) = -1.
      ENDIF

      LFMYES2 = .FALSE.
      IF(FuelS025_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_0_25_S: '',T32,F8.3)')
     >   RSTANDDATA(55)
      ELSE
         RSTANDDATA(55) = -1.
      ENDIF
      IF(FuelS251_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_25_1_S: '',T32,F8.3)')
     >   RSTANDDATA(56)
      ELSE
         RSTANDDATA(56) = -1.
      ENDIF
      IF(FuelS1_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_1_3_S: '',T32,F8.3)')
     >   RSTANDDATA(57)
      ELSE
         RSTANDDATA(57) = -1.
      ENDIF
      IF(FuelS3_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_3_6_S: '',T32,F8.3)')
     >   RSTANDDATA(58)
      ELSE
         RSTANDDATA(58) = -1.
      ENDIF
      IF(FuelS6_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_6_12_S: '',T32,F8.3)')
     >   RSTANDDATA(59)
      ELSE
         RSTANDDATA(59) = -1.
      ENDIF
      IF(FuelS12_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_12_20_S: '',T32,F8.3)')
     >   RSTANDDATA(60)
      ELSE
         RSTANDDATA(60) = -1.
      ENDIF
      IF(FuelS20_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_20_35_S: '',T32,F8.3)')
     >   RSTANDDATA(61)
      ELSE
         RSTANDDATA(61) = -1.
      ENDIF
      IF(FuelS35_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_35_50_S: '',T32,F8.3)')
     >   RSTANDDATA(62)
      ELSE
         RSTANDDATA(62) = -1.
      ENDIF
      IF(FuelS50_LI.NE.SQL_NULL_DATA) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_GT_50_S: '',T32,F8.3)')
     >   RSTANDDATA(63)
      ELSE
         RSTANDDATA(63) = -1.
      ENDIF

C     FUEL MODEL

      LFMD = .FALSE.
      IF(FuelModel_LI.NE.SQL_NULL_DATA) THEN
         LFMD = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_MODEL: '',T34,I6)')
     >   ISTANDDATA(51)
      ELSE
         ISTANDDATA(51) = -1.
      ENDIF

C     FUEL PHOTO REFERENCE

      LFOTO = .FALSE.
      IF(FOTOREF_LI.NE.SQL_NULL_DATA) THEN
         LFOTO = .TRUE.

         IF ((ISTANDDATA(52) .NE. 4) .AND. (ISTANDDATA(52) .NE. 10)
     >   .AND. (ISTANDDATA(52) .GE. 1) .AND. (ISTANDDATA(52) .LE. 32))
     >   THEN
           REF = PHOTOREF(ISTANDDATA(52))
         ELSE
           REF = 'UNKNOWN'
           LFOTO = .FALSE.
         ENDIF
         IF(LKECHO)WRITE (JOSTND,55) ISTANDDATA(52), REF

   55    FORMAT (T12,'PHOTO_REF: ',T34,I6, ' = ',A)

      ELSE
         ISTANDDATA(52) = -1.
      ENDIF

C     FUEL PHOTO CODE

      FKOD=-1
      LFOTO2 = .FALSE.
      IF(FotoCode_LI.NE.SQL_NULL_DATA) THEN
         LFOTO2 = .TRUE.
         I=INDEX(CFotocode,CHAR(0))
         IF (I.GT.0) CFotoCode(I:)=' '
         IF (CFotoCode .NE. ' ') THEN
           CFotoCode = ADJUSTL(CFotoCode)
           READ (CFotoCode,*,ERR=58)  ISTANDDATA(53)
   58      CONTINUE           
           CALL FMPHOTOCODE(ISTANDDATA(52),CFotoCode(1:13),FKOD,1)
           IF(LKECHO)WRITE (JOSTND,60) ADJUSTR(CFotoCode),FKOD
   60      FORMAT (T12,'PHOTO_CODE:',T26,A:
     >               ' CONVERTED TO CODE: ',I4)
           IF (FKOD .EQ. -1) LFOTO2 = .FALSE.
         ENDIF
      ENDIF

      FOTODATA(1) = ISTANDDATA(52)
      FOTODATA(2) = FKOD

C     Schedule an activity that changes the initial fuel values. This mimics
C     the method used in the fire model.  First the fuels photo series photo
C     selected is set, followed by tons/acre entered directly.

      CALL  FMLNKD(LFMLK)

      IF (LFMLK.AND.LFOTO.AND.LFOTO2) THEN
         CALL OPNEW(I,1,2548,2,FOTODATA(1))
      ELSEIF (LFOTO.AND.LFOTO2.AND. .NOT. LFMLK) THEN
        WRITE(JOSTND,
     >  '(T12,''FIRE MODEL NOT LINKED, FUELS PHOTO DATA IGNORED.'')')
      ELSEIF ((FOTOREF_LI.NE.SQL_NULL_DATA) .OR.
     >        (FotoCode_LI.NE.SQL_NULL_DATA)) THEN
        WRITE(JOSTND,'(T12,''MISSING PHOTO ''
     >  ''REFERENCE OR PHOTO CODE, FUELS PHOTO DATA IGNORED.'')')
      ENDIF

      IF (LFMLK.AND.LFMYES) CALL OPNEW(I,1,2521,12,RSTANDDATA(39))
      IF (LFMYES.AND. .NOT. LFMLK) WRITE(JOSTND,
     >   '(T12,''FIRE MODEL NOT LINKED, FUELS DATA IGNORED.'')')

      IF (LFMLK.AND.LFMYES2) CALL OPNEW(I,1,2553,9,RSTANDDATA(55))
      IF (LFMYES2.AND. .NOT. LFMLK) WRITE(JOSTND,
     >   '(T12,''FIRE MODEL NOT LINKED, SOFT FUELS DATA IGNORED.'')')

C     Schedule an activity that changes the initial fuel model. This mimics
C     the method used in the fire model.

      IF (LFMLK.AND.LFMD) THEN
        X(1) = FLOAT(ISTANDDATA(51))
        CALL OPNEW(I,1,2538,1,X)
      ENDIF

      IF (LFMD.AND. .NOT. LFMLK) WRITE(JOSTND,
     >   '(T12,''FIRE MODEL NOT LINKED, FUEL MODEL IGNORED.'')')

      IF(LKECHO)WRITE(JOSTND,'(T12,''END OF DATA BASE READ.'')')

      RETURN
      END
