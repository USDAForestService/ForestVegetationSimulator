      SUBROUTINE DBSSTANDIN(SQLSTR,LKECHO)
      IMPLICIT NONE
C----------
C VDBSQLITE $Id$
C----------
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

      INTEGER, PARAMETER :: MxCname = 50, MxMsg = 500
      INTEGER, PARAMETER :: NullInt  = -88885672
      REAL, PARAMETER    :: NullReal = -88885672.
      CHARACTER(LEN=*), PARAMETER :: NullChar = 'NullChar'
      CHARACTER(LEN=MxCname) ColName
      CHARACTER(LEN=*) SQLSTR
      CHARACTER(LEN=MxMsg) Msg
      CHARACTER*10 KARD2
      CHARACTER(LEN=11) CHAB,CECOREG
      CHARACTER(LEN=15) CFotoCode
      CHARACTER(LEN=LEN(DBCN)+1) TMP_DBCN
      CHARACTER(LEN=LEN(NPLT)+1) CSTAND
      CHARACTER*10 CSITECODE
      CHARACTER*40 PHOTOREF(32), REF
      REAL ARRAY2,X(1)
      REAL XXG,FOTODATA(2),RSTANDDATA(63),DUM1,XTMP
      INTEGER(KIND=4) ISTANDDATA(63)
      EQUIVALENCE (RSTANDDATA,ISTANDDATA)
      INTEGER J,I,KODE,FKOD,NUMPVREF,IXTMP,IXF,iRet
      INTEGER ColNumber,NameLen,ColumnCount
      LOGICAL LFMLK,LFMYES,LKECHO,LFMD,LFOTO,LFOTO2,LFMYES2

      integer  fsql3_prepare,fsql3_step,fsql3_finalize,fsql3_errmsg,
     >         fsql3_colname,fsql3_colcnt,fsql3_colint,fsql3_coltext
      real*4   fsql3_colreal

      INTEGER IY_LI,Lat_LI,Long_LI,Location_LI,
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

      IY_LI          = NullInt
      Lat_LI         = NullInt
      Long_LI        = NullInt
      Location_LI    = NullInt
      Habitat_LI     = NullInt
      Age_LI         = NullInt
      Aspect_LI      = NullInt
      Slope_LI       = NullInt
      MaxSDI_LI      = NullInt
      Elev_LI        = NullInt
      Basal_LI       = NullInt
      PlotArea_LI    = NullInt
      BPDBH_LI       = NullInt
      NumPlots_LI    = NullInt
      NonStock_LI    = NullInt
      SamWt_LI       = NullInt
      Stock_LI       = NullInt
      DGT_LI         = NullInt
      DGM_LI         = NullInt
      HTT_LI         = NullInt
      HTM_LI         = NullInt
      SiteSp_LI      = NullInt
      SiteIndx_LI    = NullInt
      Mort_LI        = NullInt
      MaxB_LI        = NullInt
      Model_LI       = NullInt
      PhysioR_LI     = NullInt
      ForType_LI     = NullInt
      Stand_LI       = NullInt
      DBCN_LI        = NullInt
      Region_LI      = NullInt
      Forest_LI      = NullInt
      District_LI    = NullInt
      Compartment_LI = NullInt
      Ecoregion_LI   = NullInt
      ElevFT_LI      = NullInt
      State_LI       = NullInt
      Connty_LI      = NullInt
      Fuel0_LI       = NullInt
      Fuel1_LI       = NullInt
      Fuel3_LI       = NullInt
      Fuel6_LI       = NullInt
      Fuel12_LI      = NullInt
      FuelLt_LI      = NullInt
      FuelDf_LI      = NullInt
      Fuel025_LI     = NullInt
      Fuel251_LI     = NullInt
      FuelModel_LI   = NullInt
      PvRefCode_LI   = NullInt
      FotoRef_LI     = NullInt
      FotoCode_LI    = NullInt
      Fuel20_LI      = NullInt
      Fuel35_LI      = NullInt
      Fuel50_LI      = NullInt                             
      FuelS025_LI    = NullInt
      FuelS251_LI    = NullInt
      FuelS1_LI      = NullInt
      FuelS3_LI      = NullInt
      FuelS6_LI      = NullInt
      FuelS12_LI     = NullInt
      FuelS20_LI     = NullInt
      FuelS35_LI     = NullInt
      FuelS50_LI     = NullInt

      IF(LKECHO)WRITE(JOSTND,'(/T12,''STAND-LEVEL DATA BASE READ:'')')

C     MAKE SURE WE HAVE AN OPEN CONNECTION

      IF(IinDBref.EQ.-1) CALL DBSOPEN(.FALSE.,.TRUE.,KODE)
      iRet = fsql3_prepare(IinDBref,trim(SQLSTR)//CHAR(0))
      if (iRet.gt.0) THEN
        iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
        WRITE (JOSTND,5) TRIM(SQLSTR), Msg(:iRet)
    5   FORMAT(/T12,"SQL=",A/T12,"Error Msg=",A)
        RETURN
      endif

C     Get the first record (and do not step through all records.

      iRet = fsql3_step(IinDBref)
      if (iRet.ne.1) RETURN
      
C     GET NUMBER OF COLUMNS RETURNED

      ColumnCount = fsql3_colcnt(IinDBref)
           
      DO ColNumber = 0,ColumnCount-1

        iRet = fsql3_colname(IinDBref,ColNumber,ColName,MxCname)
        ColName((iRet+1):) = ''

        SELECT CASE(ColName)

         CASE('STAND_CN')
           iRet = fsql3_coltext (IinDBref,ColNumber,TMP_DBCN,
     >                          LEN(TMP_DBCN),NullChar)
           TMP_DBCN((iRet+1):) = ' '
           IF (TMP_DBCN .ne. NullChar) DBCN_LI = 1

         CASE('STAND_ID')
           iRet = fsql3_coltext (IinDBref,ColNumber,CSTAND,
     >                           LEN(CSTAND),NullChar)
           CSTAND((iRet+1):) = ' '
           IF (CSTAND .ne. NullChar) Stand_LI = 1

         CASE('INV_YEAR')
           ISTANDDATA(1) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(1) .ne. NullInt) IY_LI = 1

         CASE('LATITUDE')
           RSTANDDATA(2) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(2) .ne. NullReal) Lat_LI = 1

         CASE('LONGITUDE')
           RSTANDDATA(3) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(3) .ne. NullReal) Long_LI = 1

         CASE('REGION')
           ISTANDDATA(29) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(29) .ne. NullInt) Region_LI = 1

         CASE('FOREST')
           ISTANDDATA(30) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(30) .ne. NullInt) Forest_LI = 1

         CASE('DISTRICT')
           ISTANDDATA(31) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(31) .ne. NullInt) District_LI = 1

         CASE('COMPARTMENT')
           ISTANDDATA(32) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(32) .ne. NullInt) Compartment_LI = 1

         CASE('ECOREGION')
          iRet = fsql3_coltext (IinDBref,ColNumber,CECOREG,
     >                          LEN(CECOREG),NullChar)
          CECOREG((iRet+1):) = ' '
          IF (CECOREG .ne. NullChar) Ecoregion_LI = 1

         CASE('LOCATION')
           ISTANDDATA(4) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(4) .ne. NullInt) Location_LI = 1

         CASE('HABITAT','PV_CODE')
          iRet = fsql3_coltext (IinDBref,ColNumber,CHAB,
     >                          LEN(CHAB),NullChar)
          CHAB((iRet+1):) = ' '
          IF (CHAB .ne. NullChar) Habitat_LI = 1

         CASE('PV_REF_CODE')
          NUMPVREF = fsql3_colint(IinDBref,ColNumber,NullInt)
          IF (NUMPVREF .ne. NullInt) PvRefCode_LI = 1

         CASE('AGE')
          ISTANDDATA(6) = fsql3_colint(IinDBref,ColNumber,NullInt)
          IF (ISTANDDATA(6) .ne. NullInt) Age_LI = 1

         CASE('ASPECT')
           RSTANDDATA(7) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(7) .ne. NullReal) Aspect_LI = 1

         CASE('SLOPE')
           RSTANDDATA(8) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(8) .ne. NullReal) Slope_LI = 1

         CASE('ELEVATION')
           RSTANDDATA(9) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(9) .ne. NullReal) Elev_LI = 1

         CASE('ELEVFT')
           RSTANDDATA(33) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(33) .ne. NullReal) ElevFt_LI = 1

         CASE('BASAL_AREA_FACTOR')
           RSTANDDATA(10) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(10) .ne. NullReal) Basal_LI = 1

         CASE('INV_PLOT_SIZE')
           RSTANDDATA(11) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(11) .ne. NullReal) PlotArea_LI = 1

         CASE('BRK_DBH')
           RSTANDDATA(12) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(12) .ne. NullReal) BPDBH_LI = 1

         CASE('NUM_PLOTS')
           ISTANDDATA(13) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(13) .ne. NullInt) NumPlots_LI = 1

         CASE('NONSTK_PLOTS')
           ISTANDDATA(14) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(14) .ne. NullInt) NonStock_LI = 1

         CASE('SAM_WT')
           RSTANDDATA(15) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(15) .ne. NullReal) SamWt_LI = 1

         CASE('STK_PCNT')
           RSTANDDATA(16) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(16) .ne. NullReal) Stock_LI = 1

         CASE('DG_TRANS')
           ISTANDDATA(17) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(17) .ne. NullInt) DGT_LI = 1

         CASE('DG_MEASURE')
           ISTANDDATA(18) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(18) .ne. NullInt) DGM_LI = 1

         CASE('HTG_TRANS')
           ISTANDDATA(19) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(19) .ne. NullInt) HTT_LI = 1

         CASE('HTG_MEASURE')
           ISTANDDATA(20) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(20) .ne. NullInt) HTM_LI = 1

         CASE('MORT_MEASURE')
           ISTANDDATA(21) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(21) .ne. NullInt) Mort_LI = 1

         CASE('SITE_SPECIES')
          iRet = fsql3_coltext (IinDBref,ColNumber,CSITECODE,
     >                          LEN(CSITECODE),NullChar)
          CSITECODE((iRet+1):) = ' '
          IF (CSITECODE .ne. NullChar) SiteSp_LI = 1

         CASE('SITE_INDEX')
           RSTANDDATA(35) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(35) .ne. NullReal) SiteIndx_LI = 1

         CASE('MAX_BA')
           RSTANDDATA(22) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(22) .ne. NullReal) MaxB_LI = 1

         CASE('MAX_SDI')
           RSTANDDATA(36) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(36) .ne. NullReal) MaxSDI_LI = 1

         CASE('MODEL_TYPE')
           ISTANDDATA(25) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(25) .ne. NullInt) Model_LI = 1

         CASE('PHYSIO_REGION')
           ISTANDDATA(26) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(26) .ne. NullInt) PhysioR_LI = 1

         CASE('FOREST_TYPE')
           ISTANDDATA(27) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(27) .ne. NullInt) ForType_LI = 1

         CASE('STATE')
           ISTANDDATA(37) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(37) .ne. NullInt) State_LI = 1

         CASE('COUNTY')
           ISTANDDATA(38) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(38) .ne. NullInt) Connty_LI = 1

         CASE('FUEL_0_1')
           RSTANDDATA(39) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(39) .ne. NullReal) Fuel0_LI = 1
         CASE('FUEL_1_3','FUEL_1_3_H')
           RSTANDDATA(40) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(40) .ne. NullReal) Fuel1_LI = 1
         CASE('FUEL_3_6','FUEL_3_6_H')
           RSTANDDATA(41) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(41) .ne. NullReal) Fuel3_LI = 1
         CASE('FUEL_6_12','FUEL_6_12_H')
           RSTANDDATA(42) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(42) .ne. NullReal) Fuel6_LI = 1
         CASE('FUEL_GT_12','FUEL_12_20','FUEL_12_20_H')
           RSTANDDATA(43) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(43) .ne. NullReal) Fuel12_LI = 1
         CASE('FUEL_LITTER')
           RSTANDDATA(44) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(44) .ne. NullReal) FuelLt_LI = 1
         CASE('FUEL_DUFF')
           RSTANDDATA(45) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(45) .ne. NullReal) FuelDf_LI = 1
         CASE('FUEL_0_25','FUEL_0_25_H')
           RSTANDDATA(46) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(46) .ne. NullReal) Fuel025_LI = 1
         CASE('FUEL_25_1','FUEL_25_1_H')
           RSTANDDATA(47) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(47) .ne. NullReal) Fuel251_LI = 1
         CASE('FUEL_20_35','FUEL_20_35_H')
           RSTANDDATA(48) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(48) .ne. NullReal) Fuel20_LI = 1
         CASE('FUEL_35_50','FUEL_35_50_H')
           RSTANDDATA(49) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(49) .ne. NullReal) Fuel35_LI = 1
         CASE('FUEL_GT_50','FUEL_GT_50_H')
           RSTANDDATA(50) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(50) .ne. NullReal) Fuel50_LI = 1
         CASE('FUEL_0_25_S')
           RSTANDDATA(55) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(55) .ne. NullReal) FuelS025_LI = 1
         CASE('FUEL_25_1_S')
           RSTANDDATA(56) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(56) .ne. NullReal) FuelS251_LI = 1
         CASE('FUEL_1_3_S')
           RSTANDDATA(57) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(57) .ne. NullReal) FuelS1_LI = 1
         CASE('FUEL_3_6_S')
           RSTANDDATA(58) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(58) .ne. NullReal) FuelS3_LI = 1
         CASE('FUEL_6_12_S')
           RSTANDDATA(59) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(59) .ne. NullReal) FuelS6_LI = 1
         CASE('FUEL_12_20_S')
           RSTANDDATA(60) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(60) .ne. NullReal) FuelS12_LI = 1
         CASE('FUEL_20_35_S')
           RSTANDDATA(61) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(61) .ne. NullReal) FuelS20_LI = 1
         CASE('FUEL_35_50_S')
           RSTANDDATA(62) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(62) .ne. NullReal) FuelS35_LI = 1
         CASE('FUEL_GT_50_S')
           RSTANDDATA(63) = fsql3_colreal(IinDBref,ColNumber,NullReal)
           IF (RSTANDDATA(63) .ne. NullReal) FuelS50_LI = 1

         CASE('FUEL_MODEL')
           ISTANDDATA(51) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(51) .ne. NullInt) FuelModel_LI = 1
         CASE('PHOTO_REF')
           ISTANDDATA(52) = fsql3_colint(IinDBref,ColNumber,NullInt)
           IF (ISTANDDATA(52) .ne. NullInt) FotoRef_LI = 1
         CASE('PHOTO_CODE')
          iRet = fsql3_coltext (IinDBref,ColNumber,CFotoCode,
     >                          LEN(CFotoCode),NullChar)
          CFotoCode((iRet+1):) = ' '
          IF (CFotoCode .ne. NullChar) FotoCode_LI = 1

         END SELECT

      ENDDO
      
      iRet = fsql3_finalize(IinDBref)

C     CHECK FOR NULLS AND ASSIGN VALUES THAT WERE NOT NULL

C     PROCESS STAND IDENTIFICATION CODE.
C     STAND CONTROL NUMBER SET BY KEYWORD (STANDCN OR STDIDENT)
C     IS HIGHEST PROIORITY. OTHERWISE USE VALUE FROM DATA BASE
C
      IF(DBCN.EQ.' ')THEN
        IF(DBCN_LI.GT.0) THEN
           TMP_DBCN = ADJUSTL(TMP_DBCN)
           DBCN = TMP_DBCN(:LEN(DBCN))
           IF(LKECHO)WRITE(JOSTND,'(T12,''STAND_CN: '',A)') TRIM(DBCN)
        ENDIF
      ELSE
         DBCN=TRIM(ADJUSTL(DBCN))
      ENDIF
      IF(NPLT.EQ.' ')THEN
        IF(Stand_LI.GT.0) THEN
           CSTAND=ADJUSTL(CSTAND)
           NPLT=CSTAND(:LEN(NPLT))
           IF(LKECHO)WRITE(JOSTND,'(T12,''STAND_ID: '',A)') TRIM(NPLT)
        ENDIF
      ELSE
        NPLT=TRIM(ADJUSTL(NPLT))
      ENDIF
C
      IF(IY_LI.NE.NullInt) THEN
         IY(1) = ISTANDDATA(1)
         IF(LKECHO)WRITE(JOSTND,'(T12,''INV_YEAR: '',T34,I6)') IY(1)
      ENDIF
      IF(Long_LI.NE.NullInt) THEN
         TLONG = RSTANDDATA(3)
         IF(LKECHO)WRITE(JOSTND,'(T12,''LONGITUDE: '',T29,F11.4)') TLONG
      ENDIF
      IF((Region_LI.NE.NullInt).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''REGION: '',T34,I6)')
     >    ISTANDDATA(29)
      IF((Forest_LI.NE.NullInt).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''FOREST: '',T34,I6)')
     >    ISTANDDATA(30)
      IF((District_LI.NE.NullInt).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''DISTRICT: '',T34,I6)')
     >    ISTANDDATA(31)
      IF((Compartment_LI.NE.NullInt).AND.
     >    LKECHO)WRITE(JOSTND,'(T12,''COMPARTMENT: '',T34,I6)')
     >    ISTANDDATA(32)

C     CONVERT REGION, FOREST, DISTRICT, COMPARTMENT INTO LOCATION
C     FOLLOWING VARIANT-SPECIFIC RULES.

      IF(Region_LI.NE.NullInt .AND.   ! Start with RFF
     >   Forest_LI.NE.NullInt) THEN
         KODFOR = ISTANDDATA(29) * 100 + ISTANDDATA(30)
         IF(VARACD.EQ.'KT' .OR.            ! Convert to RFFDD
     >      VARACD.EQ.'WS' .OR.
     >      VARACD.EQ.'SN') THEN
            KODFOR = KODFOR * 100
            IF (District_LI.NE.NullInt)
     >          KODFOR=KODFOR + ISTANDDATA(31)
         ENDIF
         IF(VARACD.EQ.'KT') THEN           ! Convert to RFFDDCCC
            KODFOR = KODFOR * 1000
            IF (Compartment_LI.NE.NullInt)
     >          KODFOR=KODFOR + ISTANDDATA(32)
         ENDIF
         IF(LKECHO)WRITE(JOSTND,'(T12,'' COMPOSITE LOC: '',T32,I8)')
     >   KODFOR
         CALL FORKOD
      ENDIF

C     Location code overrides.

      IF(Location_LI.NE.NullInt) THEN
         KODFOR = ISTANDDATA(4)
         IF(LKECHO)WRITE(JOSTND,'(T12,''LOCATION: '',T32,I8)') KODFOR
         CALL FORKOD
      ENDIF

C     SET DEFAULT LOCATION CODE IF NOT PRESENT IN INPUT DATA

      IF(KODFOR.EQ.0)CALL FORKOD
C
      IF(Lat_LI.NE.NullInt) THEN
         TLAT = RSTANDDATA(2)
         IF(LKECHO)WRITE(JOSTND,'(T12,''LATITUDE: '',T29,F11.4)') TLAT
      ENDIF

      IF(PvRefCode_LI.NE.NullInt)THEN
        IF(NUMPVREF.LE.0)THEN
          CPVREF='          '
        ELSE
          WRITE(CPVREF,'(I10)')NUMPVREF
        ENDIF
      ENDIF
      IF(Habitat_LI.NE.0) THEN
         CHAB = ADJUSTL(CHAB)
         READ (CHAB,'(I10)',ERR=40)  ISTANDDATA(5)
         GOTO 45
   40    CONTINUE
         ISTANDDATA(5)=0
   45    CONTINUE
         KARD2  = ADJUSTL(CHAB(1:10))
         ARRAY2 = ISTANDDATA(5)
         IF((VARACD.EQ.'SN').AND.
     >   (Ecoregion_LI.NE.NullInt)) THEN
           KODTYP=0
           ICL5=0
           IF(LKECHO)WRITE(JOSTND,'(T12,'' HABITAT/PV_CODE IGNORED.'')')
         ELSEIF(CHAB .NE. ' ') THEN
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

      IF(Ecoregion_LI.GT.0) THEN
        IF (VARACD.EQ.'SN') THEN
          CECOREG = ADJUSTL(CECOREG)
          READ (CECOREG,'(I10)',ERR=41)  ISTANDDATA(54)
          GOTO 46
   41     CONTINUE
          ISTANDDATA(54)=0
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

      IF(Age_LI.NE.NullInt) THEN
         IAGE = ISTANDDATA(6)
         IF(LKECHO)WRITE(JOSTND,'(T12,''AGE: '',T34,I6)') IAGE
      ENDIF
      IF(Aspect_LI.NE.NullInt) THEN
         ASPECT = RSTANDDATA(7)
         IF(LKECHO)WRITE(JOSTND,'(T12,''ASPECT: '',T34,F6.1)') ASPECT
      ENDIF
      IF(Slope_LI.NE.NullInt) THEN
         SLOPE = RSTANDDATA(8)
         IF(LKECHO)WRITE(JOSTND,'(T12,''SLOPE: '',T34,F6.2)') SLOPE
      ENDIF
      IF(Elev_LI.NE.NullInt .AND.
     >   ElevFT_LI.EQ.NullInt) THEN
         IF(RSTANDDATA(9).GT.0)ELEV = RSTANDDATA(9)
         IF(LKECHO)WRITE(JOSTND,'(T12,''ELEVATION: '',T34,F6.1)') ELEV
      ENDIF
      IF(ElevFT_LI.NE.NullInt ) THEN
         IF (VARACD.EQ.'AK') THEN
            IF(RSTANDDATA(33).GT.0.)ELEV = RSTANDDATA(33)*.1
         ELSE
            IF(RSTANDDATA(33).GT.0.)ELEV = RSTANDDATA(33)*.01
         ENDIF
         IF(LKECHO)WRITE(JOSTND,10) RSTANDDATA(33),ELEV
   10    FORMAT (T12,'ELEVFT: ',T34,F6.1,' CONVERTED TO: ',F6.1)
      ENDIF
      IF(Basal_LI.NE.NullInt) THEN
         BAF = RSTANDDATA(10)
         IF(LKECHO)WRITE(JOSTND,'(T12,''BASAL_AREA_FACTOR: '',
     >   T34,F6.1)') BAF
      ENDIF
      IF(PlotArea_LI.NE.NullInt) THEN
         FPA = RSTANDDATA(11)
         IF(LKECHO)WRITE(JOSTND,'(T12,''INV_PLOT_SIZE: '',T34,F6.0)')FPA
      ENDIF
      IF(BPDBH_LI.NE.NullInt) THEN
         BRK = RSTANDDATA(12)
         IF(LKECHO)WRITE(JOSTND,'(T12,''BRK_DBH: '',T34,F6.1)') BRK
      ENDIF
      IF(NumPlots_LI.NE.NullInt) THEN
         IPTINV = ISTANDDATA(13)
         IF(LKECHO)WRITE(JOSTND,'(T12,''NUM_PLOTS: '',T34,I6)') IPTINV
      ENDIF
      IF(NonStock_LI.NE.NullInt) THEN
         NONSTK = ISTANDDATA(14)
         IF(LKECHO)WRITE(JOSTND,'(T12,''NONSTK_PLOTS: '',T34,I6)')
     >   NONSTK
      ENDIF
      IF(SamWt_LI.NE.NullInt) THEN
         SAMWT = RSTANDDATA(15)
         IF(LKECHO)WRITE(JOSTND,'(T12,''SAM_WT: '',T24,F16.6)') SAMWT
      ENDIF
      IF(Stock_LI.NE.NullInt) THEN
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
      IF(DGT_LI.NE.NullInt) THEN
         IDG = ISTANDDATA(17)
         IF(LKECHO)WRITE(JOSTND,'(T12,''DG_TRANS: '',T34,I6)') IDG
      ENDIF
      IF(DGM_LI.NE.NullInt) THEN
         IF(ISTANDDATA(18).GT.0.)IFINT = ISTANDDATA(18)
         FINT = FLOAT(IFINT)
         IF(LKECHO)WRITE(JOSTND,'(T12,''DG_MEASURE: '',T34,I6)') IFINT
      ENDIF
      IF(HTT_LI.NE.NullInt) THEN
         IHTG = ISTANDDATA(19)
         IF(LKECHO)WRITE(JOSTND,'(T12,''HTG_TRANS: '',T34,I6)') IHTG
      ENDIF
      IF(HTM_LI.NE.NullInt) THEN
         IF(ISTANDDATA(20).GT.0.)IFINTH = ISTANDDATA(20)
         FINTH = FLOAT(IFINTH)
         IF(LKECHO)WRITE(JOSTND,'(T12,''HTG_MEASURE: '',T34,I6)') IFINTH
      ENDIF
      IF(Mort_LI.NE.NullInt) THEN
         FINTM = FLOAT(ISTANDDATA(21))
         IF(FINTM.LE.0.)FINTM=5.
         IF(LKECHO)WRITE(JOSTND,'(T12,''MORT_MEASURE: '',T34,I6)')
     >   IFIX(FINTM)
      ENDIF

C     SITE SPECIES CODE PROCESSING
      IF(SiteSp_LI.GT.0) THEN
         CSITECODE=ADJUSTL(CSITECODE)
         NAMELEN=LEN_TRIM(CSITECODE)
         DO J=1,NAMELEN
           CALL UPCASE(CSITECODE(J:J))
         ENDDO

         IF(LEN_TRIM(CSITECODE).LE.2)THEN
            IF((ICHAR(ADJUSTL(CSITECODE(1:1))).GE.48).AND.
     &      (ICHAR(ADJUSTL(CSITECODE(1:1))).LE.57).AND.
     &      (ICHAR(ADJUSTL(CSITECODE(2:2))).GE.48).AND.
     &      (ICHAR(ADJUSTL(CSITECODE(2:2))).LE.57))THEN
               CSITECODE=ADJUSTL(CSITECODE)
               CSITECODE='0'//CSITECODE(:LEN(CSITECODE)-1)
            ENDIF
         ENDIF

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
   20       FORMAT (T12,'SITE_SPECIES: ',T30,A,
     >                  ' MAPPED TO INTERNAL CODE: ',A)
         ELSE
            WRITE (JOSTND,25) ADJUSTR(CSITECODE)
   25       FORMAT (T12,'SITE_SPECIES: ',T30,A,' WAS NOT RECOGNIZED')
         ENDIF
      ENDIF

C     SITE INDEX PROCESSING

      IF(SiteIndx_LI.NE.NullInt) THEN
         IF (RSTANDDATA(35).LE. 7.) THEN   ! DUNNING CODE.
            SELECT CASE (VARACD)
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

      IF(MaxB_LI.NE.NullInt) THEN
         BAMAX = RSTANDDATA(22)
         IF(BAMAX.GT.0.)LBAMAX=.TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''MAX_BA: '',T34,F6.1)') BAMAX
      ENDIF
      IF(MaxSDI_LI.NE.NullInt) THEN
         DO I=1,MAXSP
           SDIDEF(I) = RSTANDDATA(36)
         ENDDO
         IF(LKECHO)WRITE(JOSTND,'(T12,''MAX_SDI: '',T34,F6.1)')SDIDEF(1)
      ENDIF
      IF(Model_LI.NE.NullInt) THEN
         IMODTY = ISTANDDATA(25)
         IF(LKECHO)WRITE(JOSTND,'(T12,''MODEL_TYPE: '',T34,I6)') IMODTY
      ENDIF
      IF(PhysioR_LI.NE.NullInt) THEN
         IPHREG = ISTANDDATA(26)
         IF(LKECHO)WRITE(JOSTND,'(T12,''PHYSIO_REGION: '',T34,I6)')
     >   IPHREG
      ENDIF
      IF(ForType_LI.NE.NullInt) THEN
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
          IXTMP= INT(XTMP)
          IFORTP= INT((XTMP-FLOAT(IXTMP))*1000)
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
      IF(State_LI.NE.NullInt) THEN
         ISTATE = ISTANDDATA(37)
         IF(LKECHO)WRITE(JOSTND,'(T12,''STATE: '',T34,I6)') ISTATE
      ENDIF
      IF(Connty_LI.NE.NullInt) THEN
         ICNTY = ISTANDDATA(38)
         IF(LKECHO)WRITE(JOSTND,'(T12,''COUNTY: '',T34,I6)') ICNTY
      ENDIF

C     FUEL LOAD PARAMETERS

      LFMYES = .FALSE.
      IF(Fuel0_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_0_1_H: '',T32,F8.3)')
     >   RSTANDDATA(39)
      ELSE
         RSTANDDATA(39) = -1.
      ENDIF
      IF(Fuel1_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_1_3_H: '',T32,F8.3)')
     >   RSTANDDATA(40)
      ELSE
         RSTANDDATA(40) = -1.
      ENDIF
      IF(Fuel3_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_3_6_H: '',T32,F8.3)')
     >   RSTANDDATA(41)
      ELSE
         RSTANDDATA(41) = -1.
      ENDIF
      IF(Fuel6_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_6_12_H: '',T32,F8.3)')
     >   RSTANDDATA(42)
      ELSE
         RSTANDDATA(42) = -1.
      ENDIF
      IF(Fuel12_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_12_20_H: '',T32,F8.3)')
     >   RSTANDDATA(43)
      ELSE
         RSTANDDATA(43) = -1.
      ENDIF
      IF(FuelLt_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_LITTER: '',T32,F8.3)')
     >   RSTANDDATA(44)
      ELSE
         RSTANDDATA(44) = -1.
      ENDIF
      IF(FuelDf_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_DUFF: '',T32,F8.3)')
     >   RSTANDDATA(45)
      ELSE
         RSTANDDATA(45) = -1.
      ENDIF
      IF(Fuel025_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_0_25_H: '',T32,F8.3)')
     >   RSTANDDATA(46)
      ELSE
         RSTANDDATA(46) = -1.
      ENDIF
      IF(Fuel251_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_25_1_H: '',T32,F8.3)')
     >   RSTANDDATA(47)
      ELSE
         RSTANDDATA(47) = -1.
      ENDIF
      IF(Fuel20_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_20_35_H: '',T32,F8.3)')
     >   RSTANDDATA(48)
      ELSE
         RSTANDDATA(48) = -1.
      ENDIF
      IF(Fuel35_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_35_50_H: '',T32,F8.3)')
     >   RSTANDDATA(49)
      ELSE
         RSTANDDATA(49) = -1.
      ENDIF
      IF(Fuel50_LI.NE.NullInt) THEN
         LFMYES = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_GT_50_H: '',T32,F8.3)')
     >   RSTANDDATA(50)
      ELSE
         RSTANDDATA(50) = -1.
      ENDIF

      LFMYES2 = .FALSE.
      IF(FuelS025_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_0_25_S: '',T32,F8.3)')
     >   RSTANDDATA(55)
      ELSE
         RSTANDDATA(55) = -1.
      ENDIF
      IF(FuelS251_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_25_1_S: '',T32,F8.3)')
     >   RSTANDDATA(56)
      ELSE
         RSTANDDATA(56) = -1.
      ENDIF
      IF(FuelS1_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_1_3_S: '',T32,F8.3)')
     >   RSTANDDATA(57)
      ELSE
         RSTANDDATA(57) = -1.
      ENDIF
      IF(FuelS3_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_3_6_S: '',T32,F8.3)')
     >   RSTANDDATA(58)
      ELSE
         RSTANDDATA(58) = -1.
      ENDIF
      IF(FuelS6_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_6_12_S: '',T32,F8.3)')
     >   RSTANDDATA(59)
      ELSE
         RSTANDDATA(59) = -1.
      ENDIF
      IF(FuelS12_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_12_20_S: '',T32,F8.3)')
     >   RSTANDDATA(60)
      ELSE
         RSTANDDATA(60) = -1.
      ENDIF
      IF(FuelS20_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_20_35_S: '',T32,F8.3)')
     >   RSTANDDATA(61)
      ELSE
         RSTANDDATA(61) = -1.
      ENDIF
      IF(FuelS35_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_35_50_S: '',T32,F8.3)')
     >   RSTANDDATA(62)
      ELSE
         RSTANDDATA(62) = -1.
      ENDIF
      IF(FuelS50_LI.NE.NullInt) THEN
         LFMYES2 = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_GT_50_S: '',T32,F8.3)')
     >   RSTANDDATA(63)
      ELSE
         RSTANDDATA(63) = -1.
      ENDIF

C     FUEL MODEL

      LFMD = .FALSE.
      IF(FuelModel_LI.NE.NullInt) THEN
         LFMD = .TRUE.
         IF(LKECHO)WRITE(JOSTND,'(T12,''FUEL_MODEL: '',T34,I6)')
     >   ISTANDDATA(51)
      ELSE
         ISTANDDATA(51) = -1
      ENDIF

C     FUEL PHOTO REFERENCE

      LFOTO = .FALSE.
      IF(FOTOREF_LI.NE.NullInt) THEN
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
         ISTANDDATA(52) = -1
      ENDIF

C     FUEL PHOTO CODE

      FKOD=-1
      LFOTO2 = .FALSE.
      IF(FotoCode_LI.GT.0) THEN
         LFOTO2 = .TRUE.
         I=INDEX(CFotocode,CHAR(0))
         IF (I.GT.0) CFotoCode(I:)=' '
         IF (CFotoCode .NE. ' ') THEN
           CFotoCode = ADJUSTL(CFotoCode)
           READ (CFotoCode,'(I10)',ERR=58)  ISTANDDATA(53)
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
      ELSEIF ((FOTOREF_LI.GT.0).OR.(FotoCode_LI.GT.0)) THEN
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
