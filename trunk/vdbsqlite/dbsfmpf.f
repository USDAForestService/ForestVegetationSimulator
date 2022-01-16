      SUBROUTINE DBSFMPF(IYEAR,NPLT,SFLMSU,MFLMSU,SFLMTO,MFLMTO,SFTYPE,
     &  MFTYPE,SPTRCH,MPTRCH,TORCHI,CROWNI,CNPYHT,CNPYDNST,SMORTBA,
     &  MMORTBA,SMORTVOL,MMORTVOL,SPSMOKE,MPSMOKE,SFUELMOD,SFUELWT,
     &  FUELMOD,FUELWT,KODE)
     
      IMPLICIT NONE
C----------
C VDBSQLITE $Id$
C----------
C     PURPOSE: TO POPULATE A DATABASE WITH THE FIRE MODELS POTENTIAL FIRE
C              OUTPUT.
C     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
C     INPUT:
C            THE POTFIRE OUTPUT FROM THE FIRE MODEL.
C              1: SURFACE FLAME LENGTH SEVERE
C              2: SURFACE FLAME LENGHT MODERATE
C              3: TOTAL FLAME LENGTH SEVERE
C              4: TOTAL FLAME LENGHT MODERATE
C              5: FIRE TYPE SEVERE
C              6: FIRE TYPE MODERATE
C              7: P-TORCH SEVERE
C              8: P-TORCH MODERATE
C              9: TORCH INDEX
C             10: CROWN INDEX
C             11: CANOPY HEIGHT
C             12: CANOPY DENSITY
C             13: MORTALITY BA SEVERE
C             14: MORTALITY BA MODERATE
C             15: MORTALITY VOLUME SEVERE
C             16: MORTALITY VOLUME MODERATE
C             17: POTENTIAL SMOKE SEVERE
C             18: POTENTIAL SMOKE MODERATE
C             19: SEVERE FUEL MODEL
C             20: SEVERE FUEL WEIGHT
C             21: FUEL MODEL
C             22: FUEL WEIGHT
C             23: KODE FOR WHETHER OR NOT THE REPORT ALSO DUMPS TO FILE
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
C
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

COMMONS

      INTEGER, PARAMETER :: MxMsg = 500
      CHARACTER(LEN=MxMsg) Msg
      INTEGER IYEAR,CNPYHT,SMORTBA,MMORTBA,SMORTVOL,MMORTVOL,KODE,iRet
      INTEGER FUELMOD,SFUELMOD,ColNumber
      REAL SFLMTO,MFLMTO,TORCHI,CROWNI,CNPYDNST,SPSMOKE,MPSMOKE
      REAL SFLMSU,MFLMSU,SPTRCH,MPTRCH
      DOUBLE PRECISION BSFLMTO,BMFLMTO,BTORCHI,BCROWNI
      DOUBLE PRECISION BSFLMSU,BMFLMSU,BSPTRCH,BMPTRCH
      DOUBLE PRECISION BCNPYDNST,BSPSMOKE,BMPSMOKE
      REAL FUELWT,SFUELWT
      DOUBLE PRECISION,DIMENSION(4)::BFUELWT,BSFUELWT
      DIMENSION FUELMOD(4),FUELWT(4),SFUELMOD(4),SFUELWT(4)
      CHARACTER*2000 SQLStmtStr
      CHARACTER*8 SFTYPE,MFTYPE
      CHARACTER(len=26) NPLT

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >    fsql3_prepare,fsql3_bind_double,fsql3_finalize,fsql3_errmsg

C     Initialize variables

      IF(IPOTFIRE.EQ.0) RETURN
      IF(IPOTFIRE.EQ.2) KODE = 0

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)
      
      IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
        iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_PotFire_East"//CHAR(0))
        IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_PotFire_East('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Year int null,'//
     -              'Flame_Len_Sev real null,'//
     -              'Flame_Len_Mod real null,'//
     -              'Canopy_Ht int null,'//
     -              'Canopy_Density real null,'//
     -              'Mortality_BA_Sev real null,'//
     -              'Mortality_BA_Mod real null,'//
     -              'Mortality_VOL_Sev real null,'//
     -              'Mortality_VOL_Mod real null,'//
     -              'Pot_Smoke_Sev real null,'//
     -              'Pot_Smoke_Mod real null,'//
     -              'Fuel_Mod1_Sev int null,'//
     -              'Fuel_Mod2_Sev int null,'//
     -              'Fuel_Mod3_Sev int null,'//
     -              'Fuel_Mod4_Sev int null,'//
     -              'Fuel_Wt1_Sev real null,'//
     -              'Fuel_Wt2_Sev real null,'//
     -              'Fuel_Wt3_Sev real null,'//
     -              'Fuel_Wt4_Sev real null,'//
     -              'Fuel_Mod1_Mod int null,'//
     -              'Fuel_Mod2_Mod int null,'//
     -              'Fuel_Mod3_Mod int null,'//
     -              'Fuel_Mod4_Mod int null,'//
     -              'Fuel_Wt1_Mod real null,'//
     -              'Fuel_Wt2_Mod real null,'//
     -              'Fuel_Wt3_Mod real null,'//
     -              'Fuel_Wt4_Mod real null);'//CHAR(0)
           iRet = fsql3_exec(IoutDBref,SQLStmtStr)
           IF (iRet.NE.0) THEN
             iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
             print *,"FVS_PotFire_East exec direct east error:",
     -         Msg(:iRet)
             IPOTFIRE = 0
             RETURN
           ENDIF
         ENDIF 
      ELSE !NOT SN VARIANT
        iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_PotFire"//CHAR(0))
        IF(iRet.EQ.0) THEN   
           SQLStmtStr='CREATE TABLE FVS_PotFire ('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Year int null,'//
     -              'Surf_Flame_Sev real null,'//
     -              'Surf_Flame_Mod real null,'//
     -              'Tot_Flame_Sev real null,'//
     -              'Tot_Flame_Mod real null,'//
     -              'Fire_Type_Sev text null,'//
     -              'Fire_Type_Mod text null,'//
     -              'PTorch_Sev real null,'//
     -              'PTorch_Mod real null,'//
     -              'Torch_Index real null,'//
     -              'Crown_Index real null,'//
     -              'Canopy_Ht int null,'//
     -              'Canopy_Density real null,'//
     -              'Mortality_BA_Sev real null,'//
     -              'Mortality_BA_Mod real null,'//
     -              'Mortality_VOL_Sev real null,'//
     -              'Mortality_VOL_Mod real null,'//
     -              'Pot_Smoke_Sev real null,'//
     -              'Pot_Smoke_Mod real null,'//
     -              'Fuel_Mod1 int null,'//
     -              'Fuel_Mod2 int null,'//
     -              'Fuel_Mod3 int null,'//
     -              'Fuel_Mod4 int null,'//
     -              'Fuel_Wt1 real null,'//
     -              'Fuel_Wt2 real null,'//
     -              'Fuel_Wt3 real null,'//
     -              'Fuel_Wt4 real null);'//CHAR(0)
           iRet = fsql3_exec(IoutDBref,SQLStmtStr)
           IF (iRet.NE.0) THEN
             iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
             print *,"FVS_PotFire west exec direct error:",Msg(:iRet)
             IPOTFIRE = 0
             RETURN
           ENDIF
         ENDIF
      ENDIF
 
      BSFLMTO=0D0
      BMFLMTO=0D0
      BSFLMSU=0D0
      BMFLMSU=0D0
      BSPTRCH=0D0
      BMPTRCH=0D0
      BTORCHI=0D0
      BCROWNI=0D0
      BCNPYDNST=0D0
      BSPSMOKE=0D0
      BMPSMOKE=0D0
      BSFLMTO=SFLMTO
      BMFLMTO=MFLMTO
      BSFLMSU=SFLMSU
      BMFLMSU=MFLMSU
      BSPTRCH=SPTRCH
      BMPTRCH=MPTRCH
      BTORCHI=TORCHI
      BCROWNI=CROWNI
      BCNPYDNST=CNPYDNST
      BSPSMOKE=SPSMOKE
      BMPSMOKE=MPSMOKE

      BFUELWT(1)=DBLE(INT((FUELWT(1)*100.)+0.5))
      BFUELWT(2)=DBLE(INT((FUELWT(2)*100.)+0.5))
      BFUELWT(3)=DBLE(INT((FUELWT(3)*100.)+0.5))
      BFUELWT(4)=DBLE(INT((FUELWT(4)*100.)+0.5))

      IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
        BSFUELWT(1)=DBLE(INT((SFUELWT(1)*100.)+0.5))
        BSFUELWT(2)=DBLE(INT((SFUELWT(2)*100.)+0.5))
        BSFUELWT(3)=DBLE(INT((SFUELWT(3)*100.)+0.5))
        BSFUELWT(4)=DBLE(INT((SFUELWT(4)*100.)+0.5))
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_PotFire_East (CaseID,',
     -     'StandID,Year,Flame_Len_Sev,Flame_Len_Mod,',
     -     'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,',
     -     'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,',
     -     'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1_mod,Fuel_Mod2_mod,',
     -     'Fuel_Mod3_mod,Fuel_Mod4_mod,Fuel_Wt1_mod,Fuel_Wt2_mod,',
     -     'Fuel_Wt3_mod,Fuel_Wt4_mod,Fuel_Mod1_Sev,',
     -     'Fuel_Mod2_Sev,Fuel_Mod3_Sev,Fuel_Mod4_Sev,',
     -     'Fuel_Wt1_Sev,Fuel_Wt2_Sev,Fuel_Wt3_Sev,',
     -     'Fuel_Wt4_Sev) VALUES (''',CASEID,''',''',TRIM(NPLT),
     -     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,? ',
     -     ',?,?,?,?);'
      ELSE
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_PotFire (CaseID,',
     -     'StandID,Year,Surf_Flame_Sev,Surf_Flame_Mod,',
     -     'Tot_Flame_Sev,Tot_Flame_Mod,Fire_Type_Sev,Fire_Type_Mod,',
     -     'PTorch_Sev,PTorch_Mod,Torch_Index,Crown_Index,',
     -     'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,',
     -     'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,',
     -     'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1,Fuel_Mod2,',
     -     'Fuel_Mod3,Fuel_Mod4,Fuel_Wt1,Fuel_Wt2,',
     -     'Fuel_Wt3,Fuel_Wt4) VALUES (''',CASEID,''',''',TRIM(NPLT),
     -     ''',?,?,?,?,?,''',TRIM(SFTYPE),''',''',TRIM(MFTYPE),
     -     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
      ENDIF

      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))

      IF (iRet.NE.0) THEN
         iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
         print *,"FVS_PotFire prepare error:",Msg(:iRet)
         IPOTFIRE = 0
         RETURN
      ENDIF

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFLMSU)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BMFLMSU)

      IF ((VARACD .NE. 'SN') .AND. (VARACD .NE. 'CS')) THEN
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFLMTO)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BMFLMTO)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BSPTRCH)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BMPTRCH)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BTORCHI)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BCROWNI)
      ENDIF

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref, ColNumber,CNPYHT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BCNPYDNST)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,SMORTBA)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,MMORTBA)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,SMORTVOL)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,MMORTVOL)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BSPSMOKE)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BMPSMOKE)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(1))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(2))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(3))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(4))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(1))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(2))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(3))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(4))

      IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
       ColNumber=ColNumber+1
       iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(1))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(2))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(3))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(4))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(1))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(2))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(3))

       ColNumber=ColNumber+1
       iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(4))

      ENDIF

      iRet = fsql3_step(IoutDBref)
      if (iRet.ne.0) then
         iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
         print *,"FVS_PotFire step error:",Msg(:iRet)
         IPOTFIRE = 0
      ENDIF   
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
         print *,"FVS_PotFire finalize error:",Msg(:iRet)
         IPOTFIRE = 0
      ENDIF
      RETURN

      END


