      subroutine DBSECHARV_open()

C METRIC-DBSQLITE $Id$

        IMPLICIT NONE

        include 'DBSCOM.F77'

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

        integer fsql3_tableexists,fsql3_exec,fsql3_prepare
        character(len=1000) :: SQLStmtStr
        integer iRet
        
        if(IDBSECON < 2) return
        if(IDBSECON .eq. 0) return

        ! Make sure we have an up-to-date case ID.
         call DBSCASE(1)
         
         iRet = fsql3_tableexists(IoutDBref,
     &     "FVS_EconHarvestValue_Metric"//CHAR(0))
         IF(iRet.EQ.0) THEN
           SQLStmtStr = 'CREATE TABLE FVS_EconHarvestValue_Metric ('
     &              // 'CaseID text not null,'
     &              // 'Year int not null,'
     &              // 'SpeciesFVS    text null,'
     &              // 'SpeciesPLANTS text null,'
     &              // 'SpeciesFIA    text null,'
     &              // 'Min_DIB real null,'
     &              // 'Max_DIB real null,'
     &              // 'Min_DBH real null,'
     &              // 'Max_DBH real null,'
     &              // 'TPH_Removed int null,'
     &              // 'TPH_Value int null,'
     &              // 'Tonne_Per_Ha int null,'
     &              // 'CuM_Removed int null,'
     &              // 'CuM_Value int null,'
     &              // 'Board_Removed int null,'
     &              // 'Board_Value int null,'
     &              // 'Total_Value int null);'
          iRet = fsql3_exec(IoutDBref,SQLStmtStr//CHAR(0))
          IF (iRet .NE. 0) THEN
            IDBSECON = 0
            RETURN
          ENDIF
        ENDIF

        SQLStmtStr = 'INSERT INTO FVS_EconHarvestValue_Metric ' //
     &     '(CaseID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,' //
     &      'Min_DIB,Max_DIB,Min_DBH,Max_DBH,TPH_Removed,TPH_Value,' //
     &      'Tonne_Per_Ha,CuM_Removed,CuM_Value,Board_Removed,' //
     &      'Board_Value,Total_Value) VALUES (''' // CASEID //
     &      ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);' // CHAR(0)
         iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
         iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           IDBSECON = 0
           RETURN
         ENDIF

      end subroutine DBSECHARV_open


      subroutine DBSECHARV_insert(beginAnalYear, speciesId, minDia,
     &      maxDia, minDbh, maxDbh, tpaCut, tpaValue, tonsPerAcre,
     &      ft3Volume, ft3Value, bfVolume, bfValue, totalValue)

         IMPLICIT NONE

         include 'PRGPRM.F77' !Contains MAXSP
         include 'PLOT.F77'   !Contains JSPIN(speciesId,speciesSymboltype), JSP, FIAJSP, & PLNJSP
         include 'DBSCOM.F77'

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
         
         integer fsql3_bind_int,fsql3_bind_text,fsql3_bind_double,
     &           fsql3_step,fsql3_errmsg,fsql3_reset,iRet

         character(len=8) :: species1,species2,species3
         character(len=100) :: msg
         real    :: minDia, maxDia, minDbh, maxDbh
         integer :: beginAnalYear, tpaCut, tpaValue, tonsPerAcre,
     &              ft3Volume, ft3Value, bfVolume, bfValue, totalValue
         integer, intent(in) :: speciesId
         real*8  :: minDia8, maxDia8, minDbh8, maxDbh8

         if(IDBSECON < 2) return   !ECON harvest table was not requested

C     assign FVS, PLANTS and FIA species codes
C
         species1 = JSP(speciesId)
         species2 = PLNJSP(speciesId)
         species3 = FIAJSP(speciesId)

         minDia8 = minDia
         maxDia8 = maxDia
         minDbh8 = minDbh
         maxDbh8 = maxDbh
         iRet = fsql3_bind_int(IoutDBref,1,beginAnalYear)
         iRet = fsql3_bind_text(IoutDBref,2,species1,len_trim(species1))
         iRet = fsql3_bind_text(IoutDBref,3,species2,len_trim(species2))
         iRet = fsql3_bind_text(IoutDBref,4,species3,len_trim(species3))
         if (minDia8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,5,  minDia8)
         if (maxDia8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,6,  maxDia8)
         if (minDbh8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,7,  minDbh8)
         if (maxDbh8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,8,  maxDbh8)
         if (tpaCut     .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,9,  tpaCut)
         if (tpaValue   .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,10,  tpaValue)
         if (tonsPerAcre.ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,11,  tonsPerAcre)
         if (ft3Volume  .ge.0)     
     >      iRet = fsql3_bind_int   (IoutDBref,12, ft3Volume)
         if (ft3Value   .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,13, ft3Value)
         if (bfVolume   .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,14, bfVolume)
         if (bfValue    .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,15, bfValue)
         if (totalValue .ge.0)
     >     iRet = fsql3_bind_int    (IoutDBref,16, totalValue)
         iRet = fsql3_step(IoutDBref)
         iRet = fsql3_reset(IoutDBref)
         IF (iRet>0) THEN
          iRet = fsql3_errmsg(IoutDBref, msg, 100)
          PRINT *,"FVS_EconHarvestValue_Metric step error: ",trim(msg)
        endif
      end subroutine DBSECHARV_insert

      subroutine DBSECHARV_close()
      IMPLICIT NONE

      include 'DBSCOM.F77'

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

      integer iRet,fsql3_finalize,fsql3_errmsg,fsql3_exec
      character(len=101) msg
      
      if(IDBSECON < 2) return   !ECON harvest table was not requested

      iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0)) 
      IF (iRet>0) THEN
        iRet = fsql3_errmsg(IoutDBref, msg, 100)
        PRINT *,"FVS_EconHarvestValue_Metric commit error: ",trim(msg)
      endif
      iRet = fsql3_finalize (IoutDBref)
      IF (iRet>0) THEN
        iRet = fsql3_errmsg(IoutDBref, msg, 100)
        PRINT *,"FVS_EconHarvestValue_Metric finalize error: ",trim(msg)
      endif

      end subroutine DBSECHARV_close
