      subroutine DBSECHARV_open()

C $Id$

        IMPLICIT NONE

        include 'DBSCOM.F77'

        integer fsql3_tableexists,fsql3_exec,fsql3_prepare,fsql3_step
        character(len=1000) :: SQLStmtStr
        integer iRet
        
        if(IDBSECON < 2) return
        if(IDBSECON .eq. 0) return

        ! Make sure we have an up-to-date case ID.
         call DBSCASE(1)
         
         iRet = fsql3_tableexists(IoutDBref,"FVS_EconHarvestValue"
     &                              //CHAR(0))
         IF(iRet.EQ.0) THEN
           SQLStmtStr = 'CREATE TABLE FVS_EconHarvestValue ('
     &              // 'CaseID char(36) not null,'
     &              // 'Year int not null,'
     &              // 'Species char(8) not null,'
     &              // 'Min_DIB real null,'
     &              // 'Max_DIB real null,'
     &              // 'Min_DBH real null,'
     &              // 'Max_DBH real null,'
     &              // 'TPA_Removed int null,'
     &              // 'TPA_Value int null,'
     &              // 'Tons_Per_Acre int null,'
     &              // 'Ft3_Removed int null,'
     &              // 'Ft3_Value int null,'
     &              // 'Board_Ft_Removed int null,'
     &              // 'Board_Ft_Value int null,'
     &              // 'Total_Value int null)'
          iRet = fsql3_exec(IoutDBref,SQLStmtStr)
          IF (iRet .NE. 0) THEN
            IDBSECON = 0
            RETURN
          ENDIF
        ENDIF

        SQLStmtStr = 'INSERT INTO FVS_EconHarvestValue ' //
     &     '(CaseID,Year,Species,' //
     &      'Min_DIB,Max_DIB,Min_DBH,Max_DBH,TPA_Removed,TPA_Value,' //
     &      'Tons_Per_Acre,Ft3_Removed,Ft3_Value,Board_Ft_Removed,' //
     &      'Board_Ft_Value,Total_Value) VALUES (''' // CASEID //
     &      ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?);' // CHAR(0)
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

         integer fsql3_bind_int,fsql3_bind_text,fsql3_bind_double,
     &           fsql3_step,fsql3_errmsg,iRet

         character(len=8) :: species
         character(len=100) :: msg
         real    :: minDia, maxDia, minDbh, maxDbh
         integer :: beginAnalYear, tpaCut, tpaValue, tonsPerAcre,
     &              ft3Volume, ft3Value, bfVolume, bfValue, totalValue
         integer, intent(in) :: speciesId
         real*8  :: minDia8, maxDia8, minDbh8, maxDbh8

         if(IDBSECON < 2) return   !ECON harvest table was not requested
         if(IDBSECON .eq. 0) return

         !Determine preferred species output format - keyword has precedence
         select case (ISPOUT30)
           case (1)
              species = adjustl(trim(JSP(speciesId)))
           case (2)
              species = adjustl(trim(FIAJSP(speciesId)))
           case (3)
              species = adjustl(trim(PLNJSP(speciesId)))
           case default
              if (JSPIN(speciesId) == 1) then
                 species = adjustl(trim(JSP(speciesId)))
              else if (JSPIN(speciesId) == 2) then
                 species = adjustl(trim(FIAJSP(speciesId)))
              else if (JSPIN(speciesId) == 3) then
                 species = adjustl(trim(PLNJSP(speciesId)))
              else
                 species = adjustl(trim(PLNJSP(speciesId)))
               endif
          end select                                      
          species = trim(species)
          minDia8 = minDia
          maxDia8 = maxDia
          minDbh8 = minDbh
          maxDbh8 = maxDbh
         iRet = fsql3_bind_int(IoutDBref,1,beginAnalYear)
         iRet = fsql3_bind_text(IoutDBref,2,species,len_trim(species))
         if (minDia8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,3,  minDia8)
         if (maxDia8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,4,  maxDia8)
         if (minDbh8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,5,  minDbh8)
         if (maxDbh8    .ge.0)
     >      iRet = fsql3_bind_double(IoutDBref,6,  maxDbh8)
         if (tpaCut     .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,7,  tpaCut)
         if (tpaValue   .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,8,  tpaValue)
         if (tonsPerAcre.ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,9,  tonsPerAcre)
         if (ft3Volume  .ge.0)     
     >      iRet = fsql3_bind_int   (IoutDBref,10, ft3Volume)
         if (ft3Value   .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,11, ft3Value)
         if (bfVolume   .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,12, bfVolume)
         if (bfValue    .ge.0)
     >      iRet = fsql3_bind_int   (IoutDBref,13, bfValue)
         if (totalValue .ge.0)
     >     iRet = fsql3_bind_int    (IoutDBref,14, totalValue)
         iRet = fsql3_step(IoutDBref)
         IF (iRet>0) THEN
          iRet = fsql3_errmsg(IoutDBref, msg, 100)
          PRINT *,"FVS_EconHarvestValue step error: ",trim(msg)
        endif
      end subroutine DBSECHARV_insert

      subroutine DBSECHARV_close()
      IMPLICIT NONE

      include 'DBSCOM.F77'
      integer iRet,fsql3_finalize,fsql3_errmsg
      character(len=101) msg
      
       iRet = fsql3_finalize (IoutDBref)
       IF (iRet>0) THEN
         iRet = fsql3_errmsg(IoutDBref, msg, 100)
         PRINT *,"FVS_EconHarvestValue finalize error: ",trim(msg)
       endif

      end subroutine DBSECHARV_close
