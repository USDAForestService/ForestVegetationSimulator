      SUBROUTINE ECVOL(treeId, LOGDIA, LOGVOL, isCubic)
C----------
C **ECVOL--ECON  DATE OF LAST REVISION: 0/12/2012
C----------
C Author Fred Martin, WA DNR,
! Loads gross log volumes and dibs for every FVS tree into an array by treeId by log for later use.
! Dependency: requires consistency of tree ids between VOLS.F, CUTS.F and log arrays used by ECHARV.
! Only logs of trees with net volume need to be tracked because ECHARV filters on net tree volumes.

! Called from VOLS, once for each tree, need to visit each tree to zero log arrays

C 0/12/2012 eliminated species identifier as provided duplicate information to treeId

! Variables from FVS
!  isCubic - logical to deal with possilbly different merchantability specifications for cubic versus board foot volume
!  LOGDIA - array of log diameters by log, by type (1=scale diameter, 2=DIB, 3=DOB), as defined in VOLSTD.f77.
!           Logs numbered from bottom 1st log to top of last log; 1=butt 1st log, 2=top 1st log, 3=top 2nd log, etc.,
!              with any trim ignored.
!           Scale diameter is used, which is DIB but may be rounded or truncated depending on volume equation/region.
!  LOGVOL - array of log volume by type by log - opposite indexing from LOGDIA, where type is: 1=GROSS SCRIBNER BF,
!              2=GROSS REMOVED SCRIBNER BF, 3=NET SCRIBNER BF, 4=GROSS CU, 5=GROSS REMOVED CU, 6=NET CU,
!              7=GROSS INTERNATIONAL 1/4 BF, as defined in VOLSTD.f77.
!           Gross board feet (1) and gross cubic feet (4) used, as net and removed are not calculated by FVS.
!           Net voluemes computed in echarv.f based on whole tree defect
!  treeId - FVS internal sequential tree index number, 1 to number of trees in simulation.

      implicit none

      include 'PRGPRM.F77'
      include 'ECNCOM.F77'

      integer, parameter  :: MAX_DIA_TYPE=3, MAX_VOL_TYPE=7              !These values from constants in VOLSTD
      integer             :: logId
      integer, intent(in) :: treeId
      logical             :: isCubic

      real, intent(in)    :: LOGDIA(MAX_LOGS+1, MAX_DIA_TYPE),
     &                       LOGVOL(MAX_VOL_TYPE, MAX_LOGS)

      if (.not.isEconToBe) return                                        !Can't check using econStartYear as econStartYear may not be set till later in simulation cycle

      logId = 1
      if (isCubic) then
         logFt3Vol(treeId,:) = 0.0
         logDibFt3(treeId,:) = 0.0
         do while (LOGDIA(logId+1,1) > 0.0)                              !Log loop, logId+1=top 1st log,
            logDibFt3(treeId,logId) = LOGDIA(logId+1,1)                  !1=scaling dib vs 2=predicted dib
             if (LOGVOL(4,logId) > 0.0)                                  !Cubic-foot volumes requested for logs
     &                         logFt3Vol(treeId,logId) = LOGVOL(4,logId) !4=gross cubic-feet
            logId = logId+1
         end do
      else                                                               !BF volume
         logBfVol(treeId,:) = 0.0                                        !Need to zero possibly previously computed logs
         logDibBf(treeId,:) = 0.0
         do while (LOGDIA(logId+1,1) > 0.0)                              !Log loop, logId+1=top 1st log,
            logDibBf(treeId,logId) = LOGDIA(logId+1,1)                   !1=scaling dib vs 2=predicted dib
            if (LOGVOL(1,logId) > 0.0)                                   !Board-foot volumes requested for logs
     &                          logBfVol(treeId,logId) = LOGVOL(1,logId) !1=gross board-feet
            logId = logId+1
         end do
      end if

      return
      end
