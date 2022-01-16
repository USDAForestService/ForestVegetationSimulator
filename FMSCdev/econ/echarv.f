      SUBROUTINE ECHARV (bfPerTree, dbh, ft3PerTree, GROSPC, PREM2,
     &                                           spId, treeId, ICYC, IY)
C----------
C ECON $Id$
C----------
C Author Fred Martin, WA DNR,

! Accumulates tpa and volume harvested amounts from each harvested tree record into ECON arrays.
!   Accumulates harvest in total harvest arrays, but accumulates harvest associated with specific ECON
!    keywords, e.g., varaiable harvest costs or harvest revenues, only if harvest is from diameter
!    classes or species for which ECON keywords were submitted.

! Called from CUTS once for each tree.

! The diameter (DIB/DBH) of the tree is indexed relative to cost or revenue keywords, then tpa and
!  volumes are accumulated in arrays sequenced by the same cost and revenue keyword-values.
! Volumes accumulated for board-foot logs or whole tree, not both, depends on volume calculation method and
!  submitted revenue keywords. Tree-volumes from input parameters, log-volumes dependent on and from arrays
!  assigned values in ECVOL.F

! Variables from FVS
!  bfPerTree  - net board-foot volume of passed tree, as a whole, single tree.
!  dbh        - dbh of passed tree.
!  ft3PerTree - net merchantable cubic-foot volume of passed tree, as a whole, single tree.
!  GROSPC     - multiplier to compensate for non-stockable points - computed in INITRE
!  ICYC       - cycle number of current cycle.
!  IY         - array storing beginning year of each cycle.
!  MAXSP      - maximum number of species codes used by a specific variant, from PRGPRM.F77.
!  PREM2      - removed trees per acre for the treeId record.
!  spId       - internal FVS species identifier, ISPC=FVS variable name.
!  treeId     - index to passed tree in list of all trees.

      implicit none

      include 'PRGPRM.F77'
      include 'ECNCOM.F77'

      integer :: i, keyWdIdx, logId
      integer, intent(in) :: ICYC, IY(MAXCY1), spId, treeId
      integer, dimension(MAX_LOGS) :: logDibIdx

      real :: defProp, harvTpa, logVol, PREM2, treeVol
      real, intent(in) :: bfPerTree, dbh, ft3PerTree, GROSPC

      if (econStartYear > IY(ICYC) .or. PREM2 <=0.0) return              !econStartYear must include 1st yr of cycle to include harvests

      harvTpa = PREM2 / GROSPC                                           !Adjust for non-stockable area, obtaining TPA based on total stand area of one-acre

      dbhSq            = dbhSq + (dbh*dbh * harvTpa)
      harvest(TPA)     = harvest(TPA) + harvTpa
      harvest(BF_1000) = harvest(BF_1000) + bfPerTree * harvTpa
      harvest(FT3_100) = harvest(FT3_100) + ft3PerTree * harvTpa

      do i = 1, varPctCnt
         if (dbh >= varPctDbhLo(i) .and. dbh < varPctDbhHi(i)) then
            pctBf(i)  = pctBf(i)  + bfPerTree  * harvTpa
            pctFt3(i) = pctFt3(i) + ft3PerTree * harvTpa
            pctTpa(i) = pctTpa(i) + harvTpa
         end if
      end do

      do i = 1, varHrvCnt
         if (dbh >= varHrvDbhLo(i) .and. dbh < varHrvDbhHi(i)) then
            hrvCostBf(i)  = hrvCostBf(i)  + bfPerTree * harvTpa
            hrvCostFt3(i) = hrvCostFt3(i) + ft3PerTree * harvTpa
            hrvCostTpa(i) = hrvCostTpa(i) + harvTpa
         end if
      end do

      if (bfPerTree > 0.0) then
         if (logBfVol(treeId,1) > 0.0 .and.
     &                            hrvRevCnt(spId, BF_1000_LOG) > 0) then !Bd Ft volume computed by logs
            treeVol = 0.0
            logId   = 1
            do while (logBfVol(treeId,logId) > 0.0)
               treeVol = treeVol + logBfVol(treeId,logId)
               logDibIdx(logId) = getDiaGrp(logDibBf(treeId,logId),
     &                                hrvRevCnt(spId,BF_1000_LOG),
     &                                hrvRevDia(spId,BF_1000_LOG,:),
     &                                hrvRevDiaIndx(spId,BF_1000_LOG,:))
               logId = logId + 1
            end do                                                       !End log diameters loop

            defProp = treeVol/bfPerTree                                  !Possible defect
            do i = 1, logId-1
               if (logDibIdx(i) > 0) then                                !Diameter-class exists for this log
                  logVol = (logBfVol(treeId,i) * harvTpa) / defProp
                  revVolume(spId,BF_1000_LOG,logDibIdx(i)) =
     &                 revVolume(spId,BF_1000_LOG,logDibIdx(i)) + logVol
               end if                                                    !End check logDibIdx(i)
            end do                                                       !End logId loop
         else                                                            !Board-foot volumes computed for whole-tree or no revnue specified for logs
            if (hrvRevCnt(spId,BF_1000) > 0) then                        !HRVRVN keywords submitted
              keyWdIdx = getDiaGrp(dbh, hrvRevCnt(spId,BF_1000),
     &         hrvRevDia(spId,BF_1000,:), hrvRevDiaIndx(spId,BF_1000,:))
               if (keyWdIdx > 0) revVolume(spId,BF_1000,keyWdIdx) =
     &              revVolume(spId,BF_1000,keyWdIdx) + bfPerTree*harvTpa
            end if
         end if                                                          !End check of logs versus whole-tree volume calculation
      end if                                                             !End check that board-foot volume exists

      if (ft3PerTree > 0.0) then
         if (logFt3Vol(treeId,1) > 0.0 .and.
     &                            hrvRevCnt(spId, FT3_100_LOG) > 0) then !Cubic-foot volumes computed by logs
            treeVol = 0.0
            logId = 1
            do while (logFt3Vol(treeId,logId) > 0.0)
               treeVol = treeVol + logFt3Vol(treeId,logId)
               logDibIdx(logId) = getDiaGrp(logDibFt3(treeId,logId),
     &                               hrvRevCnt(spId,FT3_100_LOG),
     &                               hrvRevDia(spId,FT3_100_LOG,:),
     &                               hrvRevDiaIndx(spId,FT3_100_LOG,:))
               logId = logId+1
            end do

            defProp = treeVol/ft3PerTree !Possible defect
            do i = 1, logId-1
               if (logDibIdx(i) > 0) then                                !Diameter-class exists for this log
                  logVol = (logFt3Vol(treeId,i) * harvTpa) / defProp
                  revVolume(spId,FT3_100_LOG,logDibIdx(i)) =
     &                 revVolume(spId,FT3_100_LOG,logDibIdx(i)) + logVol
               end if
            end do                                                       !End logId loop
         else                                                            !Cubic-foot volumes computed by whole-tree
            if (hrvRevCnt(spId,FT3_100) > 0) then                        !HRVRVN keywords submitted for species and units-of-measure
              keyWdIdx = getDiaGrp(dbh, hrvRevCnt(spId,FT3_100),
     &         hrvRevDia(spId,FT3_100,:), hrvRevDiaIndx(spId,FT3_100,:))
               if (keyWdIdx > 0) revVolume(spId,FT3_100,keyWdIdx) =
     &             revVolume(spId,FT3_100,keyWdIdx) + ft3PerTree*harvTpa
            end if
         end if                                                          !End check of logs versus whole-tree volume calculation
      end if                                                             !End check that cubic-foot volume exists

      if (hrvRevCnt(spId,TPA) > 0) then                                  !HRVRVN keywords for TPA submitted
         keyWdIdx = getDiaGrp(dbh, hrvRevCnt(spId,TPA),
     &                 hrvRevDia(spId,TPA,:), hrvRevDiaIndx(spId,TPA,:))
         if (keyWdIdx > 0) revVolume(spId,TPA,keyWdIdx) =
     &                            revVolume(spId,TPA,keyWdIdx) + harvTpa
      end if
      return

      contains

!    Returns the index-location of the input value in the classes array, or -1 if value is not w/in classes.
!    "sortIndex" is descending sorted index of "classes" array, used to access "classes" array values
      pure function getDiaGrp(value, cntClasses, classes, sortIndex)
     &                                                      result(indx)
         implicit none
         integer :: i, indx
         integer, intent(in) :: cntClasses, sortIndex(cntClasses)
         real, intent(in) :: value, classes(cntClasses)

         indx = -1
         if ( value >= classes(sortIndex(1)) ) then
            indx = sortIndex(1)                                          !position at largest class of "classes"
         else
            do i = 2, cntClasses
               if (value >=  classes(sortIndex(i))) then
                  indx = sortIndex(i)
                  exit
               end if
            end do
         end if

         return
      end function getDiaGrp


      end subroutine ECHARV
