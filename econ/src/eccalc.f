      subroutine ECCALC(IY, ICYC, JSP, MGMID, NPLT, ITITLE)
C----------
C ECON $Id$
C----------
C Author Fred Martin, WA DNR,
C Calculates costs & revenues associated with the FVS/ECON extension.
C   May throw runtime exceptions if array bounds exceeded for number of harvest costs or
C      revenues or maximum number of investment years
C   Routine is complicated because calculating SEV with appreciated costs and revenues requires
C      that values be kept over time because the aapreciation period can be variable.

C Called by GRADD during cycling, once for each FVS cycle.

C 09/08/2011 Refactored computation of bfTotal and ft3Total in calcEcon() to a single place.
C 10/12/2012 Removed saved variables & created new commons

C Variables from FVS
C  JSP    - species codes. Loaded in **BLOCK DATA**.
C  IACTK  - activityCode, code for an Event Monitor activity
C  ICYC   - cycle number of current cycle
C  IDT    - eventYear, year an Event Monitor activity is scheduled to take place
C  ISTAT  - year an Event Monitor activity was accomplished, if -1=event deleted, 0=yet to do
C  IY     - array storing beginning year of each cycle
C  KODE   - return code, where: 0 implies no errors, 1 implies the referenced activity could not be found.
C  MAXCY1 - maximum number of cycles plus 1, from PRGPRM.F77
C  MAXSP  - maximum number of species codes used by a specific variant, from PRGPRM.F77
C  MGMID  - management code
C  NPLT   - stand label
C  NTIMES - the number of times the activity exists within the the range of dates.
C  ITITLE - STDIDENT title

      implicit none

      include 'PRGPRM.F77'
      include 'ECNCOM.F77'
      include 'ECNCOMSAVES.F77'

      character (len=4),  intent(in) :: JSP(MAXSP), MGMID
      character (len=26), intent(in) :: NPLT
      character (len=72), intent(in) :: ITITLE

      integer, parameter  :: EV_DISCCOST=30, EV_DISCREVN=31,             !Event Monitor variable codes defined in algkey.f and evtstv.f
     &                       EV_FORSTVAL=32, EV_HARVCOST=33,
     &                       EV_HARVREVN=34, EV_IRR     =35,
     &                       EV_PCTCOST =36, EV_PNV     =37,
     &                       EC_RPRODVAL=38, EV_SEV     =39,
     &                       EV_DISCRATE=46, EV_UNDISCST=47,
     &                       EV_UNDISRVN=48, EV_ECCUFT  =49,
     &                       EV_ECBDFT  =50
      integer, parameter  :: FIX_PCT=1, FIX_HRV=2, VAR_PCT=3, VAR_HRV=4
      integer, intent(in) :: IY(MAXCY1), ICYC
      integer             :: beginAnalYear, beginTime, dbOutput,         !Year is simulation year, time is investment period or number of years
     &                       endAnalYear, endTime, evntCnt, i, IACTK,
     &                       IDT, IOUT, ISTAT, KODE, NTIMES, parmsCnt

      logical       :: isHarvestPct = .FALSE.

      real, parameter     :: NEAR_ZERO = 0.01
      real                :: harvCst, harvRvn, irr, pctCst, sevSum
      real, dimension(2)  :: parms                                       !2=maximum number of Event Monitor parms used in ECCALC

!    Possible initiation and re-initiation of ECON calculations
!     1. ECON starts at beginnning of cycle and no re-initialization is requested
!     2. ECON starts after the beginning of cycle and no re-initialization is requested
!     3. ECON starts at beginning of cycle and re-initialization is requested in same cycle
!     4. ECON starts after the beginning of cycle and re-initialization is requested in same cycle


      if (econStartYear >= IY(ICYC+1)) then                              !IY(ICYC+1) = begining year of next cycle
         return
      else
         call resetLocalCycleVariables()
!       Reset ECON Event Monitor variables prior to each call of ECON, table 4 values (4)30-(4)39, (4)46-(4)50
         do i = 30,39
           call EVUST4(i)
         end do
         do i = 47,50
           call EVUST4(i)
         end do

         call getIsPretendActive(isPretendActive)                        !Entry in ECSTATUS, PRETEND only used during 1st year of a cycle

         if (isFirstEcon) then                                           !Very first time ECON is called for a FVS simulation
            isFirstEcon = .FALSE.
            call EVSET4(EV_DISCRATE, discountRate)                       !Entry point in EVSET, register discount rate w/ Event Monitor
            call initializeSavedVariables()
            if (.not. noOutputTables) call writeTableHeaders()           !Options are no tables or no log/stock table, can't have log/stock table w/o summary table
            startYear     = econStartYear                                !Permits re-initiating investment period
            beginAnalYear = econStartYear
            endAnalYear   = IY(ICYC+1) - 1                               !IY(ICYC+1) = beginning year of next cycle
            rate          = discountRate / 100.0                         !Convert % to decimal
            if (doSEV) call calcAnnCostRevSEV()                          !Annual costs & revenues can be done once for an infinite time horizon 
            call processEconStart
         else                                                            !ECON has been previously called, hence now active from beginning of cycle
            beginAnalYear = IY(ICYC)
            endAnalYear   = IY(ICYC+1) - 1                               !IY(ICYC+1)= beginning year of next cycle
            call processEconStart
         end if

!       Calculate over entire cycle or remaining portion of a cycle
         beginTime = beginAnalYear - startYear + 1
         endTime   = endAnalYear   - startYear + 1
         call valueHarvest()
         call calcEcon()
      end if

!    Subtract "pretend" harvests from accumulators, harvest always occur 1st year of a cycle
      if (isPretendActive .and. harvest(TPA) > 0.0) then
         undiscCost(beginTime) = undiscCost(beginTime) -
     &                                     (harvCst + pctCst)
         undiscRev(beginTime) = undiscRev(beginTime) - harvRvn
         costUndisc           = costUndisc - (harvCst + pctCst)
         revUndisc            = revUndisc - harvRvn
         costDisc             = costDisc - computePV(harvCst + pctCst,
     &                                               beginTime-1, rate) !Costs accrue at beginning of year
         revDisc              = revDisc - computePV(harvRvn,
     &                                              beginTime, rate)    !Revenues accrue at end of year
      end if

      call resetSavedCycleVariables()                                         !Ensure correct initial values at each cycle
      return                                                             !End main ECCALC subroutine


      contains


!    Check for re-initialization of investment period and calculate years within current cycle prior to re-initialization if needed
      subroutine processEconStart()
         integer               :: evntYear = 0
         integer, dimension(1) :: econStart = (/ECON_START_YEAR/)
         real, dimension(3)    :: strtParms(3)

         call OPFIND (1, econStart, evntCnt)                             !1=#event types, activity type, returns evntCnt=#found events
         if (evntCnt <= 0) return
         do i = 1, evntCnt                                               !Read StrtEcon parameters from Event Monitor & handle its parameters
            call OPGET (i, 3, IDT, IACTK, parmsCnt, strtParms)           !Entry in OPFIND, 1=event #, 2=parms dimension, returns IDT, IACTK, parmsCnt, & parms
            call OPDONE(i, IDT)                                          !Entry in OPFIND, i=event id, IDT=date accomplished
         end do
         evntYear = IDT                                                  !IDT can be reset by later calls
         if (evntYear > beginAnalYear) then                              !Evaluate stand prior to re-initialization time
            beginTime   = beginAnalYear - startYear + 1
            endTime     = evntYear - startYear
            endAnalYear = evntYear - 1
            call valueHarvest()
            call calcEcon()
            call resetSavedCycleVariables()                                   !Since 1st year of cycle processed, reset harvest variables
            call resetLocalCycleVariables()
         end if
!       Re-initiallize variables & time to begin a new period of ECON calculations
         call initializeSavedVariables()
         discountRate  = strtParms(1)
         call EVSET4(EV_DISCRATE, discountRate)                          !Entry point in EVSET, register discount rate w/ Event Monitor
         rate          = discountRate / 100.0                            !Convert % to decimal
         sevInput      = strtParms(2)
         doSev         = .FALSE.
         if (strtParms(3) > 0.0) doSev = .TRUE.
         startYear     = evntYear
         beginAnalYear = evntYear
         endAnalYear   = IY(ICYC+1) - 1                                  !IY(ICYC+1)= beginning year of next cycle
         if (doSEV) call calcAnnCostRevSEV()                             !Annual costs & revenues can be done once for an infinite time horizon 
         return
      end subroutine processEconStart


!    Set/Reset accumulation values for ECON initiation/re-initiation that are saved across cycles
      subroutine initializeSavedVariables()
         costDisc    = 0.0; costUndisc = 0.0
         hrvCstCnt   = 0; hrvRvnCnt = 0; burnCnt    = 0; mechCnt   =   0
         hrvCstkeywd = 0; hrvCstTyp = 0; hrvCstTime = 0; hrvCstAmt = 0.0 !Arrays by hrvCstCnt
         hrvRvnkeywd = 0; hrvRvnSp  = 0; hrvRvnTime = 0; hrvRvnAmt = 0.0 !Arrays by hrvRvnCnt
         hrvRvnUnits = 0; specCstCnt= 0; specRvnCnt = 0
         revDisc     = 0.0; revUndisc  = 0.0
         undiscCost  = 0.0; undiscRev  = 0.0                            !Arrays by year
         return
      end subroutine initializeSavedVariables


!    Reset harvest & control variable values used within a single cycle but needed externally for next cycle, used 1st in echarv.f, initially set in ecinit.f
      subroutine resetSavedCycleVariables()
!       Duplicate code from ecinit.f to initialize variables used in echarv.f & eccalc.f
         dbhSq     = 0.0
         harvest   = 0.0                                                 !Harvest volume array (TPA : FT3_100)
         hrvCostBf = 0.0; hrvCostFt3 = 0.0; hrvCostTpa = 0.0             !Harvest volume by cost type arrays (1:MAX_KEYWORDS)
         pctBf     = 0.0; pctFt3     = 0.0; pctTpa     = 0.0             !PCT harvest volume by cost type arrays (1:MAX_KEYWORDS)
         revVolume = 0.0                                                 !Array(1:MAXSP, 1:MAX_REV_UNITS, 1:MAX_KEYWORDS)
         return
      end subroutine resetSavedCycleVariables


!    Reset local harvest and control variable values used within a single cycle
      subroutine resetLocalCycleVariables()
         isHarvestPct = .FALSE.
         harvCst   = 0.0; harvRvn    = 0.0; pctCst    = 0.0
         sevSum    = 0.0
         return
      end subroutine resetLocalCycleVariables


!    Value the harvests by accumulating harvest/thinning costs and revenues, must run before calcEcon
      subroutine valueHarvest()
         implicit none
         integer :: cstCnt, evntTime, i, j, k, rvncnt, time
         logical :: done
         real    :: amt, cost, price, units

         cstCnt = 0; rvnCnt = 0
         if (harvest(TPA) > 0.0) then                                    !Harvests only occur in 1st year of cycle, & reset if ECON re-initialized
            time     = beginAnalYear - econStartYear + 1                 !Appreciation time from start of ECON
            evntTime = beginTime                                         !Harvests occur end 1st year of cycle
            if (pctMinUnits>0)then
               if((harvest(pctMinUnits)<pctMinVolume)
     &          .or. (sqrt(dbhSq / harvest(TPA))<pctMinDbh)) then         !Harvest is a PCT
                  isHarvestPct = .TRUE.
                  do i = 1, fixPctCnt
                     cost   = calcAppreAmt(fixPctAmt(i),fixPctRate(i,:),
     &                                     fixPctDur(i,:), time, done)
                     pctCst = pctCst + cost
                     if (doSev) then
                        hrvCstCnt              = hrvCstCnt + 1
                        cstCnt                 = cstCnt + 1
                        hrvCstTime(hrvCstCnt)  = evntTime
                        hrvCstTyp(hrvCstCnt)   = FIX_PCT
                        hrvCstKeywd(hrvCstCnt) = i
                        hrvCstAmt(hrvCstCnt)   = 1.0                        !1.0=amt for a fixed cost
                     end if
                  end do
                  do i = 1, varPctCnt                                       !Variable PCT costs are by dbh class
                     amt  = 0.0
                     select case(varHrvUnits(i))
                        case (TPA)
                           amt = pctTpa(i)
                           units = 1.0
                        case (BF_1000)
                           amt = pctBf(i)
                           units = 1000.0
                        case (FT3_100)
                           amt = pctFt3(i)
                           units = 100.0
                     end select
                     cost = calcAppreAmt(varHrvAmt(i), varHrvRate(i,:),
     &                              varHrvDur(i,:), time, done) / units
                     if (amt > 0.0 .and. cost > 0.0) then
                        pctCst = pctCst + amt * cost
                        if (doSev) then
                           hrvCstCnt              = hrvCstCnt + 1
                           cstCnt                 = cstCnt + 1
                           hrvCstTime(hrvCstCnt)  = evntTime
                           hrvCstTyp(hrvCstCnt)   = VAR_PCT
                           hrvCstKeywd(hrvCstCnt) = i
                           hrvCstAmt(hrvCstCnt)   = amt
                        end if
                    end if
                  end do
                  call EVSET4 (EV_PCTCOST, pctCst)                          !Entry point in EVSET - register PCT cost w/ event monitor
                  undiscCost(beginTime) = undiscCost(beginTime) + pctCst
               else                                                         !Harvest is a commercial harvest
                  do i = 1, fixHrvCnt
                     cost = calcAppreAmt(fixHrvAmt(i), fixHrvRate(i,:),
     &                                       fixHrvDur(i,:), time, done)
                     harvCst = harvCst + cost
                     if (doSev) then
                        hrvCstCnt              = hrvCstCnt + 1
                        cstCnt                 = cstCnt + 1
                        hrvCstTime(hrvCstCnt)  = evntTime
                        hrvCstTyp(hrvCstCnt)   = FIX_HRV
                        hrvCstKeywd(hrvCstCnt) = i
                        hrvCstAmt(hrvCstCnt)   = 1.0                        !1.0 = amt for a fixed cost
                     end if
                  enddo
                  do i = 1, varHrvCnt                                       !Variable harvest costs are by dbh classes
                     amt  = 0.0
                     select case(varHrvUnits(i))
                        case (TPA)
                           amt = hrvCostTpa(i)
                           cost = calcAppreAmt(varHrvAmt(i),
     &                                         varHrvRate(i,:),
     &                                         varHrvDur(i,:), 
     &                                         time, done)
                        case (BF_1000)
                           amt = hrvCostBf(i)
                           cost = calcAppreAmt(varHrvAmt(i),
     &                                         varHrvRate(i,:),
     &                                         varHrvDur(i,:), 
     &                                      time, done) / 1000.0
                        case (FT3_100)
                           amt = hrvCostFt3(i)
                           cost = calcAppreAmt(varHrvAmt(i),
     &                                         varHrvRate(i,:),
     &                                         varHrvDur(i,:),
     &                                    time, done) / 100.0
                     end select
                     if (amt > 0) then
                        harvCst = harvCst + amt * cost
                        if (doSev) then
                           hrvCstCnt              = hrvCstCnt + 1
                           cstCnt                 = cstCnt + 1
                           hrvCstTime(hrvCstCnt)  = evntTime
                           hrvCstTyp(hrvCstCnt)   = VAR_HRV
                           hrvCstKeywd(hrvCstCnt) = i
                           hrvCstAmt(hrvCstCnt)   = amt
                        end if
                    end if
                  end do
                  call EVSET4 (EV_HARVCOST, harvCst)                        !Entry point in EVSET - register harvest cost w/ event monitor
                  do i = 1, MAXSP                                           !Accumulate harvest revenues
                     do j = 1, MAX_REV_UNITS
                        do k = 1, hrvRevCnt(i,j)                            !Loop is diameter-classes for the given species and units-of-measure
                           if (revVolume(i,j,k) > 0.0) then
                              price = calcAppreAmt(hrvRevPrice(i,j,k),
     &                                             hrvRevRate(i,j,k,:),
     &                                             hrvRevDur(i,j,k,:),
     &                                             time, done)
                              harvRvn = harvRvn + revVolume(i,j,k)*price
                              if (doSev) then
                                 hrvRvnCnt              = hrvRvnCnt + 1
                                 rvnCnt                 = rvnCnt + 1
                                 hrvRvnTime(hrvRvnCnt)  = evntTime
                                 hrvRvnSp(hrvRvnCnt)    = i
                                 hrvRvnUnits(hrvRvnCnt) = j
                                 hrvRvnKeywd(hrvRvnCnt) = k
                                 hrvRvnAmt(hrvRvnCnt) = revVolume(i,j,k)
                              end if
                           end if
                        end do                                              !End "k" loop by diameter-class
                     end do                                                 !End "j" loop by revenue-units
                  end do                                                    !End "i" loop by species
                  call EVSET4 (EV_HARVREVN, harvRvn)                        !Entry point in EVSET - register harvest revenue w/ event monitor
                  undiscCost(beginTime) = undiscCost(beginTime)+ harvCst
                  undiscRev(beginTime)  = undiscRev(beginTime) + harvRvn
               end if
            end if
         endif
         if (doSev .and. (hrvRvnCnt>0 .or. hrvCstCnt>0)) then
            sevSum = SevSum + sevHrvRevenues() - sevHrvCosts()           !Accumulate both current and previous harvest values
            if (isPretendActive) then
               hrvCstCnt = hrvCstCnt - cstCnt
               hrvRvnCnt = hrvRvnCnt - rvnCnt
            end if
         end if
      return
      end subroutine valueHarvest


!    Accumulate costs and revenues, compute ECON measures, and output results
      subroutine calcEcon()
         implicit none

         character (len=6), parameter :: BLANK6 = '      '
         character (len=7), parameter :: BLANK7 = '       '
         character (len=3) :: pretend
         character (len=5) :: minDiaChar, maxDiaChar, minDbhChar,
     &                        maxDbhChar
         character (len=6) :: bcRatioChar, forestValueChar, irrChar,
     &                        reprodValueChar, rrrChar, sevChar,
     &                        sevGivenChar
         character (len=7) :: tpaCutChar, tpaValueChar,
     &                        tonsPerAcreChar, t3VolumeChar,
     &                        ft3ValueChar, bfVolumeChar, bfValueChar,
     &                        valueChar, outChar(8)

         integer, parameter :: BURN=491, MECH=493, PLANT=430             !ESTAB Event Monitor activity codes
         integer :: evntTime, i, ISQNUM, j, k, kk, prevTime, time
         integer :: bfValue, bfVolume, ft3Value, ft3Volume,
     &              tonsPerAcre, tpaCut, tpaValue

         logical :: done
         logical :: forestValueCalculated, reprodValueCalculated,        !These logicals used to assign nulls in database when value was not calculated
     &              bcRatioCalculated, irrCalculated, rrrCalculated,
     &              sevCalculated, sevInputUsed

         real    :: amt, bfTotal, bfValueTotal, cost, diaMax, discSev,
     &              factor, ft3Total, ft3ValueTotal, pnv, price,
     &              tonsTotal, tpaTotal, tpaValueTotal
         real    :: maxDia, maxDbh, minDia, minDbh
         real    :: bcRatio, forestValue, reprodValue, rrr

         if (doSev) sevSum = sevSum - sevAnnCst + sevAnnRvn

!       Compute appreciated/depreciated annual costs from each ANNUCST keyword for each year of cycle
         do i = 1, annCostCnt
            do j = beginTime, endTime, 1
               time = beginAnalYear - econStartYear + j - 1             !Appreciation time from start of ECON, cost from beginning of year
               cost = calcAppreAmt(annCostAmt(i), annCostRate(i,:),
     &                             annCostDur(i,:), time, done)
               undiscCost(j) = undiscCost(j) + cost
            end do
         end do

!       Compute appreciated/depreciated annual revenues from each ANNURVN keyword for each year of cycle
         do i = 1, annRevCnt
            do j = beginTime, endTime, 1
               time = beginAnalYear - econStartYear + j                 !Appreciation time from start of ECON, revenue at end of year
               price = calcAppreAmt(annRevAmt(i), annRevRate(i,:),
     &                              annRevDur(i,:), time, done)
               undiscRev(j) = undiscRev(j) + price
            end do
         end do

!       Query for and accumulate any SPECCST keywords - need OPGET2 for w/in cycle accumulation
         do
            call OPGET2(SPEC_COST_ACTIVITY, IDT, beginAnalYear,
     &                  endAnalYear, 1, 1, parmsCnt, parms, KODE)       !1=1st activity, 1=parms dimension, returns IDT, parmsCnt, parms, and KODE=0 if ok, else=1
            if (KODE > 0) exit                                           !Exit this do loop - no more special costs
            evntTime             = IDT - startYear + 1
            undiscCost(evntTime) = undiscCost(evntTime) + parms(1)       !parms(1)=special cost amount
            call OPDON2(SPEC_COST_ACTIVITY, IDT, beginAnalYear,
     &                  endAnalYear, 1, KODE)                           !IDT=year to mark activity as done, 1=1st activity, returns KODE=1 if not marked as done
            if (doSev) specCstCnt = specCstCnt + 1
         end do
!       Accumulate SEV of all previous special costs
         if (specCstCnt > 0 .and. doSev) then
            ISQNUM = 0
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(SPEC_COST_ACTIVITY, startYear, endAnalYear,
     &                     ISQNUM, NTIMES, IDT, parmsCnt, ISTAT, KODE)  !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !Exit do loop - no more special costs
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(SPEC_COST_ACTIVITY, IDT, startYear, 
     &                    endAnalYear, ISQNUM, 1, parmsCnt, parms, KODE)!1=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !Activity not found, Check for another special revenue
                  evntTime = ISTAT - startYear + 1
                  factor  = (1.0 + rate)**endTime
                  sevSum = sevSum - ((parms(1)*factor) / (factor-1.0))  !parms(1)=special cost amount
     &                               / (1.0 + rate)**evntTime
               endif
            end do
         end if

!       Query for and accumulate any SPECRVN keywords - need OPGET2 for w/in cycle accumulation
         do
            call OPGET2(SPEC_REV_ACTIVITY, IDT, beginAnalYear,
     &                  endAnalYear, 1, 1, parmsCnt, parms, KODE)       !1=1st activity, 1=parms dimension, returns IDT, parmsCnt, parms, and KODE
            if (KODE > 0) exit                                          !Exit this do loop - no more special revenues
            evntTime            = IDT - startYear + 1
            undiscRev(evntTime) = undiscRev(evntTime) + parms(1)        !parms(1)=special revenue amount
            call OPDON2(SPEC_REV_ACTIVITY, IDT, beginAnalYear,
     &                  endAnalYear, 1, KODE)                           !IDT=year to mark activity as done, 1=1st activity, returns KODE=1 if not marked as done
            if (doSev) specRvnCnt = specRvnCnt + 1
         end do
!       Accumulate SEV of all previous special revenues
         if (specRvnCnt > 0 .and. doSev) then
            ISQNUM = 0
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(SPEC_REV_ACTIVITY, startYear, endAnalYear,
     &                     ISQNUM, NTIMES, IDT, parmsCnt, ISTAT, KODE)  !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !Exit this do loop - no more special costs
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(SPEC_REV_ACTIVITY, IDT, startYear,
     &                    endAnalYear, ISQNUM, 1, parmsCnt, parms, KODE)!1=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !Activity not found, Check for another special revenuee
                  evntTime = ISTAT - startYear + 1
                  factor  = (1.0 + rate)**endTime
                  sevSum = sevSum + ((parms(1)*factor) / (factor-1.0))  !parms(1)=special revenue amount
     &                               / (1.0 + rate)**evntTime
               endif
            end do
         end if

!       Compute appreciated/depreciated establishment costs - plant, mechanical prep & burn prep
!        Get each activity w/in time period (OPSTUS) & the date accomplished (ISTAT>0=yr accomplished, 0,-1=not done)
!        ISSUE: OPSTUS & OPGET3 return activities in scheduled, not completed, order - per acre cost by individual year could be compromised
         if (plntCostCnt > 0) then
            prevTime = -1                                                !Initialize to a nonsensical value
            ISQNUM   =  0
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(PLANT, beginAnalYear, endAnalYear, ISQNUM,
     &                     NTIMES, IDT, parmsCnt, ISTAT, KODE)          !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !No activity found
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(PLANT, IDT, beginAnalYear, endAnalYear,
     &                        ISQNUM, 2, parmsCnt, parms, KODE)         !2=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !Activity could not be found
                  time     = ISTAT - econStartYear + 1                  !Appreciation time from start of ECON
                  evntTime = ISTAT - startYear + 1
                  do j = 1, plntCostCnt
                     amt = 0.0
                     select case (plntCostUnits(j))
                        case (PER_ACRE)
                           if (prevTime /= ISTAT) then                  !Only accumulate per acre cost once per year, not per planting keyword
                              prevTime = ISTAT
                              amt      = 1.0
                           end if
                        case (TPA_1000)
                           amt  = parms(2) / 1000.0                     !parms(2)=# trees planted, convert to 1000's of trees
                     end select
                     if (amt > 0.0) then
                        cost = calcAppreAmt(plntCostAmt(j),
     &                                      plntCostRate(j,:),
     &                                      plntCostDur(j,:),
     &                                      time, done)
                        undiscCost(evntTime) = undiscCost(evntTime) +
     &                                                    amt * cost
                     end if
                  end do
               end if
            end do
         end if                                                         !End planting cost accumulation
!       Accumulate SEV of all previous planting actions
         if (plntCostCnt > 0 .and. doSev) then
            prevTime = -1                                               !Initialize to a nonsensical value
            ISQNUM = 0
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(PLANT, startYear, endAnalYear, ISQNUM,
     &                     NTIMES, IDT, parmsCnt, ISTAT, KODE)          !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !Exit this do loop - no more special costs
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(PLANT, IDT, startYear, endAnalYear,
     &                        ISQNUM, 2, parmsCnt, parms, KODE)         !2=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !Check status for another special revenue
                  evntTime = ISTAT - startYear + 1
                  do j = 1, plntCostCnt
                     amt = 0.0
                     select case (plntCostUnits(j))
                        case (PER_ACRE)
                           if (prevTime /= ISTAT) then                  !Only accumulate per acre cost once per year, not per planting keyword
                              prevTime   = ISTAT
                              amt        = 1.0
                           end if
                        case (TPA_1000)
                           amt  = parms(2) / 1000.0                     !parms(2)=# trees planted, convert to 1000's of trees
                     end select
                     if (amt > 0.0) sevSum = sevSum - calcAppreSev(amt,
     &                                                plntCostAmt(j),
     &                                                plntCostRate(j,:),
     &                                                plntCostDur(j,:),
     &                                                evntTime)
                  end do
               endif
            end do
         end if

!       Add any mechanical site prep costs
         if (mechCostAmt > 0) then
            prevTime = -1                                                !Initialize to a nonsensical value
            ISQNUM = 0                                                   !Activity occurrence id
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(MECH, beginAnalYear, endAnalYear, ISQNUM,
     &                     NTIMES, IDT, parmsCnt, ISTAT, KODE)          !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !KODE > 0 no activity found
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(MECH, IDT, beginAnalYear, endAnalYear,
     &                        ISQNUM, 1, parmsCnt, parms, KODE)         !1=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !>0 is activity not found, chk for next sequenced event
                  time     = ISTAT - econStartYear + 1                  !Appreciation time from start of ECON
                  evntTime = ISTAT - startYear + 1
                  amt      = parms(1) / 100.0                           !parms(1)=% acres treated
                  cost     = calcAppreAmt(mechCostAmt, mechCostRate,
     &                                    mechCostDur, time, done)
                  undiscCost(evntTime) = undiscCost(evntTime) + amt*cost
                  if (doSev) mechCnt = mechCnt + 1
               endif
            end do
         end if
!       Accumulate SEV of all previous mechanical site prep actions
         if (mechCnt > 0 .and. doSev) then
            prevTime = -1                                                !Initialize to a nonsensical value
            ISQNUM = 0
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(MECH, startYear, endAnalYear, ISQNUM,
     &                     NTIMES, IDT, parmsCnt, ISTAT, KODE)          !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !Exit this do loop - no more special costs
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(MECH, IDT, startYear, endAnalYear,
     &                        ISQNUM, 1, parmsCnt, parms, KODE)         !1=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !Check status for another special revenue
                  evntTime = ISTAT - startYear + 1
                  sevSum = sevSum - calcAppreSev(parms(1)/100.0,
     &                                        mechCostAmt, mechCostRate,
     &                                        mechCostDur, evntTime)
               endif
            end do
         end if

!       Add any bruning site prep costs
         if (burnCostAmt > 0) then
            prevTime = -1                                                !Initialize to a nonsensical value
            ISQNUM = 0                                                   !Activity occurrence id
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(BURN, beginAnalYear, endAnalYear, ISQNUM,
     &                     NTIMES, IDT, parmsCnt, ISTAT, KODE)           !Returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                       !KODE > 0 no activity found
               if (ISTAT > 0) then                                       !Activity accomplished
                  call OPGET3(BURN, IDT, beginAnalYear, endAnalYear,
     &                        ISQNUM, 1, parmsCnt, parms, KODE)          !1=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                    !>0=activity not found, chk for next sequenced event
                  time     = ISTAT - econStartYear + 1                   !Appreciation time from start of ECON
                  evntTime = ISTAT - startYear + 1
                  amt      = parms(1) / 100.0                            !parms(1)=% acre burned
                  cost     = calcAppreAmt(burnCostAmt, burnCostRate,
     &                                    burnCostDur, time, done)
                  undiscCost(evntTime) = undiscCost(evntTime) + amt*cost
                  if (doSev) burnCnt = burnCnt + 1
               endif
            end do
         end if
!       Accumulate SEV of all previous burning site prep actions
         if (burnCnt > 0 .and. doSev) then
            prevTime = -1                                               !Initialize to a nonsensical value
            ISQNUM = 0
            do
               ISQNUM = ISQNUM + 1
               call OPSTUS(BURN, startYear, endAnalYear, ISQNUM,
     &                     NTIMES, IDT, parmsCnt, ISTAT, KODE)          !returns NTIMES=#events, IDT=yr scheduled, ISTAT=yr done, 0=not yet done, -1=deleted
               if (KODE  > 0) exit                                      !Exit this do loop - no more special costs
               if (ISTAT > 0) then                                      !Activity accomplished
                  call OPGET3(BURN, IDT, startYear, endAnalYear,
     &                        ISQNUM, 1, parmsCnt, parms, KODE)         !1=parms dimension, returns IDT, parmsCnt, parms, and KODE
                  if (KODE > 0) cycle                                   !Check status for another special revenue
                  evntTime = ISTAT - startYear + 1
                  sevSum   = sevSum - calcAppreSev(parms(1)/100.0,
     &                                        burnCostAmt, burnCostRate,
     &                                        burnCostDur, evntTime)
               endif
            end do
         end if

!       Accumulate undiscounted & discounted costs & revenues by year
         do i = beginTime, endTime
            costUndisc = costUndisc + undiscCost(i)
            revUndisc  = revUndisc  + undiscRev(i)
            costDisc   = costDisc  + computePV(undiscCost(i), i-1, rate)!Costs accrue at beginning of year
            revDisc    = revDisc   + computePV(undiscRev(i),  i,   rate)!Revenues accrue at end of year
            call EVSET4 (EV_DISCCOST, costDisc)                         !Entry point in EVSET - register variables w/ event monitor
            call EVSET4 (EV_DISCREVN, revDisc)
            call EVSET4 (EV_UNDISCST, costUndisc)
            call EVSET4 (EV_UNDISRVN, revUndisc)
         end do

!       Compute PNV, and/or forest and tree values, and/or SEV values
         if (isPretendActive) then
            pretend = 'YES'
         else
            pretend = 'NO '
         end if
         forestValueChar = BLANK6;  reprodValueChar = BLANK6
         sevCalculated   = .FALSE.; sevChar         = BLANK6
         sevInputUsed    = .FALSE.; sevGivenChar    = BLANK6
         forestValueCalculated =.FALSE.; reprodValueCalculated =.FALSE.
         pnv = revDisc - costDisc
         call EVSET4 (EV_PNV, pnv)                                      !Entry point in EVSET - register variables w/ event monitor
         if (sevInput > 0.0) then
            discSev               = computePV(sevInput, endTime, rate)  !SEV assumed discounted at end of year
            forestValueCalculated = .TRUE.
            reprodValueCalculated = .TRUE.
            forestValue           = pnv + discSev
            reprodValue           = pnv + discSev - sevInput
            write (forestValueChar,'(i6)') nint(forestValue)
            write (reprodValueChar,'(i6)') nint(reprodValue)
            call EVSET4 (EV_FORSTVAL, forestValue)                      !Entry point in EVSET - register variables w/ event monitor
            call EVSET4 (EC_RPRODVAL, reprodValue)
            sevInputUsed = .TRUE.
            write (sevGivenChar,'(i6)') nint(sevInput)
         else if (doSev) then
            sevCalculated = .TRUE.
            write (sevChar,'(i6)') nint(sevSum)
            call EVSET4 (EV_SEV, sevSum)                                !Entry point in EVSET - register variables w/ event monitor
         end if

!       Compute internal rate of return, B/C ratio, and realizable rate of return
         bcRatioChar = BLANK6; irrChar = BLANK6; rrrChar = BLANK6
         bcRatioCalculated = .FALSE.; irrCalculated = .FALSE.
         rrrCalculated     = .FALSE.
         irr = computeIRR(rate, irrCalculated, pnv) * 100.0             !Sets irrCalculated, computes irr if possible & converts to %
         if (irrCalculated) then
            if (irr > 50.0) then
               irrChar = ' >50.0'
            else if (irr < 0.0) then
               irrChar = '  <0.0'
            else
               write (irrChar,'(f6.1)') irr
            end if
            call EVSET4 (EV_IRR, irr)                                   !Entry point in EVSET - register variables w/ event monitor
         end if
         if (costDisc > NEAR_ZERO) then
            bcRatioCalculated = .TRUE.
            bcRatio           = revDisc / costDisc
            write(bcRatioChar,'(f6.1)') bcRatio
            if (revDisc > NEAR_ZERO) then
              rrr = 100.0 * (((revDisc/costDisc) ** (1.0/endTime)) *
     &                       (1.0 + rate) - 1.0)
              rrrCalculated = .TRUE.
              write(rrrChar,'(f6.1)') rrr
            end if
         end if

!Compute harvested cubic and board foot volumes that were valued, & then output to Event Monitor
        ft3Total = 0.0; bfTotal = 0.0
        if (harvest(TPA) > 0.0 .and. (.not. isHarvestPct)) then         !Only get volumes that were actually valued
           do i = 1, MAXSP                                              !Loop over species
              do j = 1, MAX_REV_UNITS                                   !Loop over revenue quantity-units
                 do k = 1, hrvRevCnt(i,j)                               !Loop is diameter-classes for the given species and units-of-measure
                    select case (j)
                    case (FT3_100, FT3_100_LOG)
                       ft3Total = ft3Total + revVolume(i,j,k)
                    case (BF_1000, BF_1000_LOG)
                       bfTotal = bfTotal + revVolume(i,j,k)
                    end select
                 end do                                                 !End diameter loop
              end do                                                    !End revenue-units loop
           end do                                                       !End species loop
        end if
        call EVSET4(EV_ECCUFT, ft3Total)
        call EVSET4(EV_ECBDFT, bfTotal)

!       Write Summary Table for this investment period
         call GETLUN(IOUT)                                               !Returns logical unit number for writing output
         call getDbsEconStatus(dbOutput)                                 !Returns DB output: 0=no output, 1=summary table, 2=summary & harvest table
         if (dbOutput > 0 .or. (.not. noOutputTables)) then              !Options are no tables or no log/stock table, can't have log/stock table w/o summary table
         call DBSECSUM(beginAnalYear, endTime, pretend, costUndisc,
     &              revUndisc, costDisc, revDisc, pnv, irr,
     &              irrCalculated, bcRatio, bcRatioCalculated, rrr,
     &              rrrCalculated, sevSum, sevCalculated,
     &              forestValue, forestValueCalculated, reprodValue,
     &              reprodValueCalculated, nint(ft3Total),
     &              nint(bfTotal), discountRate, sevInput, sevInputUsed)
         if (.not. noOutputTables)                                      !Options are no tables or no log/stock table, can't have log/stock table w/o summary table
     &         write (IOUT,'(1x, i5, t8, i5, t16, i4, t22, a3,          !Add 7 extra columns to tabs to account for TableId
     &                t29, i7, t38, i7, t46, i7,
     &                t56, i6, t62, i6, t69, a6, t76, a6,
     &                t82, a6, t88, a6, t95, a6, t102, a6,
     &                t109, i6, t116, i6, t124, f4.1, t129, a6)')
     &               sumTableId, beginAnalYear, endTime, pretend,
     &               nint(costUndisc), nint(revUndisc), nint(costDisc),
     &               nint(revDisc), nint(pnv), irrChar, bcRatioChar,
     &               rrrChar, sevChar, forestValueChar, reprodValueChar,
     &               nint(ft3Total), nint(bfTotal), discountRate,
     &               sevGivenChar
         endif

!       Write Log Stock Volume/Value Table, only trees described by HRVRVN keywords are included
         if (.not.(noLogStockTable) .or. dbOutput == 2) then            !Output table or database output requested
            if (harvest(TPA) > 0.0 .and. isHarvestPct) then             !Only a PCT was done - costs only, no harvested tree data
               if (.not. noLogStockTable)
*TODO: Thought Nick fixed so " $#*%" not needed, but doesn't work without it
     &              write (IOUT,'(1x, i5, " $#*%", /, t2, "YEAR = ",i4,
     &                   ", PCT or Non-Commerial Harvest", //, "$#*%")')
     &                   logTableId, beginAnalYear
            else if (harvest(TPA) > 0.0) then                           !Write Harvest Volume/Value Report
               call DBSECHARV_open()
               if (.not. noLogStockTable)
     &              write (IOUT,'(1x, i5, " $#*%", /, t2, "YEAR = ",i4,
     &              t13," - PRETEND HARVEST: ", a3, /,
     &              t12,"SMALL-END DIB", t30,"TREE DBH", t42," TPA",
     &              t52, " TPA", t62, "TONS/", t70, "CU FT",t80,"CU FT",
     &              t89, "BD FT", t99, "BD FT", t109, "TOTAL", /,
     &              t2, "SPECIES", t13, "MIN", t21, "MAX", t28, "MIN",
     &              t36, "MAX", t42, "REMOVED",t52,"VALUE $",t62,"ACRE",
     &              t70, "REMOVED", t80,"VALUE $", t89, "REMOVED",
     &              t99, "VALUE $", t109, "VALUE $", /,
     &              t2, "-------",t12,"-------------",
     &              t27,"-------------",t42, "-------", t51, "--------",
     &              t61, "-------", t70,"-------", t79, "--------", t89,
     &              "-------", t98,"--------", t108, "--------", /,
     &              "$#*%")') logTableId, beginAnalYear, pretend

               tpaTotal  = -1.0; tpaValueTotal = -1.0; tonsTotal = -1.0 !-1 is used by the rtoc function to separate 0 from blank
               ft3Total  = -1.0; ft3ValueTotal = -1.0
               bfTotal   = -1.0; bfValueTotal  = -1.0
               outChar=BLANK7

               do i = 1, MAXSP                                          !Loop over species
                  do j = 1, MAX_REV_UNITS                               !Loop over revenue quantity-units
                     do k  = hrvRevCnt(i,j), 1, -1                      !Loop over diameters
                        kk = hrvRevDiaIndx(i,j,k)                       !Order output by ascending diameters
                        if (revVolume(i,j,kk) <= 0.0) then
                           cycle                                        !No volume so no values, try next
                        end if
                        if (k == 1) then                                
                           diaMax = 999.9                               !Set last diameter class maximum
                        else                                            !Set max diameter for this class to min dia of next larger class
                          diaMax = hrvRevDia(i,j,hrvRevDiaIndx(i,j,k-1))
                        end if
                        if (j < BF_1000_LOG) then                       !Diameters are for tree dbh
                           maxDbh = diaMax
                           minDbh = hrvRevDia(i,j,kk)
                           maxDia = -1.0; minDia=-1.0
                        else
                           maxDia = diaMax
                           minDia = hrvRevDia(i,j,kk)
                           maxDbh = -1.0; minDbh=-1.0
                        end if
                        bfValue     = -1; bfVolume = -1; ft3Value = -1  !-1 used in itoc & rtoc functions to separate 0 from blank
                        ft3Volume   = -1; tpaCut   = -1; tpaValue = -1
                        tonsPerAcre = -1
                        time        = beginAnalYear - econStartYear + 1 !Appreciation time from start of ECON, harvests @ end of first year of a cycle
                        price       = calcAppreAmt(hrvRevPrice(i,j,kk),
     &                                             hrvRevRate(i,j,kk,:),
     &                                             hrvRevDur(i,j,kk,:),
     &                                             time, done)
                        select case (j)
                        case (TPA)
                           tpaCut        = nint(revVolume(i,j,kk))
                           call accumulate(tpaTotal, revVolume(i,j,kk))
                           amt           = revVolume(i,j,kk) * price
                           tpaValue      = nint(amt)
                           call accumulate(tpaValueTotal, amt)
                        case (FT3_100, FT3_100_LOG)
                           if (lbsFt3Amt(i) > 0.0) then
                              amt = revVolume(i,j,kk) * (lbsFt3Amt(i)/
     &                                                           2000.0)
                              tonsPerAcre = nint(amt)
                              call accumulate(tonsTotal, amt)
                           end if
                           ft3Volume     = nint(revVolume(i,j,kk))
                           call accumulate(ft3Total, revVolume(i,j,kk))
                           amt           = revVolume(i,j,kk) * price
                           ft3Value      = nint(amt)
                           call accumulate(ft3ValueTotal, amt)
                        case default
                           bfVolume     = nint(revVolume(i,j,kk))
                           call accumulate(bfTotal, revVolume(i,j,kk))
                           amt          = revVolume(i,j,kk) * price
                           bfValue      = nint(amt)
                           call accumulate(bfValueTotal, amt)
                        end select
!                      Write rows of values by species and diameter
                        call DBSECHARV_insert(beginAnalYear, i, minDia, ! i = speciesId
     &                       maxDia, minDbh, maxDbh, tpaCut, tpaValue,
     &                       tonsPerAcre, ft3Volume, ft3Value, bfVolume,
     &                       bfValue, nint(amt))

                        if (.not. noLogStockTable) then
                           minDiaChar      = rtoc(minDia)
                           maxDiaChar      = rtoc(maxDia)
                           minDbhChar      = rtoc(minDbh)
                           maxDbhChar      = rtoc(maxDbh)
                           tpaCutChar      = itoc(tpaCut)
                           tpaValueChar    = itoc(tpaValue)
                           tonsPerAcreChar = itoc(tonsPerAcre)
                           t3VolumeChar    = itoc(ft3Volume)
                           ft3ValueChar    = itoc(ft3Value)
                           bfVolumeChar    = itoc(bfVolume)
                           bfValueChar     = itoc(bfValue)
                           valueChar       = itoc(nint(amt))
                           write (IOUT,'(1x, i5, t9, a3, t19, a5,t27,a5,
     &                      t34, a5, t42, a5, t49, a7,  t59, a7, t68,a7,
     &                      t77, a7, t87, a7, t96, a7,t106,a7,t116,a7)')
     &                      logTableId,JSP(i), minDiaChar, maxDiaChar,
     &                      minDbhChar, maxDbhChar, tpaCutChar,
     &                      tpaValueChar, tonsPerAcreChar,
     &                      t3VolumeChar, ft3ValueChar,
     &                      bfVolumeChar, bfValueChar,
     &                      valueChar
                        end if
                     end do                                              !End diameter loop
                  end do                                                 !End revenue-units loop
               end do                                                    !End species loop
               call DBSECHARV_close()

               outChar(1) = itoc(nint(tpaTotal))
               outChar(2) = itoc(nint(tpaValueTotal))
               outChar(3) = itoc(nint(tonsTotal))
               outChar(4) = itoc(nint(ft3Total))
               outChar(5) = itoc(nint(ft3ValueTotal))
               outChar(6) = itoc(nint(bfTotal))
               outChar(7) = itoc(nint(bfValueTotal))
               write (outChar(8),'(i7)') nint(max(0.0, tpaValueTotal)    !Any of these three "total" variables may equal -1.0 (i.e., "empty")
     &             + max(0.0, ft3ValueTotal) + max(0.0, bfValueTotal))

!             Write totals, add 7 extra columns to tabs or GENRPT throws runtime error
               if (.not. noLogStockTable) then
                write (IOUT,'(1x, i5, t49, "-------", t58, "--------",
     &          t68, "-------", t77, "-------", t86, "--------",
     &          t96, "-------", t105, "--------", t115, "--------")')
     &          logTableId
               write (IOUT,'(1x, i5, t11, "Totals", t49, a7, t59, a7,
     &          t68, a7, t77, a7, t87, a7, t96, a7, t106, a7, t116,
     &          a7)') logTableId, outChar(1), outChar(2), outChar(3),
     &          outChar(4), outChar(5), outChar(6), outChar(7),
     &          outChar(8)
               write (IOUT,'(1x, i5, /)') logTableId                  !Needed to write a blank line w/ GENRPT
               end if
            end if                                                       !End check for PCT versus commercial harvest for harvest data output
         end if                                                          !End check for and write Harvest report or dbs

         return
      end subroutine calcEcon

      subroutine accumulate(accumulator, increment)
        implicit none
        real, intent(inout) :: accumulator
        real, intent(in)    :: increment

        if(accumulator < 0.0) then
            accumulator = increment
        else
            accumulator = accumulator + increment
        end if
      end subroutine

      pure character(len=5) function rtoc(n)
        implicit none
        real, intent(in) :: n
        rtoc = '     '
        if(n /= -1.0) write(rtoc, '(f5.1)') n
        return
      end function rtoc

      pure character(len=7) function itoc(n)
        implicit none
        integer, intent(in) :: n
        itoc = '       '
        if (n /= -1) write(itoc, '(i7)') n
        return
      end function itoc


!    Computes internal rate of return defined as interest rate at which pnv=0, rates are in decimal.
      real function computeIRR(rate, irrCalculated, pnv)
         implicit none
         logical, intent(out) :: irrCalculated
         real, intent(in)     :: rate, pnv
         real                 :: netA, netC, rorA, rorB, rorC
         real, parameter      :: IRR_TOLERANCE=0.00001,
     &                           IRR_INCREMENT=0.0001

         irrCalculated = .FALSE.; computeIRR = 0.0
         if (costDisc < NEAR_ZERO .or. revDisc < NEAR_ZERO) return       !IRR undefined

         if (abs(pnv) < NEAR_ZERO) then
            irrCalculated = .TRUE.
            computeIRR    = rate
            return
         end if

         rorA = rate
         netA = pnv
         if (pnv >= 0.0) then
            do while ((netA > 0.0) .and. (rorA <= 0.51))
               rorA = rorA + IRR_INCREMENT
               netA = computePNV(rorA)
            end do
            if ((netA > 0.0) .and. (rorA > 0.51)) then
               irrCalculated = .TRUE.
               computeIRR    = rorA                                      !Return
            else
               rorB = rorA - IRR_INCREMENT
               rorC = (rorA + rorB) / 2.0
               netC = computePNV(rorC)
               do while (abs(netC) > NEAR_ZERO .and.
     &                                   abs(rorA-rorB) > IRR_TOLERANCE)
                  if (netC > 0.0) then
                     rorB = rorC
                  else
                     rorA = rorC
                  end if
                  rorC = (rorA + rorB) / 2.0
                  netC = computePNV(rorC)
               end do
               irrCalculated = .TRUE.
               computeIRR    = rorC                                      !Return
            end if
         else
            do while ((netA < 0.0) .and. (rorA >= 0.0))
               rorA = rorA - IRR_INCREMENT
               netA = computePNV(rorA)
            end do
            if ((netA < 0.0) .and. (rorA < 0.0)) then
               irrCalculated = .TRUE.
               computeIRR    = rorA                                      !Return
            else
               rorB = rorA + IRR_INCREMENT
               rorC = (rorA + rorB) / 2.0
               netC = computePNV(rorC)
               do while (abs(netC) > NEAR_ZERO .and.
     &                                   abs(rorA-rorB) > IRR_TOLERANCE)
                  if (netC < 0.0) then
                     rorB = rorC
                  else
                     rorA = rorC
                  end if
                  rorC = (rorA + rorB)/2.0
                  netC = computePNV(rorC)
               end do
               irrCalculated = .TRUE.
               computeIRR    = rorC                                      !Return
            end if
         end if
         return
      end function computeIRR


!    Computes the present value of an amount for a given rate and time period
      pure real function computePV(amt, time, rate)
         integer, intent(in) :: time
         real,    intent(in) :: amt, rate


         if (amt <= 0.0) then
            computePV = 0.0
         else
            computePV = amt / (1.0 + rate)**time
         end if
         return
      end function computePV


!    Computes net present value for a given rate, for all costs & revenues since ECON start
      pure real function computePNV(rate)
        implicit none
        integer          :: i
        real, intent(in) :: rate
        real             :: discCst, discRev

        discCst = 0.0; discRev = 0.0

        do i = 1, endTime
           discCst = discCst + computePV(undiscCost(i), i-1, rate)      !Costs accrue at beginning of year
           discRev = discRev + computePV(undiscRev(i),  i,   rate)      !Revenues accrue at end of year
        end do

        computePNV = discRev - discCst
        return
      end function computePNV


!    Calculate undiscounted appreciated/depreciated values resulting from rate changes.
!      All "valueRates" up to the current time are applied based on their "valueDuration" assuming
!      value-rate records are in sorted order.  "valueRates" accrue from the time that ECON is first
!      started (not the beginning time of a re-initilization of an ECON analysis).
!      "valueRates" are cummulative, all rates up to the end of the current analysis time are included.
      real function calcAppreAmt(amt, valueRate, valueDuration,
     &                                                 apprecTime, done)
         implicit none
         integer, intent(in)  :: apprecTime, valueDuration(*)
         integer              :: durationTime, i
         logical, intent(out) :: done                                   !Only used in calculating SEV w/ appreciated rate changes
         real, intent(in)     :: amt, valueRate(*)

         calcAppreAmt = amt
         done         = .TRUE.
         if (valueDuration(1) == 0) return

         durationTime = 0
         do i = 1, MAX_RATES
            if (valueDuration(i) <= 0) exit                              !No more rate changes
            durationTime = durationTime + valueDuration(i)
         end do

         if (durationTime <= 0) return

         if (apprecTime < durationTime) done = .FALSE.                   !Additional rate changes exist beyond the time of this appreciation calculation
         durationTime = apprecTime
         i = 1
         do while (i <= MAX_RATES .and. valueDuration(i) > 0)
            if (durationTime > valueDuration(i)) then
               calcAppreAmt = calcAppreAmt *
     &                    ((1.0 + valueRate(i)/100.0)**valueDuration(i))
               durationTime = durationTime - valueDuration(i)
            else
               calcAppreAmt = calcAppreAmt * (1.0 + valueRate(i)/100.0)
     &                                                    **durationTime
               exit                                                      !No more time to evaluate
            end if
            i = i + 1
         end do
         return
      end function calcAppreAmt


!    Callculate SEV including all potential rate changes for a single cost or revenue
!     The cost or revenue occurs only once during an FVS cycle and then is evaluated over perpetual
!        rotations, assuming it occurs at the same time within all future rotations.
      real function calcAppreSev(amt, priceOrCost, valueRate,
     &                                          valueDuration, evntTime)
         implicit none
         integer, intent(in) :: evntTime, valueDuration(*)
         integer             :: apprecTime, discTime
         logical             :: done
         real                :: apprecValue, factor, npv, priceOrCost,
     &                          undiscAmt
         real, intent(in)    :: amt, valueRate(*)

         calcAppreSev = 0.0
         npv          = 0
         done         = .FALSE.
         apprecTime   = evntTime + startYear - econStartYear             !Appreciation time is from start of ECON
         discTime     = evntTime

         if (valueDuration(1) /= 0) then
            do while (.not.done)
               apprecValue = calcAppreAmt(priceOrCost, valueRate,
     &                                  valueDuration, apprecTime, done)
               undiscAmt   = amt * apprecValue
               npv         = npv + undiscAmt /  (1.0 + rate)**discTime
               apprecTime  = apprecTime + endTime                        !Set next appreciation period
               discTime    = discTime   + endTime
            end do
            apprecValue  = calcAppreAmt(priceOrCost, valueRate,
     &                                  valueDuration, apprecTime, done)
            undiscAmt    = amt * apprecValue
         else
            undiscAmt    = amt * priceOrCost
         end if
         factor       = (1.0 + rate)**endTime
         calcAppreSev = npv + (undiscAmt * factor) / (factor - 1.0) /
     &                                            (1.0 + rate)**discTime
         return
      end function calcAppreSev


!    Compute SEV for all previous harvest costs
      real function sevHrvCosts()
         implicit none
         integer :: evntTime, i, kw, units
         real    :: amt, cost

         sevHrvCosts = 0.0

         do i = 1, hrvCstCnt
            cost     = 0.0
            kw       = hrvCstKeywd(i)
            evntTime = hrvCstTime(i)
            amt      = hrvCstAmt(i)
            units    = varHrvUnits(kw)
            select case(hrvCstTyp(i))
               case (FIX_PCT)
                  cost = calcAppreSev(amt, fixPctAmt(kw),
     &                      fixPctRate(kw,:), fixPctDur(kw,:), evntTime)
               case (FIX_HRV)
                  cost = calcAppreSev(amt, fixHrvAmt(kw),
     &                      fixHrvRate(kw,:), fixHrvDur(kw,:), evntTime)
               case (VAR_PCT)
                  select case(units)
                     case (TPA)
                        cost = calcAppreSev(amt, varPctAmt(kw),
     &                      varPctRate(kw,:), varPctDur(kw,:), evntTime)
                     case (BF_1000)
                        cost = calcAppreSev(amt/1000.0, varPctAmt(kw),
     &                      varPctRate(kw,:), varPctDur(kw,:), evntTime)
                     case (FT3_100)
                        cost = calcAppreSev(amt/100.0, varPctAmt(kw),
     &                      varPctRate(kw,:), varPctDur(kw,:), evntTime)
                  end select
               case (VAR_HRV)
                  select case(units)
                     case (TPA)
                        cost = calcAppreSev(amt, varHrvAmt(kw),
     &                      varHrvRate(kw,:), varHrvDur(kw,:), evntTime)
                     case (BF_1000)
                        cost = calcAppreSev(amt/1000.0, varHrvAmt(kw),
     &                      varHrvRate(kw,:), varHrvDur(kw,:), evntTime)
                     case (FT3_100)
                        cost = calcAppreSev(amt/100.0, varHrvAmt(kw),
     &                      varHrvRate(kw,:), varHrvDur(kw,:), evntTime)
                  end select
            end select
            sevHrvCosts = sevHrvCosts + cost
         end do
      end function sevHrvCosts


!    Compute SEV for all previous harvest revenues
      real function sevHrvRevenues()
         implicit none
         integer :: i, j, k, l

         sevHrvRevenues = 0.0

         do l = 1, hrvRvnCnt
            i = hrvRvnSp(l)
            j = hrvRvnUnits(l)
            k = hrvRvnKeywd(l)
            sevHrvRevenues = sevHrvRevenues + calcAppreSev(hrvRvnAmt(l),
     &                          hrvRevPrice(i,j,k), hrvRevRate(i,j,k,:),
     &                                hrvRevDur(i,j,k,:), hrvRvnTime(l))
         end do
      end function sevHrvRevenues


!    !Computes appreciation of annual costs and revenues for computing sev over an infinite time horizon 
      subroutine calcAnnCostRevSEV()
         endTime = 1; sevAnnCst = 0.0; sevAnnRvn = 0.0                   !endTime required by calcAppreSev
         do i = 1, annCostCnt
            sevAnnCst = sevAnnCst + calcAppreSev(1.0, annCostAmt(i),     !1.0=amt, 1=event time
     &                             annCostRate(i,:), annCostDur(i,:), 1)
         end do
         do i = 1, annRevCnt
            sevAnnRvn = sevAnnRvn + calcAppreSev(1.0, annRevAmt(i),      !1.0=amt, 1=event time
     &                               annRevRate(i,:), annRevDur(i,:), 1)
         end do
      end subroutine calcAnnCostRevSEV


      subroutine writeTableHeaders()
         implicit none
         integer :: headTableId

         call GETID (headTableId)                                        !Returns the index number for header information stored in genrpt.f
         call GETID (sumTableId)
         call GETID (logTableId)
         call GETLUN(IOUT)                                               !Returns logical unit number for writing output

!       Write ECON header
         write (IOUT,'(1x, i5, " $#*%", ///, 132("-"), /,
     &         50x, "FVS/ECON EXTENSION VERSION 1.0", /,
     &         "STAND ID: ", a26, 2x, "MANAGEMENT CODE: ", a4,2x, a72 /,
     &         132("-"), /, "$#*%")') headTableId, NPLT, MGMID, ITITLE

!       Write summary output table heading
         write (IOUT,'(1x, i5, " $#*%", /, "ECONOMIC ANALYSIS SUMMARY ",
     &                                 "REPORT", /,"$#*%" )') sumTableId

         write (IOUT,'(1x, i5, " $#*%", /, 132("-"), /,
     &     t7, "INVEST", t14, "PRETEND", T22, "UNDISCNTED VALUE",
     &        t39, "  PRESENT VALUE", t89, "VALUE", t96, "VALUE",
     &        t103, "MRCH", t110,  "MRCH", t117, "DISC"/,
     &     t7, "-MENT", t14, "MODE IS", t22, 16("-"), t39,16("-"),
     &        t70, " B/C", t89, "OF",  t96, "OF", t103, "CU FT",
     &        t110,  "BD FT", t117, "RATE", t124, " SEV"/,
     &     t1, " YEAR", t7, "PERIOD", T14, "ACTIVE", t22, "   COST",
     &       t31, "REVENUE", t39, "   COST", t48, "REVENUE",
     &       t56, "  PNV", t63, "  IRR",  t70, "RATIO", t76, "  RRR",
     &       t82, "  SEV", t89, "FORST", t96, "TREES",
     &       t103, "HARV", t110,  "HARV", t117, "GIVEN", t124, "GIVEN"/,
     &     132("-"), /, "$#*%" )') sumTableId

!       Write Log Stock Volume/Value table header
         if (.not. noLogStockTable) then
            write (IOUT,'(1X, I5," $#*%", ///,"HARVEST VOLUME AND ",
     &        "GROSS VALUE REPORT", /, 115("-"), /, "$#*%")') logTableId
         end if
         return
      end subroutine writeTableHeaders

      end subroutine ECCALC
