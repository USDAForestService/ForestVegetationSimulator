C COMMON BLOCK FOR FVS/ECON REQUIRED SAVE VARIABLES
C----------
C COMMON $Id$
C----------
C Author Fred Martin, WA DNR
C Created to permit start and re-start of FVS without use of "save" variable functionality

      integer, parameter :: 
     &    MAX_YEARS    = MAXCYC * 20,                                   !Logic - max cycles x 20 yrs/cycle, max cycles=40 from PRGPRM.F77
     &    MAX_HARVESTS = INT(0.5 * (MAXCYC * MAXSP + 1)),               !Logic - 1/2 # cycles have harvest x # species, max species from PRGPRM.F77
     &    MAX_COSTS    = INT(0.5 * (MAXCYC * 4*MAX_KEYWORDS + 1))       !Logic - 1/2 # cycles have harvest x max # keyword for 4 types costs, max KEYWORDS from ECNCOM.F77

! Save state variables in eccalc.f needed for control and accumulation values needed across cycles for ECON initiation/re-initiation
      integer  burnCnt, hrvCstCnt, hrvRvnCnt, logTableId, mechCnt,
     &         startYear, specCstCnt, specRvnCnt, sumTableId
      real     costDisc, costUndisc, rate, revDisc, revUndisc,
     &         sevAnnCst, sevAnnRvn
      real, dimension(MAX_YEARS)    :: undiscCost, undiscRev
!    burnCnt     - Accumulates number of burn site prep events, only needed when calculating SEV
!    costDisc    - Accumulated discounted costs over time
!    costUndisc  - Accumulated undiscounted costs over time
!    hrvCstCnt   - Accumulates number of harvest cost events and provides array index for the different cost variables
!    hrvRvnCnt   - Accumulates number of harvest revenue events and provides array index for the different revenue variables
!    logTableId  - Output table ID from genrpt.f for log/stock table
!    mechCnt     - Accumulates number of mechanical site prep events, only needed when calculating SEV
!    rate        - Interest or discount rate
!    specCstCnt  - Accumulates number of special cost events, only needed when calculating SEV
!    specRvnCnt  - Accumulates number of special cost events, only needed when calculating SEV
!    startYear   - Year ECON was initially started or re-initialized
!    sumTableId  - Output table ID from genrpt.f for ECON summary tablee
!    undiscCost  - Array containing undiscounted costs for each simulation year
!                  Array bounds (MAX_YEARS)
!    undiscRev   - Array containing undiscounted revenues for each simulation year
!                  Array bounds (MAX_YEARS)
!    revDisc     - Accumulated discounted revenues over economic analysis period
!    revUndisc   - Accumulated un-discounted revenues over economic analysis period
!    sevAnnCst   - The accumulated SEV of an annually incurred cost
!    sevAnnRvn   - The accumulated SEV of an annually received revenue
! Save state variables in eccalc.f needed for computing SEV over multiple cycles
      integer, dimension(MAX_COSTS)    :: hrvCstKeywd, hrvCstTime,
     &                                    hrvCstTyp
      integer, dimension(MAX_HARVESTS) :: hrvRvnKeywd, hrvRvnSp,
     &                                    hrvRvnTime, hrvRvnUnits
      real, dimension(MAX_COSTS)       :: hrvCstAmt
      real, dimension(MAX_HARVESTS)    :: hrvRvnAmt
!    hrvCstAmt   - Array containing total cost over time for different cost types for each cycle w/ harvest
!                  Array bounds (MAX_COSTS)
!    hrvCstKeywd - Array containing an index for tracking different harvest costs for each cycle w/ harvest
!                  Array bounds (MAX_COSTS)
!    hrvCstTime  - Array containing time when harvest cost occurred for each cycle w/ harvest
!                  Array bounds (MAX_COSTS)
!    hrvCstTyp   - Array containing the type of harvest costs, Hrv, PCT, fixed variable, etc., for each cycle w/ harvest
!                  Array bounds (MAX_COSTS)
!    hrvRvnAmt   - Array containing the total revenue over time
!                  Array bounds (MAX_HARVESTS)
!    hrvRvnKeywd - Array containing an index for tracking different harvest revenues
!                  Array bounds (MAX_HARVESTS)
!    hrvRvnSp    - Array containing an index to the species for a revenue keyword
!                  Array bounds (MAX_HARVESTS)
!    hrvRvnTime  - Array contining time when harvest revenue occurred
!                  Array bounds (MAX_HARVESTS)
!    hrvRvnUnits - Array continint the unit of measure for the harvest revenue keyword
!                  Array bounds (MAX_HARVESTS)

! Save state in ecstatus.f needed for "pretend" functionality variables
      integer pretendStartYear, pretendEndYear
      logical isPretendActive
!    pretendStartYear - Year that pretend functionality is started
!    pretendEndYear   - Year that pretend functionality will end
!    isPretendActive  - Logical identifying that pretend functionality is currently operating

      COMMON /ECONSAVE/ burnCnt, costDisc, costUndisc, 
     & hrvCstAmt, hrvCstCnt, hrvCstKeywd,
     & hrvCstTime, hrvCstTyp, hrvRvnAmt, hrvRvnCnt, hrvRvnKeywd, 
     & hrvRvnSp, hrvRvnTime, hrvRvnUnits, logTableId, mechCnt,
     & rate, revDisc, revUndisc, sevAnnCst, sevAnnRvn, startYear, 
     & specCstCnt, specRvnCnt, sumTableId, undiscCost, undiscRev,
     & pretendStartYear, pretendEndYear, isPretendActive

