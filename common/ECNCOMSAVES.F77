C COMMON BLOCK FOR FVS/ECON REQUIRED SAVE VARIABLES
C----------
C COMMON $Id$
C----------
C Author Fred Martin, WA DNR
C Created to permit start and re-start of FVS without use of "save" variable functionality
C----------
C
      integer, parameter :: 
     &    MAX_YEARS    = MAXCYC * 20,                                   !Logic - max cycles x 20 yrs/cycle, max cycles=40 from PRGPRM.F77
     &    MAX_HARVESTS = INT(0.5 * (MAXCYC * MAXSP + 1)),               !Logic - 1/2 # cycles have harvest x # species, max species from PRGPRM.F77
     &    MAX_COSTS    = INT(0.5 * (MAXCYC * 4*MAX_KEYWORDS + 1))       !Logic - 1/2 # cycles have harvest x max # keyword for 4 types costs, max KEYWORDS from ECNCOM.F77
C----------
C  Save state variables in eccalc.f needed for control and accumulation values needed across cycles for ECON initiation/re-initiation
C----------
      integer  burnCnt, hrvCstCnt, hrvRvnCnt, logTableId, mechCnt,
     &         startYear, specCstCnt, specRvnCnt, sumTableId
C
      real     costDisc, costUndisc, rate, revDisc, revUndisc,
     &         sevAnnCst, sevAnnRvn
C
      real, dimension(MAX_YEARS)    :: undiscCost, undiscRev
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C    burnCnt     - Accumulates number of burn site prep events, only needed when calculating SEV
C    costDisc    - Accumulated discounted costs over time
C    costUndisc  - Accumulated undiscounted costs over time
C    hrvCstCnt   - Accumulates number of harvest cost events and provides array index for the different cost variables
C    hrvRvnCnt   - Accumulates number of harvest revenue events and provides array index for the different revenue variables
C    logTableId  - Output table ID from genrpt.f for log/stock table
C    mechCnt     - Accumulates number of mechanical site prep events, only needed when calculating SEV
C    rate        - Interest or discount rate
C    specCstCnt  - Accumulates number of special cost events, only needed when calculating SEV
C    specRvnCnt  - Accumulates number of special cost events, only needed when calculating SEV
C    startYear   - Year ECON was initially started or re-initialized
C    sumTableId  - Output table ID from genrpt.f for ECON summary tablee
C    undiscCost  - Array containing undiscounted costs for each simulation year
C                  Array bounds (MAX_YEARS)
C    undiscRev   - Array containing undiscounted revenues for each simulation year
C                  Array bounds (MAX_YEARS)
C    revDisc     - Accumulated discounted revenues over economic analysis period
C    revUndisc   - Accumulated un-discounted revenues over economic analysis period
C    sevAnnCst   - The accumulated SEV of an annually incurred cost
C    sevAnnRvn   - The accumulated SEV of an annually received revenue
C----------
C  Save state variables in eccalc.f needed for computing SEV over multiple cycles
C----------
C
      integer, dimension(MAX_COSTS)    :: hrvCstKeywd, hrvCstTime,
     &                                    hrvCstTyp
      integer, dimension(MAX_HARVESTS) :: hrvRvnKeywd, hrvRvnSp,
     &                                    hrvRvnTime, hrvRvnUnits
C
      real, dimension(MAX_COSTS)       :: hrvCstAmt
      real, dimension(MAX_HARVESTS)    :: hrvRvnAmt
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C    hrvCstAmt   - Array containing total cost over time for different cost types for each cycle w/ harvest
C                  Array bounds (MAX_COSTS)
C    hrvCstKeywd - Array containing an index for tracking different harvest costs for each cycle w/ harvest
C                  Array bounds (MAX_COSTS)
C    hrvCstTime  - Array containing time when harvest cost occurred for each cycle w/ harvest
C                  Array bounds (MAX_COSTS)
C    hrvCstTyp   - Array containing the type of harvest costs, Hrv, PCT, fixed variable, etc., for each cycle w/ harvest
C                  Array bounds (MAX_COSTS)
C    hrvRvnAmt   - Array containing the total revenue over time
C                  Array bounds (MAX_HARVESTS)
C    hrvRvnKeywd - Array containing an index for tracking different harvest revenues
C                  Array bounds (MAX_HARVESTS)
C    hrvRvnSp    - Array containing an index to the species for a revenue keyword
C                  Array bounds (MAX_HARVESTS)
C    hrvRvnTime  - Array contining time when harvest revenue occurred
C                  Array bounds (MAX_HARVESTS)
C    hrvRvnUnits - Array continint the unit of measure for the harvest revenue keyword
C                  Array bounds (MAX_HARVESTS)
C----------
C  Save state in ecstatus.f needed for "pretend" functionality variables
C----------
C
      logical isPretendActive
C
      integer pretendStartYear, pretendEndYear
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C    pretendStartYear - Year that pretend functionality is started
C    pretendEndYear   - Year that pretend functionality will end
C    isPretendActive  - Logical identifying that pretend functionality is currently operating
C----------
C
      COMMON /ECONSAVE/ burnCnt, costDisc, costUndisc, 
     & hrvCstAmt, hrvCstCnt, hrvCstKeywd,
     & hrvCstTime, hrvCstTyp, hrvRvnAmt, hrvRvnCnt, hrvRvnKeywd, 
     & hrvRvnSp, hrvRvnTime, hrvRvnUnits, logTableId, mechCnt,
     & rate, revDisc, revUndisc, sevAnnCst, sevAnnRvn, startYear, 
     & specCstCnt, specRvnCnt, sumTableId, undiscCost, undiscRev,
     & pretendStartYear, pretendEndYear, isPretendActive

