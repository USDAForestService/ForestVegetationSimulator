      SUBROUTINE ECINIT
C----------
C **ECINIT--ECON  DATE OF LAST REVISION: 09/02/2010
C----------
C Author Fred Martin, WA DNR,

! Initializes defaults for ECON extension variables at beginning of each simulation.

! Called from INITRE, once for each simulated stand.

      implicit none

      include 'PRGPRM.F77'                                               !required by ECNCOM
      include 'ECNCOM.F77'

      integer i

!    Initialize control variables
      discountRate    =   0.0
      econStartYear   = -9999                                            !Used to ensure correct start year
      noLogStockTable = .false.
      noOutputTables  = .false.
      isEconToBe      = .false.
      pctMinDbh       =   0.0
      pctMinUnits     =     0
      pctMinVolume    =   0.0
      isFirstEcon     = .true.
      doSev           = .false.
      sevInput        =   0.0
      call setPretendStatus()                                            !Entry in ecstatus.f, initializes PRETEND state

!    Initialize volume accumulators.
      dbhSq     = 0.0
      harvest   = 0.0                                                    !Harvest volume array (TPA : FT3_100)
      hrvCostBf = 0.0; hrvCostFt3 = 0.0; hrvCostTpa = 0.0                !Harvest volume by cost type arrays (1:MAX_KEYWORDS)
      logBfVol  = 0.0; logDibBf   = 0.0; logFt3Vol  = 0.0                !Log arrays (1:MAXTRE, 1:MAX_LOGS)
      logDibFt3 = 0.0
      pctBf     = 0.0; pctFt3     = 0.0; pctTpa     = 0.0                !PCT harvest volume by cost type arrays (1:MAX_KEYWORDS)

!    Initialize revenue arrays
      hrvRevCnt   =   0                                                  !Array(1:MAXSP, 1:MAX_REV_UNITS)
      hrvRevDia   = 0.0                                                  !Array(1:MAXSP, 1:MAX_REV_UNITS, 1:MAX_KEYWORDS)
      hrvRevPrice = 0.0                                                  !Array(1:MAXSP, 1:MAX_REV_UNITS, 1:MAX_KEYWORDS)
      revVolume   = 0.0                                                  !Array(1:MAXSP, 1:MAX_REV_UNITS, 1:MAX_KEYWORDS)
      hrvRevRate  = 0.0                                                  !Array(1:MAXSP, 1:MAX_REV_UNITS, 1:MAX_KEYWORDS, 1:MAX_RATES)
      hrvRevDur   =   0                                                  !Array(1:MAXSP, 1:MAX_REV_UNITS, 1:MAX_KEYWORDS, 1:MAX_RATES)

!    Initialize cost variable arrays.
      annCostCnt      =     0
      annCostAmt      =   0.0                                            !Array(1:MAX_KEYWORDS)
      annRevCnt       =   0
      annRevAmt       =   0.0                                            !Array(1:MAX_KEYWORDS)
      burnCostAmt     =   0.0
      fixHrvAmt       =   0.0                                            !Array(1:MAX_KEYWORDS)
      fixHrvCnt       =     0
      fixPctAmt       =   0.0                                            !Array(1:MAX_KEYWORDS)
      fixPctCnt       =     0
      mechCostAmt     =   0.0
      plntCostCnt     =     0
      plntCostAmt     =   0.0                                            !Array(1:MAX_PLANT_COSTS)
      plntCostUnits   =   0.0                                            !Array(1:MAX_PLANT_COSTS)
      varHrvAmt       =   0.0                                            !Array(1:MAX_KEYWORDS)
      varHrvCnt       =     0
      varHrvDbhLo     =   0.0                                            !Array(1:MAX_KEYWORDS)
      varHrvDbhHi     = 999.0                                            !Array(1:MAX_KEYWORDS)
      varHrvUnits     =   0.0                                            !Array(1:MAX_KEYWORDS)
      varPctAmt       =   0.0                                            !Array(1:MAX_KEYWORDS)
      varPctCnt       =     0
      varPctDbhLo     =   0.0                                            !Array(1:MAX_KEYWORDS)
      varPctDbhHi     = 999.0                                            !Array(1:MAX_KEYWORDS)
      varPctUnits     =   0.0                                            !Array(1:MAX_KEYWORDS)

!    Initialize cost & rate/duration arrays, needed as not all elements may have values assigned.
      annCostRate  = 0.0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      annCostDur   =   0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      annRevRate   = 0.0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      annCostDur   =   0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      burnCostDur  =   0                                                 !Array(1:MAX_RATES)
      burnCostRate = 0.0                                                 !Array(1:MAX_RATES)
      fixHrvDur    =   0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      fixHrvRate   = 0.0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      fixPctDur    =   0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      fixPctRate   = 0.0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      mechCostDur  =   0                                                 !Array(1:MAX_RATES)
      mechCostRate = 0.0                                                 !Array(1:MAX_RATES)
      plntCostDur  =   0                                                 !Array(1:MAX_PLANT_COSTS, 1:MAX_RATES)
      plntCostRate = 0.0                                                 !Array(1:MAX_PLANT_COSTS, 1:MAX_RATES)
      varHrvDur    =   0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      varHrvRate   = 0.0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      varPctDur    =   0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)
      varPctRate   = 0.0                                                 !Array(1:MAX_KEYWORDS, 1:MAX_RATES)

      lbsFt3Amt    = 0.0                                                 !Array(1:MAXSP)

      return
      end
