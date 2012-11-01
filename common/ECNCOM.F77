C COMMON BLOCK FOR FVS/ECON
C----------
C  **ECON   DATE OF LAST REVISION:  10/01/12
C----------
C Author Fred Martin, WA DNR

C Variables from FVS
C   MAXSP  - maximum number of species codes used by a specific variant, from PRGPRM.F77
C   MAXTRE - maximum number of tree records that FVS can process, from PRGPRM.F77

      integer, parameter :: MAX_KEYWORDS = 8, MAX_LOGS = 20,
     & MAX_PLANT_COSTS = 2, MAX_RATES = 8, MAX_REV_UNITS = 5,
     & TPA         = 1, BF_1000  = 2, FT3_100  = 3, BF_1000_LOG = 4,     !Units-of-measure
     & FT3_100_LOG = 5, PER_ACRE = 6, TPA_1000 = 7,
     & PRETEND_ACTIVITY   = 2605, SEV_BEGIN_ACTIVITY = 2606,             !ECON activity codes listed in OPLIST
     & SPEC_COST_ACTIVITY = 2607, SPEC_REV_ACTIVITY  = 2608,
     & ECON_START_YEAR    = 2609

      character (len=12), dimension (7), parameter :: UNITS_LABEL=
     & (/ 'REMOVED TPA ', '1000 BF-TREE', '100 FT3-TREE',
     &    '1000 BF-LOGS', '100 Ft3-LOGS', 'ACRE        ',
     &    '1000 TPA PLT' /)

      integer annCostCnt, annRevCnt, econStartYear, fixHrvCnt,
     &         fixPctCnt, pctMinUnits, plntCostCnt, varHrvCnt, varPctCnt

      integer, dimension (MAX_KEYWORDS) :: varHrvUnits, varPctUnits

      integer, dimension (MAX_PLANT_COSTS)      :: plntCostUnits

      integer, dimension (MAXSP, MAX_REV_UNITS) :: hrvRevCnt

      integer, dimension (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS) ::
     &                                                     hrvRevDiaIndx

      logical doSev, noLogStockTable, noOutputTables, isEconToBe,
     &                                                       isFirstEcon

      logical, dimension(MAXSP, MAX_REV_UNITS) :: hasRevAmt

      real burnCostAmt, dbhSq, discountRate, mechCostAmt, pctMinDbh,
     &                                            pctMinVolume, sevInput

      real, dimension (MAXSP)        :: lbsFt3Amt

      real, dimension(TPA : FT3_100) :: harvest

      real, dimension (MAX_KEYWORDS) :: annCostAmt, annRevAmt,
     &                                  fixHrvAmt, fixPctAmt, hrvCostBf, 
     &                                  hrvCostFt3,hrvCostTpa, 
     &                                  pctBf, pctFt3, pctTpa, 
     &                                  varHrvAmt, varHrvDbhLo, 
     &                                  varHrvDbhHi, varPctAmt, 
     &                                  varPctDbhLo, varPctDbhHi

      real, dimension (MAX_PLANT_COSTS) :: plntCostAmt

      integer, dimension (MAX_RATES)    :: burnCostDur, mechCostDur

      real, dimension (MAX_RATES)       :: burnCostRate, mechCostRate

      integer, dimension (MAX_KEYWORDS, MAX_RATES) :: annCostDur,
     &             annRevDur, fixHrvDur, fixPctDur, varHrvDur, varPctDur

      integer, dimension (MAX_PLANT_COSTS, MAX_RATES) :: plntCostDur

      real, dimension (MAX_KEYWORDS, MAX_RATES) :: annCostRate,
     &        annRevRate, fixHrvRate, fixPctRate, varHrvRate, varPctRate

      real, dimension (MAX_PLANT_COSTS, MAX_RATES) :: plntCostRate

      real, dimension (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS) ::
     &                                 hrvRevPrice, hrvRevDia, revVolume

      integer, dimension (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS, MAX_RATES)
     &                                                      :: hrvRevDur

      real, dimension (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS, MAX_RATES) ::
     &                                                        hrvRevRate

      real, dimension (MAXTRE, MAX_LOGS) :: logBfVol,  logDibBf,
     &                                      logFt3Vol, logDibFt3

      COMMON /ECON1/
     & annCostCnt, annCostAmt, annCostRate, annCostDur,
     & annRevCnt, annRevAmt,  annRevRate, annRevDur,
     & burnCostAmt, burnCostRate, burnCostDur,
     & dbhSq, discountRate, doSev, econStartYear,
     & fixHrvCnt, fixHrvAmt, fixHrvRate, fixHrvDur,
     & fixPctCnt, fixPctAmt, fixPctRate, fixPctDur,
     & harvest, hasRevAmt, hrvCostBf, hrvCostFt3, hrvCostTpa,
     & hrvRevCnt, hrvRevPrice, hrvRevDia,
     & hrvRevDiaIndx, hrvRevRate, hrvRevDur,
     & isEconToBe, lbsFt3Amt, logBfVol, logDibBf, logFt3Vol, LogDibFt3,
     & mechCostAmt, mechCostRate, mechCostDur,
     & noLogStockTable, noOutputTables,
     & pctBf, pctFt3, pctMinDbh, pctMinUnits, pctMinVolume,
     & pctTpa, plntCostCnt, plntCostAmt, plntCostUnits, plntCostDur,
     & plntCostRate, revVolume, sevInput,
     & varPctCnt, varPctAmt, varPctUnits, varPctDbhLo, varPctDbhHi,
     & varPctRate, varPctDur,
     & varHrvCnt, varHrvAmt, varHrvUnits, varHrvDbhLo, varHrvDbhHi,
     & varHrvRate, varHrvDur, isFirstEcon

! VARIABLE DICTIONARY
C**************************************************************************************
! Parameters:

! MAX_KEYWORDS  - Maximum number of keywords of a given type.  Multiple keywords of a single type usually represent either
!                 different DBH-classes such as in variable harvest cost or harvest revenue keywords, or multiple keywords
!                 of the same type may represent alternative similar costs as with annual cost or fixed thinning cost keywords.
!                 For example, a maximum of MAX_KEYWORDS keywords may be entered for the PCTVRCST keyword, each with a different
!                 DBH-class specifier (i.e., smallest DBH to which the keyword cost applies).

! MAX_LOGS      - Maximum number of logs within a tree. Value based on FVS parameter for maximum number of logs.

! MAX_RATES     - Maximum number of value rate-changes and duration-periods allowed for a keyword record.

! MAX_REV_UNITS - Maximum number of revenue units, e.g., TPA, board-feet, cubic-feet, etc., supported.
C**************************************************************************************
! Variables:

! annCostCnt   - Number of annual cost (ANNCOST) keywords submitted.

! annCostAmt   - Array containing the cost amount per acre for an annual cost.
!                Array bounds (MAX_KEYWORDS)

! annCostRate  - Array containing the rate in percent by which the annCostAmt appreciates over time.
!                Array bounds (MAX_KEYWORDS, MAX_RATES)

! annCostDur   - Array containing the duration in years that an associated annCostRate is to be applied to an annCostAmt.
!                Array bounds (MAX_KEYWORDS, MAX_RATES)

! annRevCnt   - Number of annual cost (ANNUREV) keywords submitted.

! annRevAmt   - Array containing the cost amount per acre for an annual cost.
!               Array bounds (MAX_KEYWORDS)

! annRevRate  - Array containing the rate in percent by which the annRevAmt appreciates over time.
!               Array bounds (MAX_KEYWORDS, MAX_RATES)

! annRevDur   - Array containing the duration in years that an associated annRevRate is to be applied to an annRevAmt.
!               Array bounds (MAX_KEYWORDS, MAX_RATES)

! burnCostAmt   - Scaler of the cost amount  when "Full" Establishment Extension burning occurs.

! burnCostRate  - Array containing the rate in percent by which the burnCostAmt appreciates over time.
!                 Array bounds (MAX_RATES)

! burnCostDur   - Array containing the duration in years that an associated burnCostRate is to be applied to a burnCostAmt.
!                 Array bounds (MAX_RATES)

! dbhSq - Scaler of sum of diameters squared DBH of all trees removed in a harvest.

! discountRate - Discount rate in percen.

! doSev - logical indicating that SEV is to be computed

! econStartYear - Scaler specifying the year economic calculations will begin, set to >=1.

! fixHrvCnt  - Number of fixed harvest cost (HRVFXCST) keywords submitted.

! fixHrvAmt  - Array containing the fixed harvest cost amount for commercial harvests by acre.
!              Array bounds (MAX_KEYWORDS)

! fixHrvRate - Array containing the rate in percent by which the HRVFXCST keyword cost amount appreciate over time.
!              Array bounds (MAX_KEYWORDS, MAX_RATES)

! fixHrvDur  - Array containing the duration in years that an associated fixHrvRate is to be applied to a fixHrvAmt.
!              Array bounds (MAX_KEYWORDS, MAX_RATES)

! fixPctCnt  - Number of fixed PCT cost (PCTFXCST) keywords submitted.

! fixPctAmt  - Array containing the fixed Pct cost amount for PCT per acre.
!              Array bounds (MAX_KEYWORDS)

! fixPctRate - Array containing the rate in percent by which the fixPctAmt appreciates over time.
!              Array bounds (MAX_KEYWORDS, MAX_RATES)

! fixPctDur  - Array containing the duration in years that an associated fixPctRate is to be applied to a fixPctAmt.
!              Array bounds (MAX_KEYWORDS, MAX_RATES)

! harvest - Array containing accumulated harvested tpa, board-foot volume, and cubic-foot volume during a cycle.
!           Array bounds (1:3), i.e., (TPA : FT3_100)

! hasRevAmt - Array indicating if revenues have been specified for a specific species.  Used to control assigning
!             values to ALL species without overwriting previous species.
!             Array bounds (MAXSP, unitId : MAX_REV_UNITS)

! hrvCostBf  - Array containing net board foot volume of harvested trees by HRVVRCST keyword DBH-classes, array order same as HRVVRCST variables.
!              Array bounds (MAX_KEYWORDS)

! hrvCostFt3 - Array containing net cubic foot volume of harveted trees by HRVVRCST keyword DBH-classes, array order same as HRVVRCST keyword variaabless.
!              Array bounds (MAX_KEYWORDS)

! hrvCostTpa - Array containing number of harvested trees per acre by HRVVRCST keyword DBH-classes, array order same as HRVVRCST variables.
!              Array bounds (MAX_KEYWORDS)

! hrvRevCnt     - Array containing number of HRVRVN keywords submitted by species by quantity-unit type; the count equals number of
!                 DIB/DBH classes for which keywords were submitted.
!                 Array bounds (MAXSP, MAX_REV_UNITS)

! hrvRevPrice   - Array containing price by species by quantity-unit by diameter class.
!                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)

! hrvRevDia     - Array containing minimum small-end DIB/DBH by species by quantity-unit type to which the hrvRevPrice applies.
!                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)

! hrvRevDiaIndx - Array of values in descending DIB/DBH order specifying the original indexing-order of HRVRVN keywords.
!                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)

! hrvRevRate    - Array containing rate in percent by which the hrvRevPrice appreciates for a DIB/DBH class, species, and quantity-unit type.
!                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS, MAX_RATES)

! hrvRevDur     - Array containing duration in years that a hrvRevRate is used to appreciate/depreciate a hrvRevPrice for a DIB/DBH class, species, and quantity-unit type.
!                 Array bounds (MAXSP, MAX_REV_UNITS, 1, MAX_RATES)

! isEconToBe - Logical variable indicating if ECON will be potentially run sometime during simulation.
!              Cannot be checked for each cycle because VOLS occurs before event monitor.

! isFirstEcon- Logical controling operations for first pass through ECCALC

! lbsFt3Amt - Array of species-specific conversion constants for converting one cubic foot volume to pounds biomass.
!             Array bounds (MAXSP)

! logDibBf  - Array of small-end diameters inside bark (DIB) for board-foot volume ofeach log of every tree in the simulation.
!             Will have values only if volume calculation method performs log-by-log merchandising.
!             Array bounds (treeId : MAXTRE, MAX_LOGS)

! logDibFt3 - Array of small-end diameters inside bark (DIB) for cubic-foot volume of each log of every tree in the simulation.
!             Will have values only if volume calculation method performs log-by-log merchandising.
!             Array bounds (treeId : MAXTRE, MAX_LOGS)

! logBfVol  - Array of board-foot volume for each log of every tree in the simulation.
!             Will have values only if volume calculation method performs log-by-log merchandising.
!             Array bounds (treeId : MAXTRE, MAX_LOGS)

! logFt3Vol - Array of cubic-foot volume for each log of every tree in the simulation.
!             Will have values only if volume calculation method performs log-by-log merchandising.
!             Array bounds (treeId : MAXTRE, MAX_LOGS)

! mechCostAmt  - Scaler containing the cost per acre when "Full" Establishment Extension mechanical site-prep occurs.

! mechCostRate - Array containing the rate in percent by which the mechCostAmt appreciates over time.
!                Array bounds (MAX_RATES)

! mechCostDur  - Array containing the duration in years that an associated mechCostRate is to be applied to a mechCostAmt.
!                Array bounds (MAX_RATES)

! noLogStockTable - Logical variable indicating if the Log Stock Volume/Value table is to be output; TRUE=no table will be output

! noOutputTables - Logical variable indicating that no ECON output tables are to be produced; TRUE=no tables will be output

! plntCostCnt   - Number of planting cost (PLANTCST) keywords submitted.

! plntCostAmt   - Array containing the cost amount by specified units of measure when Establishment Extension planting occurs.
!                 Array bounds (MAX_PLANT_COSTS)

! plntCostUnits - Array containing the units of volume to which the plntCostAmt applies.
!                 Array bounds (MAX_PLANT_COSTS)

! plntCostRate  - Array containing the rate in percent by which the plntCostAmt appreciates over time.
!                 Array bounds (MAX_PLANT_COSTS, MAX_RATES)

! plntCostDur   - Array containing the duration in years that an associated plntCostRate is to be applied to a plntCostAmt.
!                 Array bounds (MAX_PLANT_COSTS, MAX_RATES)

! pctBf        - Array containing net board foot volume of harvested trees by PCT variable cost DBH-classes, array order same as PCT variable cost keywords.
!                Array bounds (MAX_KEYWORDS)

! pctFt3       - Array containing net cubic foot volume of harveted trees by PCT variable cost DBH-classes, array order same as PCT variable cost keywords.
!                Array bounds (MAX_KEYWORDS)

! pctDbhIndex  - Array of values in descending DBH order specifying the indexing of PCTVRCST keywords by their DBH-class specifier.
!                Array bounds (MAX_KEYWORDS)

! pctMinDbh    - Variable containing the threshold quadratic mean diameter (DBH) that differentiates a pre-commercial thinning from
!                a commercial harvest. Harvests of trees >= DBH value are commercial and are valued via the HRVRVN keyword.

! pctMinUnits  - Variable containing the units of volume used to specify minimum pct volume.

! pctMinVolume - Variable containing the threshold bfv/cfv value that differentiates a pre-commercial thinning treatment
!                from a commercial harvest. Harvests that are >= to the specified value are valued via the HRVRVN keyword.

! pctTpa - Array containing number of harvested trees per acre by PCT variable cost DBH-classes, array order same as PCT variable cost keywords.
!          Array bounds (MAX_KEYWORDS)

! revVolume - Array containing number and net volume of harvested trees by speices, revenue units-of-measure and revenue diameter-class.
!             Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)

! sevInput - Scaler specifying dollar value of a user-input soil expectation value from SEVKNOWN keyword.

! varHrvCnt   - Number of variable harvest cost (HRVVRCST) keywords submitted.

! varHrvAmt   - Array containing the variable harvest cost amount by specified volume units units for commercial harvest of trees >= a specified DBH.
!               Array bounds (MAX_KEYWORDS)

! hrvCostDbhIndex - Array of values in descending DBH order specifying the indexing of HRVVRCST keywords by their DBH-class specifier.
!                   Array bounds (MAX_KEYWORDS)

! varHrvDbh   - Array containing the minimum tree DBH ssociated with a commercial harvest variable cost.
!               Array bounds (MAX_KEYWORDS)

! varHrvUnits - Array containing the volume units to which the varHrvAmt applies.
!               Array bounds (MAX_KEYWORDS)

! varHrvRate  - Array containing the rate in percent by which the varHrvAmt appreciates over time for an associated tree DBH-size set.
!               Array bounds (MAX_KEYWORDS, MAX_RATES)

! varHrvDur   - Array containing the duration in years that an associated varHrvRate is to be
!               applied to a varHrvAmt for a specified tree DBH-size set.
!               Array bounds (MAX_KEYWORDS, MAX_RATES)

! varPctCnt   -  Number of variable PCT cost (PCTVRCST) keywords submitted.

! varPctAmt   - Array containing the cost amount by specified volume units for PCT of trees >= a specified DBH.
!               Array bounds (MAX_KEYWORDS)

! varPctDbh   - Array containing the minimum tree DBH ssociated with a PCT variable cost.
!               Array bounds (MAX_KEYWORDS)

! varPctUnits - Array containing the units of volume to which the PCTVRCST keyword cost applies.
!               Array bounds (MAX_KEYWORDS)

! varPctRate  - Array containing the rate in percent by which the varPctAmt appreciate over time for an associated tree DBH-size set.
!               Array bounds (MAX_KEYWORDS, MAX_RATES)

! varPctDur   - Array containing the duration in years that an associated varPctRate is to be applied to a varPctAmt for a specified tree DBH-size set.
!               Array bounds (MAX_KEYWORDS, MAX_RATES)
