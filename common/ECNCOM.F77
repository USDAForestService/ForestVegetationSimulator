C COMMON BLOCK FOR FVS/ECON
C----------
C COMMON $Id$
C----------
C Author Fred Martin, WA DNR
C
C Variables from FVS
C   MAXSP  - maximum number of species codes used by a specific variant, from PRGPRM.F77
C   MAXTRE - maximum number of tree records that FVS can process, from PRGPRM.F77
C----------
C
      character (len=12), dimension (7), parameter :: UNITS_LABEL=
     & (/ 'REMOVED TPA ', '1000 BF-TREE', '100 FT3-TREE',
     &    '1000 BF-LOGS', '100 Ft3-LOGS', 'ACRE        ',
     &    '1000 TPA PLT' /)
C
      integer, parameter :: MAX_KEYWORDS = 8, MAX_LOGS = 20,
     & MAX_PLANT_COSTS = 2, MAX_RATES = 8, MAX_REV_UNITS = 5,
     & TPA         = 1, BF_1000  = 2, FT3_100  = 3, BF_1000_LOG = 4,     !Units-of-measure
     & FT3_100_LOG = 5, PER_ACRE = 6, TPA_1000 = 7,
     & PRETEND_ACTIVITY   = 2605, SEV_BEGIN_ACTIVITY = 2606,             !ECON activity codes listed in OPLIST
     & SPEC_COST_ACTIVITY = 2607, SPEC_REV_ACTIVITY  = 2608,
     & ECON_START_YEAR    = 2609
C
      integer annCostCnt, annRevCnt, econStartYear, fixHrvCnt,
     &         fixPctCnt, pctMinUnits, plntCostCnt, varHrvCnt, varPctCnt
C
      integer, dimension (MAX_KEYWORDS) :: varHrvUnits, varPctUnits
C
      integer, dimension (MAX_PLANT_COSTS)      :: plntCostUnits
C
      integer, dimension (MAXSP, MAX_REV_UNITS) :: hrvRevCnt
C
      integer, dimension (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS) ::
     &                                                     hrvRevDiaIndx
C
      logical doSev, noLogStockTable, noOutputTables, isEconToBe,
     &                                                       isFirstEcon
C
      logical, dimension(MAXSP, MAX_REV_UNITS) :: hasRevAmt
C
      real burnCostAmt, dbhSq, discountRate, mechCostAmt, pctMinDbh,
     &     pctMinVolume, sevInput
C
      real, dimension (MAXSP)        :: lbsFt3Amt
C
      real, dimension(TPA : FT3_100) :: harvest
C
      real, dimension (MAX_KEYWORDS) :: 
     &        annCostAmt, annRevAmt,
     &        fixHrvAmt, fixPctAmt, hrvCostBf, 
     &        hrvCostFt3,hrvCostTpa, 
     &        pctBf, pctFt3, pctTpa, 
     &        varHrvAmt, varHrvDbhLo, 
     &        varHrvDbhHi, varPctAmt, 
     &        varPctDbhLo, varPctDbhHi
C
      real, dimension (MAX_PLANT_COSTS) :: plntCostAmt
C
      integer, dimension (MAX_RATES)    :: burnCostDur, mechCostDur
C
      real, dimension (MAX_RATES)       :: burnCostRate, mechCostRate
C
      integer, dimension (MAX_KEYWORDS, MAX_RATES) :: annCostDur,
     &       annRevDur, fixHrvDur, fixPctDur, varHrvDur, varPctDur
C
      integer, dimension (MAX_PLANT_COSTS, MAX_RATES) :: plntCostDur
C
      real, dimension (MAX_KEYWORDS, MAX_RATES) :: annCostRate,
     &      annRevRate, fixHrvRate, fixPctRate, varHrvRate, varPctRate
C
      real, dimension (MAX_PLANT_COSTS, MAX_RATES) :: plntCostRate
C
      real, dimension (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS) ::
     &      hrvRevPrice, hrvRevDia, revVolume
C
      integer, dimension (MAXSP,MAX_REV_UNITS,MAX_KEYWORDS,MAX_RATES)
     &                                             :: hrvRevDur
C
      real, dimension (MAXSP,MAX_REV_UNITS,MAX_KEYWORDS,MAX_RATES) 
     &                                             ::  hrvRevRate
C
      real, dimension (MAXTRE, MAX_LOGS) :: logBfVol,  logDibBf,
     &                                      logFt3Vol, logDibFt3
C
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
C
C----------
C VARIABLE DEFINITIONS:
C----------
C Parameters:
C
C MAX_KEYWORDS  - Maximum number of keywords of a given type.  Multiple keywords of a single type usually represent either
C                 different DBH-classes such as in variable harvest cost or harvest revenue keywords, or multiple keywords
C                 of the same type may represent alternative similar costs as with annual cost or fixed thinning cost keywords.
C                 For example, a maximum of MAX_KEYWORDS keywords may be entered for the PCTVRCST keyword, each with a different
C                 DBH-class specifier (i.e., smallest DBH to which the keyword cost applies).
C
C MAX_LOGS      - Maximum number of logs within a tree. Value based on FVS parameter for maximum number of logs.
C
C MAX_RATES     - Maximum number of value rate-changes and duration-periods allowed for a keyword record.
C
C MAX_REV_UNITS - Maximum number of revenue units, e.g., TPA, board-feet, cubic-feet, etc., supported.
C
C----------
C Variables:
C
C annCostCnt   - Number of annual cost (ANNCOST) keywords submitted.
C
C annCostAmt   - Array containing the cost amount per acre for an annual cost.
C                Array bounds (MAX_KEYWORDS)
C
C annCostRate  - Array containing the rate in percent by which the annCostAmt appreciates over time.
C                Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C annCostDur   - Array containing the duration in years that an associated annCostRate is to be applied to an annCostAmt.
C                Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C annRevCnt   - Number of annual cost (ANNUREV) keywords submitted.
C
C annRevAmt   - Array containing the cost amount per acre for an annual cost.
C               Array bounds (MAX_KEYWORDS)
C
C annRevRate  - Array containing the rate in percent by which the annRevAmt appreciates over time.
C               Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C annRevDur   - Array containing the duration in years that an associated annRevRate is to be applied to an annRevAmt.
C               Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C burnCostAmt   - Scaler of the cost amount  when "Full" Establishment Extension burning occurs.
C
C burnCostRate  - Array containing the rate in percent by which the burnCostAmt appreciates over time.
C                 Array bounds (MAX_RATES)
C
C burnCostDur   - Array containing the duration in years that an associated burnCostRate is to be applied to a burnCostAmt.
C                 Array bounds (MAX_RATES)
C
C dbhSq - Scaler of sum of diameters squared DBH of all trees removed in a harvest.
C
C discountRate - Discount rate in percen.
C
C doSev - logical indicating that SEV is to be computed
C
C econStartYear - Scaler specifying the year economic calculations will begin, set to >=1.
C
C fixHrvCnt  - Number of fixed harvest cost (HRVFXCST) keywords submitted.
C
C fixHrvAmt  - Array containing the fixed harvest cost amount for commercial harvests by acre.
C              Array bounds (MAX_KEYWORDS)
C
C fixHrvRate - Array containing the rate in percent by which the HRVFXCST keyword cost amount appreciate over time.
C              Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C fixHrvDur  - Array containing the duration in years that an associated fixHrvRate is to be applied to a fixHrvAmt.
C              Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C fixPctCnt  - Number of fixed PCT cost (PCTFXCST) keywords submitted.
C
C fixPctAmt  - Array containing the fixed Pct cost amount for PCT per acre.
C              Array bounds (MAX_KEYWORDS)
C
C fixPctRate - Array containing the rate in percent by which the fixPctAmt appreciates over time.
C              Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C fixPctDur  - Array containing the duration in years that an associated fixPctRate is to be applied to a fixPctAmt.
C              Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C harvest - Array containing accumulated harvested tpa, board-foot volume, and cubic-foot volume during a cycle.
C           Array bounds (1:3), i.e., (TPA : FT3_100)
C
C hasRevAmt - Array indicating if revenues have been specified for a specific species.  Used to control assigning
C             values to ALL species without overwriting previous species.
C             Array bounds (MAXSP, unitId : MAX_REV_UNITS)
C
C hrvCostBf  - Array containing net board foot volume of harvested trees by HRVVRCST keyword DBH-classes, array order same as HRVVRCST variables.
C              Array bounds (MAX_KEYWORDS)
C
C hrvCostFt3 - Array containing net cubic foot volume of harveted trees by HRVVRCST keyword DBH-classes, array order same as HRVVRCST keyword variaabless.
C              Array bounds (MAX_KEYWORDS)
C
C hrvCostTpa - Array containing number of harvested trees per acre by HRVVRCST keyword DBH-classes, array order same as HRVVRCST variables.
C              Array bounds (MAX_KEYWORDS)
C
C hrvRevCnt     - Array containing number of HRVRVN keywords submitted by species by quantity-unit type; the count equals number of
C                 DIB/DBH classes for which keywords were submitted.
C                 Array bounds (MAXSP, MAX_REV_UNITS)
C
C hrvRevPrice   - Array containing price by species by quantity-unit by diameter class.
C                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)
C
C hrvRevDia     - Array containing minimum small-end DIB/DBH by species by quantity-unit type to which the hrvRevPrice applies.
C                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)
C
C hrvRevDiaIndx - Array of values in descending DIB/DBH order specifying the original indexing-order of HRVRVN keywords.
C                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)
C
C hrvRevRate    - Array containing rate in percent by which the hrvRevPrice appreciates for a DIB/DBH class, species, and quantity-unit type.
C                 Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS, MAX_RATES)
C
C hrvRevDur     - Array containing duration in years that a hrvRevRate is used to appreciate/depreciate a hrvRevPrice for a DIB/DBH class, species, and quantity-unit type.
C                 Array bounds (MAXSP, MAX_REV_UNITS, 1, MAX_RATES)
C
C isEconToBe - Logical variable indicating if ECON will be potentially run sometime during simulation.
C              Cannot be checked for each cycle because VOLS occurs before event monitor.
C
C isFirstEcon- Logical controling operations for first pass through ECCALC
C
C lbsFt3Amt - Array of species-specific conversion constants for converting one cubic foot volume to pounds biomass.
C             Array bounds (MAXSP)
C
C logDibBf  - Array of small-end diameters inside bark (DIB) for board-foot volume ofeach log of every tree in the simulation.
C             Will have values only if volume calculation method performs log-by-log merchandising.
C             Array bounds (treeId : MAXTRE, MAX_LOGS)
C
C logDibFt3 - Array of small-end diameters inside bark (DIB) for cubic-foot volume of each log of every tree in the simulation.
C             Will have values only if volume calculation method performs log-by-log merchandising.
C             Array bounds (treeId : MAXTRE, MAX_LOGS)
C
C logBfVol  - Array of board-foot volume for each log of every tree in the simulation.
C             Will have values only if volume calculation method performs log-by-log merchandising.
C             Array bounds (treeId : MAXTRE, MAX_LOGS)
C
C logFt3Vol - Array of cubic-foot volume for each log of every tree in the simulation.
C             Will have values only if volume calculation method performs log-by-log merchandising.
C             Array bounds (treeId : MAXTRE, MAX_LOGS)
C
C mechCostAmt  - Scaler containing the cost per acre when "Full" Establishment Extension mechanical site-prep occurs.
C
C mechCostRate - Array containing the rate in percent by which the mechCostAmt appreciates over time.
C                Array bounds (MAX_RATES)
C
C mechCostDur  - Array containing the duration in years that an associated mechCostRate is to be applied to a mechCostAmt.
C                Array bounds (MAX_RATES)
C
C noLogStockTable - Logical variable indicating if the Log Stock Volume/Value table is to be output; TRUE=no table will be output
C
C noOutputTables - Logical variable indicating that no ECON output tables are to be produced; TRUE=no tables will be output
C
C plntCostCnt   - Number of planting cost (PLANTCST) keywords submitted.
C
C plntCostAmt   - Array containing the cost amount by specified units of measure when Establishment Extension planting occurs.
C                 Array bounds (MAX_PLANT_COSTS)
C
C plntCostUnits - Array containing the units of volume to which the plntCostAmt applies.
C                 Array bounds (MAX_PLANT_COSTS)
C
C plntCostRate  - Array containing the rate in percent by which the plntCostAmt appreciates over time.
C                 Array bounds (MAX_PLANT_COSTS, MAX_RATES)
C
C plntCostDur   - Array containing the duration in years that an associated plntCostRate is to be applied to a plntCostAmt.
C                 Array bounds (MAX_PLANT_COSTS, MAX_RATES)
C
C pctBf        - Array containing net board foot volume of harvested trees by PCT variable cost DBH-classes, array order same as PCT variable cost keywords.
C                Array bounds (MAX_KEYWORDS)
C
C pctFt3       - Array containing net cubic foot volume of harveted trees by PCT variable cost DBH-classes, array order same as PCT variable cost keywords.
C                Array bounds (MAX_KEYWORDS)
C
C pctDbhIndex  - Array of values in descending DBH order specifying the indexing of PCTVRCST keywords by their DBH-class specifier.
C                Array bounds (MAX_KEYWORDS)
C
C pctMinDbh    - Variable containing the threshold quadratic mean diameter (DBH) that differentiates a pre-commercial thinning from
C                a commercial harvest. Harvests of trees >= DBH value are commercial and are valued via the HRVRVN keyword.
C
C pctMinUnits  - Variable containing the units of volume used to specify minimum pct volume.
C
C pctMinVolume - Variable containing the threshold bfv/cfv value that differentiates a pre-commercial thinning treatment
C                from a commercial harvest. Harvests that are >= to the specified value are valued via the HRVRVN keyword.
C
C pctTpa - Array containing number of harvested trees per acre by PCT variable cost DBH-classes, array order same as PCT variable cost keywords.
C          Array bounds (MAX_KEYWORDS)
C
C revVolume - Array containing number and net volume of harvested trees by speices, revenue units-of-measure and revenue diameter-class.
C             Array bounds (MAXSP, MAX_REV_UNITS, MAX_KEYWORDS)
C
C sevInput - Scaler specifying dollar value of a user-input soil expectation value from SEVKNOWN keyword.
C
C varHrvCnt   - Number of variable harvest cost (HRVVRCST) keywords submitted.
C
C varHrvAmt   - Array containing the variable harvest cost amount by specified volume units units for commercial harvest of trees >= a specified DBH.
C               Array bounds (MAX_KEYWORDS)
C
C hrvCostDbhIndex - Array of values in descending DBH order specifying the indexing of HRVVRCST keywords by their DBH-class specifier.
C                   Array bounds (MAX_KEYWORDS)
C
C varHrvDbh   - Array containing the minimum tree DBH ssociated with a commercial harvest variable cost.
C               Array bounds (MAX_KEYWORDS)
C
C varHrvUnits - Array containing the volume units to which the varHrvAmt applies.
C               Array bounds (MAX_KEYWORDS)
C
C varHrvRate  - Array containing the rate in percent by which the varHrvAmt appreciates over time for an associated tree DBH-size set.
C               Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C varHrvDur   - Array containing the duration in years that an associated varHrvRate is to be
C               applied to a varHrvAmt for a specified tree DBH-size set.
C               Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C varPctCnt   -  Number of variable PCT cost (PCTVRCST) keywords submitted.
C
C varPctAmt   - Array containing the cost amount by specified volume units for PCT of trees >= a specified DBH.
C               Array bounds (MAX_KEYWORDS)
C
C varPctDbh   - Array containing the minimum tree DBH ssociated with a PCT variable cost.
C               Array bounds (MAX_KEYWORDS)
C
C varPctUnits - Array containing the units of volume to which the PCTVRCST keyword cost applies.
C               Array bounds (MAX_KEYWORDS)
C
C varPctRate  - Array containing the rate in percent by which the varPctAmt appreciate over time for an associated tree DBH-size set.
C               Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C varPctDur   - Array containing the duration in years that an associated varPctRate is to be applied to a varPctAmt for a specified tree DBH-size set.
C               Array bounds (MAX_KEYWORDS, MAX_RATES)
C
C-----END SEGMENT