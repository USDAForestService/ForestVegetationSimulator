C.... Code segment FMFCOM.
C----------
C  $Id$
C----------
C  **FMFCOM  FIRE
C----------
*-----------------------------------------------------------------------
*  PURPOSE:
*     COMMON BLOCK VARIABLES RELATED TO THE FIRE BURN MODEL ROUTINES.
*-----------------------------------------------------------------------
*
*COMMON BLOCK FMFCOM: (SPECIFIC BURN MODEL PARAMETERS)
*     ACTCBH: HEIGHT OF THE BOTTOM OF THE LIVE CROWN (FT)
*     ATEMP:  Air temperature
*     BURNED: Array holding the amounts of fuels consumed.
*             (1,)unpiled;(2,)piled;(3,)total
*             (,1-9) nine fuel size classes;(,10)litter;(,11)duff
*     BURNCR: Burned amounts from the crown - only used for smoke production.
*     BURNLV: Array holding the comsumed amounts of live fuels
*              1=herbs, 2=shrubs
*     BURNSEAS:Season assumed within the fuel model logic (used in LS-FFE only)
*             1 = early spring (compact leaves), 2 = before greenup,
*             3 = after greenup, 4 = fall
*     CANCLS: CANOPY CLOSURE CLASS USED IN DETERMINING WIND SPEED CORRECTION
*     CBD:    CANOPY BULK DENSITY (KG/M**3)
*     CCCHNG: Percent change in %CC from cycle t to t+1
*     CCCRIT: Critical value of CCCRIT required to trigger shrub model
*             delay (WS,NC,CA FFE-variants only)
*     CORFAC: WIND SPEED CORRECTION FACTOR BASED ON CANOPY CLOSURE
*     DEPTH:  Depth of the fuelbed, according to the current fuel model
*     DPMOD:  Depth modifier calculated based on harvest and fuel
*             treatment types
*     EXPOSR: CALCULATED MINERAL SOIL EXPOSURE FROM FIRE EFFECTS.
*     FIRACR: Area in a fire (1=stand area only, 2=area with crowning)
*     FIRTYPE: Type of fire. (1=ACTIVE, 2=PASSIVE, 3=SURFACE)
*     FLAG:   Error flag: 1=dead fuel too moist, 2=wind speed exceeds reliable
*             extrapolation, 3=gross surf/vol too small.
*     FM89YR: Year of switching from FM8/9 to FM5/26 (CA shrub delay model)
*     FMACRE: Number of acres in each fuel model each year.
*             (for landscape-level printing)
*     FMDEP:  Fuel model depth, for all fuel models (DEPTH is current fuel model)
*     FMLOAD: Fuel model surface loading for all fuel models.
*             (1,1) = dead 1 hr, (1,2) = dead 10 hr, (1,3) = dead 100 hr,
*             (1,4) = dead herb, (2,1) = live woody, (2,2) = live herb
*     FMOD:   Up to MXFMOD fuel models that are close to the current large
*             and small fuels
*     FMDUSR: Up to 4 fuel models that are set by the user to be used in
*             preference to those calculated by the model.
*     FWG:    Surface loading, lb.sqft fuel type for current fuel model
*     FWT:    Fuel model Weightings (up to MXFMOD) for use by the dynamic
*             fuel models
*     FWTUSR: Fuel model weightings of the fuel models in FMDUSR,
*             as defined by the user.
*     FWIND:  Dominant wind speed
*     IFLOGIC:fire behavior calculation method
*              (0 = old fm logic, 1 = new fm logic, 2 = use modelled loads)
*     IFMSET: when using the new fm logic, the fuel model set (0=13, 1=40, 2=53)
*     IFUELMON: Array that says whether fuel models are turned on or off for the new
*               fuel model logic (see FireCalc and FModList keywords).
*               -1 = not set, 0 = on, 1 = off
*     LARGE:  Large material (ton/acre >3") for determining fire model
*     OLARGE: Previous year LARGE
*     LATFUEL:.TRUE. if Activity Fuels present over  5 yrs; else .FALSE.
*     LATSHRB:.TRUE. if Activity has triggered FM shrub dynamics
*     LPRV89: .TRUE. if FM8/9 and NOT 5/26 in prev yr.
*     LHEAD:  FLAG FOR WHETHER TO PRINT THE HEADER FOR THE FIRE OUTPUT FILES
*     LUSRFM: Flag for whether to use the user-defined fuel models and weightings
*     LOWDBH: The lower bound of each of the 7 size clases used in
*             reporting fire-based mortality
*     LSW:    Logical SoftWood. TRUE if softwood/conifer; else FALSE
*     MEXT:   Moisture of extinction of dead fuel for the current fuel model
*     MINSOL: Area with mineral soil exposure in a single year
*     MOIS:   Moisture content of fuel types
*             Dimension (2,5), First subscript 1=dead, 2=live
*             for dead, (1,1)=1hr, 0-.25"; (1,2)=10hr .25-1"; (1,3)=100hr 1-3";
*                     (1,4)=3+; (1,5)=Duff...
*             for live, (2,1)=live woody, (2,2)=live herb
*     MOISEX: Moisture of extinction for the original fuel models
*     MPS:    Mean surf/vol per ft of fuel type for the current fuel model
*                MPS(1,1)=1hr, MPS(1,2)=10hr, MPS(1,3)=100hr DEAD!
*                MPS(1,4)=dead herb, MPS(2,1)=Live woody, MPS(2,2)=live herb
*     ND:     Number of dead fuel size classes to be condsidered for current
*                fuel model
*     NL:     Same as ND but for live fuel
*     PBRNCR: The potential amount of crown material burned
*             (in a potential fire)
*     PFLACR: The area in the landscape in a given flame
*             length-moisture category.
*     PFLAM:  Potential flame length: 1= Wild; 3= prescribed
*     PLSIZ:  The lower and upper size used for grouping areas in
*             potential flame lengths.
*     POTPAB: The new percent area burned to use when calculating
*             potential fire effects.  For 1 = severe, 2 = moderate.
*     POTSEAS:The new season to use when calculating potential fire
*             behavior.  For 1 = severe, 2 = moderate. (affects LS-FFE only)
*             1 = early spring (compact leaves), 2 = before greenup,
*             3 = after greenup, 4 = fall
*     POTFSR: potential fire spread rate, in feet/min
*             1 returns severe surface fire spread rate
*             2 returns moderate surface fire spread rate
*             3 returns severe final fire spread rate
*             4 returns moderate final fire spread rate
*     POTRINT: potential fire reaction intensity, in btu/ft2/min
*             1 = severe fire, 2 = moderate fire
*     POTEMP: The new fire temperatures to use when calculating the
*             potential flame lengths. For 1=Wildfire, 2=Prescribed.
*     POTKIL: potential fire mortality (proportion in terms of BA)
*             For 1 = severe fire, 3 = moderate fire
*     POTTYP: potential fire type 
*             pottyp(1) returns value for severe fire, 
*             pottyp(2) returns value for moderate fire
*             values are 1 = surface, 2 = passive
*                        3 = active,  4 = conditional crown 
*     POTVOL: potential fire mortality (total cuft volume)
*             For 1 = severe fire, 2 = moderate fire
*     PREWND: The new fire wind conditions to use when calculating the
*             potential flame lengths. For 1=Wildfire, 2=Prescribed.
*     PRESVL: The new fire moisture conditions to use when calculating
*             the potential flame lengths. Dimension (2,7)
*             First subscript: 1=Wildfire, 2=Prescribed.
*             Second subscript: 1 if rest of vector is defined and 0
*             if not; 2=1hr moist; 3=10hr; 4=100hr; 5=3+; 6=Duff;
*             7=Live moisture. Set by keyword.
*     PRV8/9: wt of FM8 FM9 at start of shrub model period
*     RFINAL: Final rate of fire spread whether surface or crown fire,
*             calulated in FMCFIR and used in FMPOFL and FMBURN
*     SCH:    Scorch height (ft)
*     SMALL:  Small material (ton/acre <3") for determining fire model
*     OSMALL: Previous year small
*     SLCHNG: Percent change in total fuels from cycle t to t+1
*     SLCRIT: Critical value of SLCHNG required to trigger Activity Fuels
*             state
*     SMOKE:  The amount of smoke produced in 2 size classes.
*     SCBE:   Holding variable for the variable C1 from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SFRATE: Holding variable for the fire rate info from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SIRXI:  Holding variable for XIO (Ir*XI) from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SPHIS:  Holding variable for the variable PHIS from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SRHOBQ: Holding variable for the variable RHOBQIG from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SSIGMA: Holding variable for the variable SIGMA from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SXIR:   Holding variable for the variable XIR (Ir) from FMFINT
*             (1=wild/oth, 2=fm10, 3=pres/oth)
*     SOILTP: Soil Type - used in the soil heating calculations
*             (1-Loamy Skeletal,2-Fine Silt,3-Fine,4-Coarse Silt,
*              5-Coarse Loam)
*     SURFVL: Surface area to vol ratio for all fuel models (MPS holds
*             current only)
*     TCLOAD: TOTAL CANOPY LOAD (LB/FT**2)
*     USAV:   user-entered sav (1/ft) used in new fm logic and when using
*             modelled loads to predict fire behavior
*             (array elements: 1 = 1-hr, 2 = herb, 3 = live woody)
*     UBD:    user-entered bulk density (lbs/ft3) used in new fm logic and
*             when using modelled loads to predict fire behavior
*             (array elements: 1 = live, 2 = dead)
*     ULHV:   user-entered dead and live heat content (btu/lb) used when
*             using modelled loads to predict fire behavior
C     IFMBRB: BEGINNING YEAR TO PRODUCE BURN CONDITIONS REPORT.
C     IFMBRE: ENDING YEAR TO PRODUCE BURN CONDITIONS REPORT.
C     ISHEATB:BEGINNING YEAR TO PRODUCE SOIL HEAT REPORT.
C     ISHEATE:ENDING YEAR TO PRODUCE SOIL HEAT REPORT.
C     IFMFLB: BEGINNING YEAR TO PRODUCE FUEL CONSUMPTION REPORT.
C     IFMFLE: ENDING YEAR TO PRODUCE FUEL CONSUMPTION REPORT.
C     IFMMRB: BEGINNING YEAR TO PRODUCE MORTALITY REPORT.
C     IFMMRE: ENDING YEAR TO PRODUCE MORTALITY REPORT.
C     IFLALB: BEGINNING YEAR TO PRODUCE ALL FUELS REPORT.
C     IFLALE: ENDING YEAR TO PRODUCE ALL FUELS REPORT.
C     IPFLMB: BEGINNING YEAR TO PRODUCE POTENTIAL FLAME REPORT.
C     IPFLME: ENDING YEAR TO PRODUCE POTENTIAL FLAME REPORT.
C     ISNAGB: BEGINNING YEAR TO PRODUCE SNAG REPORT.
C     ISNAGE: ENDING YEAR TO PRODUCE SNAG REPORT.
C     IFSTEP: INTERVAL TO PRODUCE THE ALL FUELS REPORT.
C     IPFSTP: INTERVAL TO PRODUCE THE POTENTIAL FLAME REPORT.
C     ISNSTP: INTERVAL TO PRODUCE THE SNAG LIST.
C     ICFPB:  BEGINNING YEAR TO PRODUCE THE CANOPY FUELS PROFILE TABLE.
C     ICFPE:  ENDING YEAR TO PRODUCE THE CANOPY FUELS PROFILE TABLE.
C     ICFPST: INTERVAL TO PRODUCE THE CANOPY FUELS PROFILE TABLE.
C     IDWRPB: BEGINNING YEAR TO PRODUCE DOWN WOOD VOLUME REPORT.
C     IDWRPE: ENDING YEAR TO PRODUCE DOWN WOOD VOLUME REPORT.
C     IDWRPI: INTERVAL TO PRODUCE THE DOWN WOOD VOLUME REPORT.
C     IDWCVB: BEGINNING YEAR TO PRODUCE DOWN WOOD COVER REPORT.
C     IDWCVE: ENDING YEAR TO PRODUCE DOWN WOOD COVER REPORT.
C     IDWCVI: INTERVAL TO PRODUCE THE DOWN WOOD COVER REPORT.
C     IDBRN : ID NUMBER FOR THE BURN CONDITIONS REPORT.
C     IDSHEAT ID NUMBER FOR THE SOIL HEAT REPORT.
C     IDFUL : ID NUMBER FOR THE FUEL CONSUMPTION REPORT.
C     IDMRT : ID NUMBER FOR THE MORTALITY REPORT.
C     IDFLAL: ID NUMBER FOR THE ALL FUELS REPORT.
C     IDPFLM: ID NUMBER FOR THE POTENTIAL FLAME LENGTH REPORT.
C     IDDWRP: ID NUMBER FOR THE DOWN WOOD VOLUME REPORT.
C     IDDWCV: ID NUMBER FOR THE DOWN WOOD COVER REPORT.
C     IBRPAS: FLAG FOR BURN CONDITIONS REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     IFLPAS: FLAG FOR FUEL CONSUMPTION REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     IMRPAS: FLAG FOR MORTALITY REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     IFAPAS: FLAG FOR ALL FUELS REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     IPFPAS: FLAG FOR POTENTIAL FLAME REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     IDWPAS: FLAG FOR DOWN WOOD VOLUME REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     IDCPAS: FLAG FOR DOWN WOOD COVER REPORT TO INSURE THAT HEADER GETS
C             WRITTEN ONLY ONCE.
C     JSNOUT: UNIT NUMBER FOR SNAG OUTPUT TABLE
C     LFLBRN: FLAG WHICH INDICATES WHETHER FUEL BURNING HAS BEEN
C             REQUESTED IN A GIVEN YEAR.
C      IDRYB: BEGINNING YEAR OF SCHEDULED DROUGHT
C      IDRYE: ENDING YEAR OF SCHEDULED DROUGHT
C
***********************************************************************
C
C     VARIABLE DECLARATIONS: COMMON BLOCK FMFCOM
C
      INTEGER  ACTCBH
      INTEGER  ATEMP, BURNSEAS
      REAL     BURNED(3,MXFLCL), BURNLV(2), BURNCR
      REAL     CANCLS(4), CORFAC(4)
      REAL     CCCHNG,CCCRIT
      REAL     CBD
      REAL     DEPTH, DPMOD
      REAL     EXPOSR
      REAL     FIRACR(2)
      INTEGER  FIRTYPE
      INTEGER  FLAG(3)
      INTEGER  FM89YR
      REAL     FMACRE(14)
      REAL     FMDEP(MXDFMD)
      INTEGER  FMDUSR(4)
      REAL     FMLOAD(MXDFMD,2,7)
      INTEGER  FMOD(MXFMOD)
      REAL     FWG(2,7)
      REAL     FWIND
      REAL     FWT(MXFMOD)
      REAL     FWTUSR(4)
      INTEGER  IFLOGIC, IFMSET, IFUELMON(MXDFMD)
      INTEGER  IFMFLE, IFMMRB, IFMMRE, IFLALB, IFLALE
      INTEGER  IDBRN, IDFUL, IDMRT, IDFLAL, IDPFLM
      INTEGER  IDSHEAT, ISHEATB, ISHEATE
      INTEGER  IBRPAS, IFLPAS, IMRPAS, IFAPAS, IPFPAS
      INTEGER  IFSTEP, IFMBRB, IFMBRE, IFMFLB
      INTEGER  IPFSTP, IPFLMB, IPFLME
      INTEGER  ISNAGB, ISNAGE, ISNSTP
      INTEGER  ICFPB, ICFPE, ICFPST
      INTEGER  IDWPAS, IDCPAS, IDDWRP, IDDWCV, IDWRPB, IDWRPE, IDWRPI 
      INTEGER  IDWCVB, IDWCVE, IDWCVI
      INTEGER  IDRYB, IDRYE
      INTEGER  JSNOUT
      REAL     LARGE, SMALL
      REAL     OLARGE, OSMALL
      LOGICAL  LHEAD, LFLBRN
      LOGICAL  LUSRFM, LATFUEL
      LOGICAL  LATSHRB, LPRV89
      LOGICAL  LSW(MAXSP)
      REAL     LOWDBH(7)
      REAL     MEXT(3)
      REAL     MINSOL
      REAL     MOIS(2,5)
      REAL     MOISEX(MXDFMD)
      INTEGER  MPS(2,4)
      INTEGER  ND, NL
      INTEGER  NFMODS
      REAL     PBRNCR
      REAL     PFLACR(4,3)
      REAL     PFLAM(4)
      INTEGER  PLSIZ(2),POTSEAS(2), POTTYP(2)
      REAL     POTPAB(2),POTEMP(2), POTKIL(4), POTVOL(2)
      REAL     POTFSR(4), POTRINT(2), PRESVL(2,8)
      REAL     PRV8, PRV9
      REAL     PREWND(2), RFINAL
      REAL     SCH, SLCHNG, SLCRIT
      INTEGER  SOILTP, SURFVL(MXDFMD,2,4)
      REAL     SMOKE(2), SCBE(3)
      REAL     SFRATE(3),SIRXI(3),SPHIS(3),SRHOBQ(3),SSIGMA(3),SXIR(3)
      REAL     TCLOAD
      REAL     USAV(3), UBD(2), ULHV
C
      COMMON/FMFCOM/ ACTCBH, ATEMP, BURNED, BURNCR, BURNLV, BURNSEAS,
     &               CANCLS, CBD, CCCHNG, CCCRIT, CORFAC,
     &               DEPTH, DPMOD, EXPOSR, FIRACR, FLAG, FM89YR,
     &               FMACRE, FMDEP, FMLOAD, FMOD, FWG, FWIND, FWT, 
     &               IFLOGIC, IFMSET, IFUELMON, LARGE, LHEAD, 
     &               LSW, MEXT, MINSOL, MOIS, MOISEX, MPS, ND, NL,
     &               OLARGE, OSMALL, LFLBRN, POTTYP, POTFSR, POTRINT,
     &               PBRNCR, PFLACR, PLSIZ, POTKIL, POTVOL, PRESVL,
     &               PRV8, PRV9, RFINAL, SCH, SMALL,
     &               SLCHNG, SLCRIT, SMOKE, SCBE, SOILTP, SURFVL,
     &               SFRATE, SIRXI, SPHIS, SRHOBQ, SSIGMA, SXIR,
     &               IFMBRB, IFMBRE, IFMFLB, LOWDBH, IDBRN, IDFUL,
     &               IFMFLE, IFMMRB, IFMMRE, IFLALB, IFLALE,
     &               IDMRT,  IDFLAL, IBRPAS, IFLPAS, IMRPAS, IFAPAS,
     &               IFSTEP, IPFSTP, IPFLMB, IPFLME, IDPFLM,
     &               IPFPAS, IDWPAS, IDCPAS, IDDWRP, IDDWCV, IDWRPB, 
     &               IDWRPE, IDWRPI, IDWCVB, IDWCVE, IDWCVI,
     &               ISNAGB, ISNAGE, ISNSTP, FIRTYPE,
     &               ICFPB, ICFPE, ICFPST,
     &               IDRYB, IDRYE, JSNOUT, PREWND, POTSEAS, POTPAB,
     &               POTEMP, FMDUSR, IDSHEAT, ISHEATB, ISHEATE,
     &               FWTUSR, LUSRFM, LATFUEL, LATSHRB, LPRV89, NFMODS, 
     &               PFLAM, TCLOAD, USAV, UBD, ULHV

C
C====================== CARBON REPORTING VARIABLES =========================
C
C     IDCRPT: ID NUMBER FOR MAIN CARBON REPORT
C     ICRPTB: BEGINNING YEAR TO PRODUCE MAIN CARBON REPORT
C     ICRPTE: ENDING YEAR TO PRODUCE MAIN CARBON REPORT
C     ICRPTI: INTERVAL TO PRODUCE THE MAIN CARBON REPORT
C     ICRPAS: FLAG FOR REPORT HEADER; TRIP AFTER FIRST CYCLE

C     IDCHRV: ID NUMBER FOR HARVESTED PRODUCTS REPORT
C     ICHRVB: BEGINNING YEAR TO PRODUCE HARVESTED PRODUCTS REPORT
C     ICHRVE: ENDING YEAR TO PRODUCE HARVESTED PRODUCTS REPORT
C     ICHRVI: INTERVAL TO PRODUCE THE HARVESTED PRODUCTS REPORT
C     ICHPAS: FLAG FOR REPORT HEADER; TRIP AFTER FIRST CYCLE
C
C     ICMETH:   CARBON COMPUTING METHOD:    0 = FFE,      1 = JENKINS
C     ICMETRC:  CARBON OUTPUT REPORT UNITS: 0 = IMPERIAL, 1 = METRIC
C     ICHABT:   1,2 CODE FOR FOREST REGION; USED TO MAP DIFFERENT
C               CARBON-FATE CURVES BASED ON GEOGRAPHIC LOCATION
C     CRDCAY:   STUMP ROOT DECAY RATE/YR; WHEN <0 (-1.0=DEFAULT) STUMP
C               ROOTS ARE NOT REPORTED
C     CDBRK(1): HARDWOOD PULP/SAWTIMBER DIAMETER BREAKPOINT
C     CDBRK(2): SOFTWOOD PULP/SAWTIMBER DIAMETER BREAKPOINT
C
C     BIOLIVE:  FFE CALCULATED LIVE BIOMASS
C     BIOREM(2): 1=JENKINS 2=FFE REMOVED BIOMASS
C     BIOSNAG:  FFE CALCULATED SNAG BIOMASS
C     BIODDW:   FFE CALCULATED DOWN WOODY DEBRIS BIOMASS
C     BIOFLR:   FFE CALCULATED LITTER AND DUFF BIOMASS
C     BIOSHRB:  FFE CALCULATED HERB AND SHRUB BIOMASS
C     BIOROOT:  JENKINS CALCULATED ROOT BIOMASS
C     BIOCON:   BIOMASS CONSUMED (1 = LITTER/DUFF, 2 = ALL ELSE)
C
C     FATE(M,N,P)      M,N AS ABOVE, P=1-40: CYCLE OF THE CUT
C
C     CARBVAL(17): ARRAY USED TO PASS INFO FROM CARBON REPORTS TO 
C                  EVENT MONITOR FUNCTION CARBSTAT; 1 - 17 CORRESPOND
C                  TO THE COLUMNS IN THE STAND CARBON REPORT AND THE 
C                  HARVESTED CARBON REPORT.
C
      INTEGER  IDCRPT, ICRPTB, ICRPTE, ICRPTI, ICRPAS
      INTEGER  IDCHRV, ICHRVB, ICHRVE, ICHRVI, ICHPAS
      INTEGER  ICMETH, ICMETRC,ICHABT
      REAL     CRDCAY, CDBRK(2), BIOCON(2)
      REAL     BIOLIVE, BIOREM(2), BIOSNAG, BIODDW, BIOFLR
      REAL     BIOSHRB, BIOROOT, FATE(2,2,MAXCYC), CARBVAL(17)

      COMMON /CRBCOM/
     &  IDCRPT, ICRPTB, ICRPTE, ICRPTI, ICRPAS,
     &  IDCHRV, ICHRVB, ICHRVE, ICHRVI, ICHPAS,
     &  ICMETH, ICMETRC, ICHABT,
     &  CRDCAY, CDBRK, BIOCON,
     &  BIOLIVE, BIOREM, BIOSNAG, BIODDW, BIOFLR,
     &  BIOSHRB, BIOROOT, FATE, CARBVAL

C====================== CFIM MODEL VARIABLES =========================

C     CFIM_ON:  Logical: TRUE = YES, DEFAULT = FALSE (CFIM MODEL)
C     CFIM_BD: Bulk Density (CFIM MODEL)
C     CFIM_DC: Drought Condidtion Code (CFIM MODEL)
C     CFIM_INPUT: INPUT INFORMATION FOR THE CFIM MODEL CALCULATIONS
C	  POTCONS:	POTENTIAL FUEL CONSUMPTION

      LOGICAL CFIM_ON
      REAL    CFIM_BD, CFIM_DC, CFIM_INPUT(26)
	  REAL	  POTCONS(3,3)
	 
      COMMON /CFIMCOM/
     &  CFIM_ON, CFIM_BD, CFIM_DC, CFIM_INPUT, POTCONS

C.... End segment.
