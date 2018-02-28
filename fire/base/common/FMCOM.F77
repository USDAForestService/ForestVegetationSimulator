Code segment FMCOM.
C----------
C  $Id$
C----------
*  PURPOSE:
*     COMMON BLOCK VARIABLES RELATED TO THE FIRE MODEL ROUTINES.
*-----------------------------------------------------------------------
*
*COMMON BLOCK FMCOM:
*     ALLDWN:    time by which the last 5% of lg. snags in each group
*                have all fallen
*     BURNYR:    YeaR of the most recent BURN in the stand (simul. fire)
*     CANMHT:    Minimum Ht of trees used in calculation of canopy base
*                 height and canopy bulk density.
*     CBHCUT:    Cutoff value used in determining canopy base height.
*     COVTYP:    Species number for the predominant cover type in the stand
*                (measured as the dominant basal area in the stand)
*     CRBURN:    Proportion of the stand that has a crown fire.
*     CROWNW:    CROWN Weight in pounds of each *live* tree in the record,
*                Dimensioned by size of material using 0:5 breakpoints
*                defined in FMPARM.F77
*     CURKIL:    FIRE-KILLED TREES THIS YEAR, BY STAND AND RECORD.
*     CWD:       Array for Coarse Woody Debris.
*                Kept as TONS per acre.  Indexed 4 ways:
*                (1:3,  -,  -,  -) 1 = Unpiled, 2 = Piled, 3 = summation
*                (  -,1:11,  -,  -) MXFLCL categories defined in FMPARM.F77
*                (  -,  -,1:2,  -) 1 = SOFT, 2 = HARD
*                (  -,  -,  -,1:4) 1 = V.Slow,..., 4 = Fast decay rate
*     CWD2B:     debris-in-waiting: dead crown material which is scheduled to
*                become down debris at appropriate points in the future.
*                Dimensioned by debris-decay-rate class, debris-size class,
*                and time into the future (category 1 is the material that
*                will come down the next time FMCADD is called).  Kept as POUNDS
*                per acre.  Updated at the beginning of each FVS cycle.
*                Includes all crown material on snags and the fire-killed but
*                not-consumed crown material on live trees.
*                (1:4, -,     -) 1 = V.Slow,..., 4 = Fast decay rate
*                (  -,0:5,    -) crown weight categoriess as defined in FMPARM.F77
*                (  -,  -,TFMAX) TFMAX as defined in FMPARM.F77
*                If the TFMAX dimension is increased, then loop-limits that
*                involve that variable should be re-examined (see FMSCRO)
*     CWD2B2:    dimensioned like CWD2B, this is crown material from trees
*                killed during the current FVS cycle.  It won't start falling
*                until after it is added to CWD2B (at the end of the year).
*                **Indexed the same way as CWD2B**
*     CWDVOL:    array for down woody debris, it holds down wood volume 
*                in cuft/acre. Dimensioned like CWD, but without the litter and
*                duff categories.  The tenth position is the total volume of the woody
*                classes instead.
*     CWDCOV:    array for down woody debris, it holds down wood cover 
*                in percent. Dimensioned like CWD, but without the litter and
*                duff categories.  The tenth position is the total cover of the woody
*                classes instead.
*     CWDCUT:    the proportion of CWD2B that has been 'CUT' by salvage
*                operations in the current year (based on the proportion
*                of total snag volume removed in the salvage operation).
*     CWDNEW:    The 'new' debris pool inputs, by source (crown/boles) and
*                size class.
*     DBHS:      DBH of Snags in each record
*     DBHSSALV:  DBH of Snags in each record passed from FMSALV to FMEVMON for
*                post-cut EM calculations
*     DECAYX:    rate-of-DECAY correction factors for each snag species class
*     DENIH:     DENsity (stems per acre) of Initially-Hard snags in each record
*     DENIS:     DENsity (stems per acre) of Initially-Soft snags in each record
*     DEND:      DENsity (stems per acre) of snags in each record at
*                time of Death
*     DKR:       Mineralization rates for logs of each species
*     DKRCLS:    1-4 Decay rate class (MAXSP): 1=v.slow;2=slow;3=med;4=fast
*     DKRDEF:    Default mineralization rates (4)
*     DSPDBH:    Density of new snags in each SPecies+DBH class
*     FALLX:     rate-of-FALL correction factors for each species
*     FIRKIL:    FIRE-KILLED TREES THIS CYCLE, BY TREE RECORD.
*     FLIVE:     Array of live fuels: 1=herbs, 2=shrubs
*     FolMC:     Foliar moisture content (%).  Currently an unreleased feature
*                on the cancalc keyword.
*     FMICR:     CROWN PROPORTION (ICR) VARIABLE IN THE FIRE MODEL (MAXTRE)
*     FMPROB:    PROB VARIABLE IN THE FIRE MODEL, BY TREE RECORD
*     FMTBA:     Fire Model Total Basal Area, by species.
*     NFMSVPX:   The number of pictures to render for a fire using the
*                SVS keyword.
*     FMORTMLT:  Tree mortality multiplier, set in FMMAIN for a given cycle
*     FTREAT:    Fuel treatment type: 0=none,1=lopping (inc.flailing),
*                2=trampling (inc chopping, crushing, chipping)
*     FUAREA:    Number of acres in various loading categories: for
*                landscape-level output
*                (1:5,-) 1=0-3",2=>3",3=all,4=duff,5=snags
*                (-,1:4) #tons/acre cat.:0-10;10-20;20-30;30+
*     GROW:      indicates for each tree record whether the crowns should
*                GROW normally, or has been reduced for a fire.
*                GROW>=1 means do grow.
*     HARD:      indicates whether initially-hard snags in each record are
*                currently still HARD (HARD=.TRUE.)
*     HARDSALV:  indicates whether initially-hard snags in each record are
*                currently still HARD (HARD=.TRUE.)
*                passed from FMSALV to FMEVMON for post-cut EM calculations
*     HARDTYP:   Harvest type 1=ground-based (inc cat or line skidding);
*                2=high-lead (inc. skyline);3=precom./helicopter
*     HARVYR:    Year in which a harvest occurred. (Also, creates
*                activity fuels for 5 years)
*     HTDEAD:    HeighT at time of Death of snags in each record
*     HTDEADSALV:HeighT at time of Death of snags in each record
*                passed from FMSALV to FMEVMON for post-cut EM calculations
*     HTIH:      current HeighT of the Initially-Hard snags in each record
*     HTIHSALV:  current HeighT of the Initially-Hard snags in each record
*                passed from FMSALV to FMEVMON for post-cut EM calculations
*     HTIS:      current HeighT of the Initially-Soft snags in each record
*     HTIHSALV:  current HeighT of the Initially-Soft snags in each record
*                passed from FMSALV to FMEVMON for post-cut EM calculations
*     HTR1:      base rate-of-height-loss for first 50% of snag height
*     HTR2:      base rate-of-height-loss for second 50% of snag height
*     HTX:       rate-of-height-loss correction factors for each species
*                 ,1=first 50% of initially hard, ,2=second 50% of initially hard
*                 ,3=first 50% of initially soft, ,4=second 50% of initially soft
*     HTXSFT:    rate-of-height-loss correction factor for initially soft snags
*     ICANSP:    species used in calculated canopy base height and canopy
*                 bulk density.  0 = conifers only, 1 = all species
*     ICBHMT:    method of calculating canopy base height.
*                 0 = standard method, 1 = ???
*     IFMYR1:    FIRST YEAR OF THE CYCLE (USED FOR LOOPING ANNUALLY WITHIN
*                A CYCLE)
*     IFMYR2:    LAST YEAR IN THE CYCLE
*     IFTYR:     year corresponding to a FuelTret keyword (determines the years a
*                fuel depth multiplier is used).  = 0 during years when
*                no depth multiplier should be applied.
*     ISALVC     0 = cut this species in a salvage cut;
*                1 = leave this species in a salvage cut.
*     ISALVS     species being cut in a salvage operation.
*     ISNGSM:    -1 if snag summary is not requested, 0 if requested but
*                no report reference number is assigned, >0 is the
*                report reference number.
*     JCOUT:     OUTPUT FILE NUMBER FOR THE 'HIDDEN' OUTPUT FILE
*     JFROUT:    ARRAY CONTAINING THE OUTPUT UNIT NUMBERS FOR THE FIRE REPORT
*                FILES
*     JLOUT:     ARRAY COMTAINING THE OUTPUT UNIT NUMBERS FOR THE LANDSCAPE
*                OUTPUT FILES
*     LANHED:    TRUE IF THE HEADERS ARE STILL TO BE PRINTED FOR THE LANDSCAPE
*                 OUTPUT
*     LDHEAD:    TRUE IF THE HEADERS ARE STILL TO BE PRINTED FOR THE FUELS OUTPUT
*     LDYNFM:    TRUE IF THE DYNAMIC FUEL MODEL IS TO BE USED
*     LEAFLF:    LEAF LiFespan in years, for each species
*     LFMON:     TRUE IF THE FIRE MODEL EXTENTION IS RUNNING
*     LFMON2:    TRUE IF IN THE FIRST YEAR OF FIRE MODEL
*     LFROUT:    LOGICAL ARRAY INDICATING WHICH FIRE REPORT FILES WILL BE PRINTED
*     LIMBRK:    LIMb BReaKage rate, the fraction of all non-foliage crown
*                material that falls off live trees each year.
*     LREMT:     LOGICAL FLAG TO INDICATE THAT REMOVALS ARE BEING TRACKED
*                FOR EM FUNCTION TREEBIO IN CALL TO TREREM
*     LSHEAD:    TRUE IF THE HEADER OF THE SNAG OUTPUT FILE STILL NEEDS TO BE
*                PRINTED
*     LVWEST:    LOGICAL: .TRUE. if a western variant; otherwise .FALSE. *FMVINIT*
*     NSNAG:     number of the highest snag record that is in use (max=MXSNAG)
*     NSNAGSALV: number of the highest snag record that is in use (max=MXSNAG)
*                passed from FMSALV to FMEVMON for post-cut EM calculations
*     MAXHT:     Maximum height of new snags in each species- and dbh-class
*     MINHT:     minimum height of new snags in each species- and dbh-class
*     NYRS:      number of years in a cycle. This is the same as IFINT in PLOT.F77
*     NZERO:     Number of snags per stand that will be considered equal to 0.
*     OLDCRL:    CRown Length of each tree record at end of the previous cycle
*     OLDCOVTYP  :  OLD COVer TYPe - COVTYP in the previous cycle
*     OLDCRW:    CRown Weights for each tree record.
*                Variable is used temporarily to carry crown weights from end
*                of one FVS cycle to the beginning of the next.  *During* a
*                cycle, variable holds the weight of each size of old material
*                that is to fall in each year of the cycle.
*                Dimensioned like CROWNW.
*     OLDHT:     HeighT of each tree record at end of the previous FVS cycle
*     OLDICT:    Cover Type (meta-species groups) in previous cycle
*     OLDICT2:   2nd Cover Type (meta-species groups) in previous cycle
*     OLDICTWT:  Wt of 2nd Cover Type (meta-species groups) in previous cycle
*     PBFRIH:    Post-Burn Fall Rate (proportion per year) of the Initially-Hard
*                snags in each record
*     PBFRIS:    Post-Burn Fall Rate (proportion per year) of the Initially-Soft
*                snags in each record
*     PBSCOR:    threshold SCORch height for a fire to activate Post-Burn rules
*     PBSIZE:    the dbh between small and large snags for Post-Burn purposes
*     PBSMAL:    proportion of SMALl snags to fall during Post-Burn period
*     PBSOFT:    proportion of SOFT snags to fall during Post-Burn period
*     PBTIME:    Post-Burn TIME period (years) over which a specified portion
*                of snags are to fall
*     PBURNYR:   YeaR of the most recent Pile BURN in the stand (pile or
*                jackpot burn with PileBurn keyword)
*     PERCOV:    The percent cover of the stand (not ccf).
*     PRDUFF:    proportion of the total fuel decomp. that goes to Duff (not air)
*     PRPILE:    proportion of fuel piled up
*     PRSNAG:    THE PROPORTION OF THE CUT THAT BECOMES SNAGS
*     PSOFT:     proportion of all snags that are soft at time of death (by species)
*     SALVSPA:   salvaged snags per acre for each snag record
*                SALVSPA(I,1) = salvaged initially hard snags per acre in
*                snag record i
*                SALVSPA(I,2) = salvaged initially soft snags per acre in
*                snag record i
*     SCCF:      STAND-LEVEL CROWN COMPETITION FACTOR (PERCENTAGE)
*     SETDECAY:  array (1:mxflcl, 1:4 for the decay rate class) that shows whether the 
*                user had reset the surface fuel decay rate with the FuelDcay keyword.
*                 -1 means the decay rate has not been set, a value greater than 0 is 
*                the actual rate set for that class.
*     SNGNEW:    New snags to add to list
*     SNPRCL:    THE UPPER DBH FOR A GROUP OF SNAGS (FOR PRINTING ONLY)
*                SNag PRint CLass
*     SPS:       SPecies of Snags in each record
*     SPSSALV:   SPecies of Snags in each record passed from FMSALV to FMEVMON for
*                post-cut EM calculations
*     TCWD:      An compressed copy of the CWD data.
*                Used to display SVS pictures with varying fuel loads
*                during a fire.
*     TCWD2:     Another compressed copy of the CWD data.
*     TFALL:     Time until crown components on dead trees will have all FALLen,
*                dimensioned by tree species and crown material size class
*                Dimensioned like CROWNW.
*     TODUFF:    Rate of conversion of CWD to duff.
*     TONRMC:    ToNs Removed (total volume) through CWD transfers
*     TONRMS:    ToNs Removed (total volume) through Salvage
*     TONRMH:    TONs ReMoved (total volume) through Harvest
*     TOTACR:    Total acrage in the landscape.
*     V2T:       Volume (cuft) to (2) Tons conversion factor.
*     YRDEAD:    YeaR of Death of the snags in each record.
*     PREMST:    Live trees remomved set in FMTREM, used in FMEVTBM
*     PREMCR:    Live crown removed set in FMTREM, used in FMEVTBM
*     ISPCC:     Species removed set in FMTREM, used in FMEVTBM
*     DBHC:      Tree DBH set in FMTREM, used in FMEVTBM
*     HTC:       Tree height set in FMTREM, used in FMEVTBM
*     CROWNWC:   Crown weight set in FMTREM, used in FMEVTBM
*     ICYCRM:    Thinning cycle for TREEBIO function
*     ITRNL:     Lenght of tree list at time of call to FMTREM from CUTS.
*                Estab trees may enter simulation between the call
*                to FMTREM and the call to FMEVTBM.
*,FMORTMLT(MAXTRE)
C     VARIABLE DECLARATIONS: COMMON BLOCK FMCOM

      REAL     ALLDWN(MAXSP)
      INTEGER  BURNYR
      REAL     CANMHT, CBHCUT
      INTEGER  COVTYP
      REAL     CRBURN
      REAL     CROWNW
      DIMENSION CROWNW(MAXTRE,0:5)
      REAL     CURKIL(MAXTRE)
      REAL     CWD(3,MXFLCL,2,5)
      REAL     CWD2B
      DIMENSION CWD2B(4,0:5,TFMAX)
      REAL     CWD2B2
      DIMENSION CWD2B2(4,0:5,TFMAX)
      REAL     CWDVOL(3,10,2,5), CWDCOV(3,10,2,5)   
      REAL     CWDCUT, CWDNEW(2,MXFLCL)
      REAL     DBHS(MXSNAG)
      REAL     DECAYX(MAXSP)
      REAL     DENIH(MXSNAG), DENIS(MXSNAG)
      REAL     DEND(MXSNAG)
      REAL     DKR(MXFLCL,4)
      INTEGER  DKRCLS(MAXSP)
      REAL     DKRDEF(4)
      REAL     DSPDBH(MAXSP,19)
      REAL     FALLX(MAXSP)
      REAL     FIRKIL(MAXTRE), FOLMC
      INTEGER  FMICR(MAXTRE)
      REAL     FMORTMLT(MAXTRE)
      INTEGER  FMKOD
      REAL     FLIVE(2)
      REAL     FMPROB(MAXTRE)
      REAL     FMTBA(MAXSP)
      REAL     FMSLOP
      INTEGER  NFMSVPX
      INTEGER  FTREAT
      REAL     FUAREA(5,4)
      INTEGER  GROW(MAXTRE)
      LOGICAL  HARD(MXSNAG)
      INTEGER  HARTYP
      INTEGER  HARVYR
      REAL     HTDEAD(MXSNAG)
      REAL     HTIH(MXSNAG)
      REAL     HTIS(MXSNAG)
      REAL     HTR1, HTR2, HTX(MAXSP,4), HTXSFT
      INTEGER  ICANSP, ICBHMT
      INTEGER  IFMYR1, IFMYR2
      INTEGER  IFTYR
      INTEGER  ISALVC, ISALVS, ISNGSM
      INTEGER  JCOUT, JFROUT(3), JLOUT(3)
      LOGICAL  LANHED
      REAL     LEAFLF(MAXSP)
      LOGICAL  LFMON, LFMON2
      LOGICAL  LFROUT(3)
      REAL     LIMBRK
      LOGICAL  LSHEAD
      LOGICAL  LDHEAD
      LOGICAL  LDYNFM
      LOGICAL  LVWEST,LREMT
      REAL     MAXHT(MAXSP,19)
      REAL     MINHT(MAXSP,19)
      INTEGER  NSNAG
      INTEGER  NYRS
      INTEGER  OLDCOVTYP, OLDICT, OLDICT2
      REAL     OLDICTWT(2)
      REAL     NZERO, OLDCRL(MAXTRE), OLDHT(MAXTRE), OLDCRW
      DIMENSION OLDCRW(MAXTRE,0:5)
      REAL     PBFRIH(MXSNAG), PBFRIS(MXSNAG), PBSCOR
      REAL     PBSOFT, PBSIZE, PBSMAL, PBTIME
      INTEGER  PBURNYR
      REAL     PERCOV
      REAL     PRDUFF(MXFLCL,4)      
      REAL     PRPILE(MXFLCL)
      REAL     PRSNAG
      REAL     PSOFT(MAXSP)
      REAL     SALVSPA(MXSNAG, 2)
      REAL     SCCF
      REAL     SETDECAY(MXFLCL,4)      
      REAL     SNGNEW(MAXTRE)
      REAL     SNPRCL(6)
      INTEGER  SPS(MXSNAG)
      REAL     TCWD(6),TCWD2(6)
      REAL     TFALL
      DIMENSION TFALL(MAXSP,0:5)
      REAL     TODUFF(MXFLCL,4)
      REAL     TONRMC, TONRMS, TONRMH
      REAL     TOTACR
      REAL     V2T(MAXSP)
      INTEGER  YRDEAD(MXSNAG)
      INTEGER  SPSSALV(MXSNAG),NSNAGSALV
      REAL     DBHSSALV(MXSNAG), HTIHSALV(MXSNAG)
      REAL     HTISSALV(MXSNAG), HTDEADSALV(MXSNAG)
      LOGICAL  HARDSALV(MXSNAG)
      REAL     PREMST(MAXTRE),PREMCR(MAXTRE)
      INTEGER  ISPCC(MAXTRE)
      REAL     DBHC(MAXTRE),HTC(MAXTRE),CROWNWC(MAXTRE,0:5)
      INTEGER  ICYCRM,ITRNL

      COMMON /FMCOM/
     &  ALLDWN, BURNYR, CANMHT, CBHCUT,
     &  COVTYP, CRBURN, CROWNW, CURKIL, CWD, CWD2B,
     &  CWD2B2, CWDCUT, CWDNEW, CWDVOL, CWDCOV,
     &  DBHS, DECAYX, DENIH, DENIS, DEND, DKR, DKRCLS,
     &  DKRDEF, DSPDBH,
     &  FALLX, FLIVE, FIRKIL, FOLMC, FMPROB, FMTBA, NFMSVPX,
     &  FMICR, FMKOD, FMSLOP,FTREAT, FMORTMLT,
     &  FUAREA, GROW, HARD, HARTYP, HARVYR, HTDEAD, HTIH,
     &  HTIS, HTR1, HTR2, HTX, HTXSFT, ICANSP, ICBHMT,
     &  ICYCRM, IFMYR1, IFMYR2, IFTYR, ISALVC, ISALVS, ISNGSM,
     &  ITRNL, JCOUT, JFROUT,
     &  JLOUT,  LANHED, LEAFLF,  LFMON, LFMON2, LFROUT,
     &  LDYNFM, LIMBRK, LDHEAD, LSHEAD, LVWEST, MAXHT, MINHT,
     &  NSNAG, NYRS, NZERO, OLDCOVTYP, OLDCRL, OLDHT, OLDCRW,
     &  OLDICT,OLDICT2,OLDICTWT,
     &  PBFRIH, PBFRIS, PBSCOR, PBSOFT, PBSIZE,
     &  PBSMAL, PBTIME, PBURNYR, PERCOV,
     &  PRDUFF, PRSNAG, PSOFT, PRPILE, SALVSPA,
     &  SCCF, SETDECAY, SNGNEW, SNPRCL, SPS, TCWD, TCWD2, TFALL,
     &  TODUFF, TONRMC, TONRMS, TONRMH,
     &  TOTACR, V2T, YRDEAD, LREMT, NSNAGSALV, HTDEADSALV,
     &  SPSSALV, DBHSSALV, HTIHSALV, HTISSALV, HARDSALV,
     &  PREMST,PREMCR,ISPCC,DBHC,HTC,CROWNWC

***********************************************************************
*     COMMON BLOCK **FMCLCOM**
*     VARIABLES RELATED TO THE EFFECT OF CLIMATE ON FFE PARAMETERS
*     THROUGH CWD DECAY RATE. IMPLEMENTED IN BC VARIANT (2013)
*     Dimensions of Q10CWD AND REFMATCWD arrays must equal
*     to MXFLCL+1 from FMPARMS.F77
*
*     Q10CWD:      Q10 FOR THE MXFMCL+1 DECAY CLASSES; THE +1 IS FOR ROOT 
*                  DECAY RATES, WHICH ARE NOT INCLUDED WITH CWD CATEGORIES
*     REFMATCWD:   REFERENCE TEMPERATURE (CELSIUS) AT WHICH THE DECAY RATE
*                  IS MEASURED. REFERENCE DECAY RATES SET IN FMVINIT
***********************************************************************
      
C     VARIABLE DECLARATIONS: COMMON BLOCK FMCLCOM

!      REAL     REFMATCWD(MXFLCL+1),Q10CWD(MXFLCL+1)
!      COMMON /FMCLCOM/ REFMATCWD, Q10CWD

C.... End segment.







