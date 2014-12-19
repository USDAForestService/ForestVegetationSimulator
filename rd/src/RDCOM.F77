CODE SEGMENT RDCOM
C----------
C  **RDCOM       LAST REVISION:  08/19/14
C----------
C REVISION HISTORY:
C   27-JUNE-2002 Lance David
C     Added variable descriptions found in the original western root disease
C     model.
C-------------------------
      INTEGER HOST(3*MAXSP)
      INTEGER IBBOUT, IDOBB, IDITYP(ITOTSP), IFRRC, IIEND, ILEND,
     &        IMCOUT, INFLAG, IOUNIT, IPCFLG(ITOTRR), IPUSH, IRCOMP,
     &        IRDOUT, IRFLAG, IRGEN(10), IRHAB, IRINIT, IRIPTY,
     &        IROOT, IRRSP, IRSNYR, IRSPTY, IRSTYP, IRTSPC(MAXSP),
     &        IRUNIT, ISPS(ITOTSP), ISTEP, ISTFLG, IYEAR,
     &        JRAGED(ITOTRR,2,5,41), JRSIT, MCTREE(ITOTRR,50),
     &        NCENTS(ITOTRR), NINSIM, NMONT, NNCENT, NNDEC, NNDMX,
     &        NNINF, NRSTEP, NSCEN(ITOTRR), NSTUMP, NTREES, NUMBB,
     &        NUMTRE

      REAL    IURATE(3*MAXSP), IIRATE(3*MAXSP),
     &        MCRATE(ITOTRR,50), MINDBH(3*MAXSP), MININF(3*MAXSP)

      REAL   AGECUR, AREANU(ITOTRR), CORINF(ITOTRR,2), CURAGE,
     &       DBHD(ITOTRR,2,5), DBHDA(ITOTRR,2,5,41), DBIFAC(ITOTSP),
     &       DECRAT(ITOTRR,2,5,41), DGTOT, DICLAS(4), DIMEN, DSEED,
     &       DSFAC(2), EXPINF(ITOTRR,2), FRINGE(ITOTRR),
     &       HABFAC(ITOTSP,ITOTRR,2), HTIFAC(ITOTSP), HTIMP, HTTOT,
     &       OFRATE(3*MAXSP), OOAREA(ITOTRR), ORATE(3*MAXSP),
     &       PAREA(ITOTRR), PCENTS(ITOTRR,100,3),
     &       PCOLO(ITOTSP,ITOTRR), PINSET, PISIZE(ITOTRR),
     &       PKILLS(ITOTSP,ITOTRR), PNINF(ITOTSP,ITOTRR), PPUSH,
     &       PRANKL(IRRTRE), PRINF(ITOTRR+MAXSP), PRKILL(ITOTRR),
     &       PROBD(ITOTRR,2,5), PROBDA(ITOTRR,2,5,41), PROOT(ITOTSP),
     &       PRPTOT(ITOTRR+MAXSP), PRREM, PRUN(ITOTRR), PTRE,
     &       RADINF(50,2), RDKILL(IRRTRE), REINEK(4), ROCRI(MAXSP),
     &       ROCRL(MAXSP), ROOT4(4), ROOTD(ITOTRR,2,5),
     &       ROOTDA(ITOTRR,2,5,41), ROOTH(4,IRRTRE), ROTSIT,
     &       ROWDOM(ITOTSP), ROWIBP(ITOTSP,2), ROWIND, ROWMIN,
     &       RRATES(ITOTRR,100), RRGEN(ITOTRR,10), RRIARE, RRIDIM,
     &       RRIMEN, RRINCS(ITOTRR), RRIRAD(IRRTRE), RRJINC(ITOTRR),
     &       RROBMR(ITOTRR), RROBNK(MAXSP), RROBOL(4), RROBRD(4),
     &       RROBSC(4), RROBTS(4), RROOTT(IRRTRE), RRPSWT(ITOTSP),
     &       RRRATE(ITOTRR), RRRSET(ITOTRR), RRSARE, RRSDBH(50),
     &       RRSFRN, RRSRAD(50), RSLOP(ITOTSP), SAREA, SDRATE(IRRTRE),
     &       SHCENT(ITOTRR,100,3), SPRQMD(ITOTRR,50),
     &       SSSFAC(ITOTSP,ITOTRR), STCUT(5), STEMSI(MAXSP),
     &       STEMSL(MAXSP), TNJUMP(ITOTRR), TSTEMS, TXP12(4), WINDN,
     &       WINDSP(MAXSP,4), WK22(IRRTRE), XDBH(2), XHT(2),
     &       XMINKL(ITOTRR), XMINLF(ITOTRR), XMTH(4,IRRTRE),
     &       XRRI(IRRTRE), XRRS(50), XXINF(5), YDBH(2), YHT(2),
     &       YRRI(IRRTRE), YRRS(50), YTKILL, YYINF(5)

      REAL   OLDPRP                 ! From common that was directly coded in rdranp.f
      
      REAL   CDF(1001)
c      DOUBLE PRECISION CDF(1001)    ! From common that was directly coded in rdranp.f
      
      LOGICAL LBBON, LRTYPE, LXNOTE

      COMMON /RDCOM/ AGECUR, AREANU, CORINF, CURAGE, DBHD, DBHDA,
     &               DBIFAC, DECRAT, DGTOT,DICLAS, DIMEN, DSEED,
     &               DSFAC, EXPINF, FRINGE, HABFAC, HOST, HTIFAC,
     &               HTIMP, HTTOT, IBBOUT, IDITYP, IDOBB, IFRRC,
     &               IIEND, IIRATE, ILEND, IMCOUT, INFLAG, IOUNIT,
     &               IPCFLG, IPUSH, IRCOMP, IRDOUT, IRFLAG, IRGEN,
     &               IRHAB, IRINIT, IRIPTY, IROOT, IRRSP,IRSNYR,
     &               IRSPTY, IRSTYP, IRTSPC, IRUNIT, ISPS, ISTEP,
     &               ISTFLG, IURATE, IYEAR, JRAGED, JRSIT, LBBON,
     &               LRTYPE, LXNOTE, MCRATE, MCTREE, MINDBH, MININF,
     &               NCENTS, NINSIM, NMONT, NNCENT, NNDEC, NNDMX,
     &               NNINF, NRSTEP, NSCEN, NSTUMP, NTREES, NUMBB,
     &               NUMTRE, OFRATE, OOAREA, ORATE, PAREA, PCENTS,
     &               PCOLO, PINSET, PISIZE, PKILLS, PNINF, PPUSH,
     &               PRANKL, PRINF, PRKILL, PROBD, PROBDA, PROOT,
     &               PRPTOT, PRREM, PRUN, PTRE, RADINF, RDKILL,
     &               REINEK, ROCRI, ROCRL, ROOT4, ROOTD, ROOTDA,
     &               ROOTH, ROTSIT, ROWDOM, ROWIBP, ROWIND, ROWMIN,
     &               RRATES, RRGEN, RRIARE, RRIDIM, RRIMEN, RRINCS,
     &               RRIRAD, RRJINC, RROBMR, RROBNK, RROBOL,
     &               RROBRD, RROBSC, RROBTS, RROOTT, RRPSWT,
     &               RRRATE, RRRSET, RRSARE, RRSDBH, RRSFRN,
     &               RRSRAD, RSLOP, SAREA, SDRATE, SHCENT, SPRQMD,
     &               SSSFAC, STCUT, STEMSI, STEMSL, TNJUMP, TSTEMS,
     &               TXP12, WINDN, WINDSP, WK22, XDBH, XHT, XMINKL,
     &               XMINLF, XMTH, XRRI, XRRS, XXINF, YDBH, YHT,
     &               YRRI, YRRS, YTKILL, YYINF

      COMMON  /PRPDAT/ OLDPRP       ! From common that was directly coded in rdranp.f
      COMMON  /PRPDATD/ CDF         ! From common that was directly coded in rdranp.f

C     AGECUR  -- YEAR IN WHICH CARRYOVER IS TO OCCUR
C                (** CURRENTLY NOT USED)
C                USED)
C     AREANU  -- THE NUMBER OF ACRES OF NEW AREA INFECTED BY ROOT
C                DISEASE IN THE CURRENT CYCLE.
C     CORINF  -- (,1) NUMBER OF TREES INFECTED THIS CYCLE INSIDE THE
C                'CORE' OF THE CENTER
C             -- (,2) NUMBER OF TREES INSIDE 'CORE' BEOFRE INFECTION
C                OCCURS
C     CURAGE  --
C     DBIFAC  --
C     DECRAT  --  DECAY RATE OF ROOTS
C     DGTOT   --
C     DICLAS  --
C     DIMEN   --
C     DSEED   --
C     DSFAC   --
C     EXPINF  -- (,1) NUMBER OF TREES INFECTED THIS CYCLE FROM
C                EXPANDING THE CENTER
C             -- (,2) NUMBER OF TREES IN THE NEWLY EXPANDED AREA
C     FRINGE  -- the area in the stand within one average root radius
C                of a center
C     HABFAC  -- (TREE SP,RR,1-2) RELATIVE TIME TO DEATH.
C                (,,2) USED IF WANT TO CHANGE TIME TO DEATH AFTER SOME
C                POINT IN TIME
C     HTIFAC  --
C     HTIMP   --
C     HTTOT   --
C     IBBOUT  -- OUTPUT FILE NUMBER FOR BARK BEETLE MORTALITY TABLE
C     IDITYP  -- DISEASE TYPE: 1 = P-TYPE ANNOSUS
C                              2 = S-TYPE ANNOSUS
C                              3 = ARMILLARIA
C                              4 = PHELLINUS
C     IDOBB  - COUNTER THAT IS USED TO KEEP TRACK OF THE NUMBER OF
C              BARK BEETLE EVENTS THAT ARE SCHEDULED.
C     IMCOUT  -- OUTPUT FILE NUMBER FOR SPREADRATE MONTE CARLO RESULTS
C     INFLAG  --
C     IPCFLG  -- FLAG FOR INITIALIZATION OF ROOT DISEASE.  0 = PATCHES
C                PLACED RANDOMLY.  1 = PATCHES PLACED BY THE USER.
C     IPUSH   --
C     IRFLAG  --
C     IRGEN(1) -- THE NUMBER OF CYCLES AFTER STAND ENTRY IN WHICH THE
C                CARRYOVER MODEL IS CALLED.  SET BY THE KEYWORD CARRY
C                DEFAULT = 4.  (*** NOT BEING USED IN COMBINED ROOT
C                DISEASE MODEL)
C     IRGEN(2) -- THE UNIT NUMBER WHERE THE "MACHINE READABLE" OUTPUT
C                IS WRITTEN.
C     IRGEN(3) -- NOT USED.
C     IRGEN(4) -- THE YEAR IN WHICH THE TREE SPATIAL DISTRIBUTION IN
C                THE STAND CHANGES FROM RANDOM TO LATTICE OR LATTICE TO
C                RANDOM, DEPENDING ON THE INITIAL DISTRIBUTION.  SET
C                BY THE KEYWORD TDISTN.
C     IRGEN(5..6) -- NOT USED.
C     IRGEN(7) -- FLAG THAT IS SET TO 1 IF THE "MACHINE READABLE"
C                OUTPUT IS WANTED.
C     IRGEN(8) -- THE YEAR IN WHICH THE TIME TO DEATH MULTIPLIERS ARE
C                CHANGED.  SET BY KEYWORD TTDMULT.
C     IRGEN(9) -- THE YEAR IN WHICH THE PROBABILITIES OF INFECTION
C                ARE CHANGED.  SET BY KEYWORD INFMULT.
C     IRHAB   -- USED WITH THE ARRAY HABFAC THAT HOLDS THE TIME TO DEATH
C                MULTIPLIERS.  SET TO 1 FOR THE DEFAULT VALUES AND SET
C                TO 2 WHEN THE USER ENTERED VALUES ARE TO REPLACE THE
C                DEFAULT VALUES.
C     IRINIT  -- SET TO 13500 (10 * MAX TREE RECORDS IN PROGNOSIS).
C                USED TO CALCULATE NUMTRE IN RRINSD.
C     IRIPTY  --
C     IROOT   -- SIMPLY INDICATES THAT A BLOCK OF RD KEYWORDS HAS BEEN
C                ENCOUNTERED AND THE RD MODEL IS ACTIVE.
C     IRSNYR  --
C     IRSPTY  --
C     IRSTYP  --
C     IRTSPC  --
C     ISPS    -- WOOD TYPE (1=RESINOUS, 2=NON-RESINOUS)
C     ISTEP   -- COUNTER FOR WHICH SLOT TO PUT STUMPS AND INFECTED
C                TREES IN: IT IS ALWAYS EQUAL TO ONE MORE THAN ICYC
C                SINCE IN THE INITIALIZATION PHASE (ICYC=0), INFECTED
C                STUFF MUST BE PUT INTO THE FIRST SLOT (ISTEP=1).
C                IN THE FIRST CYCLE (ICYC=1), STUFF IS PUT INTO THE
C                SECOND SLOT (ISTEP=2)
C     IYEAR   --
C     JRSIT   -- NUMBER OF YEARS TO 'SIT' WITHOUT DECREASING ROOT RADIUS
C     LBBON  - LOGICAL FLAG TO DETERMINE IF THE BARK BEETLES WERE
C              CLEARED OR NOT
C     LRTYPE - Logical flag to track implementation of the the RRTYPE
C              keyword in a model run.  A warning is printed to the
C              output file that the default, annosus disease, is used
C              in the simulations.
C     MCRATE  -- MONTE CARLO SIMULATION RESULTS (EACH RUN)
C     MCTREE  -- NUMBER OF TREES IN EACH MONTE CARLO SIMULATION
C     NCENTS  -- NUMBER OF CENTERS IN EACH DISEASE TYPE
C     NINSIM  -- NUMBER OF TIMES TO SIMULATE THE INSIDE INFECTION CALCULATION
C     NMONT   -- NUMBER OF TIMES TO SIMULATE THE EXTERNAL SPREADRATE CALCULATION
C     NNCENT  --
C     NNDEC   --
C     NNDMX   --
C     NNINF   --
C     NRSTEP  --
C     NSCEN   --  NUMBER OF SHRINKING CENTERS
C     NSTUMP  --
C     NTREES  -- actual number of trees used in the simulation spread rate
C     NUMTRE  -- 
C     OOAREA  --
C     PAREA   --
C     PCENTS  --
C     PCOLO   -- (TREE SP,RR) PROPN ROOT RADIUS COLONIZED AFTER DEATH
C     PINSET  --
C     PISIZE  --
C     PKILLS  -- (TREE SP,RR) PROPN ROOT RADIUS COLONIZED CAUSING DEATH 
C     PNINF   -- (TREE SP,RR) PROB. OF INFECTION GIVEN INSTANCE OF ROOT OVERLAP
C     PPUSH   --
C     PRINF   --
C     PRKILL  --
C     PROOT   --
C     PRPTOT  --
C     PRREM   --
C     PRUN    --
C     PTRE    --
C     RADINF  --
C     RDKILL  --
C     ROTSIT  --  ROOT RADIUS AT WHICH TO BEGIN 'SITTING' WITHOUT DECREASING
C     RRATES  --  SPREAD RATES FOR EACH CENTER
C     RRGEN   --
C     RRIARE  --
C     RRIDIM  --
C     RRIMEN  --
C     RRINCS  --
C     RRIRAD  --
C     RRRATE  --
C     RRRSET  --
C     RRSARE  --
C     RRSDBH  --
C     RRSFRN  --
C     RRSRAD  --
C     RSLOP   --
C     SAREA   --
C     SDRATE  --  STANDARD DEVIATION OF THE MONTE CARLO SIMULATION RESULTS
C     SHCENT  --  SHRINKING CENTER CHARACTERISTICS: 1=CENTER RADIUS WHICH
C                 SHRINKS IMMEDIATELY; 2=ANNUAL RATE OF THIS SHRINKAGE;
C                 3=YEARS AFTERWARDS OF NO SHRINKAGE (BUT WHEN TIME IS UP,
C                 THE WHOLE CENTER DISAPPEARS)
C     SPRQMD  --  QUADRATIC MEAN DIAM. OF TREES IN MONTE CARLO SIMULATION
C     SSSFAC  --
C     XDBH    --
C     XHT     --
C     XRRI    --
C     XRRS    --
C     XXINF   --
C     YDBH    --
C     YHT     --
C     YRRI    --
C     YRRS    --
C     YTKILL  --
C     YYINF   --
C
C --------------------------- WINDTHROW ---------------------------
C
C     ROWIBP  --
C     ROWDOM  --
C     ROWIND  --
C     ROWMIN  --
C     STEMSL  --
C     STEMSI  --
C     ROCRL   --
C     ROCRI   --
C     ILEND   --
C     IIEND   --
C     WINDSP  --
C     TSTEMS  --
C     WINDN   --
C
C --------------------------- BARK BEETLES ------------------------
C     NUMBB   -- NUMber of Bark Beetles eligible to act in this time
C                period, which cannot exceed 3*MAXSP
C     FRINGE(ITOTRR) -- the area in the stand that is within an average
C                root radius of a root rot infection center, for each
C                root rot type.  This area may experience greater
C                mortality due to bark beetles.
C     HOST(3*MAXSP) -- host tree species on which each beetle acts
C     MINDBH(3*MAXSP) -- MINimum host DBH at which trees become
C                susceptible to each BB
C     MININF(3*MAXSP) -- MINnimum proportion of roots INFected at which
C                IIRATE applies
C     ORATE(3*MAXSP) -- mortality rate Outside infection centers (and
C                any associated 'fringe') caused by each BB
C     OFRATE(3*MAXSP) -- mort. rate Outside infection centers in the
C                'Fringe' area that may experience greater mortality
C     IURATE(3*MAXSP) -- mort. rate Inside infection centers of
C                Uninfected trees, each BB
C     IIRATE(3*MAXSP) -- mort. rate Inside infection centers of Infected
C                trees, each BB
C     RROBBO --
C     RROBMR (BEETLE TYPE)  -- ARRAY THAT HOLDS THE MORTALITY RATE
C                THAT IS APPLIED TO ALL ELIGIBLE TREES IF AN OUTBREAK
C                OCCURS FOR EACH BEETLE TYPE.  THESE VALUES CAN BE
C                CHANGED BY THE FOUR BARK BEETLE KEYWORDS
C                (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
C     RROBSC (BEETLE TYPE)  -- ARRAY THAT HOLDS THE MINIMUM DBH (INCHES)
C                FOR TREES WHICH WILL BE CONSIDERED ELIGIBLE FOR ATTACK
C                BY EACH BEETLE TYPE.  THESE VALUES CAN BE CHANGED BY
C                THE FOUR BARK BEETLE KEYWORDS
C                (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
C     RROBRD  --
C     RROBNK  --
C     RROBTS (BEETLE TYPE)  -- ARRAY THAT HOLDS THE TREE SPECIES TO
C                INFEST FOR EACH BARK BEETLE TYPE.  THESE VALUES CAN BE
C                CHANGED BY THE FOUR BARK BEETLE KEYWORDS
C                (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
C     RROBTY  --
C     RROBOL (BEETLE TYPE)  -- ARRAY THAT HOLDS THE MINIMUM DENSITY
C                (TREES/ACRE) OF ELIGIBLE TREES WHICH MUST BE PRESENT
C                FOR AN OUTBREAK TO OCCUR FOR EACH BARK BEETLE TYPE.
C                FOR BARK BEETLE TYPE 2 THIS VALUE IS THE MINIMUM NUMBER
C                OF TREES THAT MUST BE WINDTHROWN FOR THE OUTBREAK TO
C                OCCUR. THESE VALUES CAN BE CHANGED BY THE FOUR BARK
C                BEETLE KEYWORDS (BBTYPE1, BBTYPE2, BBTYPE3, BBTYPE4)
C     DBHD (root disease species, tree type, stump class) -- (The tree
C                types are heartwood and non-heartwood.  The stumps are
C                broken into 5 size classes).  This array holds the mean
C                stump diameter (inches) for the stumps in each tree
C                type for each stump class.     
C     STCUT   --  BREAKPOINTS FOR THE STUMP SIZE CLASSES
C     ISPS    --
C     IRCOMP  --
C     ROOTH   --
C     XMTH    --
C     WK22    --
C     RROOTT  --
C     TXP12   --
C     ROOT4   --
C     IRRSP   -- FLAG THAT HOLDS THE ROOT DISEASE TYPE THAT BEING
C                SIMULATED:
C                1 = P-TYPE ANNOSUS
C                2 = S-TYPE ANNOSUS
C                3 = ARMILLARIA
C                4 = PHELLINUS
C     IFRRC   --
C     PNINF   --
C     TNJUMP  --
C     XMINKL  --
C     XMINLF  --
C     RRPSWT  --
C     PROBD   -- (DISEASE TYPE, TREE TYPE, STUMP CLASS)
C                THE TREE TYPES ARE HEARTWOOD AND NON-HEARTWOOD.
C                THE STUMPS ARE BROKEN INTO 5 DIAMETER CLASSES.
C                THIS ARRAY HOLDS THE NUMBER OF STUMPS IN THE ROOT
C                DISEASE AREA FOR EACH TREE TYPE AND STUMP CLASS FOR
C                THE CURRENT CYCLE.
C     PROBDA  -- (DISEASE TYPE, TREE TYPE, STUMP CLASS, TIME STEP)
C                THE TREE TYPES ARE HEARTWOOD AND NON-HEARTWOOD.  THE 
C                STUMPS ARE STORED IN 5 DIAMETER CLASSES. THIS ARRAY HOLDS
C                THE NUMBER OF STUMPS IN THE ROOT DISEASE AREA FOR EACH
C                TREE TYPE, STUMP CLASS AND THE CYCLE (TIME STEP) IN WHICH
C                THE STUMPS ARE CREATED.  ARRAY IS UPDATED IN RDCNTL,
C                RDPUSH, RDSPOR, RDSTP, AND RDINOC.
C     ROOTD   -- (DISEASE TYPE, TREE TYPE, STUMP CLASS) THE TREE TYPES ARE
C                HEARTWOOD AND NON-HEARTWOOD.  THE STUMPS ARE STORED IN
C                5 SIZE CLASSES.  THIS ARRAY HOLDS THE ROOT RADIUS (FEET)
C                FOR THE STUMPS IN EACH TREE TYPE FOR EACH STUMP CLASS.
C     ROOTDA  -- (DISEASE TYPE, TREE TYPE, STUMP CLASS, TIME STEP)
C                4 DIMENSIONAL ARRAY THAT HOLDS THE STUMP ROOT RADIUS (FEET)
C                FOR THE STUMPS CREATED IN EACH TIME STEP.  THE STUMPS ARE
C                CATEGORIZED BY TREE TYPE (HEARTWOOD AND NON-HEARTWOOD) AND BY
C                STUMP CLASS (THERE ARE 5 STUMP DIAMETER CLASSES).  THE
C                ARRAY IS UPDATED IN RDSTP AND RDSPOR, DECAYED IN RDINOC.
C     DBHDA   -- (root disease species, tree type, stump size class,
C                time step) -- 4 dimensional array that holds the 
C                stump diameter (inches) for the stumps created in
C                each time step.  The stumps are catergorized by root
C                disease species, tree type (heartwood and non-heartwod)
C                and by stump class (there are 5 stump diameter classes).
C                The array is updated in RDSTP and RDSPOR and decayed in
C                RDINOC. 
C     JRAGED  --
C     RRJINC  --
C     IOUNIT  --
C     IRDOUT  --
C     IRUNIT  --
C
C-----END SEGMENT
