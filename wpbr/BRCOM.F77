CODE SEGMENT BRCOM
C----------
C WPBR $Id$
C----------
C  Common block variable definitions for the Blister Rust model.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  03-JUN-1999 Lance R. David (FHTET)
C     Added logical variables BRCHDR and BRTHDR to control main header
C     printing on canker and tree list files.
C  13-SEP-2000 Lance R. David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C       Added ISPBR array and IBRDAM variable.
C     Also changed declaration of IBRTID from INTEGER*4 to INTEGER.
C  06-MAR-2001 Lance R. David (FHTET)
C     Added pathological pruning variable, LPATPR.
C     Added branch and bole canker growth rate variables (BRGRTH, BOGRTH).
C  14-MAR-2001 Lance R. David (FHTET)
C     Expanded variable DFACT to array by species and stock type.
C  21-MAR-2001 Lance R. David (FHTET)
C     added variables THPROB and TRETN to common block BRCOM.
C  24-APR-2001 Lance R. David (FHTET)
C     Added species dimension to PRPSTK and RESIST arrays.
C  01-MAY-2001 Lance R. David (FHTET)
C     Expanded tree category scalar variables to arrays.
C  08-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM and changed the manner in which the array 
C     is utilized.
C     WPBR model arrays that were dimensioned by FVS parameter MAXSP
C     is now dimensioned using WPBR parameter NBRSP.
C  11-MAY-2001 Lance R. David (FHTET)
C     Added accumulator for total BR historical mortality (TBRHMR).
C  16-MAY-2001 Lance R. David (FHTET)
C     Expanded canker growth rate variables (BRGRTH, BOGRTH) for
C     species and stock type.
C  06-NOV-2002 Lance R. David (FHTET)
C     Added variables for new RI calculations: MINRI, MAXRI, PKAGE, RISHP
C  08-MAY-2006 Lance R. David (FHTET)
C     Changed random number seed variable names to unique variables
C     BRS0, BRS1, BRSS. Changed variable NTREES to BRNTRECS.
C     Changed array ISTAT to IBRSTAT.
C  15-MAY-2006 Lance R. David (FHTET)
C     Added variables LBRDBH and LBRSUM.
C**********************************************************************
      INTEGER NBRSP
      PARAMETER (NBRSP=2)

C.... Variable declarations.

      DOUBLE PRECISION BRS0,BRS1,BRSS

      LOGICAL BRCHDR,BRCL,BRCMP,BRTHDR,BRTL,BRYES,CKINIT,
     &  LCLEN,LDFACT,LEXGO,LEXMLT(MAXTRE),LMETRIC,LPATPR,
     &  LPRGO,LPRUN,LBRDBH,LBRSUM,LBROUT(NBRSP)

      CHARACTER*4  BRSPC(NBRSP)
      CHARACTER*80 ICFMT

      INTEGER BRSPM(MAXSP),IBRDAM,IBRTID(MAXTRE),ICIN,ICRED(MAXTRE),
     &  IDCOUT,IDTOUT,ILCAN(MAXTRE),INCAN,IBRSTAT(MAXTRE),
     &  ISTCAN(10,MAXTRE),ISTOTY(MAXTRE),ITCAN(MAXTRE),
     &  BRNTRECS(NBRSP),RIMETH,LOTCA

      REAL 
     &  AVECPT(NBRSP),AVGGI(NBRSP),AVGRI(NBRSP),AVGSTS(NBRSP),
     &  AVLCPT(NBRSP),AVTCPT(NBRSP),BOGRTH(NBRSP,4),BRAGE(MAXTRE),
     &  BRDBHO(10,13,41),BRGD(MAXTRE),BRGRTH(NBRSP,4),BRHTBC(MAXTRE),
     &  BROUT(NBRSP,18,41),BRPB(MAXTRE),BRPI,
     &  D2AVE(10),D2AVL(10),D2AVT(10),
     &  D2CLN(10),D2DED(10),D2EXC(10),D2GIR(10),D2NOL(10),D2NOS(10),
     &  D2PIL(10),D2PIT(10),D2PRN(10),D2WP(10),DFACT(NBRSP,4),
     &  DOUT(10,MAXTRE),DUP(10,MAXTRE),ESTCAN(MAXTRE),EXDMIN,GI(MAXTRE),
     &  GIDEF,GIRDL(10,MAXTRE),GIRMAX,GIRMRT,HTMAX(2),HTMIN,HTPRPR,
     &  MAXRI,MINRI,OUTDST,OUTNLD,PILCA(NBRSP),PITCA(NBRSP),PKAGE,
     &  PKSHP,PRPSTK(NBRSP,4),RATINV(2),RESIST(NBRSP,4),RI(MAXTRE),
     &  RIAF(NBRSP),RIBPRP(3),RIDEF,RSF(3),SRATE(2),STSUM(NBRSP),
     &  TBRCLN(NBRSP),TBREXC(NBRSP),TBRGIR(NBRSP),TBRHMR(NBRSP),
     &  TBRHST(NBRSP),TBRMRT(NBRSP),TBRNOL(NBRSP),TBRNOS(NBRSP),
     &  TBRPRN(NBRSP),THPROB(NBRSP),TRETN(NBRSP),TSTARG(MAXTRE),
     &  UPMARK(MAXTRE),TOTCA

      REAL FACTOR(3),RIBSUM(2),RIBUS(2,3)
C.... Common blocks.

      COMMON/DPCOM/BRS0,BRS1,BRSS
      COMMON/BRCHCM/ICFMT,BRSPC
      COMMON/RIBES/FACTOR,RIBUS,RIBSUM,RSF
      COMMON/BRCOM/
     &  AVECPT,AVGGI,AVGRI,AVGSTS,AVLCPT,AVTCPT,BOGRTH,BRAGE,BRCHDR,
     &  BRCL,BRCMP,BRDBHO,BRGD,BRGRTH,BRHTBC,BROUT,BRPB,BRPI,BRSPM,
     &  BRTHDR,BRTL,BRYES,CKINIT,D2AVE,D2AVL,D2AVT,D2CLN,D2DED,D2EXC,
     &  D2GIR,D2NOL,D2NOS,D2PIL,D2PIT,D2PRN,D2WP,DFACT,DOUT,DUP,
     &  ESTCAN,EXDMIN,GI,GIDEF,GIRDL,GIRMAX,GIRMRT,HTMAX,HTMIN,
     &  HTPRPR,IBRDAM,IBRTID,ICIN,ICRED,IDCOUT,IDTOUT,ILCAN,INCAN,
     &  IBRSTAT,ISTCAN,ISTOTY,ITCAN,LCLEN,LDFACT,LEXGO,LEXMLT,LMETRIC,
     &  LOTCA,LPATPR,LPRGO,LPRUN,MAXRI,MINRI,BRNTRECS,OUTDST,OUTNLD,
     &  PILCA,PITCA,PKAGE,PKSHP,PRPSTK,RATINV,RESIST,RI,RIAF,RIBPRP,
     &  RIDEF,RIMETH,SRATE,STSUM,TBRCLN,TBREXC,TBRGIR,TBRHMR,TBRHST,
     &  TBRMRT,TBRNOL,TBRNOS,TBRPRN,THPROB,TOTCA,TRETN,TSTARG,UPMARK,
     &  LBRDBH,LBRSUM,LBROUT

C.... Definitions of variables in BRCOM common blocks.

C....   BRS0 -- Used in the random number generator routine BRANN.
C....   BRS1 -- Used in the random number generator routine BRANN.
C....   BRSS -- Used in the random number generator routine BRANN.
C.... UPMARK -- Distance up from ground level in centimeters of highest
C....           canker in the tree. This is initialized in BRCANK when
C....           canker data is entered. It is changed in BRCGRO.
C....     GI -- Growth index for the individual tree. Calculated in
C....           BRGI.
C....     RI -- Rust index for individual tree. Calculated in BRTARG
C....           and BRTREG.
C....   BRGD -- Tree ground diameter in centimeters. Calculated in
C....           BRSETP and updated in BRUPDT.
C.... BRHTBC -- Height to base of crown for the tree.  This value is
C....           initialized using the Prognosis crown ratio and is
C....           changed by pruning.
C....   BRPB -- Trees/acre dead due to blister rust. Calculated in
C....           BRCGRO.
C.... ESTCAN -- Estimated number of cankers expected in this year.
C....           Calculated in BRECAN.
C.... TSTARG -- Cumulative target area for the individual tree.
C....           Calculated in BRGI.
C....  ITCAN -- Total number of cankers for the individual tree. This
C....           number includes both potentially lethal and non-lethal
C....           cankers. This number can be larger than 10 for the tree.
C....           These cankers are just not tracked by the model.
C....           Calculated in BRCANK. Modified in BRCDEL, BRECAN.
C....  ILCAN -- Number of potentially lethal cankers for the tree.
C....           Actually this number is the cankers that will be tracked
C....           by the model (and therefore have the potential to become
C....           lethal - as opposed to the number stored in ITCAN which
C....           is used to determine spore production but which are
C....           never grown in the model). Maximum number of "lethal"
C....           cankers is 10. Calculated in BRCANK.  Modified in
C....           BRECAN and BRCDEL.
C.... ISTOTY -- Stock type of planted trees. Set in BRESTB. Modified in
C....           BRSTYP.
C.... IBRTID -- Unique tree identification number. Set in BRCANK.
C...  BRSPC  -- BR Host tree species alpha codes. Index values in BRSPM
C...            point to the appropriate position of the species in this
C...            array. Set in BRBLKD.F.
C...  BRSPM  -- A non-zero value in this array is the corresponding tree
C...            species index in the blister rust arrays. This array
C...            index/species order is that of FVS. Species that are
C...            blister rust hosts and represented by this model will
C...            have the appropriate index value in the host species
C...            position. See definition of BRSPC for list of BR host
C...            species and index values.
C...            BRSPM is set in BRBLKD.F and is variant-specific.
C...            0=Species not a host to Blister Rust
C...  IBRDAM -- Blister Rust damage code (initialized in brklkd.f)
C.... BRAGE  -- Tree age loaded from CANKDATA and is incremented on a
C....           yearly basis. If tree age is not supplied with canker
C....           data, Prognosis stand age is used to initialize it in
C....           subroutine BRSOR.
C....  ICRED -- Flag used to determine if crown ratio of the tree is to
C....           be reduced in this cycle. Set in BRCGRO and BRCSTA.
C....IBRSTAT -- Current status of tree based on the worst canker on the
C....           tree. Computed in BRTSTA.
C.... ISTCAN -- Status of individual cankers on the tree. Canker
C....           Classifications are: 0=clean tree, 1=non-lethal canker,
C....           2=prunable canker, 3=excisable canker, 4=non-salvable
C....           canker, 5=tree topkilled by canker and 7= tree killed by
C....           canker.
C....  GIRDL -- Percentage of tree bole circumference which canker has
C....           girdled. Range: 0 to 100 percent.
C.... GIRMRT -- Canker girdling percent which causes death.  This value
C....           is used when determining the canker status to define
C....           when a bole canker's girdling topkills or kills the
C....           tree depending on the cankers height relative to the
C....           height to base of crown.
C....  HTMIN -- Mininmum height in centimeters a canker must be above
C....           ground to qualify for excising.  Bole cankers below
C....           this height are assigned a status code 4 (non-salvable).
C....           Initialized in BRINIT.  changed through EXSPECS keyword
C....           field 6.
C....    DUP -- Distance up from ground level (in centimeters) of canker
C....           for either bole or branch canker. Initialized IN BRCANK.
C....           Assigned for new cankers in BRECAN.
C....   DOUT -- Distance out (in centimeters) of canker on the tree
C....           branch (0 for bole cankers). Initialized in BRCANK.
C....           Assigned for new cankers in BRECAN.
C....  TOTCA -- Total number of cankers.
C....  LOTCA -- Total number of lethal cankers.
C.... LBRDBH -- Logical variable controls writing of DBH class statistics.
C.... LBRSUM -- Logical variable controls writing of summary statistics.
C.... LBROUT -- Logical variable array set true if output info has been
C....           generated for a host to avoid writing tables full of zeros.
C....
C.... The following arrays are "tree category" variables
C....
C.... TBRHST -- Total blister rust host TPA by tree species.
C....           This is a running total for the simulation and not just
C....           current cycle live trees and mortality.
C....           Accumulated in BRSTYP and ???
C....           Should be the sum of the following 7 categories:
C....           (TBRCLN, TBRNOL, TBRPRN, TBREXC, TBRNOS, TBRGIR, TBRMRT)
C.... TBRCLN -- Total host TPA without cankers (i.e. clean).
C....           Calculated in BRTSTA.
C.... TBRNOL -- Total host TPA with non-lethal cankers. Calculated
C....           in BRTSTA.
C.... TBRPRN -- Total host TPA with prunable cankers. Calculated
C....           in BRTSTA.
C.... TBREXC -- Total host TPA with excisable cankers. Calculated
C....           in BRTSTA.
C.... TBRNOS -- Total host TPA with non-salvable cankers.
C....           Calculated in BRTSTA.
C.... TBRGIR -- Total host TPA topkilled by blister rust (i.e. a canker
C....           has girdled a tree above its "mortal" height).
C....           Calculated in BRTSTA.
C.... TBRMRT -- Total blister rust mortality TPA by tree species.
C....           This is a running total for the simulation and not just
C....           current cycle mortality.
C....           Calculated in BRTSTA ???.
C.... TBRHMR -- Total blister rust historical mortality TPA by species.
C....           This is a running total for the simulation. Set in BRINIT
C....           and accumulated in BRCGRO. Used BRSTAT to calculate
C....           proportion of trees infected (PITCA and PILCA).
C....
C.... BRNTRECS-- Number of host species tree records.  Note this is not the
C....           same as TBRHS which is total trees/acre of host species.
C.... AVTCPT -- Average total number of cankers (lethal and non-lethal)
C....           per tree per acre. Calculated in BRSTAT.
C.... AVLCPT -- Average number of lethal cankers per tree per acre.
C....           Calculated in BRSTAT.
C....  BROUT -- Array for storing the blister rust summary output data.
C....           Loaded in BRSUM.
C....   BRPI -- The value of pi, i.e. 3.1417. Set in BRBLKD.
C.... HTPRPR -- Value used to determine the threshold for pruning.
C....           Expressed as a proportion of total tree height below
C....           which cankers will be considered for pruning.
C....           Entered on PRNSPECS keyword. Initialized in BRINIT.
C.... EXDMIN -- Value used to determine the threshold for excising.
C....           This is the minimum DBH of the tree above which bole
C....           cankers will be considered for excising.  Entered on the
C....           EXSPECS keyword.  Initialized in BRINIT.
C....  HTMAX -- Values used to determine thresholds for pruning and
C....           excising. Expressed as maximum absolute tree height in
C....           centimeters below which cankers will be considered for
C....           removal. Initialized in BRINIT.  changed through
C....           PRNSPECS and EXSPECS keywords.
C.... GIRMAX -- Value used to determine whether tree is excisable or
C....           non-salvable. This is the maximum percent of the tree
C....           bole circumference that can be girdled after which a
C....           canker is no longer excisable. Entered in EXSPECS
C....           keyword. Initialized in BRINIT.
C....  SRATE -- Used to set the 'success rate' of pruning or excising.
C....           Entered on the PRUNE and EXCISE keywords. Initialized
C....           in BRINIT.
C....  AVGRI -- Average value of rust index for the stand. Calculated in
C....           BRTREG.
C....  AVGGI -- Average value of growth index for the stand. Calculated
C....           in BRTREG.
C....  DFACT -- Stand deviation factor array by species and stock type.
C....           Calculated in BRTARG.
C....           Initialized in BRINIT. Can be set with RUSTINDX keyword.
C....           When set by keyword, the supplied value WILL be used.
C....           Stand deviation factor cannot be less than 0.01.
C.... LDFACT -- Logical variable set to true when the stand deviation
C....           factor is provided by keyword RUSTINDX.
C....  STSUM -- Sum of target areas for the entire stand in the current
C....           cycle.
C.... AVGSTS -- Average sum of target area for the stand in the current
C....           cycle.
C.... AVECPT -- Average number of expected cankers per tree per acre.
C....           Calculated in BRSTAT.
C....  PILCA -- Proportion of trees per acre infected with lethal
C....           cankers. Calculated in BRSTAT.
C....  PITCA -- Proportion of trees per acre infected with lethal and
C....           non-lethal cankers. Calculated in BRSTAT.
C.... THPROB -- Total host trees per acre used in calculating PITCA
C....           and monitoring the increase of proportion of host trees
C....           with infections.
C....  TRETN -- Total trees per acre of host with infections (lethal or
C....           non-lethal).
C....LMETRIC -- Logical variable set to TRUE if canker data is provided
C....           in centimeters; FALSE if data is in feet and inches.
C....           Initialized in BRINIT. Changed by CANKDATA keyword.
C....   ICIN -- Logical unit number for where to read the canker data
C....           from.  Initialized in BRBLKD.  Also entered in the
C....           CANKDATA keyword.
C.... IDTOUT -- Logical unit number for where detailed tree output is
C....           to be written. Initialized in BRBLKD. Also entered on
C....           BRTLST keyword.
C.... IDCOUT -- Logical unit number for where detailed canker output is
C....           to be written. Initialized in BRBLKD. Also entered on
C....           BRCLST keyword.
C....  ICFMT -- Format used to read the input canker data file.
C....           Initialized in BRBLKD. Reset using the CANFMT keyword.
C....  RIBPRP - Real array of 3. Each ribes species proportion of
C....           total ribes population. Used for weighted Average
C....           method of determining stand rust index from basal area.
C....  RIDEF -- Default Rust Index value. Initialized in BRINIT.
C....           Entered directly or calculated as specified by RUSTINDX
C....           keyword.  Also may be calculated directly from ribes
C....           populations which is controlled by RIBES keyword.
C.... RIMETH -- Rust Index assignment method.  Initialized in BRINIT.
C....           Controls how the stand rust index (RIDEF) will be
C....           calculated and assigned to individual trees.
C....           Value changed only through RUSTINDX keyword.
C....  GIDEF -- Default growth index value. Initialized in BRINIT.
C.... OUTDST -- Distance out in the TREE branch in centimeters which
C....           defines a prunable canker. If a canker is at a distance
C....           out greater than the value of OUTDST then the canker is
C....           comsidered to be prunable. If the canker is at a
C....           distance out less than the value of OUTDST then the
C....           canker is considered non-prunable. Initialized in BRINIT
C....           changed through PRNSPECS keyword.
C.... OUTNLD -- Distance out on the tree branch (in centimeters) which
C....           defines a non-lethal canker. If a canker is at a
C....           distance out greater than the value of OUTNLD then the
C....           canker is considered to be non-lethal. Initialized in
C....           BRINIT. Change through the PRNSPECS keyword.
C.... RESIST -- Resistance factor to blister rust for the four types of
C....           planting stock for each host species. 
C....           The types are: 1=WILD stock, 2=F1 stock, 3=F2 stock and
C....           4=F3 stock. Factors are set in BRINIT and changed using
C....           the STOCK keyword.
C.... RATINV -- Canker inactivation rates. Initialized in BRINIT.  Also
C....           set using the INACT keyword.  RATINV(1) holds the canker
C....           inactivation rate for branch cankers; RATINV(2) holds
C....           the same for bole cankers.
C....  INCAN -- Number of unique tree ID's encountered in the blister
C....           rust canker data file. Calculated in BRCANK.
C.... PRPSTK -- Proportions of each host species stock type making up the
C....           population. Initialized in BRINIT and changed using the
C....           STOCK keyword.
C....  BRYES -- Logical variable whcih is TRUE if the blister rust model
C....           is being used in the current simulation. Initialized in
C....           BRINIT. Set in BRIN.
C.... BRCHDR -- Logical variable controlling the printing of initial header
C....           information in the detailed canker list output file.
C....           Set to TRUE in BRINIT. 
C....   BRCL -- Logical variable which triggers the detailed canker
C....           output. Initialized in BRINIT to FALSE. Set to TRUE with
C....           the BRCLST keyword.
C.... BRTHDR -- Logical variable controlling the printing of initial header
C....           information in the detailed tree list output file.
C....           Set to TRUE in BRINIT. 
C....   BRTL -- Logical variable which triggers the detailed tree list
C....           output. Initialized in BRINIT to FALSE. Set to TRUE with
C....           the BRTLST keyword.
C....  BRCMP -- Logical variable used to trigger assignment of canker
C....           statuses when a compression has been performed.
C....           subroutine BRCSTA will be called from subroutine BRPR
C....           when BRCMP is true.
C.... BOGRTH -- Bole canker diameter growth rate (cm)-specie & stock type.
C.... BRGRTH -- Branch canker radial growth rate (cm)-specie & stock type.
C.... LPATPR -- Logical variable for pathological pruning (do not change
C....           crown base heihgts) when true. Default is false.
C....  LPRUN -- Logical variable which indicated whether prunable trees
C....           will be pruned when a pruning is scheduled. Initialized
C....           to FALSE in BRINIT. Set to TRUE in BRTREG.
C....  LCLEN -- Logical variable which indicated whether clean trees
C....           will be pruned when a pruning is scheduled. Initialized
C....           to FALSE in BRINIT. Set to TRUE in BRTREG.
C....  LPRGO -- Logical variable which triggers a scheduled pruning. Set
C....           to FALSE in BRINIT. Reset to TRUE in BRTREG in the cycle
C....           in which pruning has been scheduled.
C....  LEXGO -- Logical variable which triggers a scheduled excising.
C....           Set to FALSE in BRINIT. Reset to TRUE in BRTREG in the
C....           cycle in which excising has been scheduled.
C....  RIBUS -- Array holding the number of ribes bushes per acre of
C....           the three different species of ribes.
C....           Initialized in BRBLKD. Changed with the RIBES keyword.
C.... FACTOR -- Real array of 3.  Holds calculated RI values for the
C....           three ribes species populations.  Calculated in BRIBES.
C....           After call to BRIBES, the values remaining in array are
C....           from the new population values stored in RIBUS(2,X).
C.... RIBSUM -- Array used to hold summed variables for use in
C....           calculating reduction factor.
C....    RSF -- Real array of 3. Ribes Species Factors (Hud, Lac, Vis).
C....           Calibration values for RI base line function in
C....           subroutines BRIBES and BRIBA.
C.... LEXMLT -- Logical array of size MAXTRE.  LEXMLT will be set to
C....           TRUE for a tree that has more than one (i.e. multiple)
C....           excisable cankers in any given cycle.  This is used
C....           for when excising is scheduled; in the field trees
C....           with more than 1 excisable canker will not be excised
C....           at all.  Initialized in BRINIT, updated in BRCGRO and
C....           BRCSTA, used in BRCREM.
C.... BRDBHO -- Real, 3-dimensional storage array for holding summary
C....           information for the 2-inch DBH class blister rust table.
C....           Loaded in BRSUM. BRDBHO(10,13,41) holds 10 2-inch DBH
C....           classes, 13 categories of information (see D2 variables
C....           below) and up to 41 cycles worth of data for the table.
C.... D2AVT  -- Real array to store the average total cankers/tree/acre
C....           data by 2-inch DBH class. Calculated in BRSTAT.
C.... D2AVL  -- Real array to store the average lethal cankers/tree/acre
C....           data by 2-inch DBH class. Calculated in BRSTAT.
C.... D2AVE  -- Real array to store the average expected canks/tree/acre
C....           data by 2-inch DBH class. Calculated in BRSTAT.
C.... D2PIT  -- Real array to store the proportion of infected
C....           trees/acre total (lethal and non-lethal) by 2-inch DBH
C....           class.  Calculated in BRSTAT.
C.... D2PIL  -- Real array to store the proportion of infected
C....           trees/acre (lethal only) by 2-inch DBH class.
C....           Calculated in BRSTAT.
C.... D2CLN  -- Real array to store total clean TPA by 2-inch DBH class.
C....           Calculated in BRTSTA.
C.... D2NOL  -- Real array to store total non-lethal TPA by 2-inch DBH
C....           class. Calculated in BRTSTA.
C.... D2PRN  -- Real array to store total prunable TPA by 2-inch DBH
C....           class. Calculated in BRTSTA.
C.... D2EXC  -- Real array to store total excisable TPA by 2-inch DBH
C....           class. Calculated in BRTSTA.
C.... D2NOS  -- Real array to store total non-salvable TPA by 2-inch DBH
C....           class. Calculated in BRTSTA.
C.... D2GIR  -- Real array to store total girdled/topkilled TPA by
C....           2-inch DBH class. Calculated in BRTSTA.
C.... D2DED  -- Real array to store total dead (from WPBR) TPA by
C....           2-inch DBH class. Calculated in BRTSTA.
C.... D2WP   -- Real array to store total TPA that are white pine by
C....           2-inch DBH class. Should be the totals in each of the
C....           DBH classes of each of the statuses (D2CLN, D2NOL,
C....           D2PRN, D2EXC, D2NOS, D2GIR, D2DED). Calculated in
C....           BRTSTA.
C.... CKINIT -- Logical flag set with the CANKDATA keyword to signal
C....           that canker up, out, and girdle measurements will be
C....           randomly generated according to the canker counts read
C....           from the canker data (required last canker record for
C....           each tree); cankers are generated in BRCINI if this
C....           option is requested, and if the count dictates.
C.... RIAF   -- Rust index adjustment factor (real).  Calculated once
C....           from initial tree conditions in BRTARG and used to
C....           adjust the default rust index. Default value is set to
C....           1.0 in BRINIT to allow for simulations that do not have
C....           initial canker information (tree conditions) which 
C....           includes bare ground runs where all trees are PLANTed.
C.... MINRI  -- minumum Rust Index (in subroutine BRICAL)
C.... MAXRI  -- maximum Rust Index (in subroutine BRICAL)
C.... PKAGE  -- stand age (exposure time) when Rust Index peaks
C....           (in subroutine BRICAL)
C.... RISHP  -- defines shape of curve about the peak (in subroutine BRICAL)
