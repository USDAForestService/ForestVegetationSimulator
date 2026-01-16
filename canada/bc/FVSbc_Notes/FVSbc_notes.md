# FVSbc Notes

..canada/bc/becset.f
    No modification needed

../canada/bc/blkdat.f
    No modification needed

../canada/bc/bratio.f
    No modification needed

../canada/bc/ccfcal.f
    No modification needed

../canada/bc/cfvol.f
    No modification needed

../canada/bc/cratet.f
    No modification needed

../canada/bc/crown.f
    No modification needed

../canada/bc/dgdriv.f
    Changed reporting of ISI=INT(SITEAR(ISISP)) to ISI=NINT(SITEAR(ISISP))

../canada/bc/dgf.f
    No modification needed

../canada/bc/dubscr.f
    No modification needed

../canada/bc/forkod.f
    No modification needed

../canada/bc/grincr.f
    Modified reporting format of site index (int from float) and SDI
    Added sawlog cubic parameters; adjusted reporting from MCF from WK1 to MCFV

../canada/bc/grinit.f
    Added empty default codes for ECOREG, CFCTYPE, BFCTYPE
    Added sawlog cubic foot initializers for stump, topd, and minimum diameter, set to same values as board foot
    Added SCFMIN (=0), ISTDORG (Stand orgin (=0)), LFIANVB (=FALSE)

../canada/bc/grohed.f
    No modification needed

../canada/bc/habtyp.f
    No modification needed

../canada/bc/htgf.f
    No modification needed

../canada/bc/initre.f
    Expanded array limit from 7 to 12 to match FFE and allow for expanded keyword options (ARRAY, LNOTBK, KARD) 
    Added INTEGER variables IFIACODE,MRCHLMTS
    Added GO TO 14700 for new keyword FIAVBC
    Removed SELECT CASE non BC code from Option 1 Process (~line396)
        Consider added write out for EcoRegion to write statement at line 397
    Added logic to read in ECOREG and ISTDORG in Keyword option 14: STDINFO
    Removed SELECT CASE non BC code from Option 14 STDINFO (~line879)
        Consider added write out for EcoRegion to write statement at line 879
    Added logic to option 39: MCFDLN to prevent keyword in instance that FIANVB has been requested, and provide user notification
    Added logic to option 40: BFFDLN to prevent keyword in instance that FIANVB has been requested, and provide user notification
    Added logic to option 41: MCDEFECT to prevent keyword in instance that FIANVB has been requested, and provide user notification
    Added logic to option 43: VOLUME, to prevent keyword in instance that FIANVB has been requested, and provide user notification
    Added logic to prevent calling of deprecated volume calculation method, requiring use of NVEL supported methods (MAY NEED TO PULL THIS BACK OUT FOR CANADIAN VARIANTS)
    Added sawlog min dbh, topD, and StumpHt read w/ conversion to metric
    QUESTION - SHOULD BC SUPPORT SAWLOG CUBIC VOLUME OUTPUTS?  MAY NEED TO REVISE VOLUME KEYWORD TO SUPPORT.
    PAUSED AT INDIVIDUAL SPECIES SPECS MODIFICATIONS

../canada/bc/keyopn.f
    NEEDS minor updates to conform to new keyword field lengths

../canada/bc/log.f
    No modification needed

../canada/bc/min.f
    No modification needed (Do these perform vol calc for BC?)

../canada/bc/morts.f
    No modification needed

../canada/bc/natcrz.f
    NEEDS minor change to increase volume equation definition string from 10 char to 11 char.

../canada/bc/r6crwd.f
    No modification needed

../canada/bc/regent.f
    No modification needed

../canada/bc/revise.f
    Update REV number?

../canada/bc/sitset.f
    NEEDS minor updates to standardize output and to be compatible with NSVB calls

../canada/bc/spctrn.f
    No modification needed

../canada/bc/vols.f
    NEEDS significant updating to bring up to new standard

../metric/vbase/disply.f
    NEEDS minor to moderate updating to bring up to standard

../metric/vbase/fvsstd.f
    NEEDS minor to moderate updating to bring up to standard

../metric/vbase/gheads.f
    Insignificant format change in newer version of base function

../metric/vbase/prtrls.f
    NEEDS moderate updating to bring up to standard

../metric/vbase/sumhed.f
    No modification needed

../metric/vbase/sumout.f
    NEEDS minor to moderate updating to bring up to standard
