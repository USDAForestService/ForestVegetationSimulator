CODE SEGMENT MISCOM
C
C----------
C COMMON $Id$
C----------
C  Purpose:
C     Common block variables related to the dwarf mistletoe routines.
C----------
C
C  Revision History :
C     03/01/95 - Lance R. David (MAG)
C                Declaration of real variable array DMGMLT (size MAXSP)
C                removed from common area MISTL due to change of
C                MISTGMOD keyword.
C
C     04/10/97 - Julie C. Wil-Cip (FHTET)
C                Added array DMTALL to common area MISTL due
C                to modification of MISTOE subroutine to speed up the
C                spread of DMR from large to small trees on a plot.
C
C     06/15/97 - Lance R. David (FHTET)
C                Added arrays SPPDMI and SPPTPA to common area MISTL as
C                continued the modification Julie began to speed spread
C                and intensification on small trees when infected overstory
C                exists on the plot (aka point).
C
C     03/07/08 - Lance R. David (FHTET)
C                Added variables that were local to misprt subroutine for 
C                output control that need to be maintained for each stand in 
C                PPE processing mode.
C                Variables Added are: DMFLAG, PRTTBL, LSORT4, MISTBL and ISVSP4
C
C     04/21/09 - Lance R. David (FMSC)
C                Added variables IMOUT_ (thanks to Don Robinson)
C
C     09/30/10 - Lance R. David (FMSC)
C                Added HGPDMR array for DM height growth impact proportions.
C
C     04/01/11 - Lance R. David (FMSC)
C                Added dimension to MISCYC array (1,x) used by MISDGF function
C                and (2,x) used by MISHGF (diameter and height growth impact).
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL DMFLAG,FSTMIS,FSTTBL,LSORT4,MISFLG,PRTMIS,PRTTBL,USEMRT
C
      LOGICAL MISCYC(2,MAXCYC),MISTBL(MAXSP)
C
      CHARACTER*2  CSPARR(MAXSP)
C
      INTEGER IMOUT1,IMOUT2,IMOUT3,IMOUT4,JRAN
C
      INTEGER DMPLT(MAXPLT),IDMSOUT(4),IMIST(MAXTRE),ISVSP4(4),
     &        MISFIT(MAXSP)
C
      REAL DMRMIN
C
      REAL DGPDMR(MAXSP,7),DMMMLT(MAXSP),DMMTPA(MAXTRE),
     &     DMTALL(MAXSP,MAXPLT),HGPDMR(MAXSP,7),PMCSP(MAXSP,3),
     &     PRFMST(MAXSP,6),SPPDMI(MAXSP,MAXPLT),SPPTPA(MAXSP,MAXPLT),
     &     YNGMLT(MAXSP),YPLMLT(MAXSP)
C
      COMMON /MISTL/ DGPDMR,DMFLAG,DMMMLT,DMMTPA,DMPLT,DMRMIN,DMTALL,
     &               FSTMIS,FSTTBL,HGPDMR,IMIST,ISVSP4,JRAN,LSORT4,
     &               MISCYC,MISFIT,MISFLG,MISTBL,PMCSP,PRFMST,PRTMIS,
     &               PRTTBL,SPPDMI,SPPTPA,USEMRT,YNGMLT,YPLMLT
C
      COMMON /MISTL2/ CSPARR
C
      COMMON /MISTL3/ IDMSOUT,IMOUT1,IMOUT2,IMOUT3,IMOUT4
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C     CSPARR: Array to hold 2 character representation of top 4
C                infected species; set in MISINT.
C     DGPDMR: Array containing diameter growth potential proportions
C                due to DM infection based on DMR; set in MISINT; set
C                from MISTGMOD keyword in MISDGF; saved for subsequent
C                cycles.
C     DMFLAG: Logical flag, true if mistletoe is present in the stand.
C     DMMMLT: Array containing DM mortality multipliers; set from
C                keyword in MISMRT, saved for subsequent cycles.
C     DMMTPA: Array containing TPA killed by mistletoe; set in MISMRT
C                for most variants, set in MISDIF for SW Mixed Conifer
C                and SW Ponderosa Pine variants.
C      DMPLT:
C     DMRMIN: Minimum tree DBH for DMR/DMI statistics (this size or
C                larger to be counted); set from keyword in MISIN.
C     DMTALL: Array used to keep track of the tallest DMR-infected tree
C                by species, by plot; contains that tree's height, set
C                in MISTOE each cycle.
C     FSTMIS: Logical flag for printing mistletoe summary output table
C                headings; set first time through subroutine MISPRT.
C     FSTTBL: Logical flag for printing mistletoe detail table headings;
C                set first time printed in subroutine MISPRT.
C     HGPDMR: Array containing height growth potential proportions
C                due to DM infection based on DMR; set in MISINT; set
C                from MISTHMOD keyword in MISHGF. Set values remain in
C                effect until changed by a later scheduled MISTHMOD.
C   IDMSOUT:
C     IMIST:  Array containing individual tree record dwarf mistletoe
C                (Hawksworth) ratings.
C     IMOUT1: Mistletoe output unit 1 (Top 4 species summary).
C     IMOUT2: Mistletoe output unit 2 (stand summary).
C     IMOUT3: Mistletoe output unit 3 (2" DBH class summary).
C     IMOUT4: Mistletoe output unit 4 (species/DBH class detail).
C     ISVSP4: Array of top 4 most DM infected species by number.
C     JRAN:   Status value of mistletoe random number generator.
C     LSORT4: Logical flag to set species by infection.
C     MISCYC: Logical array used for reading MISTGMOD keyword only once
C                per cycle (since MISDGF is called once per tree).
C     MISFIT: Array used to keep track of which species in the current
C                variant are mistletoe susceptible; variant specific;
C                (1=affected, 0=not affected by DM); set in MISINT.
C     MISFLG: Logical flag to turn mistletoe processing on/off; set from
C                keyword in MISIN, valid throughout a run.
C     MISTBL: Logical array - what species desired in detailed output.
C     PMCSP:  Array containing percent mortality equation coefficients
C                by species; set in MISINT.
C     PRFMST: Array containing species mistletoe removal preference
C                priorities; set from keyword in MISCPF.
C     PRTMIS: Logical flag to turn DM output table printing on/off; set
C                from keyword in MISIN.
C     PRTTBL: Logical flag to print DM detailed output tables.
C     SPPDMI: Array used to calculate DMI (ave DMR of infected trees only)
C                by species, by plot; set in MISTOE each cycle.
C     SPPTPA: Array used to accumulate trees per acre of infected trees
C                used in the calculation of point (plot) DMI.
C                by species, by plot; set in MISTOE each cycle.
C     USEMRT: Logical flag, true if using this model's mortality
C                calculations; set in MISIN0 or MISINT.
C     YNGMLT: Array containing DM spread probability(-) multipliers; set
C                from keyword in MISTOE, saved for subsequent cycles.
C     YPLMLT: Array containing DM spread probability(+) multipliers; set
C                from keyword in MISTOE, saved for subsequent cycles.
C
C-----END SEGMENT
