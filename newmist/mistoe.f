      SUBROUTINE MISTOE
      IMPLICIT NONE
C----------
C  $Id$
C----------
*  **MISTOE--MS  Date of last revision:  03/10/05
*----------------------------------------------------------------------
*  Purpose:
*     This is the main dwarf mistletoe processing routine. It reads the
*  MISTMULT keyword, calculates changes in individual tree record
*  mistletoe ratings (intensification and spread) using the Hawksworth
*  6 point rating scheme, and keeps track of species/stand infection
*  levels. It also calls routines that determine mistletoe mortality,
*  and that automatically infect a stand with mistletoe.  It no longer
*  calls the routine that prints mistletoe output tables.
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     BCONST: Constant for probability of increase equation.
*     BDMR:   Coefficients for prob(+); depends on DMR.
*     BHTG:   Multiplier for probability(+) for height growth.
*     BTPA:   Multiplier for probability(+) for TPA.
*     DCONST: Constant for probability of decrease equation.
*     DDMR:   Multiplier for probability(-); based on DMR.
*     DEBUG:  Logical flag to turn debug on or off.
*     DHTG:   Multiplier for probability(-) for height growth.
*     DTPA:   Multiplier for probability(-) for TPA.
*     HTGR10: 10 year height growth (or whatever the cycle length).
*     I1:     Beginning tree record pointer for a species.
*     I2:     Ending tree record pointer for a species.
*     I3:     Tree record loop counter.
*     IACTK:  Passed back from OPGET (unused).
*     IDATE:  Passed back from OPGET (unused).
*     IDMR:   Current tree mistletoe rating.
*     IPLT:   Plot (point) loop counter.
*     ISPC:   Species loop counter.
*     ITEMP:  Temporary storage for sorting top 4 most infected species.
*     NEWSI:  Logical flag, true if new S+I model is to be used.
*     NP:     Passed back from OPGET (unused).
*     NTODO:  Number of actions to perform in a cycle.
*     PMINUS: Probability of a rating decreasing.
*     PPLUS:  Probability of a rating increasing.
*     PRM:    Array containing MISTMULT keyword values.
*     SMR:    Array to hold stand average mistletoe ratings by species.
*     TEMP:   Temporary storage for sorting top 4 most infected species.
*     TOTTPA: Total trees per acre for the species.
*     XNUM:   Random number.
*
* Common block variables and parameters:
*     DMRATE: From DMCOM;
*     DMTALL: From MISCOM; height of tallest infected tree by spec and plot.
*     FINT:   From PLOT;   current cycle length.
*     HTG:    From ARRAYS; predicted height growth this cycle.
*     ICYC:   From CONTRL; cycle index number.
*     IMC:    From ARRAYS; management code.
*     IMIST:  From MISCOM; individual tree record mistletoe rating.
*     IND1:   From ARRAYS; tree list access.
*     ISCT:   From CONTRL; species ordered list pointer.
*     ITRE:   From CONTRL; individual tree record plot (point) number.
*     ITRN:   From CONTRL; current number of tree records in stand.
*     IVARNT: From MISCOM; current variant number (see MISINT).
*     IY:     From CONTRL; inventory year.
*     JOSTND: From CONTRL; unit number for stand output.
*     MAXSP:  From PRGPRM; maximum number species.
*     MAXPLT: From PRGPRM; maximum number of sample plots (points).
*     MAXTRE: From PRGPRM; maximum number tree records.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).
*     PROB:   From ARRAYS; (P) trees per acre per tree record.
*     SPPDMI: From MISCOM; DMI by species and point.
*     SPPTPA: From MISCOM; Trees per acre by species and point.
*     USEMRT: From MISCOM; true if using this model's mortality calcs.
*     YNGMLT: From MISCOM; multiplier for probability of DMR decrease.
*     YPLMLT: From MISCOM; multiplier for probability of DMR increase.
*
* Revision History:
*     25-JUN-96; Julie C. Williams-Cipriani (FHTET-FC)
*        Added a check of the MISFIT array at the top of the species
*        processing loop; we don't want to spread and intensify DM
*        infections on non-valid species for this variant (non-host);
*        so if a non-host species does contain DM infection from damage
*        codes from the treelist, it will remain at the input DMR for
*        the entire FVS run without changing.
*     20-JUN-97; Lance R. David (FHTET)
*        Added spread of mistletoe to trees when a uninfected tree is
*        overstoried by an infected tree of the same species on the point.
*        A uninfected tree that is overstoried has a 20% chance (by draw
*        of random number) of being infected each cycle. Infection begins
*        with a DMR of 1.
*     14-OCT-97; Lance R. David (FHTET)
*        This update is an attempt to provide a more rapid intensification
*        in smaller trees when overstoried by infected trees on the point.
*        The DMR for a tree will be increased by 2 instead of 1 when the
*        current tree is overtoried its DMR is more than 2 ratings below
*        the DMI of the sample point. The probability of DMR increase is
*        still in effect for this situation. So, if a tree's DMR is to
*        increase, it may increase by 2 instead of 1. 
*     27-FEB-1998; Lance R. David (FHTET)
*        This update replaces (14-OCT-97).
*        Intensification and spread of mistletoe when infected 
*        overstory is present on the plot (sample point) is addressed
*        in this modification.
*        Overstoried is defined as the height of a tree being less
*        than 70% the height of the tallest infected tree of the 
*        same species on the same plot.
*        Intensification in overstoried trees:
*          The DMR on individual tree records will be increased by
*          0, 1, 2, or 3 based on its current DMR and a distribution
*          determined by Helen Maffei (R6 FHP) from currently available
*          data. A random number is drawn for determining placement
*          in the magnitude-of-DMR-increase distribution.
*        Spread when overstoried:
*          Trees have a 55% chance of becoming infected when
*          overstoried. Probability of infection is based on a random
*          number draw. Overstoried trees that are selected for
*          infection will begin with a DMR of 1 (69%), 2 (18%) or
*          3 (13%).
*        Additional note:
*          All affected tree species use the same distributions for
*          spread and intensification, when overstoried, at this
*          time. It is expected that species-specific distributions
*          will be developed when possible.
*     09-MAR-1998; Lance R. David (FHTET)
*          The intensification distribution for DMR 2 and 3 have been
*          set evenly between increase of 1, 2 or 3 ratings.
*     02-AUG-99; Lance R. David (FHTET-FC)
*        Added definition and data statements for MYACTS array
*        and replaced the activity code literal in the CALL OPFIND
*        statements.
*        This change was provided by Bob Havis (FMSC) to eliminate
*        LF95 FORTRAN compiler warnings.
*     23-DEC-2004  Lance R. David (FHTET)
*        Removed inclusion of MISPRM.F77 parameter file.
*     04-AUG-2005  Lance R. David (FHTET)
*        Corrected code dealing with spread to trees not influenced
*        by overstory. Initial infections (change off DMR=0 to DMR=1)
*        did not take place on trees without an overstory source.
*     10-MAR-2008  Lance R. David (FHTET)
*        Removed local declaration of DMFLAG, variable is now in common.
*        Also removed it as argument in call to MISINF.
***********************************************************************
C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'DMCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG,NEWSI
      REAL BDMR(7),BCONST,BTPA,BHTG,DDMR,DCONST,DTPA,DHTG
      REAL PRM(4),SMR(MAXSP)
      REAL    HTGR10,P,PMINUS,PPLUS,TOTTPA,XNUM
      INTEGER I,I1,I2,I3,IACTK,IDATE,IDMR,IPLT,ISPC,
     &        MYACTS(1),NP,NTODO

C.... Data statements.
      DATA MYACTS(1) / 2001 /

C.... Probability +/- equation coefficients.  Presently the same for
C.... all variants and all species.

      DATA (BDMR(I),I=1,7)
     &   /0.0, 2.45047, 2.30723, 1.8809, 2.11457, 1.43293, 0.0/

      DATA BCONST   /-1.67226/
      DATA BTPA     /-0.0012397/
      DATA BHTG     /-0.0747205/

      DATA DDMR     /0.0983757/
      DATA DCONST   /-5.59798/
      DATA DTPA     /-1.15053E-4/
      DATA DHTG     /0.013267/

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISTOE',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,100)ICYC
  100 FORMAT(' Begin MISTOE: Cycle = ',I5)

C.... If there are no trees in the stand skip out.

      IF(ITRN.EQ.0.0) GO TO 9000

C.... Initializations.

      DMFLAG=.FALSE.

C.... If the new S+I model is in effect and if the MISTOFF
C.... keyword is not active, then NEWSI is true.

      NEWSI = .FALSE.
      IF (NEWMOD .AND. MISFLG) NEWSI = .TRUE.

      DO 110 ISPC = 1, MAXSP
         DO 105 IPLT = 1, MAXPLT
            DMTALL(ISPC,IPLT) = 0.0
            SPPDMI(ISPC,IPLT) = 0.0
            SPPTPA(ISPC,IPLT) = 0.0
  105    CONTINUE
  110 CONTINUE

C.... Process MISTMULT keyword - get probability multipliers.
C.... Skip this if the new S+I model is in use.

      IF (.NOT.NEWSI) THEN
         NTODO=0
         CALL OPFIND(1,MYACTS(1),NTODO)
         IF(NTODO.NE.0) THEN
            DO 130 I=1,NTODO
               CALL OPGET(I,3,IDATE,IACTK,NP,PRM)
               CALL OPDONE(I,IY(ICYC))

C....       Check for particular species number.
C....       MISFIT insures that only species of this variant that
C....        are affected by mistletoe will get prob. multipliers -
C....        others will be set to 0.

               ISPC=IFIX(PRM(1))
               IF(ISPC.NE.0) THEN
                  YPLMLT(ISPC)=PRM(2)*MISFIT(ISPC)
                  YNGMLT(ISPC)=PRM(3)*MISFIT(ISPC)
               ELSE

C....          Otherwise defaults to all species.

                  DO 125 ISPC=1,MAXSP
                     YPLMLT(ISPC)=PRM(2)*MISFIT(ISPC)
                     YNGMLT(ISPC)=PRM(3)*MISFIT(ISPC)
  125             CONTINUE
               ENDIF
  130       CONTINUE
         ENDIF
      ENDIF

C.... Select method of calculation.
C....  New model calls DMTREG, then maps results back to IMIST.
C....  (ELSE) Interim model works directly.

      IF (NEWSI) THEN

         CALL DMTREG
         CALL DMMDMR(DMRATE,IMIST)

      ELSE

C.... Top of species loop.

        DO 300 ISPC=1,MAXSP
          I1=ISCT(ISPC,1)

C....     If there are no trees of this species, skip out.

          IF(I1.EQ.0) GO TO 300
          I2=ISCT(ISPC,2)

C....     If this species (in this variant) is not affected by mistletoe,
C....     skip out.

          IF(MISFIT(ISPC).EQ.0) GO TO 300

C....     Get total TPA for this species and check for any mistletoe.

          TOTTPA=0.0
          SMR(ISPC)=0.0
          DO 160 I3=I1,I2
            I=IND1(I3)
            P=PROB(I)
            TOTTPA=TOTTPA+P
            SMR(ISPC)=SMR(ISPC)+IMIST(I)*P
  160     CONTINUE

C....     If the stand is mistletoe free, skip the infection probability
C....     calculations.

          SMR(ISPC)=SMR(ISPC)/TOTTPA
          IF(SMR(ISPC).EQ.0.0) GO TO 250

C....     At least one species has mistletoe; set flag.

          DMFLAG=.TRUE.

C....     Loop through tree records of the current species.
C....     -- Save height of tallest infected tree on each point.
C....     -- Accumulate DMR*TPA and TPA for infected trees for calculation
C....        of DMI on each point.
C....        Note: The SPPDMI calculation does not seem to be needed per
C....              discussion with Tommy Gregg on 6/18/97 and may want to
C....              remove the code. But what I've done so far might not
C....              be the right or permanent solution for spreading
C....              infections to smaller trees so will leave for now.
C....              -Lance David

          DO 180 I3 = I1, I2
            I = IND1(I3)
            IPLT = ITRE(I)
            IF (IMIST(I) .GT. 0) THEN
              SPPDMI(ISPC,IPLT) = SPPDMI(ISPC,IPLT)+(IMIST(I)*PROB(I))
              SPPTPA(ISPC,IPLT) = SPPTPA(ISPC,IPLT)+PROB(I)
              IF (HT(I) .GT. DMTALL(ISPC,IPLT)) THEN
                DMTALL(ISPC,IPLT) = HT(I)
              ENDIF
            ENDIF
  180     CONTINUE

C....     Now loop through the points and divide the accumulated DMIs
C....     (DMRs of infected trees only) by the total number of infected
C....     trees.

          DO 185 IPLT = 1, MAXPLT
            IF (SPPDMI(ISPC,IPLT) .GT. 0.0 
     &        .AND. SPPTPA(ISPC,IPLT) .GT. 0.0) THEN
              SPPDMI(ISPC,IPLT) = SPPDMI(ISPC,IPLT)/SPPTPA(ISPC,IPLT)
            ENDIF
  185     CONTINUE

C....     Loop for each tree record of the current species.

          DO 240 I3=I1,I2
            I=IND1(I3)
            IDMR=IMIST(I)
            IPLT=ITRE(I)
            HTGR10=HTG(I)
            PPLUS=0.0
            PMINUS=0.0

C....       If mistletoe rating is already at 6 (highest) skip the
C....       calculation for an increase in the rating probability.

            IF (IDMR.GE.6) THEN
              IDMR=6
              GO TO 200
            ENDIF

C....       Probability of an increase in rating, over 10 years.
C....       HTGR10 is adjusted to give 1 decade's growth.

            PPLUS=BCONST+BDMR(IDMR+1)+BHTG*(HTGR10*10.0/FINT)
     &          +BTPA*TOTTPA

C....       Make sure trees which are not affected by DM (i.e. PPLUS=0)
C....       don't get thrown in here anyway.

            IF (PPLUS.NE.0.0) THEN
              PPLUS=(1.0/(1.0+EXP(-PPLUS)))*YPLMLT(ISPC)
            ENDIF

C....       Scale the 10-year probability to FINT cycle length.

            IF (PPLUS.GE.1.0) THEN
              PPLUS=1.0
            ELSE
              PPLUS=1.0-(1.0-PPLUS)**(FINT/10.0)
            ENDIF

C....       If mistletoe rating is already at 0 (lowest) skip the
C....       calculation for a decrease in the rating probability.

  200       CONTINUE
            IF(IDMR.NE.0) THEN

C....         An update to the DMR (IMIST) needs to be made.
C....
C....         Probability of a decrease in rating over 10 years.
C....         HTGR10 is adjusted to give 1 decade's growth.

              PMINUS=DCONST+DDMR*IDMR+DHTG*(HTGR10*10.0/FINT)
     &          +DTPA*TOTTPA

C....         Same as PPLUS - see above.

              IF(PMINUS.NE.0.0)
     &          PMINUS=(1.0/(1.0+EXP(-PMINUS)))*YNGMLT(ISPC)

C....         Scale the 10-year probability to FINT cycle length.

              IF (PMINUS.GE.1.0) THEN
                PMINUS=1.0
              ELSE
                PMINUS=1.0-(1.0-PMINUS)**(FINT/10.0)
              ENDIF

C....         Grab a uniform random number (between 0 and 1).

              CALL RANN(XNUM)

C....         Update mistletoe ratings - NOTE: PPLUS or PPMINUS = 0 should
C....         not affect IMIST because XNUM should always be 0 or greater.
C....
C....         27-FEB-1998
C....         Intensification in smaller trees when overstoried by
C....         infected trees of the same species on the point. The DMR
C....         for individual tree records will be increased by 0, 1, 2,
C....         or 3 based on its current DMR and a distribution.
C....         Each current DMR value is treated differently, once it has
C....         been selected to be increased, as to how many ratings the
C....         DMR will change. A seperate random number is drawn for
C....         determining placement in the magnitude-of-DMR-increase
C....         distribution.
C....
C....         start of original intensification code
C....         IF(IDMR.NE.6) THEN
C....            IF(PPLUS.GT.XNUM) IMIST(I)=IMIST(I)+1
C....         ENDIF
C....         IF(IDMR.NE.0) THEN
C....            IF(PMINUS.GT.XNUM) IMIST(I)=IMIST(I)-1
C....         ENDIF
C....         end of original intensification code

              IF (IDMR.NE.6) THEN
                IF (PPLUS.GT.XNUM) THEN

C....           Use of point DMI to influence intensification
C....           is not used. 27-FEB-98
C*              IF (SPPDMI(ISPC,IPLT) .GT. (IDMR + 1) .AND.
C*   &              (DMTALL(ISPC,IPLT) * 0.7) .GT. HT(I)) THEN
C*                IMIST(I) = IMIST(I) + 2
C*              ELSE
C*                IMIST(I) = IMIST(I) + 1
C*              ENDIF

C....           The tree has been selected for an increase in DMR.
C....           If it is overstoried by an infected tree, another
C....           random number is drawn to determine the magnitude
C....           of the increase (1, 2 or 3).

                  IF (DMTALL(ISPC,IPLT) * 0.7 .GT. HT(I)) THEN

                    CALL RANN(XNUM)
                    IF (IMIST(I) .EQ. 1) THEN
                      IF (XNUM .LT. 0.61) THEN
                        IMIST(I) = IMIST(I) + 1
                      ELSEIF (XNUM .LT. 0.83) THEN
                        IMIST(I) = IMIST(I) + 2
                      ELSE
                        IMIST(I) = IMIST(I) + 3
                      ENDIF
                    ELSEIF (IMIST(I) .EQ. 2) THEN
C.... 09-MAR-98 LRD        IF (XNUM .LT. 0.45) THEN
                      IF (XNUM .LT. 0.34) THEN
                        IMIST(I) = IMIST(I) + 1
C.... 09-MAR-98 LRD       ELSEIF (XNUM .LT. 0.73) THEN
                      ELSEIF (XNUM .LT. 0.67) THEN
                        IMIST(I) = IMIST(I) + 2
                      ELSE
                        IMIST(I) = IMIST(I) + 3
                      ENDIF
                    ELSEIF (IMIST(I) .EQ. 3) THEN
C.... 09-MAR-98 LRD   IF (XNUM .LT. 0.23) THEN
                      IF (XNUM .LT. 0.34) THEN
                        IMIST(I) = IMIST(I) + 1
C.... 09-MAR-98 LRD        ELSEIF (XNUM .LT. 0.79) THEN
                      ELSEIF (XNUM .LT. 0.67) THEN
                        IMIST(I) = IMIST(I) + 2
                      ELSE
                        IMIST(I) = IMIST(I) + 3
                      ENDIF
                    ELSEIF (IMIST(I) .EQ. 4) THEN
                      IF (XNUM .LT. 0.55) THEN
                        IMIST(I) = IMIST(I) + 1
                      ELSE
                        IMIST(I) = IMIST(I) + 2
                      ENDIF
                    ELSE
                      IMIST(I) = IMIST(I) + 1
                    ENDIF
                  ELSE
                    IMIST(I) = IMIST(I) + 1
                  ENDIF
                  IF (IMIST(I) .GT. 6) IMIST(I) = 6
                ENDIF
              ENDIF

C....         Check to see if decrease in DMR will occur.
              IF(IDMR.NE.0) THEN
                IF(PMINUS.GT.XNUM) IMIST(I) = IMIST(I) - 1
              ENDIF

            ELSE
C....          The tree is not infected (DMR=0), introduce infection.
C....          Trees have a 55% chance of being infected when overstoried.
C....          Probability of infection is based on a random number draw.
C....          A tree is considered overstoried when it is 70% or less the 
C....          height of the tallest infected tree of the same species on
C....          the plot. Overstoried tree's that are selected for infection
C....          will begin with a DMR of 1 (69%), 2 (18%) or 3 (13%).
C....          Trees not influenced by infected overstory, use the original
C....          even-age spread probability.

              CALL RANN(XNUM)

              IF ((DMTALL(ISPC,IPLT) * 0.7) .GT. HT(I)) THEN
C....           55% chance that uninfected tree will becom infected.
                IF (XNUM .LT. 0.55) THEN
                  CALL RANN(XNUM)

                  IF (XNUM .LT. 0.69) THEN
                    IMIST(I) = 1
                  ELSEIF (XNUM .LT. 0.87) THEN
                     IMIST(I) = 2
                  ELSE
                     IMIST(I) = 3
                  ENDIF
                ENDIF
              ELSE
                IF(PPLUS .GT. XNUM) IMIST(I) = 1
              ENDIF
            ENDIF

C....    Bottom of tree record loop.

  240     CONTINUE

C....    Set the management code to 3 for highly infected trees.

  250     CONTINUE
          DO 260 I3=I1,I2
            I=IND1(I3)
            IDMR=IMIST(I)
            IF(IDMR.GE.4) IMC(I)=3
  260     CONTINUE

C.... Bottom of species loop.

  300   CONTINUE

      ENDIF

C.... See if any forced mistletoe infection is desired and, if so,
C.... add new infections to the stand. Also update the stand and
C.... species infection totals if this was done.

      CALL MISINF

C.... Check DMFLAG; if still FALSE then no mistletoe in any species in
C.... this stand, this cycle so skip DM mortality routine.

      IF(DMFLAG) CALL MISMRT(USEMRT)

C.... Common return.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010)ICYC
 9010 FORMAT(' End MISTOE: Cycle = ',I5)

      RETURN
      END
