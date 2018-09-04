      SUBROUTINE MISINF
***********************************************************************
C MISTOE $Id$
*----------------------------------------------------------------------
*  Purpose:
*     This routine adds dwarf mistletoe infections to a proportion of
*  trees (by entire tree record) based on parameters input on the
*  MISTPINF keyword (species, proportion of trees, infection level and
*  randomness of dispersement). Trees that are already infected or
*  trees that are not susceptible to dwarf mistletoe will not be
*  changed by this process.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     None.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on or off.
*     HTINDX: Temporary storage array for sorting by tree height.
*     I,J:    Loop counters.
*     IACTK:  Passed back from OPGET (unused).
*     IDATE:  Passed back from OPGET (unused).
*     IDMPTR: A pointer that cycles thru DM ratings from 1 to NEWLEV.
*     IDMR:   Same as IMIST.
*     INDX:   Current tree index number from TRINDX.
*     ISPC:   Species loop counter.
*     ITREE:  Tree record loop counter.
*     NEWCNT: Keeps count of TPA with new DM infections by species.
*     NEWFLG: Log. flag implies at least 1 species w/new DM infection.
*     NEWINF: Total TPA to have new forced DM infection by species.
*     NEWLEV: Level of infection to introduce (DMR 1-6) by species.
*     NEWPRP: Proportion of species to have new forced infection.
*     NEWTYP: Method of infection to introduce (0=random, 1=hi-lo,
*                2=lo-hi) by species.
*     NP:     Passed back from OPGET (unused).
*     NTODO:  Number of actions to perform in a cycle.
*     NUTYPE: Method of infection used in this cycle (from NEWTYP).
*     P:      Same as PROB.
*     PRM:    Array containing MISPINF keyword values.
*     SPTPAT: Array containing total # TPA by species.
*     TEMPTR: Temporary array used to reverse order of TRINDX.
*     TRINDX: Temporary index used to sort or randomize trees.
*
*  Common block variables and parameters:
*     HT:     From ARRAYS; individual tree record current height.
*     ICYC:   From CONTRL; cycle index number.
*     IMIST:  From MISCOM; individual tree record mistletoe rating.
*     IND1:   From ARRAYS; tree list access.
*     ISP:    From ARRAYS; current tree species.
*     ITRN:   From CONTRL; actual number of trees in stand.
*     IY:     From CONTRL; inventory year.
*     JOSTND: From CONTRL; unit number of stand output.
*     MAXSP:  From PRGPRM; maximum number species.
*     MAXTRE: From PRGPRM; maximum number tree records.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).
*     PROB:   From ARRAYS; trees per acre per tree record.
*
* Revision History:
*    02-AUG-99; Lance R. David (FHTET-FC)
*       Added definition and data statements for MYACTS array
*       and replaced the activity code literal in the CALL OPFIND
*       statements.
*       This change was provided by Bob Havis (FMSC) to eliminate
*       LF95 FORTRAN compiler warnings.
*    10-MAR-2008  Lance R. David (FHTET)
*       DMFLAG removed from argument list because it is now in common.
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG,NEWFLG
      INTEGER TRINDX(MAXTRE),TEMPTR(MAXTRE),NEWLEV(MAXSP),NEWTYP(MAXSP),
     &   IDMPTR(MAXSP)
      REAL HTINDX(MAXTRE),NEWPRP(MAXSP),NEWINF(MAXSP),NEWCNT(MAXSP),P,
     &   PRM(4),SPTPAT(MAXSP)
      INTEGER MYACTS(1)
      INTEGER I,I1,I2,I3,IACTK,IDATE,IDMR,INDX,ISPC,J,NP,NTODO,ITREE,
     &   NUTYPE

C.... Data statements.
      DATA MYACTS(1) / 2006 /

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINF',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,100) ICYC
  100 FORMAT(' Begin MISINF: Cycle = ',I5)

C.... Initializations.

      NEWFLG=.FALSE.
      NUTYPE=-1

C.... These are initializations for the MISPINF keyword values. Since
C.... MISTPINF is a single cycle type keyword (doesn't carry over to
C.... subsequent cycles), these values are not in a common block and
C.... are not initialized in MISINT.

      DO 110 I=1,MAXSP
         NEWPRP(I)=0.0
         NEWINF(I)=0.0
         NEWCNT(I)=0.0
         NEWLEV(I)=0
         NEWTYP(I)=0
         SPTPAT(I)=0.0
  110 CONTINUE

C.... Process MISTPINF keyword - get species to infect, proportion of
C.... trees of that species to infect, level of infection, and method
C.... of infection. NOTE: there can be multiple MISTPINF keywords with
C.... different species and proportions for a given cycle BUT all must
C.... use the SAME level and method of infection in a single cycle and,
C.... if multiple keywords, will default to the first one encountered.
C.... This is activity code 2006.

      NTODO=0
      CALL OPFIND(1,MYACTS(1),NTODO)
      IF(NTODO.NE.0) THEN
         DO 130 I=1,NTODO
            CALL OPGET(I,4,IDATE,IACTK,NP,PRM)
            CALL OPDONE(I,IY(ICYC))

C....       Check for particular species number.
C....       MISFIT insures that only species of this variant that
C....        are affected by mistletoe will get MISTPINF values -
C....        others will be set to 0.

            ISPC=IFIX(PRM(1))
            IF(ISPC.NE.0) THEN
               NEWPRP(ISPC)=PRM(2)*MISFIT(ISPC)
               NEWLEV(ISPC)=INT(PRM(3))*MISFIT(ISPC)
               NEWTYP(ISPC)=INT(PRM(4))*MISFIT(ISPC)
            ELSE

C....          Otherwise defaults to all species.

               DO 120 ISPC=1,MAXSP
                  NEWPRP(ISPC)=PRM(2)*MISFIT(ISPC)
                  NEWLEV(ISPC)=INT(PRM(3))*MISFIT(ISPC)
                  NEWTYP(ISPC)=INT(PRM(4))*MISFIT(ISPC)
  120          CONTINUE
            ENDIF
  130    CONTINUE
      ENDIF

      DO 140 ISPC=1,MAXSP

C....    Calculate species mistletoe infection totals.

         I1=ISCT(ISPC,1)
         IF(I1.GT.0) THEN
            I2=ISCT(ISPC,2)
            DO 135 I3=I1,I2
               SPTPAT(ISPC)=SPTPAT(ISPC)+PROB(IND1(I3))
  135       CONTINUE
         ENDIF

C....    The proportion of trees to infect cannot be less than 0.

         IF(NEWPRP(ISPC).LT.0.0) NEWPRP(ISPC)=0.0

C....    Check to see if any species will have new infections introduced
C....    and, if so, translate proportions into TPA by species. Set
C....    NEWFLG for this routine and DMFLAG to be returned to MISTOE.

         IF(NEWPRP(ISPC).GT.0.0) THEN
            NEWFLG=.TRUE.
            DMFLAG=.TRUE.

C....       The proportion of trees to infect cannot be greater than 1.

            IF(NEWPRP(ISPC).GT.1.0) NEWPRP(ISPC)=1.0
            NEWINF(ISPC)=SPTPAT(ISPC)*NEWPRP(ISPC)

C....       Check that the level of infection is between 1 and 6 where:
C....          1 => all trees infected with DMR 1 (default)
C....          2 => all trees infected with DMR 1 or 2
C....          3 => all trees infected with DMR 1, 2, or 3
C....          4 => all trees infected with DMR 1, 2, 3, or 4
C....          5 => all trees infected with DMR 1, 2, 3, 4, or 5
C....          6 => all trees infected with DMR 1, 2, 3, 4, 5, or 6

            IF(NEWLEV(ISPC).LT.1.OR.NEWLEV(ISPC).GT.6) NEWLEV(ISPC)=1

C....       Check that the method of infection is between 0 and 2 where:
C....          0 => random (default)
C....          1 => hi to lo (tallest to shortest tree)
C....          2 => lo to hi (shortest to tallest tree)
C....       NUTYPE will be set to only the first NEWTYP encountered if
C....       there is more than one species to be infected this cycle
C....       (can have only one method of infection per cycle because it
C....       involves sorting the entire treelist - not by species).

            IF(NEWTYP(ISPC).LT.0.OR.NEWTYP(ISPC).GT.2) NEWTYP(ISPC)=0
            IF(NUTYPE.EQ.-1) NUTYPE=NEWTYP(ISPC)
         ENDIF
  140 CONTINUE

C.... If NEWFLG is FALSE then no infections to be introduced for any
C.... species so skip out.

      IF(.NOT.NEWFLG) GO TO 9000

C.... Loop through entire tree list to create temporary tree and height
C.... index to be sorted or randomized (obviously can't sort original
C.... tree index).

      DO 150 ITREE=1,ITRN
         TRINDX(ITREE)=IND1(ITREE)
         HTINDX(ITREE)=HT(ITREE)
  150 CONTINUE

      IF(NUTYPE.EQ.1.OR.NUTYPE.EQ.2) THEN

C....    Sort index by tree height from tallest to shortest.

         CALL RDPSRT(ITRN,HTINDX,TRINDX,.TRUE.)

         IF(NUTYPE.EQ.2) THEN

C....       If shortest to tallest is desired then reverse TRINDX order.

            J=1
            DO 160 I=ITRN,1,-1
               TEMPTR(J)=TRINDX(I)
               J=J+1
  160       CONTINUE

C....       Copy reverse order temporary array back into TRINDX.

            DO 165 I=1,ITRN
               TRINDX(I)=TEMPTR(I)
  165       CONTINUE
         ENDIF
      ELSE

C....    This routine will return the first ITRN slots of TRINDX in
C....    random order.

         CALL MISRAN(TRINDX,ITRN)
      ENDIF

C.... Initialize a pointer to cycle through DM ratings from 1 to NEWLEV
C.... for each species.

      DO 170 I=1,MAXSP
         IDMPTR(I)=1
  170 CONTINUE

C.... Loop through trees based on sorted or randomized index (TRINDX).
C.... Update mistletoe rating if three conditions are satisfied:
C....    1. that species was targeted for infection
C....    2. current DMR is equal to 0
C....    3. total TPA newly infected has not yet exceeded desired
C....       proportion of TPA to be infected.

      DO 200 I=1,ITRN
         INDX=TRINDX(I)
         ISPC=ISP(INDX)
         P=PROB(INDX)
         IDMR=IMIST(INDX)
         IF((IDMR.EQ.0).AND.(NEWPRP(ISPC).GT.0.0).AND.
     &      ((NEWCNT(ISPC)+P).LE.NEWINF(ISPC))) THEN
            IMIST(INDX)=IDMPTR(ISPC)
            NEWCNT(ISPC)=NEWCNT(ISPC)+P

C....       Increment IDMPTR or set it back to 1 if we've passed NEWLEV.

            IDMPTR(ISPC)=IDMPTR(ISPC)+1
            IF(IDMPTR(ISPC).GT.NEWLEV(ISPC)) IDMPTR(ISPC)=1
         ENDIF
  200 CONTINUE

C.... Common return.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010) ICYC
 9010 FORMAT(' End MISINF: Cycle = ',I5)

      RETURN
      END
