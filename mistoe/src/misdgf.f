      REAL FUNCTION MISDGF(ITREE,ISPC)
***********************************************************************
*  **MISDGF--MS  Date of last revision:  12/20/03
*----------------------------------------------------------------------
*  Purpose:
*     Calculates the percentage of diameter growth lost periodically
*  due to dwarf mistletoe infection based on tree species and the
*  intensity of infection (individual tree record DMR) and adjusts
*  the normal diameter growth accordingly (called from DGDRIV once per
*  tree record). Also processes the MISTGMOD keyword.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ISPC:   (I) Current tree species.
*     ITREE:  (I) Current tree record number.
*     MISDGF: (O) Returns the 10 year proportion of potential diameter
*                growth due to mistletoe infection.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on or off.
*     IACTK:  Passed back from OPGET (unused).
*     IDATE:  Passed back from OPGET (unused).
*     IDMR:   Dwarf mistletoe rating for current tree record (0-6).
*     IKSPC:  Species of current tree record (0-11) used for keyword.
*     NP:     Passed back from OPGET (unused).
*     NTODO:  Number of actions to perform in a cycle.
*     PRMS:   Array containing MISTGMOD keyword values.
*
*  Common block variables and parameters:
*     DGPDMR: From MISCOM; diameter growth potentials based on species
*                and DMR (0-6).
*     FINT:   From PLOT; current cycle length.
*     ICYC:   From CONTRL; cycle index number.
*     IMIST:  From MISCOM; individual tree mistletoe rating.
*     IY:     From CONTRL; inventory year.
*     JOSTND: From CONTRL; unit number for stand output.
*     MAXSP:  From PRGPRM; maximum number species.
*     MISCYC: From MISCOM; logical flag to process MISTGMOD keyword
*                only once per cycle.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).

*
*  Revision History :
*     28-FEB-95; Lance R. David (MAG)
*        Changed MISTGMOD keyword to use supplemental record to
*        supply growth modification proportions for one specie's
*        DMRs 1-6. Field 2 of keyword record specifies species
*        being modified.
*        This replaces the use of a species specific growth
*        modifier multiplier (variable DMGMLT) that was applied
*        to all DMRs.
*     02-AUG-99; Lance R. David (FHTET-FC)
*        Added definition and data statements for MYACTS array
*        and replaced the activity code literal in the CALL OPFIND
*        statements.
*        This change was provided by Bob Havis (FMSC) to eliminate
*        LF95 FORTRAN compiler warnings.
*     20-DEC-03
*        Removed parameter file (misprm.f77)
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      REAL    PRMS(7)
      INTEGER MYACTS(1)
      INTEGER I,IACTK,IDATE,IDMR,IKSPC,ISPC,ITREE,NP,NTODO
C
C     DATA STATEMENTS
C
      DATA MYACTS(1) / 2005 /

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISDGF',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,100)ICYC
  100 FORMAT(' Begin MISDGF: Cycle = ',I5)

C.... Initializations.

      MISDGF=1.0

C.... Check MISCYC; this flag set to TRUE if the MISTGMOD keyword has
C.... been processed this cycle (i.e. this will happen only once per
C.... cycle even though this function is called for every tree record).

      IF(.NOT.MISCYC(1,ICYC)) THEN
         MISCYC(1,ICYC)=.TRUE.

C....    Process MISTGMOD keyword (activity code 2005) if scheduled.

         NTODO=0
         CALL OPFIND(1,MYACTS(1),NTODO)
C
         IF(NTODO.GT.0) THEN
            DO 200 I=1,NTODO
               CALL OPGET(I,7,IDATE,IACTK,NP,PRMS)
               CALL OPDONE(I,IY(ICYC))

C....          Check for a particular species number.
C....          Get mistletoe diameter growth proportions from keyword;
C....          1.0 indicates no growth potential impact (lessen DM
C....          effects on diameter); a value near 0.0 will decrease
C....          growth potential (heighten DM effects).
C....          MISFIT insures that only species of this variant that
C....          are affected by mistletoe will get diameter growth
C....          modifiers, others will be set to 1.0 which is no effect.

               IKSPC=IFIX(PRMS(1))
               IF(IKSPC.NE.0) THEN
                  DO 120 IDMR=2,7
                     IF(MISFIT(IKSPC).EQ.1) THEN
                        DGPDMR(IKSPC,IDMR)=PRMS(IDMR)
                     ELSE
                        DGPDMR(IKSPC,IDMR)=1.0
                     ENDIF
  120             CONTINUE
               ELSE

C....             Otherwise new growth modifiers applied to all
C....             affected species.

                  DO 150 IKSPC=1,MAXSP
                     DO 140 IDMR=2,7
                        IF(MISFIT(IKSPC).EQ.1) THEN
                           DGPDMR(IKSPC,IDMR)=PRMS(IDMR)
                        ELSE
                           DGPDMR(IKSPC,IDMR)=1.0
                        ENDIF
  140                CONTINUE
  150             CONTINUE
               ENDIF
  200       CONTINUE
         ENDIF
      ENDIF

C.... Calculate mistletoe diameter growth proportion including effects
C.... of user supplied proportions based on species and DMR.

      IDMR=IMIST(ITREE)
      MISDGF=DGPDMR(ISPC,IDMR+1)

C.... Force normal growth for uninfected trees.

      IF(IDMR.EQ.0) MISDGF=1.0

C.... Force upper and lower bounds on diameter growth.

      IF(MISDGF.GT.1.0) MISDGF=1.0
      IF(MISDGF.LT.0.0) MISDGF=0.0

C.... Common return.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010) ICYC, ITREE, IMIST(ITREE), ISPC,
     &   MISDGF
 9010 FORMAT(' End MISDGF: Cycle, Tree record, IMIST, SPC, MISDGF = ',
     &   I5,I7,I3,I5,F6.2)

      RETURN
      END
