      REAL FUNCTION MISHGF(ITREE,ISPC)
***********************************************************************
C MISTOE $Id$
*----------------------------------------------------------------------
*  Purpose:
*     Determines the proportion of height growth lost periodically
*  due to dwarf mistletoe infection based on tree species and the
*  intensity of infection (individual tree record DMR) and adjusts
*  the normal height growth accordingly (called from DGDRIV once per
*  tree record). Also processes the MISTHMOD keyword.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ISPC:   (I) Current tree species.
*     ITREE:  (I) Current tree record number.
*     MISHGF: (O) Returns the 10 year proportion of potential height
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
*     PRMS:   Array containing MISTHMOD keyword values.
*
*  Common block variables and parameters:
*     HGPDMR: From MISCOM; height growth potentials based on species
*                and DMR (0-6).
*     FINT:   From PLOT; current cycle length.
*     ICYC:   From CONTRL; cycle index number.
*     IMIST:  From MISCOM; individual tree mistletoe rating.
*     IY:     From CONTRL; inventory year.
*     JOSTND: From CONTRL; unit number for stand output.
*     MAXSP:  From PRGPRM; maximum number species.
*     MISCYC: From MISCOM; logical flag to process MISTHMOD keyword
*                only once per cycle.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).

*
*  Revision History :
*     30-FEB-2010 Lance R. David (FMSC)
*        Subroutine created.
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
      DATA MYACTS(1) / 2004 /

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISHGF',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,100)ICYC,MISCYC(2,ICYC)
  100 FORMAT(' Begin MISHGF: Cycle = ',I5,' MISCYC = ',L)

C.... Initializations.

      MISHGF=1.0

C.... Check MISCYC; this flag set to TRUE if the MISTHMOD keyword has
C.... been processed this cycle (i.e. this will happen only once per
C.... cycle even though this function is called for every tree record).

      IF(.NOT.MISCYC(2,ICYC)) THEN
         MISCYC(2,ICYC)=.TRUE.

C....    Process MISTHMOD keyword (activity code 2004) if scheduled.

         NTODO=0
         CALL OPFIND(1,MYACTS(1),NTODO)
C
         IF(NTODO.GT.0) THEN
            DO 200 I=1,NTODO
               CALL OPGET(I,7,IDATE,IACTK,NP,PRMS)
               CALL OPDONE(I,IY(ICYC))

C....          Check for an individual species number.
C....          Get mistletoe height growth proportions from keyword;
C....          A value of 1.0 causes no height growth potential impact
C....          and values approaching 0.0 will increasingly affect DM
C....          height growth impact (height growth reduction).
C....          MISFIT insures that only species of this variant that
C....          are affected by mistletoe will get height growth
C....          modifiers, others will be set to 1.0 which is no affect.

               IKSPC=IFIX(PRMS(1))
               IF(IKSPC.NE.0) THEN
                  DO 120 IDMR=2,7
                     IF(MISFIT(IKSPC).EQ.1) THEN
                        HGPDMR(IKSPC,IDMR)=PRMS(IDMR)
                     ELSE
                        HGPDMR(IKSPC,IDMR)=1.0
                     ENDIF
  120             CONTINUE
               ELSE

C....             Otherwise new growth modifiers applied to all
C....             affected species.

                  DO 150 IKSPC=1,MAXSP
                     DO 140 IDMR=2,7
                        IF(MISFIT(IKSPC).EQ.1) THEN
                           HGPDMR(IKSPC,IDMR)=PRMS(IDMR)
                        ELSE
                           HGPDMR(IKSPC,IDMR)=1.0
                        ENDIF
  140                CONTINUE
  150             CONTINUE
               ENDIF
  200       CONTINUE
         ENDIF
      ENDIF

C.... Set mistletoe height growth proportion including effects
C.... of user supplied proportions based on species and DMR.

      IDMR=IMIST(ITREE)
      MISHGF=HGPDMR(ISPC,IDMR+1)

C.... Force normal growth for uninfected trees.

      IF(IDMR.EQ.0) MISHGF=1.0

C.... Force upper and lower bounds on height growth.

      IF(MISHGF.GT.1.0) MISHGF=1.0
      IF(MISHGF.LT.0.0) MISHGF=0.0

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,9010) ICYC, ITREE, IMIST(ITREE), ISPC,
     &   MISHGF
 9010 FORMAT(' End MISHGF: Cycle, Tree record, IMIST, SPC, MISHGF = ',
     &   I5,I7,I3,I5,F6.2)

      RETURN
      END
