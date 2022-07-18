      SUBROUTINE FMSNGDK(VAR,KSP,D,DKTIME)
      IMPLICIT NONE
C----------
C FIRE-VBASE $Id$
C----------
C
C     SNAG DECAY PREDICTION
C
C   Purpose:
C     This routine calculates the number of years, since death, for a
C     snag to become soft, based on species, DBH, and plant association.
C
C     The base logic in this routine was extracted from its original
C     location in FMSNAG, FMSCRO, and SVSNAGE, in order to
C     structure/generalize the logic for use with both the
C     FFE snag pools, and the base FVS model snag records.
C
C   Called from: FMSNAG to compute decay time for a given FFE snag pool.
C                FMSCRO to compute decay time for a given FFE snag pool.
C                SVSNAGE to compute decay time for a given FVS snag record.
C
C   Local variable definitions:
C     D:       Diameter of current snag pool/record.
C     DKTIME:  Years, since death, for snag to become soft.
C     KSP:     Species number for current snag pool/record.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
      CHARACTER VAR*2
      INTEGER JADJ, JSML, JYRSOFT, KSP
      REAL    D, DKTIME, XMOD


C----------
C  Call SVHABT to get habitat-based multiplier for decay rate.
C  (NOTE: SVHABT currently returns a constant value of 1.0.
C         The call to SVHABT is preserved here for potential future use.
C         The XMOD multiplier is not applied to decay rates computed in
C         the FMR6SDCY routine, since it specifically addresses plant
C         association.)
C----------

      CALL SVHABT(XMOD)

C----------
C  Calculate years, since death, for snag to become soft.
C----------

      SELECT CASE (VAR)
        CASE('LS','ON')
          DKTIME = 0.65 * DECAYX(KSP) * D
          DKTIME = DKTIME * XMOD
        CASE('PN', 'WC', 'BM', 'EC', 'AK', 'OP')
          CALL FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
          DKTIME = JYRSOFT * DECAYX(KSP)
        CASE('SO')
          SELECT CASE (KODFOR)
          CASE (601,602,620,799)                     !OREGON
            CALL FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
            DKTIME = JYRSOFT * DECAYX(KSP)
          CASE DEFAULT                               !CALIFORNIA
            DKTIME = (1.24 * DECAYX(KSP) * D) + (13.82 * DECAYX(KSP))
            DKTIME = DKTIME * XMOD
          END SELECT
        CASE DEFAULT
          DKTIME = (1.24 * DECAYX(KSP) * D) + (13.82 * DECAYX(KSP))
          DKTIME = DKTIME * XMOD
      END SELECT
C
      RETURN
      END

