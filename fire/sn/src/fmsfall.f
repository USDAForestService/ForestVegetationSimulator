      Subroutine FMSFALL(IYR,KSP,D,ORIGDEN,DENTTL,ISWTCH,
     &                   RSOFT,RSMAL,DFALLN)
      IMPLICIT NONE
C----------
C  **FMSFALL--FIRE-SN  DATE OF LAST REVISION: 11/30/09
C----------
C
C     SNAG FALL PREDICTION
C
C   Purpose:
C     This routine calculates the base fall rates for snags, based
C     on species, DBH (large vs small), and whether we're down to the
C     last 5% of the snag's original stem/ac representation.
C
C     The logic in this routine was extracted from its original
C     location in FMSNAG, in order to generalize the logic for use
C     with both the FFE snag pools, and the base FVS model snag records.
C
C   Called from: FMSNAG to compute fall rate for a given FFE snag pool.
C                SVSNAGE to compute fall rate for a given FVS snag record.
C
C   Local variable definitions:
C     BASE:    Base rate-of-fall for snags of this size (I.E., rate for
C              snags with FALLX=1)
C     D:       Diameter of current snag pool/record.
C     DENTTL:  If called from FMSNAG:
C                 Sum of DENIH + DENIS for current snag pool (sum of
C                 hard + soft snags/ac).
C              If called from SVSNAGE:
C                 The total number of SVS snag records still standing
C                 that were generated from the same source tree, in
C                 the same year, as the current snag record.
C     DFALLN:  Target density of snags to fall under normal conditions
C              (where hard and soft snags fall at the same rate).
C     DZERO:   Density level (#/acre), at which snag is considered
C              equal to ZERO.
C     FALLM2:  Rate-of-fall for last 5% of lrg snags in current record
C     ISWTCH:  =1 if FMSNAG called this subroutine.
C              =2 if SVSNAGE called this subroutine.
C     KSP:     Species number for current snag pool/record.
C     ORIGDEN: If called from FMSNAG:
C                 The density (stems per acre) of snags at the time of
C                 death, for the current snag pool.
C              If called from SVSNAGE:
C                 The total number of SVS snag records that were
C                 generated from the same source tree, in the same
C                 year, as the current snag record.
C     RSMAL:   Rate of snag fall implied by PBSMAL and PBTIME
C     RSOFT:   Rate of snag fall implied by PBSOFT and PBTIME
C
C   Common variable definitions:
C     ALLDWN:  Time by which the last 5% of lrg snags in each group
C              have all fallen.
C     FALLX:   Rate-of-FALL correction factors for each species.
C              Adjusts fall rates for each species, relative to rate
C              predicted by base equation for a single base spp (PP in
C              NI variant).
C              Internal FALLX values can be overridden by the user, via
C              the SNAGFALL keyword.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
COMMONS
C
      INTEGER ISWTCH, IYR, KSP
      REAL    BASE, D, DENTTL, DFALLN, DZERO, FALLM2, MODRATE, ORIGDEN,
     &        RSOFT, RSMAL, X
C
C
C----------
C  In the first year after a fire, some work is required to determine
C  what fall rates to use in the coming years.  First, calculate
C  RSOFT and RSMAL.  These rates are the constant proportion
C  of snags that must fall each year in order for a total proportion
C  PBSOFT (or PBSMAL) of snags to have fallen after PBTIME (e.g.,
C  28% of remaining snags must fall every year in order for 90% of
C  the initial number to have fallen in 7 years:  (1-0.28)**7 = 0.1).
C  This could be done outside the snag loop except when either PBSOFT
C  of PBSMAL equals 1, which may often be the case.  So it's done here.
C----------
      IF (DENTTL .LE. 0) RETURN
      IF ((IYR - BURNYR) .EQ. 1) THEN
        DZERO = NZERO / 50.0
C
        RSOFT = 0.0
        IF (PBSOFT .GT. 0.0) THEN
          IF (PBSOFT .LT. 1.0) THEN
            RSOFT = 1 - EXP( LOG(1-PBSOFT) / PBTIME )
          ELSE
            RSOFT = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
          ENDIF
        ENDIF
C
        RSMAL = 0.0
        IF (PBSMAL .GT. 0.0) THEN
          IF (PBSMAL .LT. 1.0) THEN
            RSMAL = 1 - EXP( LOG(1-PBSMAL) / PBTIME )
          ELSE
            RSMAL = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
          ENDIF
        ENDIF
      ENDIF

C----------
C  Calculate the density of snags in this record that would fall under
C  normal conditions. This depends on species, DBH and whether 5% are
C  left.
C----------

      BASE = -0.001679 * D + 0.064311
      IF (BASE .LT. 0.01) BASE = 0.01

C----------
C  In Ozarks, fall down breakpt dbh is 12 inches.  Also for redcedar,
C  last 5% last long, even for small trees.
C----------

      MODRATE = BASE * FALLX(KSP)
      IF (MODRATE .GT. 1) MODRATE = 1
      IF ((D .LT. 12.0) .AND. (KSP .NE. 2)) THEN
        DFALLN = MODRATE * ORIGDEN
      ELSE

C----------
C  Near 5%, fall at least as many lrg snags as should fall at
C  final rate-of-fall, but be sure not to fall more than this
C  many below 5% all at once. This requires calculating FALLM2.
C  First, find the time X at which 5% of snags will be left
C  (proportion standing = -BASE*FALLX*time + 1).
C----------

        X = (0.05 - 1) / (-1*MODRATE)

C----------
C  Then find the slope of the line that passes from
C  5% at this time to 0% at ALLDWN (adjusted for the different
C  FALLX values). If ALLDWN <= time at which 5% left, assign
C  a slope of -2 (removes last 5% of snags immediately). The
C  negative of this slope is the rate FALLM2.
C----------

        IF (ALLDWN(KSP) .LE. X) THEN
          FALLM2 = 2
        ELSE
          FALLM2 = 0.05 / (ALLDWN(KSP) - X)
        ENDIF

C----------
C  Now proceed to calculate how many snags should normally fall.
C----------

        IF (DENTTL .LE. (0.05*ORIGDEN)) THEN
          DFALLN = FALLM2 * ORIGDEN
        ELSE
          DFALLN = MODRATE * ORIGDEN
          IF (DENTTL .LT. (DFALLN + 0.05*ORIGDEN)) THEN
            DFALLN = DENTTL - (ORIGDEN * (0.05 - FALLM2))
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END

