      Subroutine FMSFALL(IYR,KSP,D,ORIGDEN,DENTTL,ISWTCH,
     &                   RSOFT,RSMAL,DFALLN)
      IMPLICIT NONE
C----------
C FIRE-PN $Id: fmsfall.f 0000 2018-02-14 00:00:00Z gedixon $
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
      INTEGER ISWTCH, IYR, JADJ, JSML, JYRSOFT, KSP
      LOGICAL DEBUG
      REAL    BASE, D, DENTTL, DFALLN, DZERO, ORIGDEN,
     &        RSOFT, RSMAL
C
C----------
C  Check for debug:
C----------
      CALL DBCHK (DEBUG,'FMSFALL',7,ICYC)
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
      DFALLN = 0.
      RSOFT = 0.0
      RSMAL = 0.0
      IF (DENTTL .LE. 0) RETURN
      IF ((IYR - BURNYR) .LT. PBTIME) THEN
        DZERO = NZERO / 50.0

        IF (PBSOFT .GT. 0.0) THEN
          IF (PBSOFT .LT. 1.0) THEN
            RSOFT = 1 - EXP( LOG(1-PBSOFT) / PBTIME )
          ELSE
            RSOFT = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
          ENDIF
        ENDIF

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
C  normal conditions.  This depends on species, dbh and whether 5% are
C  left.
C
C  Call fmr6sdcy now because snag decay affects the snag fall rate through
C  an adjustment factor.  Also, fmr6sdcy holds the dbh breakpoints used
C  to determine whether a snag is small, or large.
C----------
      
      CALL FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
        
C----------
C  Call fmr6all to determine the snag fall rate.
C----------
 
      CALL FMR6FALL(KSP, JSML, JADJ, BASE)
      DFALLN = BASE * FALLX(KSP) * DENTTL

      IF (DEBUG) THEN
        WRITE(JOSTND,8) KSP, D, JSML, JADJ, JYRSOFT
    8   FORMAT(' FMSFALL KSP=',I5,' DBHS=',F6.2,' JSML=',I3,' JADJ=',I3,
     &         ' JYRSOFT=',I5)
        WRITE(JOSTND,9) BASE, FALLX(KSP)
    9   FORMAT(' FMSNAG BASE=',F6.3,' FALLX=',F6.2)
      ENDIF
C
      RETURN
      END

