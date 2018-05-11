      SUBROUTINE FMSNGHT(VVER,KSP,HTD,HTCURR,IHRD,HTSNEW)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id: fmsnght.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
C
C     SNAG HEIGHT PREDICTION
C
C   Purpose:
C     This routine predicts snag height loss due to top breakage.
C
C     The base logic in this routine was extracted from its original
C     location in FMSNAG, in order to structure/generalize the logic
C     for use with both the FFE snag pools, and the base FVS model
C     snag records.
C
C   Called from: FMSNAG to compute height loss for a given FFE snag pool.
C                SVSNAGE to compute height loss for a given FVS snag record.
C
C   Local variable definitions:
C     IHRD:    Indicates if current height loss prediction is for a hard
C              or soft snag. Used to reference appropriate HTX array elements.
C     HTCURR:  Current height of snag pool/record.
C     HTSNEW:  Updated height for snag pool/record.
C     HTD:     Height of current snag pool/record, at time of death.
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
      CHARACTER VVER*7
      INTEGER HTINDX1, HTINDX2, IHRD, KSP
      REAL    HTCURR, HTD, HTSNEW, SFTMULT, X2
C
C----------
C  Set array indices for retrieving hard/soft height loss coefficients.
C----------
C
      IF ( IHRD .EQ. 1 ) THEN
        HTINDX1 = 1
        HTINDX2 = 2
        SFTMULT = 1.0
      ELSE
        HTINDX1 = 3
        HTINDX2 = 4
        SFTMULT = HTXSFT
      ENDIF
C
C----------
C  Decrease the height of snags according to current height and
C  whether they were initially hard or soft. If height gets to
C  be less than 1.5 foot, call it 0 (the 'snag' is considered to be
C  'fuel' now instead).
C----------
C
      SELECT CASE (VVER(1:2))
        CASE('CI')
C       CI variant has special rules for WP & RC

          SELECT CASE (KSP)
            CASE (1,6)          ! WP, RC STOP LOSING HEIGHT AT 75%
              IF (HTCURR .GT. (0.75 * HTD)) THEN
                HTSNEW = HTCURR * (1.0 - HTR1 * HTX(KSP,HTINDX1)
     &                   * SFTMULT)**NYRS
              ELSE
                HTSNEW = HTCURR
              ENDIF
            CASE DEFAULT
              IF (HTCURR .GT. (0.5 * HTD)) THEN
                HTSNEW = HTCURR * (1.0 - HTR1 * HTX(KSP,HTINDX1)
     &                   * SFTMULT)**NYRS
              ELSE
                HTSNEW = HTCURR * (1.0 - HTR2 * HTX(KSP,HTINDX2)
     &                   * SFTMULT)**NYRS
              ENDIF
          END SELECT

        CASE('PN', 'WC', 'BM', 'EC', 'OP')
C       First, get the height loss rate from fmr6htls. But if the
C       height loss is adjusted by user (snagbrk keyword), make sure
C       you use their values.

          CALL FMR6HTLS(KSP,X2)
          IF (HTCURR .GT. (0.5 * HTD)) THEN
            IF ((HTX(KSP,HTINDX1) .GT. 1.01) .OR.
     &          (HTX(KSP,HTINDX1) .LT. 0.99)) THEN
              HTSNEW = HTCURR * 
     &                   (1.0 - HTR1 * HTX(KSP,HTINDX1)*SFTMULT)**NYRS
            ELSE
              HTSNEW = HTCURR * (1.0 - X2)**NYRS
            ENDIF
          ELSE
            IF ((HTX(KSP,HTINDX2) .GT. 1.01) .OR.
     &          (HTX(KSP,HTINDX2) .LT. 0.99)) THEN
              HTSNEW = HTCURR * 
     &                   (1.0 - HTR2 * HTX(KSP,HTINDX2)*SFTMULT)**NYRS
            ELSE
              HTSNEW = HTCURR * (1.0 - X2)**NYRS
            ENDIF
          ENDIF

        CASE('SO')
          SELECT CASE (KODFOR)
          CASE(505,506,509,511,701,514)                  ! CALIFORNIA

            IF (HTCURR .GT. (0.5 * HTD)) THEN
              HTSNEW = HTCURR * 
     &                   (1.0 - HTR1 * HTX(KSP,HTINDX1) *SFTMULT)**NYRS
            ELSE
              HTSNEW = HTCURR * 
     &                   (1.0 - HTR2 * HTX(KSP,HTINDX2) *SFTMULT)**NYRS
            ENDIF
C
          CASE DEFAULT                                   ! OREGON

C         First, get the height loss rate from fmr6htls. But if the
C         height loss is adjusted by user (snagbrk keyword), make sure
C         you use their values.

            CALL FMR6HTLS(KSP,X2)
            IF (HTCURR .GT. (0.5 * HTD)) THEN
              IF ((HTX(KSP,HTINDX1) .GT. 1.01) .OR.
     &            (HTX(KSP,HTINDX1) .LT. 0.99)) THEN
                HTSNEW = HTCURR * 
     &                    (1.0 - HTR1 *HTX(KSP,HTINDX1)*SFTMULT)**NYRS
              ELSE
                HTSNEW = HTCURR * (1.0 - X2)**NYRS
              ENDIF
            ELSE
              IF ((HTX(KSP,HTINDX2) .GT. 1.01) .OR.
     &            (HTX(KSP,HTINDX2) .LT. 0.99)) THEN
                HTSNEW = HTCURR * 
     &                     (1.0 - HTR2 *HTX(KSP,HTINDX2)*SFTMULT)**NYRS
              ELSE
                HTSNEW = HTCURR * (1.0 - X2)**NYRS
              ENDIF
            ENDIF
          END SELECT

        CASE DEFAULT
          IF (HTCURR .GT. (0.5 * HTD)) THEN
            HTSNEW = HTCURR * 
     &                (1.0 - HTR1 * HTX(KSP,HTINDX1) * SFTMULT)**NYRS
          ELSE
            HTSNEW = HTCURR * 
     &                 (1.0 - HTR2 * HTX(KSP,HTINDX2) * SFTMULT)**NYRS
          ENDIF

      END SELECT

      IF (HTSNEW .LT. 1.5) HTSNEW = 0.0

      RETURN
      END

