      SUBROUTINE SVSALV(IYR,MINDBH,MAXDBH,MAXAGE,OKSOFT,PROP,PROPLV)
      IMPLICIT NONE
C----------
C  **SVSALV--BASE  DATE OF LAST REVISION: 05/14/08
C----------
C
C     STAND VISUALIZATION GENERATION
C     S.N.SCHAROSCH -- Abacus -- NOV 2007
C
C   Purpose:
C     This routine removes snags from the base FVS snag list, in
C     response to a salvage operation done by the FFE, as specified
C     via SALVAGE/SALVSP keywords.
C
C   Called from: FMSALV, once at beginning of each cycle.
C
C   Local variable definitions:
C     ISALFG:  Array of salvage flags for base FVS snag records:
C                 ISALFG(i)=0: snag not available for salvage
C                 ISALFG(i)=1: snag available for salvage
C                 ISAlFG(i)=2: snag felled and salvaged
C                 ISAlFG(i)=3: snag felled and culled
C     IYR:     Current year
C     LINCL:   True if current snag record meets current species selection
C              criteria for salvage.
C     MAXDBH:  MAXimum DBH of snags to be salvaged
C     MAXAGE:  MAXimum AGE (years since death) of snags to be salvaged
C     MINDBH:  MINimum DBH of snags to be salvaged
C     OKSOFT:  Whether it's OK to salvage SOFT snags.
C              (Note: OKSOFT: 0=all snags, 1=hard only, 2=soft only)
C     PROP:    PROPortion of the eligible snags to actually salvage
C     PROPLV:  PROPortion of the eligible snags to LeaVe in the stand
C              as down material.
C
C   Common variable definitions:
C     ISALVS:  the sp/spgroup for which a specific cut/leave switch
C              was set via SALVSP keyword.
C     ISALVC   the cut/leave switch which was set for a specific
C              sp/spgroup, via the SALVSP keyword:
C              0 = cut this sp/spgroup in a salvage cut;
C              1 = leave this sp/spgroup in a salvage cut
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
      INCLUDE 'SVDATA.F77'
C
C
      INCLUDE 'SVDEAD.F77'
C
C
COMMONS
C
C
      INTEGER B4SNAGS, B4SVSNG, EL_SNAGS, I, IG, IGRP, IPUT,
     &        ISALFG(MXDEAD), ISNAG, ISREF(MXDEAD), ISVOBJ, IT,
     &        IULIM, IYR, J, K, NOTFELD, NWSVSNG, OKSOFT
      LOGICAL DEBUG, LINCL
      REAL    CULLDIF, CULLPCT, CULLTPA, EL_CULL, EL_FELL,
     &        FELLDIF, FELLPCT, FELLTPA, MAXDBH, MAXAGE, MINDBH,
     &        OVRPCT, PROP, PROPLV, UNDPCT, X
C
C
C----------
C  Check for debug:
C----------

      CALL DBCHK (DEBUG,'SVSALV',6,ICYC)

      IF (DEBUG) THEN
        WRITE (JOSTND,1000) ICYC,
     &                      ISALVS, ISALVC, MINDBH,
     &                      MAXDBH, MAXAGE, OKSOFT,
     &                      PROP, PROPLV, NDEAD,
     &                      NSVOBJ
 1000   FORMAT (' ','IN SVSALV, ICYC=',I5,':', / ,
     &          ' ',T5,'ISALVS=',I2,', ISALVC=',I2,', MINDBH=',F6.2,
     &              ', MAXDBH=',F6.2,', MAXAGE=',F6.2,', OKSOFT=',I1,
     &              ', PROP=',F4.2,', PROPLV=',F4.2,', NDEAD=',I4,
     &              ', NSVOBJ=',I5, // ,
     &          ' ',T5,'SNAG LIST BEFORE SALVAGE:', // ,
     &          ' ',T5,'  I  SPP SNGDIA OLEN IYRCOD STATUS FALLDIR',/,
     &          ' ',T5,'---- --- ------ ---- ------ ------ -------' )
C                       XXXX XXX XXX.XX XXX. XXXXXX XXXXXX XXXXXX.
        DO 20 I=1,NDEAD
          WRITE(JOSTND,1010) I, ISNSP(I), SNGDIA(I), OLEN(I),
     &                       IYRCOD(I), ISTATUS(I), FALLDIR(I)
 1010     FORMAT(' ',T5,I4,1X,I3,1X,F6.2,1X,F4.0,1X,
     &               I6,1X,I6,1X,F7.0)
   20   CONTINUE
      ENDIF

      IF (NDEAD.EQ.0) RETURN

C----------
C  Compute:
C    1) EL_SNAGS: number of eligible snags for salvaging, according to
C                 selection criteria from SALVAGE keyword.
C    2) EL_FELL: number of eligible snags to cut, according to PROP
C                field of SALVAGE keyword.
C    3) EL_CULL: number of felled snags to leave in stand, according to
C                PROPLV field of SALVAGE keyword.
C  For tallying, snag must meet following selection criteria:
C    1) Species has not been excluded from salvage via a SALVSP keyword.
C    2) DBH is within specified SALVAGE range.
C    3) Snag age is less than specified maximum SALVAGE age.
C    4) Snag condition (hard/soft) meets specified SALVAGE criteria.
C    5) Snag is still standing.
C  Snag records represent whole trees, so compute the eligible snag
C  counts as the desired percentage times the number of applicable
C  snag records.
C----------

      EL_SNAGS = 0
      DO 100 I=1,NDEAD
        ISALFG(I) = 0
        LINCL = .FALSE.
        IF(ISALVS.EQ.0 .OR. ISALVS.EQ.ISNSP(I))THEN
          LINCL = .TRUE.
        ELSEIF(ISALVS.LT.0)THEN
          IGRP = -ISALVS
          IULIM = ISPGRP(IGRP,1)+1
          DO 60 IG=2,IULIM
          IF(ISNSP(I) .EQ. ISPGRP(IGRP,IG))THEN
            LINCL = .TRUE.
            GO TO 70
          ENDIF
   60     CONTINUE
        ENDIF
   70   CONTINUE

        IF(ISALVC.EQ.0 .AND. .NOT.LINCL) GO TO 100
        IF(ISALVC.EQ.1 .AND. LINCL) GO TO 100
        IF(SNGDIA(I).LT.MINDBH .OR. SNGDIA(I).GT.MAXDBH) GO TO 100
        IF((IYR-IYRCOD(I)).GT.MAXAGE) GO TO 100
        IF(OKSOFT.EQ.1 .AND. ISTATUS(I).EQ.4) GO TO 100
        IF(OKSOFT.EQ.2 .AND. ISTATUS(I).NE.4) GO TO 100
        IF(FALLDIR(I).NE.-1) GO TO 100

        ISALFG(I) = 1
        EL_SNAGS = EL_SNAGS + 1
  100 CONTINUE

      EL_FELL = EL_SNAGS * PROP
      EL_CULL = EL_FELL * PROPLV
      B4SNAGS = NDEAD
      B4SVSNG = 0
      DO 110 I=1,NSVOBJ
        IF (IOBJTP(I).EQ.2) B4SVSNG=B4SVSNG+1
  110 CONTINUE

      IF (DEBUG) THEN
        WRITE (JOSTND,1020) EL_SNAGS, EL_FELL, EL_CULL
 1020   FORMAT (/,' ',T5,'EL_SNAGS=',I4,', EL_FELL=',F5.0,
     &               ', EL_CULL=',F5.0)
      ENDIF

      IF (EL_SNAGS.EQ.0.0) RETURN

C----------
C  Loop back through the base FVS snag list, and process snags that match
C  FFE SALVAGE operation.
C  Since salvage operations target merchantable volume, process snags
C  in order of decreasing DBH (no preference given to hard vs soft).
C----------

      CALL RDPSRT(NDEAD,SNGDIA,ISREF,.TRUE.)
      FELLTPA = 0.0
  120 CONTINUE

      DO 200 I=1,NDEAD
        IT = ISREF(I)
        IF (ISALFG(IT).NE.1) GO TO 200

C----------
C  Snag record meets selection criteria.
C  Snag records each represent one tree/ac; but SALVAGE keyword allows
C  for the simulation of partial snag removal. Therefore, we need to
C  stochastically fell snag records until we hit the desired residual
C  snag density.
C----------

        CALL SVRANN(X)
        IF (X.LE.PROP) THEN
          FELLTPA = FELLTPA + 1

C----------
C  If removing this snag record puts us over the desired salvage
C  proportion, test to see if we're better off leaving it.
C----------
          IF ((FELLTPA/FLOAT(EL_SNAGS)).GT.PROP) THEN
            OVRPCT = (FELLTPA/FLOAT(EL_SNAGS)) - PROP
            UNDPCT = ((FELLTPA-1)/FLOAT(EL_SNAGS)) - PROP
            IF (DEBUG) THEN
              WRITE (JOSTND,1030) OVRPCT, UNDPCT
 1030         FORMAT (' ',T5,'OVRPCT=',F8.4,', UNDPCT=',F8.4)
            ENDIF
            IF (OVRPCT.GT.ABS(UNDPCT)) THEN
              FELLTPA = FELLTPA - 1
              GO TO 210
            ENDIF
          ENDIF
C----------
C  Snag record selected for felling.
C----------

          ISALFG(IT) = 2
        ENDIF
        FELLDIF = (FELLTPA/FLOAT(EL_SNAGS)) - PROP
        IF (FELLDIF.GT.0.0 .OR. ABS(FELLDIF).LE.0.005) GO TO 210
  200 CONTINUE

C----------
C  If, by random chance, we did not select enough snags to meet
C  desired salvage proportion, go back through the snag list again.
C----------
      IF ((FELLTPA/FLOAT(EL_SNAGS)).LT.PROP) THEN
        IF (DEBUG) THEN
          WRITE (JOSTND,1070) FELLDIF
 1070     FORMAT (' ',T5,'INSUFFICIENT SNAG REMOVAL.',
     &                'FELLDIF=',F6.2,'. TRYING AGAIN...')
        ENDIF
        GO TO 120
      ENDIF

  210 CONTINUE

C----------
C  Go back through the snags that have been selected for felling,
C  and stochastically determine which get salvaged, and which get
C  culled.
C----------

      CULLTPA = 0.0
  220 CONTINUE

      DO 300 I=1,NDEAD
        IT = ISREF(I)
        IF (ISALFG(IT).NE.2) GO TO 300
        CALL SVRANN(X)
        IF (X.LE.PROPLV) THEN
          CULLTPA = CULLTPA + 1

C----------
C  If culling this snag record puts us over the desired cull
C  proportion, test to see if we're better off leaving it.
C----------
          IF ((CULLTPA/FELLTPA).GT.PROPLV) THEN
            OVRPCT = (CULLTPA/FELLTPA) - PROPLV
            UNDPCT = ((CULLTPA-1)/FELLTPA) - PROPLV
            IF (DEBUG) THEN
              WRITE (JOSTND,1030) OVRPCT, UNDPCT
            ENDIF
            IF (OVRPCT.GT.ABS(UNDPCT)) THEN
              CULLTPA = CULLTPA - 1
              GO TO 310
            ENDIF
          ENDIF
          ISALFG(IT) = 3
          IF (DEBUG) THEN
            WRITE (JOSTND,1080) IT, SNGDIA(IT), CULLTPA
 1080       FORMAT (' ',T5,'SELECTED TO FELL & LEAVE, SNAG:',I4,
     &                  ', SNGDIA(IT):',F5.2,', CULLTPA:',F6.0)
          ENDIF
          CALL SVRANN(X)
          FALLDIR(IT) = IFIX((360.0*X) + 0.5)
        ENDIF
        CULLDIF = (CULLTPA/FELLTPA) - PROPLV
        IF (CULLDIF.GT.0.0 .OR. ABS(CULLDIF).LE.0.005) GO TO 310
  300 CONTINUE

C----------
C  If, by random chance, we did not select enough snags to meet
C  desired cull proportion, go back through the snag list again.
C----------
      IF ((CULLTPA/FELLTPA).LT.PROPLV) THEN
        IF (DEBUG) THEN
          WRITE (JOSTND,1090)
 1090     FORMAT (' ',T5,'INSUFFICIENT SNAG CULLING, TRYING AGAIN...')
        ENDIF
        GO TO 220
      ENDIF

  310 CONTINUE

C----------
C  For snags being felled and salvaged, flag the record for
C  removal from the base FVS snag arrays, and the SVS snag objects.
C  Don't actually remove the snags yet, since they need to be
C  displayed as downed snags in the post thin/salvage SVS display.
C  Set IOBJTP to 5 to signal the salvaged snags will be removed
C  after the next call to SVOUT.
C----------

      DO 400 I=1,NDEAD
        IF (ISALFG(I).EQ.2) THEN
          ISTATUS(I) = ISTATUS(I) * (-1)
          DO 350 J=1,NSVOBJ
            IF (IOBJTP(J).EQ.2 .AND. IS2F(J).EQ.I) THEN
              IOBJTP(J) = 5
              EXIT
            ENDIF
  350     CONTINUE
        ENDIF
  400 CONTINUE

      IF (DEBUG) THEN
        WRITE (JOSTND,1100) NDEAD, NSVOBJ
 1100   FORMAT (/,
     &          ' ',T5,'LEAVING SVSALV. NDEAD=',I4,', NSVOBJ=',I5, // ,
     &          ' ',T5,'SNAG LIST AFTER SALVAGE:', // ,
     &          ' ',T5,'  I  SPP SNGDIA OLEN IYRCOD STATUS FALLDIR',/,
     &          ' ',T5,'---- --- ------ ---- ------ ------ -------' )
C                       XXXX XXX XXX.XX XXX. XXXXXX XXXXXX XXXXXX.
        DO 520 I=1,NDEAD
          WRITE(JOSTND,1010) I, ISNSP(I), SNGDIA(I), OLEN(I),
     &                       IYRCOD(I), ISTATUS(I), FALLDIR(I)
  520   CONTINUE

        NOTFELD = EL_SNAGS - FELLTPA
        FELLPCT = (FELLTPA/EL_SNAGS)*100.0
        CULLPCT = (CULLTPA/FELLTPA)*100.0
        NWSVSNG = 0
        DO 530 I=1,NSVOBJ
          IF (IOBJTP(I).EQ.2) NWSVSNG=NWSVSNG+1
  530   CONTINUE
        
        WRITE (JOSTND,1110) B4SNAGS, B4SVSNG, EL_SNAGS,
     &                      NDEAD, NWSVSNG, NOTFELD,
     &                      FELLPCT, CULLPCT
 1110   FORMAT (/,
     &          ' ',T5,16X,' Total      Total    Eligible ', / ,
     &          ' ',T5,16X,'base FVS   SVS snag  standing ', / ,
     &          ' ',T5,16X,'snag recs  objects   snag recs', / ,
     &          ' ',T5,16X,'---------  --------  ---------', / ,
     &          ' ',T5,'Before salvage:',3X,I4,6X,I5,6X,I4, / ,
     &          ' ',T5,'After salvage: ',3X,I4,6X,I5,6X,I4, / ,
     &          ' ',T5,'Snag fell pct: ',23X,F5.1, / ,
     &          ' ',T5,'Snag cull pct: ',23X,F5.1, / )
      ENDIF

      RETURN
      END

