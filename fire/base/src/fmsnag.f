      SUBROUTINE FMSNAG (IYR, YR1)
      IMPLICIT NONE
C----------
C  $Id$
C  $Id$
C----------
C     CALLED FROM: FMMAIN
C     CALLS   FMSSEE
C             FMSADD
C             FMSFALL
C             CWD1 (entry in FMCWD)
C             FMSNGHT
C             CWD2 (entry in FMCWD)
C             FMSNGDK
C
*----------------------------------------------------------------------
*  Purpose:
*     THIS SUBROUTINE CALCULATES ANNUAL CHANGES IN EACH SNAG RECORD,
*     INCLUDING THE LOSS OF SNAGS DUE TO FALLDOWN, THE TRANSITION FROM
*     'HARD' TO 'SOFT' DECAY STATES, AND HEIGHT LOSS DUE TO TOP BREAKAGE.
*     NI - This subroutine calculates annual changes in each snag record,
*          including the loss of snags due to falldown, the transition from
*          'hard' to 'soft' decay states, and height loss due to top breakage.
*          The calculations are based on discussions with Richard Teck and
*          Nick Crookston, and data from Bruce Marcot's Snag Recruitment
*          Simulator spreadsheet (Eastside version) and Cline et al. 1980.
*          'Base' rate-of-fall is calculated from DBH assuming that Marcot's
*          observed rate for <18" snags applies on average to 9" snags, and
*          his rate for >18" snags applies to 28" snags.
*     SN - MULTIPLIERS TO TWEAK THE EQUATIONS BY SPECIES WERE DEVELOPED FROM 
*          OZARK-OUACHITA WORKSHOP INPUT
*     LS - MULTIPLIERS AND EQUATIONS WERE DEVELOPED FROM LAKE STATES WORKSHOP INPUT
*     NE - FALL RATES WERE DETERMINED FROM DISCUSSIONS WITH COELI HOOVER AND LINDA
*          HEATH AND A PAPER BY YAMASAKI AND LEAK (IN PRESS, NJAF) 
*
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     BASE:   base rate-of-fall for snags of this size (i.e., rate for
*             snags with FALLX=1)
*     DFIS:   density of initially-soft snags to fall / fallen.
*     DFIH:   density of initially-hard snags to fall / fallen.
*     DFALLN: target density of snags to fall under normal conditions
*             (where hard and soft snags fall at the same rate).
*     DZERO:  Density (#/acre) of snags considered equal to ZERO
*     RSOFT:  Rate of snag fall implied by PBSOFT and PBTIME
*     RSMAL:  Rate of snag fall implied by PBSMAL and PBTIME
*     YR1:    First year in the master cycle
*
*  Common block variables and parameters:
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'

C.... Variable declarations.

      CHARACTER VVER*7
      INTEGER I, JSP, YR1, YEAR
      INTEGER IYR,NTODO,JDO,NPRM,IACTK,JYR
      REAL    BASE, DENTTL, DFIS, DFIH, DFALLN, DZERO, HTSNEW
      REAL    OLDHTH, OLDHTS, X, XS, XH
      REAL    D,DKTIME,HTD,AGE,SNUM,RSOFT,RSMAL
      LOGICAL DEBUG, LASCO

      INTEGER MYACT(1)
      REAL    PRMS(6)
      DOUBLE PRECISION SAVESO

      DATA MYACT/2522/
C
C     CHECK FOR DEBUG.
C
      CALL DBCHK (DEBUG,'FMSNAG',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC, IYR, YR1, NSNAG
    7 FORMAT(' FMSNAG CYCLE=',I2,' IYR=',I5,' YR1=',I5,' NSNAG=',I5)

C.... Begin routine.

C     Before we do anything else, in the first year check to see if the user
C     initialized some snags.  These need to be added to the snag lists.

      IF (IYR .EQ. YR1) THEN
        CALL OPFIND(1,MYACT,NTODO)
        DO 5 JDO = 1,NTODO
          CALL OPGET(JDO,6,JYR,IACTK,NPRM,PRMS)
          JSP = PRMS(1)
            D = PRMS(2)
          HTD = PRMS(3)
          AGE = PRMS(5)
          YEAR = IYR - AGE
          SNUM = PRMS(6)
          IF (DEBUG) WRITE(JOSTND,*)' IN FMSNAG JDO=',JDO,' JSP=',JSP
          CALL FMSSEE (1,JSP,D,HTD,SNUM,4,DEBUG,JOSTND)
          CALL FMSADD (YEAR,-JDO)
          CALL OPDONE(JDO,IYR)
    5   CONTINUE
      ENDIF

C     Initialize variables.

      DZERO = NZERO / 50.0
      CALL VARVER(VVER)

C     Loop over all snag records that are in use.

      IF (NSNAG.LE.0) RETURN

      IF (VVER(1:2) .EQ. 'PN' .OR.
     &    VVER(1:2) .EQ. 'WC' .OR.
     &    VVER(1:2) .EQ. 'OP' .OR.
     &    VVER(1:2) .EQ. 'SO' .OR.
     &    VVER(1:2) .EQ. 'BM' .OR.
     &    VVER(1:2) .EQ. 'EC') THEN
          CALL RANNGET(SAVESO)
      ENDIF

      DO 100 I = 1, NSNAG

C       Skip this record if there are no snags in it.

        IF (DEBUG) WRITE(JOSTND,*)'IN FMSNAG I=',I,' DENIS=',DENIS(I),
     >                            ' DENIH=',DENIH(I)

        IF ((DENIS(I) + DENIH(I)) .LE. 0.0) GOTO 100

        JSP = SPS(I)
C
C       SEE IF SPECIES IS ASPEN OR COTTONWOOD OR PAPER BIRCH - 
C       SPECIAL RULES IN UT/TT/CR
C
        LASCO = .FALSE.
        IF (VVER(1:2) .EQ. 'UT') THEN
          IF (JSP.EQ.6 .OR. JSP.EQ.18 .OR. JSP.EQ.19) LASCO = .TRUE.
        ELSE IF (VVER(1:2) .EQ. 'TT') THEN
          IF (JSP.EQ.6 .OR. JSP.EQ.15)   LASCO = .TRUE.
        ELSE IF (VVER(1:2) .EQ. 'CR' .OR.
     &        VVER(1:2) .EQ. 'SM' .OR.
     &        VVER(1:2) .EQ. 'SP' .OR.
     &        VVER(1:2) .EQ. 'BP' .OR.
     &        VVER(1:2) .EQ. 'LP' .OR.     
     &        VVER(1:2) .EQ. 'SF') THEN 
          IF (JSP.EQ.20 .OR. JSP.EQ.21 .OR. JSP.EQ.22 .OR. JSP.EQ.28)
     &        LASCO = .TRUE.
        ENDIF

C       Call FMSFALL to:
C         1) Compute special fall rates if this after a fire.
C         2) Calculate the density of snags in this record that would fall
C            ANNUALLY under normal conditions.  This depends on species, dbh and
C            whether 5% are left.

        JSP = SPS(I)
        DENTTL = DENIH(I) + DENIS(I)
        CALL FMSFALL(IYR,JSP,DBHS(I),DEND(I),DENTTL,1,
     &               RSOFT,RSMAL,DFALLN)

        IF (DEBUG) WRITE(JOSTND,*)' IN FMSNAG RSOFT=',RSOFT,
     &                            ' RSMAL=',RSMAL,' PBSOFT=',pbsoft,
     &                            ' PBSMAL=',PBSMAL

C     Now set PBFRIH and PBFRIS. Different rates apply to small snags and
C     snags that are soft AT TIME OF FIRE (whether initially hard or soft):
C     if both apply, use whichever rate is greater.  Fires do not affect
C     the fall rate of large snags that are hard at the time of the fire.

        IF ((IYR - BURNYR) .LE. 1) THEN
          PBFRIS(I) = RSOFT
          PBFRIH(I) = 0.0

          IF (DBHS(I) .LT. PBSIZE) THEN
            PBFRIH(I) = RSMAL
            IF ((PBFRIH(I) .LT. PBFRIS(I)) .AND. (.NOT. HARD(I)))
     &        PBFRIH(I) = PBFRIS(I)
            IF (PBFRIH(I) .GT. PBFRIS(I))
     &        PBFRIS(I) = PBFRIH(I)
          ENDIF
        ENDIF

C     Set DFIS and DFIH according to whichever density is largest - the
C     density falling under normal conditions, or the density to fall under
C     post-burn conditions if these are applicable.

C       ** Special handling for aspen & cottonwood (LASCO .eq. .TRUE.) **
C
        DFIS = DENIS(I) * DFALLN / (DENIS(I)+DENIH(I))
        DFIH = DENIH(I) * DFALLN / (DENIS(I)+DENIH(I))

        IF (BURNYR .GT. 0  .AND.
     >              YRDEAD(I) .LE. BURNYR) THEN
            IF (LASCO .AND. (IYR-BURNYR) .LE. 10) THEN
          DFIS = DFIS * 0.5
          DFIH = DFIH * 0.5
          XS = PBFRIS(I) * DENIS(I)
            XH = PBFRIH(I) * DENIH(I)
            IF (DFIS .LT. XS) DFIS = XS
              IF (DFIH .LT. XH) DFIH = XH   
            ELSEIF ((IYR-BURNYR) .LE. PBTIME) THEN
              XS = PBFRIS(I) * DENIS(I) 
              XH = PBFRIH(I) * DENIH(I) 
            IF (DFIS .LT. XS) DFIS = XS
              IF (DFIH .LT. XH) DFIH = XH   
            ENDIF       
          ENDIF

C     Now actually remove the snags.  If less than DZERO will be left,
C     remove them all.

        IF (DFIS .GT. (DENIS(I)-DZERO)) DFIS = DENIS(I)
        IF (DFIH .GT. (DENIH(I)-DZERO)) DFIH = DENIH(I)
        DENIS(I) = DENIS(I) - DFIS
        DENIH(I) = DENIH(I) - DFIH

C     Add the fallen snags to down debris pools.

        CALL CWD1(I, DFIH, DFIS)

C     Skip the rest of the loop if no snags are left in the current record.

        IF ((DENIS(I) + DENIH(I)) .LE. DZERO) THEN
          DENIS(I) = 0.0
          DENIH(I) = 0.0
          GOTO 100
        ENDIF

C       Call FMSNGHT to predict snag height loss due to top breakage.
C       OLDHT here is just a temporary record of the height that the snags
C       had before adjustment for breakage during the preceding timestep.

        OLDHTH = -1.0
        OLDHTS = -1.0

        IF (DENIH(I) .GT. 0.0) THEN
          OLDHTH = HTIH(I)
          CALL FMSNGHT(VVER,JSP,HTDEAD(I),HTIH(I),1,HTSNEW)
          HTIH(I) = HTSNEW
        ENDIF
        IF (DENIS(I) .GT. 0.0) THEN
          OLDHTS = HTIS(I)
          CALL FMSNGHT(VVER,JSP,HTDEAD(I),HTIS(I),0,HTSNEW)
          HTIS(I) = HTSNEW
        ENDIF

        CALL CWD2(I, DENIH(I), DENIS(I), OLDHTH, OLDHTS)

C     If all the init-hard or init-soft snags in this record have zero
C     height, set their density to zero.  Skip the rest of the loop if
C     this means there are no snags of either type left in the record.

        IF (VVER(1:2) .EQ. 'TT' .OR.
     &      VVER(1:2) .EQ. 'UT' .OR.
     &      VVER(1:2) .EQ. 'CR' .OR.
     &      VVER(1:2) .EQ. 'LP' .OR.
     &      VVER(1:2) .EQ. 'BP' .OR.
     &      VVER(1:2) .EQ. 'SP' .OR.     
     &      VVER(1:2) .EQ. 'SF' .OR.
     &      VVER(1:2) .EQ. 'SM') THEN
          IF ((DENIH(I) .GT. 0.0) .AND. (HTIH(I) .LT. 1.5)) 
     &      DENIH(I) = 0.0
          IF ((DENIS(I) .GT. 0.0) .AND. (HTIS(I) .LT. 1.5))
     &      DENIS(I) = 0.0
        ELSE
          IF ((DENIH(I) .GT. 0.0) .AND. (HTIH(I) .LT. 1.0)) 
     &      DENIH(I) = 0.0
          IF ((DENIS(I) .GT. 0.0) .AND. (HTIS(I) .LT. 1.0))
     &      DENIS(I) = 0.0
        ENDIF

        IF ((DENIS(I) + DENIH(I)) .LE. 0.0) GOTO 100

C       Call FMSNGDK to predict years, since death, for snag to become
C       soft. Update hard/soft status accordingly.

        IF ((DENIH(I) .GT. 0.0) .AND. (HARD(I))) THEN
          CALL FMSNGDK(VVER,JSP,DBHS(I),DKTIME)
          IF ((IYR - YRDEAD(I)) .GE. DKTIME) HARD(I) = .FALSE.
        ENDIF

  100 CONTINUE

      IF (VVER(1:2) .EQ. 'PN' .OR.
     &    VVER(1:2) .EQ. 'WC' .OR.
     &    VVER(1:2) .EQ. 'OP' .OR.
     &    VVER(1:2) .EQ. 'SO' .OR.
     &    VVER(1:2) .EQ. 'BM' .OR.
     &    VVER(1:2) .EQ. 'EC') THEN
          CALL RANNPUT(SAVESO)
      ENDIF

      RETURN
      END
