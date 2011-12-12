      SUBROUTINE FMSALV (IYR,SALVTPA)
      IMPLICIT NONE
C----------
C  **FMSALV  FIRE--DATE OF LAST REVISION:  03/04/09
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: CUTS
C     CALLS:       FMSVOL
C                  CWD1
C                  SVSALV
*----------------------------------------------------------------------
*  Purpose:
*     This subroutine removes existing snags as specified by the
*     SALVAGE keyword. It keeps track of both the total volume of the
*     snags that are removed, and their number. It also calculates
*     CWDCUT, which FMCADD will use to remove a proportion of future
*     snag debris from CWD2B and add it to current debris.
*     amount of material in CWD2B to account
*     NOTE:  This routine may be called in any year
*----------------------------------------------------------------------
*
*  Local variable definitions:
*      CUTDIS:  Density of Initially-Soft snags CUT in this salvage
*      CUTDIH:  Density of Initially-Hard snags CUT in this salvage
*      CUTVOL:  cumulative VOLume of snags CUT in this cycle's salvage(s)
*      IHARDV:  Volume of each Initially-HARD snag in the current record
*      ISOFTV:  Volume of each Initially-SOFT snag in the current record
*      IHARDV2  Merch volume of each Initially-HARD snag in the current record
*      ISOFTV2: Merch volume of each Initially-SOFT snag in the current record
*      KEY:     the identity number of the salvage KEYword specifications
*               that apply to this stand in this year.
*      MAXDBH:  MAXimum DBH of snags to be salvaged
*      MAXAGE:  MAXimum AGE (years since death) of snags to be salvaged
*      MINDBH:  MINimum DBH of snags to be salvaged
*      OKSOFT:  whether it's OK to salvage SOFT snags
*               (OKSOFT: 0=all snags, 1=hard only, 2=soft only)
*      PROP:    PROPortion of the eligible snags to actually salvage
*      PROPLV:  PROPortion of the eligible snags to LeaVe in the stand
*               as down material (add to CWD).
*      THISRM:  volume of snags removed in each salvage operation
*      TOTVOL:  TOTal VOLume of all snags before salvage
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
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMPROP.F77'
      INCLUDE 'CONTRL.F77'

C.... Variable declarations.

      INTEGER I, J, K, KEY, OKSOFT
      REAL    CUTDIH, CUTDIS
      REAL    CUTVOL, MAXDBH, MAXAGE, MINDBH, PROP, PROPLV
      REAL    IHARDV, ISOFTV, IHARDV2, ISOFTV2
      REAL    SALVTPA, TOTVOL, THISRM
      REAL    ABIO,MBIO,RBIO, X, XNEG1
      REAL    PRMS(7)
      INTEGER MYACT(2)
      DATA    MYACT/2501,2520/
      INTEGER IYR,JDO,NPRM,IACTK,JYR
      INTEGER IGRP,IULIM,IG
      LOGICAL LINCL,LMERCH,DEBUG

C.... Begin routine.
C     Initialize some outputs.  If a salvage is not requested in this stand
C     in this year (KEY<0).
C
      CALL DBCHK (DEBUG,'FMSALV',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING FMSALV-ICYC= ',ICYC
C
      CWDCUT = 0.0
      CUTVOL = 0.0
      SALVTPA = 0.0
      TONRMS = 0.0
      NSNAGSALV=NSNAG
      DO I = 1,NSNAG
        SALVSPA(I,1) = 0.
        SALVSPA(I,2) = 0.
        HTIHSALV(I)=HTIH(I)
        HTISSALV(I)=HTIS(I)
        SPSSALV(I)=SPS(I)
        DBHSSALV(I)=DBHS(I)
        HARDSALV(I)=HARD(I)
        HTDEADSALV(I)=HTDEAD(I)
      ENDDO
C
      CALL OPFIND(2,MYACT,KEY)
      IF (KEY .LE. 0) RETURN

C     COMPUTE TOTAL VOLUME OF SNAGS PRIOR TO ANY SALVAGE. 
C     THIS IS USED FOR CALCULATING THE PROPORTION VOL CUT, USED 
C     FOR CWD CALCS LATER. THUS, EAST/WEST VARIANT IS NOT IMPORTANT.

      TOTVOL = 0.0
      DEBUG  = .FALSE.

      LMERCH = .FALSE.
      IF (LVWEST) LMERCH = .TRUE.

      DO I = 1,NSNAG
        IF ((DENIS(I) + DENIH(I)) .GT. 0.0) THEN
          IF (DENIS(I) .GT. 0.0) THEN
            ISOFTV  = 0.0
            CALL FMSVOL(I, HTIS(I), ISOFTV,.FALSE.,0)
            TOTVOL = TOTVOL + DENIS(I) * ISOFTV
          ENDIF

          IF (DENIH(I) .GT. 0.0) THEN
            IHARDV  = 0.0
            CALL FMSVOL(I, HTIH(I), IHARDV,.FALSE.,0)
            TOTVOL = TOTVOL + DENIH(I) * IHARDV
          ENDIF
        ENDIF
      ENDDO
C
      DO JDO = 1,KEY

        CALL OPGET(JDO,6,JYR,IACTK,NPRM,PRMS)
C
C       IF THIS IS A SALVSP SETTING, PROCESS IT
C
        IF(IACTK .EQ. 2501) THEN
          ISALVS = INT(PRMS(1))
          ISALVC = INT(PRMS(2))
          CALL OPDONE(JDO,IYR)
          GO TO 200
        ENDIF
C
C       Get the parameter values for this salvage operation
C       and initialize counter. Check ranges again (in case
C       of call through PARMS)
C
        MINDBH = MAX(  0.0, PRMS(1))
        MAXDBH = MIN(999.0, PRMS(2))
        MAXAGE = MAX(  0.0, PRMS(3))
        IF (PRMS(4) .GT. 2.0 .OR. PRMS(4) .LT. 0.0) PRMS(4) = 0.0
        OKSOFT = INT(PRMS(4))
        PROP   = MIN(1.0,MAX(0.0,PRMS(5)))
        PROPLV = MIN(1.0,MAX(0.0,PRMS(6)))
C
C       Loop over all snag records that are in use.
C
        THISRM = 0.0
        DO 100 I = 1,NSNAG

        LINCL = .FALSE.
        IF(ISALVS.EQ.0 .OR. ISALVS.EQ.SPS(I))THEN
          LINCL = .TRUE.
        ELSEIF(ISALVS.LT.0)THEN
          IGRP = -ISALVS
          IULIM = ISPGRP(IGRP,1)+1
          DO 90 IG=2,IULIM
          IF(SPS(I) .EQ. ISPGRP(IGRP,IG))THEN
            LINCL = .TRUE.
            GO TO 91
          ENDIF
   90     CONTINUE
        ENDIF
   91   CONTINUE

C         Skip this record if there are no snags in it.

          IF ((DENIS(I) + DENIH(I)) .LE. 0.0) GOTO 100
C
C         Skip the rest of the routine if none of these snags are eligible
C         for salvage.
C            (Note: OKSOFT: 0=all snags, 1=hard only, 2=soft only)
C
          IF(ISALVC.EQ.0 .AND. .NOT.LINCL)
     &      GO TO 100
          IF(ISALVC.EQ.1 .AND. LINCL)
     &      GO TO 100
          IF ( DENIH(I) .LE. 0.0 .AND. OKSOFT .EQ. 1 ) GOTO 100
          IF ( DENIS(I) .LE. 0.0 .AND. OKSOFT .EQ. 2 ) GOTO 100
          IF ( (IYR-YRDEAD(I)) .GT. MAXAGE .OR.
     &         DBHS(I) .GE. MAXDBH .OR.
     &         DBHS(I) .LT. MINDBH ) GOTO 100
c
c         COMPUTE VOLUME OF SOFT AND HARD SNAGS:
C           ISOFTV,IHARDV:   TOTAL VOLUME OF SNAGS
C           ISOFTV2,IHARDV2: MERCHANTABLE VOLUME OF SNAGS (FOR C HARV REPT.)
c
C           Note: call to FMSVL2 is changed so that routine appropriately
C                 recognizes original height of snags and calculates volume
C                 as if it is a top kill.
c
          XNEG1  = -1.0
          ISOFTV = 0.0
          ISOFTV2 = 0.0
          IF (DENIS(I) .GT. 0.0) THEN
            CALL FMSVOL(I, HTIS(I), ISOFTV,DEBUG,0)
            CALL FMSVL2(SPS(I),DBHS(I),HTDEAD(I),HTIS(I),ISOFTV2,
     >        LMERCH,DEBUG,JOSTND)
C            CALL FMSVL2(SPS(I),DBHS(I),HTIS(I),XNEG1,ISOFTV2,
C     >        LMERCH,DEBUG,JOSTND)
          ENDIF

          XNEG1  = -1.0
          IHARDV2 = 0.0
          IHARDV = 0.0
          IF (DENIH(I) .GT. 0.0) THEN
            CALL FMSVOL(I, HTIH(I), IHARDV,DEBUG,0)
            CALL FMSVL2(SPS(I),DBHS(I),HTDEAD(I),HTIH(I),IHARDV2,
     >        LMERCH,DEBUG,JOSTND)
c            CALL FMSVL2(SPS(I),DBHS(I),HTIH(I),XNEG1,IHARDV2,
c     >        LMERCH,DEBUG,JOSTND)
          ENDIF
C
C         Target some of the initially-hard snags, if there are any.
C         Note that these will either all still be hard or have all
C         gone soft. If they're all soft, only cut them if soft
C         snags are eligible for salvage.
C            (Note: OKSOFT: 0=all snags, 1=hard only, 2=soft only)
C
          CUTDIH = 0.0
          IF ( (DENIH(I) .GT. 0.0) .AND.
     &         ( (HARD(I) .AND. (OKSOFT .NE. 2)) .OR.
     &           ((.NOT. HARD(I)) .AND. (OKSOFT .NE. 1)) ))
     &      CUTDIH = PROP * DENIH(I)
C
C         Target some of the initially-soft snags, if there are any
C         and if soft snags are eligible.
C
          CUTDIS = 0.0
          IF ((DENIS(I) .GT. 0.0) .AND. (OKSOFT .NE. 1))
     &      CUTDIS = PROP * DENIS(I)
C
C         Remove the snags and increment the removal counters.
C
          DENIS(I) = DENIS(I) - CUTDIS
          DENIH(I) = DENIH(I) - CUTDIH
C
C         Set some common variables for use with the SalvVol EM function
C
          SALVSPA(I,1) = SALVSPA(I,1) + CUTDIH*(1.0 - PROPLV)
          SALVSPA(I,2) = SALVSPA(I,2) + CUTDIS*(1.0 - PROPLV)
C
          IF (DENIS(I) .LE. 0.0) DENIS(I) = 0.0
          IF (DENIH(I) .LE. 0.0) DENIH(I) = 0.0
C
C         INCREMENT CUT VOLUME AND REMOVED VOLUME
C
          CUTVOL = CUTVOL + (CUTDIS*ISOFTV + CUTDIH*IHARDV)
          SALVTPA = SALVTPA + CUTDIS + CUTDIH
          THISRM = THISRM + (CUTDIS*ISOFTV + CUTDIH*IHARDV)
     &              * (1.0 - PROPLV)
C
C         LEAVE BEHIND A PROPORTION OF THE SALVAGED SNAGS
C         AND PLACE THE MATERIAL IN THE CWD POOLS
C
          CALL CWD1(I, CUTDIH*PROPLV, CUTDIS*PROPLV)
C
C         THE REMAINDER IS MARKED AS REMOVED FOR REPORTING PURPOSES
C
          TONRMS = TONRMS +
     >       (CUTDIS * ISOFTV + CUTDIH * IHARDV) * V2T(SPS(I))
     >       * (1.0 - PROPLV)
C
C         ADD SALVAGE TO C-ACCOUNTING HARVEST POOLS
C         TOTAL SNAG VOLUME IS ALWAYS CALCULATED BY THE FFE METHOD;
C         NEVER WITH JENKINS, USING THE MERCHANTABILITY CRITERIA
C         APPROPRIATE TO THE EAST/WEST VARIANTS.

          X = (CUTDIS*ISOFTV2 + CUTDIH*IHARDV2) * V2T(SPS(I))
     >       * (1.0 - PROPLV)
          K = 1
          IF (BIOGRP(SPS(I)) .GT. 5) K = 2
          J = 1
          IF (DBHS(I) .GT. CDBRK(K))  J = 2
          FATE(J, K, ICYC) = FATE(J, K, ICYC) + X

  100   CONTINUE
C
C       RECORD THE VOLUME REMOVED FROM THE STAND FOR THE ACTIVITY SUMMARY
C
        PRMS(7) = THISRM
        CALL OPCHPR(JDO,7,PRMS)
        CALL OPDONE(JDO,IYR)

C
C       CALL SVSALV TO REMOVE SNAGS FROM THE BASE FVS SNAG ARRAYS, AND
C       DELETE THE ASSOCIATED SVS SNAG OBJECTS.
C
        CALL SVSALV(IYR,MINDBH,MAXDBH,MAXAGE,OKSOFT,PROP,PROPLV)


  200   CONTINUE

      ENDDO
C
C     Calculate CWDCUT based on the volume proportion
C     of cut snags
C
      IF (TOTVOL .GT. 0.0) CWDCUT = CWDCUT + CUTVOL / TOTVOL

      RETURN
      END
