      SUBROUTINE FMMAIN
      IMPLICIT NONE
C----------
C  **FMMAIN  FIRE--DATE OF LAST REVISION:  10/13/09
C----------
C
C     THIS ROUTINE IS THE 'MAIN' FIRE ROUTINE. IT LOOPS OVER
C     THE YEARS WITHIN A CYCLE, AND LOOPS OVER EACH STAND, EACH
C     YEAR. IT CALLS MANY OF THE DIFFERENT PARTS OF THE FIRE MODEL.
C     FMD IS DOMINANT FUEL MODEL.
C
C     CALLED FROM -- GRADD  (PPE AND SINGLE-STAND)
C     CALLS: EVSET4
C            FMCBA
C            FMBURN
C            FMCADD
C            FMCWD
C            FMDOUT
C            FMGSTD
C            FMSNAG
C            FMSOUT
C            FMSSUM
C            FMTRET
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
C
COMMONS
C
      LOGICAL DEBUG
      CHARACTER VVER*7
      INTEGER I,IYR,IL,ISZ,IDC,ITM,IRTNCD
      INTEGER FMD

C     CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'FMMAIN',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
    7 FORMAT(' ENTERING FMMAIN CYCLE = ',I2,' LFMON=',L2)

C     RETURN IF THE FIRE MODEL IS NOT ACTIVE

      IF (.NOT. LFMON) RETURN

C     SET EVENT MONITOR VARIABLES (FROM **EVTSTV**)
C     420 FIRE 0 IF STAND HAS NO FIRE, 1 IF FIRE OCCURS (FM)

      CALL EVSET4(20, 0.0)
      LFIRE=.FALSE.
C
C     Loop over the years within the cycle
C
      IFMYR1 = IY(ICYC)
      IFMYR2 = IY(ICYC+1) - 1
      IF (DEBUG) WRITE(JOSTND,8) IFMYR1,IFMYR2, BURNYR, ITRN
    8 FORMAT(' IN FMMAIN IFMYR1 IFMYR2 BURNYR ITRN= ',5I5)

      DO IYR = IFMYR1,IFMYR2

         IF (DEBUG) WRITE(JOSTND,9) IYR,BURNYR
    9    FORMAT(' IN FMMAIN IYR BURNYR= ',2I5)

C        INITIALIZE THE CROWN RATIO IF ON THE FIRST YEAR
C        OF THE CYCLE.  THIS IS REQUIRED BECAUSE CUTS MAY HAVE
C        CHANGED THE CROWN RATIO IN SUPPORT OF THE PRUNE KEYWORD
C        AND REGENT MAY CHANGE IT IN THE NI VARIANT.
C
         IF (IYR.EQ.IFMYR1) THEN
            DO I=1,ITRN
               FMPROB(I) = PROB(I)
               FMICR(I)  = ICR(I)
               FIRKIL(I) = 0.0
            ENDDO
         ELSE
            TONRMS=0.0
            TONRMH=0.0
            TONRMC=0.0
         ENDIF

C        Initialize some Key variables. (R&C 07/09/96)

         SMOKE(1) = 0.0
         SMOKE(2) = 0.0
         CRBURN = 0.0
         BURNCR = 0.0
         PBRNCR = 0.0
         DO IL = 1,MXFLCL
            BURNED(1,IL) = 0.0
            BURNED(2,IL) = 0.0
            BURNED(3,IL) = 0.0
         ENDDO
         BURNLV(1) = 0.0
         BURNLV(2) = 0.0

C        Calculate the dominant cover type (as basal area)
C        Note that in the single stand case, this only needs to be
C        done once per cycle unless a burn has occurred during the cycle
C        (if the PPE version will read and write COVTYP and PERCOV then
C        that version will also need to only read it once too).

         CALL VARVER(VVER)

         IF (IYR .EQ. IFMYR1 .OR. BURNYR .EQ. IYR-1 .OR.
     &   PBURNYR .EQ. IYR-1 .OR. (VVER(1:2) .EQ. 'SN')) THEN
           CALL FMCBA (IYR,0)
         ENDIF
         
C        This resets the value of cwdcut, which is based on salvage
C        removal, back to zero in all but the first year of a cycle.
C        This is necessary since the call to fmsalv was moved to fmsdit,
C        which is only called at the beginning of each cycle.  since
C        salvage cuts now only occur on cycle breaks, cwdcut should
C        always be zero in all other years.

         IF (IYR .NE. IFMYR1) CWDCUT = 0.

C        Print out the current snag list (if requested)

         CALL FMSOUT (IYR)
         CALL FMSSUM (IYR)

C        Update conditon of existing snags for the current year.

         CALL FMSNAG (IYR, IY(1))

C        Do fuel treatment (jackpot burns and pile burns).

         CALL FMTRET (IYR)

C        Update coarse woody debris pools

         CALL FMCWD(IYR)

C        Check on user-specified fm definitions and
C        process any fueltret keywords.

         CALL FMUSRFM (IYR, FMD)

C        CALL THE ACTUAL FIRE ROUTINES (TO CALCULATE INTENSITY AND EFFECTS)

         CALL FMBURN (IYR, FMD, .TRUE.)
         CALL getfvsRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN          

C        Add this year's litterfall, crown breakage, and snag-crown-fall
C        to the CWD pools.

         CALL FMCADD

C        Print the stand-level fuel output table

         CALL FMDOUT (IYR)

C        Print the stand-level main carbon report

         CALL FMCRBOUT (IYR)

C        Print the stand-level harvest products carbon report

         CALL FMCHRVOUT (IYR)

C        If this is the first year, call evtstv to compute user-defined
C        variables. there will not be any to compute unless some are a
C        function of previously undefined variables. We set all the fire-
C        related variables to undefined so any compute expressions that
C        are functions of them will be undefined...this call attempts to
C        compute them after they have been defined. There may be unintended
C        side effects of this code!

         CALL EVTSTV (iyr)

C        Copy CWD2B2 onto CWD2B (i.e., add debris from all snags
C        killed in the previous year to the pools of material
C        scheduled to fall in the year), and zero out CWD2B2.
C        (This used to be in FMSDIT and was moved so that it occurs
C        before any cuts that may occur next cycle.)

         DO ISZ = 0,5
            DO IDC = 1,4
               DO ITM = 1,TFMAX
                  CWD2B(IDC,ISZ,ITM) = CWD2B(IDC,ISZ,ITM)
     &                 + CWD2B2(IDC,ISZ,ITM)
                  CWD2B2(IDC,ISZ,ITM) = 0.0
               ENDDO
            ENDDO
         ENDDO

C        In the last year of each cycle, record some information about
C        crown size for use in determining litterfall in the next cycle.

         IF (IYR .EQ. IFMYR2) CALL FMOLDC

      ENDDO

      CALL FMSVSYNC

      RETURN
      END



