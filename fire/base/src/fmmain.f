      SUBROUTINE FMMAIN
      IMPLICIT NONE
C----------
C  $Id$
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
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      LOGICAL DEBUG
      CHARACTER VVER*7
      INTEGER I,IYR,IL,ISZ,IDC,ITM,IRTNCD
      INTEGER FMD
C     Variables that support the use of FMORTMLT       
      INTEGER  MYACTS(1),NTODO,ITODO,NPRM,IACT,IDSP
      REAL     PRMS(4)
      DATA     MYACTS/2554/

C     CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'FMMAIN',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,6) ICYC,LFMON
    6 FORMAT(' ENTERING FMMAIN CYCLE = ',I2,' LFMON=',L2)

C     RETURN IF THE FIRE MODEL IS NOT ACTIVE

      IF (.NOT. LFMON) RETURN

C     SET EVENT MONITOR VARIABLES (FROM **EVTSTV**)
C     420 FIRE 0 IF STAND HAS NO FIRE, 1 IF FIRE OCCURS (FM)

      CALL EVSET4(20, 0.0)
      LFIRE=.FALSE.

C     Calculate the number of years in this cycle so that the decomposition
C     rates can be adjusted correctly for variable cycle lengths. 
C     This is necessary as we move from the FFE working on annual timesteps
C     to cycle timesteps. (note: this value is the same as IFINT)

      NYRS = IY(ICYC+1) - IY(ICYC)     
C
C     Loop over the years within the cycle
C
      IFMYR1 = IY(ICYC)
      IFMYR2 = IY(ICYC+1) - 1
      IF (DEBUG) WRITE(JOSTND,7) IFMYR1,IFMYR2, BURNYR, ITRN
    7 FORMAT(' IN FMMAIN IFMYR1 IFMYR2 BURNYR ITRN= ',5I5)
    
C     Process FMORTMLT

      FMORTMLT = 1.
      CALL OPFIND(1,MYACTS,NTODO)
      IF (NTODO.GT.0) THEN
        DO ITODO=1,NTODO
          CALL OPGET(ITODO,4,IDSP,IACT,NPRM,PRMS)
          CALL OPDONE(ITODO,IY(ICYC))
          IDSP = IFIX(PRMS(2))
          DO I=1,ITRN
            IF (IDSP .NE. 0 .AND. ISP(I) .NE. IDSP) CYCLE
            IF (DBH(I).GE.PRMS(3) .AND. DBH(I).LT.PRMS(4)) 
     >         FMORTMLT(I) = PRMS(1)
          ENDDO
          IF (DEBUG) WRITE(JOSTND,8) PRMS(1),IDSP,PRMS(3),PRMS(4)
    8     FORMAT(' FMORTMLT SET TO',F10.4,' FOR SPECIES I=',I3,
     >      ' MIND=',F6.2,' MAXD=',F7.1)
        ENDDO
      ENDIF

C REMOVE THE LOOP FOR RUNNING THIS JUST ON CYCLE BOUNDARIES...
C     and set IYR to be the cycle year.
      IYR = IFMYR1
C      DO IYR = IFMYR1,IFMYR2

         IF (DEBUG) WRITE(JOSTND,9) IYR,BURNYR
    9    FORMAT(' IN FMMAIN IYR BURNYR= ',2I5)

C        INITIALIZE THE CROWN RATIO IF ON THE FIRST YEAR
C        OF THE CYCLE.  THIS IS REQUIRED BECAUSE CUTS MAY HAVE
C        CHANGED THE CROWN RATIO IN SUPPORT OF THE PRUNE KEYWORD
C        AND REGENT MAY CHANGE IT IN THE NI VARIANT.
C         NOTE: cycle-boundary version no longer needs to check year
C
C         IF (IYR.EQ.IFMYR1) THEN
            DO I=1,ITRN
               FMPROB(I) = PROB(I)
               FMICR(I)  = ICR(I)
               FIRKIL(I) = 0.0
            ENDDO
C         ELSE
C            TONRMS=0.0
C            TONRMH=0.0
C            TONRMC=0.0
C         ENDIF

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
C         NOTE: cycle-boundary version will no longer need to check
C               version or year as this will be called every cycle.

C         CALL VARVER(VVER)

C         IF (IYR .EQ. IFMYR1 .OR. BURNYR .EQ. IYR-1 .OR.
C     &   PBURNYR .EQ. IYR-1 .OR. (VVER(1:2) .EQ. 'SN')) THEN
           CALL FMCBA (IYR,0)
C         ENDIF
         
C        This resets the value of cwdcut, which is based on salvage
C        removal, back to zero in all but the first year of a cycle.
C        This is necessary since the call to fmsalv was moved to fmsdit,
C        which is only called at the beginning of each cycle.  since
C        salvage cuts now only occur on cycle breaks, cwdcut should
C        always be zero in all other years.
C         NOTE: cycle-bouncary version no longer needs this, since
C                 IYR=IFMYR1 always

C         IF (IYR .NE. IFMYR1) CWDCUT = 0.
C         
C     END OF INITIALIZATION PART OF ROUTINE
C
C     DO VARIOUS ACTIVITIES AND TREATMENTS
C
C        Do fuel treatment (jackpot burns and pile burns).

         CALL FMTRET (IYR)

C        Do FuelMove keyword (formerly in FMCWD)

         CALL FMFMOV(IYR)
         
C        Check on user-specified fm definitions and
C        process any fueltret keywords.

         CALL FMUSRFM (IYR, FMD)
         
C        Simulate actual fires
         CALL FMBURN (IYR, FMD, .TRUE.)

C         
C     PRINT ALL OUTPUT FILES
C
C        Print out the current snag list (if requested)

         CALL FMSOUT (IYR)
         CALL FMSSUM (IYR)

C        Potential fire report
         
C----------
C  CALL FMPOCR SO THE CANOPY FUELS PROFILE TABLE IS PRINTED 
C  AT THE CORRECT TIME.
C  CALL NEW ROUTINE TO LOAD INFORMATION FOR CALCULATING THE 
C  FUEL MODEL VARIABLES, BUT ONLY CALL THIS TIME IF A FIRE OCCURRED.
C----------
         CALL FMPOCR(IYR,2)
         IF (BURNYR .EQ. IYR) THEN
            CALL FMCFMD3(IYR, FMD)   
         ENDIF

C----------
C  CALCULATE AND PRINT THE POTENTIAL FLAME LENGTH REPORT
C----------
         CALL FMPOFL (IYR, FMD, .TRUE.)
         CALL fvsGetRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN

C        Print the stand-level fuel output table

         CALL FMDOUT (IYR)

C        Print the stand-level main carbon report

         CALL FMCRBOUT (IYR)
C
C     TREE BOIMASS COMPONENT OUTPUT
C
         IF(IFMBMCMP.GT.0)CALL DBSFMBMCMP(IY(ICYC))

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

C         
C        UPDATE SNAG AND CWD POOLS
C
C        temporarlily reset teh value of NYRS to 1 so that we don't need to change the code again
         NYRS = 1 
         
         DO IYR = IFMYR1,IFMYR2
      
C          Update conditon of existing snags for the current year.

           CALL FMSNAG (IYR, IY(1))

C          Update coarse woody debris pools

           CALL FMCWD(IYR)

C          Add this year's litterfall, crown breakage, and snag-crown-fall
C          to the CWD pools.

           CALL FMCADD

C          Copy CWD2B2 onto CWD2B (i.e., add debris from all snags
C          killed in the previous year to the pools of material
C          scheduled to fall in the upcoming years), and zero out CWD2B2.
C          (This used to be in FMSDIT and was moved so that it occurs
C          before any cuts that may occur next cycle.)

           DO ISZ = 0,5
              DO IDC = 1,4
                 DO ITM = 1,TFMAX
                    CWD2B(IDC,ISZ,ITM) = CWD2B(IDC,ISZ,ITM)
     &                   + CWD2B2(IDC,ISZ,ITM)
                    CWD2B2(IDC,ISZ,ITM) = 0.0
                 ENDDO
              ENDDO
           ENDDO
                
         ENDDO

C        change NYRS back to its original value        
         NYRS = IY(ICYC+1) - IY(ICYC)

C        In the last year of each cycle, record some information about
C        crown size for use in determining litterfall in the next cycle.

C        IF (IYR .EQ. IFMYR2) CALL FMOLDC
         CALL FMOLDC

      CALL FMSVSYNC

      RETURN
      END