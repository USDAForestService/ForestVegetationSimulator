      SUBROUTINE FMPPHV(IYR,CRBSHT,CRBLKD,CANCOV,KFMS,FMWTS,
     >                  POKL,POVK,FMLOADS)
      IMPLICIT NONE
C----------
C  **FMPPHV  FIRE--DATE OF LAST REVISION:  10/13/2009
C----------
C
C     THIS SUBROUTINE HAS NOT BEEN INTEGRATED INTO OPEN-FVS BASE CODE
C     IT IS CALLED BY PPE, WHICH MAY BE RETIRED.
C     IF IT IS RE-USED, POP-BACK CODE AFTER LINE 139 WILL NEED TO BE ENABLED,
C     ALONG WITH SEQUENTIAL POP-BACKS UP TO MAIN - DR/ESSA/OCT2012

C     THIS ROUTINE IS BASED ON FMMAIN. THE GOAL IS TO COMPUTE THE
C     VALUE OF THE ARGUMENTS (AFTER IYR) FOR YEAR IYR. IT NEEDS
C     TO DO THIS JOB WITHOUT CREATING ANY OUTPUT.
C
C     CALLED FROM -- HVPROJ (PPE)
C     CALLS: FMCBA
C            FMBURN
C            FMCADD
C            FMCWD
C            FMSNAG
C            FMSALV
C            FMTRET
C            FMUSRFM

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
      INTEGER I,IYR,IL,ISZ,IDC,ITM,IRC,IRTNCD
      INTEGER FMD,KFMS(4)
      REAL    CRBSHT,CRBLKD,CANCOV,TOPHT,FMWTS(4),FMLOADS(4),POKL,POVK,
     &        SALVTPA, X

      REAL POKILL,POVOLK
      COMMON /FMPPHV_SAVE/ POKILL,POVOLK

C     CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'FMPPHV',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
    7 FORMAT(' ENTERING FMPPHV CYCLE = ',I2,' LFMON=',L2)

C     RETURN IF THE FIRE MODEL IS NOT ACTIVE

      IF (.NOT. LFMON) THEN
         CRBSHT = 0.
         CRBLKD = 0.
         CANCOV = 0.
         POKL   = 0.
         POVK   = 0.
         DO I=1,4
            KFMS(I) = 0
            FMWTS(I)= 0.
            FMLOADS(I)= 0.
         ENDDO
         RETURN
      ENDIF

      IFMYR1 = IY(ICYC)
      IFMYR2 = IY(ICYC+1) - 1

      IF (DEBUG) WRITE(JOSTND,8) IFMYR1,IFMYR2,BURNYR,ITRN,IYR
    8 FORMAT(' IN FMPPHV: IFMYR1 IFMYR2 BURNYR ITRN IYR= ',5I5)

C     INITIALIZE THE CROWN RATIO IF ON THE FIRST YEAR
C     OF THE CYCLE.  THIS IS REQUIRED BECAUSE CUTS MAY HAVE
C     CHANGED THE CROWN RATIO IN SUPPORT OF THE PRUNE KEYWORD
C     AND REGENT MAY CHANGE IT IN THE NI VARIANT.
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

C     Initialize some Key variables. (R&C 07/09/96)

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

C     Calculate the dominant cover type (as basal area)
C     Note that in the single stand case, this only needs to be
C     done once per cycle unless a burn has occurred during the cycle

      CALL VARVER(VVER)

      IF (IYR .EQ. IFMYR1 .OR. BURNYR .EQ. IYR-1 .OR.
     & PBURNYR .EQ. IYR-1 .OR. (VVER(1:2) .EQ. 'SN')) CALL FMCBA (IYR,0)

C     Do stand management.  Right now, this is only salvaging snags.

      CALL FMSALV (IYR,SALVTPA)

C     Update conditon of existing snags for the current year.

      CALL FMSNAG (IYR, IY(1))

C     Update coarse woody debris pools

      CALL FMCWD(IYR)

C     Do fuel treatment (jackpot burns and pile burns).

      CALL FMTRET (IYR)

C     Check on user-specified fm definitions and
C     process any fueltret keywords.

      CALL FMUSRFM (IYR, FMD)

C     CALL THE ACTUAL FIRE ROUTINES (TO CALCULATE INTENSITY AND EFFECTS)

      CALL FMBURN (IYR, FMD, .FALSE.)
!      CALL fvsGetRtnCode(IRTNCD)
!      IF (IRTNCD.NE.0) RETURN

C     STORE THE FUEL MODELS, WEIGHTS, AND OTHER DATA TO RETURN

      DO I=1,4
         KFMS(I)=FMOD(I)
         FMWTS(I)=FWT(I)
      ENDDO
      CRBSHT=ACTCBH
      CRBLKD=CBD
      CANCOV=PERCOV
      POKL=POKILL
      POVK=POVOLK

C     Add this year's litterfall, crown breakage, and snag-crown-fall
C     to the CWD pools.

      CALL FMCADD

C     Copy CWD2B2 onto CWD2B (i.e., add debris from all snags
C     killed in the previous year to the pools of material
C     scheduled to fall in the year), and zero out CWD2B2.
C     (This used to be in FMSDIT and was moved so that it occurs
C     before any cuts that may occur next cycle.)

      DO ISZ = 0,5
         DO IDC = 1,4
            DO ITM = 1,TFMAX
               CWD2B(IDC,ISZ,ITM) = CWD2B(IDC,ISZ,ITM)
     &              + CWD2B2(IDC,ISZ,ITM)
               CWD2B2(IDC,ISZ,ITM) = 0.0
            ENDDO
         ENDDO
      ENDDO

      CALL FMEVCWD(X, 1, 1, IRC)
      IF (IRC.NE.0) THEN
         FMLOADS=0.
      ELSE
         CALL FMEVCWD(FMLOADS(1), 10, 10, IRC)
         FMLOADS(1)=FMLOADS(1)+X
         CALL FMEVCWD(FMLOADS(2), 2, 2, IRC)
         CALL FMEVCWD(FMLOADS(3), 3, 3, IRC)
         CALL FMEVCWD(FMLOADS(4), 4, 9, IRC)
      ENDIF

      RETURN
      END



