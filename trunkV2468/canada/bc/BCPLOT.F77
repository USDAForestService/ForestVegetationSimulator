CODE SEGMENT BCPLOT
C----------
C  $Id$
C----------
C  DEFINITIONS OF VARIABLES IN 'BCPLOT' COMMON BLOCK:
C----------
C
C     INDEXING CONSTANTS FOR FD & PL MORTALITY MODEL
C
      INTEGER   MRT_ICH,MRT_IDF
      INTEGER   MRT_PW, MRT_LW, MRT_FD, MRT_BG, MRT_HW
      INTEGER   MRT_CW, MRT_PL, MRT_SE, MRT_PY
      INTEGER   MRT_BA_UB,MRT_DBH_UB

      PARAMETER (MRT_ICH =  1, MRT_IDF = 2)
      PARAMETER (MRT_PW  =  1, MRT_LW  = 2, MRT_FD  = 3)
      PARAMETER (MRT_BG  =  4, MRT_HW  = 5, MRT_CW  = 6)
      PARAMETER (MRT_PL  =  7, MRT_SE  = 8, MRT_PY  = 9)
      PARAMETER (MRT_BA_UB = 5, MRT_DBH_UB = 4)
C
C     STRUCTURE FOR THE BEC INFORMATION
C
      TYPE BEC_DESCRIPTION 
        SEQUENCE
        CHARACTER (LEN=3)  :: Region     !KAM  - longest strings
        CHARACTER (LEN=4)  :: Zone       !ESSF - are shown here
        CHARACTER (LEN=3)  :: SubZone    !mw2
        CHARACTER (LEN=5)  :: Series     !01-YC
        CHARACTER (LEN=7)  :: FullName   !ICHmw2
        CHARACTER (LEN=15) :: PrettyName !IDFmw1/01-YC
      END TYPE
C
C     STRUCTURE FOR THE FITTED MORTALITY MODELS.
C
      TYPE MRT_DESCRIPTION
        SEQUENCE
        CHARACTER (LEN=15)  :: PrettyName    ! BEC to site series
        CHARACTER (LEN=1)   :: pad           ! stop alignment warning
        LOGICAL             :: FIT(MAXSP)    ! TRUE=fitted
        REAL                :: CON(MAXSP)    ! constant
        REAL                :: INVDBH(MAXSP) ! 1./diameter (cm**-1)
        REAL                :: DBH(MAXSP)    ! diameter (cm)
        REAL                :: DBHSQ(MAXSP)  ! diameter (cm**2)
        REAL                :: RDBH(MAXSP)   ! relative diameter (cm/cm)
        REAL                :: BAL(MAXSP)    ! BAL (m**2/ha)
        REAL                :: SQRTBA(MAXSP) ! SQRT((m**2/ha)**0.5)
        REAL                :: SPH(MAXSP)    ! stems/ha (all trees)
      END TYPE
C
C     STRUCTURE FOR THE FITTED LARGE TREE HEIGHT GROWTH MODELS.
C
      TYPE LTH_DESCRIPTION
        SEQUENCE
        CHARACTER (LEN=15)  :: PrettyName   ! BEC to site series
        CHARACTER (LEN=1)   :: pad          ! stop alignment warning
        LOGICAL             :: FIT(MAXSP)   ! TRUE=fitted
        REAL                :: MLT(MAXSP)   ! mult (READCORH keywd)
        REAL                :: A1(MAXSP)    ! species constant
        REAL                :: B1(MAXSP)    ! 
        REAL                :: B2(MAXSP)    ! 
        REAL                :: SI(MAXSP)    ! site constant
        REAL                :: B3(MAXSP)    ! 
        REAL                :: C(MAXSP)     ! 
C-----------------------------------------------------
        REAL                :: CN(MAXSP)    ! new structure
        REAL                :: CNSI(MAXSP)  !
        REAL                :: DG(MAXSP)    !
        REAL                :: DG2(MAXSP)   ! 
        REAL                :: DBH1(MAXSP)  !
        REAL                :: DBH2(MAXSP)  ! 
        REAL                :: HT2(MAXSP)   ! 
      END TYPE
C
C     STRUCTURE FOR THE SMALL TREE GROWTH MODELS.
C
      TYPE STG_DESCRIPTION
        SEQUENCE
        CHARACTER (LEN=15)  :: PrettyName    ! BEC to site series
        CHARACTER (LEN=1)   :: pad           ! stop alignment warning
        LOGICAL             :: FIT(MAXSP)    ! TRUE=fitted
        REAL                :: CON(MAXSP)    ! spp,SI,site constants
        REAL                :: LNHT(MAXSP)   ! ln(ht) - ln(m)
        REAL                :: HT(MAXSP)     ! ht - m
        REAL                :: CCF(MAXSP)    ! ccf
        REAL                :: BAL(MAXSP)    ! BA larger trees - m**2ha
        REAL                :: EXPMLT(MAXSP) ! Mutliplier  !SBS only
        REAL                :: HCCFL(MAXSP)  ! HT/CCF      !SBS only
        REAL                :: CCFSL(MAXSP)  ! CCFS/CCFL   !SBS only
        REAL                :: SD(MAXSP)     ! standard deviation
      END TYPE

      TYPE (BEC_DESCRIPTION) BEC
      TYPE (MRT_DESCRIPTION) MORT
      TYPE (LTH_DESCRIPTION) LTH
      TYPE (STG_DESCRIPTION) STG

C     TMRT -- MORTALITY MODEL FOR FD & PL

      REAL TMRT(MRT_PY,MRT_BA_UB,MRT_DBH_UB)

C     LARGE TREE DG PARMS (NOT ALREADY IN BASE COMMON BLOCKS)
C
C     DGLD1   - LOG DBH COEFFS (NAME MODIFIED FROM NI)
C     DGBAL1  - BAL COEFFS     (NAME MODIFIED FROM NI)
C     DGCR1   - CR COEFFS      (NAME MODIFIED FROM NI)
C     DGDBAL1 - DBH/BAL COEFFS
C     DGDBAL2 - DBH/BAL COEFFS
C     DGCCF1  - CCF COEFFS     (NAME MODIFIED FROM NI)
C     DGDSQ1  - DSQ COEFFS     (NAME MODIFIED FROM NI)

      REAL        DGLD1(MAXSP), DGBAL1(MAXSP), DGCR1(MAXSP)
      REAL        DGDBAL1(MAXSP), DGDBAL2(MAXSP),DGCCF1(MAXSP)
      REAL        DGDSQ1(MAXSP)

C     ** CROWN PARMS ** (NOT ALREADY IN BASE COMMON BLOCKS)
C     CRHTDBH - HT/DBH COEFFS
C     CRHT    - HT COEFFS
C     CRDBH2  - DBH**2 COEFFS
C     CRBAL   - BAL COEFFS
C     CRLNCCF - LN(CCF) COEFFS
C     CRSD    - STD DEV

      REAL        CRHTDBH(MAXSP),CRHT(MAXSP),CRDBH2(MAXSP),CRBAL(MAXSP)
      REAL        CRLNCCF(MAXSP),CRSD(MAXSP)
      
      COMMON /BCPLT1 /   BEC
      COMMON /BCPLT2 /   MORT,LTH,STG,TMRT
      COMMON /DGCOEFFS / DGLD1,DGBAL1,DGCR1,DGDBAL1,DGDBAL2,DGCCF1,
     >					 DGDSQ1
      COMMON /CRCOEFFS / CRHTDBH,CRHT,CRDBH2,CRBAL,CRLNCCF,CRSD
     
C	VERSION 2 RE-MERGE: COMMON BLOCKS FOR INTERIM CALIBRATION OF MS,PP,ESSF
C	SOME V2 VARIABLES HAVE PERSISTED IN V3
C
C   LV2ATV - .TRUE. IF VERSION 2 INTERIM CALIBRATION IS ACTIVE (BECSET)
C   LV2HTCP - .TRUE. IF FIRST STAND HAS BEEN READ: CONTROLS COPYING OF ARCHIVE
C             HEIGHT PARAMETERS
C   LV2HDR - .TRUE. IF VERSION 2 CALIBRATION INFO HAS BEEN PROCESSED & WRITTEN TO MAIN OUTPUT
C   XHT1B  - ARCHIVE COPY OF BLKDAT HT1 ARRAY
C   XHT2B  - ARCHIVE COPY OF BLKDAT HT2 ARRAY
C   LMHTDUB - .TRUE. IF METRIC-BASED HT-DUBBING VALUES ARE IN USE (V2 & V3)
C   LLTDGOK - .TRUE. IF SPP CAN GROW IN V2; FALSE OTHERWISE
C   SEICON  - BEC CONSTANTS  (V2)
C   V2SEICOR - CORRECTION TERMS (V2)

      LOGICAL     LV2ATV,LV2HTCP,LV2HDR
      LOGICAL     LLTDGOK(MAXSP),LMHTDUB(MAXSP)
      REAL        SEISHT
      REAL        V2SEICOR(MAXSP),SEICON(MAXSP)
      REAL        XHT1B(MAXSP),XHT2B(MAXSP)

      COMMON /V2PLOT/ V2SEICOR,SEICON,LV2ATV,LV2HDR,LLTDGOK,SEISHT
      COMMON /V23DUB/ XHT1B,XHT2B,LV2HTCP,LMHTDUB
