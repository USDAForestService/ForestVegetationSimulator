      SUBROUTINE MPBINT
      IMPLICIT NONE
C----------
C  **MPBINT        DATE OF LAST REVISION:  07/02/10
C----------
C
C     INITIALIZES MPB VARIABLES.  CALLED FROM INITRE
C
C
C Revision History
C   09/26/91 Last noted revision date.
C   04/19/02 Lance R. David (FHTET)
C     Random number generator initialization with default seed value.
C     Added local variables LSET and MPSEED.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
      INCLUDE 'COLCOM.F77'
C
C
      INCLUDE 'MPBETA.F77'
C
C
COMMONS
C
      LOGICAL LSET
      INTEGER I, INC, INC1, KK, KODE
      REAL XPRN(10), XZIN(10), MPSEED, PRCENT

      REAL ADEN, AMPAVE, APD, B1INC, B3ALL, B3INC, BATK, BATKI,
     &     BNEW, BYINC, BYT, E1, E2, E3, EARAT, EDEN, EF3, EFFS,
     &     EGGS, FMX, GEX, GFML1, GFML2, GFML3, GTEB,
     &     PCTGEM, PCTGEX, PCTGF1, PCTGF2, PCTGF3, PCTGSV, PSE1,
     &     PSE3, PSPE1, PSPE3, PSURV, RHO1, RHO2, RHO3, SKILL,
     &     SS, TAGG, TMMPB, TRKILL, XEG, XXG, YOUNG

      DATA XZIN/0.0000,0.0000,0.0038,0.0128,0.0206,0.0353,0.0500,
     >          0.1429,0.1500,0.1500/
C
C     NORMAL Q VALUES (COLES ORIGINALS)
C
      DATA XPRN/2 * 1.0,0.9935,0.982,0.965,0.909,0.743,0.309,0.285,
     >          0.285/
C
C     HIGH Q VALUES (SLOW MORTALITY).
C
C*    DATA PRNOIN/2 * 1.0,0.9970,0.990,0.985,0.935,0.850,0.455,0.325,
C*   >           0.300/
C
C     LOW Q VALUES (FAST MORTALITY).
C
C*    DATA PRNOIN/1.,0.998,0.990,0.960,0.900,0.700,0.600,0.285,0.250,
C*   >           0.200/
C
C
      EPS  = 1.0D-6
      BMIN = 1.0D-10

      DO 100 I = 1,40
         WORKIN(I) = .FALSE.
  100 CONTINUE

      LSADLP = .FALSE.
      LRANST = .FALSE.
      LMPB1  = .FALSE.
      LEPI   = .FALSE.
      LAGG   = .FALSE.
      LPS    = .FALSE.
      LREP   = .FALSE.
      LDC    = .FALSE.
      LPOPDY = .FALSE.
      LDAM   = .TRUE.
      LCRES  = .TRUE.
      MPBGRF = .TRUE.
      DEBUIN = .FALSE.
      LCURMR = .FALSE.
      LINVMR = .FALSE.

      JOMPBX = 0
      SADLPP = 0.0
      INT    = 10
      BETTER(1) = 1.0
      BETTER(2) = 4.0
      MPBYR  = 0
      NCLASS = 10
      AMP1   = 1200.0
      AMP2   = 600.0
      IBACK  = 4
      CE     = 1.0
      CF1    = 0.01
      CF2    = 0.01
      CF3    = 0.5
      CRITAD = 1.5
      NG     = 2
      TAFAC  = 1.7
      IBOUSE = 0
      HS     = 1.0
      MPMXYR = 10
      DST(1) = 3000.0
      DST(2) = 500.0
      DST(3) = 500.0
      STRP   = 0.95
      NDAMS  = 0
      MPBON  = 0
      PRCENT = 7.0
      PCTCO  = 65.0
      FORLAT = 44.0
      NEPIYR = 0
      TAMIN  = 1.7
      TAMAX  = 3.0
      TATOL  = 0.2
      EPIPRB = 0.5
      PRBSCL = 1.0
      STRBUG = 500.0
      EXCON  = 640.0
      ISTDT = 1
 
      DO 200 I = 1,30
         ACTSRF(I) = 0.0
         AGGPH(I)  = 0.9
         REPL(I)   = 0.9
         PSPK(I)   = 0.0
         PSDL(I)   = 10.0
         PSPF(I)   = 0.0
         DCPF(I)   = 0.0
         DCPK(I)   = 0.0
  200 CONTINUE

      DO 300 I = 1,10
         ZINMOR(I) = XZIN(I)
         PRNOIN(I) = XPRN(I)
         CURRMR(I) = 0.0
  300 CONTINUE

C
C     OPEN DISK FILES FOR THE MPB ROUTINES.
C
      CALL MYOPEN (20, ' ', 4, 133, 0, 2, 1, 0, KODE)
      CALL MYOPEN (JOMPB, ' ', 4, 133, 0, 1, 1, 0, KODE)

C
C     CALL EVUST4 TO SET VALUE FOR MPBTPAK TO UNDEFINED.
C
      CALL EVUST4(4)

C.... Initialized random number generator with default seed value.

      MPSEED = 55329.0
      LSET   = .TRUE.
      CALL MPRNSD (LSET, MPSEED)

      RETURN
      END
