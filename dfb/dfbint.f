      SUBROUTINE DFBINT
      IMPLICIT NONE
C----------
C  **DFBINT--BASE  DATE OF LAST REVISION:  06/30/10
C----------
C
C  INITIALIZES DFB VARIABLES AND OPENS FILES.
C
C  CALLED BY :
C     INITRE   [PROGNOSIS]
C
C  CALLS :
C     MYOPEN  (SUBROUTINE)  [PROGNOSIS]
C
C  LOCAL VARIABLES :
C     I      - COUNTER INDEX.
C     KODE   - ERROR CODE SENT BACK FROM SUBROUTINE MYOPEN.
C
C  COMMON BLOCK VARIABLES USED :
C     DBEVNT - (DFBCOM)  OUTPUT
C     DEBUIN - (DFBCOM)  OUTPUT
C     EPIPRB - (DFBCOM)  OUTPUT
C     EXPCTD - (DFBCOM)  OUTPUT
C     EXSTDV - (DFBCOM)  OUTPUT
C     IDBSCH - (DFBCOM)  OUTPUT
C     IDFBYR - (DFBCOM)  OUTPUT
C     ILENTH - (DFBCOM)  OUTPUT
C     IPAST  - (DFBCOM)  OUTPUT
C     ISMETH - (DFBCOM)  OUTPUT
C     IWAIT  - (DFBCOM)  OUTPUT
C     JODFB  - (DFBCOM)  OUTPUT
C     JODFBX - (DFBCOM)  OUTPUT
C     LBAMOD - (DFBCOM)  OUTPUT
C     LDFBON - (DFBCOM)  OUTPUT
C     LEPI   - (DFBCOM)  OUTPUT
C     LINPRG - (DFBCOM)  OUTPUT
C     LINV   - (DFBCOM)  OUTPUT
C     MINDEN - (DFBCOM)  OUTPUT
C     MWINHT - (DFBCOM)  OUTPUT
C     NDAMS  - (DFBCOM)  OUTPUT
C     OKILL  - (DFBCOM)  OUTPUT
C     ORSEED - (DFBCOM)  OUTPUT
C     PRPWIN - (DFBCOM)  OUTPUT
C     WORKIN - (DFBCOM)  OUTPUT
C
C REVISION HISTORY:
C   22-MAR-2002 Lance R. David (FHTET)
C     Added variable LSET and call to DFNSD to (re)set random number generator.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'DFBCOM.F77'
C
C
COMMONS
C

      INTEGER I, KODE
      LOGICAL LSET

      DO 100 I = 1,40
         WORKIN(I) = .FALSE.
         IDFBYR(I) = 0
  100 CONTINUE
      IDFBYR(41) = 0

      DEBUIN = .FALSE.
      LBAMOD = .FALSE.
      LDFBON = .FALSE.
      LEPI   = .FALSE.
      LINPRG = .FALSE.
      LINV   = .FALSE.

      DBEVNT = 0.05
      EPIPRB = 0.5
      EXPCTD = 6.0
      EXSTDV = 2.0
      IDBSCH = 2
      ILENTH = 4
      IPAST  = 1950
      ISMETH = 2
      IWAIT  = 10
      JODFB  = 19
      JODFBX = 0
      MINDEN = 0.0
      MWINHT = 20.0
      NDAMS  = 0
      OKILL  = 0.0
      ORSEED = 55329.0
      PRPWIN = 0.8

C.... Initialized random number generator with default seed value.

      LSET = .TRUE.
      CALL DFBNSD (LSET, ORSEED)

C.... OPEN THE SUMMARY OUTPUT FILE FOR DFB.

      CALL MYOPEN (JODFB, ' ', 4, 133, 0, 1, 1, 0, KODE)

      RETURN
      END
