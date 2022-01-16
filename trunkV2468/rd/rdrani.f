      SUBROUTINE RDRANI(SSEED)
      IMPLICIT NONE
C----------
C  **RDRANI      LAST REVISION:  09/02/14
C----------
C  THIS SUBROUTINE INITIALIZES THE RANDOM NUMBER GENERATOR FOR THE
C  ROOT DISEASE MODEL.
C
C  CALLED BY :
C     RDMN1   [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C
C  PARAMETERS :
C     SSEED - SEED VALUE FOR THE ROOT DISEASE RANDOM NUMBER GENERATOR.
C
C  Revision History :
C   11/06/89 - Last revision date.
C   09/02/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMON
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDADD.F77'
C
COMMONS
C
      INTEGER  ISEED
      REAL     SSEED
      
      ISEED = INT(SSEED)
      IF (MOD(ISEED,2) .EQ. 0) ISEED = ISEED + 1
      SS = FLOAT(ISEED)
      S0 = SS

      RETURN
      END
