      SUBROUTINE VARGET (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     READ THE VARIANT SPECIFIC VARIABLES.
C
C     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C
C     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
C     IS MAXTRE (SEE PRGPRM).  
C
      INTEGER ILIMIT,IPNT,MXL,MXI,MXR
      PARAMETER (MXL=1,MXI=1,MXR=1)
      LOGICAL LOGICS(*)
      REAL WK3(MAXTRE)
      INTEGER INTS(*)
      REAL REALS(*)
      LOGICAL LDANUW
      REAL DANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = REAL(ILIMIT)
      DANUW = REAL(INTS(1))
      DANUW = REAL(IPNT)
      LDANUW = LOGICS(1)
      DANUW = REALS(1)
      DANUW = WK3(1)
C
      RETURN
      END

      SUBROUTINE VARCHGET (CBUFF, IPNT, LNCBUF)
      IMPLICIT NONE
C----------
C     Get variant-specific character data
C----------

      INCLUDE 'PRGPRM.F77'

      INTEGER LNCBUF
      CHARACTER CBUFF(LNCBUF)
      CHARACTER CDANUW
      REAL DANUW
      INTEGER IPNT
      ! Stub for variants which need to get/put character data
      ! See /bc/varget.f and /bc/varput.f for examples of VARCHGET and VARCHPUT
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW = CBUFF(1)
      DANUW = REAL(IPNT)
      DANUW = REAL(LNCBUF)
C
      RETURN
      END
