      SUBROUTINE VARPUT (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
      IMPLICIT NONE
C----------
C PG $Id$
C----------
C
C     WRITE THE VARIANT SPECIFIC VARIABLES.
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
      REAL RDANUW
      LOGICAL LDANUW
      INTEGER IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = ILIMIT
      IDANUW = INTS(1)
      IDANUW = IPNT
      LDANUW = LOGICS(1)
      RDANUW = REALS(1)
      RDANUW = WK3(1)
C
      RETURN
      END

      SUBROUTINE VARCHPUT (CBUFF, IPNT, LNCBUF)
      IMPLICIT NONE
C----------
C     Put variant-specific character data
C----------

      INCLUDE 'PRGPRM.F77'

      INTEGER LNCBUF
      CHARACTER CBUFF(LNCBUF)
      CHARACTER CDANUW
      INTEGER IDANUW
      INTEGER IPNT
      ! Stub for variants which need to get/put character data
      ! See /bc/varget.f and /bc/varput.f for examples of VARCHGET and VARCHPUT
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW = CBUFF(1)
      IDANUW = IPNT
      IDANUW = LNCBUF
C
      RETURN
      END
