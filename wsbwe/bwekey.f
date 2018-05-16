      SUBROUTINE BWEKEY(KEY,PASKEY)
      IMPLICIT NONE
C----------
C  **BWEKEY                 DATE OF LAST REVISION:  07/14/10
C----------
C
C     RETRIEVE A BUDWORM MODEL KEYWORD FROM THE KEYWORD TABLE.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1983
C
C     CALLED FROM :
C
C       OPLIST - PROGNOSIS ROUTINE WHICH PRINTS LIST OF SCHEDULED
C                OPTIONS.
C
C     PARAMETERS :
C
C     KEY - INDEX INTO TABLE.
C     PASKEY - TABLE (KEY)
C
C  Revision History:
C    17-MAY-2005 Lance R. David (FHTET)
C       Added FVS parameter file PRGPRM.F77.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C-----------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BWECOM.F77'
C
COMMONS
C
      CHARACTER*8 PASKEY
      INTEGER KEY
      PASKEY=TABLE(KEY)
      RETURN
      END
