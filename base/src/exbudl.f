      SUBROUTINE EXBUDL
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C     THE PURPOSE OF THIS SUBROUTINE IS TO SATISFY EXTRA EXTERNAL
C     REFERENCES TO THE GENDEFOL/BUDWORM ("BUDLITE") MODEL              
C
      LOGICAL     L,LKECHO
      INTEGER     I1,I2,I3,KEY
      REAL        R1
      CHARACTER*8 KEYWRD,NOKEY
      REAL RDANUW
      INTEGER IDANUW
      LOGICAL LDANUW
C
      DATA NOKEY/'*NO WSBE'/
C
C----------
C  ENTRY BWECUP
C----------
      ENTRY BWECUP
      RETURN
C
C----------
C  ENTRY BWEGO
C----------
      ENTRY BWEGO (L)
        L= .FALSE.
      RETURN
C
C----------
C  ENTRY BWEINT
C----------
      ENTRY BWEINT
      RETURN
C
C----------
C  ENTRY BWEIN
C----------
      ENTRY BWEIN(LKECHO)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = LKECHO
      RETURN
C
C----------
C  ENTRY BWEOUT
C----------
      ENTRY BWEOUT
      RETURN
C
C----------
C  ENTRY BWEKEY
C----------
      ENTRY BWEKEY (KEY, KEYWRD)
        KEYWRD=NOKEY
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = KEY
      RETURN
C
C----------
C  ENTRY BWEPPATV
C----------
      ENTRY BWEPPATV (L)
        L= .FALSE.
      RETURN
C
C----------
C  ENTRY BWEPPGT
C----------
      ENTRY BWEPPGT (R1,I1,I2,I3)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = R1
        IDANUW = I1
        IDANUW = I2
        IDANUW = I3
      RETURN
C
C----------
C  ENTRY BWEPPPT
C----------
      ENTRY BWEPPPT (R1,I1,I2,I3)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = R1
        IDANUW = I1
        IDANUW = I2
        IDANUW = I3
      RETURN
C
      END
