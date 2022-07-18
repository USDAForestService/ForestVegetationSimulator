      SUBROUTINE EXDFB
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     EXTRA EXTERNAL REFERENCES FOR DFB CALLS
C
      LOGICAL L,LNOTBK,LKECHO
      CHARACTER*8 KEYWRD,NODFB
      REAL ARRAY(7)
      DIMENSION LNOTBK(7)
      INTEGER ICODES(6),II,KEY
      REAL RDANUW
      INTEGER IDANUW
      CHARACTER*8 CDANUW
      LOGICAL LDANUW
C
      DATA NODFB/'*NO DFB'/
C
C----------
C  ENTRY DFBINT
C----------
      ENTRY DFBINT
      RETURN
C
C----------
C  ENTRY DFBIN
C----------
      ENTRY DFBIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
        CALL ERRGRO (.TRUE.,11)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW(1:8) = KEYWRD(1:8)
        RDANUW = ARRAY(1)
        LDANUW = LNOTBK(1)
        LDANUW = LKECHO
      RETURN
C
C----------
C  ENTRY DFBDAM
C----------
      ENTRY DFBDAM (II,ICODES)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = II
        IDANUW = ICODES(1)
      RETURN
C
C----------
C  ENTRY DFBSCH
C----------
      ENTRY DFBSCH
      RETURN
C
C----------
C  ENTRY DFBDRV
C----------
      ENTRY DFBDRV
      RETURN
C
C----------
C  ENTRY DFBBO
C----------
      ENTRY DFBGO (L)
        L = .FALSE.
      RETURN
C
C----------
C  ENTRY DFBOUT
C----------
      ENTRY DFBOUT
      RETURN
C
C----------
C  ENTRY DFBINV
C----------
      ENTRY DFBINV
      RETURN
C
C----------
C  ENTRY DFBWIN
C----------
      ENTRY DFBWIN (L)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = L
      RETURN
C
C----------
C  ENTRY DFBKEY
C----------
      ENTRY DFBKEY (KEY,KEYWRD)
        KEYWRD=NODFB
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = KEY
      RETURN
C
      END
