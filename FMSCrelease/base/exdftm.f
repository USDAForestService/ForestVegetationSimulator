      SUBROUTINE EXDFTM
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     EXTRA EXTERNAL REFERENCES FOR DFTM CALLS
C
      INTEGER II,MGMID,KEY
      INTEGER ICODES(6)
      LOGICAL L,LKECHO
      CHARACTER*8 KEYWRD,NODFTM
      CHARACTER*26 NPLT
      INTEGER IDANUW
      CHARACTER*8 CDANUW
      LOGICAL LDANUW
C
      DATA NODFTM/'*NO DFTM'/
C
C----------
C  ENTRY TMINIT
C----------
      ENTRY TMINIT
      RETURN
C
C----------
C  ENTRY DFTMIN
C----------
      ENTRY DFTMIN(LKECHO)
        CALL ERRGRO (.TRUE.,11)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = LKECHO
      RETURN
C
C----------
C  ENTRY TMDAM
C----------
      ENTRY TMDAM (II,ICODES)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = II
        IDANUW = ICODES(1)
      RETURN
C
C----------
C  ENTRY TMOPS
C----------
      ENTRY TMOPS
      RETURN
C
C----------
C  ENTRY TMCOUP
C----------
      ENTRY TMCOUP
      RETURN
C
C----------
C  ENTRY DFTMGO
C----------
      ENTRY DFTMGO (L)
        L = .FALSE.
      RETURN
C
C----------
C  ENTRY TMBMAS
C----------
      ENTRY TMBMAS
      RETURN
C
C----------
C  ENTRY TMHED
C----------
      ENTRY TMHED (NPLT,MGMID)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW(1:8) = NPLT(1:8)
        IDANUW = MGMID
      RETURN
C
C----------
C  ENTRY TMOUT
C----------
      ENTRY TMOUT
      RETURN
C
C----------
C  ENTRY TMKEY
C----------
      ENTRY TMKEY(KEY,KEYWRD)
        KEYWRD=NODFTM
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = KEY
      RETURN
C
      END
