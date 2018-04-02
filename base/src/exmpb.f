      SUBROUTINE EXMPB
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     EXTRA EXTERNAL REFERENCES FOR MPB CALLS
C
      LOGICAL L,LNOTBK,LKECHO
      CHARACTER*8 KEYWRD,NOMPB
      CHARACTER*26 NPLT
      DIMENSION LNOTBK(7)
      INTEGER ICODES(6)
      REAL ARRAY(7)
      INTEGER MGMID,II,KEY
      REAL RDANUW
      INTEGER IDANUW
      CHARACTER*8 CDANUW
      LOGICAL LDANUW
C
      DATA NOMPB/'*NO MPB'/
C
C----------
C  ENTRY MPBINT
C----------
      ENTRY MPBINT
      RETURN
C
C----------
C  ENTRY MPBHED
C----------
      ENTRY MPBHED (NPLT,MGMID)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW(1:8) = NPLT(1:8)
      IDANUW = MGMID
C
      RETURN
C
C----------
C  ENTRY MPBIN
C----------
      ENTRY MPBIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW(1:8) = KEYWRD(1:8)
      RDANUW = ARRAY(1)
      LDANUW = LNOTBK(1)
      LDANUW = LKECHO
C
      CALL ERRGRO (.TRUE.,11)
      RETURN
C
C----------
C  ENTRY MPBDAM
C----------
      ENTRY MPBDAM (II,ICODES)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = II
      IDANUW = ICODES(1)
C
      RETURN
C
C----------
C  ENTRY MPBOPS
C----------
      ENTRY MPBOPS
      RETURN
C
C----------
C  ENTRY MPBCUP
C----------
      ENTRY MPBCUP
      RETURN
C
C----------
C  ENTRY MPBGO
C----------
      ENTRY MPBGO (L)
      L = .FALSE.
      RETURN
C
C----------
C  ENTRY MPBOUT
C----------
      ENTRY MPBOUT
      RETURN
C
C----------
C  ENTRY MPSDLP
C----------
      ENTRY MPSDLP
      RETURN
C
C----------
C  ENTRY MPKEY
C----------
      ENTRY MPKEY(KEY,KEYWRD)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = KEY
C
      KEYWRD=NOMPB
      RETURN
C
      END
