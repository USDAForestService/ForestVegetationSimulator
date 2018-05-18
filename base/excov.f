      SUBROUTINE EXCOV
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  EXTRA EXTERNAL REFERENCES FOR CVCNOP AND CVBROW CALLS.
C----------
      REAL ARRAY(7)
      REAL XDUM(*)
      INTEGER KEY,I,J,JJ,II
      LOGICAL LTHIN,LACTV,LNOTBK(7),LKECHO
      CHARACTER*8 KEYWRD,NOCOV
      REAL RDANUW
      INTEGER IDANUW
      CHARACTER*8 CDANUW
      LOGICAL LDANUW
C
      DATA NOCOV/'*NO COVR'/
C
C----------
C  ENTRY CVINIT
C----------
      ENTRY CVINIT
      RETURN
C
C----------
C  ENTRY CVIN
C----------
      ENTRY CVIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
        CALL ERRGRO (.TRUE.,11)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW = KEYWRD
        RDANUW = ARRAY(1)
        LDANUW = LNOTBK(1)
        LDANUW = LKECHO
      RETURN
C
C----------
C  ENTRY CVGO
C----------
      ENTRY CVGO (LACTV)
        LACTV=.FALSE.
      RETURN
C
C----------
C  ENTRY CVCNOP
C----------
      ENTRY CVCNOP(LTHIN)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = LTHIN
      RETURN
C
C----------
C  ENTRY CVBROW
C----------
      ENTRY CVBROW(LTHIN)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = LTHIN
      RETURN
C
C----------
C  ENTRY CVOUT
C----------
      ENTRY CVOUT
      RETURN
C
C----------
C  ENTRY CVKEY
C----------
      ENTRY CVKEY(KEY,KEYWRD)
        KEYWRD=NOCOV
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = KEY
      RETURN
C
C     REFERENCES FOR THE PARALLEL PROCESSING SYSTEM.
C
C----------
C  ENTRY CVGET
C----------
      ENTRY CVGET (XDUM, I, J)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = XDUM(1)
        IDANUW = I
        IDANUW = J
      RETURN
C----------
C  ENTRY CVPUT
C----------
      ENTRY CVPUT (XDUM, I, J, II, JJ)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = XDUM(1)
        IDANUW = I
        IDANUW = J
        IDANUW = II
        IDANUW = JJ
      RETURN
C----------
C  ENTRY CVACTV
C----------
      ENTRY CVACTV (LACTV)
        LACTV=.FALSE.
      RETURN
C
      END
