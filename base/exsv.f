      SUBROUTINE EXSV
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION EXTERNAL REFERENCES
C     N.L.CROOKSTON -- RMRS MOSCOW -- MARCH 1998
C     A.H.DALLMANN -- RMRS MOSCOW -- JANUARY 2000
C
      INTEGER I1,I2,I
      LOGICAL L(*)
      REAL A(*),A1(*),A2(*)
      CHARACTER*(*) C
      INTEGER IDX(*)
      REAL RDANUW
      INTEGER IDANUW
      CHARACTER*1 CDANUW
      LOGICAL LDANUW
C----------
C  ENTRY SVCMP1
C----------
      ENTRY SVCMP1
      RETURN
C----------
C  ENTRY SVCMP2
C----------
      ENTRY SVCMP2(I1,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
      RETURN
C----------
C  ENTRY SVCMP3
C----------
      ENTRY SVCMP3
      RETURN
C----------
C  ENTRY SVCUTS
C----------
      ENTRY SVCUTS(I,A,A1,A2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
        RDANUW = A(1)
        RDANUW = A1(1)
        RDANUW = A2(1)
      RETURN
C----------
C  ENTRY SVMORT
C----------
      ENTRY SVMORT (I1, A, I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
        RDANUW = A(1)
      RETURN
C----------
C  ENTRY SVOUT
C----------
      ENTRY SVOUT(I1,I2,C)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
        CDANUW = C(1:1)
      RETURN
C----------
C  ENTRY SVTOBJ
C----------
      ENTRY SVTOBJ(I1)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
      RETURN
C----------
C  ENTRY SVINIT
C----------
      ENTRY SVINIT
      RETURN
C----------
C  ENTRY SVKEY
C----------
      ENTRY SVKEY(C,L,A)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW = C(1:1)
        LDANUW = L(1)
        RDANUW = A(1)
      RETURN
C----------
C  ENTRY SVSTART
C----------
      ENTRY SVSTART
      RETURN
C----------
C  ENTRY SVTDEL
C----------
      ENTRY SVTDEL(IDX,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = IDX(1)
        IDANUW = I2
      RETURN
C----------
C  ENTRY SVTRIP
C----------
      ENTRY SVTRIP(I1,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
      RETURN
C----------
C  ENTRY SVESTB
C----------
      ENTRY SVESTB(I1)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
      RETURN
C
      END




