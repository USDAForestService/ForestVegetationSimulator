      SUBROUTINE EXBGC
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     EXTERNAL REFERENCES FOR THE BGC MODEL (KELSEY MILNER)
C
C----------
      INTEGER I1,I2
      REAL ARRAY(7)
      LOGICAL LNOTBK(7),L,LKECHO
      CHARACTER*8 KEYWRD
C----------
C BGCINT CALLED FROM INITRE
C----------
      ENTRY BGCINT
      RETURN
C----------
C BGCIN CALLED FROM INITRE
C----------
      ENTRY BGCIN(KEYWRD,ARRAY,LNOTBK,LKECHO)
      CALL ERRGRO (.TRUE.,11)
      RETURN
C----------
C BGCGO CALLED FROM GRINCR
C----------
      ENTRY BGCGO(L)
      L=.FALSE.
      RETURN
C----------
C BGCGRO CALLED FROM GRINCR
C----------
      ENTRY BGCGRO(I1,I2)
      RETURN
C----------
C BGCFVS CALLED FROM GRADD
C----------
      ENTRY BGCFVS(I1)
      RETURN
C
      END
