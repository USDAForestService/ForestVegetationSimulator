      SUBROUTINE ESMSGS (JOREGT)
      IMPLICIT NONE
C----------
C  **ESMSGS DATE OF LAST REVISION:   07/25/08
C----------
C
C     CALLED FROM ESIN TO PRINT HABITAT TYPE GROUPS FOR THE
C     REGENERATION ESTABLISHMENT MODEL.
C
      INTEGER JOREGT
C
      WRITE(JOREGT,100)
  100 FORMAT('HABITAT TYPE GROUPS USED IN THE REGENERATION',
     &  ' ESTABLISHMENT MODEL',//,'GROUP   HABITAT TYPES',/,
     &  '-----   ',34('-'),/,
     &  '   1    PSME/ VAGL, LIBO, VACA',/,
     &  '   2    PSME/ CARU, CAGE, GRASS',/,
     &  '   3    PSME/ PHMA, ACGL',/,
     &  '   4    PSME/ SYAL, SPBE, SYOR, ARUV, ARCO',/,
     &  '   5    ABGR/ LIBO, CLUN-XETE',/,
     &  '   6    ABGR/ XETE, VAGL, COOC, VACA',/,
     &  '   7    ABGR/ CLUN (EXCEPT CLUN-XETE)',/,
     &  '   8    ABGR/ SPBE, ACGL, PHMA, ASCA, SETR',/,
     &  '   9    THPL/ ALL',/,
     &  '  10    TSHE/ ALL',/,
     &  '  11    ABLA/ VAGL, VASC, VACA',/,
     &  '  12    ABLA/ XETE, LIBO',/,
     &  '  13    ABLA/ CLUN, GATR',/,
     &  '  14    ABLA/ CAGE, CARU, ACGL, SPBE',/,
     &  '  15    ABLA/ MEFE, ALSI',/,
     &  '        TSME/ CLUN, XETE, MEFE, STAM',/,
     &  '  16    ABLA/ CACA, STAM, LUHI')
      RETURN
      END
