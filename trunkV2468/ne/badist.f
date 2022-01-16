      SUBROUTINE BADIST(DEBUG)
      IMPLICIT NONE
C----------
C NE $Id: badist.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'TWIGCOM.F77'
C
C COMMONS
C----------
      LOGICAL DEBUG
C
      INTEGER I,J,ICLS
      REAL TDBH,TREEBA
      LOGICAL LDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      LDANUW = DEBUG
C
C----------
C  INITIALIZE VARIABLES.
C----------
      DO  5 J=1,50
      BAU(J) = 0.0
    5 CONTINUE
C----------
C  COMPUTE BA BY DBH CLASS, BA BY SPECIES, AND BA BY DBH CLASS BY
C  SPECIES.
C----------
      DO 100 I=1,ITRN
      ICLS = IFIX(DBH(I) + 1.0)
      IF(ICLS .GT. 50) ICLS = 50
      TDBH = DBH(I)
      IF(TDBH .LT. 1.0) TDBH=1.0
      TREEBA = 0.0054542 * TDBH * TDBH * PROB(I)
      BAU(ICLS) = BAU(ICLS) + TREEBA
  100 CONTINUE
      DO 150 I=49,1,-1
  150   BAU(I)=BAU(I+1)+BAU(I)
      RETURN
      END
