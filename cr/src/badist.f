      SUBROUTINE BADIST(DEBUG)
      IMPLICIT NONE
C----------
C   ** BADIST--CR   DATE OF LAST REVISION:  02/24/09
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
      INCLUDE 'GGCOM.F77'
C
C COMMONS
C----------
      LOGICAL DEBUG
      INTEGER I,J,ISPC,ICLS,K
      REAL TOTBA,TDBH,TREEBA
C----------
C  INITIALIZE VARIABLES.
C----------
      DO 10 I=1,MAXSP
        TBA(I) = 0.0
        DO  5 J=1,41
          IF(I .EQ. 1) BAU(J) = 0.0
          BCLAS(I,J) = 0.0
          TCLAS(I,J) = 0.0
    5   CONTINUE
   10 CONTINUE
      TOTBA = 0.0
      SEEDS = 0.0
      TPAT  = 0.0
C----------
C  COMPUTE BA BY DBH CLASS, BA BY SPECIES, AND BA BY DBH CLASS BY
C  SPECIES.
C----------
      DO 100 I=1,ITRN
        ISPC = ISP(I)
        IF(LSTART) THEN
          TDBH=WK3(I)
        ELSE
          TDBH=DBH(I)
        ENDIF
        ICLS = IFIX(TDBH + 1.0)
        IF(ICLS .GT. 41) ICLS = 41
        IF(TDBH .LT. 1.0) TDBH=1.0
        IF(HT(I) .LT. 4.5) THEN
          SEEDS = SEEDS + PROB(I)
          GO TO 100
        ENDIF
        TREEBA = 0.0054542 * TDBH * TDBH * PROB(I)
        BCLAS(ISPC,ICLS) = BCLAS(ISPC,ICLS) + TREEBA
        TOTBA = TOTBA + TREEBA
        TBA(ISPC) = TBA(ISPC) + TREEBA
        BAU(ICLS) = BAU(ICLS) + TREEBA
        TCLAS(ISPC,ICLS) = TCLAS(ISPC,ICLS) + PROB(I)
        TPAT = TPAT + PROB(I)
  100 CONTINUE
C----------
C BAU NOW CONTAINS BY BY CLASS, CONVERT TO BA ABOVE CLASS.
C----------
      BAU(1)=TOTBA - BAU(1)
      IF(BAU(1) .LT. 0.0) BAU(1)=0.0
      DO 150 I=2,41
        K=I-1
        BAU(I) = BAU(K) - BAU(I)
        IF(BAU(I) .LT. 0.0) BAU(I)=0.0
  150 CONTINUE
      IF(DEBUG) THEN
        WRITE(JOSTND,*)' IN BADIST TOTBA =',TOTBA
        WRITE(JOSTND,*)' I,BCLAS(I, ),BAU(I) I=1,MAXSP BY DBH CLASS ='
        DO 160 I=1,41
          WRITE(JOSTND,155)I,(BCLAS(J,I),J=1,MAXSP),BAU(I)
  155     FORMAT(' ',I3,4(12F10.3,/' '))
  160   CONTINUE
        WRITE(JOSTND,165)TBA
  165     FORMAT(' TBA BY SPECIES =',10(11F10.4,/' ',16X))
      ENDIF
C
      RETURN
      END
