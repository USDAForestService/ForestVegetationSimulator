      SUBROUTINE FMR6SDCY (KSP, DBH, X, Y, SML)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
C     CALLED FROM: FMSNAG
C
*----------------------------------------------------------------------
*  Purpose:
*     This subroutine calculates the time (in years) it takes a hard 
*     snag to become soft.  This is based on species, plant association,
*     and size.  An adjustment factor (used in the snag fall calculations)
*     is also computed. 
*
*     The calculations are based on work done by Kim Mellen, regional
*     wildlife ecologist for region 6.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     KSP - SPECIES OF SNAG
*     DBH - DBH OF SNAG
*     X - THE NUMBER OF YEARS IT TAKES FOR THIS SNAG TO GO FROM HARD TO SOFT
*     Y - THE ADJUSTMENT FACTOR (USED IN SNAG FALL CALCULATIONS)
*         (1 = decrease, 2 = none, 3 = increase)
*     WSDBH1,WSDBH2,BMDBH1,BMDBH2,ECDBH1,ECDBH2,SODBH1,SODBH2,
*     AKDBH1,AKDBH2 - THE DBH BREAKPOINTS (IN CM) TO DETERMINE IF A TREE IS 
*     SMALL, MEDIUM, OR LARGE FOR EACH VARIANT.
*     WSSPEC,BMSPEC,ECSPEC,SOSPEC,AKSPEC - THE SPECIES GROUP (1 - 12 ) FOR EACH SPECIES
*     IN EACH VARIANT:
*          1 = cedar
*          2 = douglas-fir
*          3 = ponderosa pine
*          4 = oak/madrone
*          5 = lodgepole pine
*          6 = spruce
*          7 = hemlock
*          8 = true firs
*          9 = pacific silver fir (red fir in the east-side variants)
*          10 = misc. hardwoods
*          11 = populus group
*          12 = alders
*     PNWMC,WCWMC,SOHMC,BMHMC,ECHMC - MAPS EACH PLANT ASSOCIATION TO A
*           TEMPERATURE CODE:
*           1 = warm or hot
*           2 = moderate
*           3 = cold
*     PNWMD,WCWMD,SOWMD,BMWMD,ECWMD - MAPS EACH PLANT ASSOCIATION TO A
*           MOISTURE CODE:
*           1 = wet
*           2 = mesic
*           3 = dry
*     PNYRSOFT,ESYRSOFT,WCYRSOFT (I,J,K,L) -  ARRAY WITH YEARS TIL SOFT
*     VALUES FOR VARIOUS:
*            species (i:1-12),
*            temperatures (j:1-3),
*            moistures (k:1-3), and 
*            sizes (l:1-3, 1=small, 2=medium, 3=large)    
*     PNDCYADJ,ESDCYADJ,WCDCYADJ (I,J,K,L) - ARRAY WITH SNAG FALL ADJUSTMENT
*     FACTORS, DIMENSIONED SAME AS YRSOFT ARRAYS.
*            1 = decrease snag fall
*            2 = no adjustment
*            3 = increase snag fall
*
***********************************************************************
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77' 
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS
C----------
C  Variable declarations.
C----------
      LOGICAL DEBUG
      INTEGER I, J, K, L, X, Y, KSP, SML, SPG, TEMP, MOIS
      INTEGER WSSPEC(39),BMSPEC(18),ECSPEC(32), SOSPEC(33), AKSPEC(13)
      INTEGER PNWMC(75),PNWMD(75),WCWMC(139),WCWMD(139),SOHMC(92)
      INTEGER SOWMD(92),BMHMC(92),BMWMD(92),ECHMC(155),ECWMD(155)
      INTEGER PNYRSOFT(12,3,3,3),PNDCYADJ(12,3,3,3),ESYRSOFT(12,3,3,3)
      INTEGER ESDCYADJ(12,3,3,3),WCYRSOFT(12,3,3,3),WCDCYADJ(12,3,3,3)
      REAL    DBH, DBHCM
      REAL    WSDBH1(39),WSDBH2(39),BMDBH1(18),BMDBH2(18),ECDBH1(32)  
      REAL    ECDBH2(32),SODBH1(33),SODBH2(33),AKDBH1(13),AKDBH2(13)
C----------
C  CHECK FOR DEBUG.
C----------
      CALL DBCHK (DEBUG,'FMR6SDCY',8,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC, KSP, DBH, VARACD, ITYPE
    7 FORMAT(' FMR6SDCY CYCLE=',I2,' KSP=',I5,' DBH=',F6.2,' VARACD=',
     &       A2,' ITYPE=',I5)
C----------
C  THESE ARE THE WESTSIDE (PN/WC) DBH BREAKPOINTS (SMALL TO MED.) IN CM.
C----------
      DATA (WSDBH1(I), I=  1, 39) /
     & 25,25,25,20,25,25,25,25,25,25,
     & 20,20,25,25,20,25,25,25,25,25,
     & 25,20,20,20,25,20,25,25,20,20,
     & 20,20,20,20,20,20,20,20,20/
C----------
C  THESE ARE THE BM DBH BREAKPOINTS (SMALL TO MED.) IN CM.
C----------
      DATA (BMDBH1(I), I= 1, 18) /
     & 25, 25, 25, 25, 25, 20, 20, 25, 20, 20,
     & 20, 20, 20, 25, 20, 25, 20, 20/
C----------
C  THESE ARE THE EC DBH BREAKPOINTS (SMALL TO MED.) IN CM.
C----------
      DATA (ECDBH1(I), I= 1, 32) /
     & 25, 25, 25, 25, 25, 25, 20, 25, 20, 20,
     & 25, 25, 20, 20, 25, 25, 20, 25, 20, 25,
     & 25, 20, 20, 25, 20, 20, 25, 25, 20, 20,
     & 25, 20/
C----------
C  THESE ARE THE SO DBH BREAKPOINTS (SMALL TO MED.) IN CM.
C----------
      DATA (SODBH1(I), I= 1, 33) /
     & 25,25,25,25,25,25,20,25,25,20,
     & 20,25,20,25,25,20,25,25,25,20,
     & 20,20,25,20,25,20,25,20,25,20,
     & 20,25,20/
C----------
C  THESE ARE THE AK DBH BREAKPOINTS (SMALL TO MED.) IN CM.
C----------
      DATA (AKDBH1(I), I= 1, 13) /
     & 25,25,25,25,25,25,20,25,20,20,
     & 25,25,25/
C----------
C  SET THE MEDIUM TO LARGE DBH BREAKPOINTS IN CM.
C----------
      DO I = 1, 39
        IF (WSDBH1(I) .EQ. 20) THEN
          WSDBH2(I) = 50
        ELSE
          WSDBH2(I) = 75
        ENDIF
        IF (I .LE. 33) THEN
          IF (SODBH1(I) .EQ. 20) THEN
            SODBH2(I) = 50
          ELSE
            SODBH2(I) = 75
          ENDIF
        ENDIF
        IF (I .LE. 18) THEN
          IF (BMDBH1(I) .EQ. 20) THEN
            BMDBH2(I) = 50
          ELSE
            BMDBH2(I) = 75
          ENDIF
        ENDIF
        IF (I .LE. 13) THEN
          IF (AKDBH1(I) .EQ. 20) THEN
            AKDBH2(I) = 50
          ELSE
            AKDBH2(I) = 75
          ENDIF
        ENDIF
        IF (I .LE. 32) THEN
          IF (ECDBH1(I) .EQ. 20) THEN
            ECDBH2(I) = 50
          ELSE
            ECDBH2(I) = 75
          ENDIF
        ENDIF
      ENDDO
C----------
C  THESE ARE THE WESTSIDE (PN/WC) SPECIES GROUPS (1 - 12) TO USE FOR SNAG DECAY
C----------
      DATA (WSSPEC(I), I= 1, 39) /
     &  9, 8, 8, 9, 8, 6, 9, 1, 1, 6,
     &  5, 3, 2, 2, 3, 2, 1, 1, 7, 7,
     & 12,12,12,11, 4,11,11, 4, 1, 2,
     &  2, 5, 1,10,10,10,11,11,11/
C----------
C  THESE ARE THE BM SPECIES GROUPS (1 - 12) TO USE FOR SNAG DECAY
C----------
      DATA (BMSPEC(I), I= 1, 18) /
     &  2,  2,  2,  8,  7,  1,  5,  6,  8,  3,
     &  2,  2,  1,  1, 11, 11,  8, 11/  
C----------
C  THESE ARE THE EC SPECIES GROUPS (1 - 12) TO USE FOR SNAG DECAY
C----------
      DATA (ECSPEC(I), I= 1, 32) /
     &  2,  2,  2,  8,  1,  8,  5,  6,  8,  3,
     &  7,  7,  1,  2,  8,  8,  2,  1,  1, 12,
     & 12, 12, 11,  4, 10, 11, 11,  4, 10, 11,
     &  7, 10/
C----------
C  THESE ARE THE SO SPECIES GROUPS (1 - 12) TO USE FOR SNAG DECAY
C----------
      DATA (SOSPEC(I), I= 1, 33) /     
     &  2, 2, 2, 8, 7, 1, 5, 6, 9, 3,
     &  1, 8, 8, 8, 8, 2, 2, 1, 7, 1,
     & 12,12,12,11,11,10, 4,11, 4,11,
     & 11,2,11/
C----------
C  THESE ARE THE AK SPECIES GROUPS (1 - 12) TO USE FOR SNAG DECAY
C----------
      DATA (AKSPEC(I), I= 1, 13) /     
     &  6, 1, 9, 7, 7, 1, 5, 6, 8, 12,
     & 11,11, 9/
C----------
C  EACH PN HABITAT CODE MAPS TO EITHER WARM (1), MODERATE (2)
C  OR COLD (3).
C----------
      DATA (PNWMC(I), I=   1,  50) /
     & 2, 2, 2, 3, 3, 3, 3, 3, 2, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
      DATA (PNWMC(I), I=  51,  75) /
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
     & 2, 2, 2, 2, 2/
C----------
C  EACH PN HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).
C----------
      DATA (PNWMD(I), I=   1,  50) /
     & 3, 3, 3, 3, 3, 3, 3, 1, 3, 2,
     & 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 1, 2, 2, 1, 1, 2, 1, 1,
     & 2, 3, 2, 1, 2, 2, 3, 2, 3, 2,
     & 3, 2, 2, 2, 2, 2, 1, 3, 3, 1/
      DATA (PNWMD(I), I=  51,  75) /
     & 3, 2, 2, 2, 2, 2, 2, 1, 2, 1,
     & 1, 2, 2, 2, 2, 2, 2, 1, 1, 1,
     & 1, 1, 1, 1, 1/
C----------
C  EACH WC HABITAT CODE MAPS TO EITHER WARM (1), MODERATE (2)
C  OR COLD (3).
C----------
      DATA (WCWMC(I), I=   1,  50) /
     & 3, 3, 3, 3, 3, 3, 3, 1, 2, 1,
     & 1, 1, 1, 1, 1, 1, 2, 2, 3, 3,
     & 2, 3, 3, 3, 3, 2, 2, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 2, 2, 2, 2, 2, 3, 3, 3, 2, 3/
      DATA (WCWMC(I), I=  51, 100) /
     & 3, 3, 3, 3, 3, 2, 3, 3, 3, 1,
     & 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 1, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 3, 2, 2, 2, 1, 2, 3, 2, 2/
      DATA (WCWMC(I), I= 101, 139) /
     & 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
     & 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 1, 1, 1/
C----------
C  EACH WC HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).
C----------
      DATA (WCWMD(I), I=   1,  50) /
     & 3, 2, 2, 2, 2, 2, 3, 3, 2, 3,
     & 3, 3, 3, 3, 3, 3, 1, 1, 2, 2,
     & 2, 2, 2, 1, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 3, 2,
     & 1, 2, 2, 2, 2, 2, 2, 2, 1, 1/
      DATA (WCWMD(I), I=  51, 100) /
     & 1, 1, 1, 3, 2, 1, 2, 3, 2, 3,
     & 3, 1, 1, 1, 1, 2, 2, 1, 1, 3,
     & 1, 2, 2, 1, 1, 3, 1, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 3, 2, 3, 3,
     & 2, 3, 2, 3, 3, 3, 3, 3, 1, 2/
      DATA (WCWMD(I), I= 101, 139) /
     & 1, 1, 1, 1, 1, 1, 3, 1, 2, 2,
     & 2, 2, 2, 1, 3, 2, 3, 2, 1, 1,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 1, 2, 3, 3, 3/
C----------
C  EACH SO HABITAT CODE MAPS TO EITHER HOT (1), MODERATE (2)
C  OR COLD (3).
C----------
      DATA (SOHMC(I), I=   1,  50) /
     & 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
     & 3, 1, 2, 2, 2, 2, 2, 2, 2, 3,
     & 3, 3, 1, 3, 1, 2, 2, 2, 1, 1/
      DATA (SOHMC(I), I=  51,  92) /
     & 1, 1, 1, 1, 2, 2, 1, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 1, 3, 2, 2, 2, 1,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2/
C----------
C  EACH SO HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).
C----------
      DATA (SOWMD(I), I=   1,  50) /
     & 2, 2, 2, 1, 1, 2, 1, 1, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 2,
     & 1, 2, 1, 1, 2, 1, 1, 2, 1, 3,
     & 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 2, 3, 3/
      DATA (SOWMD(I), I=  51,  92) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 2, 3, 3, 3, 2, 3, 2, 2,
     & 2, 2, 3, 2, 3, 1, 2, 2, 2, 3,
     & 1, 3, 3, 3, 3, 2, 3, 2, 2, 2,
     & 1, 2/
C----------
C  EACH BM HABITAT CODE MAPS TO EITHER HOT (1), MODERATE (2)
C  OR COLD (3).
C----------
      DATA (BMHMC(I), I=   1,  50) /
     & 3, 3, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 2, 3, 3, 2, 3, 2, 2, 2, 3, 3, 
     & 3, 2, 2, 2, 2, 2, 3, 3, 1, 1/
      DATA (BMHMC(I), I=  51,  92) /
     & 1, 1, 2, 2, 2, 1, 2, 2, 1, 2, 
     & 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
     & 2, 1/
C----------
C  EACH BM HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).
C----------
      DATA (BMWMD(I), I=   1,  50) /
     & 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 
     & 3, 2, 3, 2, 1, 2, 3, 1, 1, 2, 
     & 1, 1, 2, 2, 2, 2, 2, 3, 2, 3,
     & 2, 3, 3, 1, 1, 1, 2, 1, 2, 3, 
     & 3, 3, 2, 2, 2, 2, 3, 3, 3, 3/
      DATA (BMWMD(I), I=  51,  92) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
     & 3, 3, 3, 3, 3, 3, 3, 1, 1, 2,
     & 2, 2, 2, 1, 1, 1, 3, 3, 3, 2, 
     & 2, 2, 3, 3, 2, 1, 3, 2, 1, 2,
     & 2, 2/
C----------
C  EACH EC HABITAT CODE MAPS TO EITHER HOT (1), MODERATE (2)
C  OR COLD (3).
C----------
      DATA (ECHMC(I), I=   1,  50) /
     & 3, 3, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 1, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 3, 3, 3, 3/
      DATA (ECHMC(I), I=  51, 100) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 2, 2, 2, 3, 3, 2, 2, 3,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 3, 3/
      DATA (ECHMC(I), I= 101, 150) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 1,
     & 1, 1, 2, 1, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
      DATA (ECHMC(I), I= 151, 155) /
     & 2, 2, 2, 2, 2/
C----------
C  EACH EC HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).
C----------
      DATA (ECWMD(I), I=   1,  50) /
     & 3, 3, 2, 2, 2, 2, 1, 2, 2, 2,
     & 3, 3, 3, 3, 3, 2, 3, 3, 3, 3,
     & 2, 2, 3, 3, 2, 2, 3, 2, 2, 2,
     & 2, 3, 2, 2, 2, 3, 3, 3, 3, 3,
     & 2, 2, 2, 2, 2, 2, 3, 2, 2, 2/
      DATA (ECWMD(I), I=  51, 100) /
     & 1, 2, 2, 2, 3, 3, 1, 2, 2, 3,
     & 1, 2, 2, 2, 2, 3, 3, 2, 2, 3,
     & 3, 3, 2, 2, 2, 2, 2, 1, 1, 1,
     & 2, 2, 2, 2, 2, 2, 2, 2, 3, 2,
     & 2, 2, 2, 2, 2, 2, 1, 2, 3, 2/
      DATA (ECWMD(I), I= 101, 150) /
     & 3, 2, 1, 1, 2, 2, 2, 1, 2, 3,
     & 3, 3, 3, 3, 2, 2, 2, 2, 2, 2,
     & 2, 3, 3, 3, 3, 3, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 3, 2, 3,
     & 2, 3, 2, 3, 3, 2, 2, 2, 2, 2/
      DATA (ECWMD(I), I= 151, 155) /
     & 2, 3, 2, 3, 2/
C----------
C  PN - YEARS TO DECAY FROM HARD TO SOFT W/ LAG
C----------
      DATA ((((PNYRSOFT(I,J,K,L), L=1,3), K=1,3), J=1,3), I=1,12) /
     & 36,42,46,36,42,46,36,42,46,36,70,94,36,70,94,36,70,94,69,76,
     & 94,69,76,94,69,76,94,21,25,27,21,25,27,21,25,27,21,40,52,21,
     & 40,52,21,40,52,39,45,52,39,45,52,39,45,52,12,13,28,12,13,28,
     & 12,14,40,12,20,53,12,20,53,14,21,53,21,23,53,21,23,53,21,23,
     & 53,5 ,7 ,7 ,5 ,7 ,7 ,5 ,7 ,7 ,5 ,10,13,5 ,10,13,5 ,10,13,9 ,
     & 11,13,9 ,11,13,9 ,11,13,11,13,14,11,13,14,11,13,14,11,20,25,
     & 11,20,25,11,20,25,19,22,25,19,22,25,19,22,25,16,19,21,16,19,
     & 21,16,19,21,16,30,40,16,30,40,16,30,40,29,34,40,29,34,40,29,
     & 34,40,11,13,14,11,13,14,11,13,14,11,20,25,11,20,25,11,20,25,
     & 19,22,25,19,22,25,19,22,25,7 ,9 ,9 ,7 ,9 ,9 ,7 ,9 ,9 ,7 ,12,
     & 15,7 ,12,15,7 ,12,15,11,14,15,11,14,15,11,14,15,7 ,9 ,10,7 ,
     & 9 ,10,7 ,9 ,10,7 ,13,16,7 ,13,16,7 ,13,16,12,14,16,12,14,16,
     & 12,14,16,4 ,6 ,7 ,4 ,6 ,7 ,4 ,6 ,7 ,4 ,10,12,4 ,10,12,4 ,10,
     & 12,9 ,11,12,9 ,11,12,9 ,11,12,4 ,6 ,7 ,4 ,6 ,7 ,4 ,6 ,7 ,4 ,
     & 9 ,12,4 ,9 ,12,4 ,9 ,12,9 ,10,12,9 ,10,12,9 ,10,12,3 ,5 ,5 ,
     & 3 ,5 ,5 ,3 ,5 ,5 ,3 ,7 ,9 ,3 ,7 ,9 ,3 ,7 ,9 ,6 ,7 ,9 ,6 ,7 ,
     & 9 ,6 ,7 ,9/
C----------
C  PN - DECAY RATE ADJUSTMENT FOR SNAG FALL (1 = DECREASE, 2 = NONE, 3 = INCREASE)                
C----------
      DATA ((((PNDCYADJ(I,J,K,L), L=1,3), K=1,3), J=1,3), I=1,12) /
     & 3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,2,1,2,1,
     & 1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,2,1,3,
     & 2,1,3,2,1,2,1,1,2,1,1,2,1,1,3,3,3,3,3,3,
     & 3,3,3,3,2,1,3,2,1,3,1,1,2,1,1,2,1,1,2,1,
     & 1,3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,2,1,2,
     & 1,1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,2,1,
     & 3,2,1,3,2,1,2,1,1,2,1,1,2,1,1,3,3,3,3,3,
     & 3,3,3,3,3,2,1,3,2,1,3,2,1,2,1,1,2,1,1,2,
     & 1,1,3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,2,1,
     & 2,1,1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,2,
     & 1,3,2,1,3,2,1,2,1,1,2,1,1,2,1,1,3,3,3,3,
     & 3,3,3,3,3,3,2,1,3,2,1,3,2,1,2,1,1,2,1,1,
     & 2,1,1,3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,2,
     & 1,2,1,1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,
     & 2,1,3,2,1,3,2,1,2,1,1,2,1,1,2,1,1,3,3,3,
     & 3,3,3,3,3,3,3,2,1,3,2,1,3,2,1,2,1,1,2,1,
     & 1,2,1,1/
C----------
C  WC - YEARS TO DECAY FROM HARD TO SOFT W/ LAG                 
C----------
      DATA ((((WCYRSOFT(I,J,K,L), L=1,3), K=1,3), J=1,3), I=1,12) /
     & 36,42,46,36,42,46,36,49,73,36,70,94,36,70,94,41,70,94,69,76,
     & 94,69,76,94,69,76,94,21,25,27,21,25,27,21,28,38,21,40,52,21,
     & 40,52,24,41,52,39,45,52,39,45,52,39,45,52,12,13,28,12,13,28,
     & 12,14,40,12,20,53,12,20,53,14,21,53,21,23,53,21,23,53,21,23,
     & 53,5 ,7 ,7 ,5 ,7 ,7 ,5 ,7 ,10,5 ,10,13,5 ,10,13,6 ,11,13,9 ,
     & 11,13,9 ,11,13,9 ,11,13,11,13,14,11,13,14,11,14,19,11,20,25,
     & 11,20,25,12,19,25,19,22,25,19,22,25,19,22,25,32,34,38,32,34,
     & 38,32,38,52,32,55,68,32,55,68,33,59,68,54,64,68,54,64,68,54,
     & 64,68,11,13,14,11,13,14,11,14,19,11,20,25,11,20,25,12,20,25,
     & 19,17,25,19,17,25,19,17,25,7 ,9 ,9 ,7 ,9 ,9 ,7 ,10,12,7 ,12,
     & 15,7 ,12,15,8 ,13,15,11,14,15,11,14,15,11,14,15,4 ,4 ,4 ,4 ,
     & 4 ,4 ,4 ,4 ,3 ,4 ,3 ,1 ,4 ,3 ,1 ,3 ,3 ,1 ,2 ,2 ,1 ,2 ,2 ,1 ,
     & 2 ,2 ,1 ,4 ,6 ,7 ,4 ,6 ,7 ,4 ,7 ,9 ,4 ,10,12,4 ,10,12,5 ,10,
     & 12,9 ,11,12,9 ,11,12,9 ,11,12,4 ,6 ,7 ,4 ,6 ,7 ,4 ,7 ,9 ,4 ,
     & 10,12,4 ,10,12,5 ,10,12,9 ,10,12,9 ,10,12,9 ,10,12,3 ,5 ,5 ,
     & 3 ,5 ,5 ,3 ,5 ,6 ,3 ,7 ,9 ,3 ,7 ,9 ,4 ,7 ,9 ,6 ,7 ,9 ,6 ,7 ,
     & 9 ,6 ,7 ,9/      
C----------
C  WC - DECAY RATE ADJUSTMENT FOR SNAG FALL (1 = DECREASE, 2 = NONE, 3 = INCREASE)
C----------
      DATA ((((WCDCYADJ(I,J,K,L), L=1,3), K=1,3), J=1,3), I=1,12) /
     & 3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,1,1,2,1,
     & 1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,2,1,3,
     & 2,1,3,1,1,2,1,1,2,1,1,2,1,1,3,3,3,3,3,3,
     & 3,3,3,3,2,1,3,2,1,3,1,1,2,1,1,2,1,1,2,1,
     & 1,3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,1,1,2,
     & 1,1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,2,1,
     & 3,2,1,3,1,1,2,1,1,2,1,1,2,1,1,3,3,3,3,3,
     & 3,3,3,3,3,2,1,3,2,1,3,1,1,2,1,1,2,1,1,2,
     & 1,1,3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,1,1,
     & 2,1,1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,2,
     & 1,3,2,1,3,1,1,2,1,1,2,1,1,2,1,1,3,3,3,3,
     & 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
     & 3,3,3,3,3,3,3,3,3,3,3,3,3,2,1,3,2,1,3,1,
     & 1,2,1,1,2,1,1,2,1,1,3,3,3,3,3,3,3,3,3,3,
     & 2,1,3,2,1,3,1,1,2,1,1,2,1,1,2,1,1,3,3,3,
     & 3,3,3,3,3,3,3,2,1,3,2,1,3,1,1,2,1,1,2,1,
     & 1,2,1,1/
C----------
C  EASTSIDE (BM, SO, EC) - YEARS TO DECAY FROM HARD TO SOFT W/ LAG                 
C----------
      DATA ((((ESYRSOFT(I,J,K,L), L=1,3), K=1,3), J=1,3), I=1,6) /
     & 184,185,246,184,185,246,184,185,246,93 ,125,185,124,185,246,
     & 184,185,246,105,125,125,124,125,185,184,185,246,86 ,96 ,107,
     & 95 ,96 ,107,95 ,96 ,107,46 ,63 ,87 ,58 ,87 ,107,86 ,87 ,107,
     & 54 ,55 ,63 ,54 ,63 ,87 ,95 ,96 ,107,54 ,53 ,147,54 ,53 ,147,
     & 58 ,53 ,147,33 ,36 ,123,34 ,50 ,147,51 ,53 ,147,32 ,33 ,92 ,
     & 33 ,36 ,123,54 ,53 ,147,43 ,44 ,53 ,46 ,47 ,53 ,46 ,47 ,53 ,
     & 22 ,31 ,42 ,27 ,42 ,53 ,41 ,44 ,53 ,25 ,27 ,31 ,26 ,31 ,42 ,
     & 43 ,44 ,53 ,39 ,40 ,49 ,41 ,42 ,49 ,43 ,42 ,49 ,21 ,29 ,38 ,
     & 25 ,38 ,49 ,37 ,40 ,49 ,24 ,26 ,29 ,24 ,29 ,38 ,41 ,42 ,49 ,
     & 58 ,59 ,68 ,58 ,59 ,68 ,63 ,59 ,68 ,29 ,40 ,55 ,37 ,55 ,68 ,
     & 54 ,59 ,68 ,34 ,38 ,40 ,37 ,40 ,55 ,58 ,59 ,68/
C     
      DATA ((((ESYRSOFT(I,J,K,L), L=1,3), K=1,3), J=1,3), I=7,12) /     
     & 55 ,56 ,67 ,55 ,56 ,67 ,61 ,56 ,67 ,28 ,40 ,54 ,35 ,54 ,67 ,
     & 53 ,56 ,67 ,32 ,36 ,40 ,32 ,40 ,54 ,55 ,56 ,67 ,45 ,46 ,55 ,
     & 48 ,49 ,55 ,48 ,49 ,55 ,24 ,33 ,44 ,29 ,44 ,55 ,43 ,46 ,55 ,
     & 27 ,29 ,33 ,28 ,33 ,44 ,45 ,46 ,55 ,36 ,40 ,46 ,39 ,40 ,46 ,
     & 41 ,40 ,46 ,20 ,27 ,37 ,25 ,37 ,46 ,36 ,37 ,46 ,23 ,25 ,27 ,
     & 23 ,27 ,37 ,39 ,40 ,46 ,25 ,26 ,33 ,27 ,28 ,33 ,28 ,28 ,33 ,
     & 13 ,19 ,26 ,16 ,26 ,33 ,25 ,26 ,33 ,15 ,17 ,19 ,16 ,19 ,26 ,
     & 25 ,26 ,33 ,24 ,26 ,36 ,26 ,27 ,36 ,27 ,27 ,36 ,13 ,18 ,25 ,
     & 16 ,25 ,36 ,24 ,25 ,36 ,15 ,16 ,18 ,15 ,18 ,25 ,25 ,26 ,36 ,
     & 17 ,18 ,23 ,18 ,19 ,23 ,18 ,19 ,23 ,9  ,13 ,17 ,11 ,17 ,23 ,
     & 16 ,18 ,23 ,10 ,12 ,13 ,10 ,13 ,17 ,17 ,19 ,23/
C----------
C  EASTSIDE (BM, SO, EC) - DECAY RATE ADJUSTMENT FOR SNAG FALL (1 = DECREASE, 2 = NONE, 3 = INCREASE)
C----------
      DATA ((((ESDCYADJ(I,J,K,L), L=1,3), K=1,3), J=1,3), I=1,12) /
     & 1,1,1,1,1,1,1,1,1,3,3,2,3,2,1,2,1,1,3,3,
     & 3,3,3,2,1,1,1,1,1,1,1,1,1,1,1,1,3,3,2,3,
     & 2,1,2,1,1,3,3,3,3,3,2,1,1,1,1,1,1,1,1,1,
     & 1,1,1,3,3,2,3,2,1,2,1,1,3,3,3,3,3,2,1,1,
     & 1,1,1,1,1,1,1,1,1,1,3,3,2,3,2,1,2,1,1,3,
     & 3,3,3,3,2,1,1,1,1,1,1,1,1,1,1,1,1,3,3,2,
     & 3,2,1,2,1,1,3,3,3,3,3,2,1,1,1,1,1,1,1,1,
     & 1,1,1,1,3,3,2,3,2,1,2,1,1,3,3,3,3,3,2,1,
     & 1,1,1,1,1,1,1,1,1,1,1,3,3,2,3,2,1,2,1,1,
     & 3,3,3,3,3,2,1,1,1,1,1,1,1,1,1,1,1,1,3,3,
     & 2,3,2,1,2,1,1,3,3,3,3,3,2,1,1,1,1,1,1,1,
     & 1,1,1,1,1,3,3,2,3,2,1,2,1,1,3,3,3,3,3,2,
     & 1,1,1,1,1,1,1,1,1,1,1,1,3,3,2,3,2,1,2,1,
     & 1,3,3,3,3,3,2,1,1,1,1,1,1,1,1,1,1,1,1,3,
     & 3,2,3,2,1,2,1,1,3,3,3,3,3,2,1,1,1,1,1,1,
     & 1,1,1,1,1,1,3,3,2,3,2,1,2,1,1,3,3,3,3,3,
     & 2,1,1,1/
C----------
C  DETERMINE THE SPECIES GROUP, TEMPERATURE CODE, MOISTURE CODE, 
C  AND SIZE CLASS - FIRST GET DBH IN TERMS OF CM.
C----------
      DBHCM = DBH * 2.54
      SELECT CASE (VARACD)
        CASE('EC')
          TEMP = ECHMC(ITYPE)
          MOIS = ECWMD(ITYPE)
          SPG = ECSPEC(KSP)
          IF (DBHCM .LT. ECDBH1(KSP)) THEN
      	    SML = 1 
          ELSEIF (DBHCM .LT. ECDBH2(KSP)) THEN
      	    SML = 2 
          ELSE 
      	    SML = 3 
          ENDIF        
        CASE('BM')
          TEMP = BMHMC(ITYPE)
          MOIS = BMWMD(ITYPE)
          SPG = BMSPEC(KSP)
          IF (DBHCM .LT. BMDBH1(KSP)) THEN
      	    SML = 1
          ELSEIF (DBHCM .LT. BMDBH2(KSP)) THEN
      	    SML = 2 
          ELSE 
      	    SML = 3 
          ENDIF          
        CASE('SO')
          TEMP = SOHMC(ITYPE)
          MOIS = SOWMD(ITYPE)
          SPG = SOSPEC(KSP)
          IF (DBHCM .LT. SODBH1(KSP)) THEN
      	    SML = 1 
          ELSEIF (DBHCM .LT. SODBH2(KSP)) THEN
      	    SML = 2 
          ELSE 
      	    SML = 3 
          ENDIF          
        CASE('PN','OP')
          TEMP = PNWMC(ITYPE)
          MOIS = PNWMD(ITYPE)
          SPG = WSSPEC(KSP)
          IF (DBHCM .LT. WSDBH1(KSP)) THEN
      	    SML = 1 
          ELSEIF (DBHCM .LT. WSDBH2(KSP)) THEN
      	    SML = 2 
          ELSE 
      	    SML = 3
          ENDIF        
        CASE('WC')
          TEMP = WCWMC(ITYPE)
          MOIS = WCWMD(ITYPE)
          SPG = WSSPEC(KSP)
          IF (DBHCM .LT. WSDBH1(KSP)) THEN
      	    SML = 1 
          ELSEIF (DBHCM .LT. WSDBH2(KSP)) THEN
      	    SML = 2 
          ELSE 
      	    SML = 3
          ENDIF    
        CASE('AK')
          TEMP = 3 !assume cold
          MOIS = 1 !assume wet
          SPG = AKSPEC(KSP)
          IF (DBHCM .LT. AKDBH1(KSP)) THEN
      	    SML = 1 
          ELSEIF (DBHCM .LT. AKDBH2(KSP)) THEN
      	    SML = 2 
          ELSE 
      	    SML = 3
          ENDIF 
      END SELECT
C----------
C  DETERMINE THE NUMBER OF YEARS FROM HARD TO SOFT AND THE SNAG FALL
C  ADJUSTMENT FACTOR.
C----------
      SELECT CASE (VARACD)
        CASE('PN','AK','OP')
          X = PNYRSOFT(SPG,TEMP,MOIS,SML)
          Y = PNDCYADJ(SPG,TEMP,MOIS,SML)
        CASE('WC')
          X = WCYRSOFT(SPG,TEMP,MOIS,SML)
          Y = WCDCYADJ(SPG,TEMP,MOIS,SML) 
        CASE DEFAULT
          X = ESYRSOFT(SPG,TEMP,MOIS,SML)
          Y = ESDCYADJ(SPG,TEMP,MOIS,SML)         
      END SELECT
C      
      IF (DEBUG) WRITE(JOSTND,8) X, Y, SPG, TEMP, MOIS, SML, DBHCM
    8 FORMAT(' FMR6SDCY X=',I5,' Y=',I5,' SPG=',I3,' TEMP=',I2,
     &       ' MOIS=',I2,' SML=',I2,' DBHCM=',F6.2)      
C      
      RETURN
      END
